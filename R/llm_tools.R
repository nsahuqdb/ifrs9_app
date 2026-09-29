# =============================================================================
# R/llm_tools.R
#
# The analytics "tools" the in-app assistant can call to answer ad-hoc
# questions over run outputs. The assistant runs an agentic loop (see
# R/llm_context.R): it emits a tool call as JSON, this layer executes the
# real data operation against the run output CSVs, and the result is fed
# back to the model. This lets the assistant answer open-ended analytics
# like "compare last quarter's run to this quarter's ECL drivers by stage"
# without us having to precompute every possible summary.
#
# All tools are READ-ONLY over files already on disk. Tools that aggregate
# or compare also return a `chart_data` block (categories + series) so the
# UI can render a chart without the model transcribing numbers (see
# R/llm_charts.R + the chart_ref mechanism in R/llm_context.R).
#
# NOTE on ECL: the ETL output does NOT contain per-account ECL — LIC
# computes ECL downstream. In AccountMaster_1.csv the ImpairmentAmount /
# OriginalECL* columns are blank by design. The assistant can still compare
# the ECL *drivers* the ETL produces: stage allocation, PD term structures
# (StPD.csv), exposures (OnBalance), ratings, collateral, EAD curves, etc.
# `describe_file` surfaces which columns are populated so the model knows
# what is analysable.
# =============================================================================


# ---- run / file resolution -------------------------------------------------

#' Resolve a run_id (or NULL = latest) to its run directory path.
.tool_run_path <- function(run_id = NULL) {
  runs <- tryCatch(list_runs(), error = function(e) NULL)
  if (is.null(runs) || nrow(runs) == 0) return(NULL)
  if (is.null(run_id) || !nzchar(run_id)) return(runs$path[1])
  m <- runs[runs$run_id == run_id, ]
  if (nrow(m) == 0) return(NULL)
  m$path[1]
}

#' Directory holding a run's output CSVs: Output/ as the engine writes it
#' (output/ in older runs); the run folder itself when it holds the CSVs.
.tool_output_dir <- function(run_path) {
  if (is.null(run_path)) return(NULL)
  for (nm in c("Output", "output")) {
    d <- file.path(run_path, nm)
    if (dir.exists(d)) return(d)
  }
  run_path
}

#' The ECL report the pages read: the OVERLAID one when an overlay has been
#' applied to the run (the first, by name), so the assistant quotes the same
#' provision as the analytics pages. Mirrored in the Python app
#' (backend/assistant/tools.py, _report_file).
.tool_report_file <- function(od) {
  ov <- sort(list.files(od, pattern = "^FinalEclReport_overlay_.*\\.csv$"))
  if (length(ov) > 0) ov[1] else "FinalEclReport.csv"
}

#' " (overlay applied - read <file>, as the pages do)" for a table read from
#' an overlaid report, else "".
.tool_note <- function(d) {
  f <- attr(d, "read_from")
  if (is.null(f) || identical(f, "FinalEclReport.csv")) return("")
  sprintf(" (overlay applied - read %s, as the pages do)", f)
}

#' Read an output CSV for a run as character columns (so blank/typed cells
#' don't surprise us). Cached per-process via a simple environment to avoid
#' re-reading the same file repeatedly within one answer.
.tool_cache <- new.env(parent = emptyenv())
.tool_read <- function(run_id, file) {
  rp <- .tool_run_path(run_id)
  if (is.null(rp)) return(NULL)
  od <- .tool_output_dir(rp)
  # allow the model to pass "AccountMaster_1" or "AccountMaster_1.csv"
  if (!grepl("\\.csv$", file)) file <- paste0(file, ".csv")
  if (tolower(file) == "finaleclreport.csv") file <- .tool_report_file(od)
  fp <- file.path(od, file)
  if (!file.exists(fp)) return(NULL)
  key <- paste0(normalizePath(fp), "::", file.info(fp)$mtime)
  if (!is.null(.tool_cache[[key]])) return(.tool_cache[[key]])
  d <- tryCatch(
    utils::read.csv(fp, stringsAsFactors = FALSE, colClasses = "character",
                    check.names = FALSE, na.strings = c("NA")),
    error = function(e) NULL)
  if (!is.null(d)) attr(d, "read_from") <- file
  .tool_cache[[key]] <- d
  d
}

#' Find a column name case-insensitively / flexibly.
.tool_col <- function(df, name) {
  if (is.null(df) || is.null(name)) return(NA_character_)
  hits <- which(tolower(colnames(df)) == tolower(name))
  if (length(hits) == 0) {
    # loose contains match
    hits <- grep(tolower(name), tolower(colnames(df)), fixed = TRUE)
  }
  if (length(hits) == 0) return(NA_character_)
  colnames(df)[hits[1]]
}

.tool_numeric <- function(x) suppressWarnings(as.numeric(gsub(",", "", x)))


# ---- individual tools ------------------------------------------------------
# Each returns a list with at least `ok` (logical) and `summary` (character,
# what the model reads). Aggregation/comparison tools also include `table`
# (data.frame) and `chart_data` (for optional rendering).

.tool_list_runs <- function(args = list()) {
  runs <- tryCatch(list_runs(), error = function(e) NULL)
  if (is.null(runs) || nrow(runs) == 0)
    return(list(ok = TRUE, summary = "No runs found."))
  runs <- utils::head(runs, 25)
  lines <- sprintf(
    "run_id=%s | started=%s | user=%s | snapshot=%s(%s) | outputs=%s | val_fail=%s",
    runs$run_id, runs$started_at, runs$user, runs$snapshot_label,
    runs$snapshot_status, runs$n_outputs, runs$n_validation_failures)
  list(ok = TRUE, summary = paste(lines, collapse = "\n"))
}

.tool_list_files <- function(args) {
  rp <- .tool_run_path(args$run_id)
  if (is.null(rp)) return(list(ok = FALSE, summary = "Run not found."))
  od <- .tool_output_dir(rp)
  csvs <- list.files(od, pattern = "\\.csv$", full.names = TRUE)
  if (length(csvs) == 0) return(list(ok = TRUE, summary = "No output CSVs."))
  rc <- vapply(csvs, function(f)
    tryCatch(length(readLines(f, warn = FALSE)) - 1L,
             error = function(e) NA_integer_), integer(1))
  list(ok = TRUE,
       summary = paste(sprintf("- %s: %s rows", basename(csvs), rc),
                       collapse = "\n"))
}

.tool_describe_file <- function(args) {
  d <- .tool_read(args$run_id, args$file)
  if (is.null(d)) return(list(ok = FALSE,
                              summary = sprintf("File '%s' not found for run.", args$file)))
  n <- nrow(d)
  cols <- colnames(d)
  pop <- vapply(cols, function(c) sum(nzchar(trimws(d[[c]])), na.rm = TRUE),
                integer(1))
  desc <- sprintf("- %s: %d/%d populated%s", cols, pop, n,
                  ifelse(pop == 0, " (blank)", ""))
  list(ok = TRUE,
       summary = sprintf("File %s%s: %d rows, %d columns.\n%s",
                         args$file, .tool_note(d), n, length(cols),
                         paste(desc, collapse = "\n")))
}

#' Group-by aggregation on one file.
#' args: run_id, file, group_by (string or vector), measure (col or NULL),
#'       fn ("count"|"sum"|"mean"|"min"|"max"|"median")
.tool_aggregate <- function(args) {
  d <- .tool_read(args$run_id, args$file)
  if (is.null(d)) return(list(ok = FALSE,
                              summary = sprintf("File '%s' not found.", args$file)))
  fn <- tolower(args$fn %||% "count")
  gb <- args$group_by
  if (is.null(gb)) gb <- character(0)
  gb <- unlist(gb)
  gcols <- vapply(gb, function(g) .tool_col(d, g), character(1))
  if (any(is.na(gcols)))
    return(list(ok = FALSE,
                summary = sprintf("group_by column(s) not found: %s",
                                  paste(gb[is.na(gcols)], collapse = ", "))))

  grp_key <- if (length(gcols) == 0) rep("all", nrow(d)) else
    do.call(paste, c(lapply(gcols, function(c) {
      v <- d[[c]]; v[is.na(v) | v == ""] <- "(blank)"; v
    }), sep = " | "))

  if (fn == "count") {
    tb <- sort(table(grp_key), decreasing = TRUE)
    res <- data.frame(group = names(tb), value = as.numeric(tb),
                      stringsAsFactors = FALSE)
  } else {
    mcol <- .tool_col(d, args$measure)
    if (is.na(mcol))
      return(list(ok = FALSE,
                  summary = sprintf("measure column '%s' not found.", args$measure)))
    mv <- .tool_numeric(d[[mcol]])
    aggfn <- switch(fn,
                    sum = function(x) sum(x, na.rm = TRUE),
                    mean = function(x) mean(x, na.rm = TRUE),
                    min = function(x) suppressWarnings(min(x, na.rm = TRUE)),
                    max = function(x) suppressWarnings(max(x, na.rm = TRUE)),
                    median = function(x) stats::median(x, na.rm = TRUE),
                    function(x) sum(x, na.rm = TRUE))
    spl <- split(mv, grp_key)
    res <- data.frame(group = names(spl),
                      value = vapply(spl, aggfn, numeric(1)),
                      stringsAsFactors = FALSE)
    res <- res[order(-res$value), ]
  }
  res <- utils::head(res, 50)
  tbl_txt <- paste(c(sprintf("%s\t%s", "group",
                             paste0(fn, if (fn != "count") paste0("(", args$measure, ")") else "")),
                     sprintf("%s\t%s", res$group,
                             formatC(res$value, format = "fg", big.mark = ","))),
                   collapse = "\n")
  label <- if (fn == "count") "count" else sprintf("%s(%s)", fn, args$measure)
  list(ok = TRUE,
       summary = sprintf("Aggregation of %s%s by [%s], %s:\n%s",
                         args$file, .tool_note(d), paste(gb, collapse = ", "),
                         label, tbl_txt),
       table = res,
       chart_data = list(type = "bar",
                         title = sprintf("%s by %s", label, paste(gb, collapse = ", ")),
                         categories = res$group,
                         series = list(list(name = label, values = res$value))))
}

#' Same aggregation on two runs, joined and diffed.
#' args: run_id_a, run_id_b, file, group_by, measure, fn
.tool_compare_runs <- function(args) {
  a <- .tool_aggregate(list(run_id = args$run_id_a, file = args$file,
                            group_by = args$group_by, measure = args$measure,
                            fn = args$fn))
  b <- .tool_aggregate(list(run_id = args$run_id_b, file = args$file,
                            group_by = args$group_by, measure = args$measure,
                            fn = args$fn))
  if (!isTRUE(a$ok)) return(a)
  if (!isTRUE(b$ok)) return(b)
  ta <- a$table; tb <- b$table
  m <- merge(ta, tb, by = "group", all = TRUE, suffixes = c("_A", "_B"))
  m$value_A[is.na(m$value_A)] <- 0
  m$value_B[is.na(m$value_B)] <- 0
  m$diff <- m$value_B - m$value_A
  m$pct_change <- ifelse(m$value_A == 0, NA, 100 * m$diff / m$value_A)
  m <- m[order(-abs(m$diff)), ]
  m <- utils::head(m, 50)

  label <- if (tolower(args$fn %||% "count") == "count") "count" else
    sprintf("%s(%s)", args$fn, args$measure)
  ra <- args$run_id_a %||% "A"; rb <- args$run_id_b %||% "B"
  hdr <- sprintf("group\t%s_A\t%s_B\tdiff\tpct_change", label, label)
  body <- sprintf("%s\t%s\t%s\t%s\t%s",
                  m$group,
                  formatC(m$value_A, format = "fg", big.mark = ","),
                  formatC(m$value_B, format = "fg", big.mark = ","),
                  formatC(m$diff, format = "fg", big.mark = ","),
                  ifelse(is.na(m$pct_change), "n/a",
                         sprintf("%+.1f%%", m$pct_change)))
  list(ok = TRUE,
       summary = sprintf(
         "Compare %s by [%s], %s.\nRun A = %s, Run B = %s.\n%s\n%s",
         args$file, paste(unlist(args$group_by), collapse = ", "), label,
         ra, rb, hdr, paste(body, collapse = "\n")),
       table = m,
       chart_data = list(
         type = "grouped_bar",
         title = sprintf("%s by %s: %s vs %s", label,
                         paste(unlist(args$group_by), collapse = ", "), ra, rb),
         categories = m$group,
         series = list(list(name = paste0("A: ", ra), values = m$value_A),
                       list(name = paste0("B: ", rb), values = m$value_B))))
}

#' Row-level diff of one file across two runs, keyed on a column.
#' args: run_id_a, run_id_b, file, key (column), columns (optional vector to
#'       compare; default all shared columns)
.tool_diff_files <- function(args) {
  da <- .tool_read(args$run_id_a, args$file)
  db <- .tool_read(args$run_id_b, args$file)
  if (is.null(da) || is.null(db))
    return(list(ok = FALSE, summary = "One or both files not found."))
  key_a <- .tool_col(da, args$key); key_b <- .tool_col(db, args$key)
  if (is.na(key_a) || is.na(key_b))
    return(list(ok = FALSE, summary = sprintf("key '%s' not found.", args$key)))

  ka <- da[[key_a]]; kb <- db[[key_b]]
  added   <- setdiff(kb, ka)   # in B not A
  removed <- setdiff(ka, kb)   # in A not B
  common  <- intersect(ka, kb)

  shared_cols <- intersect(colnames(da), colnames(db))
  cmp_cols <- args$columns
  if (is.null(cmp_cols)) cmp_cols <- setdiff(shared_cols, key_a) else
    cmp_cols <- intersect(unlist(cmp_cols), shared_cols)

  # Index by key for common rows (first occurrence)
  ia <- match(common, ka); ib <- match(common, kb)
  changed_by_col <- setNames(integer(length(cmp_cols)), cmp_cols)
  changed_rows <- 0L
  sample_changes <- character(0)
  for (i in seq_along(common)) {
    row_changed <- FALSE
    diffs <- character(0)
    for (cc in cmp_cols) {
      va <- da[[cc]][ia[i]]; vb <- db[[cc]][ib[i]]
      va[is.na(va)] <- ""; vb[is.na(vb)] <- ""
      if (!identical(va, vb)) {
        changed_by_col[cc] <- changed_by_col[cc] + 1L
        row_changed <- TRUE
        if (length(sample_changes) < 15)
          diffs <- c(diffs, sprintf("%s: '%s'->'%s'", cc, va, vb))
      }
    }
    if (row_changed) {
      changed_rows <- changed_rows + 1L
      if (length(sample_changes) < 15 && length(diffs) > 0)
        sample_changes <- c(sample_changes,
                            sprintf("%s=%s: %s", args$key, common[i],
                                    paste(diffs, collapse = "; ")))
    }
  }

  col_lines <- sprintf("- %s: %d changed", names(changed_by_col),
                       as.integer(changed_by_col))
  col_lines <- col_lines[changed_by_col > 0]
  summ <- sprintf(paste0(
    "Row-level diff of %s keyed on %s.\n",
    "Run A rows=%d, Run B rows=%d.\n",
    "Added in B: %d | Removed from A: %d | Common: %d | Common rows changed: %d\n",
    "Per-column changes:\n%s%s"),
    args$file, args$key, nrow(da), nrow(db),
    length(added), length(removed), length(common), changed_rows,
    if (length(col_lines)) paste(col_lines, collapse = "\n") else "(none)",
    if (length(sample_changes))
      paste0("\nSample changes:\n", paste(sample_changes, collapse = "\n")) else "")

  # chart: per-column change counts
  cd <- NULL
  if (any(changed_by_col > 0)) {
    nz <- changed_by_col[changed_by_col > 0]
    nz <- sort(nz, decreasing = TRUE)
    cd <- list(type = "bar",
               title = sprintf("Changed cells per column: %s", args$file),
               categories = names(nz),
               series = list(list(name = "rows changed", values = as.numeric(nz))))
  }
  list(ok = TRUE, summary = summ,
       table = data.frame(metric = c("added_in_B", "removed_from_A",
                                     "common", "common_rows_changed"),
                          value = c(length(added), length(removed),
                                    length(common), changed_rows)),
       chart_data = cd)
}

#' Numeric summary or top-N categorical for one column.
#' args: run_id, file, column, top (for categorical, default 15)
.tool_column_stats <- function(args) {
  d <- .tool_read(args$run_id, args$file)
  if (is.null(d)) return(list(ok = FALSE,
                              summary = sprintf("File '%s' not found.", args$file)))
  col <- .tool_col(d, args$column)
  if (is.na(col)) return(list(ok = FALSE,
                              summary = sprintf("column '%s' not found.", args$column)))
  raw <- d[[col]]
  num <- .tool_numeric(raw)
  numeric_frac <- mean(!is.na(num) & nzchar(trimws(raw)), na.rm = TRUE)
  if (isTRUE(numeric_frac > 0.8)) {
    v <- num[!is.na(num)]
    if (length(v) == 0) return(list(ok = TRUE,
                                    summary = sprintf("Column %s is all blank.", col)))
    qs <- stats::quantile(v, c(0, .25, .5, .75, 1), na.rm = TRUE)
    list(ok = TRUE,
         summary = sprintf(paste0("Numeric stats for %s.%s%s:\n",
                                  "n=%d, sum=%s, mean=%s, sd=%s\n",
                                  "min=%s, p25=%s, median=%s, p75=%s, max=%s"),
                           args$file, col, .tool_note(d), length(v),
                           formatC(sum(v), format = "fg", big.mark = ","),
                           formatC(mean(v), format = "fg", big.mark = ","),
                           formatC(stats::sd(v), format = "fg", big.mark = ","),
                           formatC(qs[1], format = "fg", big.mark = ","),
                           formatC(qs[2], format = "fg", big.mark = ","),
                           formatC(qs[3], format = "fg", big.mark = ","),
                           formatC(qs[4], format = "fg", big.mark = ","),
                           formatC(qs[5], format = "fg", big.mark = ",")))
  } else {
    topn <- as.integer(args$top %||% 15L)
    val <- raw; val[is.na(val) | val == ""] <- "(blank)"
    tb <- sort(table(val), decreasing = TRUE)
    tb <- utils::head(tb, topn)
    list(ok = TRUE,
         summary = sprintf("Top %d values for %s.%s%s:\n%s", topn, args$file, col,
                           .tool_note(d),
                           paste(sprintf("- %s: %d", names(tb), as.integer(tb)),
                                 collapse = "\n")),
         table = data.frame(value = names(tb), count = as.integer(tb)),
         chart_data = list(type = "bar",
                           title = sprintf("%s distribution", col),
                           categories = names(tb),
                           series = list(list(name = "count",
                                              values = as.numeric(tb)))))
  }
}

#' Pull specific rows. args: run_id, file, where (col=value, optional),
#' columns (optional), limit (default 20).
.tool_filter_rows <- function(args) {
  d <- .tool_read(args$run_id, args$file)
  if (is.null(d)) return(list(ok = FALSE,
                              summary = sprintf("File '%s' not found.", args$file)))
  lim <- as.integer(args$limit %||% 20L)
  sel <- rep(TRUE, nrow(d))
  if (!is.null(args$where) && length(args$where) > 0) {
    for (nm in names(args$where)) {
      col <- .tool_col(d, nm)
      if (is.na(col)) next
      sel <- sel & (tolower(trimws(d[[col]])) == tolower(as.character(args$where[[nm]])))
    }
  }
  sub <- d[sel, , drop = FALSE]
  if (!is.null(args$columns)) {
    cc <- vapply(unlist(args$columns), function(c) .tool_col(d, c), character(1))
    cc <- cc[!is.na(cc)]
    if (length(cc)) sub <- sub[, cc, drop = FALSE]
  }
  n_match <- nrow(sub)
  sub <- utils::head(sub, lim)
  # compact text rendering
  hdr <- paste(colnames(sub), collapse = "\t")
  body <- apply(sub, 1, function(r) paste(r, collapse = "\t"))
  list(ok = TRUE,
       summary = sprintf("Matched %d rows (showing %d)%s:\n%s\n%s",
                         n_match, nrow(sub), .tool_note(d), hdr,
                         paste(body, collapse = "\n")),
       table = sub)
}


#' Full model specification from the config files (NO run needed). Reads
#' config.yml (active model selection), config/models.yml (per-MEV intercept,
#' coefficient, p_value, std dev, weight, anchor PD, horizons), and
#' config/model_config.yml (macro model name + MEVs). args: model (optional
#' model name; default = both active internal + external models).
.tool_model_spec <- function(args = list()) {
  root <- getOption("ifrs9.project_root", ".")
  rd <- function(p) {
    fp <- file.path(root, p)
    if (file.exists(fp)) tryCatch(yaml::read_yaml(fp), error = function(e) NULL) else NULL
  }
  cfg    <- rd("config.yml")
  models <- rd("config/model.yml")
  if (is.null(models)) models <- rd("config/models.yml")   # legacy snapshots
  mcfg   <- rd("config/model_config.yml")                   # legacy; NULL-safe
  if (is.null(models)) return(list(ok = FALSE,
    summary = "config/model.yml not found."))

  active_int <- cfg$run$internal_model %||% NA
  active_ext <- cfg$run$external_model %||% NA

  want <- args$model
  if (is.null(want) || !nzchar(want)) {
    want <- unique(stats::na.omit(c(active_int, active_ext)))
  }

  fmt_model <- function(nm) {
    m <- models$models[[nm]]
    if (is.null(m)) return(sprintf("Model '%s' not found in models.yml.", nm))
    active_flag <- if (identical(nm, active_int)) " [ACTIVE internal]"
                   else if (identical(nm, active_ext)) " [ACTIVE external]" else ""
    hdr <- sprintf("### Model: %s%s\n%s\nrating_type=%s; portfolios=%s",
                   nm, active_flag,
                   trimws(m$description %||% ""),
                   m$rating_type %||% "?",
                   paste(unlist(m$portfolios %||% list()), collapse = ", "))
    comps <- m$mev_components %||% list()
    if (length(comps) == 0) return(paste0(hdr, "\n(no mev_components)"))
    rows <- vapply(comps, function(c) {
      sprintf("%s\t%s\t%s\t%s\t%s\t%s",
              c$variable %||% "?",
              .fmt_num(c$intercept), .fmt_num(c$coefficient),
              .fmt_num(c$p_value), .fmt_num(c$standard_deviation),
              if (is.null(c$weight)) "null(derive from p)" else .fmt_num(c$weight))
    }, character(1))
    tbl <- paste0("variable\tintercept\tcoefficient\tp_value\tstd_dev\tweight\n",
                  paste(rows, collapse = "\n"))
    cal <- m$calibration %||% list()
    paste0(hdr, "\nMEV components:\n", tbl,
           sprintf("\ncalibration source: %s (reviewed %s)",
                   cal$source %||% "?", cal$date_reviewed %||% "?"))
  }

  parts <- vapply(want, fmt_model, character(1))

  macro <- ""
  if (!is.null(mcfg$model)) {
    macro <- sprintf("\n### Macro model (config/model_config.yml)\nname: %s",
                     mcfg$model$name %||% "?")
    if (!is.null(mcfg$model$mevs)) {
      mn <- vapply(mcfg$model$mevs, function(x) x$name %||% "?", character(1))
      macro <- paste0(macro, "\nMEVs: ", paste(mn, collapse = "; "))
    }
  }
  anchor <- sprintf("\nttc_anchor_pd = %s; max_maturity = %s; n_forecasts = %s",
                    models$ttc_anchor_pd %||% "?",
                    models$horizons$max_maturity %||% "?",
                    models$horizons$n_forecasts %||% "?")

  list(ok = TRUE,
       summary = paste0(
         sprintf("Active models: internal=%s, external=%s.\n", active_int, active_ext),
         paste(parts, collapse = "\n\n"), "\n", macro, anchor))
}

#' Validation results for a run. Reads reports/validation.csv (the correct
#' filename — there is no 'validation_failures.csv'). args: run_id (optional),
#' only_failures (logical, default TRUE).
.tool_validation_results <- function(args = list()) {
  rp <- .tool_run_path(args$run_id)
  if (is.null(rp)) return(list(ok = FALSE, summary = "Run not found."))
  vp <- file.path(rp, "reports", "validation.csv")
  if (!file.exists(vp)) {
    v <- tryCatch(read_run_validation(rp), error = function(e) NULL)
    if (is.null(v)) return(list(ok = FALSE,
      summary = "reports/validation.csv not found for this run."))
  } else {
    v <- tryCatch(utils::read.csv(vp, stringsAsFactors = FALSE, na.strings = ""),
                  error = function(e) NULL)
  }
  if (is.null(v) || nrow(v) == 0)
    return(list(ok = TRUE, summary = "validation.csv is empty."))

  passed <- as.logical(v$passed)
  sev_col <- if ("effective_severity" %in% names(v)) "effective_severity" else "severity"
  n_pass <- sum(passed, na.rm = TRUE)
  n_fail <- sum(!passed, na.rm = TRUE)
  by_sev <- table(v[[sev_col]][!passed])

  only_fail <- isTRUE(args$only_failures %||% TRUE)
  rows <- if (only_fail) v[!passed, , drop = FALSE] else v
  rows <- utils::head(rows, 40)

  det_col <- intersect(c("details", "description"), names(rows))[1]
  lines <- vapply(seq_len(nrow(rows)), function(i) {
    sprintf("- [%s] %s | %s | %s",
            rows[[sev_col]][i] %||% "?",
            rows$id[i] %||% "?",
            rows$context[i] %||% "",
            .llm_trim(as.character(rows[[det_col]][i] %||% rows$description[i] %||% ""), 200))
  }, character(1))

  sev_txt <- paste(sprintf("%s=%d", names(by_sev), as.integer(by_sev)),
                   collapse = ", ")
  list(ok = TRUE,
       summary = sprintf(
         "Validation for run %s: %d passed, %d failed (failed by severity: %s).\nFailing checks:\n%s",
         basename(rp), n_pass, n_fail,
         if (nzchar(sev_txt)) sev_txt else "none",
         paste(lines, collapse = "\n")))
}

.fmt_num <- function(x) {
  if (is.null(x) || length(x) == 0) return("?")
  if (is.na(suppressWarnings(as.numeric(x)))) return(as.character(x))
  formatC(as.numeric(x), format = "g", digits = 10)
}


# ---- registry + executor ---------------------------------------------------

#' Human-readable tool catalogue injected into the system prompt.
llm_tool_catalogue <- function() {
  paste(
    "model_spec(model) -> FULL model specification from config files (no run needed): per-MEV intercept, coefficient, p_value, standard_deviation, weight; TTC anchor PD; horizons; macro model name. `model` optional (defaults to the active internal + external models). USE THIS for any question about model coefficients/intercepts/weights/specification.",
    "validation_results(run_id, only_failures) -> reads reports/validation.csv for a run (the correct file; there is NO validation_failures.csv): pass/fail counts, failures by severity, and the failing checks with details. only_failures defaults to true.",
    "list_runs() -> list all runs with id, date, user, snapshot, validation failures.",
    "list_files(run_id) -> output CSV files for a run and their row counts.",
    "describe_file(run_id, file) -> columns of a file, with how many rows are populated vs blank.",
    "aggregate(run_id, file, group_by, measure, fn) -> group-by aggregation. fn in {count,sum,mean,min,max,median}. For count, measure may be omitted. group_by is one column or a list of columns.",
    "compare_runs(run_id_a, run_id_b, file, group_by, measure, fn) -> same aggregation on two runs, joined with diff and pct_change. Use for quarter-over-quarter comparisons.",
    "diff_files(run_id_a, run_id_b, file, key, columns) -> row-level diff keyed on `key`: added/removed rows and per-column change counts. `columns` optional (defaults to all shared columns).",
    "column_stats(run_id, file, column, top) -> numeric summary (if numeric) or top-N value counts (if categorical).",
    "filter_rows(run_id, file, where, columns, limit) -> rows matching `where` (a map of column=value). `columns` and `limit` optional.",
    sep = "\n")
}

#' Execute a tool by name with args (a list). Returns the tool's result list.
llm_execute_tool <- function(name, args = list()) {
  args <- args %||% list()
  res <- tryCatch(
    switch(name,
           model_spec        = .tool_model_spec(args),
           validation_results= .tool_validation_results(args),
           list_runs     = .tool_list_runs(args),
           list_files    = .tool_list_files(args),
           describe_file = .tool_describe_file(args),
           aggregate     = .tool_aggregate(args),
           compare_runs  = .tool_compare_runs(args),
           diff_files    = .tool_diff_files(args),
           column_stats  = .tool_column_stats(args),
           filter_rows   = .tool_filter_rows(args),
           list(ok = FALSE, summary = sprintf("Unknown tool '%s'.", name))),
    error = function(e) list(ok = FALSE,
                             summary = sprintf("Tool '%s' errored: %s",
                                               name, conditionMessage(e))))
  res
}

if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
