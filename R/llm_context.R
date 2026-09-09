# =============================================================================
# R/llm_context.R
#
# Builds the grounding context for the in-app assistant. The assistant is a
# general chat model with NO knowledge of this specific deployment, so before
# every question we assemble a compact, factual "knowledge pack" from the
# app's own artifacts and inject it as a system message. This keeps answers
# grounded in real run data / config / methodology instead of hallucinated.
#
# Design: deterministic, rule-based retrieval (no embeddings). A lightweight
# intent classifier decides which context blocks to include so we stay within
# the model's context window. Every block is built from files already on disk:
#   - run history / manifests / validation   (run_discovery.R helpers)
#   - config.yml + config/*.yml               (yaml)
#   - methodology notes                       (NOTES.md, grep by keyword)
#   - data statistics                         (computed from output CSVs)
#   - static reference                         (data-raw/static/*.csv)
#
# The orchestrator `assistant_answer()` ties it together: classify -> gather
# context -> call llm_chat() -> return reply.
# =============================================================================


# ---------------------------------------------------------------------------
# Intent classification (keyword-based, cheap, transparent)
# ---------------------------------------------------------------------------

#' Classify a user question into one or more context categories.
#' Returns a character vector of intents (always includes "overview").
.classify_intent <- function(question) {
  q <- tolower(question %||% "")
  has <- function(...) any(vapply(c(...), function(p) grepl(p, q, fixed = TRUE),
                                  logical(1)))
  intents <- "overview"

  if (has("run", "ran", "executed", "history", "when", "last run",
          "previous", "latest", "duration", "how long"))
    intents <- c(intents, "runs")

  if (has("approv", "who ", "sign", "reject", "pending", "queue",
          "snapshot", "promoted", "governance"))
    intents <- c(intents, "approvals")

  if (has("config", "setting", "which model", "pd model", "macro",
          "scenario weight", "threshold", "parameter", "mev",
          "internal_model", "external_model", "validation_error",
          "extract date", "anchor", "horizon"))
    intents <- c(intents, "config")

  if (has("methodology", "how do", "how is", "how does", "formula",
          "survival", "marginal", "cumulative", "lgd", "ecl", "pd ",
          "stage", "sicr", "haircut", "vasicek", "eir", "why ",
          "collateral", "allocation", "unwind", "discount"))
    intents <- c(intents, "methodology")

  if (has("how many", "count", "average", "mean", "distribution",
          "stats", "statistic", "row", "chart", "table", "analytic",
          "breakdown", "by portfolio", "by rating", "by stage",
          "total", "sum", "min", "max", "top ", "largest", "smallest",
          "exposure", "intermediate"))
    intents <- c(intents, "data")

  if (has("validation", "fail", "error", "duplicate", "warning",
          "passed", "check"))
    intents <- c(intents, "validation")

  if (has("portfolio", "rating scale", "collateral type", "product",
          "reference", "fx", "currency"))
    intents <- c(intents, "static")

  unique(intents)
}


# ---------------------------------------------------------------------------
# Context blocks
# ---------------------------------------------------------------------------

#' A short, stable description of what this app/pipeline is. Always included
#' so the model knows the domain even for a bare "hi".
.ctx_overview <- function() {
  paste0(
    "## About this application\n",
    "This is the QDB IFRS 9 ETL pipeline and its Shiny control app. The ",
    "pipeline reads 12 input files (lending + investment portfolios, ",
    "collateral, customer master, staging flags, origination, repayment ",
    "schedule), transforms them, and writes 18 output CSVs consumed by the ",
    "LIC (Loss Impairment Calculator) engine to produce ECL.\n",
    "Six portfolios: Business Finance, Off BS, Al Dhameen, Tasdeer ",
    "(internally rated) and Investments, Banks and Fis (externally rated). ",
    "Three IFRS 9 stages. PD term structures are built per (portfolio, ",
    "rating, month) and written to StPD.csv. The app lets operators run the ",
    "pipeline, review validation findings, manage config snapshots, and ",
    "approve runs.\n"
  )
}


#' Compact table of all runs (newest first). Bounded to ~15 rows.
.ctx_runs <- function() {
  runs <- tryCatch(list_runs(), error = function(e) NULL)
  if (is.null(runs) || nrow(runs) == 0) {
    return("## Run history\n(No runs found in the runs directory.)\n")
  }
  runs <- utils::head(runs, 15)
  lines <- vapply(seq_len(nrow(runs)), function(i) {
    r <- runs[i, ]
    sprintf(
      "- run_id=%s | started=%s | user=%s | snapshot=%s (%s) | outputs=%s | validation_failures=%s | code=%s",
      r$run_id, r$started_at %||% "?", r$user %||% "?",
      r$snapshot_label %||% "?", r$snapshot_status %||% "?",
      r$n_outputs %||% 0, r$n_validation_failures %||% 0,
      substr(r$code_sha %||% "?", 1, 8))
  }, character(1))
  paste0("## Run history (newest first, up to 15)\n",
         paste(lines, collapse = "\n"), "\n")
}


#' Detail for one run: who approved, validation summary, outputs,
#' reconciliation. `run_id` may be NULL (then use the latest run).
.ctx_run_detail <- function(run_id = NULL) {
  runs <- tryCatch(list_runs(), error = function(e) NULL)
  if (is.null(runs) || nrow(runs) == 0) return("")
  row <- if (is.null(run_id)) runs[1, ] else {
    m <- runs[runs$run_id == run_id, ]
    if (nrow(m) == 0) runs[1, ] else m[1, ]
  }
  man <- tryCatch(read_run_manifest(row$path), error = function(e) NULL)
  val <- tryCatch(read_run_validation(row$path), error = function(e) NULL)

  out <- sprintf("## Detail for run %s\n", row$run_id)
  out <- paste0(out, sprintf("- started: %s\n- finished: %s\n- user: %s\n",
                             row$started_at %||% "?",
                             row$finished_at %||% "?",
                             row$user %||% "?"))
  # Operator-supplied run metadata (type/purpose/portfolio date/calculator)
  if (!is.null(man)) {
    rmeta <- man$run_metadata %||% list()
    if (length(rmeta) > 0) {
      out <- paste0(out, sprintf(
        "- run_type: %s\n- run_purpose: %s\n- portfolio_date: %s\n- config_version: %s\n- calculator_version: %s%s\n",
        rmeta$run_type %||% "?", rmeta$run_purpose %||% "?",
        rmeta$portfolio_date %||% "?", rmeta$config_version %||% "?",
        rmeta$calculator_label %||% rmeta$calculator_version %||% "?",
        if (isFALSE(rmeta$calculator_matches_registered))
          " (WARNING: deployed code differed from registered fingerprint)" else ""))
    }
  }
  # Approval / snapshot governance
  if (!is.null(man)) {
    snap <- man$snapshot %||% list()
    out <- paste0(out, sprintf(
      "- snapshot label: %s\n- snapshot status: %s\n- approved_by: %s\n- approved_at: %s\n- created_by: %s\n",
      snap$label %||% "?", snap$status %||% "?",
      snap$approved_by %||% "(not recorded)",
      snap$approved_at %||% "(not recorded)",
      snap$created_by %||% "(not recorded)"))
    mdl <- man$models %||% man$config$run %||% list()
    if (length(mdl) > 0) {
      out <- paste0(out, sprintf("- internal_model: %s\n- external_model: %s\n",
                                 mdl$internal_model %||% "?",
                                 mdl$external_model %||% "?"))
    }
  }
  # Validation summary
  if (!is.null(val) && nrow(val) > 0) {
    n_fail <- sum(!as.logical(val$passed), na.rm = TRUE)
    n_pass <- sum(as.logical(val$passed), na.rm = TRUE)
    out <- paste0(out, sprintf("- validation: %d passed, %d failed\n",
                               n_pass, n_fail))
    if (n_fail > 0) {
      fails <- val[!as.logical(val$passed), ]
      fl <- utils::head(fails, 12)
      msgs <- vapply(seq_len(nrow(fl)), function(i) {
        sprintf("    * [%s] %s: %s",
                fl$severity[i] %||% "?",
                fl$id[i] %||% "?",
                .llm_trim(fl$message[i] %||% fl$description[i] %||% "", 160))
      }, character(1))
      out <- paste0(out, "  failed checks:\n", paste(msgs, collapse = "\n"), "\n")
    }
  }
  out
}


#' Current configuration summary: which models, scenario-weighting modes,
#' validation gate, key horizons, anchor PD.
.ctx_config <- function() {
  root <- getOption("ifrs9.project_root", ".")
  rd <- function(p) {
    fp <- file.path(root, p)
    if (file.exists(fp)) tryCatch(yaml::read_yaml(fp), error = function(e) NULL)
    else NULL
  }
  cfg   <- rd("config.yml")
  models<- rd("config/models.yml")
  mcfg  <- rd("config/model_config.yml")
  minp  <- rd("config/model_inputs.yml")

  out <- "## Current configuration\n"
  if (!is.null(cfg$run)) {
    out <- paste0(out, sprintf(
      "- internal_model: %s\n- external_model: %s\n- on_validation_error: %s\n- extract_date: %s\n",
      cfg$run$internal_model %||% "?",
      cfg$run$external_model %||% "?",
      cfg$run$on_validation_error %||% "?",
      cfg$run$extract_date %||% "(auto-detect)"))
  }
  if (!is.null(mcfg$model)) {
    out <- paste0(out, sprintf("- macro model: %s\n",
                               mcfg$model$name %||% "?"))
    if (!is.null(mcfg$model$mevs)) {
      mev_names <- vapply(mcfg$model$mevs, function(m) m$name %||% "?", character(1))
      out <- paste0(out, sprintf("- MEVs configured: %s\n",
                                 paste(mev_names, collapse = "; ")))
    }
  }
  if (!is.null(models$ttc_anchor_pd)) {
    out <- paste0(out, sprintf("- TTC anchor PD: %s\n", models$ttc_anchor_pd))
  }
  if (!is.null(models$horizons)) {
    out <- paste0(out, sprintf("- max_maturity: %s years; n_forecasts: %s years\n",
                               models$horizons$max_maturity %||% "?",
                               models$horizons$n_forecasts %||% "?"))
  }
  if (!is.null(minp$internal_scenario_weights)) {
    out <- paste0(out, sprintf("- internal scenario weights mode: %s\n",
                               minp$internal_scenario_weights$mode %||% "?"))
  }
  if (!is.null(minp$external_scenario_weights)) {
    out <- paste0(out, sprintf("- external scenario weights mode: %s\n",
                               minp$external_scenario_weights$mode %||% "?"))
  }

  # Inline the active models' MEV coefficients so even a question that
  # doesn't trigger the model_spec tool still has the specification at hand.
  if (!is.null(models$models)) {
    active <- unique(stats::na.omit(c(cfg$run$internal_model,
                                      cfg$run$external_model)))
    for (nm in active) {
      m <- models$models[[nm]]
      if (is.null(m)) next
      comps <- m$mev_components %||% list()
      if (length(comps) == 0) next
      rows <- vapply(comps, function(c)
        sprintf("    %s: intercept=%s, coef=%s, p=%s, sd=%s, weight=%s",
                c$variable %||% "?",
                c$intercept %||% "?", c$coefficient %||% "?",
                c$p_value %||% "?", c$standard_deviation %||% "?",
                if (is.null(c$weight)) "null" else c$weight),
        character(1))
      out <- paste0(out, sprintf("- model '%s' MEV components:\n%s\n",
                                 nm, paste(rows, collapse = "\n")))
    }
  }
  out
}


#' Methodology context: pull the most relevant chunks from NOTES.md by
#' keyword overlap with the question. NOTES.md is section-delimited by
#' lines starting with "## ". We score each section by keyword hits and
#' include the top few, bounded by characters.
.ctx_methodology <- function(question, max_chars = 6000) {
  root <- getOption("ifrs9.project_root", ".")
  np <- file.path(root, "NOTES.md")
  if (!file.exists(np)) return("")
  txt <- tryCatch(readLines(np, warn = FALSE), error = function(e) character())
  if (length(txt) == 0) return("")

  # Split into sections at "## " headers
  hdr_idx <- grep("^## ", txt)
  if (length(hdr_idx) == 0) {
    return(paste0("## Methodology notes (excerpt)\n",
                  paste(utils::head(txt, 80), collapse = "\n"), "\n"))
  }
  starts <- hdr_idx
  ends   <- c(hdr_idx[-1] - 1, length(txt))
  sections <- Map(function(s, e) paste(txt[s:e], collapse = "\n"), starts, ends)

  # Score by keyword overlap
  q_words <- unique(strsplit(tolower(gsub("[^a-z0-9 ]", " ", question %||% "")),
                             "\\s+")[[1]])
  q_words <- q_words[nchar(q_words) >= 4]
  score_section <- function(sec) {
    sl <- tolower(sec)
    sum(vapply(q_words, function(w) length(gregexpr(w, sl, fixed = TRUE)[[1]]),
               integer(1)))
  }
  scores <- vapply(sections, score_section, numeric(1))
  ord <- order(scores, decreasing = TRUE)

  picked <- character(0)
  total <- 0
  for (i in ord) {
    if (scores[i] == 0) break
    sec <- sections[[i]]
    if (total + nchar(sec) > max_chars) next
    picked <- c(picked, sec)
    total <- total + nchar(sec)
    if (length(picked) >= 4) break
  }
  if (length(picked) == 0) return("")
  paste0("## Relevant methodology notes (from NOTES.md)\n",
         paste(picked, collapse = "\n\n"), "\n")
}


#' Data statistics from a run's output CSVs. Computes compact, decision-useful
#' summaries: row counts per output, AccountMaster_1 breakdown by portfolio /
#' stage, StPD coverage, etc. Bounded output.
.ctx_data <- function(run_id = NULL) {
  runs <- tryCatch(list_runs(), error = function(e) NULL)
  if (is.null(runs) || nrow(runs) == 0) return("")
  row <- if (is.null(run_id)) runs[1, ] else {
    m <- runs[runs$run_id == run_id, ]; if (nrow(m) == 0) runs[1, ] else m[1, ]
  }
  out_dir <- file.path(row$path, "output")
  if (!dir.exists(out_dir)) out_dir <- row$path
  csvs <- list.files(out_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(csvs) == 0) return("")

  out <- sprintf("## Data statistics for run %s\n", row$run_id)

  # Row counts per file
  rc <- vapply(csvs, function(f) {
    n <- tryCatch(length(readLines(f, warn = FALSE)) - 1L,
                  error = function(e) NA_integer_)
    n
  }, integer(1))
  out <- paste0(out, "Output row counts:\n",
                paste(sprintf("- %s: %s rows", basename(csvs), rc),
                      collapse = "\n"), "\n")

  # AccountMaster_1 breakdowns
  am <- file.path(out_dir, "AccountMaster_1.csv")
  if (file.exists(am)) {
    d <- tryCatch(utils::read.csv(am, stringsAsFactors = FALSE,
                                  colClasses = "character"),
                  error = function(e) NULL)
    if (!is.null(d) && nrow(d) > 0) {
      if ("AccountType" %in% names(d)) {
        tb <- sort(table(d$AccountType), decreasing = TRUE)
        out <- paste0(out, "\nAccountMaster_1 by AccountType:\n",
                      paste(sprintf("- %s: %d", names(tb), as.integer(tb)),
                            collapse = "\n"), "\n")
      }
      if ("Stage" %in% names(d)) {
        st <- d$Stage; st[st == ""] <- "(blank)"
        tb <- sort(table(st), decreasing = TRUE)
        out <- paste0(out, "\nAccountMaster_1 by Stage:\n",
                      paste(sprintf("- Stage %s: %d", names(tb), as.integer(tb)),
                            collapse = "\n"), "\n")
      }
      if ("Rating" %in% names(d)) {
        rt <- d$Rating; rt[rt == ""] <- "(blank)"
        tb <- sort(table(rt), decreasing = TRUE)
        tb <- utils::head(tb, 15)
        out <- paste0(out, "\nAccountMaster_1 by Rating (top 15):\n",
                      paste(sprintf("- %s: %d", names(tb), as.integer(tb)),
                            collapse = "\n"), "\n")
      }
      if ("OnBalance" %in% names(d)) {
        ob <- suppressWarnings(as.numeric(d$OnBalance))
        ob <- ob[!is.na(ob)]
        if (length(ob) > 0) {
          out <- paste0(out, sprintf(
            "\nOnBalance: total=%.0f, mean=%.0f, max=%.0f, n=%d\n",
            sum(ob), mean(ob), max(ob), length(ob)))
        }
      }
    }
  }

  # StPD coverage
  stpd <- file.path(out_dir, "StPD.csv")
  if (file.exists(stpd)) {
    d <- tryCatch(utils::read.csv(stpd, stringsAsFactors = FALSE,
                                  colClasses = "character"),
                  error = function(e) NULL)
    if (!is.null(d) && nrow(d) > 0) {
      pc <- intersect(c("PortfolioCode", "Portfolio"), names(d))
      if (length(pc) > 0) {
        tb <- sort(table(d[[pc[1]]]), decreasing = TRUE)
        out <- paste0(out, "\nStPD rows by portfolio:\n",
                      paste(sprintf("- %s: %d", names(tb), as.integer(tb)),
                            collapse = "\n"), "\n")
      }
    }
  }
  out
}


#' Static reference: portfolios, rating scale, collateral haircuts,
#' product->portfolio mapping. Compact.
.ctx_static <- function(max_chars = 4000) {
  root <- getOption("ifrs9.project_root", ".")
  sdir <- file.path(root, "data-raw", "static")
  if (!dir.exists(sdir)) return("")
  want <- c("portfolios.csv", "master_rating_scale.csv",
            "collateral_types.csv", "product_portfolio_mapping.csv",
            "segment_fallback_ratings.csv", "collective_assessment_rules.csv")
  out <- "## Static reference tables\n"
  for (f in want) {
    fp <- file.path(sdir, f)
    if (!file.exists(fp)) next
    lines <- tryCatch(readLines(fp, warn = FALSE), error = function(e) character())
    lines <- lines[!grepl("^#", lines)]          # drop comment header
    lines <- utils::head(lines, 30)
    chunk <- sprintf("\n### %s\n%s\n", f, paste(lines, collapse = "\n"))
    if (nchar(out) + nchar(chunk) > max_chars) break
    out <- paste0(out, chunk)
  }
  out
}


# ---------------------------------------------------------------------------
# Orchestrator
# ---------------------------------------------------------------------------

#' Assemble the full grounding context for a question.
#' @param question  the user's latest message
#' @param run_id    optional run to focus on (selected in the UI)
#' @param cfg       assistant config (for max_context_chars)
build_chat_context <- function(question, run_id = NULL, cfg = NULL) {
  if (is.null(cfg)) cfg <- llm_assistant_config()
  intents <- .classify_intent(question)

  blocks <- list(.ctx_overview())
  # Always give a compact run list + config (small, broadly useful)
  blocks <- c(blocks, list(.ctx_runs()), list(.ctx_config()))

  if (any(c("runs", "approvals", "validation", "data") %in% intents)) {
    blocks <- c(blocks, list(.ctx_run_detail(run_id)))
  }
  if ("data" %in% intents) {
    blocks <- c(blocks, list(.ctx_data(run_id)))
  }
  if ("methodology" %in% intents) {
    blocks <- c(blocks, list(.ctx_methodology(question)))
  }
  if ("static" %in% intents) {
    blocks <- c(blocks, list(.ctx_static()))
  }

  blocks <- Filter(function(b) is.character(b) && nzchar(b), blocks)
  ctx <- paste(unlist(blocks), collapse = "\n\n")

  # Hard cap on context size
  if (nchar(ctx) > cfg$max_context_chars) {
    ctx <- paste0(substr(ctx, 1, cfg$max_context_chars),
                  "\n\n[context truncated to fit the model window]")
  }
  ctx
}


# ---------------------------------------------------------------------------
# Agentic orchestrator (tool-calling loop)
# ---------------------------------------------------------------------------

#' Compact seed context always given to the model: overview + run list +
#' config. The model fetches anything deeper (data, file schemas, diffs)
#' on demand via tools, so this stays small.
.seed_context <- function(run_id = NULL, cfg = NULL) {
  if (is.null(cfg)) cfg <- llm_assistant_config()
  blocks <- list(.ctx_overview(), .ctx_runs(), .ctx_config())
  blocks <- Filter(function(b) is.character(b) && nzchar(b), blocks)
  ctx <- paste(unlist(blocks), collapse = "\n\n")
  if (nchar(ctx) > cfg$max_context_chars) {
    ctx <- paste0(substr(ctx, 1, cfg$max_context_chars), "\n\n[context truncated]")
  }
  ctx
}

#' System prompt for the agentic assistant.
.assistant_system_prompt <- function(seed, run_id = NULL) {
  focus <- if (is.null(run_id) || !nzchar(run_id)) "the latest run" else
    sprintf("run '%s'", run_id)
  paste0(
    "You are the QDB IFRS 9 ETL Assistant, embedded in the pipeline's ",
    "control app. You help analysts and auditors with questions about runs, ",
    "configuration, governance (approvals), the credit-risk methodology, and ",
    "ANALYTICS over the run output files (stages, ratings, exposures, PD term ",
    "structures, collateral, etc.), including comparing one run to another ",
    "(e.g. quarter over quarter).\n\n",
    "You can call TOOLS to fetch real data. Available tools:\n",
    llm_tool_catalogue(), "\n\n",
    "PROTOCOL: every message you send MUST be a single JSON object and nothing ",
    "else (no prose outside the JSON, no markdown fences):\n",
    "  To call a tool: {\"action\":\"tool\",\"tool\":\"<name>\",\"args\":{...}}\n",
    "  To answer:      {\"action\":\"final\",\"answer\":\"<markdown>\",\"chart_ref\":\"<tool_id or null>\"}\n",
    "After each tool call you receive an OBSERVATION with the result and a ",
    "tool_id (e.g. t1). Chain as many tool calls as you need, then answer. ",
    "To display a chart, set chart_ref to the tool_id of an aggregate / ",
    "compare_runs / diff_files / column_stats observation you want plotted ",
    "(the app renders it from the exact tool data, so do NOT retype numbers ",
    "into a chart). If no chart applies, use null.\n",
    "Emit STRICT, VALID JSON: do NOT backslash-escape underscores or other ",
    "characters inside string values (write 'VAR_NON_OIL_GDP_GROWTH', never ",
    "'VAR\\_NON...'); only use standard JSON escapes (\\n, \\\", \\\\). Put any ",
    "markdown formatting inside the 'answer' string, not around the JSON.\n\n",
    "RULES:\n",
    "1. Ground every factual claim in tool observations or the context. NEVER ",
    "invent run IDs, numbers, names, dates, or approvals. If data is missing, ",
    "say so and name the page/file that would have it.\n",
    "2. CONFIG & MODEL SPECS ARE NOT IN RUNS. For any question about the model ",
    "specification (intercept, coefficients, p-values, weights, MEVs, anchor ",
    "PD, horizons, which model is active), call model_spec \u2014 it reads the ",
    "config files directly and needs NO run. Never say a run is required for ",
    "config/model questions, and never say coefficients aren't available ",
    "without calling model_spec first.\n",
    "3. VALIDATION: to report a run's validation failures, call ",
    "validation_results (it reads reports/validation.csv). There is NO file ",
    "called 'validation_failures.csv' \u2014 do not look for or mention one.\n",
    "4. Put data in compact markdown tables in your final answer. Lead with the ",
    "direct answer, then detail.\n",
    "5. Default focus is ", focus, " when a question needs a run but doesn't ",
    "name one. For comparisons, pick the two relevant runs from the run list.\n",
    "6. ECL: the ETL output does NOT contain per-account ECL (LIC computes it ",
    "downstream; ImpairmentAmount/OriginalECL* are blank by design). For 'ECL' ",
    "questions, compare the ECL DRIVERS the ETL produces (stage allocation, ",
    "StPD PD term structures, OnBalance exposures, ratings, collateral/EAD) and ",
    "state clearly that final ECL comes from LIC.\n",
    "7. You are read-only: you cannot trigger runs, edit config, or approve. ",
    "If asked to act, name the app page that does it.\n\n",
    "CONTEXT:\n", seed)
}

#' Extract the first well-formed JSON object from a model response. Handles
#' code fences, surrounding prose, AND invalid backslash escapes that small
#' models often emit (e.g. markdown-style "\_" inside JSON strings, which is
#' not a legal JSON escape and would otherwise make the whole object
#' unparseable). Returns a parsed list or NULL.
.extract_json <- function(text) {
  if (is.null(text) || !nzchar(text)) return(NULL)
  s <- gsub("```json", "", text, fixed = TRUE)
  s <- gsub("```", "", s, fixed = TRUE)

  # Parse helper: try as-is, then with invalid escapes repaired. Models
  # frequently emit "\_", "\-", "\.", "\*" etc. inside string values
  # (a markdown-escaping habit). JSON only allows \" \\ \/ \b \f \n \r \t
  # \uXXXX. We strip the backslash from any escape that isn't one of those,
  # turning "\_" into "_" while leaving "\n", "\\", "\"" intact.
  .parse_try <- function(str) {
    out <- tryCatch(jsonlite::fromJSON(str, simplifyVector = FALSE),
                    error = function(e) NULL)
    if (!is.null(out)) return(out)
    repaired <- gsub("\\\\([^\"\\\\/bfnrtu])", "\\1", str, perl = TRUE)
    tryCatch(jsonlite::fromJSON(repaired, simplifyVector = FALSE),
             error = function(e) NULL)
  }

  out <- .parse_try(s)
  if (!is.null(out) && is.list(out)) return(out)

  chars <- strsplit(s, "", fixed = TRUE)[[1]]
  start <- which(chars == "{")[1]
  if (is.na(start)) return(NULL)
  depth <- 0L; end <- NA_integer_; in_str <- FALSE; esc <- FALSE
  for (i in start:length(chars)) {
    c <- chars[i]
    if (in_str) {
      if (esc) esc <- FALSE
      else if (c == "\\") esc <- TRUE
      else if (c == '"') in_str <- FALSE
      next
    }
    if (c == '"') { in_str <- TRUE; next }
    if (c == "{") depth <- depth + 1L
    if (c == "}") { depth <- depth - 1L; if (depth == 0L) { end <- i; break } }
  }
  if (is.na(end)) return(NULL)
  cand <- paste(chars[start:end], collapse = "")
  .parse_try(cand)
}

#' Resolve an optional chart_ref to a rendered data URI (or NULL).
.resolve_chart <- function(cref, tool_charts) {
  if (is.null(cref) || !is.character(cref) || !nzchar(cref) ||
      identical(tolower(cref), "null")) return(NULL)
  cd <- tool_charts[[cref]]
  if (is.null(cd)) return(NULL)
  tryCatch(render_chart_data(cd), error = function(e) NULL)
}

#' Top-level entry point used by the Shiny module. Runs an agentic loop:
#' the model calls analytics tools, observes results, then answers, and may
#' request a chart via chart_ref.
#'
#' @return list(text = <markdown>, chart_uri = <data URI or NULL>)
assistant_answer <- function(question, history = list(), run_id = NULL,
                             cfg = NULL) {
  if (is.null(cfg)) cfg <- llm_assistant_config()
  if (!isTRUE(cfg$enabled)) {
    return(list(text = "[assistant error] Assistant disabled in config.yml.",
                chart_uri = NULL))
  }
  seed <- tryCatch(.seed_context(run_id, cfg),
                   error = function(e) "(seed context unavailable)")
  sys  <- .assistant_system_prompt(seed, run_id)

  if (length(history) > cfg$max_history_turns * 2) {
    history <- utils::tail(history, cfg$max_history_turns * 2)
  }
  convo <- c(list(list(role = "system", content = sys)),
             history,
             list(list(role = "user", content = question)))

  tool_charts <- list()
  max_calls   <- cfg$max_tool_calls
  ntool       <- 0L

  repeat {
    raw <- llm_chat(convo, cfg = cfg)
    if (startsWith(raw, "[assistant error]")) {
      return(list(text = raw, chart_uri = NULL))
    }
    parsed <- .extract_json(raw)
    if (is.null(parsed)) {
      return(list(text = raw, chart_uri = NULL))   # plain answer, no JSON
    }
    action <- parsed$action %||% (if (!is.null(parsed$answer)) "final" else
                                  if (!is.null(parsed$tool)) "tool" else NA)

    if (identical(action, "final")) {
      txt <- parsed$answer %||% "(the assistant returned an empty answer)"
      return(list(text = txt,
                  chart_uri = .resolve_chart(parsed$chart_ref, tool_charts)))
    }

    if (identical(action, "tool")) {
      if (ntool >= max_calls) {
        convo <- c(convo,
                   list(list(role = "assistant", content = raw)),
                   list(list(role = "user",
                             content = "Tool-call limit reached. Give your final answer now as JSON with action=final.")))
        raw2 <- llm_chat(convo, cfg = cfg)
        p2 <- .extract_json(raw2)
        txt <- if (!is.null(p2) && !is.null(p2$answer)) p2$answer else raw2
        cref <- if (!is.null(p2)) p2$chart_ref else NULL
        return(list(text = txt, chart_uri = .resolve_chart(cref, tool_charts)))
      }
      ntool <- ntool + 1L
      id <- paste0("t", ntool)
      res <- llm_execute_tool(parsed$tool %||% "", parsed$args %||% list())
      if (!is.null(res$chart_data)) tool_charts[[id]] <- res$chart_data
      obs <- sprintf("OBSERVATION (tool=%s, tool_id=%s):\n%s",
                     parsed$tool %||% "?", id,
                     .llm_trim(res$summary %||% "(no result)", 6000))
      convo <- c(convo,
                 list(list(role = "assistant", content = raw)),
                 list(list(role = "user", content = obs)))
      next
    }
    return(list(text = raw, chart_uri = NULL))
  }
}

if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
