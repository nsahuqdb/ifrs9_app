# =============================================================================
# app/modules/_ui_helpers.R
#
# Shared UI building blocks for the redesigned interface. Pure presentation:
# these produce htmltools tags / reactable widgets and never touch the R engine.
# Loaded before the modules (underscore-prefixed so it sorts first).
# =============================================================================

# Page header: title + optional subtitle + optional right-side actions.
qdb_page_header <- function(title, subtitle = NULL, actions = NULL) {
  div(class = "qdb-hd",
    div(class = "qdb-hd-row",
      div(
        h3(title),
        if (!is.null(subtitle)) div(class = "sub", subtitle)),
      if (!is.null(actions)) div(class = "qdb-hd-actions", actions)))
}

# A row of KPI stat tiles. `stats` is a list of list(k=, v=, tone=, s=) where
# tone is one of "", "accent", "ok", "warn", "err" and the optional s is a
# small line under the label (a mix, a coverage, a count).
qdb_stats <- function(stats) {
  div(class = "qdb-stats",
    lapply(stats, function(s) {
      cls <- paste("qdb-stat", s$tone %||% "")
      div(class = trimws(cls),
        div(class = "v", s$v),
        div(class = "k", s$k),
        if (!is.null(s$s) && nzchar(s$s)) div(class = "s", s$s))
    }))
}

# Status pill. Maps a status string to a coloured pill.
qdb_pill <- function(text, tone = c("muted","ok","warn","err","info")) {
  tone <- match.arg(tone)
  span(class = sprintf("qdb-pill qp-%s", tone), text)
}

# Map common run/config statuses to a pill.
qdb_status_pill <- function(status) {
  s <- tolower(as.character(status %||% ""))
  tone <- if (s %in% c("approved","passed","pass","official","ok","done")) "ok"
    else if (s %in% c("pending","pending_checker","pending_final","pending_approval","warn","tested")) "warn"
    else if (s %in% c("rejected","error","failed","fail")) "err"
    else if (s %in% c("unofficial","info","draft")) "info"
    else "muted"
  qdb_pill(toupper(as.character(status %||% "\u2014")), tone)
}

# Empty-state block.
qdb_empty <- function(msg, icon_name = "inbox") {
  div(class = "qdb-empty",
    div(class = "ico", icon(icon_name)),
    div(msg))
}

# --- reactable wrapper -------------------------------------------------------
# One place to configure the look/behaviour of every table so they are
# consistent. Falls back to a plain HTML table if reactable isn't installed,
# so the app degrades gracefully.
qdb_react_available <- function() requireNamespace("reactable", quietly = TRUE)

qdb_reactable <- function(df, ..., searchable = TRUE, page_size = 12,
                          columns = NULL, on_click = NULL, compact = TRUE,
                          highlight = TRUE, wrap = FALSE, min_width = NULL,
                          default_sorted = NULL) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0)
    return(qdb_empty("No rows to show."))
  if (!qdb_react_available()) {
    # graceful fallback: simple styled HTML table
    body <- lapply(seq_len(min(nrow(df), 200)), function(i)
      tags$tr(lapply(df[i, , drop = TRUE], function(v) tags$td(HTML(as.character(v))))))
    return(tags$div(class = "qdb-rt", style = "overflow-x:auto",
      tags$table(class = "narrow-table",
        tags$thead(tags$tr(lapply(colnames(df), tags$th))),
        tags$tbody(body))))
  }
  reactable::reactable(
    df,
    searchable = searchable,
    filterable = FALSE,
    sortable = TRUE,
    resizable = TRUE,
    highlight = highlight,
    compact = compact,
    striped = FALSE,
    bordered = FALSE,
    wrap = wrap,
    pagination = TRUE,
    defaultPageSize = page_size,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(12, 25, 50, 100),
    minRows = 1,
    class = "qdb-rt",
    columns = columns,
    defaultSorted = default_sorted,
    onClick = on_click,
    theme = reactable::reactableTheme(
      borderColor = "#f1eef5",
      highlightColor = "#f7f4f9",
      cellPadding = "9px 12px",
      headerStyle = list(background = "#faf9fc")
    ),
    ...
  )
}

# Colour a numeric-ish ECL/amount column consistently (used via cell renderers).
qdb_fmt_amount <- function(x) {
  n <- suppressWarnings(as.numeric(x))
  ifelse(is.na(n), as.character(x), format(round(n), big.mark = ",", scientific = FALSE))
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Collapsible help block.
#'
#' Collapsed by default so it never gets in the way; opened when someone wants
#' to know what a term means or how a figure is arrived at. `items` is a named
#' list: name = the term, value = the explanation.
qdb_help <- function(items, label = "What do these mean?", note = NULL) {
  htmltools::tags$details(class = "qdb-help",
    htmltools::tags$summary(icon("circle-question"), label),
    htmltools::tags$dl(
      unlist(lapply(names(items), function(k) list(
        htmltools::tags$dt(k),
        htmltools::tags$dd(htmltools::HTML(items[[k]]))
      )), recursive = FALSE)),
    if (!is.null(note)) htmltools::tags$p(class = "qdb-help-note",
                                          htmltools::HTML(note)) else NULL)
}


# Every finding accepted in a run, and why -- for the run pages and the
# approval review: accepted for the run on the pipeline page, or by a standing
# suppression that took effect, with the reason, who and when
# (read_run_accepted_findings(); rebuilt for a run made before runs kept
# reports/accepted_findings.csv). NULL when nothing was accepted.
accepted_findings_ui <- function(run_path, title = "Accepted findings") {
  acc <- tryCatch(read_run_accepted_findings(run_path), error = function(e) NULL)
  if (is.null(acc) || nrow(acc) == 0) return(NULL)
  rebuilt <- any(acc$recorded == "FALSE")
  div(class = "alert alert-info", style = "padding:0.6em 0.9em; margin:0.6em 0;",
    tags$strong(sprintf("%s: %d", title, nrow(acc))),
    tags$span(class = "small-muted",
      " — accepted for this run, or by a standing suppression that took effect",
      if (rebuilt) " (rebuilt from the run's validation report and frozen suppressions file)"),
    tags$table(class = "table table-sm", style = "margin:0.4em 0 0; background:transparent;",
      tags$tr(tags$th("Check"), tags$th("Severity"), tags$th("Accepted"),
              tags$th("Reason"), tags$th("By"), tags$th("When"), tags$th("Until")),
      lapply(seq_len(nrow(acc)), function(i) tags$tr(
        tags$td(tags$code(acc$validator_id[i])), tags$td(acc$severity[i]),
        tags$td(if (identical(acc$source[i], "run")) "for this run"
                else if (identical(acc$source[i], "standing")) "standing suppression"
                else "—"),
        tags$td(acc$reason[i]), tags$td(acc$accepted_by[i]),
        tags$td(substr(acc$accepted_at[i], 1, 16)), tags$td(acc$valid_until[i])))))
}
