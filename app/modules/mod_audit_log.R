# =============================================================================
# app/modules/mod_audit_log.R
#
# The Audit log page: a filterable, human-friendly view of
# logs/etl_audit.jsonl. Every meaningful event in the system writes one
# JSON line there. Read-only.
#
# Display layout:
#   - 5 columns: ts, event, user, run_id, summary
#   - The `summary` column is computed from the event payload — one line
#     of plain English instead of raw JSON
#   - Three filter dropdowns at the top: event type, run ID, user
#
# Reads:
#   read_audit_log()      from R/audit_log.R
# =============================================================================

mod_audit_log_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(12,
      h3("Audit log"),
      p(class = "small-muted",
        textOutput(ns("audit_log_label"), inline = TRUE)),
      actionButton(ns("refresh"), "Refresh", icon = icon("rotate"),
                    class = "btn-sm btn-outline-secondary")
    )),
    fluidRow(
      column(4, selectInput(ns("event_filter"), "Event type",
                              choices = c("(all)" = ""), selected = "")),
      column(4, selectInput(ns("run_filter"), "Run ID",
                              choices = c("(all)" = ""), selected = "")),
      column(4, selectInput(ns("user_filter"), "User",
                              choices = c("(all)" = ""), selected = ""))
    ),
    fluidRow(column(12,
      DT::DTOutput(ns("audit_table"))
    ))
  )
}


# Friendly display names for event types (raw ids stay in the jsonl).
.audit_event_label <- function(ev) {
  lbl <- c(
    run_start             = "Run started",
    run_finish            = "Run finished",
    run_unofficial        = "Unofficial run",
    run_pending_checker   = "Official run (pending approval)",
    run_phase1_complete   = "Paused for overrides",
    run_export            = "Outputs exported",
    run_overrides_applied = "Overrides applied",
    validation_summary    = "Validation",
    pre_run_check         = "Pre-run check",
    snapshot_create       = "Version created",
    snapshot_clone        = "Version cloned",
    snapshot_edit         = "Config edited",
    snapshot_promote      = "Status changed",
    suppression_add       = "Suppression added"
  )
  out <- unname(lbl[ev])
  ifelse(is.na(out), ev, out)
}

# Build a one-line human-friendly summary for an audit row.
.audit_summary <- function(row) {
  ev <- row$event %||% ""
  s <- function(field) {
    v <- row[[field]]
    if (is.null(v) || (length(v) == 1 && (is.na(v) || !nzchar(as.character(v)))))
      "" else as.character(v)
  }
  or_ <- function(x, alt) if (nzchar(x)) x else alt
  switch(
    ev,
    "run_start" = sprintf(
      "Run started on config version '%s'%s",
      or_(s("snapshot"), "default config"),
      if (nzchar(s("ecl_scenario")) && s("ecl_scenario") != "weighted")
        sprintf(" \u2014 scenario: %s", s("ecl_scenario")) else ""
    ),
    "run_finish" = sprintf(
      "Run finished \u2014 %s output files%s",
      or_(s("n_outputs"), "?"),
      if (!is.null(row$duration_seconds))
        sprintf(" in %.0fs", as.numeric(row$duration_seconds)) else ""
    ),
    "run_export" = "Outputs downloaded",
    "run_overrides_applied" = {
      n <- s("n_overrides")
      if (nzchar(n) && n != "0") {
        parts <- c(
          if (nzchar(s("n_rating_overrides")) && s("n_rating_overrides") != "0")
            sprintf("%s rating", s("n_rating_overrides")),
          if (nzchar(s("n_stage_overrides")) && s("n_stage_overrides") != "0")
            sprintf("%s stage", s("n_stage_overrides")),
          if (nzchar(s("n_restructuring_overrides")) && s("n_restructuring_overrides") != "0")
            sprintf("%s restructuring", s("n_restructuring_overrides")))
        sprintf("Applied %s override%s%s", n, if (n == "1") "" else "s",
                 if (length(parts) > 0)
                   sprintf(" (%s)", paste(parts, collapse = ", ")) else "")
      } else {
        "No overrides applied"
      }
    },
    "run_unofficial" = sprintf(
      "Unofficial run finished \u2014 %s output files, %s override%s%s (auto-approved)",
      or_(s("n_outputs"), "?"), or_(s("n_overrides"), "0"),
      if (s("n_overrides") == "1") "" else "s",
      if (nzchar(s("ecl_scenario")) && s("ecl_scenario") != "weighted")
        sprintf("; scenario: %s", s("ecl_scenario")) else ""
    ),
    "run_pending_checker" = sprintf(
      "Official run finished \u2014 %s output files, %s override%s; pending approval",
      or_(s("n_outputs"), "?"), or_(s("n_overrides"), "0"),
      if (s("n_overrides") == "1") "" else "s"
    ),
    "run_phase1_complete" = sprintf(
      "Phase 1 done (%s lending customers, %s investment accounts) \u2014 paused for overrides",
      or_(s("n_customers_lending"), "?"), or_(s("n_accounts_investments"), "?")
    ),
    "validation_summary" = sprintf(
      "%s checks: %s of %s passed%s",
      or_(s("stage"), "Validation"), or_(s("n_pass"), "?"), or_(s("n_total"), "?"),
      {
        bits <- c(
          if (nzchar(s("n_error")) && s("n_error") != "0")
            sprintf("%s errors", s("n_error")),
          if (nzchar(s("n_warn")) && s("n_warn") != "0")
            sprintf("%s warnings", s("n_warn")))
        if (length(bits) > 0) paste0(" (", paste(bits, collapse = ", "), ")") else ""
      }
    ),
    "pre_run_check" = sprintf(
      "Pre-run check: %s of %s checks passed",
      or_(s("n_pass"), "?"), or_(s("n_total"), "?")
    ),
    "snapshot_create" = sprintf(
      "Created version '%s'%s",
      s("snapshot"),
      if (nzchar(s("parent"))) sprintf(" based on '%s'", s("parent"))
      else " from the default config"
    ),
    "snapshot_clone" = sprintf(
      "Cloned version '%s' into new draft '%s'",
      s("cloned_from"), s("snapshot")
    ),
    "snapshot_edit" = sprintf(
      "Edited %s in version '%s'%s",
      or_(basename(s("relpath")), "a file"), s("snapshot"),
      if (nzchar(s("n_rows"))) sprintf(" \u2014 now %s rows", s("n_rows")) else ""
    ),
    "snapshot_promote" = {
      rank <- c(draft = 1, tested = 2, pending_final = 3, pending = 3,
                 approved = 4, rejected = 0, archived = 0)
      f <- s("from_status"); t <- s("to_status")
      dir <- if (!is.na(rank[t]) && !is.na(rank[f]) && rank[t] < rank[f])
        "moved back to" else "moved to"
      sprintf("Version '%s' %s %s (was %s)", s("snapshot"), dir,
               or_(t, "?"), or_(f, "?"))
    },
    "suppression_add" = sprintf(
      "Check '%s' suppressed until %s",
      s("validator_id"), or_(s("valid_until"), "?")
    ),
    # Unknown event type \u2014 show only fields that HAVE a value.
    {
      std <- c("ts", "event", "user", "run_id")
      extras <- setdiff(names(row), std)
      vals <- vapply(extras, function(k) s(k), character(1))
      keep <- nzchar(vals)
      if (!any(keep)) "" else
        paste(sprintf("%s=%s", extras[keep], vals[keep]), collapse = ", ")
    }
  )
}


mod_audit_log_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh <- reactiveVal(0)
    observeEvent(input$refresh, refresh(refresh() + 1))

    audit <- reactive({
      refresh()
      tryCatch(read_audit_log(),
                error = function(e) {
                  showNotification(paste("Failed to read audit log:",
                                          conditionMessage(e)), type = "error")
                  tibble::tibble()
                })
    })

    observe({
      a <- audit()
      events <- if ("event" %in% names(a)) sort(unique(a$event)) else character()
      runs   <- if ("run_id" %in% names(a))
                  sort(unique(a$run_id[!is.na(a$run_id) & nzchar(a$run_id)]),
                       decreasing = TRUE)
                else character()
      users  <- if ("user" %in% names(a)) sort(unique(a$user)) else character()

      updateSelectInput(session, "event_filter",
                         choices = c("(all)" = "",
                                      setNames(events, .audit_event_label(events))),
                         selected = isolate(input$event_filter) %||% "")
      updateSelectInput(session, "run_filter",
                         choices = c("(all)" = "", runs),
                         selected = isolate(input$run_filter) %||% "")
      updateSelectInput(session, "user_filter",
                         choices = c("(all)" = "", users),
                         selected = isolate(input$user_filter) %||% "")
    })

    output$audit_log_label <- renderText({
      p <- audit_log_path()
      p_norm <- normalizePath(p, mustWork = FALSE)
      if (!file.exists(p)) {
        sprintf("audit log: %s   (does not exist yet)", p_norm)
      } else {
        sprintf("audit log: %s   |   %d events", p_norm, nrow(audit()))
      }
    })

    output$audit_table <- DT::renderDT({
      a <- audit()
      if (nrow(a) == 0) {
        return(DT::datatable(
          data.frame(message =
            "No audit events yet. Run the pipeline, or edit versions/suppressions."),
          options = list(dom = "t", ordering = FALSE),
          rownames = FALSE))
      }

      if (nzchar(input$event_filter %||% ""))
        a <- a[a$event == input$event_filter, , drop = FALSE]
      if (nzchar(input$run_filter %||% "") && "run_id" %in% names(a))
        a <- a[!is.na(a$run_id) & a$run_id == input$run_filter, , drop = FALSE]
      if (nzchar(input$user_filter %||% "") && "user" %in% names(a))
        a <- a[a$user == input$user_filter, , drop = FALSE]

      if (nrow(a) == 0) {
        return(DT::datatable(
          data.frame(message = "No events match the current filter."),
          options = list(dom = "t", ordering = FALSE),
          rownames = FALSE))
      }

      summaries <- vapply(seq_len(nrow(a)),
                           function(i) .audit_summary(as.list(a[i, , drop = FALSE])),
                           character(1))

      fmt_ts <- function(x) sub("T", " ", substr(as.character(x), 1, 19))
      display <- data.frame(
        time    = fmt_ts(a$ts %||% NA_character_),
        action  = .audit_event_label(a$event %||% NA_character_),
        user    = a$user %||% NA_character_,
        run     = ifelse(is.na(a$run_id %||% NA_character_), "",
                          as.character(a$run_id)),
        details = summaries,
        stringsAsFactors = FALSE
      )
      display <- display[order(a$ts, decreasing = TRUE), ]

      DT::datatable(
        display,
        rownames = FALSE,
        colnames = c("Time", "Action", "User", "Run", "Details"),
        class = "compact stripe hover",
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          deferRender = TRUE,
          dom = "lftip",
          columnDefs = list(list(width = "150px", targets = 0),
                             list(width = "130px", targets = 1))
        )
      )
    })
  })
}
