# =============================================================================
# app/modules/mod_suppressions.R
#
# Suppression manager. A suppression marks a specific validator as
# "we know it's failing and that's OK, here's why". Suppressed findings
# still RUN and are recorded; they just don't trip gating. Each suppression
# carries a required reason and approver — the audit trail is the point.
#
# Calls these pipeline helpers (from R/validation_suppressions.R):
#   load_suppressions, add_suppression
#
# This module reads suppressions from the LIVE config/. Snapshot-scoped
# suppressions (inside config_snapshots/<label>/config/) are read by
# run_etl when running that snapshot but are not edited here — they're
# implicitly frozen with the snapshot.
# =============================================================================

mod_suppressions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(12,
      h3("Validation suppressions"),
      p(class = "small-muted",
        "A suppression marks a validator as accepted-failing. The validator ",
        "still runs and the finding is still recorded; it just stops blocking ",
        "the run. Use this for known data-quality issues that have been ",
        "investigated and accepted by the team. Every suppression requires ",
        "a reason and approver — the audit log captures both."),
      p(class = "small-muted",
        "A suppression is STANDING: it accepts its finding in every run until it ",
        "expires or is removed. To accept a finding for one run only, use ",
        tags$strong("Accept for this run"), " on the Run pipeline page \u2014 ",
        "the next run asks again.")
    )),
    fluidRow(
      column(5,
        card(
          card_header("Add suppression"),
          textInput(ns("new_validator_id"), "Validator ID",
                     placeholder = "e.g. INPUT_ACA_contract_fk"),
          textAreaInput(ns("new_reason"), "Reason",
                         rows = 4,
                         placeholder = "Why is it OK to suppress this validator? Reference any ticket / decision date."),
          dateInput(ns("new_valid_until"), "Valid until (optional)",
                     value = NA),
          p(class = "small-muted",
            "Leave blank for no expiry. With an expiry date, the suppression ",
            "automatically lapses and the validator is back to ERROR/WARN."),
          actionButton(ns("do_add"), "Add suppression",
                        icon = icon("shield-halved"),
                        class = "btn-primary"),
          uiOutput(ns("add_status"))
        )
      ),
      column(7,
        card(
          card_header("Validator catalog (for reference)"),
          p(class = "small-muted",
            "These are the validator IDs available to suppress, drawn from ",
            "the most recent run's validation report. Click an ID to copy ",
            "into the Validator ID field above."),
          uiOutput(ns("validator_catalog"))
        )
      )
    ),
    fluidRow(column(12,
      hr(),
      h4("Suppressions, and what became of them"),
      p(class = "small-muted",
        textOutput(ns("path_label"), inline = TRUE),
        " \u2014 nothing is ever deleted: a removed suppression stays, with who ",
        "removed it and why."),
      DT::DTOutput(ns("active_table")),
      card(
        card_header("Remove a suppression"),
        p(class = "small-muted",
          "Ends it from today. The finding then blocks again and is asked about ",
          "on each run. The audit log records the removal."),
        uiOutput(ns("remove_pick")),
        textInput(ns("remove_reason"), "Reason (required)",
                  placeholder = "e.g. findings are now accepted run by run"),
        actionButton(ns("do_remove"), "Remove", icon = icon("trash"),
                     class = "btn-outline-danger")
      )
    ))
  )
}


mod_suppressions_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh <- reactiveVal(0)

    suppressions_path <- function() {
      file.path(getOption("ifrs9.project_root", getwd()),
                "config", "validation_suppressions.yml")
    }

    suppr <- reactive({
      refresh()
      tryCatch(load_suppressions(suppressions_path()),
                error = function(e) {
                  showNotification(paste("Failed to read suppressions:",
                                          conditionMessage(e)),
                                    type = "error")
                  tibble::tibble(validator_id = character(),
                                  reason = character(),
                                  approved_by = character(),
                                  approved_at = character(),
                                  valid_until = character())
                })
    })

    output$path_label <- renderText({
      p <- suppressions_path()
      sprintf("file: %s   |   %d entries",
              normalizePath(p, mustWork = FALSE),
              nrow(suppr()))
    })

    # ---- Validator catalog -------------------------------------------
    # Show every validator that has FAILED in at least one historical
    # run (deduped by validator ID; severity = max seen). That way the
    # catalog stays useful even if the latest run happens to be clean.
    # The "last seen" column tells the user which run last produced
    # the failure — useful when deciding whether the issue is still
    # current.
    output$validator_catalog <- renderUI({
      runs <- tryCatch(list_runs(), error = function(e) NULL)
      if (is.null(runs) || nrow(runs) == 0) {
        return(p(class = "small-muted",
                  "No runs found yet. Once you have a run on disk, every ",
                  "validator that has FAILED in any run will appear here ",
                  "as a candidate to suppress. Suppressing means: the ",
                  "validator still runs and is recorded, but it stops ",
                  "blocking the run."))
      }

      # Walk ALL runs (newest first) and collect failed validators.
      # Cap at the most-recent 20 runs to keep this fast for large
      # archives.
      runs_to_scan <- head(runs, 20)
      collected <- list()
      for (i in seq_len(nrow(runs_to_scan))) {
        v <- tryCatch(read_run_validation(runs_to_scan$path[i]),
                       error = function(e) NULL)
        if (is.null(v) || nrow(v) == 0) next
        failed <- v[!as.logical(v$passed), , drop = FALSE]
        if (nrow(failed) == 0) next
        failed$last_seen_run <- runs_to_scan$run_id[i]
        collected[[length(collected) + 1]] <- failed
      }

      if (length(collected) == 0) {
        return(p(class = "small-muted",
                  "No failed validators across the most recent ",
                  "20 runs. Nothing to suppress right now. ",
                  "If a validator starts failing in a future run, ",
                  "it will show up here automatically."))
      }

      all_failed <- do.call(rbind, collected)
      # Dedup on validator ID, keeping the most recent occurrence
      # (preserves the worst severity since we walk newest-first).
      keep <- !duplicated(all_failed$id)
      uniq <- all_failed[keep, , drop = FALSE]

      stages <- intersect(c("INPUT", "TRANSFORM", "DERIVED"),
                          unique(uniq$stage))
      tagList(
        p(class = "small-muted",
          tags$em("Copy a validator ID into the form on the left to add a suppression. "),
          sprintf("Showing %d unique failed validators across the last %d runs.",
                  nrow(uniq), nrow(runs_to_scan))),
        lapply(stages, function(stg) {
          rows <- uniq[uniq$stage == stg, , drop = FALSE]
          if (nrow(rows) == 0) return(NULL)
          tagList(
            tags$h6(stg),
            tags$ul(class = "list-unstyled",
              lapply(seq_len(nrow(rows)), function(i) {
                row <- rows[i, ]
                tags$li(
                  tags$span(class = sprintf("pill pill-%s",
                                              tolower(row$severity)),
                            row$severity),
                  " ",
                  tags$code(row$id, style = "font-size: 0.9em;"),
                  tags$span(class = "small-muted",
                            sprintf(" — %s (last seen in run %s)",
                                    row$description %||% "",
                                    row$last_seen_run))
                )
              })
            )
          )
        })
      )
    })

    # ---- Add suppression -------------------------------------------
    observeEvent(input$do_add, {
      vid <- trimws(input$new_validator_id %||% "")
      reason <- trimws(input$new_reason %||% "")
      vu <- input$new_valid_until
      vu_str <- if (!is.null(vu) && !is.na(vu)) format(vu) else ""

      if (!nzchar(vid)) {
        showNotification("Validator ID is required.", type = "warning"); return()
      }
      if (!nzchar(reason)) {
        showNotification("Reason is required (audit trail).", type = "warning"); return()
      }

      result <- tryCatch(
        add_suppression(
          path = suppressions_path(),
          validator_id = vid,
          reason = reason,
          approved_by = Sys.info()[["user"]] %||% "unknown",
          valid_until = if (nzchar(vu_str)) vu_str else NULL
        ),
        error = function(e) e
      )
      if (inherits(result, "error")) {
        showNotification(paste("Add failed:", conditionMessage(result)),
                          type = "error", duration = 10)
      } else {
        showNotification(paste("Added suppression for:", vid),
                          type = "message", duration = 5)
        updateTextInput(session, "new_validator_id", value = "")
        updateTextAreaInput(session, "new_reason", value = "")
        refresh(refresh() + 1)
      }
    })

    # ---- History: every entry, active, expired or removed ------------
    history <- reactive({
      refresh()
      raw <- tryCatch(yaml::read_yaml(suppressions_path()), error = function(e) NULL)
      ent <- raw$suppressions %||% list()
      if (length(ent) == 0) return(NULL)
      f <- function(e, k) as.character(e[[k]] %||% "")
      df <- data.frame(
        validator_id   = vapply(ent, f, "", k = "validator_id"),
        reason         = vapply(ent, f, "", k = "reason"),
        approved_by    = vapply(ent, f, "", k = "approved_by"),
        approved_at    = vapply(ent, f, "", k = "approved_at"),
        valid_until    = vapply(ent, f, "", k = "valid_until"),
        removed_by     = vapply(ent, f, "", k = "removed_by"),
        removed_at     = vapply(ent, f, "", k = "removed_at"),
        removal_reason = vapply(ent, f, "", k = "removal_reason"),
        stringsAsFactors = FALSE)
      vu <- suppressWarnings(as.Date(df$valid_until))
      df$status <- ifelse(nzchar(df$removed_by), "removed",
                          ifelse(is.na(vu) | vu >= Sys.Date(), "active", "expired"))
      df[, c("status", setdiff(names(df), "status"))]
    })

    output$active_table <- DT::renderDT({
      s <- history()
      if (is.null(s) || nrow(s) == 0) {
        return(DT::datatable(
          data.frame(message = "No suppressions yet."),
          options = list(dom = "t", ordering = FALSE),
          rownames = FALSE))
      }
      DT::datatable(
        s,
        rownames = FALSE,
        class = "narrow-table compact",
        options = list(pageLength = 15)
      )
    })

    # ---- Remove: end from today, keep the entry -----------------------
    output$remove_pick <- renderUI({
      ids <- sort(active_suppression_ids(suppr()))
      if (length(ids) == 0) {
        return(p(class = "small-muted", "No suppression is in force."))
      }
      selectInput(ns("remove_id"), "Suppression in force", choices = ids)
    })

    observeEvent(input$do_remove, {
      vid <- input$remove_id %||% ""
      why <- trimws(input$remove_reason %||% "")
      if (!nzchar(vid)) {
        showNotification("No suppression is in force.", type = "warning"); return()
      }
      if (!nzchar(why)) {
        showNotification("A reason is required (audit trail).", type = "warning")
        return()
      }
      n <- tryCatch(remove_suppression(suppressions_path(), vid, why,
                                       Sys.info()[["user"]] %||% "unknown"),
                    error = function(e) e)
      if (inherits(n, "error")) {
        showNotification(paste("Remove failed:", conditionMessage(n)),
                         type = "error", duration = 10)
      } else {
        showNotification(sprintf("Removed the suppression of %s. From the next pre-run check it blocks again and is asked about on each run.", vid),
                         type = "message", duration = 8)
        updateTextInput(session, "remove_reason", value = "")
        refresh(refresh() + 1)
      }
    })
  })
}
