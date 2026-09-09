# =============================================================================
# app/modules/mod_runs.R
#
# The Runs page: a table of all past runs newest-first, with a detail
# panel that renders manifest, validation, reconciliation, and an output
# browser for a selected run.
#
# Read-only. Reads:
#   list_runs(runs_dir_default())            from R/run_discovery.R
#   read_run_manifest(path)
#   read_run_validation(path)
#   read_run_reconciliation(path)
#   list_run_outputs(path)
# =============================================================================

mod_runs_ui <- function(id) {
  ns <- NS(id)
  div(class = "qdb-page",
    qdb_page_header(
      "Runs",
      subtitle = textOutput(ns("runs_dir_label"), inline = TRUE),
      actions = actionButton(ns("refresh"), "Refresh", icon = icon("rotate"),
                             class = "btn-sm btn-outline-secondary")),
    uiOutput(ns("runs_stats")),
    div(class = "qdb-card", style = "padding:6px 6px 2px",
      reactable::reactableOutput(ns("runs_table_rt"))),
    # Detail block — single renderUI keyed on the selected run; inline HTML /
    # reactable tables, server-side search. See history for why this pattern.
    uiOutput(ns("detail_block"))
  )
}

# Render a data.frame as a simple Bootstrap HTML table (reliable inside
# renderUI, unlike DTOutput). Truncates to `max_rows` with a note. `escape`
# FALSE lets pre-formatted HTML (status pills) through.
.runs_html_table <- function(df, max_rows = 200,
                             class = "table table-sm narrow-table compact",
                             escape = TRUE) {
  if (is.null(df) || nrow(df) == 0)
    return(tags$p(class = "small-muted", "(no rows)"))
  note <- NULL
  if (nrow(df) > max_rows) {
    note <- tags$p(class = "small-muted",
                   sprintf("Showing first %d of %d rows.", max_rows, nrow(df)))
    df <- df[seq_len(max_rows), , drop = FALSE]
  }
  cell <- function(v) if (isTRUE(escape)) tags$td(as.character(v)) else tags$td(HTML(as.character(v)))
  body <- lapply(seq_len(nrow(df)), function(i)
    tags$tr(lapply(df[i, , drop = TRUE], cell)))
  tagList(note,
    tags$div(style = "overflow-x:auto",
      tags$table(class = class,
        tags$thead(tags$tr(lapply(colnames(df), tags$th))),
        tags$tbody(body))))
}


mod_runs_server <- function(id, on_select_run = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh_trigger <- reactiveVal(0)
    observeEvent(input$refresh, refresh_trigger(refresh_trigger() + 1))

    # Safety-net: poll the runs directory every 2 seconds and trigger a
    # refresh when any run_status.yml has been modified. This catches
    # the case where mod_approval_queue's cross-module signal is missed
    # by Shiny's invalidation graph (e.g. tab not yet rendered when the
    # signal fires). Cheap to compute — file.info() on the 18-or-so
    # run_status.yml files takes well under a millisecond.
    status_mtimes <- reactivePoll(
      intervalMillis = 2000,
      session = session,
      checkFunc = function() {
        d <- runs_dir_default()
        if (!dir.exists(d)) return("")
        paths <- list.files(d, pattern = "^run_status\\.yml$",
                             recursive = TRUE, full.names = TRUE)
        if (length(paths) == 0) return("")
        # Hash of (path, mtime) pairs — changes whenever any status file
        # changes or is added/removed.
        info <- file.info(paths)
        paste(paths, info$mtime, collapse = "|")
      },
      valueFunc = function() Sys.time()
    )

    runs_tbl <- reactive({
      refresh_trigger()
      # Cross-module signal: bump from mod_run_trigger when phase 2
      # finishes, and from mod_approval_queue when a run is approved
      # or rejected (so the status pill re-renders here).
      if (!is.null(session$userData$runs_changed)) {
        session$userData$runs_changed()
      }
      # Safety net: also depend on the directory poll.
      status_mtimes()
      r <- tryCatch(list_runs(), error = function(e) {
        showNotification(paste("Failed to list runs:", conditionMessage(e)),
                          type = "error")
        list_runs.empty()
      })
      # Annotate each row with its approval status (pending_approval /
      # approved / rejected / unknown). Pre-H12 runs and runs whose
      # phase 2 errored before writing run_status.yml come back as
      # "unknown" so they're still visible.
      tryCatch(annotate_runs_with_status(r), error = function(e) {
        r$status <- rep("unknown", nrow(r))
        r
      })
    })

    output$runs_dir_label <- renderText({
      d <- runs_dir_default()
      d_norm <- normalizePath(d, mustWork = FALSE)
      exists <- dir.exists(d)
      n <- nrow(runs_tbl())
      sprintf("runs directory: %s   |   exists: %s   |   %d runs found",
              d_norm,
              if (exists) "yes" else "NO — run with keep_history=TRUE first",
              n)
    })

    # ---- KPI stat tiles above the table -------------------------------
    output$runs_stats <- renderUI({
      r <- runs_tbl()
      n <- nrow(r)
      if (n == 0) return(NULL)
      st <- as.character(r$status %||% rep(NA, n))
      n_appr <- sum(st == "approved", na.rm = TRUE)
      n_pend <- sum(st %in% c("pending_checker","pending_approval"), na.rm = TRUE)
      n_unoff <- sum(st == "unofficial", na.rm = TRUE)
      n_valfail <- sum(suppressWarnings(as.numeric(r$n_validation_failures)) > 0, na.rm = TRUE)
      qdb_stats(list(
        list(k = "Total runs", v = n, tone = "accent"),
        list(k = "Approved", v = n_appr, tone = "ok"),
        list(k = "Pending checker", v = n_pend, tone = if (n_pend > 0) "warn" else ""),
        list(k = "Unofficial", v = n_unoff, tone = ""),
        list(k = "With validation fails", v = n_valfail, tone = if (n_valfail > 0) "err" else "")))
    })

    # Build the display frame once (shared by the table).
    .runs_display <- reactive({
      r <- runs_tbl()
      if (nrow(r) == 0) return(NULL)
      fmt <- function(x) { x <- as.character(x); x[is.na(x) | x == ""] <- "\u2014"; x }
      short_dt <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; substr(x, 1, 16) }
      pretty_type <- function(x) { x <- as.character(x)
        ifelse(x == "official","Official", ifelse(x == "unofficial","Unofficial", fmt(x))) }
      pretty_purpose <- function(x) { x <- as.character(x)
        ifelse(x == "regulatory","Regulatory",
        ifelse(x == "non_regulatory","Non-regulatory",
        ifelse(x == "impact","Impact", fmt(x)))) }
      out <- data.frame(
        status     = as.character(r$status %||% rep(NA_character_, nrow(r))),
        run_id     = r$run_id,
        purpose    = pretty_purpose(r$run_purpose),
        outputs    = suppressWarnings(as.integer(r$n_outputs)),
        type       = pretty_type(r$run_type),
        scenario   = ifelse(is.na(r$ecl_scenario) | r$ecl_scenario == "weighted",
                            "Weighted", r$ecl_scenario),
        run_date   = short_dt(r$started_at),
        portfolio_date = fmt(r$portfolio_date),
        run_by     = fmt(r$user),
        approver   = fmt(r$approver),
        config_version = ifelse(is.na(r$config_version) & is.na(r$snapshot_label),
                              "(live config)",
                              fmt(ifelse(is.na(r$config_version), r$snapshot_label, r$config_version))),
        calculator = fmt(ifelse(is.na(r$calculator_label), r$calculator_version, r$calculator_label)),
        val_fail   = suppressWarnings(as.integer(r$n_validation_failures)),
        recon      = ifelse(r$has_reconciliation, "yes", "no"),
        stringsAsFactors = FALSE)
      out
    })

    output$runs_table_rt <- reactable::renderReactable({
      d <- .runs_display()
      if (is.null(d) || nrow(d) == 0)
        return(reactable::reactable(data.frame(Message = "No runs found. Run the pipeline with keep_history=TRUE."),
               sortable = FALSE, class = "qdb-rt"))
      status_cell <- reactable::JS("
        function(cellInfo){
          var s=(cellInfo.value||'').toString().toLowerCase();
          var label=s.replace('pending_checker','pending checker').replace('pending_approval','pending checker').toUpperCase();
          var tone='qp-muted';
          if(s==='approved') tone='qp-ok';
          else if(s.indexOf('pending')>-1) tone='qp-warn';
          else if(s==='unofficial') tone='qp-info';
          else if(s==='rejected') tone='qp-err';
          return '<span class=\"qdb-pill '+tone+'\">'+label+'</span>';
        }")
      # A compact "run" cell: bold id on top, run date muted underneath.
      run_cell <- reactable::JS("
        function(ci){
          var d=ci.row['run_date']||'';
          return '<div style=\"line-height:1.35\"><div style=\"font-weight:700\">'+ci.value+
                 '</div><div style=\"font-size:11.5px;color:#8a94a6\">'+d+'</div></div>';
        }")
      # Health cell: outputs + validation fails + recon as small pills together.
      health_cell <- reactable::JS("
        function(ci){
          var vf=parseInt(ci.row['val_fail'])||0;
          var out=parseInt(ci.row['outputs'])||0;
          var rec=(ci.row['recon']==='yes');
          var h='<div style=\"display:flex;gap:6px;flex-wrap:wrap\">';
          h+='<span class=\"qdb-pill qp-muted\">'+out+' outputs</span>';
          h+= vf>0 ? '<span class=\"qdb-pill qp-err\">'+vf+' fails</span>'
                   : '<span class=\"qdb-pill qp-ok\">0 fails</span>';
          h+= rec ? '<span class=\"qdb-pill qp-ok\">recon</span>' : '';
          return h+'</div>';
        }")
      # Expandable detail: the secondary attributes, laid out as a clean grid.
      # Built in R with htmltools so reactable renders real HTML (a JS string
      # here gets escaped and shows raw "div span..." markup).
      row_details <- function(index) {
        r <- d[index, , drop = FALSE]
        field <- function(k, v) {
          v <- as.character(v)
          if (length(v) == 0 || is.na(v) || !nzchar(v)) v <- "\u2014"
          div(style = "flex:1 1 0;min-width:0",
            div(style = "font-size:10px;text-transform:uppercase;letter-spacing:.04em;color:#8a94a6;white-space:nowrap", k),
            div(style = "font-weight:600;font-size:12.5px;margin-top:1px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap", title = v, v))
        }
        div(style = paste("display:flex;flex-wrap:nowrap;gap:14px;padding:10px 14px;",
                          "background:#faf9fc;border-radius:8px;margin:2px 0"),
          field("Type", r$type),
          field("Scenario", r$scenario),
          field("Run date", r$run_date),
          field("Portfolio date", r$portfolio_date),
          field("Run by", r$run_by),
          field("Approver", r$approver),
          field("Config version", r$config_version),
          field("Calculator", r$calculator))
      }
      reactable::reactable(
        d, selection = "single", onClick = "select",
        searchable = TRUE, highlight = TRUE, compact = TRUE, wrap = TRUE,
        defaultPageSize = 12, showPageSizeOptions = TRUE,
        pageSizeOptions = c(12, 25, 50), class = "qdb-rt",
        defaultSorted = list(run_date = "desc"),
        details = row_details,
        theme = reactable::reactableTheme(
          rowSelectedStyle = list(backgroundColor = "#efe7f3"),
          borderColor = "#f1eef5", highlightColor = "#f7f4f9",
          headerStyle = list(background = "#faf9fc")),
        defaultColDef = reactable::colDef(headerClass = "qdb-th", vAlign = "center"),
        columns = list(
          .selection = reactable::colDef(show = FALSE),
          status  = reactable::colDef(name = "Status", html = TRUE, cell = status_cell, width = 150),
          run_id  = reactable::colDef(name = "Run", html = TRUE, cell = run_cell, minWidth = 140),
          purpose = reactable::colDef(name = "Purpose", minWidth = 130),
          run_date = reactable::colDef(show = FALSE),
          type = reactable::colDef(show = FALSE),
          scenario = reactable::colDef(show = FALSE),
          portfolio_date = reactable::colDef(show = FALSE),
          run_by = reactable::colDef(show = FALSE),
          approver = reactable::colDef(show = FALSE),
          config_version = reactable::colDef(show = FALSE),
          calculator = reactable::colDef(show = FALSE),
          outputs = reactable::colDef(name = "Health", html = TRUE, cell = health_cell, minWidth = 230),
          val_fail = reactable::colDef(show = FALSE),
          recon = reactable::colDef(show = FALSE)
        )
      )
    })

    # ---- Detail block --------------------------------------------------
    selected_idx <- reactive({
      st <- reactable::getReactableState("runs_table_rt", "selected")
      if (is.null(st) || length(st) == 0) return(NULL)
      st[1]
    })
    selected_run <- reactive({
      idx <- selected_idx()
      if (is.null(idx)) return(NULL)
      r <- runs_tbl()
      if (idx > nrow(r)) return(NULL)
      r[idx, , drop = FALSE]
    })

    # bumped whenever outputs on disk change (e.g. after applying an overlay),
    # so the Outputs file list refreshes without a manual page refresh.
    outputs_bump <- reactiveVal(0)

    .selected_run_status <- reactive({
      sr <- selected_run(); if (is.null(sr)) return(NA_character_)
      s <- tryCatch(read_run_status(sr$path), error = function(e) NULL)
      if (is.null(s)) return("unknown")
      s$status %||% "unknown"
    })

    # Export card body (plain builder; called inside detail_block's render).
    .export_body <- function(sr, status) {
      can_export <- isTRUE(status == "approved") || isTRUE(status == "unofficial")
      if (can_export) {
        notice <- if (status == "unofficial")
          div(class = "alert alert-info", style = "margin-bottom:0.5em;",
              tags$strong("Unofficial run."), " The export bundle is marked UNOFFICIAL.")
        else NULL
        return(tagList(notice,
          div(style = "margin-bottom:0.75em;",
            checkboxInput(ns("export_include_inputs"),
                          "Include input files (~30-80 MB extra)", value = TRUE, width = "350px"),
            downloadButton(ns("export_run"), "Export run package (zip)",
                           class = "btn-outline-primary btn-sm", icon = icon("file-zipper")))))
      }
      pending_msg <- paste("This run is awaiting checker approval. Export becomes",
        "available after a checker approves the run on the Approval queue tab.")
      explainer <- switch(as.character(status),
        "pending_checker"  = pending_msg,
        "pending_approval" = pending_msg,
        "rejected"         = "This run was rejected. Rejected runs are not exportable.",
        "unknown"          = paste("This run has no recorded approval status",
                                   "(run_status.yml missing/unparseable). Export is disabled."),
        paste0("Export is disabled in status '", status, "'."))
      div(class = "alert alert-secondary", style = "margin-bottom:0.75em;",
          tags$strong("Export not available"),
          tags$p(style = "margin-bottom:0;margin-top:0.4em;", explainer))
    }

    # ---- ONE renderUI for the entire detail block --------------------------
    # Everything that depends on the selected run is built here inline, so it
    # renders fresh every time a run is (re-)selected. Tables are HTML, not
    # DTOutput. The CSV preview is the only nested output (depends on the file
    # picker input, so it re-fires on pick).
    output$detail_block <- renderUI({
      sr <- selected_run()
      if (is.null(sr))
        return(qdb_empty("Select a run above to see its manifest, validation, reconciliation, overrides and outputs.", "hand-pointer"))
      outputs_bump()
      status <- .selected_run_status()
      paths <- tryCatch(list_run_outputs(sr$path), error = function(e) character(0))

      tagList(
        div(class = "qdb-run-title",
          h4(sprintf("Run %s", sr$run_id)),
          qdb_status_pill(status),
          span(class = "path", sr$path)),

        card(card_header("Export"), .export_body(sr, status)),

        card(card_header("Apply ECL overlay to this run"),
          p(class = "small-muted",
            "Apply a saved overlay to this completed run without re-running the ",
            "pipeline. The model report is preserved; a new overlaid report is ",
            "written alongside it. To correct a wrong overlay, Remove it below ",
            "then apply the corrected one (all overlays are listed, including drafts)."),
          fluidRow(
            column(7, selectInput(ns("apply_overlay_pick"), NULL,
                                  choices = .overlay_choices(),
                                  selected = "")),
            column(5, actionButton(ns("do_apply_overlay"), "Apply overlay",
                                    icon = icon("layer-group"), class = "btn-primary"))),
          uiOutput(ns("apply_overlay_result")),
          hr(style = "margin:10px 0"),
          strong("Overlays applied to this run"),
          uiOutput(ns("applied_overlays_list"))),

        navset_card_tab(
          nav_panel("Manifest", .manifest_body(sr)),
          nav_panel("Validation", .validation_body(sr)),
          nav_panel("Overrides", .overrides_body(sr)),
          nav_panel("Reconciliation", .reconciliation_body(sr)),
          nav_panel("Outputs",
            if (length(paths) == 0)
              qdb_empty("No output CSVs found.", "folder-open")
            else tagList(
              div(style = "max-width:420px;margin-bottom:6px",
                selectInput(ns("output_pick"), "File",
                            choices = basename(paths), width = "100%")),
              uiOutput(ns("output_meta")),
              reactable::reactableOutput(ns("output_preview_rt")))))
      )
    })

    # ---- Run export download handler ---------------------------------
    # Builds the zip via build_run_export() and streams it to the
    # browser. The zip is built on each click — we don't cache, so
    # the user always gets the freshest snapshot of the run's state
    # (e.g. if the run was approved between clicks, the new
    # run_status.yml is in the bundle).
    # ---- Export block: gated on approval ---------------------------
    # The selected run's status drives whether the download is offered.
    # Approval is recorded in runs/<id>/reports/run_status.yml; here
    # we read it via the helper from R/run_approval.R.

    # ---- Run export download handler ---------------------------------
    # Defense in depth: even though the button is hidden for non-
    # approved runs, the handler also refuses to build a bundle for
    # them. This guards against the download URL being hit directly
    # (e.g. from a bookmark) on a run whose status changed since the
    # bookmark was made.
    output$export_run <- downloadHandler(
      filename = function() {
        sr <- selected_run()
        if (is.null(sr)) return("ifrs9_run_export.zip")
        sprintf("ifrs9_run_%s.zip", sr$run_id)
      },
      content = function(file) {
        sr <- selected_run()
        if (is.null(sr)) {
          writeLines("(no run selected)", file); return()
        }
        status <- .selected_run_status()
        # Defense in depth: handler refuses unless the run is approved
        # OR unofficial. Same rule as the export_block UI gate.
        if (!isTRUE(status %in% c("approved", "unofficial"))) {
          writeLines(c(
            "(this run is not exportable; export refused)",
            sprintf("run_id: %s", sr$run_id),
            sprintf("status: %s", status)
          ), file)
          showNotification(
            sprintf("Export refused: run is in status '%s' (not approved or unofficial).",
                     status),
            type = "warning", duration = 8)
          return()
        }
        withProgress(message = "Building run package", value = 0.3, {
          res <- tryCatch(
            build_run_export(
              run_path = sr$path,
              dest_zip = file,
              include_inputs = isTRUE(input$export_include_inputs)
            ),
            error = function(e) list(ok = FALSE,
                                       message = conditionMessage(e))
          )
          setProgress(1)
        })
        if (!isTRUE(res$ok)) {
          showNotification(paste("Export failed:", res$message),
                            type = "error", duration = 12)
        } else {
          showNotification(sprintf("Exported %s",
                                     basename(res$path %||% file)),
                            type = "message", duration = 4)
        }
      },
      contentType = "application/zip"
    )

    # ---- Manifest panel (inline builder) ------------------------------
    .manifest_body <- function(sr) {
      m <- read_run_manifest(sr$path)
      if (is.null(m)) return(p("No manifest.json found."))
      run <- m$run; snap <- m$snapshot
      if (!is.null(m$inputs) && length(m$inputs) > 0) {
        df <- tryCatch(
          if (is.data.frame(m$inputs)) m$inputs
          else as.data.frame(do.call(rbind, lapply(m$inputs, as.data.frame))),
          error = function(e) NULL)
        inputs_tbl <- tagList(h5("Inputs"), .runs_html_table(df, max_rows = 100))
      } else {
        inputs_tbl <- tagList(h5("Inputs"), p(class = "small-muted", "(none recorded)"))
      }
      tagList(
        h5("Run"),
        tags$dl(class = "row",
          tags$dt(class = "col-sm-3", "run_id"),       tags$dd(class = "col-sm-9", run$run_id %||% "\u2014"),
          tags$dt(class = "col-sm-3", "started_at"),   tags$dd(class = "col-sm-9", run$started_at %||% "\u2014"),
          tags$dt(class = "col-sm-3", "finished_at"),  tags$dd(class = "col-sm-9", run$finished_at %||% "\u2014"),
          tags$dt(class = "col-sm-3", "duration"),     tags$dd(class = "col-sm-9", sprintf("%.1f s", as.numeric(run$duration_seconds %||% NA))),
          tags$dt(class = "col-sm-3", "user"),         tags$dd(class = "col-sm-9", run$user %||% "\u2014"),
          tags$dt(class = "col-sm-3", "hostname"),     tags$dd(class = "col-sm-9", run$hostname %||% "\u2014"),
          tags$dt(class = "col-sm-3", "code_sha"),     tags$dd(class = "col-sm-9", tags$code(run$code_sha %||% "\u2014"))
        ),
        if (!is.null(snap)) tagList(
          h5("Version"),
          tags$dl(class = "row",
            tags$dt(class = "col-sm-3", "label"),  tags$dd(class = "col-sm-9", snap$label),
            tags$dt(class = "col-sm-3", "status"), tags$dd(class = "col-sm-9",
              tags$span(class = sprintf("pill pill-%s", snap$status %||% "draft"), snap$status %||% "?")),
            tags$dt(class = "col-sm-3", "code_sha at creation"),
              tags$dd(class = "col-sm-9", tags$code(snap$code_sha_at_creation %||% "\u2014"))))
        else p(class = "small-muted", "No version \u2014 run used live config."),
        inputs_tbl
      )
    }

    # ---- Validation panel (inline builder + server-side search) -------
    .validation_body <- function(sr) {
      v <- read_run_validation(sr$path)
      if (is.null(v) || nrow(v) == 0)
        return(qdb_empty("No validation report for this run.", "clipboard-check"))
      v$passed <- as.logical(v$passed)
      n_pass <- sum(v$passed); n_fail <- sum(!v$passed)
      tagList(
        qdb_stats(list(
          list(k = "Checks", v = nrow(v), tone = "accent"),
          list(k = "Passed", v = n_pass, tone = "ok"),
          list(k = "Failed", v = n_fail, tone = if (n_fail > 0) "err" else ""))),
        reactable::reactableOutput(ns("validation_rt")))
    }

    output$validation_rt <- reactable::renderReactable({
      sr <- selected_run(); if (is.null(sr)) return(NULL)
      v <- read_run_validation(sr$path); if (is.null(v) || nrow(v) == 0) return(NULL)
      v$passed <- as.logical(v$passed)
      v$suppressed <- as.logical(v$suppressed %||% FALSE)
      eff <- v$effective_severity %||% v$severity
      st <- ifelse(v$passed, "PASS",
             ifelse(v$suppressed %in% TRUE, "SUPPR", toupper(as.character(eff))))
      df <- data.frame(status = st, stage = v$stage, id = v$id, context = v$context,
        description = v$description, message = v$message, stringsAsFactors = FALSE)
      df <- df[order(v$passed), , drop = FALSE]
      status_cell <- reactable::JS("
        function(c){var s=(c.value||'').toString().toUpperCase();var t='qp-muted';
          if(s==='PASS')t='qp-ok';else if(s==='ERROR')t='qp-err';
          else if(s==='WARN')t='qp-warn';else if(s==='INFO')t='qp-info';
          return '<span class=\"qdb-pill '+t+'\">'+s+'</span>';}")
      reactable::reactable(df, searchable = TRUE, highlight = TRUE, compact = TRUE,
        wrap = TRUE, defaultPageSize = 15, class = "qdb-rt",
        theme = reactable::reactableTheme(borderColor = "#f1eef5",
          highlightColor = "#f7f4f9", headerStyle = list(background = "#faf9fc")),
        columns = list(
          status = reactable::colDef(name = "Status", html = TRUE, cell = status_cell, minWidth = 90),
          stage = reactable::colDef(name = "Stage", minWidth = 80),
          id = reactable::colDef(name = "ID", minWidth = 130),
          context = reactable::colDef(name = "Context", minWidth = 110),
          description = reactable::colDef(name = "Description", minWidth = 220),
          message = reactable::colDef(name = "Message", minWidth = 260)))
    })

    # ---- Reconciliation panel (inline builder) ------------------------
    .reconciliation_body <- function(sr) {
      r <- read_run_reconciliation(sr$path)
      if (is.null(r) || is.null(r$md_path))
        return(p(class = "small-muted",
                  "No reconciliation for this run. Configure ",
                  tags$code("paths$reference_outputs"), " in config.yml to enable."))
      mm <- if (nrow(r$mismatches) == 0)
              p(class = "small-muted", "(no mismatch CSVs)")
            else .runs_html_table(r$mismatches[, c("file", "size_bytes")], max_rows = 100)
      tagList(
        h5("Reconciliation summary"),
        HTML(markdown::markdownToHTML(r$md_path, fragment.only = TRUE)),
        h5("Mismatches"), mm)
    }

    # ---- Overrides panel (inline builder) -----------------------------
    .overrides_body <- function(sr) {
      ov_dir <- file.path(sr$path, "overrides")
      if (!dir.exists(ov_dir))
        return(p(class = "small-muted",
                  "No overrides directory for this run. Pre-H12 runs do not ",
                  "record overrides at the run level."))
      files <- sort(list.files(ov_dir, pattern = "\\.csv$", full.names = TRUE))
      if (length(files) == 0) return(p(class = "small-muted", "(no override files)"))
      sections <- lapply(files, function(f) {
        df <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE),
                       error = function(e) NULL)
        if (is.null(df)) return(tagList(h6(basename(f)), p(class = "small-muted", "(could not read file)")))
        if (nrow(df) == 0) return(tagList(h6(basename(f)), p(class = "small-muted", "(none applied)")))
        tagList(h6(basename(f)), .runs_html_table(df, max_rows = 200))
      })
      do.call(tagList, sections)
    }

    # ---- Outputs: picked path + meta + preview ------------------------
    # output_meta and output_preview_html both depend on input$output_pick,
    # so they re-fire reliably when the user picks a file (even nested).
    .picked_output_path <- reactive({
      sr <- selected_run(); if (is.null(sr)) return(NULL)
      outputs_bump()
      pick <- input$output_pick
      if (is.null(pick) || !nzchar(pick)) return(NULL)
      paths <- list_run_outputs(sr$path)
      hit <- paths[basename(paths) == pick]
      if (length(hit) == 0) return(NULL)
      hit[1]
    })

    output$output_meta <- renderUI({
      full <- .picked_output_path()
      if (is.null(full)) return(tags$p(class = "small-muted", "(pick a file from the dropdown)"))
      if (!file.exists(full)) return(tags$p(class = "small-muted",
                                            sprintf("(file not found on disk: %s)", full)))
      info <- file.info(full)
      n_lines <- length(readLines(full, n = 5001L, warn = FALSE))
      hint <- if (n_lines > 5000) " (5000+; first 5000 shown)" else ""
      tags$p(class = "small-muted",
              sprintf("path: %s   |   size: %.1f KB   |   lines: %d%s",
                      full, info$size / 1024, n_lines, hint))
    })

    # CSV preview as an HTML table (renders reliably, unlike a nested DTOutput),
    # with SERVER-SIDE search: the row filter is applied here before rendering,
    # CSV preview as a reactable: built-in search (find a customer/contract),
    # sort, and pagination. Renders reliably because reactable returns a
    # self-contained widget. Driven by .picked_output_path() so it re-fires on
    # file pick or run change.
    output$output_preview_rt <- reactable::renderReactable({
      full <- .picked_output_path()
      if (is.null(full) || !file.exists(full))
        return(reactable::reactable(data.frame(File = "(pick a file)"),
               sortable = FALSE, class = "qdb-rt"))
      df <- tryCatch(
        utils::read.csv(full, nrows = 20000, stringsAsFactors = FALSE,
                        check.names = FALSE, na.strings = c("", "NA", "NaN")),
        error = function(e) NULL)
      if (is.null(df))
        return(reactable::reactable(data.frame(Error = sprintf("Could not read %s", basename(full))),
               sortable = FALSE, class = "qdb-rt"))
      if (nrow(df) == 0)
        return(reactable::reactable(data.frame(Note = "(empty file - 0 rows)"),
               sortable = FALSE, class = "qdb-rt"))
      # identifier columns as text so filters match exact ids
      id_pattern <- "(?i)(^id$|_id$|id_|Id$|ID$|^contract|customer|account|overlay)"
      for (i in grep(id_pattern, colnames(df), perl = TRUE)) df[[i]] <- as.character(df[[i]])
      reactable::reactable(
        df, searchable = FALSE, filterable = TRUE, highlight = TRUE, compact = TRUE,
        wrap = FALSE, resizable = TRUE, defaultPageSize = 15, showPageSizeOptions = TRUE,
        pageSizeOptions = c(15, 30, 60, 120), class = "qdb-rt",
        defaultColDef = reactable::colDef(minWidth = 120,
          headerStyle = list(background = "#faf9fc")),
        theme = reactable::reactableTheme(borderColor = "#f1eef5",
          highlightColor = "#f7f4f9", headerStyle = list(background = "#faf9fc")))
    })

    # ---- Apply overlay to the selected completed run -------------------
    .ovl_path_runs <- function()
      file.path(getOption("ifrs9.project_root", getwd()), "config", "overlays.yml")

    ovl_store_runs <- reactiveFileReader(
      1000, session, .ovl_path_runs(),
      function(p) tryCatch(read_overlays_yaml(p), error = function(e) list()))

    # Overlay choices, built fresh each time the detail block renders (i.e. each
    # time a run is selected) so the dropdown is ALWAYS populated - not only for
    # the first run. Reads the reactive store so it also reflects newly saved
    # overlays. Called inline from detail_block's selectInput.
    .overlay_choices <- function() {
      bs <- tryCatch(ovl_store_runs(), error = function(e) list())
      ch <- c("(select an overlay)" = "")
      for (b in bs) {
        st <- b$status %||% "draft"
        ch <- c(ch, stats::setNames(b$id, sprintf("%s [%s]", b$id, toupper(st))))
      }
      ch
    }

    observeEvent(input$do_apply_overlay, {
      sr <- selected_run()
      id <- input$apply_overlay_pick %||% ""
      if (is.null(sr)) { showNotification("Select a run first.", type = "warning"); return() }
      if (!nzchar(id)) { showNotification("Pick an overlay to apply.", type = "warning"); return() }
      b <- tryCatch(get_overlay(id, .ovl_path_runs()), error = function(e) NULL)
      if (is.null(b)) { showNotification("Overlay not found.", type = "error"); return() }
      res <- tryCatch(apply_overlay_to_run(sr$path, b), error = function(e) e)
      if (inherits(res, "error")) {
        output$apply_overlay_result <- renderUI(div(class = "alert alert-danger",
          style = "margin-top:8px", conditionMessage(res)))
      } else if (isFALSE(res$ok)) {
        output$apply_overlay_result <- renderUI(tagList(
          div(class = "alert alert-danger", style = "margin-top:8px",
              icon("triangle-exclamation"),
              sprintf(" Conflict: %d contract(s) matched by more than one rule. Overlay not applied - resolve on the ECL overlays page.",
                      nrow(res$conflicts))),
          .runs_html_table(utils::head(res$conflicts, 10), max_rows = 10)))
      } else {
        t <- res$totals; f <- function(x) format(round(x), big.mark = ",")
        st <- toupper(res$status %||% "draft")
        output$apply_overlay_result <- renderUI(div(class = "alert alert-success",
          style = "margin-top:8px",
          icon("circle-check"),
          sprintf(" Overlay '%s' [%s] applied. Model %s \u2192 overlay +%s \u2192 final %s (+%.2f%%). Written: %s",
                  id, st, f(t$model), f(t$overlay), f(t$final),
                  100 * t$overlay / max(t$model, 1), basename(res$out_report))))
        outputs_bump(outputs_bump() + 1)   # new overlay CSV -> refresh Outputs list
        showNotification("Overlay applied to run. See it in the Outputs tab.", type = "message")
      }
    })

    # List overlays already applied to the selected run, each with a Remove
    # button so a wrong overlay can be cleared and the corrected one reapplied.
    output$applied_overlays_list <- renderUI({
      sr <- selected_run(); if (is.null(sr)) return(NULL)
      outputs_bump()
      ap <- tryCatch(list_applied_overlays(sr$path), error = function(e) NULL)
      if (is.null(ap) || nrow(ap) == 0)
        return(p(class = "small-muted", style = "margin-top:6px",
                 "None applied yet. Applying one writes a report alongside the model report."))
      rows <- lapply(seq_len(nrow(ap)), function(i) {
        idv <- ap$overlay_id[i]
        div(style = "display:flex;align-items:center;gap:10px;padding:6px 0;border-bottom:1px solid #f1f3f5",
          div(style = "flex:1",
            tags$strong(idv),
            span(class = "small-muted", sprintf("  \u2014 %s  \u00b7  applied %s",
                                                ap$report_file[i], ap$applied_at[i]))),
          actionButton(ns(paste0("rmovl_", i)), "Remove", class = "btn-sm btn-outline-danger",
            onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                              ns("remove_applied_click"), idv)))
      })
      do.call(tagList, rows)
    })

    observeEvent(input$remove_applied_click, {
      idv <- input$remove_applied_click
      showModal(modalDialog(
        title = "Remove applied overlay",
        sprintf("Remove the overlaid output for '%s' from this run? The model report and everything else are untouched. You can then apply a corrected overlay.", idv),
        footer = tagList(modalButton("Cancel"),
          actionButton(ns("remove_applied_confirm"), "Remove", class = "btn-danger")),
        easyClose = TRUE))
      session$userData$applied_rm_id <- idv
    })
    observeEvent(input$remove_applied_confirm, {
      removeModal()
      sr <- selected_run(); idv <- session$userData$applied_rm_id
      if (is.null(sr) || is.null(idv)) return()
      res <- tryCatch(remove_applied_overlay(sr$path, idv), error = function(e) e)
      if (inherits(res, "error")) {
        showNotification(paste("Remove failed:", conditionMessage(res)), type = "error")
      } else if (isFALSE(res$ok)) {
        showNotification("Nothing to remove for that overlay.", type = "warning")
      } else {
        showNotification(sprintf("Removed: %s", paste(res$removed, collapse = ", ")), type = "message")
        output$apply_overlay_result <- renderUI(NULL)
        outputs_bump(outputs_bump() + 1)   # refresh applied list + Outputs
      }
    })

    # Keep detail outputs un-suspended so they render on first tab view / after
    # a re-select rather than staying blank. Called last, after all outputs are
    # defined (outputOptions requires the output to exist).
    # Keep the detail block and the file-picker-driven outputs alive even when
    # their tab/panel is hidden, so they render on first view.
    for (.o in c("detail_block", "output_meta", "output_preview_rt",
                 "validation_rt", "apply_overlay_result",
                 "applied_overlays_list")) {
      try(outputOptions(output, .o, suspendWhenHidden = FALSE), silent = TRUE)
    }
  })
}


# ---- Tiny helper ---------------------------------------------------------
list_runs.empty <- function() {
  tibble::tibble(
    run_id = character(), path = character(),
    started_at = character(), finished_at = character(),
    duration_seconds = numeric(),
    user = character(), code_sha = character(),
    snapshot_label = character(), snapshot_status = character(),
    n_outputs = integer(), n_validation_failures = integer(),
    has_reconciliation = logical()
  )
}
