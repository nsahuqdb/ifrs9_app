# =============================================================================
# app/modules/mod_run_trigger.R
#
# H12: phased run workflow with mid-run pause for overrides.
#
# UX flow:
#   Step 1 — Pre-run check (input-stage validators only)
#   Step 2 — Click "Start run" → run_etl_phase1 runs synchronously
#               (~10s for 6.6k contracts). State held in module reactiveVal.
#   Step 3 — PAUSE PAGE: cm_view shown as filterable DataTable. User adds
#               overrides to a buffer (separate reactive). Three override
#               types: rating, stage, restructuring. Each row has a
#               required reason.
#   Step 4 — Click "Continue" → run_etl_phase2 runs with overrides
#               applied → outputs written → run lands as pending_approval.
#
# State machine (held in reactiveVal phase_state):
#   "idle"          — no run started, show config picker
#   "phase1_done"   — phase 1 finished, show pause page
#   "phase2_done"   — phase 2 finished, show summary
# =============================================================================

mod_run_trigger_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(12,
      h3("Run pipeline"),
      p(class = "small-muted",
        "Pick a config version (or the default config), run a fast pre-check, then start ",
        "the run. The pipeline pauses after computing customer-level views ",
        "to let you make overrides. After Continue, the run finishes and ",
        "lands as ", tags$em("pending_approval"), ".")
    )),
    uiOutput(ns("page_body"))
  )
}


mod_run_trigger_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # State machine
    phase_state <- reactiveVal("idle")
    phase1_state <- reactiveVal(NULL)
    phase2_result <- reactiveVal(NULL)
    calc_env_rv <- reactiveVal(NULL)   # archived calculator env for this run (or NULL = live code)
    pre_run_results <- reactiveVal(NULL)
    # Pricing readiness from the pre-run check: pre_run_readiness()'s result,
    # or list(error = ...) when it could not run. Cleared with the check.
    readiness_res <- reactiveVal(NULL)
    refresh_snapshots <- reactiveVal(0)

    # Pre-run check is computed against a specific (run_type, snapshot)
    # pair: unofficial + live config validates against the live config
    # file; official + an approved snapshot validates against the frozen
    # snapshot. If the user changes EITHER setting after the check has
    # passed, the previous result is stale and could let them bypass the
    # gate (e.g. validate against unofficial-relaxed rules, then switch
    # to official and run). Clear pre_run_results whenever either input
    # changes so the gate forces a fresh check.
    # Resolve the static dir that actually holds scenario_severity.csv, trying
    # the snapshot dir (if picked), the config'd static_dir, and common
    # fallbacks. Deployed environments don't always have data-raw/static at
    # getwd(), which previously left the scenario list empty (only "weighted").
    .scenario_csv_path <- function(pick) {
      root <- getOption("ifrs9.project_root", getwd())
      cfg_static <- tryCatch({
        cfg <- yaml::read_yaml(file.path(root, "config.yml"))
        sd <- cfg$paths$static_dir %||% "data-raw/static"
        if (!grepl("^(/|[A-Za-z]:)", sd)) file.path(root, sd) else sd
      }, error = function(e) file.path(root, "data-raw/static"))
      cands <- c(
        if (!is.null(pick) && pick != "__LIVE__")
          tryCatch(file.path(snapshot_paths(pick, snaps_root())$static_dir,
                             "scenario_severity.csv"), error = function(e) NULL),
        file.path(cfg_static, "scenario_severity.csv"),
        file.path(root, "data-raw", "static", "scenario_severity.csv"),
        file.path(root, "inst", "static", "scenario_severity.csv"),
        file.path(getwd(), "data-raw", "static", "scenario_severity.csv"))
      cands <- cands[!vapply(cands, is.null, logical(1))]
      hit <- cands[file.exists(cands)]
      if (length(hit) > 0) hit[1] else NA_character_
    }

    observe({
      pick <- input$snapshot_pick %||% "__LIVE__"
      rt <- input$run_type %||% "unofficial"      # explicit dependency: re-fire on run_type
      csv <- .scenario_csv_path(pick)
      sev <- if (!is.na(csv))
        tryCatch(utils::read.csv(csv, stringsAsFactors = FALSE,
                                 fileEncoding = "UTF-8-BOM"),
                 error = function(e) tryCatch(utils::read.csv(csv, stringsAsFactors = FALSE),
                                              error = function(e2) NULL))
      else NULL
      scen <- if (!is.null(sev) && "scenario" %in% colnames(sev))
        as.character(sev$scenario) else character(0)
      scen <- scen[!is.na(scen) & nzchar(trimws(scen))]
      ch <- c("Weighted (probability-weighted PDs)" = "weighted")
      if (rt != "official" && length(scen) > 0)
        ch <- c(ch, setNames(scen, paste0("Scenario: ", scen)))
      sel <- "weighted"
      if (!(sel %in% ch)) sel <- "weighted"
      # scenario picker removed - every run covers all scenarios
    })

    observeEvent(input$run_type, {
      if (!is.null(pre_run_results())) {
        pre_run_results(NULL); readiness_res(NULL)
        showNotification(
          "Run type changed — pre-run check cleared. Please run pre-run check again.",
          type = "warning", duration = 6)
      }
    }, ignoreInit = TRUE)

    # Couple run purpose to run type:
    #   official   -> regulatory (only)
    #   unofficial -> non-regulatory | impact
    observeEvent(input$run_type, {
      rt <- input$run_type %||% "unofficial"
      if (rt == "official") {
        updateSelectInput(session, "run_purpose",
                          choices = c("Regulatory" = "regulatory"),
                          selected = "regulatory")
      } else {
        updateSelectInput(session, "run_purpose",
                          choices = c("Non-regulatory" = "non_regulatory",
                                      "Impact analysis" = "impact"),
                          selected = isolate(input$run_purpose) %||% "non_regulatory")
      }
    }, ignoreInit = FALSE)

    # Populate calculator version dropdown from the registry (active first).
    observe({
      cv <- tryCatch(list_calculator_versions(), error = function(e) NULL)
      choices <- c("(active)" = "")
      sel <- ""
      if (!is.null(cv) && nrow(cv) > 0) {
        labels <- ifelse(cv$active,
                         sprintf("%s — ACTIVE", cv$label),
                         cv$label)
        ch <- stats::setNames(cv$id, labels)
        # active first
        ord <- order(!cv$active)
        choices <- c(choices, ch[ord])
      }
      updateSelectInput(session, "calculator_pick", choices = choices,
                        selected = sel)
    })

    # Overlays store, auto-refreshed when config/overlays.yml changes.
    ovl_store <- reactiveFileReader(
      1000, session,
      file.path(getOption("ifrs9.project_root", getwd()), "config", "overlays.yml"),
      function(p) tryCatch(read_overlays_yaml(p), error = function(e) list()))

    # Populate the overlay dropdown. For OFFICIAL (regulatory) runs only
    # approved overlays are offered; for UNOFFICIAL runs drafts/pending are
    # offered too (so they can be tested), each tagged with its status.
    observe({
      bs <- ovl_store()
      rt <- input$run_type %||% "unofficial"
      ch <- c("(none)" = "")
      for (b in bs) {
        st <- b$status %||% "draft"
        if (rt == "official" && st != "approved") next
        ch <- c(ch, stats::setNames(b$id, sprintf("%s [%s]", b$id, toupper(st))))
      }
      # overlay picker removed - overlays are applied from the Runs page
    })

    # output$overlay_meta removed with the picker.


    output$calculator_meta <- renderUI({
      id <- input$calculator_pick %||% ""
      rec <- tryCatch(calculator_version_for_run(id = if (nzchar(id)) id else NULL),
                      error = function(e) NULL)
      if (is.null(rec)) return(NULL)
      # Will this selection run archived code (isolated) or the live code?
      # The "(active)" default (empty id) always runs the live deployed code.
      runs_archived <- if (!nzchar(id)) FALSE else tryCatch({
        is.environment(make_calc_run_env(id))
      }, error = function(e) FALSE)
      exec_badge <- if (isTRUE(runs_archived)) {
        span(class = "pill pill-info", "runs this version's archived code")
      } else {
        span(class = "pill pill-approved", "runs the live deployed code")
      }
      div(class = "small-muted", style = "margin-top:-0.4em; margin-bottom:0.6em;",
          sprintf("Calculator %s · fingerprint %s ",
                  rec$label %||% rec$id %||% "?",
                  substr(rec$code_hash %||% "?", 1, 10)), exec_badge)
    })

    observeEvent(input$snapshot_pick, {
      if (!is.null(pre_run_results())) {
        pre_run_results(NULL); readiness_res(NULL)
        showNotification(
          "Version selection changed — pre-run check cleared. Please run pre-run check again.",
          type = "warning", duration = 6)
      }
    }, ignoreInit = TRUE)

    # ---- Input source state (H16a) -------------------------------------
    # input_validation: NULL until user clicks "Validate inputs". Then a
    #   tibble(check, status, detail) — status PASS or FAIL per check.
    # input_dir_override: the resolved input directory for THIS run if
    #   non-default. NULL when source = "configured".
    # input_source_meta: the {kind, details} dict that gets passed to
    #   run_etl_phase1 and persisted into reports/input_source.yml.
    # The "Pre-run check" button is gated on input_validation being
    #   non-NULL and having zero FAILs (i.e. user has explicitly run
    #   the structural check and it passed).
    input_validation   <- reactiveVal(NULL)
    input_dq           <- reactiveVal(NULL)   # data-quality preview (advisory, non-gating)
    input_header_strips <- reactiveVal(integer(0))  # auto-fixed repeated-header counts
    input_dir_override <- reactiveVal(NULL)
    input_source_meta  <- reactiveVal(list(kind = "configured",
                                            details = list()))

    # Override buffers — three editable tables held in memory
    overrides_rating <- reactiveVal(.empty_override_buf("rating"))
    overrides_stage  <- reactiveVal(.empty_override_buf("stage"))
    overrides_restr  <- reactiveVal(.empty_override_buf("restructuring"))

    snaps_root <- function() {
      proj_root <- getOption("ifrs9.project_root", getwd())
      getOption("ifrs9.snapshots_dir",
                file.path(proj_root, "config_snapshots"))
    }

    # ============== TOP-LEVEL PAGE ROUTER =============================
    output$page_body <- renderUI({
      switch(phase_state(),
        "idle"        = .ui_idle(ns),
        "phase1_done" = .ui_pause(ns),
        "phase2_done" = .ui_summary(ns),
        .ui_idle(ns)
      )
    })

    # ============== STEP 1: IDLE PAGE =================================
    # Snapshot dropdown — rendered reactively. Re-fires whenever:
    #   * the local refresh_snapshots() reactiveVal is bumped (used by
    #     this module after a phase 2 run), OR
    #   * any other module bumps session$userData$snapshots_changed
    #     (e.g. snapshot manager promotes a snapshot, approval queue
    #     approves a snapshot). Without this cross-module signal,
    #     snapshots created or approved during the session don't
    #     appear here until the app is restarted.
    output$snapshot_pick_ui <- renderUI({
      refresh_snapshots()
      if (!is.null(session$userData$snapshots_changed)) {
        session$userData$snapshots_changed()
      }
      s <- tryCatch(list_snapshots(snaps_root()),
                     error = function(e) list_snapshots.empty())
      default_choice <- c("(default config)" = "__LIVE__")
      active_label <- NA_character_
      if (nrow(s) > 0) {
        # Newest version first, so the latest sits at the top of the list.
        s <- s[order(s$created_at %||% rep("", nrow(s)), decreasing = TRUE),
               , drop = FALSE]
        # Active version = the latest approved one (s is already newest-first).
        # It is pinned to the very top and pre-selected.
        appr <- s[(s$status %||% "") == "approved", , drop = FALSE]
        if (nrow(appr) > 0) active_label <- appr$label[1]
        labels <- sprintf("%s [%s]%s — %s",
                           s$label, s$status %||% "?",
                           ifelse(!is.na(active_label) & s$label == active_label,
                                  " · active", ""),
                           substr(s$description %||% "", 1, 50))
        ver_choices <- setNames(s$label, labels)
        if (!is.na(active_label)) {
          ai <- which(s$label == active_label)
          idx <- c(ai, setdiff(seq_len(nrow(s)), ai))        # active pinned top
          choices <- c(setNames(s$label[idx], labels[idx]),
                       default_choice)                        # default at bottom
          default_pick <- active_label
        } else {
          choices <- c(default_choice, ver_choices)           # no approved -> default top
          default_pick <- "__LIVE__"
        }
      } else {
        choices <- default_choice
        default_pick <- "__LIVE__"
      }
      selectInput(ns("snapshot_pick"), "Config version",
                   choices = choices,
                   selected = isolate(input$snapshot_pick) %||% default_pick)
    })

    output$snapshot_meta <- renderUI({
      pick <- input$snapshot_pick
      if (is.null(pick) || pick == "__LIVE__") {
        return(p(class = "small-muted",
                  "Default config: reads ", tags$code("config/"), " and ",
                  tags$code("data-raw/static/"), " as they currently sit on disk."))
      }
      meta <- tryCatch(read_snapshot_metadata(pick, snaps_root()),
                        error = function(e) NULL)
      if (is.null(meta)) return(p(class = "small-muted",
                                    "(could not read version metadata)"))
      tags$dl(class = "row",
        tags$dt(class = "col-sm-4", "status"),
          tags$dd(class = "col-sm-8",
            tags$span(class = sprintf("pill pill-%s", meta$status %||% "draft"),
                      meta$status %||% "?")),
        tags$dt(class = "col-sm-4", "description"),
          tags$dd(class = "col-sm-8", meta$description %||% "—")
      )
    })

    # ============== INPUT SOURCE (H16a) =====================================
    # When the source kind changes, reset the validation result. The
    # user has to revalidate after switching source.
    observeEvent(input$input_source_kind, {
      input_validation(NULL)
      input_dir_override(NULL)
      input_source_meta(list(kind = input$input_source_kind, details = list()))
    }, ignoreInit = TRUE)

    drop_root_path <- reactive({
      cfg_path <- file.path(getOption("ifrs9.project_root", getwd()),
                              "config.yml")
      cfg <- tryCatch(load_run_config(cfg_path), error = function(e) NULL)
      if (is.null(cfg)) return("")
      cfg$paths$data_drop_root %||% ""
    })

    # Renders a different sub-UI based on which input-source radio is
    # selected. For "configured" we just show a hint about the path;
    # for "drop_folder" a dropdown of available drops; for "upload" a
    # fileInput accepting .zip.
    output$input_source_picker <- renderUI({
      kind <- input$input_source_kind %||% "configured"
      if (kind == "configured") {
        cfg_path <- file.path(getOption("ifrs9.project_root", getwd()),
                                "config.yml")
        cfg <- tryCatch(load_run_config(cfg_path), error = function(e) NULL)
        path <- if (is.null(cfg)) "(unable to read config)" else cfg$paths$input_dir
        return(p(class = "small-muted",
                  "Reads from ", tags$code(path)))
      }
      if (kind == "drop_folder") {
        drop_root <- drop_root_path()
        drops <- tryCatch(list_data_drops(drop_root),
                           error = function(e) NULL)
        if (is.null(drops) || nrow(drops) == 0) {
          return(div(class = "alert alert-warning",
                      "No data drop folders found at ",
                      tags$code(drop_root %||% "(unset)"),
                      ". Set ", tags$code("paths.data_drop_root"),
                      " in config.yml to point at the data team's drop ",
                      "folder, or use the configured directory or zip ",
                      "upload instead."))
        }
        labels <- ifelse(drops$looks_complete,
                          sprintf("%s — %d files (complete)",
                                  drops$name, drops$n_files),
                          sprintf("%s — %d files (INCOMPLETE)",
                                  drops$name, drops$n_files))
        # Default selection: most recent (drops are pre-sorted newest first)
        return(tagList(
          selectInput(ns("drop_pick"), "Pick a drop folder",
                       choices = setNames(drops$path, labels),
                       selected = drops$path[1], width = "650px"),
          p(class = "small-muted",
            "Listing immediate subfolders of ",
            tags$code(drop_root), ".")
        ))
      }
      if (kind == "upload") {
        # Surface the configured upload limit so a user hitting it has
        # an immediate breadcrumb to fix it (config.yml::run.max_upload_size_mb).
        max_bytes <- getOption("shiny.maxRequestSize", 5 * 1024 * 1024)
        max_mb <- round(max_bytes / 1024 / 1024)
        return(tagList(
          fileInput(ns("zip_upload"), "Choose a zip file",
                     accept = c(".zip", "application/zip",
                                 "application/x-zip-compressed")),
          p(class = "small-muted",
            sprintf("Upload limit: %d MB. ", max_mb),
            "The zip should contain the 12 input files at the top level, ",
            "or wrapped in a single folder (e.g. ",
            tags$code("Input/AccountMaster.xlsx"),
            " or ", tags$code("Input/AccountMaster.xls"), "). ",
            "Each file is accepted as either Office Open XML (.xlsx) or ",
            "Oracle SQL*Plus HTML (.xls) — the loader auto-detects the ",
            "format from the file's byte signature. ",
            "Raise the limit in ", tags$code("config.yml"),
            " under ", tags$code("run.max_upload_size_mb"),
            " if you need more.")
        ))
      }
      NULL
    })

    # When user clicks "Validate inputs", resolve the directory based on
    # source kind, run the structural check, store the result.
    observeEvent(input$do_validate_inputs, {
      kind <- input$input_source_kind %||% "configured"
      resolved_dir <- NULL
      meta <- list(kind = kind, details = list())

      if (kind == "configured") {
        cfg_path <- file.path(getOption("ifrs9.project_root", getwd()),
                                "config.yml")
        cfg <- tryCatch(load_run_config(cfg_path), error = function(e) NULL)
        if (is.null(cfg)) {
          showNotification("Could not read config.yml", type = "error")
          return()
        }
        resolved_dir <- cfg$paths$input_dir
        meta$details <- list(path = resolved_dir)
      } else if (kind == "drop_folder") {
        sel <- input$drop_pick
        if (is.null(sel) || !nzchar(sel)) {
          showNotification("Pick a drop folder first.", type = "warning")
          return()
        }
        resolved_dir <- sel
        meta$details <- list(path = sel, drop_name = basename(sel))
      } else if (kind == "upload") {
        upload <- input$zip_upload
        if (is.null(upload) || nrow(upload) == 0) {
          showNotification("Pick a zip file first.", type = "warning")
          return()
        }
        result <- tryCatch(
          acquire_inputs_from_zip(upload$datapath[1]),
          error = function(e) e
        )
        if (inherits(result, "error")) {
          showNotification(paste("Zip extract failed:",
                                   conditionMessage(result)),
                            type = "error", duration = 10)
          return()
        }
        resolved_dir <- result$path
        meta$details <- list(
          path           = resolved_dir,
          source_zip     = upload$name[1],
          extracted_at   = result$extracted_at
        )
      }

      withProgress(message = "Validating inputs", value = 0.3, {
        check <- tryCatch(validate_input_directory(resolved_dir),
                           error = function(e) NULL)
        setProgress(1)
      })
      if (is.null(check)) {
        showNotification("Validation failed unexpectedly.", type = "error")
        input_validation(NULL); return()
      }

      input_validation(check)
      n_fail <- sum(check$status == "FAIL")
      if (n_fail == 0) {
        input_dir_override(if (kind == "configured") NULL else resolved_dir)
        input_source_meta(meta)
        showNotification(sprintf("Inputs OK (%d checks passed). Pre-run check is enabled.",
                                   nrow(check)),
                          type = "message", duration = 5)
      } else {
        input_dir_override(NULL)
        input_source_meta(list(kind = "configured", details = list()))
        showNotification(sprintf("%d structural check(s) failed. Fix and re-validate.",
                                   n_fail),
                          type = "warning", duration = 8)
      }

      # Data-quality preview: run the full INPUT_* validator suite now so the
      # operator sees duplicates / blank IDs / unknown ratings AT THIS STEP,
      # not only in the final validation report. Advisory only — these do NOT
      # gate the Pre-run check (they are re-run, and gated per config, during
      # the actual run). Only runs if the directory resolved.
      if (!is.null(resolved_dir) && dir.exists(resolved_dir)) {
        dq <- tryCatch(
          withProgress(message = "Checking input data quality", value = 0.5, {
            root <- getOption("ifrs9.project_root", getwd())
            rc <- tryCatch(load_run_config(file.path(root, "config.yml")),
                           error = function(e) NULL)
            # Honour the selected config version: when a snapshot is picked,
            # validate against ITS static files (so e.g. an NFG code added in
            # a draft clears the coverage error here too, not only in the
            # pre-run check). Default config -> live data-raw/static.
            pick_now <- input$snapshot_pick %||% "__LIVE__"
            static_dir <- if (pick_now != "__LIVE__") {
              tryCatch(snapshot_paths(pick_now, snaps_root())$static_dir,
                        error = function(e)
                          rc$paths$static_dir %||% file.path(root, "data-raw/static"))
            } else {
              rc$paths$static_dir %||% file.path(root, "data-raw/static")
            }
            reset_header_strip_log()
            inputs <- read_all_inputs(resolved_dir, verbose = FALSE)
            input_header_strips(get_header_strip_log())
            static <- load_static_reference(static_dir)
            # Adopt the reporting date from the input data before validating,
            # so a stale config date never trips the consistency check.
            if (exists("apply_input_extract_date")) {
              rc <- apply_input_extract_date(rc, inputs)
            }
            # Prepopulate the "Portfolio (as-of) date" field with the date the
            # uploaded data actually carries, so the operator never sets it by
            # hand. Falls back to today only if the inputs carry no date.
            if (exists("resolve_input_extract_date")) {
              .in_date <- resolve_input_extract_date(inputs)
              if (!is.na(.in_date)) {
                updateDateInput(session, "portfolio_date", value = .in_date)
              }
            }
            run_validation_suite("INPUT", build_input_validators(),
                                 args = list(inputs = inputs, static = static,
                                             run_cfg = rc),
                                 verbose = FALSE)
          }),
          error = function(e) NULL)
        input_dq(dq)
      } else {
        input_dq(NULL)
        input_header_strips(integer(0))
      }
    })

    output$input_validation_result <- renderUI({
      v <- input_validation()
      if (is.null(v)) {
        return(p(class = "small-muted", style = "margin-top: 0.5em;",
                  "Click Validate inputs to enable Pre-run check."))
      }
      n_fail <- sum(v$status == "FAIL")
      n_pass <- sum(v$status == "PASS")
      pill <- if (n_fail == 0) {
        sprintf('<span class="pill pill-pass">All %d checks passed</span>',
                n_pass)
      } else {
        sprintf('<span class="pill pill-error">%d FAIL, %d PASS — Pre-run check disabled</span>',
                n_fail, n_pass)
      }
      # When there are failures, also surface them as an inline
      # bulleted list so the user sees what's missing without scrolling
      # the table or squinting at it. The full table renders below for
      # detail.
      fail_block <- NULL
      if (n_fail > 0) {
        failed <- v[v$status == "FAIL", , drop = FALSE]
        fail_block <- div(style = "margin-top: 0.5em;",
          tags$strong("Failures:"),
          tags$ul(class = "small-muted",
            lapply(seq_len(nrow(failed)), function(i) {
              tags$li(
                tags$code(failed$check[i]),
                if (nzchar(as.character(failed$detail[i] %||% ""))) {
                  tags$span(class = "small-muted",
                            sprintf(" — %s", failed$detail[i]))
                } else NULL
              )
            })
          )
        )
      }
      tagList(
        div(style = "margin-top: 0.75em;", HTML(pill)),
        fail_block,
        DT::DTOutput(ns("input_validation_table"))
      )
    })

    output$input_validation_table <- DT::renderDT({
      v <- input_validation()
      if (is.null(v) || nrow(v) == 0) return(NULL)
      # Only surface the failed checks; passes are counted in the summary
      # line above and recorded in the run's validation.csv.
      v <- v[v$status != "PASS", , drop = FALSE]
      if (nrow(v) == 0) return(NULL)
      v$status_pill <- '<span class="pill pill-error">FAIL</span>'
      DT::datatable(
        data.frame(status = v$status_pill,
                    check  = v$check,
                    detail = v$detail,
                    stringsAsFactors = FALSE),
        rownames = FALSE,
        escape = FALSE,
        class = "narrow-table compact",
        options = list(pageLength = 12, dom = "tip")
      )
    })

    # ---- Data-quality preview (advisory; does NOT gate the run) ----------
    output$input_dq_result <- renderUI({
      v <- input_dq()
      strips <- input_header_strips()
      # Informational note about auto-fixed repeated header rows.
      strip_block <- NULL
      if (length(strips) > 0 && sum(strips) > 0) {
        total <- sum(strips)
        per_file <- paste(sprintf("%s (%d)", names(strips), as.integer(strips)),
                          collapse = ", ")
        strip_block <- div(style = "margin-top:1em;",
          HTML(sprintf('<span class="pill pill-pass">Auto-fixed: removed %d repeated header row(s)</span>',
                       total)),
          p(class = "small-muted", style = "margin-top:0.4em;",
            sprintf("Repeated header rows were detected and removed from: %s. ",
                    per_file),
            "These recur at whatever interval the source export uses (e.g. ",
            "every 10,000 or 50,000 rows) and are stripped automatically ",
            "before validation."))
      }
      if (is.null(v) || nrow(v) == 0) return(strip_block)
      passed <- as.logical(v$passed)
      sevcol <- if ("effective_severity" %in% names(v)) "effective_severity" else "severity"
      n_fail <- sum(!passed, na.rm = TRUE)
      if (n_fail == 0) {
        return(tagList(strip_block, div(style = "margin-top:1em;",
          HTML('<span class="pill pill-pass">Data-quality checks: all clear</span>'))))
      }
      by_sev <- table(v[[sevcol]][!passed])
      sev_txt <- paste(sprintf("%s %d", names(by_sev), as.integer(by_sev)),
                       collapse = " · ")
      tagList(
        strip_block,
        div(style = "margin-top:1em;",
          HTML(sprintf('<span class="pill pill-pending">Data-quality preview: %d finding(s) (%s)</span>',
                       n_fail, sev_txt)),
          p(class = "small-muted", style = "margin-top:0.4em;",
            "Advisory \u2014 these do not block the run. They are re-checked ",
            "during the run and gated per the on_validation_error setting. ",
            "Duplicates show the offending rows so you can fix them at source.")),
        DT::DTOutput(ns("input_dq_table"))
      )
    })

    output$input_dq_table <- DT::renderDT({
      v <- input_dq()
      if (is.null(v) || nrow(v) == 0) return(NULL)
      passed <- as.logical(v$passed)
      fails <- v[!passed, , drop = FALSE]
      if (nrow(fails) == 0) return(NULL)
      sevcol <- if ("effective_severity" %in% names(fails)) "effective_severity" else "severity"
      detcol <- intersect(c("details", "description"), names(fails))[1]
      sev_pill <- function(s) {
        cls <- ifelse(s == "ERROR", "pill-error",
               ifelse(s == "WARN", "pill-pending", "pill-info"))
        sprintf('<span class="pill %s">%s</span>', cls, s)
      }
      # detail: prefer the validator's rendered message (duplicates w/ rows)
      detail <- vapply(seq_len(nrow(fails)), function(i) {
        d <- fails[[detcol]][i]
        if (is.list(d)) d <- d[[1]]
        msg <- tryCatch({
          if (is.list(d) && !is.null(d$message)) d$message else as.character(d)
        }, error = function(e) "")
        if (length(msg) == 0 || is.na(msg) || !nzchar(msg))
          msg <- as.character(fails$description[i] %||% "")
        msg
      }, character(1))
      # One-line "where to look": config-coverage findings carry the config
      # file to edit; everything else points at the input file (context).
      src <- vapply(seq_len(nrow(fails)), function(i) {
        d <- fails[[detcol]][i]
        if (is.list(d)) d <- d[[1]]
        cfg <- if (is.list(d) && !is.null(d$config)) as.character(d$config) else NA_character_
        if (!is.na(cfg) && nzchar(cfg)) {
          sprintf("Fix in CONFIG: edit %s in the Config manager (draft version), then approve and re-run", cfg)
        } else {
          ctx <- as.character(fails$context[i] %||% "")
          if (nzchar(ctx) && !is.na(ctx)) sprintf("Look in INPUT file: %s", ctx) else ""
        }
      }, character(1))
      detail <- ifelse(nzchar(src),
                       paste0(detail, " \u2014 ", src), detail)
      DT::datatable(
        data.frame(severity = vapply(fails[[sevcol]], sev_pill, character(1)),
                    check    = fails$id,
                    context  = fails$context %||% "",
                    detail   = detail,
                    stringsAsFactors = FALSE),
        rownames = FALSE, escape = FALSE, class = "narrow-table compact",
        options = list(pageLength = 12, dom = "tip", scrollX = TRUE)
      )
    })

    # Pre-run check button: only enabled after structural validation
    # of the chosen input source has passed.
    output$pre_run_button_slot <- renderUI({
      v <- input_validation()
      can_run <- !is.null(v) && sum(v$status == "FAIL") == 0
      if (!can_run) {
        return(tags$button(
          id = ns("do_pre_run"),
          class = "btn btn-primary action-button",
          disabled = NA,
          style = "opacity: 0.5; cursor: not-allowed;",
          icon("magnifying-glass"), " Pre-run check",
          tags$span(class = "small-muted", style = "margin-left: 0.5em;",
                    "(validate inputs first)")
        ))
      }
      actionButton(ns("do_pre_run"), "Pre-run check",
                    icon = icon("magnifying-glass"),
                    class = "btn-primary")
    })

    # Pre-run check
    observeEvent(input$do_pre_run, {
      pick <- input$snapshot_pick
      cfg_path <- file.path(getOption("ifrs9.project_root", getwd()),
                              "config.yml")
      ovr <- input_dir_override()
      withProgress(message = "Pre-run check", value = 0.3, {
        res <- tryCatch(
          if (pick == "__LIVE__") {
            pre_run_check(config_path = cfg_path, verbose = FALSE,
                            input_dir_override = ovr)
          } else {
            pre_run_check(snapshot = pick, verbose = FALSE,
                            input_dir_override = ovr)
          },
          error = function(e) {
            showNotification(paste("Pre-run failed:", conditionMessage(e)),
                              type = "error", duration = 10)
            NULL
          }
        )
        setProgress(1)
      })
      pre_run_results(res)
      # Pricing readiness: build the LIC files into a temporary folder and stop
      # before pricing, so the contracts that would get no ECL, or come out
      # blank in LIC, are known before the run starts -- a collateral id
      # missing from the allocation file, a rating with no PD curve, a
      # contract with no EIR. Nothing is written to runs/.
      readiness_res(NULL)
      if (!is.null(res)) {
        rd <- tryCatch(
          withProgress(message = "Pricing readiness: building the LIC files",
                       value = 0.3, {
            out <- if (pick == "__LIVE__") {
              pre_run_readiness(config_path = cfg_path, input_dir_override = ovr)
            } else {
              pre_run_readiness(snapshot = pick, input_dir_override = ovr)
            }
            setProgress(1)
            out
          }),
          error = function(e) list(error = conditionMessage(e)))
        readiness_res(rd)
      }
    })

    # Unsuppressed READY_* errors from the readiness check: they gate Start
    # exactly as the pre-run ERRORs do.
    .ready_errors <- function(rd) {
      v <- rd$validation
      if (is.null(v) || nrow(v) == 0 || !"stage" %in% names(v)) return(v[0, ])
      eff <- if ("effective_severity" %in% names(v)) v$effective_severity else v$severity
      v[v$stage == "READY" & !as.logical(v$passed) & eff == "ERROR", , drop = FALSE]
    }

    output$readiness_status <- renderUI({
      rd <- readiness_res()
      if (is.null(rd)) return(NULL)
      if (!is.null(rd$error)) {
        return(div(class = "alert alert-danger", style = "margin-top: 0.8em;",
                   tags$strong("The readiness check failed: "), rd$error))
      }
      tab <- rd$readiness$table
      s <- readiness_table_summary(tab)
      if (is.null(tab) || (s$contracts %||% 0) == 0) {
        return(div(class = "alert alert-warning", style = "margin-top: 0.8em;",
                   "Readiness could not be assessed."))
      }
      fmt <- function(x) formatC(x %||% 0, format = "d", big.mark = ",")
      n_no <- s[["No ECL"]]$contracts %||% 0
      n_bl <- s[["Blank in LIC"]]$contracts %||% 0
      n_ck <- s[["Priced - check"]]$contracts %||% 0
      verdict <- if (n_no + n_bl > 0) {
        div(class = "alert alert-danger",
            sprintf("%s contract(s) would get NO ECL and %s would come out BLANK in LIC. See the reasons below.",
                    fmt(n_no), fmt(n_bl)))
      } else {
        div(class = "alert alert-success", "Every contract will be priced.")
      }
      errs <- .ready_errors(rd)
      tagList(
        tags$hr(),
        h5(tags$strong("Pricing readiness"), " — will every contract get an ECL?"),
        tags$table(class = "table table-sm", style = "width: auto;",
          tags$tr(tags$th("Contracts"), tags$th("No ECL"), tags$th("Blank in LIC"),
                  tags$th("Priced — check")),
          tags$tr(tags$td(fmt(s$contracts)), tags$td(fmt(n_no)), tags$td(fmt(n_bl)),
                  tags$td(fmt(n_ck)))),
        verdict,
        if (nrow(errs) > 0)
          p(tags$span(class = "pill pill-error",
                      sprintf("%d READY ERROR — Run blocked", nrow(errs))),
            " Fix at source, or accept a finding with a reason on ",
            tags$strong("Validation suppressions"), "."),
        tags$details(
          tags$summary("Reasons, fixes and the row funnel"),
          DT::DTOutput(ns("readiness_reasons")),
          tags$br(),
          DT::DTOutput(ns("readiness_funnel")))
      )
    })

    output$readiness_reasons <- DT::renderDT({
      rd <- readiness_res()
      if (is.null(rd) || !is.null(rd$error)) return(NULL)
      r <- readiness_reasons(rd$readiness$table)
      if (nrow(r) == 0) return(NULL)
      r$exposure <- formatC(r$exposure, format = "f", digits = 0, big.mark = ",")
      DT::datatable(r[, c("severity", "check", "contracts", "exposure", "text", "fix")],
                    rownames = FALSE, class = "narrow-table compact",
                    options = list(pageLength = 20, dom = "t", scrollX = TRUE))
    })

    output$readiness_funnel <- DT::renderDT({
      rd <- readiness_res()
      if (is.null(rd) || !is.null(rd$error)) return(NULL)
      f <- rd$readiness$funnel
      if (is.null(f) || nrow(f) == 0) return(NULL)
      DT::datatable(f, rownames = FALSE, class = "narrow-table compact",
                    options = list(pageLength = 20, dom = "t", scrollX = TRUE))
    })

    output$pre_run_status <- renderUI({
      r <- pre_run_results()
      if (is.null(r)) {
        return(p(class = "small-muted",
                  "Click \"Pre-run check\" to run input-stage validators."))
      }
      n_pass <- sum(r$passed)
      n_err  <- sum(!r$passed & r$severity == "ERROR" & !(r$suppressed %||% FALSE))
      n_warn <- sum(!r$passed & r$severity == "WARN"  & !(r$suppressed %||% FALSE))
      summary_pill <- if (n_err > 0) {
        tags$span(class = "pill pill-error", sprintf("%d ERROR — Run blocked", n_err))
      } else if (n_warn > 0) {
        tags$span(class = "pill pill-warn", sprintf("%d WARN — review then proceed", n_warn))
      } else {
        tags$span(class = "pill pill-pass", sprintf("All %d checks passed", n_pass))
      }
      tagList(
        h5(summary_pill),
        DT::DTOutput(ns("pre_run_table"))
      )
    })

    output$pre_run_table <- DT::renderDT({
      r <- pre_run_results()
      if (is.null(r) || nrow(r) == 0) return(NULL)
      r$passed <- as.logical(r$passed)
      # Show ONLY the flagged checks (ERROR / WARN / INFO failures). Passing
      # checks stay out of the UI; the run's reports/validation.csv still
      # records every test, passed or not.
      r <- r[!r$passed, , drop = FALSE]
      if (nrow(r) == 0) return(NULL)
      r$status <- sprintf('<span class="pill pill-%s">%s</span>',
                          tolower(r$severity), toupper(r$severity))
      DT::datatable(
        data.frame(status=r$status, id=r$id, context=r$context,
                    description=r$description,
                    stringsAsFactors=FALSE),
        rownames = FALSE, escape = FALSE,
        class = "narrow-table compact",
        options = list(pageLength = 25, dom = "tip")
      )
    })

    # ---- Run type gate (Official requires approved snapshot) ----------
    # Computes once: is the current (run_type, snapshot) combination
    # valid? Used both to render an explanation under the radio AND
    # to gate the Start run button.
    .run_type_validity <- reactive({
      run_type <- input$run_type %||% "unofficial"
      pick <- input$snapshot_pick %||% "__LIVE__"
      if (run_type == "unofficial") {
        return(list(ok = TRUE, reason = ""))
      }
      # Official rules:
      if (pick == "__LIVE__") {
        return(list(ok = FALSE,
                    reason = paste(
                      "Official runs require an approved version.",
                      "Default config has no approval status \u2014 pick an",
                      "approved version, or switch to Unofficial.")))
      }
      meta <- tryCatch(read_snapshot_metadata(pick, snaps_root()),
                        error = function(e) NULL)
      if (is.null(meta)) {
        return(list(ok = FALSE,
                    reason = "Could not read version metadata."))
      }
      if (!isTRUE(meta$status == "approved")) {
        return(list(ok = FALSE,
                    reason = sprintf(paste(
                      "Official runs require an approved version.",
                      "'%s' is currently '%s'. Promote it to approved",
                      "first, or switch to Unofficial."),
                      pick, meta$status %||% "unknown")))
      }
      list(ok = TRUE, reason = "")
    })

    output$run_type_gate <- renderUI({
      v <- .run_type_validity()
      if (isTRUE(v$ok)) {
        if ((input$run_type %||% "unofficial") == "official") {
          return(div(class = "alert alert-success", style = "margin-top: 0.5em;",
                      tags$strong("Official run"), " — subject to approval."))
        } else {
          return(div(class = "alert alert-secondary", style = "margin-top: 0.5em;",
                      tags$strong("Unofficial run"), " — skips approval. ",
                      "Export is marked UNOFFICIAL."))
        }
      }
      div(class = "alert alert-warning", style = "margin-top: 0.5em;",
          tags$strong("Cannot start an Official run: "),
          v$reason)
    })

    output$start_run_slot <- renderUI({
      r <- pre_run_results()
      pre_run_ok <- !is.null(r) && nrow(r) > 0 &&
                  sum(!r$passed & r$severity == "ERROR" &
                       !(r$suppressed %||% FALSE)) == 0
      rd <- readiness_res()
      ready_ok <- !is.null(rd) && is.null(rd$error) && nrow(.ready_errors(rd)) == 0
      type_v <- .run_type_validity()
      can_run <- pre_run_ok && ready_ok && isTRUE(type_v$ok)

      label <- if ((input$run_type %||% "unofficial") == "official") {
        "Start OFFICIAL run"
      } else {
        "Start unofficial run"
      }
      if (!can_run) {
        return(tags$button(
          id = ns("do_phase1"),
          class = "btn btn-success action-button",
          disabled = NA,
          style = "opacity: 0.5; cursor: not-allowed;",
          icon("play"), " ", label
        ))
      }
      actionButton(ns("do_phase1"), label,
                    icon = icon("play"), class = "btn-success")
    })

    # ============== START RUN -> PHASE 1 =============================
    observeEvent(input$do_phase1, {
      pick <- input$snapshot_pick
      cfg_path <- file.path(getOption("ifrs9.project_root", getwd()),
                              "config.yml")
      err <- NULL
      state <- NULL
      ovr <- input_dir_override()
      src_meta <- input_source_meta()
      run_type_val <- input$run_type %||% "unofficial"
      run_purpose_val <- input$run_purpose %||% NA_character_
      portfolio_date_val <- input$portfolio_date
      calc_ver_val <- input$calculator_pick %||% NA_character_
      # Resolve the calculator environment: if a non-active (archived)
      # version is chosen, run that version's archived code in isolation;
      # otherwise run the live code. Held for phase 2 so both phases use the
      # same calculator.
      calc_env <- tryCatch(make_calc_run_env(calc_ver_val),
                           error = function(e) NULL)
      calc_env_rv(calc_env)
      phase1_fn <- if (is.environment(calc_env) &&
                       !is.null(calc_env$run_etl_phase1))
                     calc_env$run_etl_phase1 else run_etl_phase1
      # Runs are always weighted; per-scenario provisions are produced as
      # additional outputs rather than as a different kind of run.
      ecl_scenario_val <- "weighted"
      base_args <- list(keep_history = TRUE, verbose = FALSE,
                        ecl_scenario = ecl_scenario_val,
                        input_dir_override = ovr, input_source_meta = src_meta,
                        run_type = run_type_val, run_purpose = run_purpose_val,
                        portfolio_date = portfolio_date_val,
                        calculator_version = calc_ver_val)
      withProgress(message = "Phase 1: load + validate + transform", value = 0.1, {
        state <- tryCatch(
          if (pick == "__LIVE__") {
            call_with_supported_args(phase1_fn,
              c(list(config_path = cfg_path), base_args))
          } else {
            call_with_supported_args(phase1_fn,
              c(list(snapshot = pick), base_args))
          },
          error = function(e) { err <<- conditionMessage(e); NULL }
        )
        setProgress(1)
      })
      if (is.null(state)) {
        showNotification(paste("Phase 1 failed:", err),
                          type = "error", duration = 15)
        return()
      }
      phase1_state(state)
      # Reset override buffers for the new run
      overrides_rating(.empty_override_buf("rating"))
      overrides_stage(.empty_override_buf("stage"))
      overrides_restr(.empty_override_buf("restructuring"))
      phase_state("phase1_done")
    })

    # ============== STEP 2: PAUSE PAGE ==============================
    output$pause_summary <- renderUI({
      st <- phase1_state(); if (is.null(st)) return(NULL)
      tagList(
        h4(sprintf("Run %s — paused for review", st$run_id)),
        p(class = "small-muted",
          sprintf("Customers: %d   |   Investments: %d   |   Validation findings (so far): %d",
                  nrow(st$cm_view %||% data.frame()),
                  nrow(st$inv_view %||% data.frame()),
                  if (!is.null(st$validation)) sum(!st$validation$passed) else 0))
      )
    })

    output$cm_view_table <- DT::renderDT({
      st <- phase1_state(); if (is.null(st)) return(NULL)
      cm <- st$cm_view
      cols <- intersect(c("customer_id", "customer_name", "rating_final",
                            "stage_final", "restructuring_final",
                            "watchlist_status", "exposure_total",
                            "max_dpd"),
                         colnames(cm))
      df <- cm[, cols, drop = FALSE]
      # Cast ID columns to character so DT renders a search box, not a
      # numeric range slider. Same logic as the Outputs preview.
      id_pattern <- "(?i)(^id$|_id$|id_|Id$|ID$|^contract|customer$|account)"
      id_cols <- grep(id_pattern, colnames(df), perl = TRUE)
      for (i in id_cols) df[[i]] <- as.character(df[[i]])
      DT::datatable(
        df,
        rownames = FALSE, filter = "top",
        selection = "single",
        class = "narrow-table compact",
        options = list(pageLength = 15, scrollX = TRUE)
      )
    })

    # When a row is selected, populate the override editor below
    selected_customer <- reactive({
      st <- phase1_state(); if (is.null(st)) return(NULL)
      idx <- input$cm_view_table_rows_selected
      if (is.null(idx) || length(idx) == 0) return(NULL)
      cm <- st$cm_view
      cm[idx, , drop = FALSE]
    })

    output$override_editor <- renderUI({
      sel <- selected_customer()
      if (is.null(sel)) {
        return(p(class = "small-muted",
                  "Select a customer above to add overrides."))
      }
      cid <- as.character(sel$customer_id)

      # ---- Compute valid override choices given the calculated state ----
      # Rating: all 21 internal QDB ratings (lending is all-internal in this
      # workbook). Read from the static reference loaded in phase 1.
      rating_choices <- c("(no change)" = "")
      st <- phase1_state()
      if (!is.null(st) && !is.null(st$static$master_rating_scale)) {
        mrs <- st$static$master_rating_scale
        internal <- mrs[mrs$rating_type == "Internal", , drop = FALSE]
        # Order by hierarchy ascending so the dropdown reads from best to worst
        internal <- internal[order(internal$hierarchy), , drop = FALSE]
        rating_choices <- c(rating_choices, setNames(internal$rating, internal$rating))
      }

      # Stage: only worsening transitions allowed.
      cur_stage <- as.character(sel$stage_final %||% "Stage 1")
      stage_choices <- c("(no change)" = "")
      if (cur_stage == "Stage 1") {
        stage_choices <- c(stage_choices, "Stage 2", "Stage 3")
      } else if (cur_stage == "Stage 2") {
        stage_choices <- c(stage_choices, "Stage 3")
      }
      # Stage 3 (or unknown) -> no transitions possible; dropdown shows only "(no change)"

      # Restructuring: flip whichever way is the opposite of current.
      cur_restr <- as.character(sel$restructuring_final %||% "")
      restr_choices <- c("(no change)" = "")
      if (cur_restr == "Restructured") {
        restr_choices <- c(restr_choices, "Not Restructured" = "Not Restructured")
      } else {
        # Treat anything-not-Restructured as the unrestructured case
        restr_choices <- c(restr_choices, "Restructured" = "Restructured")
      }

      tagList(
        h5(sprintf("Override for customer %s — %s", cid,
                    sel$customer_name %||% "")),
        tags$dl(class = "row",
          tags$dt(class = "col-sm-3", "calculated rating"),
          tags$dd(class = "col-sm-9", as.character(sel$rating_final %||% "—")),
          tags$dt(class = "col-sm-3", "calculated stage"),
          tags$dd(class = "col-sm-9", as.character(sel$stage_final %||% "—")),
          tags$dt(class = "col-sm-3", "calculated restructuring"),
          tags$dd(class = "col-sm-9",
                   if (nzchar(cur_restr)) cur_restr else "Not Restructured")
        ),
        fluidRow(
          column(4,
            selectInput(ns("ov_rating_val"), "Override rating",
                         choices = rating_choices, selected = "")),
          column(4,
            selectInput(ns("ov_stage_val"), "Override stage",
                         choices = stage_choices, selected = "")),
          column(4,
            selectInput(ns("ov_restr_val"), "Override restructuring",
                         choices = restr_choices, selected = ""))
        ),
        if (cur_stage == "Stage 3") {
          p(class = "small-muted",
            tags$em("Stage 3 cannot be overridden — it is already the worst stage."))
        },
        textAreaInput(ns("ov_reason"), "Reason (required)",
                       rows = 2,
                       placeholder = "e.g. credit committee decision 2026-Q1, see ticket #..."),
        actionButton(ns("ov_apply"), "Add override",
                      icon = icon("plus"), class = "btn-primary")
      )
    })

    observeEvent(input$ov_apply, {
      sel <- selected_customer()
      if (is.null(sel)) return()
      cid    <- as.character(sel$customer_id)
      reason <- trimws(input$ov_reason %||% "")
      r_val  <- trimws(input$ov_rating_val %||% "")
      s_val  <- input$ov_stage_val %||% ""
      x_val  <- input$ov_restr_val %||% ""

      if (!nzchar(reason)) {
        showNotification("Reason is required for any override.",
                          type = "warning")
        return()
      }
      if (!nzchar(r_val) && !nzchar(s_val) && !nzchar(x_val)) {
        showNotification("Pick at least one value to override.",
                          type = "warning")
        return()
      }

      if (nzchar(r_val)) {
        overrides_rating(.add_override_row(overrides_rating(),
                                             cid, r_val, reason,
                                             prior = as.character(sel$rating_final %||% "")))
      }
      if (nzchar(s_val)) {
        overrides_stage(.add_override_row(overrides_stage(),
                                            cid, s_val, reason,
                                            prior = as.character(sel$stage_final %||% "")))
      }
      if (nzchar(x_val)) {
        overrides_restr(.add_override_row(overrides_restr(),
                                            cid, x_val, reason,
                                            prior = as.character(sel$restructuring_final %||% "")))
      }
      showNotification(sprintf("Override added for %s", cid),
                        type = "message", duration = 3)
      updateTextInput(session, "ov_rating_val", value = "")
      updateSelectInput(session, "ov_stage_val", selected = "")
      updateSelectInput(session, "ov_restr_val", selected = "")
      updateTextAreaInput(session, "ov_reason", value = "")
    })

    output$overrides_pending <- renderUI({
      r <- overrides_rating(); s <- overrides_stage(); x <- overrides_restr()
      tagList(
        h5(sprintf("Pending overrides: rating=%d, stage=%d, restructuring=%d",
                    nrow(r), nrow(s), nrow(x))),
        if (nrow(r) > 0) tagList(h6("Rating overrides"), DT::DTOutput(ns("tbl_r"))),
        if (nrow(s) > 0) tagList(h6("Stage overrides"),  DT::DTOutput(ns("tbl_s"))),
        if (nrow(x) > 0) tagList(h6("Restructuring overrides"), DT::DTOutput(ns("tbl_x")))
      )
    })

    output$tbl_r <- DT::renderDT(.dt_compact(overrides_rating()))
    output$tbl_s <- DT::renderDT(.dt_compact(overrides_stage()))
    output$tbl_x <- DT::renderDT(.dt_compact(overrides_restr()))

    # ============== CONTINUE -> PHASE 2 =============================
    observeEvent(input$do_phase2, {
      st <- phase1_state(); if (is.null(st)) return()
      ov <- list(
        rating        = .convert_to_phase2(overrides_rating(), "override_rating"),
        stage         = .convert_to_phase2(overrides_stage(),  "override_stage"),
        restructuring = .convert_to_phase2(overrides_restr(),  "override_restructuring")
      )
      err <- NULL
      result <- NULL
      calc_env <- calc_env_rv()
      phase2_fn <- if (is.environment(calc_env) &&
                       !is.null(calc_env$run_etl_phase2))
                     calc_env$run_etl_phase2 else run_etl_phase2
      withProgress(message = "Phase 2: derived + write outputs", value = 0.1, {
        result <- tryCatch(
          call_with_supported_args(phase2_fn,
            list(st, overrides = ov, reconcile = TRUE)),
          error = function(e) { err <<- conditionMessage(e); NULL }
        )
        setProgress(1)
      })
      if (is.null(result)) {
        showNotification(paste("Phase 2 failed:", err),
                          type = "error", duration = 15)
        return()
      }
      # Stamp run metadata into the manifest from the app layer, so it is
      # recorded even when an archived (older) calculator produced the run.
      # For an archived run, fingerprint the archived code so the recorded
      # hash matches the registered version (badge shows a match).
      tryCatch({
        run_dir <- result$output_dir %||% result$run_dir %||%
                   file.path(runs_dir_default(), result$run_id)

        # Apply the selected ECL overlay (if any) to the completed run,
        # non-destructively: writes FinalEclReport_overlay_<id>.csv alongside
        # the model report and records the overlay in the manifest.
        ov_id <- ""   # overlays are applied post-run from the Runs page
        if (nzchar(ov_id)) {
          ovp <- file.path(getOption("ifrs9.project_root", getwd()), "config", "overlays.yml")
          b <- tryCatch(get_overlay(ov_id, ovp), error = function(e) NULL)
          if (!is.null(b)) {
            ores <- tryCatch(apply_overlay_to_run(run_dir, b), error = function(e) e)
            if (inherits(ores, "error")) {
              showNotification(paste("Overlay failed:", conditionMessage(ores)), type = "error", duration = 12)
            } else if (isFALSE(ores$ok)) {
              showNotification(sprintf("Overlay '%s' has a conflict (%d contract(s) matched by >1 rule) - not applied.",
                                       ov_id, nrow(ores$conflicts)), type = "error", duration = 15)
            } else {
              showNotification(sprintf("Overlay '%s' applied [%s]: final ECL %s.",
                                       ov_id, toupper(ores$status %||% "draft"),
                                       format(round(ores$totals$final), big.mark = ",")),
                               type = "message", duration = 10)
            }
          }
        }
        code_dir <- if (is.environment(calc_env))
                      calc_version_code_dir(input$calculator_pick %||% "") else NULL
        calc_rec <- calculator_version_for_run(
          id = input$calculator_pick %||% NULL, code_dir = code_dir)
        augment_manifest_run_metadata(run_dir, list(
          run_type           = input$run_type %||% NA_character_,
          run_purpose        = input$run_purpose %||% NA_character_,
          portfolio_date     = as.character(input$portfolio_date %||% NA),
          config_version     = if ((input$snapshot_pick %||% "") == "__LIVE__")
                                 NA_character_ else input$snapshot_pick,
          calculator_version = calc_rec$id %||% (input$calculator_pick %||% NA_character_),
          calculator_label   = calc_rec$label %||% NA_character_,
          calculator_code_hash = calc_rec$code_hash %||% NA_character_,
          calculator_matches_registered = calc_rec$matches_registered %||% NA,
          overlay_applied    = NA_character_
        ))
      }, error = function(e) NULL)

      phase2_result(result)
      phase_state("phase2_done")
      # Notify Runs page + Approval queue that a new run exists on
      # disk. Without this, those modules only refresh on manual
      # button-click and miss the just-completed run.
      if (!is.null(session$userData$runs_changed)) {
        session$userData$runs_changed(
          session$userData$runs_changed() + 1)
      }
    })

    observeEvent(input$do_cancel, {
      phase_state("idle")
      phase1_state(NULL)
      phase2_result(NULL)
      calc_env_rv(NULL)
    })

    # ============== STEP 3: SUMMARY PAGE ============================
    output$run_summary <- renderUI({
      r <- phase2_result(); if (is.null(r)) return(NULL)
      is_official <- (r$run_type %||% "official") == "official"
      n_ov <- (r$overrides_applied$rating %||% 0) +
              (r$overrides_applied$stage %||% 0) +
              (r$overrides_applied$restructuring %||% 0)
      scen <- r$ecl_scenario %||% "weighted"
      scen_txt <- if (identical(scen, "weighted")) "" else
        sprintf(" \u2014 scenario: %s", scen)
      heading <- if (is_official)
        sprintf("Run %s complete \u2014 pending approval%s", r$run_id, scen_txt)
      else
        sprintf("Run %s complete \u2014 unofficial (auto-approved)%s", r$run_id, scen_txt)
      next_step <- if (is_official)
        tagList("Visit the ", tags$strong("Approval queue"),
                 " page to approve or reject this run, or the ",
                 tags$strong("Runs"), " page to view manifest, validation and outputs.")
      else
        tagList("This was an unofficial run \u2014 no approval needed. Open the ",
                 tags$strong("Runs"), " page to view its manifest, validation and outputs.")
      tagList(
        h4(heading),
        tags$ul(
          tags$li(sprintf("Duration: %.1fs", r$duration_seconds)),
          tags$li(sprintf("Outputs: %d files", length(r$output_paths %||% list()))),
          tags$li(sprintf("Overrides applied: %d (rating=%d, stage=%d, restructuring=%d)",
                            n_ov,
                            r$overrides_applied$rating %||% 0,
                            r$overrides_applied$stage %||% 0,
                            r$overrides_applied$restructuring %||% 0)),
          tags$li(sprintf("Path: %s", r$run_dir))
        ),
        if (!identical(scen, "weighted"))
          div(class = "alert alert-warning", style = "padding:0.5em 0.8em;",
              sprintf("Scenario run (%s) \u2014 stress figure, not the reported provision.", scen)),
        p(class = "small-muted", next_step),
        actionButton(ns("do_new_run"), "Start another run",
                      icon = icon("rotate"), class = "btn-secondary")
      )
    })

    observeEvent(input$do_new_run, {
      phase_state("idle")
      phase1_state(NULL)
      phase2_result(NULL)
      pre_run_results(NULL); readiness_res(NULL)
    })
  })
}


# ---- UI fragments per state -----------------------------------------

.ui_idle <- function(ns) {
  tagList(
    fluidRow(column(12,
      card(
        card_header("1. Input source"),
        p(class = "small-muted",
          "Pick where the 12 input files come from. Validate the choice ",
          "before running. The Pre-run check stays disabled until input ",
          "validation passes."),
        radioButtons(ns("input_source_kind"), label = NULL,
                      choices = c(
                        "Use the configured input directory"     = "configured",
                        "Pick a folder from the data drop"       = "drop_folder",
                        "Upload a zip from my computer"           = "upload"
                      ),
                      selected = "configured", inline = FALSE),
        uiOutput(ns("input_source_picker")),
        div(style = "margin-top: 0.75em;",
          actionButton(ns("do_validate_inputs"), "Validate inputs",
                        icon = icon("circle-check"),
                        class = "btn-outline-primary")
        ),
        uiOutput(ns("input_validation_result")),
        uiOutput(ns("input_dq_result"))
      )
    )),
    fluidRow(
      column(6,
        card(
          card_header("2. Configuration"),
          # Snapshot dropdown is rendered as a reactive uiOutput rather
          # than a static selectInput + updateSelectInput. Reason: the
          # update-style approach has a race — when the app starts, the
          # observe fires before the client has registered the
          # selectInput, so the update message is dropped and the
          # dropdown stays at its default ("(live config)" only).
          # Rendering reactively guarantees the choices are correct
          # the first time the dropdown reaches the client.
          uiOutput(ns("snapshot_pick_ui")),
          uiOutput(ns("snapshot_meta")),
          # Run type — H18.
          # Official: requires an *approved* snapshot. Lands as
          #   pending_checker → goes through approval workflow → exportable
          #   as a sanctioned deliverable.
          # Unofficial: any snapshot OR live config. Skips approval, lands
          #   as `unofficial` (terminal). Exportable but clearly marked.
          radioButtons(ns("run_type"), "Run type",
                        choices = c(
                          "Unofficial — for testing / what-if analysis" = "unofficial",
                          "Official — sanctioned deliverable (needs approved version)" = "official"
                        ),
                        selected = "unofficial"),
          # Purpose is coupled to run type:
          #   official   -> regulatory
          #   unofficial -> non-regulatory | impact
          # The choices are repopulated when run type changes (server side).
          selectInput(ns("run_purpose"), "Run purpose",
                       choices = c("Non-regulatory" = "non_regulatory",
                                   "Impact analysis" = "impact"),
                       selected = "non_regulatory"),
          # No ECL scenario picker any more. Every run prices the book under
          # the probability-weighted PD curve (the reported provision) AND
          # under each individual scenario, writing StPD_<scenario>.csv and
          # FinalEclReport_scenario_<scenario>.csv alongside the weighted
          # outputs. Scenario analysis and what-if reweighting therefore work
          # from any single run, with nothing to re-run.
          div(class = "small-muted", style = "margin:2px 0 10px",
              icon("layer-group"),
              " Every run is priced on the probability-weighted PD curve and on each scenario. The weighted figure is the provision; the per-scenario files are written alongside it for analysis."),
          # Calculator (code) version that will produce this run.
          selectInput(ns("calculator_pick"), "Calculator version",
                       choices = c("(active)" = ""),
                       selected = ""),
          uiOutput(ns("calculator_meta")),
          # No overlay picker here. Overlays are post-model adjustments and are
          # applied to a COMPLETED run from the Runs page, where they can also
          # be removed and reapplied without re-running the pipeline. Choosing
          # one up front only duplicated that, and made it look as though the
          # overlay were part of the model.
          div(class = "small-muted", style = "margin:2px 0 10px",
              icon("layer-group"),
              " ECL overlays are applied after the run, from the Runs page."),
          # Portfolio (as-of) date. Auto-populated from the uploaded input's
          # EXTRACTDA when "Validate inputs" runs, so the operator does not set
          # it by hand; defaults to today only until inputs are validated.
          dateInput(ns("portfolio_date"), "Portfolio (as-of) date",
                     value = Sys.Date(), weekstart = 1),
          uiOutput(ns("run_type_gate")),
          # Pre-run check is gated on input validation: the slot below
          # renders the button as enabled OR with a disabled-with-reason
          # message depending on the validation state.
          uiOutput(ns("pre_run_button_slot")),
          uiOutput(ns("start_run_slot"))
        )
      ),
      column(6,
        card(
          card_header("3. Pre-run findings"),
          uiOutput(ns("pre_run_status")),
          uiOutput(ns("readiness_status"))
        )
      )
    )
  )
}


.ui_pause <- function(ns) {
  tagList(
    fluidRow(column(12,
      card(
        card_header("Run paused — review and override"),
        uiOutput(ns("pause_summary")),
        p(class = "small-muted",
          "Below is the calculated customer-level view. Click a row to add ",
          "overrides for rating, stage, or restructuring. When done, click ",
          tags$strong("Continue"), " to finish the run."),
        DT::DTOutput(ns("cm_view_table"))
      )
    )),
    fluidRow(
      column(6,
        card(
          card_header("Add override"),
          uiOutput(ns("override_editor"))
        )
      ),
      column(6,
        card(
          card_header("Pending overrides for this run"),
          uiOutput(ns("overrides_pending"))
        )
      )
    ),
    fluidRow(column(12,
      hr(),
      div(style = "margin-bottom: 1em;",
        actionButton(ns("do_phase2"), "Continue (apply overrides + finish)",
                      icon = icon("forward"), class = "btn-success"),
        tags$span(style = "margin-left: 0.5em;",
          actionButton(ns("do_cancel"), "Cancel run",
                        icon = icon("xmark"), class = "btn-secondary"))
      )
    ))
  )
}


.ui_summary <- function(ns) {
  fluidRow(column(12,
    card(
      card_header("Run complete"),
      uiOutput(ns("run_summary"))
    )
  ))
}


# ---- Override-buffer helpers ---------------------------------------

.empty_override_buf <- function(kind) {
  data.frame(
    customer_id = character(),
    value       = character(),
    prior_value = character(),
    reason      = character(),
    stringsAsFactors = FALSE
  )
}

.add_override_row <- function(buf, customer_id, value, reason, prior = "") {
  rbind(buf, data.frame(
    customer_id = customer_id,
    value       = value,
    prior_value = prior,
    reason      = reason,
    stringsAsFactors = FALSE
  ))
}

.dt_compact <- function(df) {
  if (nrow(df) == 0) return(NULL)
  DT::datatable(
    df, rownames = FALSE, class = "narrow-table compact",
    options = list(pageLength = 5, dom = "tip")
  )
}

# Convert UI buffer (customer_id, value, prior_value, reason) into the
# shape expected by run_etl_phase2's overrides argument.
.convert_to_phase2 <- function(buf, value_col) {
  if (nrow(buf) == 0) return(NULL)
  out <- data.frame(
    customer_id = buf$customer_id,
    reason      = buf$reason,
    stringsAsFactors = FALSE
  )
  out[[value_col]] <- buf$value
  out
}
