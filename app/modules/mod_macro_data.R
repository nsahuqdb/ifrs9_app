# =============================================================================
# Macro data: the macro store, and the new quarter's forecasts filled from it.
# The Shiny side of the Python app's "Macro data" page, on the same package
# functions (macro_refresh, macro_import_file, macro_propose, macro_apply) and
# the same store folder (the project's macro_store/, or IFRS9_MACRO_STORE).
# =============================================================================

mod_macro_data_ui <- function(id) {
  ns <- NS(id)
  div(class = "qdb-page",
    qdb_page_header("Macro data",
      subtitle = "The macro store, and the new quarter's forecasts from it"),
    navset_card_tab(
      nav_panel("Store",
        uiOutput(ns("store_head")),
        fluidRow(
          column(6, div(class = "qdb-card", style = "padding:12px",
            h5("Refresh from the IMF and FRED"),
            uiOutput(ns("groups")),
            checkboxInput(ns("all_ds"), "Also the datasets not fetched by default", FALSE),
            passwordInput(ns("fred"), "FRED API key (optional)", width = "100%"),
            textInput(ns("label"), "Label", placeholder = "e.g. WEO October 2026", width = "100%"),
            actionButton(ns("refresh"), "Refresh", class = "btn-sm btn-primary"))),
          column(6, div(class = "qdb-card", style = "padding:12px",
            h5("Import a file"),
            fileInput(ns("file"), "CSV or Excel: long (date, value) or wide (series by column)",
                      accept = c(".csv", ".xlsx", ".xls"), width = "100%"),
            fluidRow(
              column(6, selectInput(ns("src"), "Source", c("QCB", "NPC", "MOF", "IMF", "OTHER"))),
              column(6, textInput(ns("dsn"), "Dataset", placeholder = "e.g. MONETARY"))),
            fluidRow(
              column(4, textInput(ns("ctry"), "Country", "QAT")),
              column(4, selectInput(ns("freq"), "Frequency",
                                    c("from the dates" = "", "A", "Q", "M"))),
              column(4, textInput(ns("unit"), "Unit"))),
            actionButton(ns("import"), "Import", class = "btn-sm btn-outline-primary")))),
        uiOutput(ns("store_log"))),
      nav_panel("Variables",
        reactable::reactableOutput(ns("vars")),
        uiOutput(ns("var_pick")),
        uiOutput(ns("var_chart"))),
      nav_panel("Find a series",
        fluidRow(
          column(6, textInput(ns("q"), "Search", placeholder = "e.g. oil, credit, NPL, policy rate",
                              width = "100%")),
          column(3, uiOutput(ns("q_ctry"))),
          column(3, selectInput(ns("q_src"), "Source",
                                c("any" = "", "IMF", "DM", "DBN", "FRED", "STATIC", "QCB", "NPC",
                                  "MOF"), width = "100%"))),
        reactable::reactableOutput(ns("hits"))),
      nav_panel("New quarter",
        div(class = "qdb-card", style = "padding:12px",
          fluidRow(
            column(3, dateInput(ns("rd"), "Reporting date",
                                value = as.Date(sprintf("%s-%02d-01", format(Sys.Date(), "%Y"),
                                  3 * ((as.integer(format(Sys.Date(), "%m")) - 1) %/% 3) + 1)) - 1)),
            column(5, uiOutput(ns("base"))),
            column(2, numericInput(ns("fy"), "First forecast year", 0, 0, 2100, 1)),
            column(2, div(style = "margin-top:24px",
              actionButton(ns("propose"), "Propose", class = "btn-sm btn-primary"))))),
        uiOutput(ns("prop")))))
}

mod_macro_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    root <- function() getOption("ifrs9.project_root", getwd())
    snaps_root <- function() getOption("ifrs9.snapshots_dir", file.path(root(), "config_snapshots"))
    st <- .ss_store()
    bump <- reactiveVal(0)

    output$groups <- renderUI({
      ds <- macro_datasets(root())
      selectInput(ns("grp"), "Dataset groups (empty: every dataset marked for fetching)",
                  sort(unique(ds$group)), multiple = TRUE, width = "100%")
    })
    output$store_head <- renderUI({
      bump(); v <- macro_vintages(st)
      if (!nrow(v)) return(div(class = "alert alert-secondary",
                               "The store is empty. Refresh it below, or import a file."))
      v <- v[order(v$created, decreasing = TRUE), , drop = FALSE]
      .ss_metrics(list(list("Latest vintage", v$vintage[1]),
                       list("Made", substr(v$created[1], 1, 16)),
                       list("Series", .an_money(v$series[1])),
                       list("Observations", .an_money(v$observations[1]))))
    })
    last_log <- reactiveVal(NULL)
    observeEvent(input$refresh, {
      r <- withProgress(message = "Fetching every dataset (a few minutes)…", value = 0.3,
        tryCatch(macro_refresh(st, root(), groups = if (length(input$grp)) input$grp,
                               fred_key = if (nzchar(input$fred)) input$fred, label = input$label,
                               only_default = !isTRUE(input$all_ds)),
                 error = function(e) e))
      if (inherits(r, "error")) return(showNotification(conditionMessage(r), type = "error"))
      last_log(r$log); bump(bump() + 1)
      showNotification(sprintf("Saved vintage %s.", r$vintage), type = "message")
    })
    observeEvent(input$import, {
      f <- input$file
      if (is.null(f) || !nzchar(input$dsn))
        return(showNotification("Choose a file and name its dataset.", type = "warning"))
      ext <- tools::file_ext(f$name); p <- paste0(f$datapath, ".", ext)
      file.copy(f$datapath, p, overwrite = TRUE)
      r <- tryCatch(macro_import_file(st, p, input$src, input$dsn, country = input$ctry,
                                      freq = if (nzchar(input$freq)) input$freq, unit = input$unit),
                    error = function(e) e)
      if (inherits(r, "error")) return(showNotification(conditionMessage(r), type = "error"))
      bump(bump() + 1)
      showNotification(sprintf("Saved vintage %s.", r$vintage), type = "message")
    })
    output$store_log <- renderUI({
      lg <- last_log(); if (!length(lg)) return(NULL)
      d <- tryCatch(do.call(rbind, lapply(lg, as.data.frame)), error = function(e) NULL)
      if (is.null(d)) return(NULL)
      tagList(h5("What the refresh fetched"), qdb_reactable(d, searchable = FALSE))
    })

    concepts <- reactive({ bump()
      v <- macro_latest(st); if (is.null(v)) return(NULL)
      macro_resolve_concepts(st, v, root()) })
    output$vars <- reactable::renderReactable({
      cn <- concepts(); if (is.null(cn)) return(NULL)
      reactable::reactable(data.frame(Variable = cn$label, Unit = cn$unit,
        From = ifelse(nzchar(cn$series_id), cn$series_id, "— not in the store"),
        `Last actual` = cn$last_actual, First = cn$first_year, Last = cn$last_year,
        `In the VAR` = ifelse(cn$core, "yes", ""), check.names = FALSE),
        compact = TRUE, defaultPageSize = 40)
    })
    output$var_pick <- renderUI({
      cn <- concepts(); if (is.null(cn)) return(NULL)
      cn <- cn[nzchar(cn$series_id), , drop = FALSE]
      selectInput(ns("var_v"), "Show", stats::setNames(cn$concept, cn$label))
    })
    output$var_chart <- renderUI({
      req(input$var_v)
      a <- macro_concept_annual(st, input$var_v, NULL, root())
      sdv <- macro_concept_sd(st, input$var_v, NULL, root())
      if (!nrow(a) || !.an_echarts()) return(NULL)
      tagList(
        echarts4r::e_charts(a, year) |>
          echarts4r::e_line(value, legend = FALSE, itemStyle = list(color = .AN_COL$plum)) |>
          echarts4r::e_tooltip(trigger = "axis") |>
          echarts4r::e_grid(left = 50, right = 20, top = 20, bottom = 30),
        p(class = "small-muted", sprintf(
          "Standard deviation of the actual years: %.2f. A shock in standard deviations uses this.",
          sdv)))
    })

    output$q_ctry <- renderUI({
      cc <- macro_countries(root())
      selectInput(ns("q_c"), "Country", c("any" = "", cc$code), width = "100%")
    })
    output$hits <- reactable::renderReactable({
      if (!nzchar(input$q %||% "") && !nzchar(input$q_c %||% "") && !nzchar(input$q_src %||% ""))
        return(NULL)
      h <- macro_search(st, input$q %||% "", source = if (nzchar(input$q_src)) input$q_src,
                        country = if (nzchar(input$q_c %||% "")) input$q_c)
      if (!nrow(h)) return(NULL)
      reactable::reactable(h[, intersect(c("series_id", "name", "unit", "first", "last", "values",
                                           "last_actual"), names(h))],
                           compact = TRUE, searchable = TRUE, defaultPageSize = 20)
    })

    snaps <- reactive({ bump(); tryCatch(list_snapshots(snaps_root()), error = function(e) NULL) })
    output$base <- renderUI({
      s <- snaps()
      ch <- c("Live config" = "")
      if (!is.null(s) && nrow(s))
        ch <- c(ch, stats::setNames(s$label, sprintf("%s [%s]", s$label, s$status)))
      selectInput(ns("base_v"), "Config to start from", ch, width = "100%")
    })
    .dirs <- function(label) {
      if (nzchar(label %||% "")) {
        d <- file.path(snaps_root(), label)
        return(list(config = file.path(d, "config"), static = file.path(d, "static")))
      }
      list(config = file.path(root(), "config"), static = file.path(root(), "data-raw", "static"))
    }
    prop <- eventReactive(input$propose, {
      d <- .dirs(input$base_v)
      withProgress(message = "Reading the store…", value = 0.5,
        tryCatch(macro_propose(st, d$config, d$static, as.character(input$rd), project_root = root(),
                               first_year = if (isTRUE(input$fy > 0)) input$fy),
                 error = function(e) list(ok = FALSE, reason = conditionMessage(e))))
    })
    output$prop <- renderUI({
      p <- prop(); if (is.null(p)) return(NULL)
      if (!isTRUE(p$ok)) return(div(class = "alert alert-warning", p$reason))
      ch <- p$changes
      ck <- p$checks
      bad <- if (is.data.frame(ck)) ck$check[!ck$ok] else character(0)
      s <- snaps(); base <- input$base_v %||% ""
      is_draft <- nzchar(base) && !is.null(s) && identical(s$status[s$label == base], "draft")
      rd <- as.Date(input$rd)
      tagList(
        h5(p$rule),
        .ss_metrics(list(list("Vintage", p$vintage), list("Values that change", .an_money(p$changed)),
                         list("Non-oil GDP from", p$nonoil$series_id))),
        lapply(p$warnings, function(w) div(class = "alert alert-warning", w)),
        if (length(bad)) div(class = "alert alert-danger",
                             paste("Failed:", paste(bad, collapse = "; "))),
        if (!is.null(ch) && nrow(ch))
          qdb_reactable(transform(ch, current = round(current, 2), proposed = round(proposed, 2),
                                  change = sprintf("%+.2f", change)), searchable = FALSE,
                        page_size = 15),
        h5("Write into a draft config version"),
        if (is_draft) p(class = "small-muted", sprintf("%s is a draft: the proposal is written into it.", base))
        else textInput(ns("new_draft"), "New draft",
                       sprintf("%d-Q%d-macro-draft", as.integer(format(rd, "%Y")),
                               (as.integer(format(rd, "%m")) - 1) %/% 3 + 1)),
        actionButton(ns("write"), "Write", class = "btn-sm btn-outline-primary",
                     disabled = if (length(bad)) "disabled"))
    })
    observeEvent(input$write, {
      p <- prop(); base <- input$base_v %||% ""
      s <- snaps()
      is_draft <- nzchar(base) && !is.null(s) && identical(s$status[s$label == base], "draft")
      target <- if (is_draft) base else input$new_draft
      user <- Sys.info()[["user"]] %||% "unknown"
      r <- tryCatch({
        if (!is_draft) {
          d <- .dirs(base)
          create_snapshot(target, sprintf("Macro forecasts for %s from vintage %s",
                                          as.character(input$rd), p$vintage), user,
                          parent = if (nzchar(base)) base, config_dir = d$config,
                          static_dir = d$static, snapshots_root = snaps_root())
        }
        dd <- .dirs(target)
        p2 <- macro_propose(st, dd$config, dd$static, as.character(input$rd), project_root = root(),
                            first_year = if (isTRUE(input$fy > 0)) input$fy)
        macro_apply(p2, target, snapshots_root = snaps_root(), edited_by = user)
      }, error = function(e) list(ok = FALSE, message = conditionMessage(e)))
      bump(bump() + 1)
      if (!isTRUE(r$ok)) return(showNotification(r$message %||% "Could not write the draft.",
                                                 type = "error"))
      showNotification(sprintf("Written into %s. Test it with a run, then move it on under Config versions.",
                               target), type = "message", duration = 10)
    })
  })
}
