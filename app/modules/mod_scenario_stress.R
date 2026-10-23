# =============================================================================
# Scenario stress: shocks on any macro variable, several at once, priced on a
# run. The Shiny side of the Python app's "Scenario stress" page, on the same
# package functions (stress_prepare, stress_transmit, stress_shapley,
# stress_sensitivities, stress_ecl_at_risk, stress_reverse), so the two apps
# give the same figures. Scenarios are saved to config/stress_scenarios.yml,
# the file the Python app reads and writes too.
#
# Reading a run for scenarios (contracts, curves, sectors, the macro model) and
# building the Monte Carlo proxy are the slow parts, so both are kept per run,
# macro-store vintage and stress config.
# =============================================================================

.SS_SHAPES <- c("One year" = "one_year", "Held" = "held", "Builds up" = "build",
                "Recovers" = "recover", "Year by year" = "custom")
.ss_cache <- new.env(parent = emptyenv())

.ss_root <- function() getOption("ifrs9.project_root", getwd())
.ss_store <- function() {
  d <- Sys.getenv("IFRS9_MACRO_STORE", file.path(.ss_root(), "macro_store"))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  macro_store(d)
}

# The latest vintage; an empty store is seeded with the package's own history
# so scenarios work before the first refresh.
.ss_vintage <- function(st) {
  v <- macro_latest(st)
  if (is.null(v) || !nzchar(v %||% "")) {
    macro_refresh(st, .ss_root(), sources = "STATIC", label = "Package history")
    v <- macro_latest(st)
  }
  v
}

.ss_cfg_key <- function(cfg) {
  p <- cfg[["_path"]] %||% ""
  paste(p, if (file.exists(p)) as.character(file.mtime(p)) else "")
}

.ss_get <- function(key, build) {
  if (!exists(key, envir = .ss_cache, inherits = FALSE))
    assign(key, build(), envir = .ss_cache)
  get(key, envir = .ss_cache, inherits = FALSE)
}

.ss_model <- function(st, v, cfg) {
  mm <- cfg$macro_model %||% list()
  .ss_get(paste("model", st$root, v, .ss_cfg_key(cfg)), function()
    macro_fit_model(st, vintage = v, project_root = .ss_root(),
                    min_years = mm$min_years %||% 12, tightness = mm$tightness %||% 0.2,
                    cross = mm$cross %||% 0.5, start_year = mm$start_year %||% 1990,
                    extra = macro_concepts(.ss_root())$concept))
}

.ss_bar <- function(df, x, y, height = NULL) {
  if (is.null(df) || !nrow(df)) return(.an_na("Nothing to show."))
  if (!.an_echarts())
    return(qdb_reactable(data.frame(df[[x]], .an_signed(df[[y]])), searchable = FALSE))
  d <- df[order(df[[y]]), , drop = FALSE]
  d$x_ <- d[[x]]; d$y_ <- d[[y]]
  cj <- htmlwidgets::JS(sprintf("function(p){var c=[%s]; return c[p.dataIndex];}",
    paste0("'", ifelse(d$y_ >= 0, .AN_COL$err, .AN_COL$ok), "'", collapse = ",")))
  echarts4r::e_charts(d, x_, height = height %||% max(220, 28 * nrow(d) + 60)) |>
    echarts4r::e_bar(y_, legend = FALSE, bar_width = "55%", itemStyle = list(color = cj)) |>
    echarts4r::e_flip_coords() |>
    echarts4r::e_x_axis(axisLabel = list(formatter = .an_m_axis())) |>
    echarts4r::e_tooltip() |>
    echarts4r::e_grid(left = 230, right = 30, top = 10, bottom = 30)
}

.ss_metrics <- function(items)
  div(class = "qdb-stats", style = "margin:8px 0",
      lapply(items, function(it) div(class = "qdb-stat",
        div(class = "lbl", it[[1]]), div(class = "val", it[[2]]))))

mod_scenario_stress_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("head")),
    navset_pill(
      nav_panel("Build and price",
        div(class = "qdb-card", style = "padding:12px;margin-top:8px",
          fluidRow(
            column(8, uiOutput(ns("pick"))),
            column(2, div(style = "margin-top:24px",
              actionButton(ns("load"), "Load", class = "btn-sm btn-outline-primary")))),
          uiOutput(ns("meta")),
          uiOutput(ns("rows")),
          actionButton(ns("add"), "Add a shock", icon = icon("plus"),
                       class = "btn-sm btn-outline-secondary"),
          uiOutput(ns("knock_ui")),
          uiOutput(ns("lever_pick")),
          uiOutput(ns("preview")),
          div(style = "margin-top:8px",
            actionButton(ns("go"), "Price", icon = icon("play"), class = "btn-sm btn-primary"),
            actionButton(ns("split"), "Split by shock", class = "btn-sm btn-outline-primary",
                         style = "margin-left:4px"),
            actionButton(ns("save"), "Save", icon = icon("floppy-disk"),
                         class = "btn-sm btn-outline-secondary", style = "margin-left:4px"))),
        uiOutput(ns("result"))),
      nav_panel("Compare",
        div(class = "qdb-card", style = "padding:12px;margin-top:8px",
          uiOutput(ns("cmp_pick")),
          actionButton(ns("cmp_go"), "Compare", class = "btn-sm btn-primary")),
        uiOutput(ns("cmp_result"))),
      nav_panel("Sensitivity",
        div(class = "qdb-card", style = "padding:12px;margin-top:8px",
          fluidRow(
            column(3, numericInput(ns("sens_sd"), "Adverse move (standard deviations)", 1,
                                   0.25, 5, 0.25, width = "100%")),
            column(4, div(style = "margin-top:30px",
              checkboxInput(ns("sens_knock"), "Move the other variables with it", FALSE))),
            column(2, div(style = "margin-top:24px",
              actionButton(ns("sens_go"), "Run", class = "btn-sm btn-primary"))))),
        uiOutput(ns("sens_result"))),
      nav_panel("ECL at risk",
        div(class = "qdb-card", style = "padding:12px;margin-top:8px",
          fluidRow(
            column(3, numericInput(ns("mc_n"), "Paths", 2000, 200, 20000, 200, width = "100%")),
            column(3, numericInput(ns("mc_seed"), "Seed", 20261023, 1, 2^31 - 2, 1,
                                   width = "100%")),
            column(3, numericInput(ns("mc_thr"), "Provision threshold (QAR m, optional)",
                                   NA, 0, step = 50, width = "100%")),
            column(2, div(style = "margin-top:24px",
              actionButton(ns("mc_go"), "Simulate", class = "btn-sm btn-primary"))))),
        uiOutput(ns("mc_result"))),
      nav_panel("Reverse stress",
        div(class = "qdb-card", style = "padding:12px;margin-top:8px",
          fluidRow(
            column(4, numericInput(ns("rev_t"), "Increase in provision to reach (QAR m)", 300,
                                   1, step = 50, width = "100%")),
            column(2, div(style = "margin-top:24px",
              actionButton(ns("rev_go"), "Find the path", class = "btn-sm btn-primary"))))),
        uiOutput(ns("rev_result")))))
}

mod_scenario_stress_server <- function(id, out_dir, inputs, report) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cfg <- stress_config_read(.ss_root())
    st <- .ss_store()

    setup <- reactive({
      v <- .ss_vintage(st)
      m <- .ss_model(st, v, cfg)
      cons <- macro_concepts(.ss_root())
      cons <- cons[cons$concept %in% m$concepts, , drop = FALSE]
      list(v = v, model = m, labels = stats::setNames(cons$label, cons$concept),
           library = scenario_library(), saved = scenario_read(scenarios_path(.ss_root())))
    })
    rc <- reactive({
      od <- out_dir(); i <- inputs(); d <- report()
      if (is.null(od) || is.null(i) || isFALSE(i$ok) || is.null(d)) return(NULL)
      s <- setup()
      run_path <- if (basename(od) %in% c("Output", "output", "OUTPUT")) dirname(od) else od
      .ss_get(paste("rc", od, s$v, .ss_cfg_key(cfg)), function()
        withProgress(message = "Reading the run for scenarios…", value = 0.5,
          stress_prepare(i, d, run_path, st, model = s$model, cfg = cfg,
                         project_root = .ss_root())))
    })
    proxy <- function(r) {
      s <- setup()
      .ss_get(paste("proxy", out_dir(), s$v, .ss_cfg_key(cfg)), function()
        withProgress(message = "Decomposing the book for path pricing…", value = 0.3,
                     stress_build_proxy(r)))
    }

    output$head <- renderUI({
      r <- rc(); s <- setup()
      if (is.null(r)) return(.an_na("This run's engine inputs could not be read."))
      p(class = "small-muted", style = "margin-top:8px",
        sprintf("Forecast years %d–%d · macro data %s · provision %s. ", r$first_year,
                r$first_year + r$H - 1, s$v, .an_money(sum(r$base$ecl, na.rm = TRUE))),
        paste(r$notes, collapse = " "))
    })

    # ---- the scenario on screen -----------------------------------------
    shocks <- reactiveVal(list())
    uid <- reactiveVal(0)
    lever <- reactiveVal(list())
    meta <- reactiveVal(list(name = "Scenario", description = "", knock_on = TRUE))
    # The editor lives inside the Analytics page's rendered body, so its
    # inputs are drawn from these values rather than updated in place.
    .load <- function(scn) {
      ids <- seq_along(scn$shocks) + uid(); uid(uid() + length(scn$shocks))
      shocks(stats::setNames(scn$shocks, paste0("s", ids)))
      meta(list(name = scn$name %||% "Scenario", description = scn$description %||% "",
                knock_on = !isFALSE(scn$knock_on)))
      lever(scn$levers %||% list())
      res(NULL); spl(NULL)
    }
    output$meta <- renderUI({ m <- meta()
      fluidRow(
        column(4, textInput(ns("name"), "Name", m$name, width = "100%")),
        column(8, textInput(ns("desc"), "Description", m$description, width = "100%")))
    })
    output$knock_ui <- renderUI(div(style = "margin-top:10px",
      checkboxInput(ns("knock"), "Move the other variables with the shocks (macro model)",
                    value = meta()$knock_on)))
    choices <- reactive({
      s <- setup()
      c(stats::setNames(paste0("L:", seq_along(s$library)),
                        paste("Library:", vapply(s$library, `[[`, "", "name"))),
        if (length(s$saved)) stats::setNames(paste0("S:", seq_along(s$saved)),
                        paste("Saved:", vapply(s$saved, `[[`, "", "name"))),
        "Blank" = "B")
    })
    output$pick <- renderUI(selectInput(ns("pick_v"), "Start from", choices(), width = "100%"))
    observeEvent(setup(), { if (!length(shocks())) .load(setup()$library[[1]]) }, once = TRUE)
    observeEvent(input$load, {
      k <- input$pick_v; s <- setup()
      if (identical(k, "B")) return(.load(scenario_new("New scenario")))
      j <- as.integer(sub("^.:", "", k))
      .load(if (startsWith(k, "L")) s$library[[j]] else s$saved[[j]])
    })
    observeEvent(input$add, {
      sh <- .read_rows(shocks()); uid(uid() + 1)
      sh[[paste0("s", uid())]] <- scenario_shock(names(setup()$labels)[1], 0)
      shocks(sh)
    })
    observeEvent(input$rm, {
      sh <- .read_rows(shocks()); sh[[input$rm]] <- NULL; shocks(sh)
    })
    .read_rows <- function(sh) {
      for (k in names(sh)) {
        g <- function(f, d) { v <- input[[paste0(k, "_", f)]]; if (is.null(v)) d else v }
        shape <- g("shape", sh[[k]]$shape)
        path <- suppressWarnings(as.numeric(strsplit(gsub(";", ",", g("path",
                  paste(sh[[k]]$path %||% numeric(0), collapse = ", "))), ",")[[1]]))
        sh[[k]] <- scenario_shock(g("concept", sh[[k]]$concept), g("size", sh[[k]]$size),
                                  shape, g("unit", sh[[k]]$unit), g("start", sh[[k]]$start),
                                  g("years", sh[[k]]$years),
                                  if (identical(shape, "custom")) path[!is.na(path)] else NULL)
      }
      sh
    }
    output$rows <- renderUI({
      s <- setup(); sh <- shocks(); H <- cfg$horizon_years %||% 5
      labs <- stats::setNames(names(s$labels), s$labels)
      hdr <- fluidRow(class = "small-muted",
        column(3, "Variable"), column(1, "Size"), column(1, "In"), column(2, "Shape"),
        column(1, "From year"), column(1, "Over years"), column(2, "Year by year"))
      tagList(if (length(sh)) hdr, lapply(names(sh), function(k) { x <- sh[[k]]
        fluidRow(
          column(3, selectInput(ns(paste0(k, "_concept")), NULL, labs, x$concept, width = "100%")),
          column(1, numericInput(ns(paste0(k, "_size")), NULL, x$size, width = "100%")),
          column(1, selectInput(ns(paste0(k, "_unit")), NULL, c("units" = "native", "sd" = "sd"),
                                x$unit, width = "100%")),
          column(2, selectInput(ns(paste0(k, "_shape")), NULL, .SS_SHAPES, x$shape, width = "100%")),
          column(1, numericInput(ns(paste0(k, "_start")), NULL, x$start, 1, H, 1, width = "100%")),
          column(1, numericInput(ns(paste0(k, "_years")), NULL, x$years, 1, H, 1, width = "100%")),
          column(2, textInput(ns(paste0(k, "_path")), NULL,
                              paste(x$path %||% numeric(0), collapse = ", "),
                              placeholder = "year by year", width = "100%")),
          column(1, tags$button(type = "button", class = "btn btn-sm btn-outline-secondary",
            onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("rm"), k),
            icon("xmark"))))
      }))
    })
    output$lever_pick <- renderUI({
      pk <- tryCatch(stress_read(), error = function(e) list())
      nm <- c("None", vapply(pk, function(p) p$name %||% "Stress", ""))
      selectInput(ns("lever_v"), "Parameter stress on top", nm,
                  if ((lever()$name %||% "") %in% nm) lever()$name else "None")
    })
    scn <- reactive({
      sh <- unname(.read_rows(shocks()))
      lv <- list()
      if (!is.null(input$lever_v) && input$lever_v != "None") {
        pk <- tryCatch(stress_read(), error = function(e) list())
        lv <- Filter(function(p) identical(p$name, input$lever_v), pk)[[1]] %||% list()
      }
      scenario_new(input$name %||% meta()$name, input$desc %||% meta()$description, sh,
                   knock_on = if (is.null(input$knock)) meta()$knock_on else isTRUE(input$knock),
                   levers = lv)
    })
    output$preview <- renderUI({
      x <- scn(); s <- setup()
      if (!length(x$shocks)) return(NULL)
      sp <- tryCatch(scenario_paths(x, s$model, cfg$horizon_years %||% 5), error = function(e) NULL)
      if (is.null(sp)) return(NULL)
      sev <- sp$severity
      p(class = "small-muted", sprintf(
        "Severity: %.2f standard deviations from the baseline jointly, about 1 in %s.",
        sev$distance, .an_money(sev$one_in)))
    })
    observeEvent(input$save, {
      x <- scn(); keep <- Filter(function(s) !identical(s$name, x$name), setup()$saved)
      scenario_write(c(keep, list(x)), scenarios_path(.ss_root()))
      showNotification(sprintf("Saved %s.", x$name), type = "message")
    })

    res <- reactiveVal(NULL); spl <- reactiveVal(NULL)
    observeEvent(input$go, {
      r <- rc(); if (is.null(r)) return()
      withProgress(message = "Repricing…", value = 0.5, res(stress_transmit(r, scn())))
      spl(NULL)
    })
    observeEvent(input$split, {
      r <- rc(); if (is.null(r)) return()
      withProgress(message = "Pricing every combination of the shocks…", value = 0.5,
                   spl(stress_shapley(r, scn())))
    })
    output$result <- renderUI({
      r <- res(); if (is.null(r)) return(NULL)
      if (!isTRUE(r$ok)) return(div(class = "alert alert-warning", r$reason))
      wf <- r$waterfall; wf <- wf[abs(wf$change) >= 0.5, , drop = FALSE]
      s <- rbind(data.frame(label = "Provision now", amount = r$before, kind = "total"),
                 data.frame(label = wf$layer, amount = wf$change, kind = rep("delta", nrow(wf))),
                 data.frame(label = "Under the scenario", amount = r$after, kind = "total"))
      sp <- spl()
      tagList(
        .ss_metrics(list(list("Provision now", .an_money(r$before)),
                         list("Under the scenario", .an_money(r$after)),
                         list("Change", .an_signed(r$delta)), list("% change", .an_pct(r$pct)),
                         list("Severity", paste("1 in", .an_money(r$severity$one_in))))),
        if (.an_echarts()) .an_waterfall(s, zoom = TRUE),
        fluidRow(
          column(6, h5("By sector"), .ss_bar(utils::head(r$by_segment, 12), "segment", "change")),
          column(6, h5("By portfolio"), .ss_bar(r$by_portfolio, "portfolio", "change"))),
        if (!is.null(sp) && isTRUE(sp$ok)) tagList(
          h5("Split by shock"), .ss_bar(sp$parts, "part", "shapley"),
          p(class = "small-muted", sprintf(
            "Alone the shocks add to %s; together %s. The difference, %s, is how they compound, shared out in the split.",
            .an_signed(sp$sum_alone), .an_signed(sp$total), .an_signed(sp$interaction)))))
    })

    # ---- compare --------------------------------------------------------
    output$cmp_pick <- renderUI({
      ch <- choices(); ch <- ch[ch != "B"]
      selectInput(ns("cmp_v"), "Scenarios", ch, utils::head(ch, 4), multiple = TRUE, width = "100%")
    })
    cmp <- eventReactive(input$cmp_go, {
      r <- rc(); s <- setup(); if (is.null(r)) return(NULL)
      withProgress(message = "Repricing the scenarios…", value = 0.5,
        lapply(input$cmp_v, function(k) { j <- as.integer(sub("^.:", "", k))
          stress_transmit(r, if (startsWith(k, "L")) s$library[[j]] else s$saved[[j]]) }))
    })
    output$cmp_result <- renderUI({
      x <- Filter(function(r) isTRUE(r$ok), cmp() %||% list())
      if (!length(x)) return(NULL)
      t <- data.frame(scenario = vapply(x, `[[`, "", "name"),
                      ecl = vapply(x, `[[`, 0, "after"), change = vapply(x, `[[`, 0, "delta"),
                      pct = vapply(x, `[[`, 0, "pct"),
                      one_in = vapply(x, function(r) r$severity$one_in, 0),
                      moved = vapply(x, function(r) as.numeric(r$moved), 0))
      t <- t[order(-t$change), , drop = FALSE]
      tagList(h5("Change in provision"), .ss_bar(t, "scenario", "change"),
        qdb_reactable(data.frame(Scenario = t$scenario, Provision = .an_money(t$ecl),
                                 Change = .an_signed(t$change), `% change` = .an_pct(t$pct),
                                 Severity = paste("1 in", .an_money(t$one_in)),
                                 `Re-staged` = .an_money(t$moved), check.names = FALSE),
                      searchable = FALSE),
        p(class = "small-muted", "Each scenario is priced on its own from the same run."))
    })

    # ---- sensitivity ------------------------------------------------------
    sens <- eventReactive(input$sens_go, {
      r <- rc(); if (is.null(r)) return(NULL)
      withProgress(message = "One repricing per variable…", value = 0.5,
        stress_sensitivities(r, size_sd = input$sens_sd, knock_on = isTRUE(input$sens_knock)))
    })
    output$sens_result <- renderUI({
      x <- sens(); if (is.null(x) || is.null(x$table)) return(NULL)
      t <- x$table; t$variable <- paste0(t$label, " (", t$move, ")")
      tagList(.ss_bar(t, "variable", "change"),
        qdb_reactable(data.frame(Variable = t$label, Channel = t$channel, Move = t$move,
                                 `In units` = round(t$move_native, 3),
                                 Change = .an_signed(t$change), `% change` = .an_pct(t$change_pct),
                                 check.names = FALSE), searchable = FALSE, page_size = 25))
    })

    # ---- ECL at risk ------------------------------------------------------
    mc <- eventReactive(input$mc_go, {
      r <- rc(); if (is.null(r)) return(NULL)
      P <- proxy(r)
      thr <- if (is.na(input$mc_thr %||% NA)) NULL else input$mc_thr * 1e6
      withProgress(message = "Drawing macro paths and pricing them…", value = 0.6,
        stress_ecl_at_risk(r, P, n = input$mc_n, seed = input$mc_seed, threshold = thr,
                           scenarios = c(setup()$library, setup()$saved)))
    })
    output$mc_result <- renderUI({
      x <- mc(); if (is.null(x)) return(NULL)
      sm <- x$summary
      items <- c(list(list("Provision now", .an_money(x$base)),
                      list("Mean over paths", .an_money(mean(x$ecl)))),
                 lapply(seq_len(nrow(sm)), function(j)
                   list(sprintf("%s (%s)", sm$measure[j], .an_signed(sm$change[j])),
                        .an_money(sm$value[j]))))
      if (!is.null(x$p_exceed))
        items <- c(items, list(list("Chance above threshold", .an_pct0(100 * x$p_exceed, 1))))
      h <- graphics::hist(x$ecl, breaks = 40, plot = FALSE)
      hd <- data.frame(mid = h$mids, count = h$counts)
      hist_chart <- if (.an_echarts())
        echarts4r::e_charts(hd, mid) |>
          echarts4r::e_bar(count, legend = FALSE, barCategoryGap = "0%",
                           itemStyle = list(color = .AN_COL$plum_l)) |>
          echarts4r::e_x_axis(axisLabel = list(formatter = .an_m_axis())) |>
          echarts4r::e_tooltip() |> echarts4r::e_grid(left = 60, right = 20, top = 10, bottom = 40)
      named <- x$named
      tagList(.ss_metrics(items), hist_chart,
        fluidRow(
          column(6, h5("What drives the tail"),
                 .ss_bar(transform(x$by_channel, change = tail_change), "channel", "change")),
          column(6, h5("Tail by sector"),
                 .ss_bar(utils::head(transform(x$by_segment, change = tail_change), 10),
                         "segment", "change"))),
        h5("Where the scenarios fall"),
        qdb_reactable(data.frame(Scenario = named$scenario, Change = .an_signed(named$proxy_change),
                                 Percentile = sprintf("%.1f", named$percentile)),
                      searchable = FALSE),
        if (!is.null(x$loss_summary)) tagList(
          h5("Credit loss over the next year (customers defaulting at their stressed PD)"),
          .ss_metrics(c(list(list("Expected loss", .an_money(x$expected_loss))),
                        lapply(seq_len(nrow(x$loss_summary)), function(j)
                          list(x$loss_summary$measure[j], .an_money(x$loss_summary$value[j])))))))
    })

    # ---- reverse ----------------------------------------------------------
    rev <- eventReactive(input$rev_go, {
      r <- rc(); if (is.null(r)) return(NULL)
      P <- proxy(r)
      withProgress(message = "Searching, then confirming with a full repricing…", value = 0.6,
                   stress_reverse(r, P, input$rev_t * 1e6))
    })
    output$rev_result <- renderUI({
      x <- rev(); if (is.null(x)) return(NULL)
      if (!isTRUE(x$ok) || !isTRUE(x$found))
        return(div(class = "alert alert-secondary", x$reason %||% "No path found."))
      t <- x$paths
      yc <- grep("^[0-9]{4}$", names(t), value = TRUE)
      show <- data.frame(Variable = t$label, `Worst (sd)` = sprintf("%+.2f", t$worst_sd),
                         check.names = FALSE)
      for (y in yc) show[[y]] <- sprintf("%+.2f", t[[y]])
      wf <- x$result$waterfall
      tagList(
        .ss_metrics(list(list("Target", .an_signed(x$target)),
                         list("Confirmed change", .an_signed(x$full_change)),
                         list("Distance", sprintf("%.2f sd", x$distance)),
                         list("Likelihood", paste("1 in", .an_money(x$one_in))))),
        p(class = "small-muted",
          "The most likely combination of macro moves that raises the provision by the target: the one nearest the baseline, given how the variables have moved together."),
        qdb_reactable(show, searchable = FALSE, page_size = 20),
        if (!is.null(wf)) tagList(h5("Through which channel"),
          .ss_bar(wf[abs(wf$change) >= 0.5, , drop = FALSE], "layer", "change")))
    })
  })
}
