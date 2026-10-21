# =============================================================================
# app/modules/mod_analytics.R
#
# Analytics page. Three modes, each a set of tabs:
#
#   Single run      Overview | Staging | PD | LGD & collateral |
#                   Exposure & maturity | Concentration | Scenarios |
#                   Data quality
#   Compare runs    Movement | Migration
#   Stress testing  Stress test | Sensitivity (Each lever, To a target,
#                   Staging threshold) | Macro & scenarios | Roll forward
#
# Movement is ecl_bridge(): every contract repriced with compute_ecl(), the
# function the pipeline prices with, a cause at a time. The stress tabs reprice
# with the same function, through one stress function (stress_apply()), so no
# tab has an ECL calculation of its own.
#
# All computation lives in the ifrs9qdb package (pure functions, no Shiny).
# This module only selects runs, loads reports and renders. Charts use
# echarts4r and fall back to reactable tables when it is not installed, so an
# un-updated deployment still renders.
# =============================================================================

.an_echarts <- function() requireNamespace("echarts4r", quietly = TRUE)

.AN_COL <- list(plum = "#5b1f6e", plum_l = "#8e5aa3", teal = "#0d9488",
                ok = "#059669", warn = "#d97706", err = "#dc2626",
                grey = "#8a94a6", ink = "#1f2430")
.AN_PAL <- c("#5b1f6e", "#8e5aa3", "#0d9488", "#c9a9d8", "#7c3aed",
             "#a78bfa", "#5eead4", "#8a94a6", "#d97706", "#059669")

.an_money <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  # trimmed: format() pads a vector to its widest element
  ifelse(is.na(x), "\u2014", trimws(format(round(x), big.mark = ",", scientific = FALSE)))
}
.an_pct <- function(x, digits = 2) {
  ifelse(is.na(x), "\u2014", sprintf(paste0("%+.", digits, "f%%"), x))
}
.an_pct0 <- function(x, digits = 2) {
  ifelse(is.na(x), "\u2014", sprintf(paste0("%.", digits, "f%%"), x))
}
.an_m_axis <- function() htmlwidgets::JS(
  "function(v){var a=Math.abs(v); if(a>=1e9) return (v/1e9).toFixed(1)+'b'; if(a>=1e6) return (v/1e6).toFixed(0)+'m'; if(a>=1e3) return (v/1e3).toFixed(0)+'k'; return v;}")

.an_na <- function(msg = "Not available in this report.")
  div(class = "qdb-empty", style = "padding:26px 10px",
      div(class = "ico", icon("circle-info")), div(msg))

.an_signed <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "\u2014", paste0(ifelse(x > 0, "+", ""), .an_money(x)))
}

# A round step for about `x` per tick: 1, 2 or 5 times a power of ten.
.an_nice <- function(x) {
  p <- 10^floor(log10(x)); f <- x / p
  p * (if (f <= 1) 1 else if (f <= 2) 2 else if (f <= 5) 5 else 10)
}
# Axis labels in the unit the tick step reads in, so ticks never repeat.
.an_axis_fmt <- function(step) {
  u <- if (step >= 1e9) list(1e9, "b") else if (step >= 1e6) list(1e6, "m")
       else if (step >= 1e3) list(1e3, "k") else list(1, "")
  d <- max(0, -floor(log10(step / u[[1]]) + 1e-9))
  htmlwidgets::JS(sprintf(
    "function(v){return (v/%s).toLocaleString('en-US',{minimumFractionDigits:%d,maximumFractionDigits:%d})+'%s';}",
    format(u[[1]], scientific = FALSE), d, d, u[[2]]))
}

# A waterfall of `s` (label, amount, kind = "total" | "delta"): totals stand on
# the axis, each step starts where the last ended. `zoom` starts the axis near
# the lowest point the bars reach rather than at zero -- beside a provision in
# the billions the causes are otherwise too small to see.
.an_waterfall <- function(s, zoom = FALSE) {
  amt <- s$amount; n <- length(amt)
  base <- numeric(n); vis <- numeric(n); lvl <- numeric(n); run <- 0
  for (i in seq_len(n)) {
    if (s$kind[i] == "total") { base[i] <- 0; vis[i] <- amt[i]; run <- amt[i] }
    else {
      if (amt[i] >= 0) { base[i] <- run; vis[i] <- amt[i] }
      else { base[i] <- run + amt[i]; vis[i] <- -amt[i] }
      run <- run + amt[i]
    }
    lvl[i] <- run
  }
  cols <- ifelse(s$kind == "total", .AN_COL$plum,
          ifelse(amt >= 0, .AN_COL$err, .AN_COL$ok))
  js_arr <- function(x) paste0("[", paste(x, collapse = ","), "]")
  col_js <- htmlwidgets::JS(sprintf("function(p){var c=%s; return c[p.dataIndex];}",
                                    js_arr(paste0("'", cols, "'"))))
  amt_js <- js_arr(sprintf("%.4f", amt))
  tot_js <- js_arr(ifelse(s$kind == "total", "1", "0"))
  lab_js <- htmlwidgets::JS(sprintf(paste0(
    "function(p){var a=%s,t=%s,v=a[p.dataIndex],s=Math.abs(v);",
    "var x=s>=1e9?(s/1e9).toFixed(2)+'b':s>=1e6?(s/1e6).toFixed(1)+'m':s>=1e3?(s/1e3).toFixed(0)+'k':s.toFixed(0);",
    "return (t[p.dataIndex]||v==0?'':(v<0?'\u2212':'+'))+x;}"), amt_js, tot_js))
  yax <- list(axisLabel = list(formatter = .an_m_axis()))
  if (zoom && n > 0) {
    lo <- min(lvl); hi <- max(lvl)
    pad <- max((hi - lo) * 0.18, abs(hi) * 0.002, 1)
    if (lo - pad > 0) {
      step <- .an_nice((hi - lo + 2 * pad) / 6)
      yax <- list(min = floor((lo - pad) / step) * step,
                  max = ceiling((hi + pad) / step) * step, interval = step,
                  axisLabel = list(formatter = .an_axis_fmt(step)))
    }
  }
  df <- data.frame(step = factor(s$label, levels = unique(s$label)), base = base, vis = vis)
  do.call(echarts4r::e_y_axis, c(list(
    echarts4r::e_charts(df, step) |>
      # stackStrategy "all": a step that takes the running total below zero
      # still starts where the last one ended
      echarts4r::e_bar(base, stack = "w", legend = FALSE, stackStrategy = "all",
                       itemStyle = list(color = "transparent"),
                       emphasis = list(disabled = TRUE), tooltip = list(show = FALSE)) |>
      echarts4r::e_bar(vis, stack = "w", legend = FALSE, stackStrategy = "all",
                       barWidth = "55%", itemStyle = list(color = col_js),
                       label = list(show = TRUE, position = "top", fontSize = 10,
                                    color = "#4a5162", formatter = lab_js))), yax)) |>
    echarts4r::e_x_axis(axisLabel = list(interval = 0, rotate = 22, fontSize = 10)) |>
    echarts4r::e_tooltip(formatter = htmlwidgets::JS(sprintf(
      "function(p){var a=%s; return p.name+'<br/><b>'+a[p.dataIndex].toLocaleString('en-US',{maximumFractionDigits:0})+'</b>';}",
      amt_js))) |>
    echarts4r::e_grid(bottom = 90, left = 75, right = 20, top = 28) |>
    echarts4r::e_toolbox_feature("saveAsImage")
}

# ---- Movement: the ECL bridge (ifrs9qdb ecl_bridge()) ----------------------
.BL_LEVELS <- c("Whole book" = "book", "Customer" = "customer",
                "Facility" = "facility", "Account type" = "account_type",
                "Segment" = "segment", "Stage" = "stage", "Rating" = "rating")
.BL_PLURAL <- c(customer = "customers", facility = "facilities",
                account_type = "account types", segment = "segments",
                stage = "stages", rating = "ratings")
# The waterfall's steps, as they are shown. The engine prices more causes than
# this (BRIDGE_COMPONENTS); several are folded together here so the chart
# reads as the steps a reviewer asks about. Same groups as the Python app.
#   key -> engine components it sums; .BL_ALWAYS: shown even when zero
.BL_GROUPS <- list(
  derecognised = "derecognised", new_business = "new_business",
  exposure = "exposure", stage = "stage", rating = "rating",
  macro = "macro", model = "model", methodology = c("settings", "method"),
  pd_curves = "pd_curves",
  lgd = "lgd", overlay = "overlay", other = "other")
.BL_ALWAYS <- c("derecognised", "new_business", "exposure", "stage", "rating",
                "macro", "model", "lgd", "overlay")
.BL_LABEL <- c(derecognised = "Derecognised", new_business = "New business",
               exposure = "Exposure change", stage = "Stage migration",
               rating = "Rating migration", macro = "Macro variable change",
               model = "Model change", methodology = "Methodology change",
               pd_curves = "Macro and model change",
               lgd = "LGD & collateral", overlay = "Overlay", other = "Unexplained")
.BL_WHAT <- c(
  derecognised = "In the prior run and not in this one, at its prior ECL.",
  new_business = "New in this run, at its ECL.",
  exposure = "Contracts in both runs: repayments, drawdowns and amortisation -- the balance, the EAD curve and the remaining term -- at the prior stage, rating, PD curves and LGD.",
  stage = "Between 12-month and lifetime ECL, or into Stage 3 (booked at the outstanding balance).",
  rating = "The new rating, on the prior run's PD curves.",
  macro = "The macro inputs: MEV forecasts, scenario weights and severities, and the GDP history, on the prior model.",
  model = "The macro PD model itself: its variables, coefficients, TTC anchor and PDs, and how the variables combine. Zero when the model did not change.",
  methodology = "How the ECL is calculated rather than with what: the ECL settings in model.yml (EAD fallback rules, Stage 3 method, exposure cap) and, when the prior run was priced by an earlier version of the engine, the change in the calculation itself. Shown only when it is not zero.",
  pd_curves = "Macro inputs and model together: one of the runs has no saved config, so the two cannot be told apart.",
  lgd = "Collateral, and so LGD, and the EIR.",
  overlay = "The post-model management overlay.",
  other = "Should be zero: what the repricing could not explain, and contracts that could not be repriced in both runs.")

# One column per shown step, summing the engine's components.
.bl_grouped <- function(df) {
  for (k in names(.BL_GROUPS)) {
    have <- intersect(.BL_GROUPS[[k]], names(df))
    df[[paste0("g_", k)]] <- if (length(have)) rowSums(as.matrix(df[, have, drop = FALSE])) else 0
  }
  df
}
# The step columns worth a column in a table: non-zero somewhere.
.bl_shown_cols <- function(df)
  names(.BL_GROUPS)[vapply(names(.BL_GROUPS), function(k)
    sum(abs(df[[paste0("g_", k)]]), na.rm = TRUE) >= 0.5, logical(1))]

# The bridge reprices both runs contract by contract -- seconds for a book of
# thousands -- so the last few pairs are kept, keyed on each run's folder and
# report and the report's time stamp: moving between levels, tabs and sessions
# reads it rather than redoing it.
.an_bridge_cache <- new.env(parent = emptyenv())
.an_bridge <- function(dir_a, rep_a, dir_b, rep_b) {
  key <- paste(dir_a, rep_a, file.mtime(rep_a), dir_b, rep_b, file.mtime(rep_b),
               sep = "|")
  hit <- .an_bridge_cache$items[[key]]
  if (!is.null(hit)) return(hit)
  br <- ecl_bridge(load_bridge_run(dir_a, rep_a), load_bridge_run(dir_b, rep_b))
  if (isTRUE(br$ok))
    .an_bridge_cache$items <- utils::tail(
      c(.an_bridge_cache$items, stats::setNames(list(br), key)), 4)
  br
}

# The steps worth a bar: exposure, stage, rating and LGD always; the macro
# variables whenever the runs' config tells them from the model; the model
# when it changed; the combined PD-curve step only when they cannot be told
# apart; anything else when it moved.
.bl_shown <- function(steps, br) {
  amt <- stats::setNames(steps$amount, steps$key)
  split <- isTRUE(br$split)
  out <- steps[steps$key == "opening", c("key", "label", "amount", "kind")]
  for (k in names(.BL_GROUPS)) {
    a <- sum(amt[intersect(.BL_GROUPS[[k]], names(amt))])
    if (k == "macro" && !split) next      # its move is in "Macro and model change"
    if (k %in% .BL_ALWAYS || abs(a) >= (if (k == "other") 1 else 0.5))
      out <- rbind(out, data.frame(key = k, label = .BL_LABEL[[k]], amount = a, kind = "delta"))
  }
  rbind(out, steps[steps$key == "closing", c("key", "label", "amount", "kind")])
}

# The lever sensitivity's moves, by the name each is edited under.
.TN_KEYS <- c(pd_multiplier = "PD multiplier", rating_notches = "Rating notches",
              collateral_pct = "Collateral %", exposure_pct = "Exposure %",
              lgd_base = "LGD base", lgd_floor = "LGD floor",
              dpd_threshold = "DPD threshold", default_top_n = "Largest N default",
              contagion = "Contagion", tasdeer_collective = "Tasdeer collective")

mod_analytics_ui <- function(id) {
  ns <- NS(id)
  div(class = "qdb-page",
    qdb_page_header(
      "Analytics",
      subtitle = "Provision movement, risk profile, concentration and data quality"),
    div(class = "qdb-card", style = "padding:10px 12px",
      fluidRow(
        column(3, selectInput(ns("mode"), "View",
                              choices = c("Single run" = "single",
                                          "Compare two runs" = "compare",
                                          "Stress testing" = "stress"),
                              width = "100%")),
        column(4, uiOutput(ns("pick_a"))),
        column(4, uiOutput(ns("pick_b"))),
        column(1, div(style = "margin-top:24px",
                      actionButton(ns("refresh"), NULL, icon = icon("rotate"),
                                   class = "btn-sm btn-outline-secondary"))))),
    div(class = "qdb-loading",
      uiOutput(ns("kpis")),
      uiOutput(ns("body")))
  )
}

mod_analytics_server <- function(id, runs_root = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    bump <- reactiveVal(0)
    observeEvent(input$refresh, bump(bump() + 1))

    runs_tbl <- reactive({
      bump()
      root <- runs_root %||% file.path(getOption("ifrs9.project_root", getwd()), "runs")
      r <- tryCatch(list_runs(root), error = function(e) NULL)
      if (is.null(r) || nrow(r) == 0) return(NULL)
      r[order(r$started_at, decreasing = TRUE), , drop = FALSE]
    })
    run_choices <- reactive({
      r <- runs_tbl(); if (is.null(r)) return(character(0))
      stats::setNames(r$run_id,
        sprintf("%s  \u00b7  %s", r$run_id, substr(as.character(r$started_at), 1, 10)))
    })

    output$pick_a <- renderUI({
      ch <- run_choices()
      lbl <- if (identical(input$mode, "compare")) "Prior run (opening)" else "Run"
      sel <- if (identical(input$mode, "compare") && length(ch) > 1) ch[2] else ch[1]
      selectInput(ns("run_a"), lbl, choices = ch, selected = sel, width = "100%")
    })
    output$pick_b <- renderUI({
      if (!identical(input$mode, "compare")) return(NULL)
      ch <- run_choices()
      selectInput(ns("run_b"), "Current run (closing)", choices = ch,
                  selected = ch[1], width = "100%")
    })

    .run_dir_for <- function(run_id) {
      r <- runs_tbl(); if (is.null(r) || is.null(run_id) || !nzchar(run_id)) return(NULL)
      row <- r[r$run_id == run_id, , drop = FALSE]
      if (nrow(row) == 0) NULL else row$path[1]
    }
    # The report a run is read from: its overlay report when it has one.
    .report_path_for <- function(run_id) {
      d <- .run_dir_for(run_id); if (is.null(d)) return(NULL)
      paths <- tryCatch(list_run_outputs(d), error = function(e) character(0))
      ov <- paths[grepl("^FinalEclReport_overlay_.*\\.csv$", basename(paths))]
      base <- paths[basename(paths) == "FinalEclReport.csv"]
      if (length(ov) > 0) ov[1] else if (length(base) > 0) base[1] else NULL
    }
    .report_for <- function(run_id) {
      pick <- .report_path_for(run_id)
      if (is.null(pick)) return(NULL)
      df <- tryCatch(utils::read.csv(pick, stringsAsFactors = FALSE, check.names = FALSE,
                                     na.strings = c("", "NA", "NaN")),
                     error = function(e) NULL)
      list(data = an_normalise(df), file = basename(pick))
    }
    .out_dir_for <- function(run_id) {
      r <- runs_tbl()
      if (is.null(r) || is.null(run_id) || length(run_id) != 1 ||
          is.na(run_id) || !nzchar(run_id)) return(NULL)
      row <- r[r$run_id == run_id, , drop = FALSE]
      if (nrow(row) == 0) return(NULL)
      d <- file.path(row$path[1], "Output")
      if (dir.exists(d)) d else row$path[1]
    }
    inp_a <- reactive({ bump(); an_load_engine_inputs(.out_dir_for(input$run_a)) })
    inp_b <- reactive({ bump(); an_load_engine_inputs(.out_dir_for(input$run_b)) })

    rep_a <- reactive({ bump(); .report_for(input$run_a) })
    rep_b <- reactive({ bump(); .report_for(input$run_b) })
    dat_a <- reactive({ a <- rep_a(); if (is.null(a)) NULL else a$data })
    dat_b <- reactive({ b <- rep_b(); if (is.null(b)) NULL else b$data })

    # ------------------------------------------------------------ KPIs -----
    output$kpis <- renderUI({
      # Compare two runs has no row of its own: Movement opens with the two
      # runs side by side and the movement for what is selected.
      if (identical(input$mode, "compare")) {
        return(NULL)
      } else {
        d <- dat_a(); if (is.null(d)) return(NULL)
        e <- sum(d$exposure); l <- sum(d$ecl)
        s3 <- sum(d$ecl[d$stage == 3], na.rm = TRUE)
        qdb_stats(list(
          list(k = "Contracts", v = .an_money(nrow(d)), tone = "accent"),
          list(k = "Exposure", v = .an_money(e), tone = ""),
          list(k = "ECL provision", v = .an_money(l), tone = "warn"),
          list(k = "Coverage", v = .an_pct0(100 * l / max(e, 1)), tone = "ok"),
          list(k = "Stage 3 share", v = .an_pct0(100 * s3 / max(l, 1), 1), tone = ""),
          list(k = "Customers", v = .an_money(length(unique(d$customer))), tone = "")))
      }
    })

    # ------------------------------------------------------------ body -----
    output$body <- renderUI({
      if (identical(input$mode, "compare")) {
        a <- dat_a(); b <- dat_b()
        if (is.null(a) || is.null(b))
          return(qdb_empty("Pick two runs that both have an ECL report.", "code-compare"))
        if (identical(input$run_a, input$run_b))
          return(qdb_empty("Pick two different runs to compare.", "code-compare"))
        navset_card_tab(
          nav_panel("Movement",
            p(class = "small-muted", style = "margin-top:8px",
              "Why the provision moved, for the whole book or for the customers, facilities, account types, segments, stages or ratings you choose: each run's figures side by side, then the waterfall between them."),
            qdb_help(c(
              stats::setNames(as.list(unname(.BL_WHAT)), unname(.BL_LABEL[names(.BL_WHAT)])),
              list("Choosing a level" = "A contract counts in one group: the customer, facility, account type, segment, stage or rating it is in now, or, if it was derecognised, the one it left. So a stage's Prior ECL is what its current contracts had in the prior run, not the prior run's total for that stage: a contract that moved from Stage 1 to Stage 2 is in Stage 2, with its move under Stage migration. The groups add up to the whole book.")),
              note = "Each contract is repriced from the prior run to the current one a cause at a time, in the order shown, on each run's frozen config, so the steps sum exactly to the closing ECL. Macro variable change and Model change are told apart by pricing once more on PD curves built from the current run's macro inputs and the prior run's model. Model change is the PD model alone. The ECL settings in model.yml, and the difference when a prior run was priced by an earlier version of the engine (it is first repriced on this one), are Methodology change, shown only when not zero. Unexplained appears only if the repricing leaves something over."),
            radioButtons(ns("bl_level"), "Compare", choices = .BL_LEVELS, inline = TRUE,
                         selected = isolate(input$bl_level) %||% "book"),
            uiOutput(ns("bl_pick")),
            uiOutput(ns("bl_body"))),
          nav_panel("Migration",
            p(class = "small-muted", style = "margin-top:8px",
              "Who moved, on stage and on rating, counted as customers because both are set per customer. What each move cost, and which contracts carried it, is on Movement (Stage migration and Rating migration)."),
            qdb_help(list(
              "Stage migration" = "Customers moving between IFRS 9 stages, counted as customers because staging is set per customer.",
              "Deterioration" = "Customers moving to a worse stage (1&rarr;2, 2&rarr;3).",
              "Improvement" = "Customers moving to a better stage, normally a cure.",
              "Rating migration" = "Customers moving between rating grades, ordered along the scale's own hierarchy (best first).",
              "Upgrade / Downgrade" = "Decided by hierarchy rank, not alphabetically \u2014 a downgrade is a move to a worse grade.",
              "Rating scale" = "Internal (QDB grades) for Business Finance, Off BS, Al Dhameen and Tasdeer; external (agency grades) for Investments and Banks. Both reuse hierarchy 1&ndash;21, so they are never mixed."),
              note = "Each cell shows the number of customers making that move."),
            fluidRow(column(4, uiOutput(ns("pick_rt2")))),
            fluidRow(
              column(7, h5("Stage migration (customers)"), uiOutput(ns("c_ctrans"))),
              column(5, h5("Rating direction"), uiOutput(ns("c_migsum")))),
            h5(style = "margin-top:14px", "Customers whose stage changed"),
            reactable::reactableOutput(ns("t_stagemove")),
            hr(), h5("Rating migration matrix"), uiOutput(ns("c_ratmig"))))
      } else if (identical(input$mode, "stress")) {
        # One stress function behind every tab (stress_apply()): a stress test
        # names who it reaches and what changes, so a what-if on named
        # customers, a staging-policy change and a package are all one;
        # Sensitivity moves its levers one at a time.
        navset_card_tab(
          nav_panel("Stress test",
            p(class = "small-muted", style = "margin-top:8px",
              "Name who a stress reaches and what changes for them, then Run. Several run side by side."),
            qdb_help(list(
              "Stress" = "Who it reaches — every contract in the portfolios AND stages AND customers named; an empty one does not narrow it — and what changes for them. A package, a what-if on named customers and a staging-policy change are all one.",
              "Customer ids" = "Comma or semicolon separated; a name with spaces is one id. The customers named are shown as they stand while you type, so a mistyped id shows before anything is priced.",
              "Stage 2 DPD threshold" = "Stage 2 applies when days past due exceed this and are at most 90 (<code>DPD &gt; threshold</code>). Left at the run's own, with every trigger on, the reported stages stand, manual overrides included.",
              "Triggers" = "Contagion lifts a customer's Stage 1 facilities when another is Stage 2 or worse (Tasdeer excluded); Tasdeer is collectively Stage 2; the watchlist and the local flags (restructuring and the other five indicators) force Stage 2.",
              "Move to stage" = "Sets the ECL horizon: Stage 1 is capped at 12 months, Stage 2 runs to maturity, Stage 3 books the full outstanding.",
              "Largest N default" = "The scope's N largest customers by exposure forced to Stage 3, booking their full outstanding.",
              "PD multiplier" = "Scales every cumulative PD curve, capped at 1.",
              "PD scenario" = "Prices the scope on that scenario's PD curves, from this run's own scenario output, instead of the weighted ones.",
              "Rating notches" = "Moves along each contract's own rating scale. Positive is a downgrade, and it selects a different PD curve.",
              "LGD base and floor" = "The unsecured loss rate and the minimum unsecured share, this run's own to start with. Raising the floor bites hardest on well-secured lending.",
              "Collateral %" = "Scales collateral value, which feeds LGD through the coverage ratio.",
              "Exposure %" = "Scales the balance and therefore the EAD curve, a supplied schedule included.",
              "Extend maturity" = "Moves the maturity date by calendar months; a supplied repayment schedule is re-profiled onto the new term.",
              "Standard packages" = "Mild and severe downturn, collateral revaluation, staging tightened to 30 days and the largest five defaulting, each on the internally rated portfolios.",
              "Save" = "Writes <code>config/stress_packages.yml</code>, which the Python app reads too, so the same stresses run next quarter."),
              note = "Every lever reaches the scope and nothing else, and the book is repriced with the production ECL function, so the engine's EAD, LGD, staging, discounting and exposure cap all still apply. Nothing is re-run and nothing is written."),
            uiOutput(ns("stp_note")),
            div(style = "margin:4px 0 10px",
              actionButton(ns("stp_add"), "Add a package", icon = icon("plus"),
                           class = "btn-sm btn-outline-primary"),
              actionButton(ns("stp_std"), "Add the standard packages", icon = icon("layer-group"),
                           class = "btn-sm btn-outline-primary", style = "margin-left:4px"),
              actionButton(ns("stp_load"), "Load saved", icon = icon("folder-open"),
                           class = "btn-sm btn-outline-secondary", style = "margin-left:4px"),
              actionButton(ns("stp_save"), "Save", icon = icon("floppy-disk"),
                           class = "btn-sm btn-outline-secondary", style = "margin-left:4px")),
            uiOutput(ns("stp_editor")),
            div(style = "margin:8px 0 14px",
              actionButton(ns("stp_run"), "Run", icon = icon("play"),
                           class = "btn-sm btn-primary")),
            uiOutput(ns("stp_result"))),
          nav_panel("Sensitivity",
            p(class = "small-muted", style = "margin-top:8px",
              "Which lever the provision turns on, and how far it would have to move. Each repricing goes through the same stress function as Stress test."),
            navset_pill(
              nav_panel("Each lever",
                qdb_help(list(
                  "What this shows" = "Where the provision is actually sensitive. A lever with a large bar is one to watch; a flat one is not worth stressing.",
                  "One at a time" = "Every lever starts from the same base and moves alone, so the bars are comparable. They do not add up — combining levers is a stress test.",
                  "Standard moves" = "PD +25%, one and two notch downgrades, collateral down 25% and 50%, exposure +20%, LGD base and floor shifts, DPD threshold 60 to 30, the largest five defaulting, contagion off, Tasdeer not collective."),
                  note = "Runs a full repricing per lever, so it takes several seconds."),
                uiOutput(ns("tn_levers")),
                div(style = "margin:6px 0 12px",
                  actionButton(ns("tn_run"), "Price each lever", icon = icon("chart-simple"),
                               class = "btn-sm btn-primary")),
                uiOutput(ns("tn_result"))),
              nav_panel("To a target",
                qdb_help(list(
                  "Target" = "The increase in the total provision you want to reach, in percent.",
                  "Required level" = "How far that lever must move to get there, found by repricing repeatedly and narrowing in — not read off a grid.",
                  "Beyond range" = "The lever cannot reach the target on its own, even at the extreme of its range. The percentage shown is the most it achieves.",
                  "PD multiplier" = "Every cumulative PD curve scaled by this factor.",
                  "Rating downgrade" = "Notches every in-scope contract moves down its own scale.",
                  "Collateral value" = "Collateral falls to this percentage of current value.",
                  "Largest customers defaulting" = "How many of the biggest customers would have to reach Stage 3."),
                  note = "Each lever is solved on its own, with everything else at base. The lever needing the smallest move is the one the provision is most exposed to."),
                div(class = "qdb-card", style = "padding:12px",
                  fluidRow(
                    column(3, numericInput(ns("rv_target"), "Target provision increase (%)",
                                           value = 25, min = 1, max = 500, step = 5, width = "100%")),
                    column(4, uiOutput(ns("rv_scope"))),
                    column(3, div(style = "margin-top:24px",
                      actionButton(ns("rv_run"), "Solve", icon = icon("magnifying-glass"),
                                   class = "btn-sm btn-primary"))))),
                uiOutput(ns("rv_result"))),
              nav_panel("Staging threshold",
                qdb_help(list(
                  "Threshold" = "Stage 2 applies when days past due exceed it and are at most 90 (<code>DPD &gt; threshold</code>).",
                  "Policy in force" = "The threshold this run used, from its frozen config. It is always priced, and every other row is measured from it.",
                  "Why so flat" = "Most of the book is staged by watchlist, restructuring and contagion rather than by days past due, so moving the threshold moves comparatively little. That is the finding."),
                  note = "Each threshold is a full repricing, with everything else held. A different policy with other levers is a stress test."),
                div(class = "qdb-card", style = "padding:12px",
                  fluidRow(
                    column(4, selectInput(ns("sw_days"), "Thresholds to price (days)",
                                          choices = c(0, 15, 30, 45, 60, 75, 90),
                                          selected = c(0, 30, 60, 90), multiple = TRUE,
                                          width = "100%")),
                    column(4, uiOutput(ns("sw_scope"))),
                    column(3, div(style = "margin-top:24px",
                      actionButton(ns("sw_run"), "Reprice", icon = icon("play"),
                                   class = "btn-sm btn-primary"))))),
                uiOutput(ns("sw_result"))))),
          nav_panel("Macro & scenarios",
            p(class = "small-muted", style = "margin-top:8px",
              "Change the macro path or the scenario weights, rebuild the PD curves and reprice."),
            qdb_help(list(
              "What this does" = "Replaces the macro forecast and the scenario weights as asked, then re-runs the whole PD chain — <code>stress_mevs</code> to the annual term structure to the monthly StPD — and reprices every contract with the production ECL function. The change propagates through the model rather than being applied to the answer.",
              "Shock" = "An absolute shift applied to every year of one variable. −2 on GDP growth means two percentage points lower across the whole path.",
              "Model weight" = "Each variable's weight in the model. A variable weighted zero will not move the provision however far it is shocked — that is the model, not a fault.",
              "Scenario weights" = "Internal weights run on <code>auto_non_oil_gdp_cdf</code>, and the loader takes that forecast from Non-Oil GDP in the FIRST TWO years of this table — so by default the weights follow the path. Hold them to isolate the PD effect, or set them yourself: they start at the weights this run is priced with, and left there reprice to the booked figure exactly.",
              "Ten points onto each scenario" = "Each row adds 10 percentage points to one scenario, taken from the others in proportion, and reprices the book on this run's own forecast — from the weights you set, or this run's own.",
              "External scale" = "Investments and Banks & FIs keep their own weights, which follow GCC growth year by year.",
              "Held fixed" = "Collateral, LGD, staging and the EAD curves are untouched, so what you see is the macro effect alone."),
              note = "This rebuilds the term structure and the monthly PD curves before repricing, so it takes a few seconds; the ten-point sensitivity reprices five times."),
            uiOutput(ns("mev_editor")),
            div(class = "qdb-card", style = "padding:12px;margin-bottom:8px",
              radioButtons(ns("mev_wmode"), "Scenario weights",
                choices = c(
                  "Follow the new path (as the model is configured)" = "auto",
                  "Hold this run's weights" = "hold",
                  "Set my own" = "custom"),
                selected = "auto"),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'custom'", ns("mev_wmode")),
                uiOutput(ns("mev_wcustom")))),
            div(style = "margin:8px 0 14px",
              actionButton(ns("mev_run"), "Rebuild and reprice", icon = icon("play"),
                           class = "btn-sm btn-primary"),
              actionButton(ns("sens_run"), "Ten points onto each scenario",
                           icon = icon("scale-balanced"),
                           class = "btn-sm btn-outline-primary", style = "margin-left:8px"),
              actionButton(ns("mev_reset"), "Reset to the run's forecast",
                           icon = icon("rotate-left"),
                           class = "btn-sm btn-outline-secondary", style = "margin-left:8px")),
            uiOutput(ns("mev_result")),
            uiOutput(ns("c_sens")), reactable::reactableOutput(ns("t_sens"))),
          nav_panel("Roll forward",
            p(class = "small-muted", style = "margin-top:8px",
              "What the provision becomes in n months if nothing else changes."),
            qdb_help(list(
              "What this is" = "A mechanical roll of the existing book. The contracts are untouched; only the reporting date moves, so this isolates TIME DECAY from any credit view.",
              "Balance" = "Advanced along each contract's own EAD curve — elapsed months are dropped, so the balance is what the schedule says it will be at that date, not today's.",
              "PD" = "Conditional on surviving those months: <code>(cumPD(k+t) &minus; cumPD(k)) / (1 &minus; cumPD(k))</code>. Using the original curve would charge for a default already known not to have happened.",
              "Matured" = "Contracts reaching maturity inside the window have run off and carry no provision. Reported separately rather than dropped quietly.",
              "Not a forecast" = "Staging, ratings and collateral are held. Comparing this with the actual next-quarter run separates what time did from what credit did."),
              note = "This is NOT the same as shortening maturity, which keeps today's balance and squeezes the remaining repayments into less time."),
            div(class = "qdb-card", style = "padding:12px",
              fluidRow(
                column(3, numericInput(ns("rf_months"), "Roll forward (months)",
                                       value = 12, min = 1, max = 60, step = 3, width = "100%")),
                column(4, uiOutput(ns("rf_scope"))),
                column(3, div(style = "margin-top:24px",
                  actionButton(ns("rf_run"), "Roll forward", icon = icon("forward"),
                               class = "btn-sm btn-primary"))))),
            uiOutput(ns("rf_result"))))
      } else {
        d <- dat_a()
        if (is.null(d)) return(qdb_empty("No ECL report found for that run.", "chart-line"))
        navset_card_tab(
          nav_panel("Overview",
            qdb_help(list(
              "ECL" = "Expected credit loss: the discounted sum of monthly losses, <code>EAD &times; LGD &times; marginal PD</code>, discounted at the EIR.",
              "Exposure" = "On-balance outstanding at the reporting date.",
              "ECL coverage %" = "ECL divided by exposure. The headline provisioning rate.",
              "Stage 1" = "Performing. ECL over a 12-month horizon.",
              "Stage 2" = "Significant increase in credit risk. ECL over the remaining life.",
              "Stage 3" = "Credit-impaired. Provisioned at the full outstanding under the current configuration."),
              note = "Coverage normally rises from Stage 1 to Stage 3; a portfolio that breaks that pattern is worth a look."),
            fluidRow(
              column(6, h5("ECL and coverage by stage"), uiOutput(ns("c_stage"))),
              column(6, h5("ECL by portfolio"), uiOutput(ns("c_portfolio")))),
            hr(),
            fluidRow(
              column(7, h5("Coverage heatmap \u2014 portfolio by stage"), uiOutput(ns("c_heat"))),
              column(5, h5("Portfolio profile"), reactable::reactableOutput(ns("t_profile"))))),
          nav_panel("Staging",
            p(class = "small-muted", style = "margin-top:8px",
              "Staging is set per customer, so these count customers."),
            qdb_help(list(
              "Staging rule" = "Stage 3 if the customer's worst DPD is over 90, or by a Stage 3 override. Stage 2 if DPD is above the threshold but at most 90, or watchlist, or restructured, or by a Stage 2 override, or the portfolio is Tasdeer. Otherwise Stage 1.",
              "Restructured" = "Local Flag 1, QDB's restructuring flag. Includes restructuring overrides.",
              "Stage override" = "A reviewed override (the run's stage overrides) that put the customer in Stage 2 or 3.",
              "Tasdeer" = "Collectively assessed as Stage 2 regardless of any other trigger.",
              "Another facility of the customer" = "Contagion: nothing on the contract itself, but the customer has a facility in Stage 2 or 3. Tasdeer is excluded.",
              "Local Flag 3 and the default flag" = "Not reasons of their own. The ETL sets Local Flag 3 on every customer its rule puts in Stage 2 and on Stage 2 overrides, and the default flag when the worst DPD is over 90 or on a Stage 3 override, so they restate the stage.",
              "Staging consistency" = "Re-applies the staging rule and reports only genuine disagreements. A manual stage in the source (AccountMaster.Stage) cannot be replayed from the report, so it appears as a difference."),
              note = "Staging is decided per customer, but Tasdeer and contagion act per contract, so the Stage 2 reasons are contract-level."),

            reactable::reactableOutput(ns("t_stagedist")),
            uiOutput(ns("thr_note")),
            hr(),
            fluidRow(
              column(6, h5("Why each Stage 2 contract is in Stage 2"),
                     p(class = "small-muted", "A contract with more than one reason counts under each."),
                     uiOutput(ns("c_s2trig"))),
              column(6, h5("Reason combinations"),
                     p(class = "small-muted", "Each Stage 2 contract once, so these add up."),
                     uiOutput(ns("c_s2overlap")))),
            reactable::reactableOutput(ns("t_s2trig")),
            hr(), h5("Stage 3 drivers"),
            p(class = "small-muted", "Each Stage 3 customer once. The default flag is not counted separately: the ETL sets it when the worst DPD is over 90 or on a Stage 3 override."),
            reactable::reactableOutput(ns("t_s3")),
            hr(), h5("DPD distribution by stage (customers)"), uiOutput(ns("c_dpdstage")),
            hr(), h5("Staging consistency"), uiOutput(ns("t_stagechk"))),
          nav_panel("PD",
            qdb_help(list(
              "Lifetime PD" = "Cumulative probability of default from now to the end of the ECL horizon.",
              "Stage 3" = "Defaulted, so its PD is 100% and it is provisioned at the full outstanding. The report's PD Lifetime Value for Stage 3 is the curve's figure, which the provision does not use.",
              "Weighted PD" = "Exposure-weighted average PD for the segment \u2014 large exposures count more.",
              "Simple mean PD" = "Unweighted average across contracts. A gap against the weighted figure means the risk sits in a few large names.",
              "Rating scales" = "Internal (QDB 1+ \u2026 QDB 9) and external (Aaa \u2026 C) scales reuse hierarchy 1&ndash;21, so they are shown side by side, never mixed.",
              "PD term structure" = "One set of curves for the internally rated book and one for the externally rated, each shared by every portfolio on that scale, from the run's StPD output.",
              "Marginal PD" = "The increment each month: <code>cumulative PD(m) &minus; cumulative PD(m&minus;1)</code>. This is what the ECL sum uses."),
              note = "ECL coverage should rise as the rating worsens. A kink usually means a rating or curve mapping is wrong rather than a genuine risk pattern."),

            h5("Exposure-weighted lifetime PD"),
            fluidRow(column(4, selectInput(ns("pd_by"), NULL,
              choices = c("Stage" = "stage", "Portfolio" = "portfolio",
                          "Rating" = "rating", "Account type" = "account_type"),
              width = "100%"))),
            uiOutput(ns("pd_view")),
            hr(),
            h5("Lifetime PD distribution"), uiOutput(ns("c_pddist")),
            hr(), h5("PD term structure"),
            fluidRow(
              column(3, uiOutput(ns("pick_scale"))),
              column(6, uiOutput(ns("pick_ratings"))),
              column(3, selectInput(ns("curve_view"), "View",
                choices = c("Selected ratings" = "lines", "All ratings as a heatmap" = "heat"),
                width = "100%"))),
            fluidRow(
              column(6, h5("Cumulative"), uiOutput(ns("c_pdterm"))),
              column(6, h5("Marginal PD by month"), uiOutput(ns("c_pdmarg"))))),
          nav_panel("LGD & collateral",
            qdb_help(list(
              "LGD" = "Loss given default: <code>0.45 &times; max(0.5, (exposure &minus; collateral) / exposure)</code>, floored at 0.225.",
              "The 0.45" = "Base unsecured loss rate.",
              "The 0.5" = "At least half of any exposure is treated as unsecured, however much collateral is held.",
              "The 0.225 floor" = "The lowest LGD any contract can carry, being 0.45 &times; 0.5. It applies to Stage 2 as well.",
              "Collateral coverage" = "Net collateral value divided by exposure. Over 100% means over-collateralised \u2014 the floor still applies.",
              "On floor" = "Contracts sitting exactly at 0.225, i.e. secured enough that more collateral would not reduce the provision.",
              "Collateral by type" = "Total collateral value by type.",
              "Orphan allocations" = "Collateral allocations pointing at a collateral record that does not exist. LIC returns NaN coverage for these, which blanks the whole contract's ECL."),
              note = "Because of the floor, collateral beyond roughly 50% coverage does not reduce LGD any further."),
            fluidRow(
              column(6, h5("LGD distribution"), uiOutput(ns("c_lgddist"))),
              column(6, h5("Collateral coverage bands"), uiOutput(ns("c_collbands")))),
            hr(),
            fluidRow(
              column(6, h5("LGD vs collateral coverage"), uiOutput(ns("c_lgdscatter"))),
              column(6, h5("LGD floor incidence"), reactable::reactableOutput(ns("t_floor")))),
            hr(), h5("Collateral composition"), uiOutput(ns("c_coll"))),
          nav_panel("Exposure & maturity",
            qdb_help(list(
              "Run-off profile" = "Exposure and ECL grouped by remaining months to maturity \u2014 how quickly the book matures.",
              "Exposure bands" = "Contracts grouped by ticket size, showing whether the book is many small facilities or a few large ones.",
              "Vintage" = "Grouped by the year the facility was opened. Coverage by vintage shows whether a particular year's lending is performing worse.",
              "Delinquency" = "Grouped by days past due. Coverage should climb steeply across the buckets.",
              "ECL coverage %" = "ECL divided by exposure within each band.",
              "EAD run-off" = "Total exposure at default by month ahead, from the supplied monthly EAD curves: the expected amortisation of the book. Off BS, Al Dhameen and Tasdeer carry no curve, so they are outside it."),
              note = "Longer-dated exposure normally carries higher coverage, because the lifetime PD accumulates over more months."),
            fluidRow(
              column(6, h5("Run-off profile by remaining maturity"), uiOutput(ns("c_maturity"))),
              column(6, h5("Exposure size bands"), uiOutput(ns("c_sizebands")))),
            hr(),
            fluidRow(
              column(6, h5("Vintage by origination year"), uiOutput(ns("c_vintage"))),
              column(6, h5("Delinquency profile"), uiOutput(ns("c_dpd")))),
            hr(), h5("Aggregate EAD run-off (supplied curves)"), uiOutput(ns("c_runoff"))),
          nav_panel("Concentration",
            fluidRow(column(4, selectInput(ns("conc_level"), "Measure by",
              choices = c("Contract" = "contract", "Customer" = "customer"),
              selected = "customer", width = "100%"))),
            p(class = "small-muted", "One borrower with several facilities counts once."),
            qdb_help(list(
              "Measured by customer" = "One borrower with ten facilities is a single exposure, not ten. This is the right unit for concentration.",
              "Largest N share" = "Percentage of the total provision held by the largest N customers.",
              "HHI" = "Sum of the squared percentage shares of the provision. Near 0 means many equal exposures; 10,000 means one name holds everything.",
              "Equal customers" = "10,000 divided by the HHI \u2014 the number of equally sized customers that would give the same concentration. Easier to judge than the index.",
              "Concentration curve" = "The cumulative share of the provision held by the largest customers. A steep start means the provision depends on a few names."),
              note = "The 1,500 and 2,500 marks are the usual competition-authority bands \u2014 a market convention, not a QCB limit. Single-obligor and large-exposure limits are not tested here."),

            fluidRow(
              column(6, h5("Concentration of provision"), uiOutput(ns("c_lorenz"))),
              column(6, h5("Concentration measures"), uiOutput(ns("t_hhi")))),
            hr(), h5("Largest contributors to the provision"),
            p(class = "small-muted", "Follows the Measure by selection above."),
            reactable::reactableOutput(ns("t_top"))),
          nav_panel("Scenarios",
            p(class = "small-muted", style = "margin-top:8px",
              "The provision under each of the five scenarios, beside the weighted figure the run reports."),
            qdb_help(list(
              "Scenario" = "One of the five forward-looking macro scenarios. Each has its own PD curve.",
              "Severity z" = "The scenario's severity as a standard normal shift. Negative is a downturn.",
              "Weighted (reported)" = "The probability-weighted provision — the figure actually reported.",
              "vs Base" = "Difference against the Base Case scenario."),
              note = "Every run prices the book under all five scenarios as well as the weighted curve, so these come from the run's own outputs. What a different weighting would cost is on Stress testing → Macro & scenarios."),
            uiOutput(ns("scen_origin")),
            uiOutput(ns("c_scen")), reactable::reactableOutput(ns("t_scen"))),
          nav_panel("Data quality",
            p(class = "small-muted", style = "margin-top:8px",
              "Conditions that distort a provision."),
            qdb_help(list(
              "Exposure but zero ECL" = "A Stage 1 or 2 contract with a balance but no provision \u2014 usually a missing PD curve or unresolved collateral.",
              "ECL exceeds exposure" = "Provision larger than the balance. The engine caps this, so rows here mean the cap is off or the run predates it.",
              "Missing rating" = "No rating means no PD bucket, so no ECL can be computed.",
              "Collateral coverage missing" = "Coverage could not be computed, often an allocation pointing at a collateral record that is absent.",
              "Implausible months on book" = "A months-on-book outside a sensible range, which points at a bad open date.",
              "Drilling in" = "Open any finding to see the contracts it covers, largest exposure first."),
              note = "These are reported, never corrected. Severity is a guide to what distorts a provision most."),

            reactable::reactableOutput(ns("t_dq"))),
          nav_panel("Customer ECL calculator",
            p(class = "small-muted", style = "margin-top:8px",
              "Pick a customer and download an Excel workbook that rebuilds its provision from the run's inputs with formulas, so the figures can be checked against the report."),
            qdb_help(list(
              "What is in it" = "Summary, Settings, Lines (the customer's positions as the run read them), Overrides, Staging, LGD, PD (the run's monthly PD curves), EAD and ECL month by month.",
              "Inputs and formulas" = "Blue cells are the run's inputs; black cells are formulas. Change an input and the provision follows; the report column stays as booked.",
              "The check" = "The Summary marks each position OK when the workbook's ECL equals the report's to the cent, the overlay included.",
              "PD curve" = "The run's monthly cumulative PD curve (StPD.csv) for the position's portfolio and rating bucket, as an input: the macro model and the scenario weights behind it are the same for every customer."),
              note = "Each export is written to the audit log, since the workbook carries the customer's positions."),
            uiOutput(ns("calc_pick")),
            uiOutput(ns("calc_cust")),
            div(style = "margin-top:10px",
              downloadButton(ns("calc_dl"), "Download the calculator (.xlsx)",
                             class = "btn-sm btn-primary"))))
      }
    })

    # ---------------------------------------------------------- charts -----
    # -------------------------------------------------------- movement -----
    bridge_r <- reactive({
      req(identical(input$mode, "compare"))
      bump()
      ra <- input$run_a; rb <- input$run_b
      if (is.null(ra) || is.null(rb) || identical(ra, rb)) return(NULL)
      if (!exists("ecl_bridge", mode = "function"))
        return(list(ok = FALSE, reason = "this needs a newer ifrs9qdb (with ecl_bridge()); re-install the engine."))
      da <- .run_dir_for(ra); db <- .run_dir_for(rb)
      pa <- .report_path_for(ra); pb <- .report_path_for(rb)
      if (is.null(da) || is.null(db) || is.null(pa) || is.null(pb)) return(NULL)
      withProgress(message = "Repricing both runs, contract by contract\u2026", value = 0.4,
        tryCatch(.an_bridge(da, pa, db, pb),
                 error = function(e) list(ok = FALSE, reason = conditionMessage(e))))
    })
    bl_level <- reactive({
      lv <- input$bl_level %||% "book"
      if (lv %in% .BL_LEVELS) lv else "book"
    })
    bl_members_tbl <- reactive({
      br <- bridge_r(); lv <- bl_level()
      if (is.null(br) || !isTRUE(br$ok) || lv == "book") return(NULL)
      bridge_members(br$rows, lv)
    })
    # One picker per level, so each level keeps its own choice.
    .bl_pick_id <- function(lv) paste0("bl_members_", lv)
    bl_sel <- reactive({
      lv <- bl_level(); if (lv == "book") return(character(0))
      as.character(input[[.bl_pick_id(lv)]] %||% character(0))
    })
    output$bl_pick <- renderUI({
      lv <- bl_level(); m <- bl_members_tbl()
      if (lv == "book" || is.null(m)) return(NULL)
      if (nrow(m) == 0) return(.an_na("Nothing at this level in either run."))
      nm <- tolower(names(.BL_LEVELS)[.BL_LEVELS == lv])
      tagList(
        p(class = "small-muted", sprintf(
          "A contract counts in the %s it is in now (a derecognised one in the one it left), so Prior ECL is what these contracts had in the prior run%s",
          nm, if (lv %in% c("stage", "rating", "segment", "account_type"))
            sprintf(", not the prior run's total for that %s.", nm) else ".")),
        selectizeInput(ns(.bl_pick_id(lv)), paste("Which", .BL_PLURAL[[lv]]),
                       choices = NULL, multiple = TRUE, width = "100%",
                       options = list(placeholder = "Type an id or a name \u2014 largest move first",
                                      plugins = list("remove_button"))))
    })
    # The choices are sent as they are typed for (server = TRUE): a level can
    # hold thousands of customers or facilities.
    observe({
      lv <- bl_level(); m <- bl_members_tbl()
      if (lv == "book" || is.null(m) || nrow(m) == 0) return()
      lab <- sprintf("%s  \u00b7  ECL %s \u2192 %s (%s)", m$label, .an_money(m$ecl_a),
                     .an_money(m$ecl_b), .an_signed(m$change))
      keep <- intersect(isolate(input[[.bl_pick_id(lv)]]) %||% character(0), m$value)
      updateSelectizeInput(session, .bl_pick_id(lv), server = TRUE,
                           choices = stats::setNames(m$value, lab), selected = keep)
    })
    bl_view <- reactive({
      br <- bridge_r(); lv <- bl_level(); m <- bl_sel()
      if (is.null(br) || !isTRUE(br$ok) || (lv != "book" && length(m) == 0)) return(NULL)
      bridge_view(br$rows, lv, m)
    })

    .bl_move_style <- function(v)
      c(if (isTRUE(v > 0)) list(color = .AN_COL$err) else if (isTRUE(v < 0)) list(color = .AN_COL$ok),
        list(fontWeight = "600"))
    .bl_txt <- function(x) if (is.null(x) || length(x) == 0 || is.na(x[1])) "\u2014" else as.character(x[1])
    .bl_stage_mix <- function(p) {
      mix <- unlist(p$stage_exposure); tot <- sum(mix)
      if (!length(mix) || !tot) return("")
      mix <- mix[order(names(mix))]
      paste(sprintf("S%s %.0f%%", names(mix), 100 * mix / tot), collapse = " \u00b7 ")
    }
    .bl_ratings <- function(p) {
      r <- p$ratings
      if (is.null(r) || nrow(r) <= 1) return("")
      # shares of the selection's whole exposure, not of the ratings listed
      tot <- p$exposure %||% 0
      if (!is.finite(tot) || tot <= 0) tot <- sum(r$exposure)
      if (!tot) tot <- 1
      r <- utils::head(r, 3)
      paste(sprintf("%s %.0f%%", r$rating, 100 * r$exposure / tot), collapse = " \u00b7 ")
    }
    .bl_side <- function(title, sub, p) {
      single <- isTRUE(p$contracts == 1)
      stg <- if (is.null(p$worst_stage) || is.na(p$worst_stage)) "\u2014"
             else if (single) paste("Stage", p$worst_stage) else paste("worst", p$worst_stage)
      div(class = "qdb-card bl-side",
        div(class = "bl-side-hd", tags$b(title), span(class = "small-muted", sub)),
        qdb_stats(list(
          list(k = "Exposure", v = .an_money(p$exposure),
               s = paste("off balance", .an_money(p$exposure_off))),
          list(k = "ECL", v = .an_money(p$ecl), tone = "accent",
               s = paste0("coverage ", .an_pct0(p$coverage),
                          if (isTRUE(abs(p$overlay) > 0)) paste0(" \u00b7 overlay ", .an_money(p$overlay)) else "")),
          list(k = "Rating", v = .bl_txt(p$rating), s = .bl_ratings(p)),
          list(k = "Stage", v = stg, s = .bl_stage_mix(p)))),
        qdb_stats(list(
          list(k = "PD, lifetime", v = .an_pct0(100 * (p$pd %||% NA_real_))),
          list(k = "LGD", v = .an_pct0(100 * (p$lgd %||% NA_real_))),
          list(k = "Contracts", v = .an_money(p$contracts),
               s = paste(.an_money(p$customers), "customer(s)")),
          list(k = "Days past due",
               v = if (is.null(p$dpd_max) || is.na(p$dpd_max)) "\u2014" else .an_money(p$dpd_max),
               s = "the most, across the contracts"))))
    }

    output$bl_body <- renderUI({
      br <- bridge_r()
      if (is.null(br))
        return(qdb_empty("Pick two different runs that both have an ECL report.", "code-compare"))
      if (!isTRUE(br$ok))
        return(.an_na(paste("The movement could not be worked out:", br$reason)))
      lv <- bl_level()
      if (lv != "book" && length(bl_sel()) == 0)
        return(tagList(
          p(class = "small-muted", sprintf(
            "Every %s below, each with its own waterfall in one row. Choose one or more above to see the detail.",
            tolower(names(.BL_LEVELS)[.BL_LEVELS == lv]))),
          reactable::reactableOutput(ns("bl_by"))))
      v <- bl_view(); if (is.null(v)) return(NULL)
      ch <- br$changes
      mv <- v$closing - v$opening
      tone <- if (mv > 0) "err" else if (mv < 0) "ok" else ""
      sub <- function(side) paste(br$runs[[side]]$run, "\u00b7", .bl_txt(br$runs[[side]]$extract_date))
      chg_tbl <- function(items) {
        if (!length(items)) return(NULL)
        val <- function(x) if (is.null(x) || length(x) == 0 || is.na(x[1])) "\u2014" else as.character(x[1])
        tags$table(class = "table table-sm", style = "margin:4px 0 10px",
          tags$tr(tags$th("What"), tags$th("Prior"), tags$th("Current")),
          lapply(items, function(it) tags$tr(tags$td(tags$code(it$item)),
                                             tags$td(val(it$before)), tags$td(val(it$after)))))
      }
      tagList(
        fluidRow(column(6, .bl_side("Prior run", sub("a"), v$before)),
                 column(6, .bl_side("Current run", sub("b"), v$after))),
        qdb_stats(list(
          list(k = "Prior ECL", v = .an_money(v$opening), tone = "accent"),
          list(k = "Current ECL", v = .an_money(v$closing), tone = "accent"),
          list(k = "Movement", v = .an_signed(mv), tone = tone),
          list(k = "% change", v = if (v$opening != 0) .an_pct(100 * mv / v$opening) else "\u2014",
               tone = tone))),
        uiOutput(ns("bl_chart")),
        p(class = "small-muted", sprintf(
          "Each contract is repriced from the prior run to this one a cause at a time, in the order shown, so the bars sum exactly to the current ECL (residual %.2f).%s",
          v$residual, if (isTRUE(ch$model_changed)) "" else " The PD model did not change, so Model change is zero.")),
        lapply(br$notes, function(n) div(class = "alert alert-info", style = "padding:0.5em 0.8em; margin:0.4em 0", n)),
        reactable::reactableOutput(ns("bl_steps")),
        tags$details(class = "qdb-help",
          tags$summary(icon("sliders"), "What changed in the model, the macro inputs and the ECL settings"),
          if (!isTRUE(ch$known)) p("A run has no frozen config, so this cannot be said.")
          else tagList(
            p(tags$b("PD model"), if (isTRUE(ch$model_changed)) " \u2014 changed" else " \u2014 unchanged"),
            chg_tbl(ch$model),
            p(tags$b("Macro inputs"), if (isTRUE(ch$macro_changed)) " \u2014 changed" else " \u2014 unchanged"),
            chg_tbl(ch$macro),
            p(tags$b("ECL settings"), if (isTRUE(ch$settings_changed)) " \u2014 changed" else " \u2014 unchanged"),
            chg_tbl(ch$settings))),
        h5(style = "margin-top:14px", "Contracts behind it"),
        p(class = "small-muted", "Largest moves first. Each row's causes sum to its change."),
        reactable::reactableOutput(ns("bl_contracts")),
        h5(style = "margin-top:14px",
           if (lv == "book") "By segment" else paste("Every", tolower(names(.BL_LEVELS)[.BL_LEVELS == lv]))),
        reactable::reactableOutput(ns("bl_by")))
    })

    output$bl_chart <- renderUI({
      v <- bl_view(); br <- bridge_r(); if (is.null(v)) return(NULL)
      s <- .bl_shown(v$steps, br)
      if (!.an_echarts())
        return(p(class = "small-muted", "The chart needs the echarts4r package; the steps are in the table below."))
      .an_waterfall(s, zoom = TRUE)
    })

    output$bl_steps <- reactable::renderReactable({
      v <- bl_view(); br <- bridge_r(); if (is.null(v)) return(NULL)
      s <- .bl_shown(v$steps, br)
      s <- s[s$kind == "delta", , drop = FALSE]
      df <- data.frame(Step = s$label, Amount = s$amount,
                       `% of prior` = if (v$opening != 0) 100 * s$amount / v$opening else NA_real_,
                       `What it is` = unname(.BL_WHAT[s$key]), check.names = FALSE)
      .rt(df, sortable = FALSE, pagination = FALSE, columns = list(
        Step = reactable::colDef(minWidth = 150),
        Amount = reactable::colDef(align = "right", minWidth = 110, cell = function(v) .an_signed(v),
          style = .bl_move_style),
        `% of prior` = reactable::colDef(align = "right", minWidth = 80,
          cell = function(v) if (is.na(v)) "\u2014" else .an_pct(v)),
        `What it is` = reactable::colDef(minWidth = 320)))
    })

    output$bl_contracts <- reactable::renderReactable({
      v <- bl_view(); if (is.null(v)) return(NULL)
      ct <- v$contracts
      if (is.null(ct) || nrow(ct) == 0) return(NULL)
      ct <- .bl_grouped(ct)
      comp <- .bl_shown_cols(ct)
      arrow <- function(a, b) paste(ifelse(is.na(a), "", a), "\u2192", ifelse(is.na(b), "", b))
      df <- data.frame(Contract = ct$contract,
                       Customer = ifelse(is.na(ct$customer_b), ct$customer_a, ct$customer_b),
                       Name = ct$name, Status = ct$status,
                       Stage = arrow(ct$stage_a, ct$stage_b),
                       Rating = arrow(ct$rating_a, ct$rating_b),
                       `ECL prior` = ct$ecl_a, `ECL current` = ct$ecl_b, Change = ct$change,
                       check.names = FALSE, stringsAsFactors = FALSE)
      for (k in comp) df[[.BL_LABEL[[k]]]] <- ct[[paste0("g_", k)]]
      money <- c("ECL prior", "ECL current", unname(.BL_LABEL[comp]))
      cols <- c(stats::setNames(lapply(money, function(nm)
                  reactable::colDef(align = "right", minWidth = 105,
                                    cell = function(v) .an_money(v))), money),
                list(Change = reactable::colDef(align = "right", minWidth = 105,
                  cell = function(v) .an_signed(v),
                  style = .bl_move_style),
                  Name = reactable::colDef(minWidth = 170)))
      .rt(df, defaultPageSize = 10, searchable = TRUE, columns = cols)
    })

    output$bl_by <- reactable::renderReactable({
      br <- bridge_r(); if (is.null(br) || !isTRUE(br$ok)) return(NULL)
      lv <- bl_level(); if (lv == "book") lv <- "segment"
      by <- bridge_by(br$rows, lv)
      if (is.null(by) || nrow(by) == 0) return(NULL)
      by <- .bl_grouped(by)
      steps <- .bl_shown_cols(by)
      lvl_name <- names(.BL_LEVELS)[.BL_LEVELS == lv]
      # the group with a customer's or a facility's name
      df <- data.frame(g = by$label %||% by$group, `Prior ECL` = by$opening,
                       check.names = FALSE, stringsAsFactors = FALSE)
      names(df)[1] <- lvl_name
      for (k in steps) df[[.BL_LABEL[[k]]]] <- by[[paste0("g_", k)]]
      df$`Current ECL` <- by$closing
      df$Change <- by$change
      money <- c("Prior ECL", unname(.BL_LABEL[steps]), "Current ECL")
      cols <- c(stats::setNames(lapply(money, function(nm)
                  reactable::colDef(align = "right", minWidth = 105,
                                    cell = function(v) .an_money(v))), money),
                stats::setNames(list(reactable::colDef(minWidth = 190)), lvl_name),
                list(Change = reactable::colDef(align = "right", minWidth = 105,
                  cell = function(v) .an_signed(v),
                  style = .bl_move_style)))
      .rt(df, defaultPageSize = 12, searchable = nrow(df) > 12, columns = cols)
    })

    output$c_ctrans <- renderUI({
      # Filter to the same portfolios as the rating charts, so switching the
      # scale changes every chart on the tab rather than only the rating ones.
      ia <- inp_b() %||% inp_a()
      rt <- input$rating_type2 %||% "1"
      da <- an_filter_rating_type(dat_a(), ia, rt)
      db <- an_filter_rating_type(dat_b(), ia, rt)
      tr <- customer_stage_migration(da, db)
      if (is.null(tr)) return(.an_na("No overlapping customers."))
      tr$from_l <- paste("Stage", tr$from); tr$to_l <- paste("Stage", tr$to)
      if (!.an_echarts())
        return(qdb_reactable(data.frame(From = tr$from_l, To = tr$to_l,
                 Customers = tr$customers), searchable = FALSE, page_size = 9))
      echarts4r::e_charts(tr, from_l) |>
        echarts4r::e_heatmap(to_l, customers,
                             itemStyle = list(borderWidth = 2, borderColor = "#fff"),
                             label = list(show = TRUE, fontSize = 12, fontWeight = "bold",
                                          formatter = htmlwidgets::JS(
                               "function(p){return p.value[2];}"))) |>
        echarts4r::e_visual_map(customers,
                                inRange = list(color = c("#f7f4f9", "#c9a9d8", .AN_COL$plum)),
                                orient = "vertical", right = 0, top = "middle") |>
        echarts4r::e_tooltip(formatter = htmlwidgets::JS(
          "function(p){return p.value[0]+' \u2192 '+p.value[1]+'<br/><b>'+p.value[2]+'</b> customers';}")) |>
        echarts4r::e_grid(left = 80, right = 95, bottom = 40, top = 15) |>
        echarts4r::e_x_axis(name = "From") |> echarts4r::e_y_axis(name = "To")
    })


    output$c_migsum <- renderUI({
      ia <- inp_b() %||% inp_a(); rt <- input$rating_type2 %||% "1"
      ms <- migration_summary(an_filter_rating_type(dat_a(), ia, rt),
                              an_filter_rating_type(dat_b(), ia, rt),
                              levels = .rt2_levels())
      if (is.null(ms)) return(.an_na("Ratings not comparable between these runs."))
      if (!.an_echarts())
        return(qdb_reactable(data.frame(Direction = ms$direction, Contracts = ms$n,
                 Exposure = .an_money(ms$exposure)), searchable = FALSE))
      ms$col <- ifelse(ms$direction == "downgrade", .AN_COL$err,
                ifelse(ms$direction == "upgrade", .AN_COL$ok, .AN_COL$grey))
      cj <- htmlwidgets::JS(sprintf("function(p){var c=[%s]; return c[p.dataIndex];}",
                                    paste0("'", ms$col, "'", collapse = ",")))
      echarts4r::e_charts(ms, direction) |>
        echarts4r::e_bar(n, legend = FALSE, bar_width = "50%",
                         itemStyle = list(color = cj)) |>
        echarts4r::e_tooltip() |>
        echarts4r::e_grid(left = 60, right = 20, top = 15, bottom = 35)
    })

    output$c_ratmig <- renderUI({
      ia <- inp_b() %||% inp_a(); rt <- input$rating_type2 %||% "1"
      mg <- rating_migration(an_filter_rating_type(dat_a(), ia, rt),
                             an_filter_rating_type(dat_b(), ia, rt),
                             levels = .rt2_levels())
      if (is.null(mg)) return(.an_na("Ratings not comparable between these runs."))
      lv <- attr(mg, "levels")
      if (!.an_echarts())
        return(qdb_reactable(data.frame(From = mg$from, To = mg$to, Contracts = mg$n),
                             page_size = 12))
      echarts4r::e_charts(mg, from) |>
        echarts4r::e_heatmap(to, n, itemStyle = list(borderWidth = 1.5, borderColor = "#fff")) |>
        echarts4r::e_visual_map(n, inRange = list(color = c("#f7f4f9", "#c9a9d8", .AN_COL$plum)),
                                orient = "horizontal", left = "center", bottom = 0) |>
        echarts4r::e_tooltip(formatter = htmlwidgets::JS(
          "function(p){return p.value[0]+' \u2192 '+p.value[1]+'<br/><b>'+p.value[2]+'</b> contracts';}")) |>
        echarts4r::e_x_axis(name = "From", type = "category", data = lv,
                            axisLabel = list(rotate = 35, fontSize = 10)) |>
        echarts4r::e_y_axis(name = "To", type = "category", data = lv) |>
        echarts4r::e_grid(left = 95, right = 30, bottom = 95, top = 15)
    })

    output$c_stage <- renderUI({
      p <- run_profile(dat_a(), "stage"); if (is.null(p)) return(NULL)
      p$group <- paste("Stage", p$group)
      if (!.an_echarts())
        return(qdb_reactable(data.frame(Stage = p$group, ECL = .an_money(p$ecl),
                 `ECL coverage %` = .an_pct0(p$coverage)), searchable = FALSE))
      echarts4r::e_charts(p, group) |>
        echarts4r::e_bar(ecl, name = "ECL", bar_width = "45%",
                         itemStyle = list(color = .AN_COL$plum)) |>
        echarts4r::e_line(coverage, name = "ECL coverage %", y_index = 1, symbolSize = 9,
                          lineStyle = list(width = 3),
                          itemStyle = list(color = .AN_COL$teal)) |>
        echarts4r::e_y_axis(index = 0, axisLabel = list(formatter = .an_m_axis())) |>
        echarts4r::e_tooltip(trigger = "axis") |> echarts4r::e_legend(bottom = 0) |>
        echarts4r::e_grid(left = 70, right = 55, top = 20, bottom = 50)
    })

    output$c_portfolio <- renderUI({
      p <- run_profile(dat_a(), "portfolio"); if (is.null(p)) return(NULL)
      p <- utils::head(p, 8)
      if (!.an_echarts())
        return(qdb_reactable(data.frame(Portfolio = p$group, ECL = .an_money(p$ecl)),
                             searchable = FALSE))
      echarts4r::e_charts(p, group) |>
        echarts4r::e_pie(ecl, radius = c("45%", "70%"), legend = FALSE,
                         label = list(formatter = "{b}: {d}%", fontSize = 11)) |>
        echarts4r::e_color(.AN_PAL) |> echarts4r::e_tooltip()
    })

    output$c_heat <- renderUI({
      sm <- segment_matrix(dat_a(), "portfolio", "stage", "coverage")
      if (is.null(sm)) return(NULL)
      sm$col <- paste("Stage", sm$col)
      sm <- sm[!is.na(sm$value), , drop = FALSE]
      if (nrow(sm) == 0) return(NULL)
      if (!.an_echarts())
        return(qdb_reactable(data.frame(Portfolio = sm$row, Stage = sm$col,
                 `ECL coverage %` = .an_pct0(sm$value)), page_size = 12))
      echarts4r::e_charts(sm, row) |>
        echarts4r::e_heatmap(col, value, itemStyle = list(borderWidth = 2, borderColor = "#fff")) |>
        echarts4r::e_visual_map(value, inRange = list(color = c("#e7f6f0", "#fdf3e3", "#fce9e9")),
                                orient = "vertical", right = 0, top = "middle") |>
        echarts4r::e_tooltip(formatter = htmlwidgets::JS(
          "function(p){return p.value[0]+' \u00b7 '+p.value[1]+'<br/>coverage <b>'+Number(p.value[2]).toFixed(2)+'%</b>';}")) |>
        echarts4r::e_x_axis(axisLabel = list(rotate = 25, fontSize = 10)) |>
        echarts4r::e_grid(left = 95, right = 90, bottom = 75, top = 15)
    })

    output$c_pddist <- renderUI({
      h <- pd_distribution(dat_a()); if (is.null(h)) return(.an_na())
      if (!.an_echarts())
        return(qdb_reactable(data.frame(Band = h$band, Contracts = h$contracts),
                             searchable = FALSE, page_size = 10))
      echarts4r::e_charts(h, band) |>
        echarts4r::e_bar(contracts, legend = FALSE, bar_width = "70%",
                         itemStyle = list(color = .AN_COL$plum)) |>
        echarts4r::e_x_axis(axisLabel = list(rotate = 40, fontSize = 9)) |>
        echarts4r::e_tooltip() |>
        echarts4r::e_grid(left = 65, right = 20, top = 15, bottom = 75)
    })

    # by rating: one chart per scale, side by side, best grade first
    .pd_scale_chart <- function(p) {
      if (is.null(p) || nrow(p) == 0) return(.an_na("No contracts on this rating scale."))
      if (!.an_echarts())
        return(qdb_reactable(data.frame(Rating = p$rating, `PD %` = .an_pct0(100 * p$pd_w),
                 `ECL coverage %` = .an_pct0(p$coverage), check.names = FALSE), page_size = 25))
      p$pd_pct <- 100 * p$pd_w
      echarts4r::e_charts(p, rating) |>
        echarts4r::e_bar(pd_pct, name = "Weighted lifetime PD %", bar_width = "45%",
                         itemStyle = list(color = .AN_COL$plum_l)) |>
        echarts4r::e_line(coverage, name = "ECL coverage %", symbolSize = 8,
                          lineStyle = list(width = 3),
                          itemStyle = list(color = .AN_COL$teal)) |>
        echarts4r::e_x_axis(axisLabel = list(rotate = 40, fontSize = 9)) |>
        echarts4r::e_tooltip(trigger = "axis") |> echarts4r::e_legend(bottom = 0) |>
        echarts4r::e_grid(left = 60, right = 25, top = 15, bottom = 80)
    }
    .pd_scale_table <- function(p) {
      if (is.null(p) || nrow(p) == 0) return(NULL)
      df <- data.frame(Rating = p$rating, Contracts = p$contracts, Exposure = p$exposure,
                       `Weighted PD %` = 100 * p$pd_w, `Simple mean PD %` = 100 * p$pd_mean,
                       ECL = p$ecl, `ECL coverage %` = p$coverage, check.names = FALSE)
      .rt(df, defaultPageSize = 25, sortable = FALSE, columns = list(
        Exposure = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        ECL = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        `Weighted PD %` = reactable::colDef(align = "right", cell = function(v) .an_pct0(v)),
        `Simple mean PD %` = reactable::colDef(align = "right", cell = function(v) .an_pct0(v)),
        `ECL coverage %` = reactable::colDef(align = "right", cell = function(v) .an_pct0(v))))
    }
    .pd_scales <- reactive({ pd_by_scale(dat_a(), inp_a()) })

    output$c_lgddist <- renderUI({
      h <- lgd_distribution(dat_a())
      if (is.null(h)) return(.an_na("LGD is not populated in this report."))
      if (!.an_echarts())
        return(qdb_reactable(data.frame(LGD = h$band, Contracts = h$contracts),
                             searchable = FALSE, page_size = 10))
      echarts4r::e_charts(h, band) |>
        echarts4r::e_bar(contracts, legend = FALSE, bar_width = "70%",
                         itemStyle = list(color = .AN_COL$teal)) |>
        echarts4r::e_x_axis(name = "LGD", axisLabel = list(rotate = 40, fontSize = 9)) |>
        echarts4r::e_tooltip() |>
        echarts4r::e_grid(left = 65, right = 20, top = 15, bottom = 70)
    })

    output$c_collbands <- renderUI({
      cb <- collateral_bands(dat_a()); if (is.null(cb)) return(.an_na())
      if (!.an_echarts())
        return(qdb_reactable(data.frame(Band = cb$band, Contracts = cb$contracts,
                 Exposure = .an_money(cb$exposure)), searchable = FALSE))
      echarts4r::e_charts(cb, band) |>
        echarts4r::e_bar(exposure, name = "Exposure", legend = FALSE, bar_width = "60%",
                         itemStyle = list(color = .AN_COL$plum)) |>
        echarts4r::e_y_axis(axisLabel = list(formatter = .an_m_axis())) |>
        echarts4r::e_x_axis(axisLabel = list(rotate = 25, fontSize = 10)) |>
        echarts4r::e_tooltip() |>
        echarts4r::e_grid(left = 75, right = 20, top = 15, bottom = 65)
    })

    output$c_lgdscatter <- renderUI({
      s <- lgd_vs_collateral(dat_a())
      if (is.null(s)) return(.an_na("Needs both LGD and collateral coverage."))
      if (!.an_echarts())
        return(qdb_reactable(utils::head(data.frame(`Coll cov %` = round(s$collcov, 1),
                 LGD = round(s$lgd, 3), check.names = FALSE), 50), page_size = 10))
      echarts4r::e_charts(s, collcov) |>
        echarts4r::e_scatter(lgd, symbol_size = 6, legend = FALSE,
                             itemStyle = list(color = .AN_COL$plum, opacity = 0.45)) |>
        echarts4r::e_x_axis(name = "Collateral coverage %", nameLocation = "middle", nameGap = 28) |>
        echarts4r::e_y_axis(name = "LGD") |>
        echarts4r::e_tooltip() |>
        echarts4r::e_grid(left = 65, right = 25, top = 15, bottom = 55)
    })

    output$c_maturity <- renderUI({
      m <- maturity_profile(dat_a()); if (is.null(m)) return(.an_na())
      if (!.an_echarts())
        return(qdb_reactable(data.frame(Band = m$band, Exposure = .an_money(m$exposure),
                 `ECL coverage %` = .an_pct0(m$coverage)), searchable = FALSE))
      echarts4r::e_charts(m, band) |>
        echarts4r::e_bar(exposure, name = "Exposure", bar_width = "50%",
                         itemStyle = list(color = .AN_COL$plum)) |>
        echarts4r::e_line(coverage, name = "ECL coverage %", y_index = 1, symbolSize = 8,
                          lineStyle = list(width = 3), itemStyle = list(color = .AN_COL$teal)) |>
        echarts4r::e_y_axis(index = 0, axisLabel = list(formatter = .an_m_axis())) |>
        echarts4r::e_x_axis(axisLabel = list(rotate = 30, fontSize = 9)) |>
        echarts4r::e_tooltip(trigger = "axis") |> echarts4r::e_legend(bottom = 0) |>
        echarts4r::e_grid(left = 70, right = 55, top = 15, bottom = 75)
    })

    output$c_sizebands <- renderUI({
      b <- exposure_bands(dat_a()); if (is.null(b)) return(NULL)
      if (!.an_echarts())
        return(qdb_reactable(data.frame(Band = b$band, Contracts = b$contracts,
                 Exposure = .an_money(b$exposure)), searchable = FALSE))
      echarts4r::e_charts(b, band) |>
        echarts4r::e_bar(contracts, name = "Contracts", bar_width = "50%",
                         itemStyle = list(color = .AN_COL$plum_l)) |>
        echarts4r::e_line(exposure, name = "Exposure", y_index = 1, symbolSize = 7,
                          itemStyle = list(color = .AN_COL$teal)) |>
        echarts4r::e_y_axis(index = 1, axisLabel = list(formatter = .an_m_axis())) |>
        echarts4r::e_x_axis(axisLabel = list(rotate = 30, fontSize = 9)) |>
        echarts4r::e_tooltip(trigger = "axis") |> echarts4r::e_legend(bottom = 0) |>
        echarts4r::e_grid(left = 60, right = 65, top = 15, bottom = 75)
    })

    output$c_vintage <- renderUI({
      v <- vintage_profile(dat_a()); if (is.null(v)) return(.an_na())
      if (!.an_echarts())
        return(qdb_reactable(data.frame(Vintage = v$vintage, Exposure = .an_money(v$exposure),
                 `ECL coverage %` = .an_pct0(v$coverage)), searchable = FALSE, page_size = 12))
      v$vintage <- as.character(v$vintage)
      echarts4r::e_charts(v, vintage) |>
        echarts4r::e_bar(exposure, name = "Exposure", bar_width = "55%",
                         itemStyle = list(color = .AN_COL$plum)) |>
        echarts4r::e_line(coverage, name = "ECL coverage %", y_index = 1, symbolSize = 8,
                          lineStyle = list(width = 3), itemStyle = list(color = .AN_COL$warn)) |>
        echarts4r::e_y_axis(index = 0, axisLabel = list(formatter = .an_m_axis())) |>
        echarts4r::e_tooltip(trigger = "axis") |> echarts4r::e_legend(bottom = 0) |>
        echarts4r::e_grid(left = 70, right = 55, top = 15, bottom = 50)
    })

    output$c_dpd <- renderUI({
      p <- dpd_profile(dat_a()); if (is.null(p)) return(.an_na())
      if (!.an_echarts())
        return(qdb_reactable(data.frame(Bucket = p$band, Contracts = p$contracts,
                 Exposure = .an_money(p$exposure), `ECL coverage %` = .an_pct0(p$coverage)),
                 searchable = FALSE))
      echarts4r::e_charts(p, band) |>
        echarts4r::e_bar(exposure, name = "Exposure", bar_width = "55%",
                         itemStyle = list(color = .AN_COL$warn)) |>
        echarts4r::e_line(coverage, name = "ECL coverage %", y_index = 1, symbolSize = 8,
                          lineStyle = list(width = 3), itemStyle = list(color = .AN_COL$err)) |>
        echarts4r::e_y_axis(index = 0, axisLabel = list(formatter = .an_m_axis())) |>
        echarts4r::e_tooltip(trigger = "axis") |> echarts4r::e_legend(bottom = 0) |>
        echarts4r::e_grid(left = 70, right = 55, top = 15, bottom = 50)
    })

    output$c_lorenz <- renderUI({
      lv <- input$conc_level %||% "customer"
      l <- lorenz_curve(dat_a(), level = lv)
      if (is.null(l)) return(.an_na("Not enough rows to measure concentration."))
      if (!.an_echarts()) {
        cc <- concentration(dat_a(), level = lv)
        return(qdb_reactable(data.frame(`Top N` = cc$top_n, Share = .an_pct0(cc$share),
                 check.names = FALSE), searchable = FALSE))
      }
      echarts4r::e_charts(l, pct_contracts) |>
        echarts4r::e_area(pct_ecl, legend = FALSE, symbol = "none",
                          itemStyle = list(color = .AN_COL$plum),
                          areaStyle = list(opacity = 0.18)) |>
        echarts4r::e_x_axis(name = sprintf("%% of %ss", lv), nameLocation = "middle",
                            nameGap = 28, max = 100) |>
        echarts4r::e_y_axis(name = "% of ECL", max = 100) |>
        echarts4r::e_tooltip(trigger = "axis", formatter = htmlwidgets::JS(
          sprintf("function(p){return 'Top '+Number(p[0].value[0]).toFixed(0)+'%% of %ss hold <b>'+Number(p[0].value[1]).toFixed(1)+'%%</b> of ECL';}", lv))) |>
        echarts4r::e_grid(left = 60, right = 25, top = 20, bottom = 55)
    })

    # ---- scenario & what-if ------------------------------------------------
    # Runs whose ecl_scenario is a named scenario (not "weighted"): each is the
    # one-hot run for that scenario.
    # Per-scenario provisions. Preferred source is the SELECTED run: its frozen
    # config lets the PD chain be re-run once per scenario and priced against
    # the run's own EAD curves and LGDs. Falls back to one-hot scenario runs if
    # that is not possible (e.g. a run predating config_used).
    scen_runs <- reactive({
      r <- runs_tbl(); if (is.null(r)) return(NULL)
      sc <- as.character(r$ecl_scenario %||% rep(NA, nrow(r)))
      keep <- !is.na(sc) & nzchar(sc) & tolower(sc) != "weighted"
      if (!any(keep)) return(NULL)
      x <- r[keep, , drop = FALSE]
      x <- x[order(x$started_at, decreasing = TRUE), , drop = FALSE]
      x[!duplicated(as.character(x$ecl_scenario)), , drop = FALSE]
    })

    scen_source <- reactive({
      d <- .out_dir_for(input$run_a)
      # Preferred: the run's own per-scenario reports, written by every run.
      if (!is.null(d)) {
        fo <- scenario_ecl_from_outputs(d)
        if (isTRUE(fo$ok))
          return(list(ok = TRUE, ecl = fo$ecl, origin = "outputs",
                      weighted = fo$weighted))
      }
      if (!is.null(d)) {
        r <- tryCatch(scenario_ecl_single_run(d, inp_a(), dat_a()),
                      error = function(e) list(ok = FALSE, reason = conditionMessage(e)))
        if (isTRUE(r$ok))
          return(list(ok = TRUE, ecl = r$ecl, origin = "single",
                      covered = r$covered, contracts = r$contracts))
        single_reason <- r$reason
      } else single_reason <- "No run selected."
      sr <- scen_runs()
      if (!is.null(sr) && nrow(sr) > 0) {
        out <- vapply(seq_len(nrow(sr)), function(i) {
          rp <- .report_for(sr$run_id[i])
          if (is.null(rp) || is.null(rp$data)) return(NA_real_)
          report_total(rp$data)
        }, numeric(1))
        names(out) <- as.character(sr$ecl_scenario)
        out <- out[!is.na(out)]
        if (length(out) > 0)
          return(list(ok = TRUE, ecl = out, origin = "runs"))
      }
      list(ok = FALSE, reason = paste(
        "Per-scenario provisions could not be derived from this run:", single_reason,
        "You can also produce them by running the pipeline once per scenario."))
    })

    scen_ecl <- reactive({ se <- scen_source(); if (isTRUE(se$ok)) se$ecl else NULL })
    scen_sev <- reactive({ scenario_severity(.out_dir_for(input$run_a)) })

    # The internal weights the run is priced with, as the engine resolves them
    # from its frozen config -- not the config file's explicit_weights, which
    # on an automatic mode are a rounded snapshot.
    run_weights <- reactive({
      bump()
      d <- .out_dir_for(input$run_a); if (is.null(d)) return(NULL)
      rw <- run_scenario_weights(d)
      if (isTRUE(rw$ok)) unlist(rw$internal) else NULL
    })

    output$scen_origin <- renderUI({
      se <- scen_source()
      if (is.null(se) || isFALSE(se$ok))
        return(div(class = "alert alert-secondary", style = "margin-top:8px",
                   icon("circle-info"), " ",
                   se$reason %||% "Scenario provisions are not available for this run."))
      if (identical(se$origin, "outputs"))
        div(class = "alert alert-success", style = "margin-top:8px",
            icon("circle-check"), " From this run's per-scenario ECL reports.")
      else if (identical(se$origin, "single"))
        div(class = "alert alert-info", style = "margin-top:8px",
            icon("circle-info"),
            sprintf(" Rebuilt from this run's frozen config (%s contracts).",
                    .an_money(se$covered %||% NA)))
      else
        div(class = "alert alert-secondary", style = "margin-top:8px",
            icon("circle-info"), " From separate per-scenario runs.")
    })

    output$c_scen <- renderUI({
      e <- scen_ecl(); if (is.null(e) || length(e) == 0) return(.an_na())
      cmp <- scenario_comparison(e, scen_sev()); if (is.null(cmp)) return(NULL)
      se <- scen_source()
      if (!is.null(se$weighted) && !is.na(se$weighted))
        cmp <- rbind(cmp[, c("scenario", "ecl")],
                     data.frame(scenario = "Weighted (reported)", ecl = se$weighted))[
                       , c("scenario", "ecl"), drop = FALSE]
      if (!.an_echarts())
        return(qdb_reactable(data.frame(Scenario = cmp$scenario, ECL = .an_money(cmp$ecl)),
                             searchable = FALSE))
      cj <- htmlwidgets::JS(sprintf("function(p){var c=[%s]; return c[p.dataIndex];}",
        paste0("'", ifelse(grepl("^Weighted", cmp$scenario), .AN_COL$teal, .AN_COL$plum),
               "'", collapse = ",")))
      echarts4r::e_charts(cmp, scenario) |>
        echarts4r::e_bar(ecl, legend = FALSE, bar_width = "55%",
                         itemStyle = list(color = cj)) |>
        echarts4r::e_y_axis(axisLabel = list(formatter = .an_m_axis())) |>
        echarts4r::e_x_axis(axisLabel = list(rotate = 20, fontSize = 10)) |>
        echarts4r::e_tooltip() |>
        echarts4r::e_grid(left = 80, right = 25, top = 15, bottom = 70)
    })

    output$t_scen <- reactable::renderReactable({
      e <- scen_ecl(); if (is.null(e)) return(NULL)
      cmp <- scenario_comparison(e, scen_sev()); if (is.null(cmp)) return(NULL)
      se <- scen_source()
      if (!is.null(se$weighted) && !is.na(se$weighted)) {
        add <- cmp[1, , drop = FALSE]; add[] <- NA
        add$scenario <- "Weighted (reported provision)"; add$ecl <- se$weighted
        if ("vs_base" %in% colnames(cmp)) {
          b <- cmp$ecl[cmp$scenario == "Base Case"]
          if (length(b) == 1 && b > 0) {
            add$vs_base <- se$weighted - b
            add$vs_base_pct <- 100 * (se$weighted - b) / b
          }
        }
        cmp <- rbind(cmp, add)
      }
      df <- data.frame(Scenario = cmp$scenario,
                       `Severity z` = if ("severity_z" %in% colnames(cmp)) cmp$severity_z else NA,
                       ECL = cmp$ecl,
                       `vs Base` = if ("vs_base" %in% colnames(cmp)) cmp$vs_base else NA,
                       `vs Base %` = if ("vs_base_pct" %in% colnames(cmp)) cmp$vs_base_pct else NA,
                       check.names = FALSE)
      .rt(df, sortable = FALSE, pagination = FALSE, columns = list(
        `Severity z` = reactable::colDef(align = "right",
          cell = function(v) if (is.na(v)) "\u2014" else sprintf("%+.4f", v)),
        ECL = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        `vs Base` = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        `vs Base %` = reactable::colDef(align = "right",
          cell = function(v) if (is.na(v)) "\u2014" else .an_pct(v))))
    })

    # The weights set by hand on Macro & scenarios, in percent, or NULL when
    # the weights are not set by hand. An input left at its (rounded) starting
    # value is the run's own weight exactly, so the run's own weights reprice
    # to the booked figure.
    mev_weights_in <- reactive({
      if (!identical(input$mev_wmode, "custom")) return(NULL)
      w <- run_weights(); if (is.null(w)) return(NULL)
      out <- vapply(names(w), function(s) {
        v <- suppressWarnings(as.numeric(input[[paste0("mevw_", make.names(s))]]))
        if (length(v) != 1 || is.na(v) || isTRUE(all.equal(v, round(100 * w[[s]], 2))))
          100 * w[[s]] else v
      }, numeric(1))
      stats::setNames(out, names(w))
    })

    # Ten points onto each scenario in turn, from the weights set by hand or
    # the run's own, each repriced as Rebuild and reprice does.
    sens_r <- eventReactive(input$sens_run, {
      w <- mev_weights_in(); i <- inp_a(); d <- dat_a(); od <- .out_dir_for(input$run_a)
      if (is.null(i) || isFALSE(i$ok) || is.null(d) || is.null(od)) return(NULL)
      if (!is.null(w) && sum(w) <= 0) return(NULL)
      withProgress(message = "Repricing the book once per scenario…", value = 0.3,
        scenario_weights_sensitivity(i, d, od,
                                     weights = if (is.null(w)) NULL else w / sum(w)))
    }, ignoreInit = TRUE)

    output$c_sens <- renderUI({
      sv <- sens_r()
      if (is.null(sv)) return(NULL)
      shocked <- any(vapply(unique(mev_base()$idx %||% integer(0)), function(k) {
        v <- suppressWarnings(as.numeric(input[[paste0("mev_shock_", k)]]))
        length(v) == 1 && !is.na(v) && v != 0
      }, logical(1)))
      head_ <- tagList(hr(), h5("Ten points onto each scenario"),
        p(class = "small-muted", paste0(
          "Each row adds 10 points to one scenario, taken from the others in proportion, and reprices the book on this run's own forecast",
          if (shocked) " (the shifts above are not applied)" else "",
          ". If the downturn were ten points more likely, what would be booked?")))
      if (!.an_echarts())
        return(tagList(head_, qdb_reactable(data.frame(Scenario = sv$scenario,
                 Change = .an_money(sv$change)), searchable = FALSE)))
      sv <- sv[order(sv$change), , drop = FALSE]
      cj <- htmlwidgets::JS(sprintf("function(p){var c=[%s]; return c[p.dataIndex];}",
        paste0("'", ifelse(sv$change >= 0, .AN_COL$err, .AN_COL$ok), "'", collapse = ",")))
      tagList(head_,
        echarts4r::e_charts(sv, scenario) |>
          echarts4r::e_bar(change, legend = FALSE, bar_width = "55%",
                           itemStyle = list(color = cj)) |>
          echarts4r::e_flip_coords() |>
          echarts4r::e_x_axis(axisLabel = list(formatter = .an_m_axis())) |>
          echarts4r::e_tooltip() |>
          echarts4r::e_grid(left = 150, right = 30, top = 10, bottom = 30))
    })

    output$t_sens <- reactable::renderReactable({
      sv <- sens_r(); if (is.null(sv)) return(NULL)
      df <- data.frame(Scenario = sv$scenario,
                       `Weight before` = 100 * sv$weight_before,
                       `Weight after` = 100 * sv$weight_after,
                       `This scenario's ECL` = sv$ecl_scenario,
                       `Provision before` = sv$ecl_base,
                       `Provision after` = sv$ecl_shifted,
                       Change = sv$change, `Change %` = sv$pct, check.names = FALSE)
      .rt(df, sortable = FALSE, pagination = FALSE, columns = list(
        `Weight before` = reactable::colDef(align = "right", cell = function(v) .an_pct0(v)),
        `Weight after` = reactable::colDef(align = "right", cell = function(v) .an_pct0(v)),
        `This scenario's ECL` = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        `Provision before` = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        `Provision after` = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        Change = reactable::colDef(align = "right", cell = function(v) .an_money(v),
          style = function(v) list(color = if (v >= 0) .AN_COL$err else .AN_COL$ok,
                                   fontWeight = "600")),
        `Change %` = reactable::colDef(align = "right", cell = function(v) .an_pct(v))))
    })

    output$pick_rt2 <- renderUI({
      i <- inp_b() %||% inp_a(); ch <- an_rating_types(i)
      if (is.null(ch) || length(ch) == 0) return(NULL)
      selectInput(ns("rating_type2"), "Rating scale", choices = ch, width = "100%")
    })
    .rt2_levels <- reactive({
      i <- inp_b() %||% inp_a()
      an_rating_levels(i, input$rating_type2 %||% "1")
    })

    # ---- MEV forecast ------------------------------------------------------
    mev_base <- reactive({ mev_forecast_table(.out_dir_for(input$run_a)) })
    mev_wts  <- reactive({ mev_weights_table(.out_dir_for(input$run_a)) })

    observeEvent(input$mev_reset, {
      tb <- mev_base(); if (is.null(tb)) return()
      for (r in seq_len(nrow(tb)))
        updateNumericInput(session, paste0("mev_", tb$idx[r], "_", tb$year[r]),
                           value = tb$value[r])
      for (i in unique(tb$idx)) updateNumericInput(session, paste0("mev_shock_", i), value = 0)
    })

    output$mev_editor <- renderUI({
      tb <- mev_base()
      if (is.null(tb))
        return(.an_na("This run has no frozen config, so its MEV forecast cannot be read."))
      w <- mev_wts()
      yrs <- sort(unique(tb$year))
      blocks <- lapply(sort(unique(tb$idx)), function(i) {
        sub <- tb[tb$idx == i, , drop = FALSE]
        wt <- if (!is.null(w) && i %in% w$idx) w$weight[w$idx == i][1] else NA_real_
        lab <- sub$label[1]
        div(class = "qdb-card", style = "padding:12px;margin-bottom:8px",
          fluidRow(
            column(7, tags$strong(lab),
              if (!is.na(wt)) span(class = sprintf("qdb-pill %s",
                                    if (wt > 0) "qp-ok" else "qp-muted"),
                                   style = "margin-left:8px",
                                   sprintf("model weight %.2f", wt)) else NULL,
              if (!is.na(wt) && wt == 0)
                div(class = "small-muted", "Weighted zero in the model, so changes here will not move the provision.")
              else NULL),
            column(5, numericInput(ns(paste0("mev_shock_", i)),
                                   "Shift every year by", value = 0, step = 0.5,
                                   width = "100%"))),
          fluidRow(lapply(yrs, function(y) {
            v <- sub$value[sub$year == y]
            column(2, numericInput(ns(paste0("mev_", i, "_", y)),
                                   sprintf("Year %d", y),
                                   value = if (length(v)) v[1] else 0,
                                   step = 0.1, width = "100%"))
          })))
      })
      tagList(blocks)
    })

    output$mev_wcustom <- renderUI({
      w <- run_weights()
      sc <- names(w)
      if (length(sc) == 0) return(.an_na("Scenario weights could not be read for this run."))
      def <- function(s2) round(100 * w[[s2]], 2)
      tagList(
        fluidRow(lapply(sc, function(s2)
          column(2, numericInput(ns(paste0("mevw_", make.names(s2))), s2,
                                 value = def(s2), min = 0, max = 100, step = 0.5,
                                 width = "100%")))),
        div(class = "small-muted",
            "They start at the weights this run is priced with; left there, they reprice to the booked figure exactly. Normalised to 100% before use."))
    })

    mev_result <- eventReactive(input$mev_run, {
      i <- inp_a(); d <- dat_a(); tb <- mev_base()
      if (is.null(i) || isFALSE(i$ok) || is.null(d) || is.null(tb)) return(NULL)
      newtb <- tb
      newtb$value <- vapply(seq_len(nrow(tb)), function(r) {
        v <- suppressWarnings(as.numeric(input[[paste0("mev_", tb$idx[r], "_", tb$year[r])]]))
        if (length(v) != 1 || is.na(v)) tb$value[r] else v
      }, numeric(1))
      shock <- list()
      for (k in unique(tb$idx)) {
        v <- suppressWarnings(as.numeric(input[[paste0("mev_shock_", k)]]))
        if (length(v) == 1 && !is.na(v) && v != 0) shock[[as.character(k)]] <- v
      }
      wm <- input$mev_wmode %||% "auto"
      wts <- if (identical(wm, "custom")) mev_weights_in() else NULL
      if (identical(wm, "custom") && (is.null(wts) || sum(wts) <= 0))
        return(structure(list(), class = "stress_error",
                         msg = "Set at least one scenario weight above zero."))
      mev_stress(i, d, .out_dir_for(input$run_a), mev_new = newtb, shock = shock,
                 weight_mode = wm, weights = wts)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    output$mev_result <- renderUI({
      r <- mev_result()
      if (is.null(r))
        return(div(class = "alert alert-secondary", style = "margin-top:10px",
          icon("circle-info"), " Edit the forecast or set a shift, then rebuild."))
      if (stress_failed(r)) return(.an_na(stress_error_message(r)))
      if (is.null(r$before)) return(.an_na("The PD chain produced no result."))
      pc <- 100 * r$delta / max(r$before, 1)
      tagList(
        qdb_stats(list(
          list(k = "Provision now", v = .an_money(r$before), tone = ""),
          list(k = "On this macro path", v = .an_money(r$after), tone = "accent"),
          list(k = "Change", v = .an_money(r$delta),
               tone = if (r$delta > 0) "err" else if (r$delta < 0) "ok" else ""),
          list(k = "% change", v = sprintf("%+.2f%%", pc),
               tone = if (r$delta > 0) "err" else if (r$delta < 0) "ok" else ""),
          list(k = "Contracts priced", v = .an_money(r$priced), tone = ""))),
        # a run priced by an earlier version of the calculation: the change is
        # measured from the run rebuilt on this engine, not its own provision
        if (!is.null(r$note)) div(class = "alert alert-info", style = "margin:8px 0",
                                  icon("circle-info"), " ", r$note),
        if (!is.null(r$weights_used)) {
          wu <- r$weights_used; wb <- r$weights_base
          nm <- names(wu)
          same <- !is.null(wb) && length(wb) == length(wu) &&
                  isTRUE(all.equal(unname(as.numeric(wb[nm])), unname(as.numeric(wu)),
                                   tolerance = 1e-6))
          tagList(hr(),
            h5("Scenario weights used"),
            p(class = "small-muted", switch(r$weight_mode,
              auto = if (same) "Recomputed from the new path; unchanged from this run's weights."
                     else "Recomputed from the new path \u2014 years 1 and 2 of Non-Oil GDP drive them.",
              hold = "Held at this run's weights (what its config gives on this engine), so the change shown is the PD effect alone.",
              custom = "Set manually and normalised to 100%.")),
            qdb_reactable(data.frame(Scenario = nm,
              `This run` = if (is.null(wb)) rep("\u2014", length(nm))
                           else sprintf("%.2f%%", 100 * as.numeric(wb[nm])),
              `Used here` = sprintf("%.2f%%", 100 * as.numeric(wu)),
              check.names = FALSE), searchable = FALSE, page_size = 6))
        } else NULL,
        if (!is.null(r$movers)) {
          aff <- r$movers[r$movers$change != 0, , drop = FALSE]
          if (nrow(aff) == 0) NULL else tagList(hr(),
            h5(sprintf("Customers affected (%s)", .an_money(nrow(aff)))),
            .drill_customers(aff, page = 12))
        } else NULL,
        if (!is.null(r$by_portfolio)) tagList(hr(), h5("By portfolio"),
          qdb_reactable(data.frame(Portfolio = r$by_portfolio$portfolio,
            Contracts = r$by_portfolio$contracts,
            Before = .an_money(r$by_portfolio$before),
            After = .an_money(r$by_portfolio$after),
            Change = .an_money(r$by_portfolio$change), check.names = FALSE),
            searchable = FALSE, page_size = 8)) else NULL)
    })

    # ---- roll forward ------------------------------------------------------
    # The portfolios a sensitivity or the roll forward reaches: the internally
    # rated ones unless chosen otherwise.
    .scope_picker <- function(id) {
      d <- dat_a(); i <- inp_a(); if (is.null(d)) return(NULL)
      pf <- sort(unique(d$portfolio[nzchar(d$portfolio) & !is.na(d$portfolio)]))
      int <- internal_portfolios(i, d) %||% pf
      ext <- external_portfolios(i, d)
      tagList(
        selectInput(ns(id), "Scope", choices = pf, selected = int,
                    multiple = TRUE, width = "100%"),
        if (length(ext) > 0) div(class = "small-muted",
          sprintf("%s excluded by default \u2014 externally rated, so they do not price against the internal PD curves.",
                  paste(ext, collapse = ", "))) else NULL)
    }
    output$rf_scope <- renderUI(.scope_picker("rf_portfolios"))
    output$rv_scope <- renderUI(.scope_picker("rv_portfolios"))
    output$sw_scope <- renderUI(.scope_picker("sw_portfolios"))

    rf_result <- eventReactive(input$rf_run, {
      i <- inp_a(); d <- dat_a()
      if (is.null(i) || isFALSE(i$ok) || is.null(d)) return(NULL)
      k <- suppressWarnings(as.numeric(input$rf_months))
      if (length(k) != 1 || is.na(k) || k <= 0) k <- 12
      r <- roll_forward(i, d, .out_dir_for(input$run_a), months = k,
                        portfolios = input$rf_portfolios)
      if (is.null(r)) { r <- list(); attr(r, "failed") <- TRUE }
      r
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    output$rf_result <- renderUI({
      i <- inp_a()
      if (is.null(i) || isFALSE(i$ok))
        return(.an_na("The run's engine inputs could not be read."))
      r <- rf_result()
      if (is.null(r))
        return(div(class = "alert alert-secondary", style = "margin-top:10px",
          icon("circle-info"), " Choose a horizon and click Roll forward."))
      if (stress_failed(r)) return(.an_na(stress_error_message(r)))
      if (isTRUE(attr(r, "failed")) || is.null(r$before))
        return(.an_na(stress_diagnosis(inp_a(), dat_a(), .out_dir_for(input$run_a))))
      pc <- 100 * r$delta / max(r$before, 1)
      tagList(
        qdb_stats(list(
          list(k = "Provision today", v = .an_money(r$before), tone = ""),
          list(k = sprintf("In %d months", r$months), v = .an_money(r$after), tone = "accent"),
          list(k = "Change", v = .an_money(r$delta),
               tone = if (r$delta > 0) "err" else "ok"),
          list(k = "% change", v = sprintf("%+.2f%%", pc),
               tone = if (r$delta > 0) "err" else "ok"),
          list(k = "Contracts matured", v = .an_money(r$matured), tone = "warn"),
          list(k = "Exposure run off", v = .an_money(r$matured_exposure), tone = ""))),
        p(class = "small-muted",
          sprintf("Exposure falls from %s to %s as the book amortises.",
                  .an_money(r$exposure_before), .an_money(r$exposure_after))),
        if (!is.null(r$stages)) tagList(hr(), h5("By stage"),
          qdb_reactable(data.frame(Stage = r$stages$stage, Contracts = r$stages$contracts,
            Today = .an_money(r$stages$before),
            Later = .an_money(r$stages$after),
            Change = .an_money(r$stages$change), check.names = FALSE),
            searchable = FALSE, page_size = 5)) else NULL,
        if (!is.null(r$movers)) {
          aff <- r$movers[r$movers$change != 0, , drop = FALSE]
          if (nrow(aff) == 0) NULL else tagList(hr(),
            h5(sprintf("Customers affected (%s)", .an_money(nrow(aff)))),
            .drill_customers(aff, page = 12))
        } else NULL,
        if (!is.null(r$by_portfolio)) tagList(hr(), h5("By portfolio"),
          qdb_reactable(data.frame(Portfolio = r$by_portfolio$portfolio,
            Contracts = r$by_portfolio$contracts, Matured = r$by_portfolio$matured,
            `Exposure today` = .an_money(r$by_portfolio$exposure_before),
            `Exposure later` = .an_money(r$by_portfolio$exposure_after),
            Today = .an_money(r$by_portfolio$before),
            Later = .an_money(r$by_portfolio$after),
            Change = .an_money(r$by_portfolio$change), check.names = FALSE),
            searchable = FALSE, page_size = 8)) else NULL)
    })

    output$tn_levers <- renderUI({
      d <- dat_a()
      ext <- external_portfolios(inp_a(), d)
      L <- tornado_levers()
      keymap <- .TN_KEYS
      rows <- lapply(seq_along(L), function(k) {
        x <- L[[k]]
        fid <- ns(paste0("tn_val_", k))
        ctl <- if (is.logical(x$value))
          checkboxInput(fid, unname(keymap[x$key] %||% x$key), value = x$value)
        else
          numericInput(fid, unname(keymap[x$key] %||% x$key), value = x$value,
                       step = if (abs(x$value) >= 10) 5 else 0.05, width = "100%")
        column(3, div(style = "margin-bottom:4px",
          ctl,
          checkboxInput(ns(paste0("tn_on_", k)), "include", value = TRUE)))
      })
      tagList(
        if (length(ext) > 0) div(class = "small-muted", style = "margin-bottom:6px",
          sprintf("Scoped to the internally-rated portfolios. %s are externally rated and do not price against the internal PD curves.",
                  paste(ext, collapse = ", "))) else NULL,
        p(class = "small-muted", "Each move is applied on its own, from the same base:"),
        div(class = "qdb-card", style = "padding:12px",
            do.call(fluidRow, rows)))
    })

    # ---- each lever ----------------------------------------------------------
    tn_result <- eventReactive(input$tn_run, {
      i <- inp_a(); d <- dat_a()
      if (is.null(i) || isFALSE(i$ok) || is.null(d)) return(NULL)
      L <- tornado_levers()
      keep <- vapply(seq_along(L), function(k) {
        v <- input[[paste0("tn_on_", k)]]; is.null(v) || isTRUE(v)
      }, logical(1))
      L <- lapply(seq_along(L), function(k) {
        x <- L[[k]]
        v <- input[[paste0("tn_val_", k)]]
        if (!is.null(v) && length(v) == 1 && !is.na(v)) {
          nv <- if (is.logical(x$value)) isTRUE(v) else as.numeric(v)
          # a move left as it is keeps its own name ("Largest 5 default");
          # an edited one says what it was set to
          if (!isTRUE(all.equal(nv, x$value))) {
            x$value <- nv
            x$label <- sprintf("%s = %s", unname(.TN_KEYS[x$key] %||% x$key),
                               if (is.logical(nv)) (if (nv) "on" else "off")
                               else format(nv))
          }
        }
        x
      })[keep]
      base <- stress_default("tornado")
      base$portfolios <- internal_portfolios(i, d)
      r <- stress_tornado(i, d, .out_dir_for(input$run_a), base_spec = base, levers = L)
      if (is.null(r)) { r <- data.frame(); attr(r, "failed") <- TRUE }
      r
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    output$tn_result <- renderUI({
      i <- inp_a()
      if (is.null(i) || isFALSE(i$ok))
        return(.an_na("The run's engine inputs could not be read."))
      t <- tn_result()
      if (is.null(t))
        return(div(class = "alert alert-secondary", style = "margin-top:10px",
          icon("circle-info"), " Set the moves you want, then Run. It reprices once per lever, so allow a few seconds."))
      if (stress_failed(t)) return(.an_na(stress_error_message(t)))
      if (isTRUE(attr(t, "failed")) || nrow(t) == 0)
        return(.an_na(stress_diagnosis(inp_a(), dat_a(), .out_dir_for(input$run_a))))
      base <- attr(t, "base")
      chart <- if (.an_echarts()) {
        x <- t[order(t$change), , drop = FALSE]
        cj <- htmlwidgets::JS(sprintf("function(p){var c=[%s]; return c[p.dataIndex];}",
          paste0("'", ifelse(x$change >= 0, .AN_COL$err, .AN_COL$ok), "'", collapse = ",")))
        echarts4r::e_charts(x, lever) |>
          echarts4r::e_bar(change, legend = FALSE, bar_width = "60%",
                           itemStyle = list(color = cj)) |>
          echarts4r::e_flip_coords() |>
          echarts4r::e_x_axis(axisLabel = list(formatter = .an_m_axis())) |>
          echarts4r::e_tooltip() |>
          echarts4r::e_grid(left = 190, right = 30, top = 10, bottom = 30)
      } else NULL
      tagList(
        qdb_stats(list(
          list(k = "Provision at base", v = .an_money(base), tone = ""),
          list(k = "Most sensitive to", v = t$lever[1], tone = "warn"),
          list(k = "That lever's effect", v = .an_money(t$change[1]),
               tone = if (t$change[1] >= 0) "err" else "ok"))),
        chart,
        qdb_reactable(data.frame(Lever = t$lever,
          Provision = .an_money(t$provision), Change = .an_money(t$change),
          `% change` = sprintf("%+.2f%%", t$pct),
          `Contracts re-staged` = t$moved, check.names = FALSE),
          searchable = FALSE, page_size = 14))
    })

    # ---- reverse stress ----------------------------------------------------
    rv_result <- eventReactive(input$rv_run, {
      i <- inp_a(); d <- dat_a()
      if (is.null(i) || isFALSE(i$ok) || is.null(d)) return(NULL)
      tgt <- suppressWarnings(as.numeric(input$rv_target))
      if (length(tgt) != 1 || is.na(tgt) || tgt <= 0) tgt <- 25
      sp <- stress_default("reverse")
      sp$portfolios <- input$rv_portfolios %||% internal_portfolios(i, d)
      r <- reverse_stress_all(i, d, .out_dir_for(input$run_a), tgt, base_spec = sp)
      if (is.null(r)) { r <- data.frame(); attr(r, "failed") <- TRUE }
      r
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    output$rv_result <- renderUI({
      i <- inp_a()
      if (is.null(i) || isFALSE(i$ok))
        return(.an_na("The run's engine inputs could not be read."))
      r <- rv_result()
      if (is.null(r))
        return(div(class = "alert alert-secondary", style = "margin-top:10px",
          icon("circle-info"), " Set a target and click Solve. Each lever is solved separately, so this takes a few seconds."))
      if (isTRUE(attr(r, "failed")) || nrow(r) == 0)
        return(.an_na(stress_diagnosis(inp_a(), dat_a(), .out_dir_for(input$run_a))))
      tgt <- suppressWarnings(as.numeric(input$rv_target))
      reach <- r[r$found, , drop = FALSE]
      lead <- if (nrow(reach) > 0)
        div(class = "alert alert-info", style = "margin-top:10px",
            icon("circle-info"),
            sprintf(" The provision is most exposed to %s \u2014 that is the smallest move needed to reach +%.0f%%.",
                    reach$lever[1], tgt))
      else div(class = "alert alert-warning", style = "margin-top:10px",
            icon("triangle-exclamation"),
            sprintf(" No single lever reaches +%.0f%% on its own. A combination would be needed \u2014 build it on Stress test.", tgt))
      tagList(lead,
        qdb_reactable(data.frame(
          Lever = r$lever,
          `Required level` = r$required,
          `Reaches target` = ifelse(r$found, "yes", "no"),
          `Provision increase` = sprintf("%+.1f%%", r$achieved_pct),
          `Provision` = .an_money(r$provision),
          check.names = FALSE), searchable = FALSE, page_size = 10))
    })

    # ---- stress test -------------------------------------------------------
    # One editor for every stress: a package, a what-if on named customers
    # and a staging-policy change are all a scope and the levers moved for it,
    # priced by stress_apply(). Each package carries a uid, so its inputs keep
    # their values when another is added or removed. A threshold or LGD
    # parameter left at the run's own is stored as NULL (the run's own), so a
    # saved package follows whichever run it is applied to.
    stp_list <- reactiveVal(NULL)
    stp_uid <- reactiveVal(0L)
    .stp_new <- function(sp) {
      u <- isolate(stp_uid()) + 1L; stp_uid(u)
      d <- stress_default(sp$name %||% "Stress")
      for (k in names(sp)) d[k] <- list(sp[[k]])
      d$.uid <- u
      d
    }
    .stp_clean <- function(sp) { sp$.uid <- NULL; sp }
    .stp_ids <- function(x) {
      x <- trimws(unlist(strsplit(as.character(unlist(x %||% character(0))), "[,;]")))
      x[nzchar(x)]
    }
    stp_run_thr <- reactive(staging_threshold(.out_dir_for(input$run_a)))
    .scen_names <- function(out_dir) {
      if (is.null(out_dir)) return(character(0))
      f <- list.files(out_dir, pattern = "^FinalEclReport_scenario_.*\\.csv$")
      sort(gsub("_", " ", sub("\\.csv$", "", sub("^FinalEclReport_scenario_", "", f))))
    }

    # one package's fields back off the screen
    .stp_read <- function(cur) {
      u <- cur$.uid
      key <- function(nm) sprintf("stp_%s_%d", nm, u)
      f <- function(nm, default) { v <- input[[key(nm)]]; if (is.null(v)) default else v }
      # A cleared multi-select sends NULL. Once the package's inputs are in the
      # browser (its name box always sends a value) that is an empty choice;
      # before then it is not drawn yet, and the package's own value stands.
      multi <- function(nm, default) {
        v <- input[[key(nm)]]
        if (!is.null(v)) v else if (!is.null(input[[key("name")]])) character(0) else default
      }
      num <- function(nm, default) {
        v <- suppressWarnings(as.numeric(f(nm, default)))
        if (length(v) != 1 || is.na(v)) default else v
      }
      run_thr <- stp_run_thr(); ctx <- inp_a()$ctx
      thr <- num("thr", cur$dpd_threshold %||% run_thr)
      lgdb <- num("lgdb", cur$lgd_base %||% ctx$lgd_base)
      lgdf <- num("lgdf", cur$lgd_floor %||% ctx$lgd_floor)
      to <- as.character(f("to", if (is.null(cur$stage_to)) "" else cur$stage_to))
      own <- function(v, run) if (is.null(v) || isTRUE(all.equal(v, run))) NULL else v
      list(name = { x <- trimws(f("name", cur$name)); if (nzchar(x)) x else cur$name },
           portfolios = as.character(unlist(multi("pf", cur$portfolios))),
           stages = as.integer(unlist(multi("st", cur$stages))),
           customers = .stp_ids(f("cust", paste(.stp_ids(cur$customers), collapse = ", "))),
           dpd_threshold = own(thr, run_thr),
           contagion = isTRUE(f("contagion", cur$contagion)),
           tasdeer_collective = isTRUE(f("tasdeer", cur$tasdeer_collective)),
           watchlist_triggers = isTRUE(f("watch", cur$watchlist_triggers)),
           local_triggers = isTRUE(f("local", cur$local_triggers)),
           stage_to = if (!nzchar(to) || to == "0") NULL else as.integer(to),
           default_top_n = as.integer(num("topn", cur$default_top_n)),
           pd_multiplier = num("pd", cur$pd_multiplier),
           pd_scenario = as.character(f("scen", cur$pd_scenario %||% "")),
           rating_notches = as.integer(num("notch", cur$rating_notches)),
           lgd_base = own(lgdb, ctx$lgd_base),
           lgd_floor = own(lgdf, ctx$lgd_floor),
           collateral_pct = num("coll", cur$collateral_pct),
           exposure_pct = num("exp", cur$exposure_pct),
           maturity_years = num("mat", cur$maturity_years),
           .uid = u)
    }
    .stp_live <- function() lapply(stp_list() %||% list(), .stp_read)

    observeEvent(input$stp_add, {
      l <- .stp_live()
      stp_list(c(l, list(.stp_new(list(
        name = sprintf("Stress %d", length(l) + 1),
        portfolios = internal_portfolios(inp_a(), dat_a()) %||% character(0))))))
    })
    observeEvent(input$stp_std, {
      std <- standard_packages(internal_portfolios(inp_a(), dat_a()) %||% character(0))
      stp_list(c(.stp_live(), lapply(std, .stp_new)))
    })
    observeEvent(input$stp_remove_click, {
      u <- suppressWarnings(as.integer(input$stp_remove_click)); l <- .stp_live()
      if (length(l) > 1) stp_list(Filter(function(x) !identical(x$.uid, u), l))
    })
    observeEvent(input$stp_load, {
      saved <- tryCatch(stress_read(), error = function(e) list())
      if (length(saved) == 0) {
        showNotification("No saved stresses found.", type = "warning")
      } else {
        stp_list(lapply(saved, .stp_new))
        showNotification(sprintf("Loaded %d package(s).", length(saved)), type = "message")
      }
    })
    observeEvent(input$stp_save, {
      live <- .stp_live(); stp_list(live)
      ok <- tryCatch({ stress_write(lapply(live, .stp_clean)); TRUE },
                     error = function(e) FALSE)
      showNotification(if (ok) sprintf("Saved %d package(s).", length(live))
                       else "Could not write config/stress_packages.yml.",
                       type = if (ok) "message" else "error")
    })

    output$stp_note <- renderUI({
      ext <- external_portfolios(inp_a(), dat_a())
      if (length(ext) == 0) return(NULL)
      div(class = "small-muted", style = "margin:2px 0 8px",
          sprintf("A new package is scoped to the internally rated portfolios. %s are externally rated: their PD curves are the agency scale's, so a rating or PD lever does not reach them.",
                  paste(ext, collapse = ", ")))
    })

    output$stp_editor <- renderUI({
      d <- dat_a(); i <- inp_a()
      if (is.null(i) || isFALSE(i$ok) || is.null(d))
        return(.an_na("The run's engine inputs could not be read, so the book cannot be repriced."))
      l <- stp_list()
      if (is.null(l)) {
        # first visit: one package on the internally rated book
        l <- list(.stp_new(list(name = "Stress 1",
                                portfolios = internal_portfolios(i, d) %||% character(0))))
        stp_list(l)
      }
      pf <- sort(unique(d$portfolio[nzchar(d$portfolio) & !is.na(d$portfolio)]))
      scen <- .scen_names(.out_dir_for(input$run_a))
      run_thr <- stp_run_thr()
      tagList(lapply(l, function(sp) {
        cur <- isolate(.stp_read(sp))         # keep whatever is on screen now
        u <- cur$.uid
        fid <- function(nm) ns(sprintf("stp_%s_%d", nm, u))
        lab <- function(x, sub = NULL) div(style = "margin:8px 0 2px", tags$strong(x),
          if (!is.null(sub)) span(class = "small-muted", paste0(" — ", sub)) else NULL)
        div(class = "qdb-card", style = "padding:12px;margin-bottom:8px",
          fluidRow(
            column(10, textInput(fid("name"), NULL, value = cur$name,
                                 placeholder = "Package name", width = "100%")),
            column(2, div(style = "text-align:right",
              tags$button(type = "button", class = "btn btn-sm btn-outline-danger",
                onclick = sprintf("Shiny.setInputValue('%s', %d, {priority:'event'})",
                                  ns("stp_remove_click"), u),
                "Remove")))),
          lab("Who", "every contract in these portfolios, stages and customers; leave one empty not to narrow it"),
          fluidRow(
            column(5, selectInput(fid("pf"), "Portfolios", choices = pf,
                                  selected = intersect(cur$portfolios, pf),
                                  multiple = TRUE, width = "100%")),
            column(2, selectInput(fid("st"), "Stages", choices = c(1, 2, 3),
                                  selected = cur$stages, multiple = TRUE, width = "100%")),
            column(5, textInput(fid("cust"), "Customer ids",
                                value = paste(cur$customers, collapse = ", "),
                                placeholder = "comma separated; a name with spaces is one id",
                                width = "100%"))),
          uiOutput(ns(sprintf("stp_info_%d", u))),
          lab("Staging"),
          fluidRow(
            column(3, numericInput(fid("thr"), "Stage 2 DPD threshold",
                                   value = cur$dpd_threshold %||% run_thr,
                                   min = 0, max = 365, step = 5, width = "100%")),
            column(3, selectInput(fid("to"), "Move to stage",
                                  choices = c("Unchanged" = "", "1" = "1", "2" = "2", "3" = "3"),
                                  selected = as.character(cur$stage_to %||% ""),
                                  width = "100%")),
            column(3, numericInput(fid("topn"), "Largest N default",
                                   value = cur$default_top_n, min = 0, max = 100, step = 1,
                                   width = "100%"))),
          div(style = "display:flex;gap:18px;flex-wrap:wrap",
            checkboxInput(fid("contagion"), "Contagion", cur$contagion),
            checkboxInput(fid("tasdeer"), "Tasdeer collective", cur$tasdeer_collective),
            checkboxInput(fid("watch"), "Watchlist triggers", cur$watchlist_triggers),
            checkboxInput(fid("local"), "Local flags trigger", cur$local_triggers)),
          lab("PD and rating"),
          fluidRow(
            column(3, numericInput(fid("pd"), "PD multiplier", value = cur$pd_multiplier,
                                   min = 0, max = 10, step = 0.05, width = "100%")),
            column(4, selectInput(fid("scen"), "PD scenario",
                                  choices = c("Weighted, as the run" = "", scen),
                                  selected = cur$pd_scenario, width = "100%")),
            column(3, numericInput(fid("notch"), "Rating notches", value = cur$rating_notches,
                                   min = -10, max = 20, step = 1, width = "100%"))),
          lab("LGD, collateral, exposure and term"),
          fluidRow(
            column(2, numericInput(fid("lgdb"), "LGD base",
                                   value = cur$lgd_base %||% i$ctx$lgd_base,
                                   min = 0, max = 1, step = 0.01, width = "100%")),
            column(2, numericInput(fid("lgdf"), "LGD floor",
                                   value = cur$lgd_floor %||% i$ctx$lgd_floor,
                                   min = 0, max = 1, step = 0.05, width = "100%")),
            column(2, numericInput(fid("coll"), "Collateral %", value = cur$collateral_pct,
                                   min = 0, max = 300, step = 5, width = "100%")),
            column(2, numericInput(fid("exp"), "Exposure %", value = cur$exposure_pct,
                                   min = 0, max = 300, step = 5, width = "100%")),
            column(3, numericInput(fid("mat"), "Extend maturity (years)",
                                   value = cur$maturity_years, min = -10, max = 30,
                                   step = 0.25, width = "100%"))))
      }))
    })

    # The customers named, as they stand, while the ids are typed: read the
    # way the stress will read them, so a mistyped id shows before anything
    # is priced. One output per package, each on its own id field.
    observe({
      for (sp in stp_list() %||% list()) local({
        u <- sp$.uid
        output[[sprintf("stp_info_%d", u)]] <- renderUI({
          d <- dat_a()
          ids <- input[[sprintf("stp_cust_%d", u)]]
          if (is.null(d) || is.null(ids) || !nzchar(trimws(ids))) return(NULL)
          lk <- customer_lookup(d, ids, spaces = FALSE)
          if (is.null(lk)) return(NULL)
          tagList(
            if (!is.null(lk$found)) qdb_reactable(data.frame(
                Customer = lk$found$customer, Portfolios = lk$found$portfolios,
                Rating = lk$found$rating, Stage = lk$found$stage,
                Contracts = lk$found$contracts,
                Exposure = .an_money(lk$found$exposure),
                ECL = .an_money(lk$found$ecl),
                `ECL coverage %` = .an_pct0(lk$found$coverage),
                check.names = FALSE), searchable = FALSE, page_size = 5) else NULL,
            if (length(lk$missing) > 0)
              div(class = "small-muted", style = "color:#dc2626",
                  sprintf("Not in this run: %s", paste(lk$missing, collapse = ", ")))
            else NULL)
        })
      })
    })

    stp_result <- eventReactive(input$stp_run, {
      i <- inp_a(); d <- dat_a()
      if (is.null(i) || isFALSE(i$ok) || is.null(d)) return(NULL)
      live <- .stp_live(); stp_list(live)
      od <- .out_dir_for(input$run_a)
      detail <- withProgress(message = "Repricing the book…", value = 0.3,
        lapply(live, function(sp) stress_apply(i, d, od, .stp_clean(sp))))
      list(detail = detail, specs = live)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # One stress's result: the tiles, then by portfolio, the stage moves and
    # one customer table.
    .stp_detail <- function(x) {
      tone <- if (x$delta > 0) "err" else if (x$delta < 0) "ok" else ""
      tagList(
        qdb_stats(list(
          list(k = "Provision now", v = .an_money(x$before), tone = ""),
          list(k = "Under this stress", v = .an_money(x$after), tone = "accent"),
          list(k = "Change", v = .an_signed(x$delta), tone = tone),
          list(k = "% change", v = sprintf("%+.2f%%", 100 * x$delta / max(x$before, 1)),
               tone = tone),
          list(k = "Contracts in scope", v = .an_money(x$scoped), tone = ""),
          list(k = "Contracts re-staged", v = .an_money(x$moved), tone = "warn"))),
        if (length(x$notes)) p(class = "small-muted", paste0(paste(x$notes, collapse = "; "), ".")) else NULL,
        if (abs(x$delta) < 1e-6)
          div(class = "alert alert-secondary", icon("circle-info"),
              " Nothing in this stress changes the provision.") else NULL,
        if (!is.null(x$by_portfolio)) tagList(h6(style = "margin-top:10px", "By portfolio"),
          qdb_reactable(data.frame(
            Portfolio = x$by_portfolio$portfolio, Moved = x$by_portfolio$moved,
            `ECL before` = .an_money(x$by_portfolio$before),
            `ECL after` = .an_money(x$by_portfolio$after),
            Change = .an_signed(x$by_portfolio$change), check.names = FALSE),
            searchable = FALSE, page_size = 8)) else NULL,
        # Stage moves stay as a summary: an aggregate BY MOVE, which the
        # customer table below does not give.
        if (!is.null(x$migration)) tagList(h6("Stage moves"),
          qdb_reactable(data.frame(Move = x$migration$move,
            Contracts = x$migration$contracts, Customers = x$migration$customers,
            Exposure = .an_money(x$migration$exposure),
            `ECL change` = .an_money(x$migration$ecl_change), check.names = FALSE),
            searchable = FALSE, page_size = 6)) else NULL,
        # ONE customer table: the N largest when "largest N default" was used,
        # with their outcome; otherwise whoever the stress moved.
        if (!is.null(x$defaulted_rows)) tagList(
          h6(sprintf("The %s largest customers selected", .an_money(nrow(x$defaulted_rows)))),
          .drill_customers(x$defaulted_rows, page = 12))
        else if (!is.null(x$movers)) {
          aff <- x$movers[x$movers$change != 0, , drop = FALSE]
          if (nrow(aff) == 0) NULL else tagList(
            h6(sprintf("Customers affected (%s)", .an_money(nrow(aff)))),
            .drill_customers(aff, page = 12))
        } else NULL)
    }

    output$stp_result <- renderUI({
      i <- inp_a()
      if (is.null(i) || isFALSE(i$ok)) return(NULL)
      r <- stp_result()
      if (is.null(r))
        return(div(class = "alert alert-secondary", style = "margin-top:10px",
          icon("circle-info"), " Set up one or more stresses, then Run."))
      ok <- !vapply(r$detail, function(x) is.null(x) || stress_failed(x), logical(1))
      fails <- lapply(which(!ok), function(k) {
        x <- r$detail[[k]]
        div(class = "alert alert-warning", style = "margin-top:8px",
            icon("triangle-exclamation"), sprintf(" %s: %s", r$specs[[k]]$name,
            if (is.null(x)) stress_diagnosis(inp_a(), dat_a(), .out_dir_for(input$run_a))
            else stress_error_message(x)))
      })
      det <- r$detail[ok]; sps <- r$specs[ok]
      if (length(det) == 0) return(tagList(fails))
      if (length(det) == 1) return(tagList(fails, .stp_detail(det[[1]])))
      cmp <- data.frame(
        name = vapply(sps, function(s) s$name %||% "(unnamed)", character(1)),
        before = vapply(det, `[[`, numeric(1), "before"),
        after = vapply(det, `[[`, numeric(1), "after"),
        delta = vapply(det, `[[`, numeric(1), "delta"),
        scoped = vapply(det, function(x) as.numeric(x$scoped), numeric(1)),
        moved = vapply(det, function(x) as.numeric(x$moved), numeric(1)),
        stringsAsFactors = FALSE)
      cmp$pct <- 100 * cmp$delta / pmax(cmp$before, 1)
      ord <- order(-cmp$delta)
      chart <- if (.an_echarts()) {
        x <- cmp[order(cmp$delta), , drop = FALSE]
        cj <- htmlwidgets::JS(sprintf("function(p){var c=[%s]; return c[p.dataIndex];}",
          paste0("'", ifelse(x$delta >= 0, .AN_COL$err, .AN_COL$ok), "'", collapse = ",")))
        echarts4r::e_charts(x, name) |>
          echarts4r::e_bar(delta, legend = FALSE, bar_width = "55%",
                           itemStyle = list(color = cj)) |>
          echarts4r::e_flip_coords() |>
          echarts4r::e_x_axis(axisLabel = list(formatter = .an_m_axis())) |>
          echarts4r::e_tooltip() |>
          echarts4r::e_grid(left = 190, right = 30, top = 10, bottom = 30)
      } else NULL
      tagList(fails,
        h5("Stresses compared"), chart,
        p(class = "small-muted",
          "Each priced on its own from the same run, so the changes are comparable; they do not add up."),
        qdb_reactable(data.frame(Package = cmp$name[ord],
          `Provision before` = .an_money(cmp$before[ord]),
          `Provision after` = .an_money(cmp$after[ord]),
          Change = .an_signed(cmp$delta[ord]),
          `% change` = sprintf("%+.2f%%", cmp$pct[ord]),
          `In scope` = .an_money(cmp$scoped[ord]),
          `Re-staged` = .an_money(cmp$moved[ord]), check.names = FALSE),
          searchable = FALSE, page_size = 12),
        lapply(ord, function(k) tagList(hr(), h5(cmp$name[k]), .stp_detail(det[[k]]))))
    })

    # ---- staging threshold sweep -------------------------------------------
    sw_result <- eventReactive(input$sw_run, {
      i <- inp_a(); d <- dat_a()
      if (is.null(i) || isFALSE(i$ok) || is.null(d)) return(NULL)
      th <- suppressWarnings(as.numeric(input$sw_days)); th <- th[!is.na(th)]
      if (length(th) == 0) return(NULL)
      sp <- stress_default("sweep")
      sp$portfolios <- input$sw_portfolios %||% internal_portfolios(i, d)
      r <- withProgress(message = "Repricing at each threshold…", value = 0.3,
        staging_threshold_sweep(i, d, .out_dir_for(input$run_a), thresholds = th,
                                base_spec = sp))
      if (is.null(r)) { r <- data.frame(); attr(r, "failed") <- TRUE }
      r
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    output$sw_result <- renderUI({
      i <- inp_a()
      if (is.null(i) || isFALSE(i$ok))
        return(.an_na("The run's engine inputs could not be read."))
      sw <- sw_result()
      if (is.null(sw))
        return(div(class = "alert alert-secondary", style = "margin-top:10px",
          icon("circle-info"), " Choose the thresholds and click Reprice. Each is a full repricing, so several take a few seconds."))
      if (isTRUE(attr(sw, "failed")) || nrow(sw) == 0)
        return(.an_na(stress_diagnosis(inp_a(), dat_a(), .out_dir_for(input$run_a))))
      ref <- attr(sw, "reference")
      base <- sw$ecl[sw$threshold == ref]
      base <- if (length(base) == 1) base else NA_real_
      wide <- max(sw$ecl) - base
      vs <- sw[[sprintf("vs_%g", ref)]]
      tbl <- qdb_reactable(data.frame(
        `DPD threshold` = sw$threshold, Provision = .an_money(sw$ecl),
        `Re-staged` = .an_money(sw$moved), `Customers re-staged` = .an_money(sw$customers_moved),
        `vs the run's policy` = if (is.null(vs)) "—" else .an_signed(vs),
        check.names = FALSE), searchable = FALSE, page_size = 10)
      chart <- if (.an_echarts()) {
        x <- sw; x$threshold <- as.character(x$threshold)
        echarts4r::e_charts(x, threshold) |>
          echarts4r::e_line(ecl, legend = FALSE, symbolSize = 9,
                            lineStyle = list(width = 3),
                            itemStyle = list(color = .AN_COL$plum)) |>
          echarts4r::e_x_axis(name = "DPD threshold", nameLocation = "middle", nameGap = 28) |>
          echarts4r::e_y_axis(scale = TRUE, axisLabel = list(formatter = .an_m_axis())) |>
          echarts4r::e_tooltip(trigger = "axis") |>
          echarts4r::e_grid(left = 80, right = 25, top = 15, bottom = 55)
      } else NULL
      tagList(
        qdb_stats(list(
          list(k = "Policy in force", v = sprintf("%g days", ref), tone = ""),
          list(k = "Provision at that policy", v = .an_money(base), tone = "accent"),
          list(k = "Widest alternative", v = .an_signed(wide),
               tone = if (isTRUE(wide > 0)) "err" else ""),
          list(k = "As a share", v = .an_pct(100 * wide / max(base, 1)), tone = ""))),
        chart, tbl)
    })

    # ---- drill-down --------------------------------------------------------
    # One renderer for every "which contracts sit behind this number" table, so
    # the drill-downs look the same wherever they appear. Collapsed into an
    # expandable row, so nothing is added to the page until it is asked for.
    .drill_customers <- function(df, page = 8) {
      if (is.null(df) || nrow(df) == 0)
        return(div(class = "small-muted", style = "padding:8px 12px", "No contracts."))
      arrow <- function(a, b) {
        a <- as.character(a); b <- as.character(b)
        ifelse(is.na(a) & is.na(b), "\u2014",
        ifelse(is.na(b) | a == b, a, paste(a, "\u2192", b)))
      }
      out <- data.frame(
        Customer = df$customer,
        Facilities = df$facilities %||% rep(1L, nrow(df)),
        Portfolio = df$portfolio,
        Rating = arrow(df$rating, df$rating_after),
        Stage = arrow(df$stage, df$stage_after),
        Exposure = df$exposure,
        `ECL before` = df$ecl_before, `ECL after` = df$ecl_after,
        `Coverage before` = df$coverage_before, `Coverage after` = df$coverage_after,
        Change = df$change, check.names = FALSE)
      if (!is.null(df$outcome)) out$Outcome <- df$outcome
      reactable::reactable(out, class = "qdb-rt", compact = TRUE,
        defaultPageSize = page, searchable = nrow(out) > page,
        columns = list(
          Exposure = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
          `ECL before` = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
          `ECL after` = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
          `Coverage before` = reactable::colDef(align = "right",
            cell = function(v) .an_pct0(v)),
          `Coverage after` = reactable::colDef(align = "right",
            cell = function(v) .an_pct0(v)),
          Change = reactable::colDef(align = "right", cell = function(v) .an_money(v),
            style = function(v) list(color = if (isTRUE(v >= 0)) .AN_COL$err else .AN_COL$ok,
                                     fontWeight = "600"))))
    }

    # Generic contract-level drill (data-quality findings).
    .drill_contracts <- function(df, page = 8) {
      if (is.null(df) || nrow(df) == 0)
        return(div(class = "small-muted", style = "padding:8px 12px", "No contracts."))
      money <- intersect(c("amount", "exposure", "exposure_before", "exposure_after",
                           "ecl", "ecl_before", "ecl_after"), names(df))
      nice <- c(contract = "Contract", customer = "Customer", portfolio = "Portfolio",
                rating = "Rating", stage = "Stage", amount = "Amount",
                exposure = "Exposure", exposure_before = "Exposure before",
                exposure_after = "Exposure after", ecl = "ECL",
                ecl_before = "ECL before", ecl_after = "ECL after",
                stage_before = "Stage before", stage_after = "Stage after")
      out <- df
      names(out) <- ifelse(names(out) %in% names(nice), nice[names(out)], names(out))
      cols <- stats::setNames(lapply(names(out), function(nm) {
        if (tolower(gsub(" ", "_", nm)) %in% money ||
            nm %in% c("Amount", "Exposure", "ECL", "ECL before", "ECL after",
                      "Exposure before", "Exposure after"))
          reactable::colDef(align = "right", cell = function(v) .an_money(v))
        else reactable::colDef()
      }), names(out))
      reactable::reactable(out, class = "qdb-rt", compact = TRUE,
                           defaultPageSize = page, searchable = nrow(out) > page,
                           columns = cols)
    }

    # ---- migration ----------------------------------------------------------
    output$t_stagemove <- reactable::renderReactable({
      a <- dat_a(); b <- dat_b(); if (is.null(a) || is.null(b)) return(NULL)
      # on the rating scale chosen, as the heatmap beside it
      ia <- inp_b() %||% inp_a(); rt <- input$rating_type2 %||% "1"
      cm <- customer_stage_migration(an_filter_rating_type(a, ia, rt),
                                     an_filter_rating_type(b, ia, rt))
      if (is.null(cm)) return(NULL)
      cm <- cm[cm$from != cm$to, , drop = FALSE]
      if (nrow(cm) == 0) return(reactable::reactable(
        data.frame(Result = "No customer changed stage between these runs."),
        sortable = FALSE, class = "qdb-rt"))
      lbl <- sprintf("Stage %s \u2192 Stage %s", cm$from, cm$to)
      dir <- ifelse(cm$to > cm$from, "Deterioration", "Improvement")
      df <- data.frame(Movement = lbl, Direction = dir, Customers = cm$customers,
                       `Exposure now` = cm$exposure, `ECL now` = cm$ecl,
                       check.names = FALSE)
      df <- df[order(-df$Customers), , drop = FALSE]
      .rt(df, defaultPageSize = 10, columns = list(
        Direction = reactable::colDef(cell = function(v)
          htmltools::tags$span(class = paste("qdb-pill",
            if (identical(v, "Deterioration")) "qp-err" else "qp-ok"), v), minWidth = 120),
        `Exposure now` = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        `ECL now` = reactable::colDef(align = "right", cell = function(v) .an_money(v))))
    })

    # ---- staging -----------------------------------------------------------
    # Read on the run's own staging policy: the Stage 2 DPD threshold it froze
    # (config_used/static/staging_thresholds.csv), not an assumed 60. What a
    # different threshold would cost is Staging policy, under What-if &
    # scenarios.
    .thr <- reactive({
      d <- .out_dir_for(input$run_a)
      if (is.null(d)) return(60)
      staging_threshold(d, 60)
    })
    output$thr_note <- renderUI({
      p(class = "small-muted", style = "margin-top:6px",
        sprintf("Days past due under this run's policy: Stage 2 above %s, Stage 3 above 90. The other triggers are below.",
                format(.thr())))
    })

    output$t_stagedist <- reactable::renderReactable({
      sd <- staging_distribution(dat_a()); if (is.null(sd)) return(NULL)
      df <- data.frame(Stage = sd$stage, Customers = sd$customers,
                       `% of customers` = sd$pct_customers, Contracts = sd$contracts,
                       Exposure = sd$exposure, ECL = sd$ecl,
                       `ECL coverage %` = sd$coverage, `% of ECL` = sd$pct_ecl,
                       check.names = FALSE)
      .rt(df, sortable = FALSE, pagination = FALSE, columns = list(
        Exposure = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        ECL = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        `% of customers` = reactable::colDef(align = "right", cell = function(v) .an_pct0(v, 1)),
        `ECL coverage %` = reactable::colDef(align = "right", cell = function(v) .an_pct0(v)),
        `% of ECL` = reactable::colDef(align = "right", cell = function(v) .an_pct0(v, 1))))
    })

    # the run's reviewed stage overrides, so "Stage override" can be named
    .stage_ov <- reactive({
      d <- .out_dir_for(input$run_a); if (is.null(d)) return(NULL)
      run <- if (basename(d) == "Output") dirname(d) else d
      f <- file.path(run, "overrides", "stage_overrides.csv")
      if (!file.exists(f)) return(NULL)
      tryCatch(utils::read.csv(f, colClasses = "character"), error = function(e) NULL)
    })
    .s2 <- reactive({ stage2_triggers(dat_a(), .thr(), .stage_ov()) })

    output$c_s2trig <- renderUI({
      t <- .s2(); if (is.null(t)) return(.an_na("No Stage 2 customers."))
      if (!.an_echarts())
        return(qdb_reactable(data.frame(Reason = t$trigger, Contracts = t$contracts),
                             searchable = FALSE))
      t <- t[t$contracts > 0, , drop = FALSE]
      t <- t[order(t$contracts), , drop = FALSE]
      echarts4r::e_charts(t, trigger) |>
        echarts4r::e_bar(contracts, legend = FALSE, bar_width = "60%",
                         itemStyle = list(color = .AN_COL$plum)) |>
        echarts4r::e_flip_coords() |> echarts4r::e_tooltip() |>
        echarts4r::e_grid(left = 165, right = 30, top = 10, bottom = 30)
    })

    output$t_s2trig <- reactable::renderReactable({
      t <- .s2(); if (is.null(t)) return(NULL)
      df <- data.frame(Reason = t$trigger, Contracts = t$contracts, Customers = t$customers,
                       `% of Stage 2 contracts` = t$pct_contracts, Exposure = t$exposure,
                       ECL = t$ecl, check.names = FALSE)
      .rt(df, sortable = FALSE, pagination = FALSE, columns = list(
        Exposure = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        ECL = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        `% of Stage 2 contracts` = reactable::colDef(align = "right", cell = function(v) .an_pct0(v, 1))))
    })

    output$c_s2overlap <- renderUI({
      o <- stage2_trigger_overlap(dat_a(), .thr(), .stage_ov())
      if (is.null(o)) return(.an_na("No Stage 2 customers."))
      if (!.an_echarts())
        return(qdb_reactable(data.frame(Combination = o$combination, Contracts = o$contracts),
                             searchable = FALSE))
      o <- o[order(o$contracts), , drop = FALSE]
      echarts4r::e_charts(o, combination) |>
        echarts4r::e_bar(contracts, legend = FALSE, bar_width = "60%",
                         itemStyle = list(color = .AN_COL$plum_l)) |>
        echarts4r::e_flip_coords() |> echarts4r::e_tooltip() |>
        echarts4r::e_grid(left = 165, right = 30, top = 10, bottom = 30)
    })


    output$t_s3 <- reactable::renderReactable({
      s <- stage3_drivers(dat_a(), .stage_ov()); if (is.null(s)) return(NULL)
      df <- data.frame(Reason = s$driver, Customers = s$customers,
                       `% of Stage 3` = s$pct_customers, Exposure = s$exposure,
                       ECL = s$ecl, check.names = FALSE)
      .rt(df, sortable = FALSE, pagination = FALSE, columns = list(
        Exposure = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        ECL = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        `% of Stage 3` = reactable::colDef(align = "right", cell = function(v) .an_pct0(v, 1))))
    })

    output$c_dpdstage <- renderUI({
      x <- dpd_by_stage(dat_a()); if (is.null(x)) return(.an_na())
      if (!.an_echarts())
        return(qdb_reactable(x, searchable = FALSE, page_size = 15))
      echarts4r::e_charts(echarts4r::group_by(x, stage), band) |>
        echarts4r::e_bar(customers, stack = "s") |>
        echarts4r::e_color(c(.AN_COL$ok, .AN_COL$warn, .AN_COL$err)) |>
        echarts4r::e_tooltip(trigger = "axis") |> echarts4r::e_legend(bottom = 0) |>
        echarts4r::e_x_axis(name = "Days past due") |>
        echarts4r::e_grid(left = 65, right = 25, top = 15, bottom = 55)
    })

    output$t_stagechk <- renderUI({
      k <- staging_consistency(dat_a(), .thr())
      if (is.null(k)) return(.an_na())
      if (isFALSE(k$rule_available))
        return(.an_na("The staging rule could not be replayed for this run."))
      if (is.null(k$findings))
        return(div(class = "alert alert-success", icon("circle-check"),
          sprintf(" All %s contracts match the staging rule.",
                  .an_money(k$checked))))
      sev <- function(v) {
        tone <- if (identical(v, "error")) "qp-err" else
                if (identical(v, "warn")) "qp-warn" else "qp-info"
        htmltools::tags$span(class = paste("qdb-pill", tone), toupper(v))
      }
      f <- k$findings
      tagList(
        p(class = "small-muted",
          sprintf("%s of %s contracts differ from the rule.",
                  .an_money(k$mismatches), .an_money(k$checked))),
        reactable::reactable(
          data.frame(Severity = f$severity, Check = f$check, Contracts = f$contracts,
                     Customers = f$customers, Exposure = f$exposure, ECL = f$ecl,
                     Note = f$note, check.names = FALSE),
          class = "qdb-rt", compact = TRUE, wrap = TRUE, sortable = FALSE,
          pagination = FALSE, columns = list(
            Severity = reactable::colDef(cell = sev, minWidth = 80),
            Check = reactable::colDef(minWidth = 220),
            Exposure = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
            ECL = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
            Note = reactable::colDef(minWidth = 320))))
    })

    # ---- model curves (from the run's engine inputs) ----------------------
    # One set of curves per rating scale, shared by every portfolio on it, so
    # the picker is the scale and the ratings, not portfolio and bucket.
    .curves <- reactive({ pd_curves_by_rating(inp_a()) })
    output$pick_scale <- renderUI({
      c <- .curves(); if (is.null(c)) return(NULL)
      sc <- intersect(c("Internal", "External"), unique(c$scale))
      selectInput(ns("curve_scale"), "Rating scale", choices = sc, width = "100%")
    })
    .scale_curves <- reactive({
      c <- .curves(); if (is.null(c)) return(NULL)
      sc <- input$curve_scale %||% "Internal"
      c[c$scale == sc, , drop = FALSE]
    })
    output$pick_ratings <- renderUI({
      c <- .scale_curves(); if (is.null(c) || nrow(c) == 0) return(NULL)
      r <- unique(c[order(c$bucket), "rating"])
      sel <- r[unique(round(seq(1, length(r), length.out = min(5, length(r)))))]
      selectizeInput(ns("curve_ratings"), "Ratings", choices = r, selected = sel,
                     multiple = TRUE, width = "100%",
                     options = list(plugins = list("remove_button")))
    })
    .curve_subset <- function(ts) {
      sel <- input$curve_ratings
      if (is.null(ts) || is.null(sel) || length(sel) == 0) return(NULL)
      x <- ts[ts$rating %in% sel, , drop = FALSE]
      if (nrow(x) == 0) return(NULL)
      x$rating <- factor(x$rating, levels = unique(ts[order(ts$bucket), "rating"]))
      x[order(x$rating, x$month), , drop = FALSE]
    }
    .curve_cols <- function(n) grDevices::hcl.colors(max(n, 2), "viridis")[seq_len(n)]

    output$c_pdterm <- renderUI({
      ts <- .scale_curves()
      if (is.null(ts) || nrow(ts) == 0) return(.an_na("StPD not found in this run's outputs."))
      if (identical(input$curve_view, "heat")) {
        h <- ts[ts$month %% 6 == 0, , drop = FALSE]
        h$rating <- factor(h$rating, levels = unique(ts[order(ts$bucket), "rating"]))
        h <- h[order(h$rating, h$month), , drop = FALSE]
        h$rating <- as.character(h$rating)
        if (!.an_echarts())
          return(qdb_reactable(utils::head(h, 60), searchable = FALSE, page_size = 12))
        return(echarts4r::e_charts(h, month) |>
          echarts4r::e_heatmap(rating, cum_pd) |>
          echarts4r::e_visual_map(cum_pd,
            inRange = list(color = c("#e7f6f0", "#fdf3e3", "#fce9e9"))) |>
          echarts4r::e_tooltip(formatter = htmlwidgets::JS(
            "function(p){return 'month '+p.value[0]+', '+p.value[1]+'<br/>cum PD <b>'+Number(p.value[2]).toFixed(2)+'%</b>';}")) |>
          echarts4r::e_x_axis(name = "Month") |>
          echarts4r::e_grid(left = 95, right = 80, top = 15, bottom = 45))
      }
      ts <- .curve_subset(ts)
      if (is.null(ts)) return(.an_na("Pick one or more ratings to plot."))
      if (!.an_echarts())
        return(qdb_reactable(utils::head(ts, 60), searchable = FALSE, page_size = 12))
      echarts4r::e_charts(echarts4r::group_by(ts, rating), month) |>
        echarts4r::e_line(cum_pd, symbol = "none", lineStyle = list(width = 2)) |>
        echarts4r::e_x_axis(name = "Month", nameLocation = "middle", nameGap = 26) |>
        echarts4r::e_y_axis(name = "Cumulative PD %") |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_legend(type = "scroll", bottom = 0) |>
        echarts4r::e_color(.curve_cols(length(unique(ts$rating)))) |>
        echarts4r::e_grid(left = 70, right = 30, top = 15, bottom = 60)
    })

    output$c_pdmarg <- renderUI({
      ts <- .curve_subset(.scale_curves())
      if (is.null(ts)) return(.an_na())
      ts <- ts[ts$month <= 60, , drop = FALSE]
      if (!.an_echarts())
        return(qdb_reactable(utils::head(ts, 60), searchable = FALSE, page_size = 12))
      echarts4r::e_charts(echarts4r::group_by(ts, rating), month) |>
        echarts4r::e_line(marginal_pd, symbol = "none", lineStyle = list(width = 2)) |>
        echarts4r::e_x_axis(name = "Month", nameLocation = "middle", nameGap = 26) |>
        echarts4r::e_y_axis(name = "Marginal PD %") |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_legend(type = "scroll", bottom = 0) |>
        echarts4r::e_color(.curve_cols(length(unique(ts$rating)))) |>
        echarts4r::e_grid(left = 70, right = 30, top = 15, bottom = 60)
    })

    output$c_runoff <- renderUI({
      r <- ead_runoff(inp_a())
      if (is.null(r)) return(.an_na("LifeTimeParameterOther not found in this run."))
      if (!.an_echarts())
        return(qdb_reactable(data.frame(Month = r$month, Exposure = .an_money(r$exposure)),
                             searchable = FALSE, page_size = 12))
      echarts4r::e_charts(r, month) |>
        echarts4r::e_area(exposure, legend = FALSE, symbol = "none",
                          itemStyle = list(color = .AN_COL$plum),
                          areaStyle = list(opacity = 0.18)) |>
        echarts4r::e_x_axis(name = "Months ahead", nameLocation = "middle", nameGap = 26) |>
        echarts4r::e_y_axis(axisLabel = list(formatter = .an_m_axis())) |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_grid(left = 75, right = 25, top = 15, bottom = 55)
    })

    output$c_coll <- renderUI({
      ca <- collateral_analysis(inp_a())
      if (is.null(ca)) return(.an_na("Collateral files not found in this run."))
      warn <- if (ca$orphan_allocations > 0)
        div(class = "alert alert-warning", style = "margin-bottom:8px",
            icon("triangle-exclamation"),
            sprintf(" %s allocation(s) across %s contract(s) point at a collateral record that does not exist. LIC returns NaN coverage for these, which blanks the whole contract's ECL.",
                    .an_money(ca$orphan_allocations), .an_money(ca$orphan_contracts)))
      else div(class = "alert alert-success", style = "margin-bottom:8px",
               icon("circle-check"), " No orphan collateral allocations.")
      body <- if (is.null(ca$by_type)) .an_na("No collateral type breakdown available.")
        else if (!.an_echarts())
          qdb_reactable(data.frame(Type = ca$by_type$type, Records = ca$by_type$records,
                        Value = .an_money(ca$by_type$value)), searchable = FALSE)
        else {
          bt <- utils::head(ca$by_type, 10)
          echarts4r::e_charts(bt, type) |>
            echarts4r::e_bar(value, legend = FALSE, bar_width = "60%",
                             itemStyle = list(color = .AN_COL$teal)) |>
            echarts4r::e_flip_coords() |>
            echarts4r::e_x_axis(axisLabel = list(formatter = .an_m_axis())) |>
            echarts4r::e_tooltip() |>
            echarts4r::e_grid(left = 150, right = 30, top = 10, bottom = 30)
        }
      tagList(warn, body)
    })

    # ---------------------------------------------------------- tables -----
    .rt <- function(df, ...) reactable::reactable(df, class = "qdb-rt", compact = TRUE, ...)


    output$t_profile <- reactable::renderReactable({
      p <- run_profile(dat_a(), "portfolio"); if (is.null(p)) return(NULL)
      df <- data.frame(Portfolio = p$group, Contracts = p$contracts,
                       Exposure = p$exposure, ECL = p$ecl,
                       `ECL coverage %` = p$coverage, check.names = FALSE)
      .rt(df, defaultPageSize = 8, columns = list(
        Exposure = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        ECL = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        `ECL coverage %` = reactable::colDef(align = "right", cell = function(v) .an_pct0(v))))
    })

    output$pd_view <- renderUI({
      by <- input$pd_by %||% "stage"
      if (identical(by, "rating")) {
        b <- .pd_scales(); if (is.null(b)) return(.an_na())
        sc <- intersect(c("Internal", "External"), unique(b$scale))
        part <- function(k) b[b$scale == k, , drop = FALSE]
        return(tagList(
          fluidRow(lapply(sc, function(k) column(12 / length(sc),
            h5(paste(k, "rating")), .pd_scale_chart(part(k))))),
          p(class = "small-muted", "Best grade first. Stage 3 contracts count at 100%."),
          fluidRow(lapply(sc, function(k) column(12 / length(sc),
            h5(paste(k, "rating")), .pd_scale_table(part(k)))))))
      }
      p <- pd_profile(dat_a(), by); if (is.null(p)) return(.an_na())
      lab <- c(stage = "Stage", portfolio = "Portfolio", account_type = "Account type")[[by]]
      g <- if (identical(by, "stage")) paste("Stage", p$group) else p$group
      df <- data.frame(g, Contracts = p$contracts, Exposure = p$exposure,
                       `Weighted PD %` = 100 * p$pd_w, `Simple mean PD %` = 100 * p$pd_mean,
                       ECL = p$ecl, check.names = FALSE)
      names(df)[1] <- lab
      tagList(
        p(class = "small-muted", "Stage 3 counts at 100%: defaulted, so provisioned at the full outstanding."),
        .rt(df, defaultPageSize = 12, columns = list(
          Exposure = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
          ECL = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
          `Weighted PD %` = reactable::colDef(align = "right", cell = function(v) .an_pct0(v)),
          `Simple mean PD %` = reactable::colDef(align = "right", cell = function(v) .an_pct0(v)))))
    })

    output$t_floor <- reactable::renderReactable({
      f <- lgd_floor_stats(dat_a(), by = "portfolio")
      if (is.null(f)) return(NULL)
      df <- data.frame(Portfolio = f$group, Contracts = f$contracts,
                       `On floor` = f$on_floor, `% on floor` = f$pct_on_floor,
                       `Weighted LGD` = f$lgd_w, check.names = FALSE)
      .rt(df, defaultPageSize = 8, columns = list(
        `% on floor` = reactable::colDef(align = "right", cell = function(v) .an_pct0(v, 1)),
        `Weighted LGD` = reactable::colDef(align = "right",
          cell = function(v) if (is.na(v)) "\u2014" else sprintf("%.4f", v))))
    })

    output$t_hhi <- renderUI({
      d <- dat_a(); if (is.null(d)) return(NULL)
      lv <- input$conc_level %||% "customer"
      cc <- concentration(d, level = lv)
      cv <- customer_view(d)
      n_cust <- if (is.null(cv)) NA_integer_ else nrow(cv)
      h <- hhi(d, "customer"); bandinfo <- hhi_band(h); n_eq <- hhi_equivalent_n(h)
      top1 <- if (!is.null(cv)) {
        e <- sort(cv$ecl[cv$ecl > 0], decreasing = TRUE)
        if (length(e) > 0) 100 * e[1] / sum(e) else NA_real_
      } else NA_real_
      top10 <- if (!is.null(cc) && 10 %in% cc$top_n) cc$share[cc$top_n == 10][1] else NA_real_
      tone <- if (bandinfo$tone == "ok") "ok" else if (bandinfo$tone == "warn") "warn" else "err"
      tagList(
        qdb_stats(list(
          list(k = "Biggest customer's share of ECL", v = .an_pct0(top1, 1), tone = ""),
          list(k = "Top 10 customers' share", v = .an_pct0(top10, 1), tone = ""),
          list(k = "Customers in the book",
               v = if (is.na(n_cust)) "\u2014" else .an_money(n_cust), tone = ""))),
        div(style = "margin:2px 0 10px",
          span(class = sprintf("qdb-pill qp-%s", tone), toupper(bandinfo$band))),
        p(class = "small-muted",
          HTML(sprintf(paste0(
            "The provision is spread as thinly as it would be across <b>%s equally sized customers</b>, ",
            "out of %s actually in the book. Concentration index %s (under 1,500 is regarded as ",
            "diversified, over 2,500 as concentrated \u2014 a market convention, not a QCB limit)."),
            if (is.na(n_eq)) "\u2014" else .an_money(round(n_eq)),
            if (is.na(n_cust)) "\u2014" else .an_money(n_cust),
            .an_money(h)))),
        if (!is.null(cc)) qdb_reactable(
          data.frame(`Largest N` = cc$top_n, `Their ECL` = .an_money(cc$ecl),
                     `Share of total provision` = .an_pct0(cc$share),
                     check.names = FALSE),
          searchable = FALSE, page_size = 6) else NULL)
    })

    output$t_top <- reactable::renderReactable({
      t <- top_contributors(dat_a(), 25, level = input$conc_level %||% "customer")
      if (is.null(t)) return(NULL)
      lv <- input$conc_level %||% "customer"
      df <- data.frame(
        `Contract` = t$contract, Customer = t$customer,
        Portfolio = t$portfolio, Rating = t$rating, Stage = t$stage,
                       Exposure = t$exposure, ECL = t$ecl,
                       `ECL coverage %` = t$coverage, `Share %` = t$share,
                       check.names = FALSE)
      if (identical(lv, "customer")) names(df)[1] <- "Facilities"
      .rt(df, defaultPageSize = 12, searchable = TRUE, columns = list(
        Exposure = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        ECL = reactable::colDef(align = "right", cell = function(v) .an_money(v),
                                style = list(fontWeight = "600")),
        `ECL coverage %` = reactable::colDef(align = "right", cell = function(v) .an_pct0(v)),
        `Share %` = reactable::colDef(align = "right", cell = function(v) .an_pct0(v))))
    })

    # ------------------------------------------- customer ECL calculator ---
    calc_customers <- reactive({
      d <- .run_dir_for(input$run_a); if (is.null(d)) return(NULL)
      tryCatch(run_customers(d), error = function(e) NULL)
    })
    output$calc_pick <- renderUI({
      cu <- calc_customers()
      if (is.null(cu) || nrow(cu) == 0)
        return(qdb_empty("This run has no positions to export.", "file-excel"))
      lab <- sprintf("%s%s (%s, %d position%s)", cu$customer_id,
                     ifelse(nzchar(cu$customer_name), paste0(" \u2014 ", cu$customer_name), ""),
                     cu$book, cu$positions, ifelse(cu$positions == 1, "", "s"))
      selectizeInput(ns("calc_customer"), "Customer",
                     choices = stats::setNames(cu$customer_id, lab), width = "100%",
                     options = list(placeholder = "Type an id or a name"))
    })
    output$calc_cust <- renderUI({
      d <- dat_a(); id <- input$calc_customer
      if (is.null(d) || is.null(id) || !nzchar(id)) return(NULL)
      lk <- tryCatch(customer_lookup(d, id, spaces = FALSE), error = function(e) NULL)
      f <- lk$found
      if (is.null(f) || nrow(f) == 0) return(NULL)
      p(class = "small-muted", sprintf(
        "%s: %d position%s, exposure %s, ECL in the report %s (worst stage %s). The workbook's Summary tab rebuilds the provision position by position.",
        id, as.integer(f$contracts[1]), if (f$contracts[1] == 1) "" else "s",
        .an_money(f$exposure[1]), .an_money(f$ecl[1]), f$stage[1]))
    })
    output$calc_dl <- downloadHandler(
      filename = function() {
        id <- gsub("[^A-Za-z0-9_-]", "_", input$calc_customer %||% "customer")
        sprintf("ECL_calculator_%s_%s.xlsx", input$run_a %||% "run", id)
      },
      content = function(file) {
        d <- .run_dir_for(input$run_a); id <- input$calc_customer
        if (is.null(d) || is.null(id) || !nzchar(id)) {
          showNotification("Pick a run and a customer first.", type = "warning")
          return(invisible(NULL))
        }
        withProgress(message = "Building the workbook\u2026", value = 0.4, {
          ok <- tryCatch({ customer_ecl_workbook(d, id, file); TRUE },
                         error = function(e) {
                           showNotification(paste("The calculator could not be built:",
                                                  conditionMessage(e)),
                                            type = "error", duration = 10)
                           FALSE })
        })
        if (isTRUE(ok))
          tryCatch(audit_event(list(event = "customer_calculator_exported",
                                    run_id = input$run_a, customer_id = id,
                                    user = Sys.info()[["user"]])),
                   error = function(e) NULL)
      })

    output$t_dq <- reactable::renderReactable({
      q <- data_quality(dat_a())
      if (is.null(q)) return(reactable::reactable(
        data.frame(Result = "No data-quality findings."), sortable = FALSE, class = "qdb-rt"))
      sev_cell <- function(v) {
        tone <- if (identical(v, "error")) "qp-err" else
                if (identical(v, "warn")) "qp-warn" else "qp-info"
        htmltools::tags$span(class = paste("qdb-pill", tone), toupper(v))
      }
      df <- data.frame(Severity = q$severity, Check = q$check, Contracts = q$contracts,
                       `% of book` = q$pct, Exposure = q$exposure, ECL = q$ecl,
                       Note = q$note, check.names = FALSE)
      det <- data_quality_detail(dat_a())
      .rt(df, defaultPageSize = 12, wrap = TRUE,
        details = if (is.null(det)) NULL else function(index) {
          .drill_contracts(det[[q$check[index]]])
        },
        columns = list(
        Severity = reactable::colDef(cell = sev_cell, minWidth = 90),
        Check = reactable::colDef(minWidth = 170),
        `% of book` = reactable::colDef(align = "right", cell = function(v) .an_pct0(v)),
        Exposure = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        ECL = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        Note = reactable::colDef(minWidth = 320)))
    })
  })
}
