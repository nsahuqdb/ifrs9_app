# =============================================================================
# app/modules/mod_analytics.R
#
# Analytics page. Two modes, each a set of tabs:
#
#   Single run    Overview | PD | LGD & collateral | Exposure & maturity |
#                 Concentration | Data quality
#   Compare runs  ECL walk | Risk migration | Attribution | Flows
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
  ifelse(is.na(x), "\u2014", format(round(x), big.mark = ",", scientific = FALSE))
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
                                          "What-if & scenarios" = "scenario"),
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
      if (identical(input$mode, "scenario")) lbl <- "Run to reweight"
      sel <- if (identical(input$mode, "compare") && length(ch) > 1) ch[2] else ch[1]
      selectInput(ns("run_a"), lbl, choices = ch, selected = sel, width = "100%")
    })
    output$pick_b <- renderUI({
      if (!identical(input$mode, "compare")) return(NULL)
      ch <- run_choices()
      selectInput(ns("run_b"), "Current run (closing)", choices = ch,
                  selected = ch[1], width = "100%")
    })

    .report_for <- function(run_id) {
      r <- runs_tbl(); if (is.null(r) || is.null(run_id) || !nzchar(run_id)) return(NULL)
      row <- r[r$run_id == run_id, , drop = FALSE]
      if (nrow(row) == 0) return(NULL)
      paths <- tryCatch(list_run_outputs(row$path[1]), error = function(e) character(0))
      ov <- paths[grepl("^FinalEclReport_overlay_.*\\.csv$", basename(paths))]
      base <- paths[basename(paths) == "FinalEclReport.csv"]
      pick <- if (length(ov) > 0) ov[1] else if (length(base) > 0) base[1] else NULL
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
    walk_r <- reactive({
      a <- dat_a(); b <- dat_b()
      if (is.null(a) || is.null(b)) return(NULL)
      ecl_walk(a, b)
    })

    # ------------------------------------------------------------ KPIs -----
    output$kpis <- renderUI({
      if (identical(input$mode, "compare")) {
        w <- walk_r(); if (is.null(w)) return(NULL)
        chg <- w$closing - w$opening
        qdb_stats(list(
          list(k = "Opening ECL", v = .an_money(w$opening), tone = ""),
          list(k = "Closing ECL", v = .an_money(w$closing), tone = "accent"),
          list(k = "Movement", v = .an_money(chg), tone = if (chg > 0) "err" else "ok"),
          list(k = "% change", v = sprintf("%+.2f%%", 100 * chg / max(w$opening, 1)),
               tone = if (chg > 0) "err" else "ok"),
          list(k = "Stage migrated", v = .an_money(w$counts$migrated), tone = "warn"),
          list(k = "New / left",
               v = sprintf("%s / %s", .an_money(w$counts$arrived), .an_money(w$counts$left)),
               tone = "")))
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
          nav_panel("ECL walk",
            p(class = "small-muted", style = "margin-top:8px",
              "Movement by cause; the steps sum to the closing balance."),
            qdb_help(list(
              "Opening / Closing" = "Total ECL of the prior and current run.",
              "Derecognised" = "ECL of contracts in the prior run that are absent from the current one \u2014 repaid, closed or written off. Always negative.",
              "New business" = "ECL of contracts present now but not before.",
              "Exposure movement" = "For contracts in both runs: <code>(exposure now &minus; exposure before) &times; coverage before</code>. The provision effect of lending more or less, at the old risk level.",
              "Stage migration" = "For contracts whose stage changed: <code>exposure now &times; (coverage now &minus; coverage before)</code>.",
              "Risk &amp; model" = "The same coverage effect for contracts whose stage did NOT change \u2014 rating moves, PD or LGD changes, model or config changes.",
              "Other" = "Contracts with no exposure in the prior run, so no prior coverage exists to split the movement with. Their whole change sits here rather than being spread.",
              "Coverage" = "ECL divided by on-balance exposure.",
              "Drilling in" = "Open any step in the table to see the contracts behind it, largest first."),
              note = "The steps sum exactly to the closing balance. Exposure is measured at the OLD coverage, then coverage change at the NEW exposure; that order is fixed so the walk is reproducible each quarter."),

            uiOutput(ns("c_walk")),
            reactable::reactableOutput(ns("t_walk"))),
          nav_panel("Risk migration",
            fluidRow(column(4, uiOutput(ns("pick_rt2")))),
            p(class = "small-muted", "Customer counts, ratings ordered best first."),
            qdb_help(list(
              "Stage migration" = "Customers moving between IFRS 9 stages, counted as customers because staging is set per customer.",
              "Rating migration" = "Customers moving between rating grades, ordered along the scale's own hierarchy (best first).",
              "Upgrade / Downgrade" = "Decided by hierarchy rank, not alphabetically \u2014 a downgrade is a move to a worse grade.",
              "Rating scale" = "Internal (QDB grades) for Business Finance, Off BS, Al Dhameen and Tasdeer; external (agency grades) for Investments and Banks. Both reuse hierarchy 1&ndash;21, so they are never mixed."),
              note = "Each cell shows the number of customers making that move."),

            fluidRow(
              column(7, h5("Stage migration (customers)"), uiOutput(ns("c_ctrans"))),
              column(5, h5("Rating direction"), uiOutput(ns("c_migsum")))),
            hr(), h5("Rating migration matrix"), uiOutput(ns("c_ratmig"))),
          nav_panel("Attribution",
            fluidRow(column(4, selectInput(ns("attr_by"), "Group by",
              choices = c("Portfolio" = "portfolio", "Stage" = "stage",
                          "Rating" = "rating"), width = "100%"))),
            uiOutput(ns("c_move")),
            reactable::reactableOutput(ns("t_attr")),
            hr(),
            h5("EAD / PD / LGD / horizon attribution"),
            p(class = "small-muted",
              "One factor substituted at a time; the effects sum to the movement."),
            qdb_help(list(
              "Movement by segment" = "Change in ECL for each portfolio, stage or rating between the two runs.",
              "Horizon" = "Effect of time passing \u2014 contracts moving closer to maturity, so fewer months of loss remain.",
              "EAD" = "Effect of the exposure-at-default curve changing.",
              "PD" = "Effect of the probability-of-default term structure changing (rating moves and model updates).",
              "LGD" = "Effect of loss-given-default changing, mostly collateral.",
              "Not attributable" = "Contracts lacking a PD or EAD curve in one of the runs. Reported separately rather than spread across the factors."),
              note = "The four factors are found by re-running the ECL formula on each run's own inputs and substituting ONE factor at a time, in the order horizon, EAD, PD, LGD. They sum exactly to the movement on the contracts covered."),

            uiOutput(ns("c_factor")),
            uiOutput(ns("t_factor_note"))),
          nav_panel("Segments",
            p(class = "small-muted", style = "margin-top:8px",
              "Exposure and coverage before and after, by segment."),
            qdb_help(list(
              "Exposure before / after" = "On-balance exposure in each run.",
              "ECL coverage" = "ECL divided by exposure, shown for both runs so a change in provisioning is separable from a change in size.",
              "ECL change" = "Closing ECL minus opening ECL for that segment."),
              note = "A segment can grow while its coverage falls, or shrink while its coverage rises \u2014 showing both makes which one happened obvious."),

            fluidRow(column(4, selectInput(ns("seg_by"), "Group by",
              choices = c("Portfolio" = "portfolio", "Stage" = "stage",
                          "Rating" = "rating", "Account type" = "account_type"),
              width = "100%"))),
            reactable::reactableOutput(ns("t_seg"))),
          nav_panel("Staging movement",
            p(class = "small-muted", style = "margin-top:8px",
              "Customers whose stage changed, and what it cost."),
            qdb_help(list(
              "Deterioration" = "Customers moving to a worse stage (1&rarr;2, 2&rarr;3).",
              "Improvement" = "Customers moving to a better stage, normally a cure.",
              "ECL now" = "The provision those customers carry in the current run.",
              "Trigger mix" = "Why Stage 2 customers are in Stage 2, in both runs, so a shift in the reason is visible."),
              note = "Counted as customers: staging is a customer-level decision."),

            reactable::reactableOutput(ns("t_stagemove")),
            hr(), h5("Stage 2 trigger mix, both runs"),
            reactable::reactableOutput(ns("t_trigdelta"))),
          nav_panel("Flows",
            qdb_help(list(
              "New business" = "Contracts present in the current run but not the prior one.",
              "Derecognised" = "Contracts present before but not now \u2014 repaid, closed or written off.",
              "ECL coverage" = "Provision as a percentage of exposure for that flow, which shows whether new lending is riskier or safer than what left."),
              note = NULL),
            h5("What came on and off the book"),
            reactable::reactableOutput(ns("t_flow")),
            hr(), h5("Largest movements by contract"),
            reactable::reactableOutput(ns("t_movers"))))
      } else if (identical(input$mode, "scenario")) {
        se <- scen_source()
        if (is.null(se) || isFALSE(se$ok))
          return(qdb_empty(se$reason %||% "Scenario provisions are not available for this run.",
                           "sliders"))
        navset_card_tab(
          nav_panel("What-if",
            p(class = "small-muted", style = "margin-top:8px",
              "Add a rule, enter the customers it applies to, set what changes, then Apply."),
            qdb_help(list(
              "Rule" = "Who a change applies to (customer ids, or portfolio and stage filters) and what changes for them. Several rules can apply different changes to different customers at once.",
              "Conflict" = "A contract matched by more than one rule. It is skipped rather than having the rules combined.",
              "Move to stage" = "Sets the ECL horizon: Stage 1 is capped at 12 months, Stage 2 runs to maturity, Stage 3 books the full outstanding.",
              "Rating notches" = "Moves along that customer's own rating scale. Positive is a downgrade, and it selects a different PD curve.",
              "Collateral %" = "Scales collateral value, which feeds LGD through the coverage ratio and the 0.225 floor.",
              "Exposure %" = "Scales the balance and therefore the EAD curve.",
              "Extend maturity" = "Shifts the maturity date, lengthening the ECL horizon.",
              "PD scenario" = "Substitutes that scenario's PD curve from this run's own scenario output.",
              "Facilities" = "Shows \u201c2 of 4\u201d when a rule touched only part of a customer's relationship."),
              note = "The book is repriced with the production ECL function, so every rule inherits the engine's own EAD, LGD, staging and discounting logic. Nothing is re-run and nothing is written."),

            uiOutput(ns("wi_rules_ui")),
            div(style = "margin:8px 0 14px",
              actionButton(ns("wi_add_rule"), "Add rule", icon = icon("plus"),
                           class = "btn-sm btn-outline-primary"),
              actionButton(ns("wi_apply"), "Apply all rules", icon = icon("play"),
                           class = "btn-sm btn-primary", style = "margin-left:8px")),
            uiOutput(ns("wi_result"))),
          nav_panel("Staging policy",
            p(class = "small-muted", style = "margin-top:8px",
              "Change the staging rule and reprice. Scope it to the whole book or chosen portfolios."),
            qdb_help(list(
              "DPD threshold" = "Stage 2 applies when days past due exceed this and are at most 90. The rule is <code>DPD &gt; threshold</code>, so 60 means 61 days and over.",
              "Contagion" = "A customer with any Stage 2 or worse facility has its Stage 1 facilities bumped to Stage 2. Tasdeer is excluded.",
              "Tasdeer collective" = "Tasdeer is staged 2 regardless of any other trigger.",
              "Watchlist / local flags" = "Whether those flags force Stage 2. Local flags cover restructuring and the other five local indicators.",
              "Scope" = "Only contracts in scope are re-staged; everything else keeps its reported stage, so a portfolio stress cannot silently move the rest of the book."),
              note = "Re-staging uses the report's own staging function, and repricing uses the production ECL function, so the result is what a run under this policy would produce."),
            div(class = "qdb-card", style = "padding:12px",
              fluidRow(
                column(4, uiOutput(ns("sp_scope"))),
                column(3, numericInput(ns("sp_thr"), "Stage 2 DPD threshold",
                                       value = 60, min = 0, max = 90, step = 5, width = "100%")),
                column(5, div(style = "margin-top:24px",
                  checkboxInput(ns("sp_contagion"), "Contagion between a customer's facilities", TRUE),
                  checkboxInput(ns("sp_tasdeer"), "Tasdeer collectively Stage 2", TRUE)))),
              fluidRow(
                column(4, checkboxInput(ns("sp_watch"), "Watchlist forces Stage 2", TRUE)),
                column(4, checkboxInput(ns("sp_local"), "Local flags force Stage 2", TRUE)),
                column(4, actionButton(ns("sp_apply"), "Apply policy", icon = icon("play"),
                                       class = "btn-sm btn-primary")))),
            uiOutput(ns("sp_result")),
            hr(), h5("Provision across DPD thresholds"),
            p(class = "small-muted", "Everything else held at the settings above."),
            uiOutput(ns("sp_sweep"))),
          nav_panel("Stress packages",
            p(class = "small-muted", style = "margin-top:8px",
              "Combine levers into a named stress, save it, and compare packages side by side."),
            qdb_help(list(
              "Package" = "A named set of levers applied together. Saved to <code>config/stress_packages.yml</code> so the same stress can be re-run next quarter and compared like for like.",
              "Scope" = "Whole book, or chosen portfolios. Only contracts in scope are affected.",
              "PD multiplier" = "Scales every cumulative PD curve, capped at 1. A blunt but standard supervisory shock.",
              "LGD base" = "The unsecured loss rate, normally 0.45.",
              "LGD unsecured floor" = "The minimum unsecured share, normally 0.5. Raising it RAISES the LGD floor (base x floor), so it bites hardest on well-secured lending.",
              "Collateral %" = "Scales collateral value; LGD then moves through the real formula.",
              "Exposure %" = "Scales balances and therefore the EAD curves.",
              "Rating notches" = "Moves every in-scope contract along its own rating scale. Positive is a downgrade.",
              "Default largest N" = "Forces the N largest customers by provision to Stage 3, which books their full outstanding. The classic single-name concentration stress.",
              "Staging policy" = "The same levers as the Staging policy tab, so a package can combine a policy change with a parameter shock."),
              note = "Every lever is applied by varying the inputs to the production ECL function or its context, so the engine's EAD waterfall, LGD formula, horizons, discounting and exposure cap all still apply."),
            uiOutput(ns("stp_note")),
            uiOutput(ns("stp_editor")),
            div(style = "margin:8px 0 14px",
              actionButton(ns("stp_add"), "Add package", icon = icon("plus"),
                           class = "btn-sm btn-outline-primary"),
              actionButton(ns("stp_run"), "Run all", icon = icon("play"),
                           class = "btn-sm btn-primary", style = "margin-left:8px"),
              actionButton(ns("stp_save"), "Save", icon = icon("floppy-disk"),
                           class = "btn-sm btn-outline-secondary", style = "margin-left:8px"),
              actionButton(ns("stp_load"), "Load saved", icon = icon("folder-open"),
                           class = "btn-sm btn-outline-secondary", style = "margin-left:4px")),
            uiOutput(ns("stp_result"))),
          nav_panel("Reverse stress",
            p(class = "small-muted", style = "margin-top:8px",
              "Set a provision increase and see how far each lever would have to move to cause it."),
            qdb_help(list(
              "Target" = "The increase in the total provision you want to reach, in percent.",
              "Required level" = "How far that lever must move to get there, found by repricing repeatedly and narrowing in \u2014 not read off a grid.",
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
          nav_panel("Roll forward",
            p(class = "small-muted", style = "margin-top:8px",
              "What the provision becomes in n months if nothing else changes."),
            qdb_help(list(
              "What this is" = "A mechanical roll of the existing book. The contracts are untouched; only the reporting date moves, so this isolates TIME DECAY from any credit view.",
              "Balance" = "Advanced along each contract's own EAD curve \u2014 elapsed months are dropped, so the balance is what the schedule says it will be at that date, not today's.",
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
            uiOutput(ns("rf_result"))),
          nav_panel("Lever sensitivity",
            p(class = "small-muted", style = "margin-top:8px",
              "Each lever moved on its own, ranked by effect on the provision. Edit any move before running."),
            qdb_help(list(
              "What this shows" = "Where the provision is actually sensitive. A lever with a large bar is one to watch; a flat one is not worth stressing.",
              "One at a time" = "Every lever starts from the same base and moves alone, so the bars are comparable. They do not add up \u2014 combining levers needs a stress package.",
              "Standard moves" = "PD +25%, one and two notch downgrades, collateral down 25% and 50%, exposure +20%, LGD base and floor shifts, DPD threshold 60 to 30, the largest five defaulting, contagion off, Tasdeer not collective."),
              note = "Runs a full repricing per lever, so it takes several seconds."),
            uiOutput(ns("tn_levers")),
            div(style = "margin:6px 0 12px",
              actionButton(ns("tn_run"), "Run", icon = icon("chart-simple"),
                           class = "btn-sm btn-primary")),
            uiOutput(ns("tn_result"))),
          nav_panel("MEV forecast",
            p(class = "small-muted", style = "margin-top:8px",
              "Change the macro path for the next five years and rebuild the PD curves."),
            qdb_help(list(
              "What this does" = "Replaces the macro forecast, then re-runs the whole PD chain \u2014 <code>stress_mevs</code> to the annual term structure to the monthly StPD \u2014 and reprices. The change propagates through the model rather than being applied to the answer.",
              "Versus scenario weights" = "Reweighting moves probability between the five scenarios but leaves the macro path alone. This changes the path itself, so all five scenarios shift together.",
              "Shock" = "An absolute shift applied to every year of one variable. \u22122 on GDP growth means two percentage points lower across the whole path.",
              "Model weight" = "Each variable's weight in the model. A variable weighted zero will not move the provision however far it is shocked \u2014 that is the model, not a fault.",
              "Held fixed" = "Collateral, LGD, staging and the EAD curves are untouched, so what you see is the macro effect alone.",
              "Scenario weights" = "Internal weights run on <code>auto_non_oil_gdp_cdf</code>, and the loader takes that forecast from Non-Oil GDP in the FIRST TWO years of this table \u2014 so by default the weights move with the path. Hold them to isolate the PD effect, or set them yourself.",
              "Which years matter" = "Only years 1 and 2 of Non-Oil GDP feed the weights (<code>n_forecast_years: 2</code>). Later years change the curves but not the weights."),
              note = "This rebuilds the term structure and the monthly PD curves before repricing, so it takes a few seconds."),
            uiOutput(ns("mev_editor")),
            div(class = "qdb-card", style = "padding:12px;margin-bottom:8px",
              radioButtons(ns("mev_wmode"), "Scenario weights",
                choices = c(
                  "Let them follow the new path (as the model is configured)" = "auto",
                  "Hold them at this run's weights" = "hold",
                  "Set them myself" = "custom"),
                selected = "auto"),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'custom'", ns("mev_wmode")),
                uiOutput(ns("mev_wcustom")))),
            div(style = "margin:8px 0 14px",
              actionButton(ns("mev_run"), "Rebuild and reprice", icon = icon("play"),
                           class = "btn-sm btn-primary"),
              actionButton(ns("mev_reset"), "Reset to the run's forecast",
                           icon = icon("rotate-left"),
                           class = "btn-sm btn-outline-secondary", style = "margin-left:8px")),
            uiOutput(ns("mev_result"))),
          nav_panel("Scenario comparison",
            p(class = "small-muted", style = "margin-top:8px",
              "Provision under each scenario."),
            qdb_help(list(
              "Scenario" = "One of the five forward-looking macro scenarios. Each has its own PD curve.",
              "Severity z" = "The scenario's severity as a standard normal shift. Negative is a downturn.",
              "Weighted (reported)" = "The probability-weighted provision \u2014 the figure actually reported.",
              "vs Base" = "Difference against the Base Case scenario."),
              note = "Every run prices the book under all five scenarios as well as the weighted curve, so these come from the run's own outputs."),

            uiOutput(ns("scen_origin")),
            uiOutput(ns("c_scen")), reactable::reactableOutput(ns("t_scen"))),
          nav_panel("What-if reweighting",
            p(class = "small-muted", style = "margin-top:8px",
              "Set the weights; the provision is the weighted sum of the scenario provisions."),
            qdb_help(list(
              "Weight" = "Probability assigned to each scenario. They are normalised to 100%.",
              "Scenario ECL" = "The provision if that scenario were certain.",
              "Contribution" = "Weight multiplied by that scenario's provision.",
              "Provision at these weights" = "The sum of the contributions."),
              note = "ECL is linear in the marginal PDs, so the provision under any weighting is exactly the weighted sum of the per-scenario provisions \u2014 no re-run is needed."),

            uiOutput(ns("scen_sliders")),
            uiOutput(ns("t_whatif"))),
          nav_panel("Sensitivity",
            p(class = "small-muted", style = "margin-top:8px",
              "Each row adds 10pp to one scenario, taken from the others in proportion."),
            qdb_help(list(
              "Weight before / after" = "Each row adds 10 percentage points to one scenario and removes the same 10pp from the others in proportion, so the five still total 100%.",
              "Provision after" = "Recomputed as the weighted sum of the per-scenario provisions.",
              "Change" = "Provision after minus provision before."),
              note = "The scenario with the largest change is the one the provision is most exposed to."),

            uiOutput(ns("c_sens")), reactable::reactableOutput(ns("t_sens"))))
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
              "Staging rule" = "Stage 3 if DPD &gt; 90 or the default flag is set. Stage 2 if DPD is above the threshold but at most 90, or watchlist, or any local flag, or the portfolio is Tasdeer. Otherwise Stage 1.",
              "Tasdeer" = "Collectively assessed as Stage 2 regardless of any other trigger.",
              "Contagion" = "A customer with any Stage 2 or worse facility has its Stage 1 facilities bumped to Stage 2. Tasdeer is excluded.",
              "Stage override" = "A stage forced in the source data, which overrides the rule.",
              "Has trigger vs sole reason" = "Triggers overlap, so \u2018has trigger\u2019 shares add to more than 100%; \u2018sole reason\u2019 counts only contracts where that trigger is the single cause and is additive.",
              "Staging consistency" = "Re-applies the staging rule and reports only genuine disagreements. Overrides cannot be replayed from the report, so they appear as differences."),
              note = "Staging is decided per customer, but Tasdeer and contagion act per contract, so the trigger breakdown is contract-level."),

            reactable::reactableOutput(ns("t_stagedist")),
            hr(),
            fluidRow(
              column(6, h5("Why each Stage 2 contract is in Stage 2"), uiOutput(ns("c_s2trig"))),
              column(6, h5("Trigger combinations"), uiOutput(ns("c_s2overlap")))),
            hr(),
            fluidRow(column(4, numericInput(ns("dpd_thr"), "Stage 2 DPD threshold",
                                            value = 60, min = 0, max = 90, width = "100%"))),
            hr(), h5("Stage 3 drivers"), reactable::reactableOutput(ns("t_s3")),
            hr(), h5("DPD distribution by stage (customers)"), uiOutput(ns("c_dpdstage")),
            hr(), h5("Staging consistency"), uiOutput(ns("t_stagechk"))),
          nav_panel("PD",
            fluidRow(column(4, uiOutput(ns("pick_rt")))),
            p(class = "small-muted", "One rating scale at a time."),
            qdb_help(list(
              "Lifetime PD" = "Cumulative probability of default from now to the end of the ECL horizon.",
              "Weighted PD" = "Exposure-weighted average PD for the segment \u2014 large exposures count more.",
              "Simple mean PD" = "Unweighted average across contracts. A gap against the weighted figure means the risk sits in a few large names.",
              "PD bucket" = "The rating's hierarchy number, which selects the PD curve for that portfolio.",
              "Rating scale" = "Internal and external scales reuse hierarchy 1&ndash;21, so only one scale is shown at a time."),
              note = "ECL coverage should rise as the rating worsens. A kink usually means a rating or curve mapping is wrong rather than a genuine risk pattern."),

            fluidRow(
              column(6, h5("Lifetime PD distribution"), uiOutput(ns("c_pddist"))),
              column(6, h5("PD and coverage by rating"), uiOutput(ns("c_pdrating")))),
            hr(),
            h5("Exposure-weighted PD by segment"),
            fluidRow(column(4, selectInput(ns("pd_by"), NULL,
              choices = c("Stage" = "stage", "Portfolio" = "portfolio",
                          "Rating" = "rating", "Account type" = "account_type"),
              width = "100%"))),
            reactable::reactableOutput(ns("t_pd"))),
          nav_panel("LGD & collateral",
            qdb_help(list(
              "LGD" = "Loss given default: <code>0.45 &times; max(0.5, (exposure &minus; collateral) / exposure)</code>, floored at 0.225.",
              "The 0.45" = "Base unsecured loss rate.",
              "The 0.5" = "At least half of any exposure is treated as unsecured, however much collateral is held.",
              "The 0.225 floor" = "The lowest LGD any contract can carry, being 0.45 &times; 0.5. It applies to Stage 2 as well.",
              "Collateral coverage" = "Net collateral value divided by exposure. Over 100% means over-collateralised \u2014 the floor still applies.",
              "On floor" = "Contracts sitting exactly at 0.225, i.e. secured enough that more collateral would not reduce the provision."),
              note = "Because of the floor, collateral beyond roughly 50% coverage does not reduce LGD any further."),
            fluidRow(
              column(6, h5("LGD distribution"), uiOutput(ns("c_lgddist"))),
              column(6, h5("Collateral coverage bands"), uiOutput(ns("c_collbands")))),
            hr(),
            fluidRow(
              column(6, h5("LGD vs collateral coverage"), uiOutput(ns("c_lgdscatter"))),
              column(6, h5("LGD floor incidence"), reactable::reactableOutput(ns("t_floor"))))),
          nav_panel("Exposure & maturity",
            qdb_help(list(
              "Run-off profile" = "Exposure and ECL grouped by remaining months to maturity \u2014 how quickly the book matures.",
              "Exposure bands" = "Contracts grouped by ticket size, showing whether the book is many small facilities or a few large ones.",
              "Vintage" = "Grouped by the year the facility was opened. Coverage by vintage shows whether a particular year's lending is performing worse.",
              "Delinquency" = "Grouped by days past due. Coverage should climb steeply across the buckets.",
              "ECL coverage %" = "ECL divided by exposure within each band."),
              note = "Longer-dated exposure normally carries higher coverage, because the lifetime PD accumulates over more months."),
            fluidRow(
              column(6, h5("Run-off profile by remaining maturity"), uiOutput(ns("c_maturity"))),
              column(6, h5("Exposure size bands"), uiOutput(ns("c_sizebands")))),
            hr(),
            fluidRow(
              column(6, h5("Vintage by origination year"), uiOutput(ns("c_vintage"))),
              column(6, h5("Delinquency profile"), uiOutput(ns("c_dpd"))))),
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
          nav_panel("Model curves",
            fluidRow(
              column(4, uiOutput(ns("pick_pf"))),
              column(5, uiOutput(ns("pick_buckets"))),
              column(3, selectInput(ns("curve_view"), "View",
                choices = c("Selected curves" = "lines", "All curves as a heatmap" = "heat"),
                width = "100%"))),
            p(class = "small-muted", "Pick buckets to compare, or use the heatmap for all."),
            qdb_help(list(
              "PD term structure" = "Cumulative probability of default by month, one curve per portfolio and rating bucket, taken from the run's StPD output.",
              "Marginal PD" = "The increment each month: <code>cumulative PD(m) &minus; cumulative PD(m&minus;1)</code>. This is what the ECL sum uses.",
              "Rating bucket" = "The rating's hierarchy number. A higher bucket is a worse grade and should carry a higher curve.",
              "EAD run-off" = "Total exposure at default by month ahead, from the supplied monthly EAD curves. It shows the expected amortisation of the book.",
              "Collateral by type" = "Total collateral value by type.",
              "Orphan allocations" = "Collateral allocations pointing at a collateral record that does not exist. LIC returns NaN coverage for these, which blanks the whole contract's ECL."),
              note = "The EAD run-off covers only contracts with a supplied curve. Off BS, Al Dhameen and Tasdeer carry none, so they are outside that chart."),

            h5("PD term structure \u2014 cumulative"), uiOutput(ns("c_pdterm")),
            hr(), h5("Marginal PD by month"), uiOutput(ns("c_pdmarg")),
            hr(),
            fluidRow(
              column(6, h5("Aggregate EAD run-off (supplied curves)"),
                     uiOutput(ns("c_runoff"))),
              column(6, h5("Collateral composition"), uiOutput(ns("c_coll"))))),
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

            reactable::reactableOutput(ns("t_dq"))))
      }
    })

    # ---------------------------------------------------------- charts -----
    output$c_walk <- renderUI({
      w <- walk_r(); if (is.null(w)) return(NULL)
      s <- w$steps
      if (!.an_echarts())
        return(qdb_reactable(data.frame(Step = s$label, Amount = .an_money(s$amount)),
                             searchable = FALSE, page_size = 10))
      amt <- s$amount; base <- numeric(length(amt)); vis <- numeric(length(amt)); run <- 0
      for (i in seq_along(amt)) {
        if (s$kind[i] == "total") { base[i] <- 0; vis[i] <- amt[i]; run <- amt[i] }
        else {
          if (amt[i] >= 0) { base[i] <- run; vis[i] <- amt[i] }
          else { base[i] <- run + amt[i]; vis[i] <- -amt[i] }
          run <- run + amt[i]
        }
      }
      cols <- ifelse(s$kind == "total", .AN_COL$plum,
              ifelse(amt >= 0, .AN_COL$err, .AN_COL$ok))
      col_js <- htmlwidgets::JS(sprintf("function(p){var c=[%s]; return c[p.dataIndex];}",
                                        paste0("'", cols, "'", collapse = ",")))
      amt_js <- paste0("[", paste(sprintf("%.4f", amt), collapse = ","), "]")
      df <- data.frame(step = factor(s$label, levels = s$label), base = base, vis = vis)
      echarts4r::e_charts(df, step) |>
        echarts4r::e_bar(base, stack = "w", legend = FALSE,
                         itemStyle = list(color = "transparent")) |>
        echarts4r::e_bar(vis, stack = "w", legend = FALSE, bar_width = "55%",
                         itemStyle = list(color = col_js)) |>
        echarts4r::e_y_axis(axisLabel = list(formatter = .an_m_axis())) |>
        echarts4r::e_x_axis(axisLabel = list(interval = 0, rotate = 22, fontSize = 10)) |>
        echarts4r::e_tooltip(formatter = htmlwidgets::JS(sprintf(
          "function(p){var a=%s; return p.name+'<br/><b>'+a[p.dataIndex].toLocaleString(undefined,{maximumFractionDigits:0})+'</b>';}", amt_js))) |>
        echarts4r::e_grid(bottom = 90, left = 75, right = 20, top = 20) |>
        echarts4r::e_toolbox_feature("saveAsImage")
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

    output$c_move <- renderUI({
      m <- movement_by(dat_a(), dat_b(), input$attr_by %||% "portfolio")
      if (is.null(m) || nrow(m) == 0) return(NULL)
      m <- utils::head(m, 12); m <- m[order(m$change), , drop = FALSE]
      if (!.an_echarts())
        return(qdb_reactable(data.frame(Group = m$group, Change = .an_money(m$change)),
                             searchable = FALSE, page_size = 12))
      cj <- htmlwidgets::JS(sprintf("function(p){var c=[%s]; return c[p.dataIndex];}",
        paste0("'", ifelse(m$change >= 0, .AN_COL$err, .AN_COL$ok), "'", collapse = ",")))
      echarts4r::e_charts(m, group) |>
        echarts4r::e_bar(change, legend = FALSE, bar_width = "60%",
                         itemStyle = list(color = cj)) |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_x_axis(axisLabel = list(formatter = .an_m_axis())) |>
        echarts4r::e_tooltip() |>
        echarts4r::e_grid(left = 140, right = 30, top = 10, bottom = 30)
    })

    output$c_factor <- renderUI({
      fa <- factor_attribution_exact(inp_a(), dat_a(), inp_b(), dat_b())
      if (is.null(fa))
        return(.an_na(factor_attribution_diagnosis(inp_a(), dat_a(), inp_b(), dat_b())))
      e <- fa$effects
      if (!.an_echarts())
        return(qdb_reactable(data.frame(Factor = e$factor, Effect = .an_money(e$effect)),
                             searchable = FALSE))
      cj <- htmlwidgets::JS(sprintf("function(p){var c=[%s]; return c[p.dataIndex];}",
        paste0("'", ifelse(e$effect >= 0, .AN_COL$err, .AN_COL$ok), "'", collapse = ",")))
      echarts4r::e_charts(e, factor) |>
        echarts4r::e_bar(effect, legend = FALSE, bar_width = "45%",
                         itemStyle = list(color = cj)) |>
        echarts4r::e_y_axis(axisLabel = list(formatter = .an_m_axis())) |>
        echarts4r::e_tooltip() |>
        echarts4r::e_grid(left = 80, right = 25, top = 15, bottom = 35)
    })

    output$t_factor_note <- renderUI({
      fa <- factor_attribution_exact(inp_a(), dat_a(), inp_b(), dat_b())
      if (is.null(fa)) return(NULL)
      e <- fa$effects
      df <- data.frame(Factor = c(e$factor, "Sum of effects", "Not attributable"),
                       Effect = c(e$effect, sum(e$effect), fa$uncovered),
                       check.names = FALSE)
      tagList(
        qdb_reactable(data.frame(Factor = df$Factor, Effect = .an_money(df$Effect)),
                      searchable = FALSE, page_size = 6),
        p(class = "small-muted",
          sprintf(paste("Attributed on %s of %s common contracts (those with both a PD",
                        "curve and an EAD curve in each run); the rest is shown as not",
                        "attributable rather than spread across the factors.",
                        "Recomputation residual %s. Substitution order: horizon, EAD, PD, LGD."),
                  .an_money(fa$covered), .an_money(fa$contracts),
                  format(round(fa$residual, 2), big.mark = ","))))
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
      h <- pd_distribution(dat_rt()); if (is.null(h)) return(.an_na())
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

    output$c_pdrating <- renderUI({
      p <- pd_by_rating(dat_rt()); if (is.null(p)) return(.an_na())
      # order along the scale's own hierarchy, not alphabetically
      lv <- .rt_levels()
      if (!is.null(lv)) {
        p <- an_order_by_rating(p, "group", lv)
        if (nrow(p) == 0) return(.an_na("No contracts on this rating scale."))
        p$group <- as.character(p$group)
      }
      if (!.an_echarts())
        return(qdb_reactable(data.frame(Rating = p$group, `PD %` = .an_pct0(100 * p$pd_w),
                 `ECL coverage %` = .an_pct0(p$coverage), check.names = FALSE), page_size = 12))
      p$pd_pct <- 100 * p$pd_w
      echarts4r::e_charts(p, group) |>
        echarts4r::e_bar(pd_pct, name = "Weighted PD %", bar_width = "45%",
                         itemStyle = list(color = .AN_COL$plum_l)) |>
        echarts4r::e_line(coverage, name = "ECL coverage %", symbolSize = 8,
                          lineStyle = list(width = 3),
                          itemStyle = list(color = .AN_COL$teal)) |>
        echarts4r::e_x_axis(axisLabel = list(rotate = 40, fontSize = 9)) |>
        echarts4r::e_tooltip(trigger = "axis") |> echarts4r::e_legend(bottom = 0) |>
        echarts4r::e_grid(left = 60, right = 25, top = 15, bottom = 80)
    })

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

    cfg_weights <- reactive({
      d <- .out_dir_for(input$run_a)
      cu <- if (is.null(d)) NULL else an_config_used(d)
      p <- if (!is.null(cu)) file.path(cu$config, "model_inputs.yml") else
           file.path(getOption("ifrs9.project_root", getwd()), "config", "model_inputs.yml")
      scenario_weights(p)
    })

    output$scen_origin <- renderUI({
      se <- scen_source(); if (is.null(se) || isFALSE(se$ok)) return(NULL)
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

    output$scen_sliders <- renderUI({
      e <- scen_ecl(); if (is.null(e)) return(NULL)
      w <- cfg_weights()
      def <- function(s) {
        if (!is.null(w) && s %in% w$scenario) {
          v <- w$internal[w$scenario == s][1]
          if (!is.na(v)) return(round(100 * v, 2))
        }
        round(100 / length(e), 2)
      }
      tagList(
        fluidRow(lapply(names(e), function(s)
          column(2, numericInput(ns(paste0("w_", make.names(s))), s,
                                 value = def(s), min = 0, max = 100, step = 0.5,
                                 width = "100%")))),
        div(style = "margin-bottom:8px",
          actionButton(ns("w_reset"), "Reset to config weights",
                       class = "btn-sm btn-outline-secondary", icon = icon("rotate-left"))))
    })

    observeEvent(input$w_reset, {
      e <- scen_ecl(); w <- cfg_weights()
      if (is.null(e)) return()
      for (s in names(e)) {
        v <- if (!is.null(w) && s %in% w$scenario) w$internal[w$scenario == s][1] else NA
        if (length(v) != 1) v <- NA
        updateNumericInput(session, paste0("w_", make.names(s)),
                           value = if (is.na(v)) round(100 / length(e), 2) else round(100 * v, 2))
      }
    })

    user_weights <- reactive({
      e <- scen_ecl(); if (is.null(e)) return(NULL)
      cw <- cfg_weights()
      # The weight inputs only exist once the reweighting tab has rendered its
      # sliders. Opening Sensitivity first would otherwise give every scenario a
      # weight of zero and show nothing. Fall back to the config weights, then
      # to an equal split, so the tab stands on its own.
      fallback <- function(s) {
        if (!is.null(cw) && s %in% cw$scenario) {
          v <- cw$internal[cw$scenario == s][1]
          if (length(v) == 1 && !is.na(v)) return(100 * v)
        }
        100 / length(e)
      }
      w <- vapply(names(e), function(s) {
        v <- suppressWarnings(as.numeric(input[[paste0("w_", make.names(s))]]))
        if (length(v) != 1 || is.na(v)) fallback(s) else v
      }, numeric(1))
      names(w) <- names(e)
      w
    })

    output$t_whatif <- renderUI({
      se <- scen_source()
      e <- scen_ecl(); w <- user_weights()
      if (is.null(e) || is.null(w)) return(NULL)
      r <- scenario_reweight(e, w)
      if (is.null(r)) return(div(class = "alert alert-warning",
        "Weights must sum to more than zero."))
      cw <- cfg_weights()
      base <- if (!is.null(cw)) {
        bw <- stats::setNames(cw$internal, cw$scenario)
        scenario_reweight(e, bw[!is.na(bw)])
      } else NULL
      raw_sum <- sum(w)
      diff <- if (!is.null(base)) r$total - base$total else NA_real_
      tagList(
        qdb_stats(list(
          list(k = "Provision at these weights", v = .an_money(r$total), tone = "accent"),
          list(k = "At config weights",
               v = if (is.null(base)) "\u2014" else .an_money(base$total), tone = ""),
          list(k = "Weighted (as reported)",
               v = if (is.null(se$weighted) || is.na(se$weighted)) "\u2014"
                   else .an_money(se$weighted), tone = ""),
          list(k = "Difference", v = if (is.na(diff)) "\u2014" else .an_money(diff),
               tone = if (!is.na(diff) && diff > 0) "err" else "ok"),
          list(k = "Weights entered", v = sprintf("%.2f%%", raw_sum),
               tone = if (abs(raw_sum - 100) > 0.01) "warn" else "ok"))),
        if (abs(raw_sum - 100) > 0.01)
          div(class = "alert alert-info",
              sprintf("Weights total %.2f%%; they have been normalised to 100%% for the calculation.",
                      raw_sum)) else NULL,
        qdb_reactable(
          data.frame(Scenario = names(r$normalised_weights),
                     `Weight used` = sprintf("%.2f%%", 100 * as.numeric(r$normalised_weights)),
                     `Scenario ECL` = .an_money(e[names(r$normalised_weights)]),
                     Contribution = .an_money(e[names(r$normalised_weights)] *
                                              as.numeric(r$normalised_weights)),
                     check.names = FALSE),
          searchable = FALSE, page_size = 8))
    })

    output$c_sens <- renderUI({
      e <- scen_ecl(); w <- user_weights()
      if (is.null(e) || is.null(w)) return(NULL)
      sv <- scenario_sensitivity(e, w); if (is.null(sv)) return(.an_na())
      if (!.an_echarts())
        return(qdb_reactable(data.frame(Scenario = sv$scenario,
                 Change = .an_money(sv$change)), searchable = FALSE))
      sv <- sv[order(sv$change), , drop = FALSE]
      cj <- htmlwidgets::JS(sprintf("function(p){var c=[%s]; return c[p.dataIndex];}",
        paste0("'", ifelse(sv$change >= 0, .AN_COL$err, .AN_COL$ok), "'", collapse = ",")))
      echarts4r::e_charts(sv, scenario) |>
        echarts4r::e_bar(change, legend = FALSE, bar_width = "55%",
                         itemStyle = list(color = cj)) |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_x_axis(axisLabel = list(formatter = .an_m_axis())) |>
        echarts4r::e_tooltip() |>
        echarts4r::e_grid(left = 150, right = 30, top = 10, bottom = 30)
    })

    output$t_sens <- reactable::renderReactable({
      e <- scen_ecl(); w <- user_weights()
      if (is.null(e) || is.null(w)) return(NULL)
      sv <- scenario_sensitivity(e, w); if (is.null(sv)) return(NULL)
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
      w <- cfg_weights()
      sc <- if (!is.null(w)) w$scenario else names(scen_ecl() %||% character(0))
      if (length(sc) == 0) return(.an_na("Scenario weights could not be read for this run."))
      def <- function(s2) {
        if (!is.null(w) && s2 %in% w$scenario) {
          v <- w$internal[w$scenario == s2][1]
          if (length(v) == 1 && !is.na(v)) return(round(100 * v, 2))
        }
        round(100 / length(sc), 2)
      }
      tagList(
        fluidRow(lapply(sc, function(s2)
          column(2, numericInput(ns(paste0("mevw_", make.names(s2))), s2,
                                 value = def(s2), min = 0, max = 100, step = 0.5,
                                 width = "100%")))),
        div(class = "small-muted", "Normalised to 100% before use."))
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
      wts <- NULL
      if (identical(wm, "custom")) {
        w <- cfg_weights()
        sc <- if (!is.null(w)) w$scenario else names(scen_ecl() %||% character(0))
        wts <- stats::setNames(vapply(sc, function(s2) {
          v <- suppressWarnings(as.numeric(input[[paste0("mevw_", make.names(s2))]]))
          if (length(v) != 1 || is.na(v)) 0 else v
        }, numeric(1)), sc)
      }
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
              hold = "Held at this run's weights, so the change shown is the PD effect alone.",
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
    output$rf_scope <- renderUI({
      d <- dat_a(); i <- inp_a(); if (is.null(d)) return(NULL)
      pf <- sort(unique(d$portfolio[nzchar(d$portfolio) & !is.na(d$portfolio)]))
      int <- internal_portfolios(i, d) %||% pf
      ext <- external_portfolios(i, d)
      tagList(
        selectInput(ns("rf_portfolios"), "Scope", choices = pf, selected = int,
                    multiple = TRUE, width = "100%"),
        if (length(ext) > 0) div(class = "small-muted",
          sprintf("%s excluded by default \u2014 externally rated, so they do not price against the internal PD curves.",
                  paste(ext, collapse = ", "))) else NULL)
    })

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
      keymap <- c(pd_multiplier = "PD multiplier", rating_notches = "Rating notches",
                  collateral_pct = "Collateral %", exposure_pct = "Exposure %",
                  lgd_base = "LGD base", lgd_floor = "LGD floor",
                  dpd_threshold = "DPD threshold", default_top_n = "Largest N default",
                  contagion = "Contagion", tasdeer_collective = "Tasdeer collective")
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

    # ---- tornado ---    # ---- tornado -----------------------------------------------------------
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
          x$value <- if (is.logical(x$value)) isTRUE(v) else as.numeric(v)
          x$label <- sprintf("%s = %s", sub(" .*$", "", x$label),
                             if (is.logical(x$value)) (if (x$value) "on" else "off")
                             else format(x$value))
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
    output$rv_scope <- renderUI({
      d <- dat_a(); i <- inp_a(); if (is.null(d)) return(NULL)
      pf <- sort(unique(d$portfolio[nzchar(d$portfolio) & !is.na(d$portfolio)]))
      int <- internal_portfolios(i, d) %||% pf
      ext <- external_portfolios(i, d)
      tagList(
        selectInput(ns("rv_portfolios"), "Scope", choices = pf, selected = int,
                    multiple = TRUE, width = "100%"),
        if (length(ext) > 0) div(class = "small-muted",
          sprintf("%s excluded by default \u2014 externally rated, so they do not price against the internal PD curves.",
                  paste(ext, collapse = ", "))) else NULL)
    })

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
            sprintf(" No single lever reaches +%.0f%% on its own. A combination would be needed \u2014 use a stress package.", tgt))
      tagList(lead,
        qdb_reactable(data.frame(
          Lever = r$lever,
          `Required level` = r$required,
          `Reaches target` = ifelse(r$found, "yes", "no"),
          `Provision increase` = sprintf("%+.1f%%", r$achieved_pct),
          `Provision` = .an_money(r$provision),
          check.names = FALSE), searchable = FALSE, page_size = 10))
    })

    # ---- stress packages ---------------------------------------------------
    stp_list <- reactiveVal(list(stress_default("Stress 1")))
    stp_next <- reactiveVal(2)

    observeEvent(input$stp_add, {
      n <- stp_next(); stp_list(c(stp_list(), list(stress_default(sprintf("Stress %d", n)))))
      stp_next(n + 1)
    })
    observeEvent(input$stp_remove_click, {
      nm <- input$stp_remove_click; l <- stp_list()
      if (length(l) > 1) stp_list(Filter(function(x) !identical(x$name, nm), l))
    })
    observeEvent(input$stp_load, {
      saved <- tryCatch(stress_read(), error = function(e) list())
      if (length(saved) == 0) {
        showNotification("No saved stress packages found.", type = "warning")
      } else {
        stp_list(saved)
        showNotification(sprintf("Loaded %d package(s).", length(saved)), type = "message")
      }
    })
    observeEvent(input$stp_save, {
      live <- lapply(seq_along(stp_list()), function(k) .stp_read(k, stp_list()[[k]]))
      stp_list(live)
      ok <- tryCatch({ stress_write(live); TRUE }, error = function(e) FALSE)
      showNotification(if (ok) sprintf("Saved %d package(s).", length(live))
                       else "Could not write config/stress_packages.yml.",
                       type = if (ok) "message" else "error")
    })

    # read one package's fields back off the screen
    .stp_read <- function(k, cur) {
      f <- function(nm, default) {
        v <- input[[paste0("stp_", nm, "_", k)]]
        if (is.null(v)) default else v
      }
      nnum <- function(nm, default) {
        v <- suppressWarnings(as.numeric(f(nm, default)))
        if (length(v) != 1 || is.na(v)) default else v
      }
      list(name = { x <- trimws(f("name", cur$name)); if (nzchar(x)) x else cur$name },
           portfolios = f("pf", cur$portfolios),
           dpd_threshold = nnum("thr", cur$dpd_threshold),
           contagion = isTRUE(f("contagion", cur$contagion)),
           tasdeer_collective = isTRUE(f("tasdeer", cur$tasdeer_collective)),
           watchlist_triggers = isTRUE(f("watch", cur$watchlist_triggers)),
           local_triggers = isTRUE(f("local", cur$local_triggers)),
           pd_multiplier = nnum("pd", cur$pd_multiplier),
           lgd_base = nnum("lgdb", cur$lgd_base),
           lgd_floor = nnum("lgdf", cur$lgd_floor),
           collateral_pct = nnum("coll", cur$collateral_pct),
           exposure_pct = nnum("exp", cur$exposure_pct),
           rating_notches = as.integer(nnum("notch", cur$rating_notches)),
           default_top_n = as.integer(nnum("topn", cur$default_top_n)))
    }

    output$stp_note <- renderUI({
      ext <- external_portfolios(inp_a(), dat_a())
      if (length(ext) == 0) return(NULL)
      div(class = "small-muted", style = "margin:2px 0 8px",
          sprintf("Scoped to the internally-rated portfolios by default. %s are externally rated and do not price against the internal PD curves, so a stress leaves them unchanged.",
                  paste(ext, collapse = ", ")))
    })

    output$stp_editor <- renderUI({
      d <- dat_a(); l <- stp_list()
      pf <- if (is.null(d)) character(0) else
        sort(unique(d$portfolio[nzchar(d$portfolio) & !is.na(d$portfolio)]))
      int_pf <- internal_portfolios(inp_a(), d) %||% pf
      ext_pf <- external_portfolios(inp_a(), d)
      tagList(lapply(seq_along(l), function(k) {
        cur <- isolate(.stp_read(k, l[[k]]))
        fid <- function(nm) ns(paste0("stp_", nm, "_", k))
        div(class = "qdb-card", style = "padding:12px;margin-bottom:8px",
          fluidRow(
            column(5, textInput(fid("name"), NULL, value = cur$name,
                                placeholder = "Package name", width = "100%")),
            column(5, selectInput(fid("pf"), NULL, choices = pf,
                                  selected = if (length(cur$portfolios) > 0) cur$portfolios
                                             else int_pf,
                                  multiple = TRUE, width = "100%")),
            column(2, div(style = "text-align:right",
              actionButton(ns("stp_remove_click"), "Remove", class = "btn-sm btn-outline-danger",
                onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                                  ns("stp_remove_click"), cur$name))))),
          fluidRow(
            column(2, numericInput(fid("pd"), "PD multiplier", value = cur$pd_multiplier,
                                   min = 0, max = 10, step = 0.05, width = "100%")),
            column(2, numericInput(fid("lgdb"), "LGD base", value = cur$lgd_base,
                                   min = 0, max = 1, step = 0.01, width = "100%")),
            column(2, numericInput(fid("lgdf"), "LGD floor", value = cur$lgd_floor,
                                   min = 0, max = 1, step = 0.05, width = "100%")),
            column(2, numericInput(fid("coll"), "Collateral %", value = cur$collateral_pct,
                                   min = 0, max = 200, step = 5, width = "100%")),
            column(2, numericInput(fid("exp"), "Exposure %", value = cur$exposure_pct,
                                   min = 0, max = 300, step = 5, width = "100%")),
            column(2, numericInput(fid("notch"), "Rating notches", value = cur$rating_notches,
                                   min = -10, max = 10, step = 1, width = "100%"))),
          fluidRow(
            column(2, numericInput(fid("topn"), "Default largest N", value = cur$default_top_n,
                                   min = 0, max = 100, step = 1, width = "100%")),
            column(2, numericInput(fid("thr"), "DPD threshold", value = cur$dpd_threshold,
                                   min = 0, max = 90, step = 5, width = "100%")),
            column(8, div(style = "margin-top:24px;display:flex;gap:18px;flex-wrap:wrap",
              checkboxInput(fid("contagion"), "Contagion", cur$contagion),
              checkboxInput(fid("tasdeer"), "Tasdeer collective", cur$tasdeer_collective),
              checkboxInput(fid("watch"), "Watchlist triggers", cur$watchlist_triggers),
              checkboxInput(fid("local"), "Local flags trigger", cur$local_triggers)))))
      }))
    })

    stp_result <- eventReactive(input$stp_run, {
      i <- inp_a(); d <- dat_a()
      if (is.null(i) || isFALSE(i$ok) || is.null(d)) return(NULL)
      live <- lapply(seq_along(stp_list()), function(k) .stp_read(k, stp_list()[[k]]))
      stp_list(live)
      od <- .out_dir_for(input$run_a)
      cmp <- stress_compare(i, d, od, live)
      out <- list(compare = cmp,
                  detail = lapply(live, function(sp) stress_apply(i, d, od, sp)),
                  specs = live)
      if (is.null(cmp)) attr(out, "failed") <- TRUE
      out
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    output$stp_result <- renderUI({
      i <- inp_a()
      if (is.null(i) || isFALSE(i$ok))
        return(.an_na("The run's engine inputs could not be read."))
      r <- stp_result()
      if (is.null(r))
        return(div(class = "alert alert-secondary", style = "margin-top:10px",
          icon("circle-info"), " Define one or more packages, then Run all."))
      if (isTRUE(attr(r, "failed")))
        return(.an_na(stress_diagnosis(inp_a(), dat_a(), .out_dir_for(input$run_a))))
      bad <- Filter(function(x) stress_failed(x), r$detail %||% list())
      if (length(bad) > 0) return(.an_na(stress_error_message(bad[[1]])))
      if (isTRUE(attr(r, "failed")))
        return(.an_na(stress_diagnosis(inp_a(), dat_a(), .out_dir_for(input$run_a))))
      cmp <- r$compare
      if (is.null(cmp))
        return(.an_na(stress_diagnosis(inp_a(), dat_a(), .out_dir_for(input$run_a))))
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
          echarts4r::e_grid(left = 150, right = 30, top = 10, bottom = 30)
      } else NULL
      detail <- lapply(seq_along(r$detail), function(k) {
        x <- r$detail[[k]]; sp <- r$specs[[k]]
        if (is.null(x)) return(NULL)
        tagList(hr(), h5(sp$name %||% "(unnamed)"),
          if (length(x$notes)) p(class = "small-muted", paste(x$notes, collapse = "; ")) else NULL,
          qdb_stats(list(
            list(k = "Provision", v = .an_money(x$after), tone = "accent"),
            list(k = "Change", v = .an_money(x$delta),
                 tone = if (x$delta > 0) "err" else if (x$delta < 0) "ok" else ""),
            list(k = "% change", v = sprintf("%+.2f%%", 100 * x$delta / max(x$before, 1)),
                 tone = if (x$delta > 0) "err" else "ok"),
            list(k = "Contracts re-staged", v = .an_money(x$moved), tone = "warn"))),
          if (!is.null(x$by_portfolio)) qdb_reactable(data.frame(
            Portfolio = x$by_portfolio$portfolio, Moved = x$by_portfolio$moved,
            `ECL before` = .an_money(x$by_portfolio$before),
            `ECL after` = .an_money(x$by_portfolio$after),
            Change = .an_money(x$by_portfolio$change), check.names = FALSE),
            searchable = FALSE, page_size = 6) else NULL,
          # Stage moves stay as a summary: it is an aggregate BY MOVE, which the
          # customer table below does not give. No drill-down here, because that
          # would repeat the same customers a third time.
          if (!is.null(x$migration)) tagList(
            h6("Stage moves"),
            qdb_reactable(data.frame(Move = x$migration$move,
              Contracts = x$migration$contracts, Customers = x$migration$customers,
              Exposure = .an_money(x$migration$exposure),
              `ECL change` = .an_money(x$migration$ecl_change), check.names = FALSE),
              searchable = FALSE, page_size = 6)) else NULL,
          # ONE customer table. Previously "forced to default", "stage moves"
          # drill-down and "largest movements" all listed the same names under
          # three headings.
          # When "default largest N" was used, show exactly those N with their
          # outcome; otherwise show whoever the package moved.
          if (!is.null(x$defaulted_rows)) tagList(
            h6(sprintf("The %s largest customers selected", .an_money(nrow(x$defaulted_rows)))),
            .drill_customers(x$defaulted_rows, page = 12))
          else if (!is.null(x$movers)) {
            aff <- x$movers[x$movers$change != 0, , drop = FALSE]
            if (nrow(aff) == 0) NULL else tagList(
              h6(sprintf("Customers affected (%s)", .an_money(nrow(aff)))),
              .drill_customers(aff, page = 12))
          } else NULL)
      })
      tagList(
        h5("Packages compared"), chart,
        qdb_reactable(data.frame(Package = cmp$name,
          `Provision before` = .an_money(cmp$before),
          `Provision after` = .an_money(cmp$after),
          Change = .an_money(cmp$delta),
          `% change` = sprintf("%+.2f%%", cmp$pct),
          `Re-staged` = cmp$moved, check.names = FALSE),
          searchable = FALSE, page_size = 10),
        detail)
    })

    # ---- staging policy stress ---------------------------------------------
    output$sp_scope <- renderUI({
      d <- dat_a(); i <- inp_a(); if (is.null(d)) return(NULL)
      pf <- sort(unique(d$portfolio[nzchar(d$portfolio) & !is.na(d$portfolio)]))
      int <- internal_portfolios(i, d) %||% pf
      ext <- external_portfolios(i, d)
      tagList(
        selectInput(ns("sp_portfolios"), "Scope", choices = pf, selected = int,
                    multiple = TRUE, width = "100%"),
        if (length(ext) > 0) div(class = "small-muted",
          sprintf("%s excluded by default \u2014 externally rated, so they do not price against the internal PD curves.",
                  paste(ext, collapse = ", "))) else NULL)
    })

    sp_args <- reactive(list(
      dpd_threshold = { v <- suppressWarnings(as.numeric(input$sp_thr))
                        if (length(v) != 1 || is.na(v)) 60 else v },
      contagion = isTRUE(input$sp_contagion),
      tasdeer_collective = isTRUE(input$sp_tasdeer),
      watchlist_triggers = isTRUE(input$sp_watch),
      local_triggers = isTRUE(input$sp_local),
      portfolios = input$sp_portfolios))

    sp_result <- eventReactive(input$sp_apply, {
      i <- inp_a(); d <- dat_a()
      if (is.null(i) || isFALSE(i$ok) || is.null(d)) return(NULL)
      a <- sp_args()
      r <- do.call(staging_policy_stress, c(list(inputs = i, report = d,
                                                 out_dir = .out_dir_for(input$run_a)), a))
      if (is.null(r)) { r <- list(); attr(r, "failed") <- TRUE }
      r
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    output$sp_result <- renderUI({
      i <- inp_a()
      if (is.null(i) || isFALSE(i$ok))
        return(.an_na("The run's engine inputs could not be read."))
      r <- sp_result()
      if (is.null(r))
        return(div(class = "alert alert-secondary", style = "margin-top:10px",
          icon("circle-info"), " Set the policy above, then Apply."))
      if (stress_failed(r)) return(.an_na(stress_error_message(r)))
      if (isTRUE(attr(r, "failed")) || is.null(r$before))
        return(.an_na(stress_diagnosis(inp_a(), dat_a(), .out_dir_for(input$run_a))))
      pc <- 100 * r$delta / max(r$before, 1)
      st <- r$stages
      tabs <- if (!is.null(st)) qdb_reactable(data.frame(
          Stage = st$stage,
          `Contracts before` = st$contracts_before, `Contracts after` = st$contracts_after,
          `ECL before` = .an_money(st$ecl_before), `ECL after` = .an_money(st$ecl_after),
          check.names = FALSE), searchable = FALSE, page_size = 5) else NULL
      mig <- if (!is.null(r$migration)) tagList(hr(), h5("Contracts that changed stage"),
        qdb_reactable(data.frame(Move = r$migration$move,
          Contracts = r$migration$contracts, Customers = r$migration$customers,
          Exposure = .an_money(r$migration$exposure),
          `ECL change` = .an_money(r$migration$ecl_change), check.names = FALSE),
          searchable = FALSE, page_size = 8)) else NULL
      bypf <- if (!is.null(r$by_portfolio)) tagList(hr(), h5("Effect by portfolio"),
        qdb_reactable(data.frame(Portfolio = r$by_portfolio$portfolio,
          Contracts = r$by_portfolio$contracts, Moved = r$by_portfolio$moved,
          `ECL before` = .an_money(r$by_portfolio$before),
          `ECL after` = .an_money(r$by_portfolio$after),
          Change = .an_money(r$by_portfolio$change), check.names = FALSE),
          searchable = FALSE, page_size = 8)) else NULL
      movers <- if (!is.null(r$movers)) {
        aff <- r$movers[r$movers$change != 0, , drop = FALSE]
        if (nrow(aff) == 0) NULL else tagList(hr(),
          h5(sprintf("Customers affected (%s)", .an_money(nrow(aff)))),
          .drill_customers(aff, page = 12))
      } else NULL
      tagList(
        qdb_stats(list(
          list(k = "Provision now", v = .an_money(r$before), tone = ""),
          list(k = "Under this policy", v = .an_money(r$after), tone = "accent"),
          list(k = "Change", v = .an_money(r$delta),
               tone = if (r$delta > 0) "err" else if (r$delta < 0) "ok" else ""),
          list(k = "% change", v = sprintf("%+.2f%%", pc),
               tone = if (r$delta > 0) "err" else if (r$delta < 0) "ok" else ""),
          list(k = "Contracts re-staged", v = .an_money(r$moved), tone = "warn"),
          list(k = "Customers affected", v = .an_money(r$moved_customers), tone = ""))),
        tabs, mig, bypf, movers)
    })

    output$sp_sweep <- renderUI({
      i <- inp_a(); d <- dat_a()
      if (is.null(i) || isFALSE(i$ok) || is.null(d)) return(NULL)
      if (is.null(sp_result())) return(NULL)
      a <- sp_args(); a$dpd_threshold <- NULL
      sw <- do.call(staging_threshold_sweep,
                    c(list(inputs = i, report = d, out_dir = .out_dir_for(input$run_a)), a))
      if (is.null(sw)) return(NULL)
      tbl <- qdb_reactable(data.frame(
        `DPD threshold` = sw$threshold, `Provision` = .an_money(sw$ecl),
        `Re-staged` = sw$moved,
        `vs 60` = if ("vs_60" %in% names(sw)) .an_money(sw$vs_60) else "\u2014",
        check.names = FALSE), searchable = FALSE, page_size = 8)
      if (!.an_echarts()) return(tbl)
      sw$threshold <- as.character(sw$threshold)
      tagList(
        echarts4r::e_charts(sw, threshold) |>
          echarts4r::e_line(ecl, legend = FALSE, symbolSize = 9,
                            lineStyle = list(width = 3),
                            itemStyle = list(color = .AN_COL$plum)) |>
          echarts4r::e_x_axis(name = "DPD threshold", nameLocation = "middle", nameGap = 28) |>
          echarts4r::e_y_axis(axisLabel = list(formatter = .an_m_axis())) |>
          echarts4r::e_tooltip(trigger = "axis") |>
          echarts4r::e_grid(left = 80, right = 25, top = 15, bottom = 55),
        tbl)
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

    # Generic contract-level drill (walk steps, data-quality findings).
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

    # ---- what-if (rules) ----------------------------------------------------
    wi_rules <- reactiveVal(list(whatif_default_rule(1)))
    wi_next_id <- reactiveVal(2)

    observeEvent(input$wi_add_rule, {
      rs <- wi_rules(); nid <- wi_next_id()
      wi_rules(c(rs, list(whatif_default_rule(nid))))
      wi_next_id(nid + 1)
    })
    observeEvent(input$wi_remove_rule_click, {
      rid <- input$wi_remove_rule_click
      rs <- wi_rules()
      if (length(rs) > 1) wi_rules(Filter(function(r) r$id != rid, rs))
    })

    # Read a rule's current field values back from its own inputs, so the
    # rules list always reflects what is on screen (and a rebuilt UI - e.g.
    # after Add/Remove - keeps every other rule's typed values).
    .wi_read_rule <- function(r) {
      fid <- function(f) paste0("wi_", f, "_", r$id)
      g <- function(f, default) { v <- input[[fid(f)]]; if (is.null(v)) default else v }
      list(id = r$id,
           label = { lb <- trimws(g("label", r$label)); if (nzchar(lb)) lb else r$label },
           customer_ids = g("cust", r$customer_ids),
           portfolios = g("pf", r$portfolios),
           stages = suppressWarnings(as.numeric(g("stages", r$stages))),
           stage_to = g("stage_to", r$stage_to),
           notches = suppressWarnings(as.integer(g("notches", r$notches) %||% 0)),
           collateral_pct = suppressWarnings(as.numeric(g("coll", r$collateral_pct) %||% 100)),
           lgd_override = { v <- suppressWarnings(as.numeric(g("lgd", NA))); if (length(v) == 0) NA_real_ else v },
           exposure_pct = suppressWarnings(as.numeric(g("exp", r$exposure_pct) %||% 100)),
           pd_scenario = g("scen", r$pd_scenario),
           maturity_years = { v <- suppressWarnings(as.numeric(g("mat", r$maturity_years)))
                              if (length(v) != 1 || is.na(v)) 0 else v })
    }

    output$wi_rules_ui <- renderUI({
      d <- dat_a(); rules <- wi_rules()
      pf_choices <- if (!is.null(d)) sort(unique(d$portfolio[nzchar(d$portfolio)])) else character(0)
      st_choices <- if (!is.null(d)) sort(unique(d$stage[!is.na(d$stage)])) else numeric(0)
      scen_choices <- c("Weighted (no change)" = "", names(scen_ecl() %||% character(0)))
      tagList(lapply(rules, function(r) {
        cur <- isolate(.wi_read_rule(r))     # keep whatever is on screen right now
        fid <- function(f) ns(paste0("wi_", f, "_", r$id))
        div(class = "qdb-card", style = "padding:12px;margin-bottom:8px",
          fluidRow(
            column(6, textInput(fid("label"), NULL, value = cur$label,
                                placeholder = "Rule name", width = "100%")),
            column(6, div(style = "text-align:right",
              actionButton(ns("wi_remove_rule_click"), "Remove", class = "btn-sm btn-outline-danger",
                onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                                  ns("wi_remove_rule_click"), r$id))))),
          fluidRow(
            column(5, textInput(fid("cust"), "Applies to \u2014 customer ids",
                   value = cur$customer_ids,
                   placeholder = "comma separated, e.g. 100195, 100024", width = "100%")),
            column(4, selectInput(fid("pf"), "or portfolios", choices = pf_choices,
                   selected = cur$portfolios, multiple = TRUE, width = "100%")),
            column(3, selectInput(fid("stages"), "and stages", choices = st_choices,
                   selected = cur$stages, multiple = TRUE, width = "100%"))),
          uiOutput(ns(paste0("wi_info_", r$id))),
          hr(style = "margin:8px 0"),
          fluidRow(
            column(2, selectInput(fid("stage_to"), "Move to stage",
              choices = c("Unchanged" = "", "1" = "1", "2" = "2", "3" = "3"),
              selected = cur$stage_to, width = "100%")),
            column(2, numericInput(fid("notches"), "Rating notches",
                   value = cur$notches, min = -10, max = 10, step = 1, width = "100%")),
            column(2, numericInput(fid("coll"), "Collateral %",
                   value = cur$collateral_pct, min = 0, max = 200, step = 5, width = "100%")),
            column(2, numericInput(fid("lgd"), "or set LGD",
                   value = cur$lgd_override, min = 0, max = 1, step = 0.01, width = "100%")),
            column(2, numericInput(fid("exp"), "Exposure %",
                   value = cur$exposure_pct, min = 0, max = 300, step = 5, width = "100%")),
            column(2, selectInput(fid("scen"), "PD scenario",
                   choices = scen_choices, selected = cur$pd_scenario, width = "100%"))),
          fluidRow(
            column(3, numericInput(fid("mat"), "Extend maturity (years)",
                   value = cur$maturity_years, min = -10, max = 30, step = 0.25,
                   width = "100%"))))
      }))
    })

    # Customer details render as the ids are typed, not on Apply: the point is
    # to see the customer's current state BEFORE deciding what to change. One
    # output per rule, each depending only on its own id field.
    observe({
      rules <- wi_rules()
      lapply(rules, function(r) local({
        rid <- r$id
        output[[paste0("wi_info_", rid)]] <- renderUI({
          d <- dat_a()
          ids <- input[[paste0("wi_cust_", rid)]]
          if (is.null(d) || is.null(ids) || !nzchar(trimws(ids))) return(NULL)
          lk <- customer_lookup(d, ids)
          if (is.null(lk)) return(NULL)
          tagList(
            if (!is.null(lk$found)) qdb_reactable(data.frame(
                Customer = lk$found$customer, Portfolios = lk$found$portfolios,
                Rating = lk$found$rating, Stage = lk$found$stage,
                Contracts = lk$found$contracts,
                Exposure = .an_money(lk$found$exposure),
                ECL = .an_money(lk$found$ecl),
                `ECL coverage %` = .an_pct0(lk$found$coverage),
                LGD = ifelse(is.na(lk$found$lgd), "\u2014",
                             formatC(lk$found$lgd, format = "f", digits = 3)),
                check.names = FALSE), searchable = FALSE, page_size = 5) else NULL,
            if (length(lk$missing) > 0)
              div(class = "small-muted", style = "color:#dc2626",
                  sprintf("Not found: %s", paste(lk$missing, collapse = ", ")))
            else NULL)
        })
      }))
    })

    wi_result <- eventReactive(input$wi_apply, {
      i <- inp_a(); d <- dat_a(); rules <- wi_rules()
      if (is.null(i) || isFALSE(i$ok) || is.null(d) || length(rules) == 0) return(NULL)
      live <- lapply(rules, .wi_read_rule)
      wi_rules(live)   # persist what was actually applied
      out_dir <- .out_dir_for(input$run_a)
      whatif_reprice_rules(i, d, live, out_dir = out_dir)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    output$wi_result <- renderUI({
      i <- inp_a()
      if (is.null(i) || isFALSE(i$ok))
        return(.an_na("The run's engine inputs could not be read, so the book cannot be repriced."))
      r <- wi_result()
      if (is.null(r))
        return(div(class = "alert alert-secondary", style = "margin-top:10px", icon("circle-info"),
          " Set up rules above, then Apply."))
      pc <- 100 * r$delta / max(r$baseline, 1)
      tiles <- qdb_stats(list(
        list(k = "Provision now", v = .an_money(r$baseline), tone = ""),
        list(k = "Under this what-if", v = .an_money(r$whatif), tone = "accent"),
        list(k = "Change", v = .an_money(r$delta),
             tone = if (r$delta > 0) "err" else if (r$delta < 0) "ok" else ""),
        list(k = "% change", v = sprintf("%+.2f%%", pc),
             tone = if (r$delta > 0) "err" else if (r$delta < 0) "ok" else ""),
        list(k = "Customers changed", v = .an_money(r$customers_affected), tone = "")))

      conflicts <- if (!is.null(r$conflicts) && nrow(r$conflicts) > 0)
        tagList(div(class = "alert alert-danger", style = "margin-top:10px",
          icon("triangle-exclamation"),
          sprintf(" %s contract(s) matched by more than one rule and were skipped.",
                  nrow(r$conflicts))),
          qdb_reactable(r$conflicts, searchable = TRUE, page_size = 6)) else NULL

      unpriced <- if (!is.null(r$unpriced) && nrow(r$unpriced) > 0)
        tagList(div(class = "alert alert-info", style = "margin-top:10px",
          icon("circle-info"),
          sprintf(" %s selected contract(s) could not be priced and are excluded.",
                  format(sum(r$unpriced$contracts), big.mark = ","))),
          qdb_reactable(data.frame(Portfolio = r$unpriced$portfolio, Reason = r$unpriced$reason,
                        Contracts = r$unpriced$contracts, Exposure = .an_money(r$unpriced$exposure)),
                        searchable = FALSE, page_size = 6)) else NULL

      by_cust <- if (!is.null(r$by_customer) && nrow(r$by_customer) > 0) {
        bc <- utils::head(r$by_customer, 60)
        # An arrow only where something actually moved; otherwise the single
        # value, so an unchanged field reads "0.45", not "0.45 -> 0.45".
        arrow <- function(a, b) {
          if (length(a) == 0) a <- NA; if (length(b) == 0) b <- NA
          if (is.na(a) && is.na(b)) return("\u2014")
          if (is.na(b) || identical(as.character(a), as.character(b))) return(as.character(a))
          sprintf("%s \u2192 %s", a, b)
        }
        arrow_num <- function(a, b, digits = 3, tol = 1e-9) {
          if (length(a) == 0) a <- NA_real_; if (length(b) == 0) b <- NA_real_
          if (is.na(a) && is.na(b)) return("\u2014")
          if (is.na(b) || (!is.na(a) && abs(a - b) < tol))
            return(formatC(a, format = "f", digits = digits))
          sprintf("%s \u2192 %s", formatC(a, format = "f", digits = digits),
                  formatC(b, format = "f", digits = digits))
        }
        df <- data.frame(Customer = bc$group, Rule = bc$rule,
          Facilities = ifelse(bc$changed == bc$contracts,
                              as.character(bc$contracts),
                              sprintf("%d of %d", bc$changed, bc$contracts)),
          Rating = mapply(arrow, bc$rating_before, bc$rating_after),
          Stage = mapply(arrow, bc$stage_before, bc$stage_after),
          LGD = if (is.null(bc$lgd_before) || is.null(bc$lgd_after))
                  rep("\u2014", nrow(bc))
                else mapply(arrow_num, bc$lgd_before, bc$lgd_after),
          Exposure = bc$exposure, `ECL before` = bc$before, `ECL after` = bc$after,
          `ECL change` = bc$change, check.names = FALSE)
        tagList(hr(), h5("Effect by customer"),
          NULL,
          reactable::reactable(df, class = "qdb-rt", compact = TRUE, defaultPageSize = 15,
            searchable = TRUE, columns = list(
            Exposure = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
            `ECL before` = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
            `ECL after` = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
            `ECL change` = reactable::colDef(align = "right", cell = function(v) .an_money(v),
              style = function(v) list(color = if (v >= 0) .AN_COL$err else .AN_COL$ok,
                                       fontWeight = "600")))))
      } else NULL

      by_seg <- if (!is.null(r$by_segment) && nrow(r$by_segment) > 0) {
        bs <- r$by_segment[r$by_segment$change != 0, , drop = FALSE]
        if (nrow(bs) == 0) NULL else {
          # Before vs after per portfolio: the table already gives the change,
          # so the chart shows the two levels side by side instead of repeating it.
          chart <- if (.an_echarts()) {
            x <- bs[order(-abs(bs$change)), , drop = FALSE]
            echarts4r::e_charts(x, group) |>
              echarts4r::e_bar(before, name = "Before", bar_width = "35%",
                               itemStyle = list(color = .AN_COL$grey)) |>
              echarts4r::e_bar(after, name = "After", bar_width = "35%",
                               itemStyle = list(color = .AN_COL$plum)) |>
              echarts4r::e_y_axis(axisLabel = list(formatter = .an_m_axis())) |>
              echarts4r::e_x_axis(axisLabel = list(rotate = 20, fontSize = 10)) |>
              echarts4r::e_tooltip(trigger = "axis") |> echarts4r::e_legend(bottom = 0) |>
              echarts4r::e_grid(left = 80, right = 25, top = 15, bottom = 60)
          } else NULL
          tagList(hr(), h5("Effect by portfolio"),
            NULL,
            chart,
            qdb_reactable(data.frame(Portfolio = bs$group, Contracts = bs$contracts,
              Before = .an_money(bs$before), After = .an_money(bs$after),
              Change = .an_money(bs$change), check.names = FALSE), searchable = FALSE))
        }
      } else NULL

      tagList(tiles,
        NULL,
        conflicts, unpriced, by_cust, by_seg)
    })

    # ---- new comparison tables ---------------------------------------------
    output$t_seg <- reactable::renderReactable({
      a <- dat_a(); b <- dat_b(); if (is.null(a) || is.null(b)) return(NULL)
      by <- input$seg_by %||% "portfolio"
      pa <- run_profile(a, by); pb <- run_profile(b, by)
      if (is.null(pa) || is.null(pb)) return(NULL)
      m <- merge(pa, pb, by = "group", all = TRUE, suffixes = c("_a", "_b"))
      for (cc in c("exposure_a","exposure_b","ecl_a","ecl_b","contracts_a","contracts_b"))
        m[[cc]][is.na(m[[cc]])] <- 0
      m$d_ecl <- m$ecl_b - m$ecl_a
      m$d_exp <- m$exposure_b - m$exposure_a
      m <- m[order(-abs(m$d_ecl)), , drop = FALSE]
      df <- data.frame(Group = m$group,
                       `Exposure before` = m$exposure_a, `Exposure after` = m$exposure_b,
                       `Exposure change` = m$d_exp,
                       `Coverage before` = m$coverage_a, `Coverage after` = m$coverage_b,
                       `ECL change` = m$d_ecl, check.names = FALSE)
      .rt(df, defaultPageSize = 12, columns = list(
        `Exposure before` = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        `Exposure after` = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        `Exposure change` = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        `Coverage before` = reactable::colDef(align = "right", cell = function(v) .an_pct0(v)),
        `Coverage after` = reactable::colDef(align = "right", cell = function(v) .an_pct0(v)),
        `ECL change` = reactable::colDef(align = "right", cell = function(v) .an_money(v),
          style = function(v) list(color = if (v >= 0) .AN_COL$err else .AN_COL$ok,
                                   fontWeight = "600"))))
    })

    output$t_stagemove <- reactable::renderReactable({
      a <- dat_a(); b <- dat_b(); if (is.null(a) || is.null(b)) return(NULL)
      cm <- customer_stage_migration(a, b); if (is.null(cm)) return(NULL)
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

    output$t_trigdelta <- reactable::renderReactable({
      a <- dat_a(); b <- dat_b(); if (is.null(a) || is.null(b)) return(NULL)
      thr <- .thr()
      ta <- stage2_triggers(a, thr); tb <- stage2_triggers(b, thr)
      if (is.null(ta) || is.null(tb)) return(NULL)
      ta <- ta[ta$basis == "any", , drop = FALSE]
      tb <- tb[tb$basis == "any", , drop = FALSE]
      m <- merge(ta[, c("trigger","customers","ecl")],
                 tb[, c("trigger","customers","ecl")],
                 by = "trigger", all = TRUE, suffixes = c("_a","_b"))
      for (cc in c("customers_a","customers_b","ecl_a","ecl_b")) m[[cc]][is.na(m[[cc]])] <- 0
      df <- data.frame(Trigger = m$trigger,
                       `Customers before` = m$customers_a, `Customers after` = m$customers_b,
                       Change = m$customers_b - m$customers_a,
                       `ECL after` = m$ecl_b, check.names = FALSE)
      .rt(df, sortable = FALSE, pagination = FALSE, columns = list(
        Change = reactable::colDef(align = "right",
          style = function(v) list(color = if (v >= 0) .AN_COL$err else .AN_COL$ok,
                                   fontWeight = "600")),
        `ECL after` = reactable::colDef(align = "right", cell = function(v) .an_money(v))))
    })

    # ---- rating scale -------------------------------------------------------
    output$pick_rt <- renderUI({
      i <- inp_a(); ch <- an_rating_types(i)
      if (is.null(ch) || length(ch) == 0) return(NULL)
      selectInput(ns("rating_type"), "Rating scale", choices = ch, width = "100%")
    })
    .rt_sel <- reactive({ input$rating_type %||% "1" })
    # report restricted to the portfolios that use the selected scale
    dat_rt <- reactive({
      d <- dat_a(); i <- inp_a()
      if (is.null(d) || is.null(i) || isFALSE(i$ok)) return(d)
      an_filter_rating_type(d, i, .rt_sel())
    })
    .rt_levels <- reactive({ an_rating_levels(inp_a(), .rt_sel()) })

    # ---- staging -----------------------------------------------------------
    # Default the Stage 2 DPD threshold from the run's own static reference
    # (staging_thresholds.csv) rather than assuming 60.
    .thr_default <- reactive({
      d <- .out_dir_for(input$run_a)
      if (is.null(d)) return(60)
      staging_threshold(d, 60)
    })
    observeEvent(.thr_default(), {
      updateNumericInput(session, "dpd_thr", value = .thr_default())
    }, ignoreInit = TRUE)
    .thr <- reactive({
      v <- suppressWarnings(as.numeric(input$dpd_thr))
      if (length(v) != 1 || is.na(v)) .thr_default() else v
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

    output$c_s2trig <- renderUI({
      t <- stage2_triggers(dat_a(), .thr()); if (is.null(t)) return(.an_na("No Stage 2 customers."))
      sole <- t[t$basis == "sole", , drop = FALSE]
      if (!.an_echarts())
        return(qdb_reactable(data.frame(Trigger = sole$trigger, Contracts = sole$contracts),
                             searchable = FALSE))
      sole <- sole[sole$contracts > 0, , drop = FALSE]
      echarts4r::e_charts(sole, trigger) |>
        echarts4r::e_pie(contracts, radius = c("42%", "68%"), legend = FALSE,
                         label = list(formatter = "{b}: {c}", fontSize = 10)) |>
        echarts4r::e_color(.AN_PAL) |> echarts4r::e_tooltip()
    })

    output$c_s2overlap <- renderUI({
      o <- stage2_trigger_overlap(dat_a(), .thr())
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
      s <- stage3_drivers(dat_a()); if (is.null(s)) return(NULL)
      df <- data.frame(Driver = s$driver, Customers = s$customers,
                       `% of Stage 3` = s$pct_customers, Exposure = s$exposure,
                       check.names = FALSE)
      .rt(df, sortable = FALSE, pagination = FALSE, columns = list(
        Exposure = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
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
    output$pick_pf <- renderUI({
      i <- inp_a(); if (is.null(i) || length(i$pd) == 0) return(NULL)
      pf <- unique(sub("\\|.*$", "", names(i$pd)))
      selectInput(ns("curve_pf"), "Portfolio",
                  choices = c("All" = "", stats::setNames(pf, pf)), width = "100%")
    })

    output$pick_buckets <- renderUI({
      i <- inp_a(); if (is.null(i) || isFALSE(i$ok) || length(i$pd) == 0) return(NULL)
      pf <- input$curve_pf %||% ""
      k <- names(i$pd)
      if (nzchar(pf)) k <- k[startsWith(k, paste0(pf, "|"))]
      if (length(k) == 0) return(NULL)
      b <- suppressWarnings(as.integer(sub("^.*\\|", "", k)))
      ord <- order(b); k <- k[ord]; b <- b[ord]
      sel <- k[unique(pmax(1, round(seq(1, length(k), length.out = min(5, length(k))))))]
      selectInput(ns("curve_buckets"), "Rating buckets", choices = stats::setNames(k, paste("Bucket", b)),
                  selected = sel, multiple = TRUE, width = "100%")
    })

    .curve_subset <- function(ts) {
      sel <- input$curve_buckets
      if (is.null(ts) || is.null(sel) || length(sel) == 0) return(ts)
      x <- ts[ts$curve %in% sel, , drop = FALSE]
      if (nrow(x) == 0) ts else x
    }

    output$c_pdterm <- renderUI({
      ts <- pd_term_structure(inp_a(), input$curve_pf %||% "")
      if (is.null(ts)) return(.an_na("StPD not found in this run's outputs."))
      if (identical(input$curve_view, "heat")) {
        h <- ts[ts$month %% 6 == 0, , drop = FALSE]
        h$bucket <- sub("^.*\\|", "", h$curve)
        if (!.an_echarts())
          return(qdb_reactable(utils::head(h, 60), searchable = FALSE, page_size = 12))
        return(echarts4r::e_charts(h, month) |>
          echarts4r::e_heatmap(bucket, cum_pd) |>
          echarts4r::e_visual_map(cum_pd,
            inRange = list(color = c("#e7f6f0", "#fdf3e3", "#fce9e9"))) |>
          echarts4r::e_tooltip(formatter = htmlwidgets::JS(
            "function(p){return 'month '+p.value[0]+', bucket '+p.value[1]+'<br/>cum PD <b>'+Number(p.value[2]).toFixed(2)+'%</b>';}")) |>
          echarts4r::e_x_axis(name = "Month") |> echarts4r::e_y_axis(name = "Bucket") |>
          echarts4r::e_grid(left = 70, right = 80, top = 15, bottom = 45))
      }
      ts <- .curve_subset(ts)
      if (!.an_echarts())
        return(qdb_reactable(utils::head(ts, 60), searchable = FALSE, page_size = 12))
      g <- echarts4r::e_charts(echarts4r::group_by(ts, curve), month) |>
        echarts4r::e_line(cum_pd, symbol = "none", lineStyle = list(width = 2)) |>
        echarts4r::e_x_axis(name = "Month", nameLocation = "middle", nameGap = 26) |>
        echarts4r::e_y_axis(name = "Cumulative PD %") |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_legend(type = "scroll", bottom = 0) |>
        echarts4r::e_color(.AN_PAL) |>
        echarts4r::e_grid(left = 70, right = 30, top = 15, bottom = 60)
      g
    })

    output$c_pdmarg <- renderUI({
      ts <- pd_term_structure(inp_a(), input$curve_pf %||% "", max_month = 60)
      if (is.null(ts)) return(.an_na())
      ts <- .curve_subset(ts)
      if (!.an_echarts())
        return(qdb_reactable(utils::head(ts, 60), searchable = FALSE, page_size = 12))
      echarts4r::e_charts(echarts4r::group_by(ts, curve), month) |>
        echarts4r::e_line(marginal_pd, symbol = "none", lineStyle = list(width = 2)) |>
        echarts4r::e_x_axis(name = "Month", nameLocation = "middle", nameGap = 26) |>
        echarts4r::e_y_axis(name = "Marginal PD %") |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_legend(type = "scroll", bottom = 0) |>
        echarts4r::e_color(.AN_PAL) |>
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

    output$t_walk <- reactable::renderReactable({
      w <- walk_r(); if (is.null(w)) return(NULL)
      s <- w$steps
      pct <- ifelse(s$kind == "delta", 100 * s$amount / max(w$opening, 1), NA_real_)
      df <- data.frame(Step = s$label, Amount = s$amount, `% of opening` = pct,
                       check.names = FALSE)
      det <- ecl_walk_detail(dat_a(), dat_b())
      .rt(df, sortable = FALSE, pagination = FALSE,
        details = if (is.null(det)) NULL else function(index) {
          lbl <- s$label[index]
          if (!lbl %in% names(det))
            return(div(class = "small-muted", style = "padding:8px 12px",
                       "Opening and closing are totals, not a set of contracts."))
          .drill_contracts(det[[lbl]])
        },
        columns = list(
        Step = reactable::colDef(minWidth = 160),
        Amount = reactable::colDef(align = "right", cell = function(v) .an_money(v),
          style = function(v, i) if (s$kind[i] == "total") list(fontWeight = "700")
                                 else list(color = if (v >= 0) .AN_COL$err else .AN_COL$ok)),
        `% of opening` = reactable::colDef(align = "right",
          cell = function(v) if (is.na(v)) "\u2014" else .an_pct(v))))
    })

    output$t_attr <- reactable::renderReactable({
      m <- movement_by(dat_a(), dat_b(), input$attr_by %||% "portfolio")
      if (is.null(m)) return(NULL)
      df <- data.frame(Group = m$group, Opening = m$ecl_prev, Closing = m$ecl_curr,
                       Change = m$change, `%` = m$pct, check.names = FALSE)
      .rt(df, defaultPageSize = 12, columns = list(
        Opening = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        Closing = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        Change = reactable::colDef(align = "right", cell = function(v) .an_money(v),
          style = function(v) list(color = if (v >= 0) .AN_COL$err else .AN_COL$ok,
                                   fontWeight = "600")),
        `%` = reactable::colDef(align = "right",
          cell = function(v) if (is.na(v)) "\u2014" else .an_pct(v))))
    })


    output$t_flow <- reactable::renderReactable({
      f <- flow_profile(dat_a(), dat_b()); if (is.null(f)) return(NULL)
      df <- data.frame(Flow = f$flow, Contracts = f$contracts, Exposure = f$exposure,
                       ECL = f$ecl, `ECL coverage %` = f$coverage, check.names = FALSE)
      .rt(df, sortable = FALSE, pagination = FALSE, columns = list(
        Exposure = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        ECL = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        `ECL coverage %` = reactable::colDef(align = "right", cell = function(v) .an_pct0(v))))
    })

    output$t_movers <- reactable::renderReactable({
      a <- dat_a(); b <- dat_b(); if (is.null(a) || is.null(b)) return(NULL)
      m <- merge(a[, c("contract", "customer", "portfolio", "ecl", "stage", "rating")],
                 b[, c("contract", "ecl", "stage", "rating")],
                 by = "contract", suffixes = c("_a", "_b"))
      if (nrow(m) == 0) return(NULL)
      m$change <- m$ecl_b - m$ecl_a
      m <- m[order(-abs(m$change)), , drop = FALSE]
      m <- utils::head(m, 25)
      arrow <- function(x, y) {
        x <- as.character(x); y <- as.character(y)
        ifelse(is.na(x) & is.na(y), "\u2014",
        ifelse(is.na(y) | x == y, x, paste(x, "\u2192", y)))
      }
      df <- data.frame(Contract = m$contract, Customer = m$customer,
                       Portfolio = m$portfolio,
                       Stage = arrow(m$stage_a, m$stage_b),
                       Rating = arrow(m$rating_a, m$rating_b),
                       Before = m$ecl_a, After = m$ecl_b, Change = m$change,
                       check.names = FALSE)
      .rt(df, defaultPageSize = 12, searchable = TRUE, columns = list(
        Before = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        After = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        Change = reactable::colDef(align = "right", cell = function(v) .an_money(v),
          style = function(v) list(color = if (v >= 0) .AN_COL$err else .AN_COL$ok,
                                   fontWeight = "600"))))
    })

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

    output$t_pd <- reactable::renderReactable({
      p <- pd_profile(dat_rt(), input$pd_by %||% "stage"); if (is.null(p)) return(NULL)
      if (identical(input$pd_by, "rating")) {
        lv <- .rt_levels()
        if (!is.null(lv)) { p <- an_order_by_rating(p, "group", lv); p$group <- as.character(p$group) }
      }
      df <- data.frame(Group = p$group, Contracts = p$contracts, Exposure = p$exposure,
                       `Weighted PD %` = 100 * p$pd_w, `Simple mean PD %` = 100 * p$pd_mean,
                       ECL = p$ecl, check.names = FALSE)
      .rt(df, defaultPageSize = 12, columns = list(
        Exposure = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        ECL = reactable::colDef(align = "right", cell = function(v) .an_money(v)),
        `Weighted PD %` = reactable::colDef(align = "right", cell = function(v) .an_pct0(v)),
        `Simple mean PD %` = reactable::colDef(align = "right", cell = function(v) .an_pct0(v))))
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
