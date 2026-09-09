# =============================================================================
# app/modules/mod_overlays.R
#
# ECL Overlays manager - a rule-builder page. One overlay ID is a BUNDLE of
# rules with its own approval state (draft -> pending -> approved/rejected).
#
# Each rule is a row: Method -> Level -> Target -> Value -> Reason. Rows are
# added with insertUI (so existing rows KEEP their values when you add another)
# and removed with removeUI. The whole bundle saves under one overlay ID; if the
# ID already exists you are asked whether to REPLACE it or APPEND the new rules.
# =============================================================================

.OVL_METHODS <- c("Uplift %" = "uplift_pct",
                  "Higher-of (floor % of exposure)" = "higher_of",
                  "Absolute add (QAR)" = "absolute_add")
.OVL_LEVEL_CHOICES <- c("Whole book" = "whole_book", "Stage" = "stage",
                        "Portfolio" = "portfolio", "Rating" = "rating",
                        "Flag" = "flag", "Sector" = "sector",
                        "Customer" = "customer", "Contract" = "contract")

# Build one rule row's UI (created ONCE by insertUI; never re-rendered, so its
# input values persist across add/remove of other rows). The Target field and
# the Value helper are the only reactive bits, and they react only to THIS row.
.ovl_rule_row_ui <- function(ns, rid) {
  m <- ns(paste0("m_", rid)); l <- ns(paste0("l_", rid))
  v <- ns(paste0("v_", rid)); cm <- ns(paste0("c_", rid))
  div(class = "rule-row", id = ns(paste0("row_", rid)),
    selectInput(m, NULL, choices = .OVL_METHODS, width = "100%"),
    selectInput(l, NULL, choices = .OVL_LEVEL_CHOICES, width = "100%"),
    uiOutput(ns(paste0("target_", rid))),
    div(numericInput(v, NULL, value = NA, width = "100%"),
        uiOutput(ns(paste0("vhelp_", rid)))),
    textInput(cm, NULL, placeholder = "reason for this rule", width = "100%"),
    actionButton(ns(paste0("del_", rid)), NULL, icon = icon("trash"),
                 class = "btn-sm btn-outline-danger",
                 onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                                   ns("del_rule"), rid)))
}

mod_overlays_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(sprintf("
      #%s .rule-row{display:grid;grid-template-columns:1.4fr 1.2fr 1.8fr 1fr 2fr auto;
        gap:10px;align-items:start;padding:10px;border:1px solid #eef0f2;border-radius:10px;
        margin-bottom:8px;background:#fbfcfd}
      #%s .rule-row .form-group{margin-bottom:0}
      #%s .rule-row .shiny-input-container{width:100%%!important}
      #%s .vhelp{font-size:11px;color:#8a94a6;margin-top:3px;line-height:1.35}
      #%s .rh{display:grid;grid-template-columns:1.4fr 1.2fr 1.8fr 1fr 2fr auto;gap:10px;
        font-size:11px;font-weight:700;color:#6b7280;text-transform:uppercase;
        letter-spacing:.03em;padding:0 10px 4px}
      #%s .ovl-item{border:1px solid #e5e7eb;border-radius:12px;padding:12px 15px;margin-bottom:10px;background:#fff}
      #%s .ovl-item h5{margin:0;font-size:14.5px;font-weight:650;display:inline}
      #%s .stbadge{display:inline-block;padding:2px 10px;border-radius:11px;font-size:11px;font-weight:600;color:#fff;margin-left:8px}
      #%s .st-draft{background:#9ca3af}#%s .st-pending{background:#d97706}#%s .st-approved{background:#059669}#%s .st-rejected{background:#dc2626}
      #%s .ovl-rules{font-size:12.5px;color:#374151;margin-top:8px}
      #%s .ovl-rules li{margin-bottom:3px}
      #%s .ovl-total{font-variant-numeric:tabular-nums;display:flex;gap:22px;margin-bottom:10px;flex-wrap:wrap}
    ", ns(""),ns(""),ns(""),ns(""),ns(""),ns(""),ns(""),ns(""),ns(""),ns(""),ns(""),ns(""),ns(""),ns(""),ns("")))),

    fluidRow(column(12,
      h3("ECL overlays"),
      p(class="small-muted",
        "Build a bundle of post-model adjustment rules and save it under one overlay ID. ",
        "Each rule: choose a method, the level it applies at, the target, the value and a ",
        "reason. Overlays follow an approval flow (draft \u2192 pending \u2192 approved) like config ",
        "versions. Stage 3 is never touched, and a contract hit by two rules blocks the run."),
      div(class="alert alert-light", style="border:1px solid #e5e7eb;font-size:12.5px",
        strong("Value guide: "),
        "Uplift % \u2014 enter the percentage (e.g. ", strong("15"), " = +15% of model ECL).  ",
        "Higher-of \u2014 enter the floor as a percentage of exposure (e.g. ", strong("5"), " = 5% of exposure).  ",
        "Absolute add \u2014 enter the amount in QAR (e.g. ", strong("1,000,000"), "), spread across matched contracts by exposure."))),

    layout_columns(col_widths = c(7, 5),

      card(
        card_header(textOutput(ns("builder_title"), inline = TRUE)),
        fluidRow(
          column(6, textInput(ns("b_id"), "Overlay ID", placeholder="e.g. OV-2026Q2")),
          column(6, textInput(ns("b_owner"), "Owner", value="FRM"))),
        hr(style="margin:8px 0"),
        div(class="rh",
            span("Method"), span("Level"), span("Target"), span("Value"), span("Reason"), span("")),
        div(id = ns("rules_container")),
        actionButton(ns("add_rule"), "Add rule", icon=icon("plus"), class="btn-outline-primary btn-sm"),
        hr(),
        div(
          actionButton(ns("save_bundle"), "Save overlay", icon=icon("floppy-disk"), class="btn-primary"),
          actionButton(ns("clear_bundle"), "Clear", icon=icon("eraser"), class="btn-outline-secondary"),
          actionButton(ns("preview_bundle"), "Preview impact", icon=icon("flask"), class="btn-outline-primary")),
        uiOutput(ns("builder_status")),
        uiOutput(ns("preview_panel"))
      ),

      card(
        card_header("Saved overlays"),
        p(class="small-muted", "Click Edit to load a bundle back into the builder. Use the approval buttons to move it through draft \u2192 pending \u2192 approved."),
        uiOutput(ns("saved_list"))
      )
    )
  )
}

mod_overlays_server <- function(id, latest_report = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ovl_path <- reactive(file.path(getOption("ifrs9.project_root", getwd()), "config", "overlays.yml"))
    refresh <- reactiveVal(0)
    active_rids <- reactiveVal(integer(0))   # rule rows currently in the DOM
    next_id  <- reactiveVal(1L)
    editing  <- reactiveVal(NULL)

    bundles <- reactive({ refresh(); tryCatch(read_overlays_yaml(ovl_path()), error=function(e) list()) })
    output$builder_title <- renderText(if (is.null(editing())) "New overlay" else paste("Editing:", editing()))

    # ---- add / remove rule rows via insert/removeUI (values persist) ------
    # reactiveVal reads/writes are isolated so these helpers work whether called
    # from an observeEvent (reactive) or from session$onFlushed (non-reactive).
    add_row <- function(seed = NULL) {
      rid <- isolate(next_id()); isolate(next_id(rid + 1L))
      insertUI(selector = paste0("#", ns("rules_container")), where = "beforeEnd",
               ui = .ovl_rule_row_ui(ns, rid), immediate = TRUE)
      isolate(active_rids(c(active_rids(), rid)))

      # per-row: Target field reacts to this row's Level
      output[[paste0("target_", rid)]] <- renderUI({
        lvl <- input[[paste0("l_", rid)]] %||% "whole_book"
        tid <- ns(paste0("t_", rid))
        if (lvl == "whole_book")
          tags$div(class="small-muted", style="padding-top:7px", "applies to the whole book (all Stage 1 & 2 contracts)")
        else if (lvl == "stage")
          selectizeInput(tid, NULL, choices=c("1","2"), multiple=TRUE, width="100%",
                         options=list(placeholder="pick stage(s)"))
        else if (lvl == "portfolio")
          selectizeInput(tid, NULL, multiple=TRUE, width="100%",
                         choices=c("Business Finance","Off BS","Al Dhameen","Tasdeer","Investments","Banks and Fis"),
                         options=list(placeholder="pick portfolio(s)"))
        else if (lvl == "flag")
          selectizeInput(tid, NULL, multiple=TRUE, width="100%",
                         choices=c("watchlist","default","default_gcc","insolvency","local1","local2","local3","local4","local5","local6"),
                         options=list(placeholder="pick flag(s)"))
        else
          textInput(tid, NULL, placeholder=paste0("enter ", lvl, " id(s), comma-separated"), width="100%")
      })
      # per-row: Value helper reacts to this row's Method
      output[[paste0("vhelp_", rid)]] <- renderUI({
        mth <- input[[paste0("m_", rid)]] %||% "uplift_pct"
        txt <- switch(mth,
          uplift_pct   = "percent, e.g. 15 = +15%",
          higher_of    = "floor % of exposure, e.g. 5 = 5%",
          absolute_add = "QAR amount, e.g. 1000000")
        tags$div(class="vhelp", txt)
      })

      # seed values (used when editing)
      if (!is.null(seed)) {
        updateSelectInput(session, paste0("m_", rid), selected = seed$method)
        updateSelectInput(session, paste0("l_", rid), selected = seed$level)
        updateNumericInput(session, paste0("v_", rid),
          value = if (identical(seed$method,"absolute_add")) seed$value else seed$value*100)
        updateTextInput(session, paste0("c_", rid), value = seed$comment %||% "")
        if (!identical(seed$level,"whole_book")) {
          later::later(function() {
            if (seed$level %in% c("stage","portfolio","flag"))
              updateSelectizeInput(session, paste0("t_", rid), selected = trimws(strsplit(seed$target %||% "", ",")[[1]]))
            else
              updateTextInput(session, paste0("t_", rid), value = seed$target %||% "")
          }, 0.25)
        }
      }
      rid
    }

    remove_row <- function(rid) {
      removeUI(selector = paste0("#", ns(paste0("row_", rid))), immediate = TRUE)
      isolate(active_rids(setdiff(active_rids(), rid)))
    }

    # start with one empty row, once, after the session is ready
    session$onFlushed(function() {
      if (length(isolate(active_rids())) == 0) add_row()
    }, once = TRUE)

    observeEvent(input$add_rule, { add_row() })
    observeEvent(input$del_rule, {
      rid <- as.integer(input$del_rule)
      remove_row(rid)
      if (length(active_rids()) == 0) add_row()   # never leave zero rows
    })

    # ---- collect builder -> bundle ---------------------------------------
    collect_rules <- function() {
      rules <- list()
      for (rid in active_rids()) {
        method <- input[[paste0("m_", rid)]]; level <- input[[paste0("l_", rid)]]
        tgt <- input[[paste0("t_", rid)]]; val <- input[[paste0("v_", rid)]]
        cmt <- input[[paste0("c_", rid)]]
        if (is.null(method)) next
        target <- if (identical(level, "whole_book")) "" else (if (is.null(tgt)) "" else paste(tgt, collapse=","))
        raw <- if (is.null(val) || is.na(val)) NA_real_ else as.numeric(val)
        value <- if (identical(method, "absolute_add")) raw else raw/100
        rules[[length(rules)+1]] <- list(method=method, level=level %||% "whole_book",
          target=target, value=value, comment=trimws(cmt %||% ""))
      }
      rules
    }

    make_bundle <- function(rules, existing = NULL) {
      list(
        id = trimws(input$b_id %||% ""),
        owner = trimws(input$b_owner %||% "FRM"),
        status = "draft",                                  # new/edited => draft
        created_at = existing$created_at %||% format(Sys.Date()),
        transitions = {
          tr <- existing$transitions %||% list()
          reason <- if (is.null(existing)) "created" else "edited - re-approval required"
          tr[[length(tr)+1]] <- list(to="draft", by=Sys.info()[["user"]] %||% "unknown",
                                     at=format(Sys.time(),"%Y-%m-%dT%H:%M:%S"), reason=reason)
          tr
        },
        rules = rules)
    }

    # ---- save (with ID-exists handling) ----------------------------------
    do_save <- function(mode = c("new","replace","append")) {
      mode <- match.arg(mode)
      rules <- collect_rules()
      bid <- trimws(input$b_id %||% "")
      if (!nzchar(bid)) { showNotification("Overlay ID is required.", type="warning"); return() }
      if (length(rules) == 0) { showNotification("Add at least one rule.", type="warning"); return() }
      existing <- get_overlay(bid, ovl_path())
      if (mode == "append" && !is.null(existing))
        rules <- c(existing$rules %||% list(), rules)
      b <- make_bundle(rules, existing = if (mode=="new") NULL else existing)
      res <- tryCatch(upsert_overlay(b, ovl_path()), error=function(e) e)
      if (inherits(res, "error")) {
        output$builder_status <- renderUI(div(class="alert alert-danger", style="margin-top:10px", conditionMessage(res)))
      } else {
        showNotification(sprintf("Saved overlay '%s' (%d rule(s), status: draft).", b$id, length(rules)), type="message")
        output$builder_status <- renderUI(NULL)
        editing(NULL); reset_builder(); refresh(refresh()+1)
      }
    }

    observeEvent(input$save_bundle, {
      bid <- trimws(input$b_id %||% "")
      if (!nzchar(bid)) { showNotification("Overlay ID is required.", type="warning"); return() }
      existing <- get_overlay(bid, ovl_path())
      if (!is.null(existing) && is.null(editing())) {
        # ID already exists and we're not explicitly editing it -> ask
        showModal(modalDialog(
          title = paste0("Overlay '", bid, "' already exists"),
          sprintf("It has %d rule(s). Replace it with the rules in the builder, or append the builder's rules to it?",
                  length(existing$rules %||% list())),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("save_append"), "Append rules", class="btn-outline-primary"),
            actionButton(ns("save_replace"), "Replace", class="btn-danger")),
          easyClose = TRUE))
      } else {
        do_save(if (is.null(editing())) "new" else "replace")
      }
    })
    observeEvent(input$save_replace, { removeModal(); do_save("replace") })
    observeEvent(input$save_append,  { removeModal(); do_save("append") })

    observeEvent(input$clear_bundle, { editing(NULL); reset_builder() })
    reset_builder <- function() {
      updateTextInput(session, "b_id", value="")
      for (rid in active_rids()) remove_row(rid)
      add_row()
    }

    # ---- preview ----------------------------------------------------------
    observeEvent(input$preview_bundle, {
      rep <- latest_report()
      if (is.null(rep) || nrow(rep)==0) { output$preview_panel <- renderUI(div(class="alert alert-warning", style="margin-top:10px","No completed run to preview against.")); return() }
      b <- make_bundle(collect_rules())
      pr <- tryCatch(preview_overlays(rep, b), error=function(e) list(err=conditionMessage(e)))
      output$preview_panel <- renderUI(render_preview(pr))
    })
    # Small inline HTML table. render_preview() is called from inside a
    # renderUI, so it must return TAGS. Calling a Shiny render function
    # directly (renderTable(x)(), DT::renderDT(x)()) fails with
    # 'argument "name" is missing' because those return
    # function(shinysession, name, ...) - that was the Preview impact bug.
    .ovl_tbl <- function(df, max_rows = 10) {
      if (is.null(df) || nrow(df) == 0)
        return(tags$p(class = "small-muted", "(nothing to show)"))
      if (nrow(df) > max_rows) df <- df[seq_len(max_rows), , drop = FALSE]
      tags$div(style = "overflow-x:auto",
        tags$table(class = "narrow-table",
          tags$thead(tags$tr(lapply(colnames(df), tags$th))),
          tags$tbody(lapply(seq_len(nrow(df)), function(i)
            tags$tr(lapply(df[i, , drop = TRUE],
                           function(v) tags$td(as.character(v))))))))
    }

    render_preview <- function(pr) {
      if (!is.null(pr$err)) return(div(class="alert alert-warning", style="margin-top:10px", pr$err))
      fmt <- function(x) format(round(x), big.mark=",", scientific=FALSE)
      if (!is.null(pr$conflicts)) return(tagList(
        div(class="alert alert-danger", style="margin-top:10px", icon("triangle-exclamation"),
            sprintf(" Conflict: %d contract(s) matched by more than one rule - resolve before running.", nrow(pr$conflicts))),
        .ovl_tbl(pr$conflicts, 10)))
      tot <- pr$total
      s <- pr$summary
      tbl <- NULL
      if (!is.null(s) && nrow(s) > 0) {
        keep <- intersect(c("overlay_id","type","contracts","ecl_model","overlay_amount","ecl_final"),
                          colnames(s))
        s <- s[, keep, drop = FALSE]
        nice <- c(overlay_id="Rule", type="Method", contracts="Contracts",
                  ecl_model="Model", overlay_amount="Overlay", ecl_final="Final")
        colnames(s) <- unname(nice[colnames(s)])
        for (cc in intersect(c("Model","Overlay","Final"), colnames(s)))
          s[[cc]] <- format(round(suppressWarnings(as.numeric(s[[cc]]))),
                            big.mark=",", scientific=FALSE)
        tbl <- .ovl_tbl(s, 25)
      }
      tagList(hr(),
        div(class="ovl-total",
          div(strong("Model: "), fmt(tot$model)),
          div(strong("Overlay: "), span(style="color:#7c3aed", paste0("+", fmt(tot$overlay)))),
          div(strong("Final: "), span(style="color:#059669", fmt(tot$final)),
              span(class="small-muted", sprintf(" (+%.2f%%)", 100*tot$overlay/max(tot$model,1))))),
        tbl)
    }

    # ---- saved overlays list + approval ----------------------------------
    output$saved_list <- renderUI({
      bs <- bundles()
      if (length(bs)==0) return(div(class="small-muted", style="text-align:center;padding:30px", "No overlays yet."))
      tagList(lapply(bs, function(b) {
        st <- b$status %||% "draft"
        rules_html <- tags$ul(class="ovl-rules", lapply(b$rules %||% list(), function(r) {
          mlab <- names(.OVL_METHODS)[.OVL_METHODS==r$method]; if (length(mlab)==0) mlab <- r$method
          vtxt <- switch(r$method,
            uplift_pct=sprintf("+%.4g%%", r$value*100),
            higher_of=sprintf("floor %.4g%% of exp", r$value*100),
            absolute_add=sprintf("QAR %s", format(r$value, big.mark=",", scientific=FALSE)))
          tgt <- if (identical(r$level,"whole_book")) "whole book" else sprintf("%s: %s", r$level, r$target)
          tags$li(strong(mlab), " \u2014 ", vtxt, " on ", tgt,
                  if (nzchar(r$comment %||% "")) span(class="small-muted", paste0("  (", r$comment, ")")))
        }))
        approve_btns <- div(style="margin-top:8px",
          if (st %in% c("draft","rejected"))
            actionButton(ns(paste0("submit_", b$id)), "Submit for approval", class="btn-sm btn-outline-warning",
              onclick=sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("submit_click"), b$id)),
          if (st == "pending") tagList(
            actionButton(ns(paste0("appr_", b$id)), "Approve", class="btn-sm btn-success",
              onclick=sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("approve_click"), b$id)),
            actionButton(ns(paste0("rej_", b$id)), "Reject", class="btn-sm btn-outline-danger",
              onclick=sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("reject_click"), b$id))),
          actionButton(ns(paste0("ed_", b$id)), "Edit", class="btn-sm btn-link",
              onclick=sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("edit_click"), b$id)),
          actionButton(ns(paste0("rm_", b$id)), "Remove", class="btn-sm btn-link text-danger",
              onclick=sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("remove_click"), b$id)))
        div(class="ovl-item",
          h5(b$id), span(class=paste0("stbadge st-", st), toupper(st)),
          span(class="small-muted", style="float:right", paste0(length(b$rules %||% list()), " rule(s) \u00b7 owner ", b$owner %||% "")),
          rules_html, approve_btns)
      }))
    })

    observeEvent(input$edit_click, {
      b <- get_overlay(input$edit_click, ovl_path()); if (is.null(b)) return()
      editing(b$id)
      updateTextInput(session, "b_id", value=b$id)
      updateTextInput(session, "b_owner", value=b$owner %||% "FRM")
      for (rid in active_rids()) remove_row(rid)
      for (r in (b$rules %||% list())) add_row(seed = r)
      showNotification(paste0("Loaded '", b$id, "' for editing. Saving resets it to draft."), type="message")
    })

    observeEvent(input$remove_click, {
      idv <- input$remove_click
      showModal(modalDialog(title="Remove overlay",
        paste0("Remove overlay '", idv, "' and all its rules?"),
        footer=tagList(modalButton("Cancel"),
          actionButton(ns("remove_confirm"), "Remove", class="btn-danger")), easyClose=TRUE))
      session$userData$ovl_pending_rm <- idv
    })
    observeEvent(input$remove_confirm, {
      removeModal()
      tryCatch({ remove_overlay(session$userData$ovl_pending_rm, ovl_path())
        showNotification("Removed.", type="message"); refresh(refresh()+1) },
        error=function(e) showNotification(conditionMessage(e), type="error"))
    })

    who <- function() Sys.info()[["user"]] %||% "unknown"
    observeEvent(input$submit_click, {
      tryCatch({ set_overlay_status(input$submit_click, "pending", who(), "submitted", ovl_path())
        showNotification("Submitted for approval.", type="message"); refresh(refresh()+1) },
        error=function(e) showNotification(conditionMessage(e), type="error")) })
    observeEvent(input$approve_click, {
      tryCatch({ set_overlay_status(input$approve_click, "approved", who(), "approved", ovl_path())
        showNotification("Approved.", type="message"); refresh(refresh()+1) },
        error=function(e) showNotification(conditionMessage(e), type="error")) })
    observeEvent(input$reject_click, {
      tryCatch({ set_overlay_status(input$reject_click, "rejected", who(), "rejected", ovl_path())
        showNotification("Rejected.", type="message"); refresh(refresh()+1) },
        error=function(e) showNotification(conditionMessage(e), type="error")) })
  })
}
