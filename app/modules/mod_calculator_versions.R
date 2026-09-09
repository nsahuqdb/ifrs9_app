# =============================================================================
# app/modules/mod_calculator_versions.R
#
# Manage the calculator (code) version registry: view registered versions,
# register a new one (stamped with the current R/ code fingerprint), and set
# the active version used by default for new runs. Lives under Config ->
# Calculator versions.
# =============================================================================

mod_calculator_versions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(12,
      h3("Calculator versions"),
      p(class = "small-muted",
        "The calculator is the R computation code that produces a run. ",
        "Registering a version archives an immutable copy of the code and ",
        "stamps its fingerprint, so you can re-run any past version later \u2014 ",
        "for example an impact run comparing last quarter's calculator with ",
        "the current one on the same portfolio date. The ",
        tags$strong("active"), " version is the default for new runs; ",
        "selecting an older archived version on the Run pipeline page runs ",
        "that version's code in isolation.")
    )),
    fluidRow(
      column(7,
        div(class = "card",
          div(class = "card-header", "Registered versions"),
          div(class = "card-body", DT::DTOutput(ns("versions_table")))
        )
      ),
      column(5,
        div(class = "card",
          div(class = "card-header", "Current deployed code"),
          div(class = "card-body", uiOutput(ns("current_fp")))
        ),
        div(class = "card", style = "margin-top:1em;",
          div(class = "card-header", "Register a new version"),
          div(class = "card-body",
            textInput(ns("new_id"), "Version id", placeholder = "e.g. v1.1"),
            textInput(ns("new_label"), "Label", placeholder = "e.g. v1.1 — survival PD fix"),
            textAreaInput(ns("new_desc"), "Description", height = "70px",
                          placeholder = "What changed in this calculator version?"),
            checkboxInput(ns("new_active"), "Make this the active version", value = TRUE),
            actionButton(ns("do_register"), "Register version",
                         class = "btn-primary", icon = icon("plus")),
            uiOutput(ns("register_msg"))
          )
        ),
        div(class = "card", style = "margin-top:1em;",
          div(class = "card-header", "Set active version"),
          div(class = "card-body",
            selectInput(ns("active_pick"), NULL, choices = character(0)),
            actionButton(ns("do_activate"), "Set active",
                         class = "btn-outline-secondary", icon = icon("check")),
            uiOutput(ns("activate_msg"))
          )
        )
      )
    )
  )
}

mod_calculator_versions_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh <- reactiveVal(0)

    versions <- reactive({
      refresh()
      tryCatch(list_calculator_versions(), error = function(e) NULL)
    })

    output$versions_table <- DT::renderDT({
      v <- versions()
      if (is.null(v) || nrow(v) == 0) {
        return(DT::datatable(data.frame(message = "No versions registered."),
                             options = list(dom = "t"), rownames = FALSE))
      }
      disp <- data.frame(
        active      = ifelse(v$active, "\u2605", ""),
        id          = v$id,
        label       = v$label,
        description = v$description,
        created     = v$created_at,
        by          = v$created_by,
        fingerprint = ifelse(nzchar(v$code_hash), substr(v$code_hash, 1, 12), "(none)"),
        code        = ifelse(v$archived, "archived \u2713", "not archived"),
        stringsAsFactors = FALSE
      )
      DT::datatable(disp, rownames = FALSE, selection = "none",
                    class = "narrow-table compact",
                    options = list(dom = "tp", pageLength = 10, scrollX = TRUE))
    })

    output$current_fp <- renderUI({
      refresh()
      fp <- tryCatch(compute_code_fingerprint(), error = function(e) NA_character_)
      rec <- tryCatch(calculator_version_for_run(), error = function(e) NULL)
      tagList(
        p(tags$strong("Active version: "),
          if (!is.null(rec)) sprintf("%s", rec$label %||% rec$id %||% "?") else "(none)"),
        p(tags$strong("Current R/ fingerprint: "), tags$code(substr(fp %||% "?", 1, 16))),
        if (!is.null(rec)) {
          if (isTRUE(rec$matches_registered))
            span(class = "pill pill-approved", "deployed code matches active version")
          else if (isFALSE(rec$matches_registered))
            span(class = "pill pill-rejected", "deployed code differs from active version")
          else
            span(class = "pill pill-draft", "active version has no registered fingerprint")
        }
      )
    })

    observe({
      v <- versions()
      ch <- if (is.null(v) || nrow(v) == 0) character(0) else
        stats::setNames(v$id, ifelse(v$active, paste0(v$label, " (active)"), v$label))
      updateSelectInput(session, "active_pick", choices = ch)
    })

    observeEvent(input$do_register, {
      id_new <- trimws(input$new_id %||% "")
      if (!nzchar(id_new)) {
        output$register_msg <- renderUI(div(class = "small-muted",
          style = "color:#9e2a2b;", "Version id is required."))
        return(invisible())
      }
      res <- tryCatch(
        register_calculator_version(
          id = id_new,
          label = if (nzchar(trimws(input$new_label %||% ""))) input$new_label else id_new,
          description = input$new_desc %||% "",
          make_active = isTRUE(input$new_active)),
        error = function(e) e)
      if (inherits(res, "error")) {
        output$register_msg <- renderUI(div(class = "small-muted",
          style = "color:#9e2a2b;", conditionMessage(res)))
      } else {
        output$register_msg <- renderUI(div(class = "small-muted",
          style = "color:#1f6e5b;", sprintf("Registered %s.", id_new)))
        updateTextInput(session, "new_id", value = "")
        updateTextInput(session, "new_label", value = "")
        updateTextAreaInput(session, "new_desc", value = "")
        refresh(refresh() + 1)
      }
    })

    observeEvent(input$do_activate, {
      id_sel <- input$active_pick %||% ""
      if (!nzchar(id_sel)) return(invisible())
      res <- tryCatch(set_active_calculator_version(id_sel), error = function(e) e)
      if (inherits(res, "error")) {
        output$activate_msg <- renderUI(div(class = "small-muted",
          style = "color:#9e2a2b;", conditionMessage(res)))
      } else {
        output$activate_msg <- renderUI(div(class = "small-muted",
          style = "color:#1f6e5b;", sprintf("Active version set to %s.", id_sel)))
        refresh(refresh() + 1)
      }
    })
  })
}
