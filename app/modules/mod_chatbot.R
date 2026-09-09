# =============================================================================
# app/modules/mod_chatbot.R
#
# In-app AI assistant ("Assistant" tab). A chat interface backed by QDB's
# internal LLM endpoint (see R/llm_client.R) and grounded on the app's own
# artifacts (see R/llm_context.R). Answers questions about runs, config,
# intermediate/output data, governance/approvals, and methodology.
#
# Read-only: the assistant cannot trigger runs, edit config, or approve
# anything. It only reads run manifests, validation reports, config files,
# output CSVs, and NOTES.md to ground its answers.
#
# Conversation state is per-session (in a reactiveVal); nothing is persisted.
# =============================================================================

mod_chatbot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(12,
      h3("Assistant"),
      p(class = "small-muted",
        "Ask about pipeline runs, configuration, approvals, methodology, or ",
        "analytics over the output files \u2014 including comparing one run to ",
        "another (e.g. quarter over quarter), diffing files, and breaking ",
        "down any column by stage / portfolio / rating. Answers come back as ",
        "tables and charts where useful, grounded on this deployment's real ",
        "run data. The assistant is read-only \u2014 it can't trigger runs or ",
        "change settings.")
    )),
    fluidRow(
      column(8,
        # Run focus selector + status
        div(style = "display:flex; gap:1em; align-items:center; margin-bottom:0.6em;",
            div(style = "flex:1;",
                selectInput(ns("focus_run"), "Focus run (for data / governance questions):",
                            choices = c("Latest run" = ""), width = "100%")),
            div(uiOutput(ns("conn_badge")))
        ),
        # Chat transcript
        div(id = ns("transcript"),
            style = paste0("border:1px solid #e0d4e6; border-radius:0.5em; ",
                           "padding:0.8em; height:52vh; overflow-y:auto; ",
                           "background:#fcfbfd;"),
            uiOutput(ns("messages"))
        ),
        # Composer
        div(style = "display:flex; gap:0.5em; margin-top:0.6em;",
            div(style = "flex:1;",
                textAreaInput(ns("composer"), label = NULL,
                              placeholder = "Ask a question\u2026  (Ctrl+Enter to send)",
                              width = "100%", height = "70px",
                              resize = "vertical")),
            div(style = "display:flex; flex-direction:column; gap:0.4em;",
                actionButton(ns("send"), "Send", class = "btn-primary"),
                actionButton(ns("clear"), "Clear", class = "btn-outline-secondary btn-sm"))
        ),
        # Ctrl+Enter to send
        tags$script(HTML(sprintf(
          "document.addEventListener('keydown', function(e){
             var ta = document.getElementById('%s');
             if (ta && document.activeElement === ta && e.ctrlKey && e.key === 'Enter') {
               document.getElementById('%s').click();
             }
           });", ns("composer"), ns("send"))))
      ),
      column(4,
        div(class = "card",
          div(class = "card-header", "Try asking"),
          div(class = "card-body",
            tags$ul(style = "padding-left:1.1em; margin-bottom:0;",
              tags$li("Compare the last two runs: account count by stage, and chart it."),
              tags$li("Quarter over quarter, how did OnBalance by portfolio change?"),
              tags$li("Diff AccountMaster_1 between the latest two runs \u2014 which columns changed and how many rows?"),
              tags$li("Show the rating distribution in the latest run as a chart."),
              tags$li("Which macro PD model are we using, and who approved the latest run?"),
              tags$li("How is the final LGD per contract calculated?"),
              tags$li("Compare StPD rows by portfolio across the last two runs."),
              tags$li("What's the total and mean OnBalance in the latest run?")
            )
          )
        ),
        div(class = "small-muted", style = "margin-top:0.8em;",
            "The assistant can query any output file and column on demand, ",
            "compare two runs, diff files row-by-row, and render charts. ",
            "Pick a Focus run to anchor single-run questions; for ",
            "comparisons it picks the runs from your wording or the run list. ",
            "Note: per-account ECL is produced by LIC downstream, so the ",
            "assistant compares ECL drivers (stages, PDs, exposures, ratings, ",
            "collateral) rather than booked ECL.")
      )
    )
  )
}


mod_chatbot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    cfg <- llm_assistant_config()

    # Convert the UI's message history (which may carry a chart_uri field)
    # into the role/content-only shape the LLM API expects.
    .to_llm_history <- function(msgs) {
      lapply(msgs, function(m) list(role = m$role, content = m$content))
    }

    # Conversation state: list of list(role=, content=)
    history <- reactiveVal(list())
    busy    <- reactiveVal(FALSE)

    # ---- Populate the focus-run dropdown -------------------------------
    observe({
      runs <- tryCatch(list_runs(), error = function(e) NULL)
      choices <- c("Latest run" = "")
      if (!is.null(runs) && nrow(runs) > 0) {
        labels <- sprintf("%s  (%s)", runs$run_id,
                          substr(runs$started_at %||% "", 1, 16))
        ch <- stats::setNames(runs$run_id, labels)
        choices <- c(choices, ch)
      }
      updateSelectInput(session, "focus_run", choices = choices)
    })

    # ---- Connection badge ----------------------------------------------
    output$conn_badge <- renderUI({
      if (!isTRUE(cfg$enabled)) {
        return(span(class = "pill pill-draft", "assistant disabled"))
      }
      span(class = "pill pill-info", title = cfg$endpoint,
           sprintf("model: %s", cfg$model))
    })

    # ---- Render the transcript -----------------------------------------
    output$messages <- renderUI({
      msgs <- history()
      if (length(msgs) == 0) {
        return(div(class = "small-muted",
                   "No messages yet. Ask a question to get started."))
      }
      bubbles <- lapply(msgs, function(m) {
        is_user <- identical(m$role, "user")
        align <- if (is_user) "flex-end" else "flex-start"
        bg     <- if (is_user) "#5b1f6e" else "#ffffff"
        fg     <- if (is_user) "#ffffff" else "#212529"
        border <- if (is_user) "none" else "1px solid #e0d4e6"
        label  <- if (is_user) "You" else "Assistant"
        body   <- if (is_user) {
          tags$div(style = "white-space:pre-wrap;", m$content)
        } else {
          # Render assistant markdown (tables, lists, code). The assistant
          # emits GitHub-flavored tables, so prefer commonmark with
          # extensions enabled (best table support); fall back to the
          # markdown package, then to plain preformatted text.
          html <- NULL
          if (requireNamespace("commonmark", quietly = TRUE)) {
            html <- tryCatch(
              commonmark::markdown_html(m$content, extensions = TRUE),
              error = function(e) NULL)
          }
          if (is.null(html) && requireNamespace("markdown", quietly = TRUE)) {
            html <- tryCatch(
              markdown::markdownToHTML(text = m$content, fragment.only = TRUE),
              error = function(e) NULL)
          }
          if (is.null(html)) {
            tags$div(style = "white-space:pre-wrap;", m$content)
          } else {
            HTML(html)
          }
        }
        # Optional chart rendered server-side (base64 PNG data URI)
        chart_el <- NULL
        if (!is_user && !is.null(m$chart_uri) && nzchar(m$chart_uri %||% "")) {
          chart_el <- tags$img(src = m$chart_uri,
                               style = "max-width:100%; margin-top:0.5em; border:1px solid #e0d4e6; border-radius:0.4em;")
        }
        div(style = sprintf("display:flex; justify-content:%s; margin:0.4em 0;", align),
            div(style = sprintf(paste0("max-width:85%%; background:%s; color:%s; ",
                                       "border:%s; border-radius:0.6em; ",
                                       "padding:0.5em 0.75em;"),
                                bg, fg, border),
                div(style = "font-size:0.7em; opacity:0.7; margin-bottom:0.2em;", label),
                body,
                chart_el))
      })
      # Busy indicator
      if (isTRUE(busy())) {
        bubbles <- c(bubbles, list(
          div(style = "display:flex; justify-content:flex-start; margin:0.4em 0;",
              div(class = "small-muted", style = "padding:0.5em;",
                  tags$em("Assistant is thinking\u2026")))
        ))
      }
      tagList(bubbles)
    })

    # ---- Send handler ---------------------------------------------------
    do_send <- function() {
      q <- trimws(input$composer %||% "")
      if (!nzchar(q) || isTRUE(busy())) return(invisible())
      if (!isTRUE(cfg$enabled)) {
        showNotification("Assistant is disabled in config.yml.", type = "error")
        return(invisible())
      }

      # Append user message, clear composer, show busy
      h <- history()
      h <- c(h, list(list(role = "user", content = q)))
      history(h)
      updateTextAreaInput(session, "composer", value = "")
      busy(TRUE)

      focus <- input$focus_run %||% ""
      run_id <- if (nzchar(focus)) focus else NULL

      # Call the model inside withProgress so the user gets visible feedback
      # during the (synchronous, multi-step) agentic call.
      result <- withProgress(
        message = "Assistant is analysing\u2026",
        value = 0.4,
        {
          tryCatch(
            assistant_answer(question = q,
                             history  = .to_llm_history(utils::head(h, length(h) - 1)),
                             run_id   = run_id,
                             cfg      = cfg),
            error = function(e)
              list(text = paste0("[assistant error] ", conditionMessage(e)),
                   chart_uri = NULL)
          )
        }
      )

      h <- history()
      h <- c(h, list(list(role = "assistant",
                          content = result$text %||% "(no answer)",
                          chart_uri = result$chart_uri)))
      history(h)
      busy(FALSE)

      # Auto-scroll transcript to bottom
      session$sendCustomMessage("ifrs9_scroll_transcript", ns("transcript"))
    }

    observeEvent(input$send, do_send())

    observeEvent(input$clear, {
      history(list())
    })
  })
}
