# =============================================================================
# app/modules/mod_snapshot_editor.R
#
# H14: edit a draft snapshot's contents through the UI.
#
# UI shape:
#   1. Pick a snapshot from a dropdown.
#   2. If status != "draft": see an info card explaining that edits
#      are gated, with a Clone-to-draft form.
#   3. If status == "draft": pick an editable file from a whitelist
#      dropdown. YAML files render as a textarea; CSV files render
#      as a DT::editable table. Save button validates + writes
#      atomically.
#
# Calls these pipeline helpers (R/snapshots.R):
#   list_snapshots, read_snapshot_metadata, snapshot_paths
#   editable_snapshot_files, clone_snapshot,
#   save_snapshot_yaml, save_snapshot_csv
# =============================================================================

mod_snapshot_editor_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Hide the file-input text box + progress bar so "Upload CSV" renders as
    # a plain button in the toolbar (the input chrome is what looks clunky).
    tags$style(HTML(sprintf(paste0(
      "#%s .upload-slim .input-group .form-control{display:none;}",
      "#%s .upload-slim .progress{display:none;}",
      "#%s .upload-slim .input-group{width:auto;}",
      "#%s .upload-slim .form-group{margin-bottom:0;}",
      "#%s .cfg-toolbar .btn{height:31px; padding:4px 12px;}",
      "#%s .v-row .form-group{margin-bottom:0;}"),
      ns("wrap"), ns("wrap"), ns("wrap"), ns("wrap"), ns("wrap"), ns("wrap")))),
    div(id = ns("wrap"),
    fluidRow(column(12,
      div(style = "display:flex; align-items:center; gap:1em; margin-bottom:0.3em;",
          h4("Edit config", style = "margin:0;"),
          actionButton(ns("refresh"), "Refresh", icon = icon("rotate"),
                        class = "btn-sm btn-outline-secondary")),
      tags$details(style = paste0("font-size:0.88em; color:#555; ",
                                    "margin: 0.2em 0 0.7em 0;"),
        tags$summary(style = "cursor:pointer;",
                      "How to use this page"),
        tags$ol(style = "margin: 0.4em 0 0.2em 1em;",
          tags$li("Pick a draft version (others show a Clone-to-draft form)."),
          tags$li("Pick a file: CSVs open as an editable table (or download, edit, re-upload); YAMLs in a code editor."),
          tags$li("Save is atomic \u2014 the whole file writes or nothing changes."),
          tags$li("When done: Manage versions \u2192 \u2192 pending; a reviewer approves on the Approval queue."))),
      div(class = "v-row",
          style = "display:flex; align-items:center; gap:0.6em;",
          tags$span(style = "font-weight:600; color:#333;", "Version:"),
          selectInput(ns("snap_pick"), NULL,
                       choices = c("(pick one)" = ""),
                       selected = "", width = "340px"))
    )),
    uiOutput(ns("body"))
    )
  )
}


mod_snapshot_editor_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh <- reactiveVal(0)

    snaps_root <- function() {
      proj_root <- getOption("ifrs9.project_root", getwd())
      getOption("ifrs9.snapshots_dir",
                file.path(proj_root, "config_snapshots"))
    }

    snaps <- reactive({
      refresh()
      tryCatch(list_snapshots(snaps_root()),
                error = function(e) NULL)
    })

    observeEvent(input$refresh, refresh(refresh() + 1))

    # Update snapshot dropdown whenever the list changes
    observe({
      s <- snaps()
      choices <- c("(pick one)" = "")
      if (!is.null(s) && nrow(s) > 0) {
        rank <- c(draft = 0, pending = 1, approved = 2, archived = 3)
        ord  <- order(rank[s$status %||% rep("draft", nrow(s))], s$created_at)
        s2 <- s[ord, , drop = FALSE]
        labels <- sprintf("%s [%s]", s2$label, s2$status %||% "?")
        choices <- c(choices, setNames(s2$label, labels))
      }
      updateSelectInput(session, "snap_pick", choices = choices,
                         selected = isolate(input$snap_pick) %||% "")
    })

    selected_meta <- reactive({
      refresh()
      pick <- input$snap_pick
      if (is.null(pick) || !nzchar(pick)) return(NULL)
      tryCatch(read_snapshot_metadata(pick, snaps_root()),
                error = function(e) NULL)
    })

    # =========== TOP-LEVEL BODY ROUTER ==============================
    output$body <- renderUI({
      m <- selected_meta()
      if (is.null(m)) {
        return(p(class = "small-muted",
                  "Pick a version to start editing."))
      }
      if (isTRUE(m$status == "draft")) return(.editor_body(ns, m))
      .non_draft_body(ns, m)
    })


    # =========== NON-DRAFT: INFO + CLONE FORM =======================
    output$clone_form <- renderUI({
      m <- selected_meta()
      if (is.null(m)) return(NULL)
      tagList(
        h5("Clone to a new draft"),
        p(class = "small-muted",
          "Create a new ", tags$em("draft"), " version starting from ",
          "this one's contents. The new draft inherits everything but is ",
          "free to edit. The lineage chain ", tags$code("parent"),
          " is preserved."),
        textInput(ns("clone_label"), "New label",
                   placeholder = "e.g. 2026-Q2-draft-1"),
        textAreaInput(ns("clone_desc"), "Description",
                       rows = 2,
                       placeholder = "What's the goal of this revision?"),
        actionButton(ns("do_clone"), "Clone to new draft",
                      icon = icon("code-branch"),
                      class = "btn-primary")
      )
    })

    observeEvent(input$do_clone, {
      m <- selected_meta(); if (is.null(m)) return()
      new_label <- trimws(input$clone_label %||% "")
      desc <- trimws(input$clone_desc %||% "")
      if (!nzchar(new_label)) {
        showNotification("New label is required.", type = "warning"); return()
      }
      if (!grepl("^[A-Za-z0-9._-]+$", new_label)) {
        showNotification("Label must be [A-Za-z0-9._-]+",
                          type = "warning"); return()
      }
      if (!nzchar(desc)) {
        showNotification("Description is required.", type = "warning"); return()
      }
      result <- tryCatch(
        clone_snapshot(
          source_label = m$label,
          new_label = new_label,
          description = desc,
          created_by = Sys.info()[["user"]] %||% "unknown",
          snapshots_root = snaps_root()
        ),
        error = function(e) e
      )
      if (inherits(result, "error")) {
        showNotification(paste("Clone failed:", conditionMessage(result)),
                          type = "error", duration = 10)
      } else {
        showNotification(sprintf("Cloned %s -> %s (draft)",
                                   m$label, new_label),
                          type = "message")
        updateTextInput(session, "clone_label", value = "")
        updateTextAreaInput(session, "clone_desc", value = "")
        refresh(refresh() + 1)
        # Auto-pick the new draft
        updateSelectInput(session, "snap_pick", selected = new_label)
      }
    })


    # =========== DRAFT: FILE EDITOR =================================
    files_choice <- reactive({
      m <- selected_meta(); if (is.null(m)) return(NULL)
      sp <- snapshot_paths(m$label, snaps_root())
      eligible <- editable_snapshot_files(sp$snapshot_dir)
      if (is.null(eligible) || nrow(eligible) == 0) return(NULL)
      eligible
    })

    observe({
      ec <- files_choice()
      if (is.null(ec)) return()
      if (!isTRUE(input$show_advanced)) {
        ec <- ec[!ec$advanced, , drop = FALSE]
      }
      # Grouped dropdown: named list of lists -> <optgroup>s. Friendly label
      # first, filename in parentheses.
      groups <- unique(ec$group)
      choices <- lapply(groups, function(g) {
        sub <- ec[ec$group == g, , drop = FALSE]
        setNames(sub$relpath,
                 sprintf("%s  (%s)", sub$label, basename(sub$relpath)))
      })
      names(choices) <- groups
      updateSelectInput(session, "file_pick",
                         choices = c(list(" " = c("(pick a file)" = "")), choices),
                         selected = isolate(input$file_pick) %||% "")
    })

    file_meta <- reactive({
      m <- selected_meta(); if (is.null(m)) return(NULL)
      pick <- input$file_pick
      if (is.null(pick) || !nzchar(pick)) return(NULL)
      sp <- snapshot_paths(m$label, snaps_root())
      ec <- editable_snapshot_files(sp$snapshot_dir)
      hit <- which(ec$relpath == pick)
      if (length(hit) != 1) return(NULL)
      list(
        snapshot = m$label,
        relpath  = pick,
        kind     = ec$kind[hit],
        label    = ec$label[hit],
        help     = ec$help[hit],
        full     = file.path(sp$snapshot_dir, pick)
      )
    })

    output$file_help <- renderUI({
      fm <- file_meta(); if (is.null(fm)) return(NULL)
      # Collapsed by default so the table gets the full page; expand to read.
      tags$details(
        style = paste0("border-left: 3px solid #5b1f6e; padding: 0.15em 0.7em;",
                        " margin-bottom: 0.5em; font-size: 0.88em; color:#444;"),
        tags$summary(style = "cursor:pointer; outline:none;",
                      tags$strong(fm$label),
                      tags$span(style = "color:#888;",
                                 sprintf("  (%s) \u2014 what is this?",
                                         basename(fm$relpath)))),
        div(style = "padding: 0.3em 0 0.2em 0;", fm$help)
      )
    })

    # "How-to guide" — renders CONFIG_GUIDE.md in a modal. Uses the markdown
    # package when available; otherwise falls back to preformatted text.
    observeEvent(input$show_guide, {
      root <- getOption("ifrs9.project_root", getwd())
      gpath <- file.path(root, "CONFIG_GUIDE.md")
      body <- if (!file.exists(gpath)) {
        p("CONFIG_GUIDE.md not found in the project root.")
      } else if (requireNamespace("markdown", quietly = TRUE)) {
        HTML(markdown::markdownToHTML(gpath, fragment.only = TRUE))
      } else {
        tags$pre(style = "white-space: pre-wrap; font-size: 0.9em;",
                  paste(readLines(gpath, warn = FALSE), collapse = "\n"))
      }
      showModal(modalDialog(
        title = "Config guide \u2014 what to edit, where",
        body, size = "l", easyClose = TRUE,
        footer = modalButton("Close")))
    })

    output$editor_panel <- renderUI({
      fm <- file_meta()
      if (is.null(fm)) {
        return(p(class = "small-muted", "Pick a file above to edit."))
      }
      if (!file.exists(fm$full)) {
        return(p(class = "small-muted",
                  sprintf("File does not exist in this version: %s",
                          fm$relpath)))
      }
      if (fm$kind == "yaml") {
        text <- paste(readLines(fm$full, warn = FALSE), collapse = "\n")
        editor_widget <- if (requireNamespace("shinyAce", quietly = TRUE)) {
          # Real code editor: line numbers, syntax highlighting, jump-to-line.
          shinyAce::aceEditor(
            outputId  = ns("yaml_text"),
            value     = text,
            mode      = "yaml",
            theme     = "github",
            height    = "550px",
            fontSize  = 13,
            showLineNumbers = TRUE,
            highlightActiveLine = TRUE,
            autoScrollEditorIntoView = TRUE,
            debounce  = 200
          )
        } else {
          # Fallback for installations without shinyAce. Functional but
          # no line numbers — recommend the install in the helper text.
          textAreaInput(ns("yaml_text"), label = NULL, value = text,
                         rows = 25, width = "100%")
        }
        tagList(
          div(class = "cfg-toolbar",
              style = paste0("display:flex; align-items:center; gap:0.4em; ",
                              "margin: 0.2em 0 0.5em 0;"),
              tags$code(fm$relpath),
              tags$span(style = "flex:1;"),
              actionButton(ns("yaml_revert"), "Discard",
                            icon = icon("arrow-rotate-left"),
                            class = "btn-sm btn-outline-secondary"),
              actionButton(ns("yaml_save"), "Save (validated)",
                            icon = icon("floppy-disk"),
                            class = "btn-sm btn-primary")),
          editor_widget
        )
      } else if (fm$kind == "csv") {
        tagList(
          # Toolbar: everything on one row above the table. Left: bulk
          # download/upload (edit in Excel, re-upload). Middle: row ops.
          # Right: save/discard.
          div(class = "cfg-toolbar",
              style = paste0("display:flex; align-items:center; gap:0.4em; ",
                              "flex-wrap:wrap; margin: 0.2em 0 0.6em 0;"),
              actionButton(ns("csv_add_row"), "Add row", icon = icon("plus"),
                            class = "btn-sm btn-outline-secondary"),
              actionButton(ns("csv_delete_row"), "Delete row", icon = icon("trash"),
                            class = "btn-sm btn-outline-secondary"),
              tags$span(style = paste0("border-left:1px solid #d5d5d5; ",
                                        "height:22px; margin:0 0.5em;")),
              downloadButton(ns("csv_download"), "Download",
                              class = "btn-sm btn-outline-secondary"),
              div(class = "upload-slim", style = "display:inline-block;",
                  fileInput(ns("csv_upload"), label = NULL,
                             accept = c(".csv", "text/csv"),
                             buttonLabel = tagList(icon("upload"), " Upload"),
                             width = "110px")),
              tags$span(style = "flex:1;"),
              actionButton(ns("csv_revert"), "Discard", icon = icon("arrow-rotate-left"),
                            class = "btn-sm btn-outline-secondary"),
              actionButton(ns("csv_save"), "Save", icon = icon("floppy-disk"),
                            class = "btn-sm btn-primary")),
          DT::DTOutput(ns("csv_table"))
        )
      } else {
        p(class = "small-muted",
          sprintf("Unknown file kind '%s'", fm$kind))
      }
    })

    # ---- YAML save / revert ---------------------------------------
    observeEvent(input$yaml_save, {
      fm <- file_meta(); if (is.null(fm) || fm$kind != "yaml") return()
      text <- input$yaml_text %||% ""
      result <- tryCatch(
        save_snapshot_yaml(label = fm$snapshot, relpath = fm$relpath,
                            text = text, validate = TRUE,
                            snapshots_root = snaps_root()),
        error = function(e) list(ok = FALSE,
                                  message = conditionMessage(e))
      )
      if (isTRUE(result$ok)) {
        showNotification(
          sprintf("Saved %s.", basename(fm$relpath)),
          type = "message", duration = 3)
        return()
      }
      # ---- Save failed: render an actionable error modal --------------
      err_msg <- result$message %||% "(no message)"
      ctx_block <- .yaml_error_context(text, err_msg)
      showModal(modalDialog(
        title = "YAML save failed",
        tags$p("The YAML did not parse. The file on disk was NOT changed."),
        tags$p(tags$strong("Error message:")),
        tags$pre(style = "white-space: pre-wrap; background: #f8d7da; padding: 0.6em;",
                  err_msg),
        if (!is.null(ctx_block)) {
          tagList(
            tags$p(tags$strong(sprintf("Context around line %d (▶ marks the line):",
                                          ctx_block$line))),
            tags$pre(style = "background: #fff3cd; padding: 0.6em; font-size: 0.85em; line-height: 1.4;",
                      ctx_block$snippet)
          )
        } else {
          tags$p(class = "small-muted",
                  "(Could not parse a line number from the error.)")
        },
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Close")
      ))
    })

    observeEvent(input$yaml_revert, {
      fm <- file_meta(); if (is.null(fm) || fm$kind != "yaml") return()
      text <- paste(readLines(fm$full, warn = FALSE), collapse = "\n")
      if (requireNamespace("shinyAce", quietly = TRUE)) {
        shinyAce::updateAceEditor(session, "yaml_text", value = text)
      } else {
        updateTextAreaInput(session, "yaml_text", value = text)
      }
      showNotification("Reloaded from disk.", type = "message", duration = 3)
    })

    # ---- CSV editor ------------------------------------------------
    # Buffers for CSV state. csv_buffer holds the editable data; the
    # comment_header buffer holds any leading `#` provenance lines so
    # we can write them back on save (otherwise saving would silently
    # strip the file's metadata block).
    csv_buffer <- reactiveVal(NULL)
    csv_comment_header <- reactiveVal(character())

    # When the picked file changes, reload BOTH buffers from disk
    observeEvent(file_meta(), {
      fm <- file_meta()
      if (is.null(fm) || fm$kind != "csv") {
        csv_buffer(NULL); csv_comment_header(character()); return()
      }
      result <- tryCatch(
        read_static_csv_with_header(fm$full),
        error = function(e) NULL
      )
      if (is.null(result)) {
        csv_buffer(NULL); csv_comment_header(character())
        return()
      }
      csv_buffer(result$data)
      csv_comment_header(result$comment_header %||% character())
      csv_file_seq(isolate(csv_file_seq()) + 1)   # triggers a fresh render
    }, ignoreNULL = FALSE)

    # Render the table only when a (new) file is loaded; all subsequent
    # changes (cell edits, add/delete row) go through the proxy with
    # replaceData(resetPaging = FALSE), so the table STAYS on the current
    # page instead of snapping back to page 1 on every change.
    csv_file_seq <- reactiveVal(0)
    csv_proxy <- DT::dataTableProxy("csv_table")
    .csv_page_len <- 15

    output$csv_table <- DT::renderDT({
      csv_file_seq()                     # re-render only on file (re)load
      df <- isolate(csv_buffer())
      if (is.null(df)) return(NULL)
      DT::datatable(
        df,
        editable = list(target = "cell"),
        selection = "single",
        rownames = FALSE,
        class = "compact stripe hover row-border",
        options = list(pageLength = .csv_page_len, scrollX = TRUE,
                        lengthMenu = c(15, 25, 50, 100),
                        dom = "lftip")
      )
    })

    # Append a new empty row and jump to its page so it is visible.
    observeEvent(input$csv_add_row, {
      df <- csv_buffer(); if (is.null(df)) return()
      newrow <- df[0, , drop = FALSE]
      newrow[1, ] <- NA
      df <- rbind(df, newrow)
      csv_buffer(df)
      DT::replaceData(csv_proxy, df, resetPaging = FALSE, rownames = FALSE)
      plen <- tryCatch(input$csv_table_state$length %||% .csv_page_len,
                        error = function(e) .csv_page_len)
      DT::selectPage(csv_proxy, ceiling(nrow(df) / max(1, plen)))
      showNotification("Row added below.", type = "message", duration = 2)
    })

    observeEvent(input$csv_delete_row, {
      df <- csv_buffer(); if (is.null(df)) return()
      sel <- input$csv_table_rows_selected
      if (is.null(sel) || length(sel) == 0) {
        showNotification("Select a row first.", type = "warning", duration = 3)
        return()
      }
      df <- df[-sel, , drop = FALSE]
      csv_buffer(df)
      DT::replaceData(csv_proxy, df, resetPaging = FALSE, rownames = FALSE)
      showNotification("Row removed.", type = "message", duration = 2)
    })

    # ---- Bulk download / upload -----------------------------------------
    # Download writes the CURRENT table (including unsaved edits) in exactly
    # the on-disk format — same writer as Save, comment header preserved —
    # so the file can be edited in Excel and re-uploaded as-is.
    output$csv_download <- downloadHandler(
      filename = function() {
        fm <- file_meta()
        if (is.null(fm)) "config.csv" else basename(fm$relpath)
      },
      content = function(file) {
        df <- csv_buffer()
        if (is.null(df)) df <- data.frame()
        write_static_csv_with_header(file, df,
          comment_header = csv_comment_header() %||% character())
      }
    )

    # Upload replaces the table AFTER a column check (names + order must
    # match the current file). Loaded into the buffer only — nothing is
    # written until Save, so it can be reviewed or discarded.
    observeEvent(input$csv_upload, {
      up <- input$csv_upload
      if (is.null(up) || is.null(up$datapath)) return()
      cur <- csv_buffer()
      new_df <- tryCatch({
        # skip the same comment header if the user kept it in the file
        first <- readLines(up$datapath, n = 50, warn = FALSE)
        skip <- 0
        while (skip < length(first) && grepl("^\\s*#", first[skip + 1])) {
          skip <- skip + 1
        }
        utils::read.csv(up$datapath, stringsAsFactors = FALSE,
                         check.names = FALSE, skip = skip,
                         colClasses = "character", na.strings = c("NA", ""))
      }, error = function(e) e)
      if (inherits(new_df, "error")) {
        showNotification(paste("Could not read the file:",
                                conditionMessage(new_df)),
                          type = "error", duration = 6)
        return()
      }
      if (!is.null(cur) &&
          !identical(trimws(colnames(new_df)), trimws(colnames(cur)))) {
        showNotification(
          sprintf("Columns do not match. Expected: %s",
                   paste(colnames(cur), collapse = ", ")),
          type = "error", duration = 8)
        return()
      }
      # Coerce to the current column types so downstream save is consistent.
      if (!is.null(cur)) {
        for (j in seq_along(cur)) {
          if (is.numeric(cur[[j]])) {
            new_df[[j]] <- suppressWarnings(as.numeric(new_df[[j]]))
          }
        }
      }
      csv_buffer(new_df)
      csv_file_seq(isolate(csv_file_seq()) + 1)   # re-render fresh
      showNotification(sprintf("Loaded %d rows — review, then Save.",
                                nrow(new_df)),
                        type = "message", duration = 4)
    })

    # Capture cell edits into the buffer
    observeEvent(input$csv_table_cell_edit, {
      info <- input$csv_table_cell_edit
      df <- csv_buffer()
      if (is.null(df)) return()
      r <- info$row; c <- info$col + 1
      old_val <- df[r, c]
      new_val <- info$value
      if (is.numeric(old_val)) {
        coerced <- suppressWarnings(as.numeric(new_val))
        if (is.na(coerced) && nzchar(new_val)) {
          showNotification(sprintf("Cell expects numeric; got '%s'", new_val),
                            type = "warning")
          return()
        }
        new_val <- coerced
      } else if (is.integer(old_val)) {
        coerced <- suppressWarnings(as.integer(new_val))
        if (is.na(coerced) && nzchar(new_val)) {
          showNotification(sprintf("Cell expects integer; got '%s'", new_val),
                            type = "warning")
          return()
        }
        new_val <- coerced
      }
      df[r, c] <- new_val
      csv_buffer(df)
    })

    observeEvent(input$csv_save, {
      fm <- file_meta(); if (is.null(fm) || fm$kind != "csv") return()
      df <- csv_buffer()
      if (is.null(df)) {
        showNotification("Nothing to save.", type = "warning"); return()
      }
      # Drop rows that are entirely empty (e.g. an added row never filled),
      # so a blank line is never written into the config.
      if (nrow(df) > 0) {
        blank <- apply(df, 1, function(r) all(is.na(r) | !nzchar(trimws(as.character(r)))))
        if (any(blank)) df <- df[!blank, , drop = FALSE]
        csv_buffer(df)
      }
      result <- tryCatch(
        save_snapshot_csv(label = fm$snapshot, relpath = fm$relpath,
                            df = df,
                            comment_header = csv_comment_header(),
                            snapshots_root = snaps_root()),
        error = function(e) list(ok = FALSE,
                                  message = conditionMessage(e))
      )
      if (isTRUE(result$ok)) {
        showNotification(sprintf("Saved (%d rows).", nrow(df)),
                          type = "message", duration = 3)
      } else {
        showNotification(paste("Save failed:", result$message),
                          type = "error", duration = 12)
      }
    })

    observeEvent(input$csv_revert, {
      fm <- file_meta(); if (is.null(fm) || fm$kind != "csv") return()
      result <- tryCatch(
        read_static_csv_with_header(fm$full),
        error = function(e) NULL
      )
      if (is.null(result)) {
        csv_buffer(NULL); csv_comment_header(character())
      } else {
        csv_buffer(result$data)
        csv_comment_header(result$comment_header %||% character())
      }
      csv_file_seq(isolate(csv_file_seq()) + 1)
      showNotification("Changes discarded.", type = "message", duration = 2)
    })
  })
}


# ============================================================================
# UI fragments per snapshot status. Inline functions (not exported).
# ============================================================================

.editor_body <- function(ns, m) {
  base_txt <- {
    bs <- m$base_source %||% ""
    if (is.character(bs) && grepl("^snapshot:", bs)) {
      sprintf("based on %s", sub("^snapshot:", "", bs))
    } else if (!is.null(m$parent) && nzchar(as.character(m$parent))) {
      sprintf("based on %s", m$parent)
    } else {
      "based on the live config (first version)"
    }
  }
  tagList(
    div(style = paste0("font-size:0.9em; color:#555; margin-bottom:0.4em;"),
        tags$strong(sprintf("Draft: %s", m$label)),
        sprintf(" \u00b7 %s", base_txt),
        if (nzchar(m$description %||% "")) tags$em(sprintf(" \u00b7 %s", m$description))),
    div(style = "display: flex; align-items: flex-end; gap: 1.2em; flex-wrap: wrap;",
        selectInput(ns("file_pick"), NULL,
                     choices = c("(pick a file)" = ""),
                     width = "480px"),
        div(style = "padding-bottom: 14px;",
            checkboxInput(ns("show_advanced"), "Advanced files",
                           value = FALSE, width = "160px")),
        div(style = "padding-bottom: 18px;",
            actionLink(ns("show_guide"), "How-to",
                        icon = icon("circle-question")))),
    uiOutput(ns("file_help")),
    uiOutput(ns("editor_panel"))
  )
}

.non_draft_body <- function(ns, m) {
  tagList(
    div(class = "alert alert-warning", style = "margin-bottom: 1em;",
        sprintf("This version is in status '%s'. ", m$status),
        "Edits are locked. Clone it to a new draft below to make a revision."),
    uiOutput(ns("clone_form"))
  )
}


#' Pull line/column from a yaml::yaml.load error message and build a
#' multi-line context snippet around it.
#'
#' libyaml emits errors of the form:
#'   "Parser error: while parsing a block collection at line N, column M
#'    did not find expected '-' indicator at line P, column Q"
#'
#' We pull the LAST line/column pair (the actual offending location)
#' and return a 5-line window with a marker on the offending line.
#'
#' @param text     the whole text the user is trying to save
#' @param err_msg  conditionMessage from yaml::yaml.load
#' @return list(line=int, snippet=character) or NULL if no line found
.yaml_error_context <- function(text, err_msg) {
  if (is.null(text) || !nzchar(text) || is.null(err_msg)) return(NULL)
  # libyaml messages can mention up to two locations; prefer the last
  # ("at line P, column Q") since that's where the actual mistake is.
  matches <- gregexpr("line\\s+(\\d+),\\s*column\\s+(\\d+)",
                      err_msg, perl = TRUE)[[1]]
  if (matches[1] == -1) return(NULL)
  starts  <- as.numeric(matches)
  lengths <- attr(matches, "match.length")
  # Take the last match
  i <- length(starts)
  m <- regmatches(err_msg,
                  regexpr("line\\s+(\\d+),\\s*column\\s+(\\d+)",
                          substr(err_msg, starts[i],
                                  starts[i] + lengths[i] - 1),
                          perl = TRUE))
  nums <- regmatches(m, regexec("line\\s+(\\d+),\\s*column\\s+(\\d+)", m))[[1]]
  if (length(nums) < 3) return(NULL)
  err_line <- as.integer(nums[2])
  if (is.na(err_line)) return(NULL)

  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  if (length(lines) == 0) return(NULL)
  lo <- max(1L, err_line - 2L)
  hi <- min(length(lines), err_line + 2L)
  width <- nchar(as.character(hi))
  out <- vapply(lo:hi, function(n) {
    marker <- if (n == err_line) "▶" else " "
    sprintf("%s %*d  %s", marker, width, n, lines[n])
  }, character(1))
  list(line = err_line, snippet = paste(out, collapse = "\n"))
}
