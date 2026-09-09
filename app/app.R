# =============================================================================
# app/app.R
#
# Read-only Shiny app for browsing IFRS9 ETL pipeline runs, snapshots,
# validation findings, audit log, and outputs.
#
# Architecture:
#   - This file launches the app; it is intentionally tiny.
#   - UI and server are split into per-page modules under app/modules/.
#   - The pipeline R/ code is sourced at startup so the app can call
#     run_discovery, snapshots, audit_log, and load_static helpers
#     without re-implementing them.
#
# Run locally:
#   setwd("path/to/ifrs9_etl")
#   shiny::runApp("app")
#
# Deployment (Posit Connect / Shiny Server):
#   The app expects the project root as the working directory at runtime
#   so relative paths (`R/`, `runs/`, `config_snapshots/`, `logs/`) resolve.
#   On Posit Connect, deploy the entire project; the server will set CWD
#   to the deployment root automatically.
#
# Configuration (all optional, with sensible defaults):
#   options(ifrs9.runs_dir       = "runs")            # where run_etl writes
#   options(ifrs9.snapshots_dir  = "config_snapshots")
#   options(ifrs9.audit_log      = "logs/etl_audit.jsonl")
# =============================================================================

# ---- Required packages --------------------------------------------------
required_pkgs <- c("shiny", "bslib", "DT", "jsonlite", "yaml", "tibble",
                    "markdown", "readr")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                        logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Missing packages required by the Shiny app: ",
       paste(missing_pkgs, collapse = ", "),
       "\nInstall with:  install.packages(c(",
       paste(sprintf("'%s'", missing_pkgs), collapse = ", "),
       "))")
}

library(shiny)
library(bslib)


# ---- Resolve project root + source pipeline -----------------------------
# Pipeline modules expose helpers used by the app's modules:
#   list_runs, read_run_manifest, read_run_validation,
#   read_run_reconciliation, list_run_outputs   (run_discovery.R)
#   list_snapshots, read_snapshot_metadata, snapshot_paths,
#   diff_snapshots                              (snapshots.R)
#   read_audit_log                              (audit_log.R)
.app_root <- function() {
  # The engine moved into the ifrs9qdb package, so R/run_etl.R is no longer a
  # marker for the project root. Key off what the PROJECT still owns: the run
  # configuration and the app folder itself. Both layouts are supported -
  # runApp("app") from the project root, and runApp() from inside app/.
  is_root <- function(d) {
    file.exists(file.path(d, "config.yml")) &&
      dir.exists(file.path(d, "app")) &&
      dir.exists(file.path(d, "config"))
  }
  here <- getwd()
  if (is_root(here)) return(here)
  up <- normalizePath("..", mustWork = FALSE)
  if (is_root(up)) return(up)
  up2 <- normalizePath("../..", mustWork = FALSE)
  if (is_root(up2)) return(up2)
  stop("Cannot locate the project root from ", here, ".\n",
       "  Expected a folder containing config.yml, app/ and config/.\n",
       "  Run shiny::runApp(\"app\") from the project root.",
       call. = FALSE)
}

.project_root <- .app_root()

# Pin every "where to look" path to absolute paths derived from project_root.
# Without this, a relative path like "runs" resolves against whatever
# getwd() is at the moment the module reads it — which differs between
# Rscript, RStudio, shiny::runApp(), and Posit Connect. Setting the
# options here once at startup makes the rest of the app cwd-independent
# for the helpers we control.
options(
  ifrs9.project_root   = .project_root,
  ifrs9.runs_dir       = file.path(.project_root, "runs"),
  ifrs9.snapshots_dir  = file.path(.project_root, "config_snapshots"),
  ifrs9.audit_log      = file.path(.project_root, "logs", "etl_audit.jsonl")
)

# ---- File upload limit -----------------------------------------------------
# Default Shiny upload cap is 5 MB, way too small for an IFRS9 input
# bundle (the AccountCollateralAllocation and RepaymentSchedule xlsx
# files alone routinely run tens of MB each). We default to 500 MB and
# let a deployer override via config.yml::run.max_upload_size_mb.
local({
  size_mb <- 500
  cfg_path <- file.path(.project_root, "config.yml")
  if (file.exists(cfg_path)) {
    cfg <- tryCatch(yaml::read_yaml(cfg_path), error = function(e) NULL)
    cfg_size <- cfg$run$max_upload_size_mb
    if (!is.null(cfg_size) &&
        is.numeric(cfg_size) &&
        cfg_size > 0) {
      size_mb <- cfg_size
    }
  }
  options(shiny.maxRequestSize = size_mb * 1024 * 1024)
  message(sprintf("[app] upload size limit: %d MB", size_mb))
})

# Move the working directory to the project root for the duration of the
# Shiny session. Several pipeline functions use bare relative paths (e.g.
# `config_path = "config.yml"` is the default for run_etl) and aren't
# parameterised on a project-root option. Setting cwd here ONCE makes
# every default resolve correctly. On Posit Connect the deploy bundle
# is the project root and cwd is set by the platform — this line is a
# no-op there but harmless.
setwd(.project_root)

# ---- Load the engine ------------------------------------------------------
#
# The calculation engine, the ECL report builder and the whole analytics and
# stress-testing layer live in the ifrs9qdb package. The app carries only its
# own UI modules, so there is one copy of the logic and the app cannot drift
# from what the package tests cover.
#
# Internals are attached deliberately. The package exports its public API
# (325 documented functions) and keeps dot-prefixed helpers private, which is
# right for a package but leaves the app short of about two dozen it genuinely
# uses -- .fer_classify_stage, .an_ingredients, .stress_rows and the like.
# Rather than widen the package's public surface to suit one caller, or pepper
# the modules with `:::`, the namespace is attached once here and the reason
# recorded.
local({
  if (!requireNamespace("ifrs9qdb", quietly = TRUE)) {
    stop("The ifrs9qdb package is not installed.\n",
         "  install.packages(\"ifrs9qdb_1.0.1.tar.gz\", repos = NULL, type = \"source\")",
         call. = FALSE)
  }
  library(ifrs9qdb)
  ns <- asNamespace("ifrs9qdb")
  hidden <- setdiff(ls(ns, all.names = TRUE), ls("package:ifrs9qdb", all.names = TRUE))
  for (nm in hidden) {
    if (!exists(nm, envir = globalenv(), inherits = FALSE)) {
      assign(nm, get(nm, envir = ns), envir = globalenv())
    }
  }
  message(sprintf("[app] ifrs9qdb %s loaded (%d exported, %d internal attached)",
                  as.character(utils::packageVersion("ifrs9qdb")),
                  length(ls("package:ifrs9qdb")), length(hidden)))
})

# The assistant helpers are app-only and stay outside the package.
local({
  r_dir <- file.path(.project_root, "R")
  for (f in c("llm_client.R", "llm_tools.R", "llm_charts.R", "llm_context.R")) {
    p <- file.path(r_dir, f)
    if (file.exists(p)) source(p, local = FALSE)
  }
})


# ---- Source page modules -------------------------------------------------
local({
  mods <- list.files(file.path(.project_root, "app", "modules"),
                      pattern = "\\.R$", full.names = TRUE)
  for (m in mods) source(m, local = FALSE)
})


# ---- Top-level UI --------------------------------------------------------
ui <- page_navbar(
  title = tagList(
    tags$img(src = "qdb_logo.jpg", height = "32px",
              class = "qdb-navbar-logo",
              style = "margin-right: 12px; margin-top: -4px;"),
    tags$span("IFRS9 ETL Runs",
              style = "font-weight: 600; vertical-align: middle;")
  ),
  theme = bs_theme(version = 5, bootswatch = "flatly",
                    primary = "#5b1f6e"),
  fillable = FALSE,
  header = tags$head(
    # Browser tab favicon
    tags$link(rel = "icon", type = "image/jpeg",
               href = "qdb_logo.jpg"),
    # Modern design system (see app/www/design-system.css)
    tags$link(rel = "stylesheet", href = "design-system.css"),
    tags$style(HTML("
      /* The logo is dark purple on a JPG with white background. The
         flatly navbar is dark, so the logo's white background creates
         an ugly white block AND the dark text is hard to read. The
         filter chain below: (1) brightness(0) makes every pixel
         black, (2) invert(1) flips it to white, (3) the JPG's white
         background, also inverted, becomes black — which we hide via
         mix-blend-mode: screen so it lets the navbar show through. */
      .qdb-navbar-logo {
        filter: brightness(0) invert(1);
        mix-blend-mode: screen;
      }
      .small-muted { color: #6c757d; font-size: 0.85em; }
      .pill { display: inline-block; padding: 0.15em 0.5em; border-radius: 0.4em;
              font-size: 0.8em; font-weight: 600; }
      .pill-error    { background: #dc3545; color: white; }
      .pill-warn     { background: #fd7e14; color: white; }
      .pill-info     { background: #0dcaf0; color: white; }
      .pill-pass     { background: #198754; color: white; }
      .pill-suppr    { background: #6c757d; color: white; }
      .pill-approved { background: #198754; color: white; }
      .pill-pending  { background: #fd7e14; color: white; }
      .pill-draft    { background: #6c757d; color: white; }
      .pill-archived { background: #adb5bd; color: white; }
      .pill-tested        { background: #0d6efd; color: white; }
      .pill-pending_final { background: #fd7e14; color: white; }
      .pill-rejected      { background: #dc3545; color: white; }
      .narrow-table th, .narrow-table td { font-size: 0.85em; padding: 0.4em; }
      /* Assistant chat: make rendered markdown tables readable */
      #chatbot-transcript table { border-collapse: collapse; margin: 0.5em 0; width: 100%; }
      #chatbot-transcript th, #chatbot-transcript td {
        border: 1px solid #d9c9e2; padding: 0.3em 0.5em; font-size: 0.85em; text-align: left; }
      #chatbot-transcript th { background: #f6f3f8; }
      #chatbot-transcript pre { background: #f6f3f8; padding: 0.5em; border-radius: 0.4em; overflow-x: auto; }
      #chatbot-transcript code { background: #f0ebf4; padding: 0.05em 0.3em; border-radius: 0.3em; }
      #chatbot-transcript p:last-child { margin-bottom: 0; }
      /* Subtle QDB-purple accents on cards */
      .card-header { background-color: #f6f3f8; border-bottom: 1px solid #e0d4e6; }
      .btn-primary { background-color: #5b1f6e; border-color: #5b1f6e; }
      .btn-primary:hover { background-color: #4a1758; border-color: #4a1758; }

      /* ---- Spacious, elegant navbar with hover dropdowns ---- */
      .navbar-nav .nav-link { padding-left: 1rem; padding-right: 1rem; font-weight: 500; }
      .navbar-nav .nav-item { margin: 0 0.1rem; }
      .navbar .dropdown-menu {
        border: 1px solid #e0d4e6; border-radius: 0.5rem;
        box-shadow: 0 8px 24px rgba(91,31,110,0.15);
        padding: 0.35rem;
        min-width: 14rem;
      }
      .navbar .dropdown-item { border-radius: 0.35rem; padding: 0.5rem 0.9rem; font-weight: 500; }
      .navbar .dropdown-item:hover, .navbar .dropdown-item:focus {
        background-color: #f6f3f8; color: #5b1f6e; }
      .navbar .dropdown-item.active, .navbar .dropdown-item:active {
        background-color: #5b1f6e; color: #fff; }
      .navbar .dropdown-divider { margin: 0.3rem 0.2rem; border-top-color: #e0d4e6; }
      .navbar .dropdown-toggle { cursor: pointer; }
      /* Desktop: open on hover, snappy and non-finicky. The menu sits flush
         under the toggle (no margin gap), and an invisible bridge spans any
         sub-pixel gap so moving the pointer from the toggle into the menu
         never drops the hover. */
      @media (min-width: 992px) {
        .navbar .nav-item.dropdown > .dropdown-menu {
          display: block;            /* present in DOM so we can animate */
          margin-top: 0; top: 100%; left: 0;
          opacity: 0; visibility: hidden; transform: translateY(4px);
          transition: opacity .12s ease, transform .12s ease, visibility .12s;
          pointer-events: none;
        }
        .navbar .nav-item.dropdown:hover > .dropdown-menu,
        .navbar .nav-item.dropdown:focus-within > .dropdown-menu {
          opacity: 1; visibility: visible; transform: translateY(0);
          pointer-events: auto;
        }
        /* immediate close after picking an item, until the pointer leaves */
        .navbar .nav-item.dropdown > .dropdown-menu.ifrs9-force-closed {
          opacity: 0 !important; visibility: hidden !important;
          pointer-events: none !important;
        }
        /* invisible hover bridge under the toggle */
        .navbar .nav-item.dropdown::after {
          content: ''; position: absolute; left: 0; right: 0; top: 100%;
          height: 10px;
        }
        .navbar .nav-item.dropdown:not(:hover):not(:focus-within)::after {
          display: none;
        }
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('ifrs9_scroll_transcript', function(id){
        var el = document.getElementById(id);
        if (el) { setTimeout(function(){ el.scrollTop = el.scrollHeight; }, 50); }
      });

      // Navbar dropdown behaviour: smooth hover on desktop, taps on mobile,
      // and never sticky. On desktop we detach Bootstrap's click-toggle so a
      // click can't leave a menu stuck open; CSS :hover/:focus-within drive
      // visibility. Picking an item closes the menu immediately.
      (function(){
        function bind(){
          var desktop = window.matchMedia('(min-width: 992px)').matches;
          document.querySelectorAll('.navbar .nav-link.dropdown-toggle').forEach(function(t){
            if (desktop){
              if (t.getAttribute('data-bs-toggle')){
                t.setAttribute('data-ifrs9-toggle', t.getAttribute('data-bs-toggle'));
                t.removeAttribute('data-bs-toggle');
              }
              if (!t.dataset.ifrs9NoClick){
                t.dataset.ifrs9NoClick = '1';
                t.addEventListener('click', function(e){ e.preventDefault(); });
              }
            } else if (t.getAttribute('data-ifrs9-toggle')){
              t.setAttribute('data-bs-toggle', t.getAttribute('data-ifrs9-toggle'));
            }
          });
          document.querySelectorAll('.navbar .nav-item.dropdown').forEach(function(dd){
            if (dd.dataset.ifrs9Bound) return;
            dd.dataset.ifrs9Bound = '1';
            var menu = dd.querySelector('.dropdown-menu');
            // re-enable hover once the pointer leaves the dropdown
            dd.addEventListener('mouseleave', function(){
              if (menu) menu.classList.remove('ifrs9-force-closed');
            });
            dd.querySelectorAll('.dropdown-item').forEach(function(it){
              it.addEventListener('click', function(){
                if (menu) menu.classList.add('ifrs9-force-closed');
                if (document.activeElement) document.activeElement.blur();
              });
            });
          });
        }
        if (document.readyState !== 'loading') setTimeout(bind, 0);
        document.addEventListener('DOMContentLoaded', bind);
        document.addEventListener('shiny:connected', function(){ setTimeout(bind, 60); });
        var rz; window.addEventListener('resize', function(){
          clearTimeout(rz); rz = setTimeout(bind, 120);
        });
      })();
    "))
  ),
  # Grouped navigation. Related pages are tucked under hover dropdowns so the
  # top bar stays spacious. The three former "snapshot" pages now live under
  # a single "Config" menu (config snapshots = versioned configuration).
  nav_menu(
    title = "Runs",
    icon  = icon("list-check"),
    nav_panel("Browse runs",    mod_runs_ui("runs")),
    nav_panel("Run pipeline",   mod_run_trigger_ui("run_trigger")),
    nav_panel("Approval queue", mod_approval_queue_ui("approval"))
  ),
  nav_menu(
    title = "Config",
    icon  = icon("sliders"),
    nav_panel("Config versions",       mod_snapshots_ui("snapshots")),
    nav_panel("Manage config versions", mod_snapshot_manager_ui("snapshot_manager")),
    nav_panel("Edit config",           mod_snapshot_editor_ui("snapshot_editor")),
    nav_panel("Calculator versions",   mod_calculator_versions_ui("calc_versions")),
    "----",
    nav_panel("ECL overlays",            mod_overlays_ui("overlays")),
    nav_panel("Validation suppressions", mod_suppressions_ui("suppressions"))
  ),
  nav_panel("Analytics",   mod_analytics_ui("analytics"), icon = icon("chart-line")),
  nav_panel("Audit log",   mod_audit_log_ui("audit"), icon = icon("clock-rotate-left")),
  nav_panel("Assistant", mod_chatbot_ui("chatbot"), icon = icon("robot")),
  nav_spacer(),
  nav_panel("Help", mod_help_ui("help"), icon = icon("circle-question")),
  nav_item(
    # The code SHA shown here is captured at app startup. If you pull
    # new code without restarting Shiny, this banner will be stale —
    # restart to refresh.
    local({
      sha <- tryCatch(get_current_code_sha(), error = function(e) NA_character_)
      sha_short <- if (is.na(sha)) "?" else substr(sha, 1, 8)
      sha_full  <- if (is.na(sha)) "(SHA unavailable)" else sha
      tags$span(class = "small-muted",
                style = "margin-right: 1.2em;",
                title = paste0("Full code SHA: ", sha_full),
                "code: ", tags$code(sha_short))
    })
  ),
  nav_item(
    tags$span(class = "small-muted",
              sprintf("user: %s", Sys.info()[["user"]] %||% "unknown"))
  )
)


# ---- Top-level server ----------------------------------------------------
server <- function(input, output, session) {
  # Cross-module refresh signals. When one module changes snapshots
  # (create / promote / approve / reject), it bumps this counter; other
  # modules that show snapshot lists observe this value and re-read
  # from disk. Without this, the Run pipeline page's dropdown only
  # reflects the snapshot state at app start, missing any approvals
  # done during the session.
  session$userData$snapshots_changed <- reactiveVal(0)

  # Same pattern for runs: when the Run pipeline finishes phase 2,
  # the Runs page and Approval queue need to know so they re-read
  # the runs/ directory and pick up the new run. When approve/reject
  # happens, the Runs page (status pill column) needs to re-read.
  session$userData$runs_changed <- reactiveVal(0)

  mod_runs_server("runs",
                  on_select_run = function(run_path) {})
  mod_run_trigger_server("run_trigger")
  mod_approval_queue_server("approval")
  mod_snapshots_server("snapshots")
  mod_snapshot_manager_server("snapshot_manager")
  mod_snapshot_editor_server("snapshot_editor")
  mod_suppressions_server("suppressions")
  mod_analytics_server("analytics")
  mod_audit_log_server("audit")
  mod_help_server("help")
  mod_chatbot_server("chatbot")
  mod_calculator_versions_server("calc_versions")

  # Latest FinalEclReport for the overlay preview (dry-run), if a run exists.
  latest_report_r <- reactive({
    session$userData$runs_changed()
    rd <- tryCatch(runs_dir_default(), error = function(e) NULL)
    if (is.null(rd) || !dir.exists(rd)) return(NULL)
    runs <- tryCatch(list_runs(rd), error = function(e) NULL)
    if (is.null(runs) || nrow(runs) == 0) return(NULL)
    for (i in seq_len(nrow(runs))) {
      cand <- runs$path[i]
      p <- file.path(cand, "Output", "FinalEclReport.csv")
      if (!file.exists(p)) p <- file.path(cand, "FinalEclReport.csv")
      if (file.exists(p))
        return(tryCatch(as.data.frame(
          readr::read_csv(p, show_col_types = FALSE, progress = FALSE),
          check.names = FALSE), error = function(e) NULL))
    }
    NULL
  })
  mod_overlays_server("overlays", latest_report = latest_report_r)
}


# ---- Launch (when sourced via shiny::runApp("app")) ---------------------
shinyApp(ui = ui, server = server)
