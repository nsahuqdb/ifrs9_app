# =============================================================================
# R/llm_client.R
#
# Thin client for QDB's internal LLM endpoint, which is OpenAI-compatible
# (POST /v1/chat/completions). Used by the in-app assistant (the "Assistant"
# tab) to answer questions about runs, config, intermediate data, and
# methodology.
#
# Endpoint reference (from the QDB-LLM Postman collection):
#   POST https://aimodel.qdb.qa/v1/chat/completions
#   Content-Type: application/json
#   body: { model, messages:[{role,content}], temperature, top_p }
#
# Configuration lives under the `assistant:` block in config.yml. Nothing
# here logs the request body or any API key.
# =============================================================================

#' Read the assistant configuration from config.yml.
#'
#' Returns a list with defaults applied. `enabled` gates the whole feature.
#' The API key (if the deployment needs one) is NEVER stored in config.yml —
#' it is read at call time from the environment variable named in
#' `api_key_env` (default "QDB_LLM_API_KEY"). If that env var is unset, no
#' Authorization header is sent (matches the Postman collection, which uses
#' no auth header).
llm_assistant_config <- function(cfg_path = NULL) {
  if (is.null(cfg_path)) {
    root <- getOption("ifrs9.project_root", ".")
    cfg_path <- file.path(root, "config.yml")
  }
  cfg <- list()
  if (file.exists(cfg_path)) {
    cfg <- tryCatch(yaml::read_yaml(cfg_path), error = function(e) list())
  }
  a <- cfg$assistant %||% list()

  list(
    enabled          = isTRUE(a$enabled %||% TRUE),
    endpoint         = a$endpoint     %||% "https://aimodel.qdb.qa/v1/chat/completions",
    model            = a$model        %||% "unsloth/gemma-3-12b-it",
    temperature      = as.numeric(a$temperature %||% 0.3),
    top_p            = as.numeric(a$top_p %||% 0.8),
    max_tokens       = as.integer(a$max_tokens %||% 1024L),
    timeout_seconds  = as.numeric(a$timeout_seconds %||% 60),
    api_key_env      = a$api_key_env  %||% "QDB_LLM_API_KEY",
    max_history_turns= as.integer(a$max_history_turns %||% 8L),
    max_context_chars= as.integer(a$max_context_chars %||% 24000L),
    max_tool_calls   = as.integer(a$max_tool_calls %||% 6L),
    verify_ssl       = isTRUE(a$verify_ssl %||% TRUE)
  )
}


#' Low-level POST to the chat-completions endpoint.
#'
#' Tries httr2 first (modern, pulled in by rvest), then httr, then the
#' `curl` package. Returns the parsed JSON response as a list, or throws
#' with a clean message. Never prints the body or key.
#'
#' @param url      endpoint URL
#' @param payload  list to be JSON-encoded as the request body
#' @param api_key  optional bearer token (character) or NULL
#' @param timeout  seconds
#' @param verify_ssl  whether to verify the TLS certificate
.llm_post <- function(url, payload, api_key = NULL, timeout = 60,
                      verify_ssl = TRUE) {
  body_json <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  headers <- c("Content-Type" = "application/json")
  if (!is.null(api_key) && nzchar(api_key)) {
    headers["Authorization"] <- paste("Bearer", api_key)
  }

  # ---- Path 1: httr2 ----
  if (requireNamespace("httr2", quietly = TRUE)) {
    req <- httr2::request(url)
    req <- httr2::req_headers(req, !!!as.list(headers))
    req <- httr2::req_body_raw(req, body_json, type = "application/json")
    req <- httr2::req_timeout(req, timeout)
    if (!verify_ssl) {
      req <- httr2::req_options(req, ssl_verifypeer = 0, ssl_verifyhost = 0)
    }
    # Don't auto-error on non-2xx; we want to surface the body message.
    req <- httr2::req_error(req, is_error = function(resp) FALSE)
    resp <- httr2::req_perform(req)
    status <- httr2::resp_status(resp)
    txt <- httr2::resp_body_string(resp)
    if (status >= 400) {
      stop(sprintf("LLM endpoint returned HTTP %d: %s", status,
                   .llm_trim(txt, 300)), call. = FALSE)
    }
    return(jsonlite::fromJSON(txt, simplifyVector = FALSE))
  }

  # ---- Path 2: httr ----
  if (requireNamespace("httr", quietly = TRUE)) {
    cfg <- if (!verify_ssl) httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L)
           else httr::config()
    resp <- httr::POST(
      url,
      httr::add_headers(.headers = headers),
      body = body_json,
      cfg,
      httr::timeout(timeout)
    )
    status <- httr::status_code(resp)
    txt <- httr::content(resp, as = "text", encoding = "UTF-8")
    if (status >= 400) {
      stop(sprintf("LLM endpoint returned HTTP %d: %s", status,
                   .llm_trim(txt, 300)), call. = FALSE)
    }
    return(jsonlite::fromJSON(txt, simplifyVector = FALSE))
  }

  # ---- Path 3: curl package ----
  if (requireNamespace("curl", quietly = TRUE)) {
    h <- curl::new_handle()
    curl::handle_setheaders(h, .list = as.list(headers))
    curl::handle_setopt(h, post = TRUE, postfields = body_json,
                        timeout = as.integer(timeout))
    if (!verify_ssl) {
      curl::handle_setopt(h, ssl_verifypeer = 0L, ssl_verifyhost = 0L)
    }
    resp <- curl::curl_fetch_memory(url, handle = h)
    txt <- rawToChar(resp$content)
    if (resp$status_code >= 400) {
      stop(sprintf("LLM endpoint returned HTTP %d: %s", resp$status_code,
                   .llm_trim(txt, 300)), call. = FALSE)
    }
    return(jsonlite::fromJSON(txt, simplifyVector = FALSE))
  }

  stop("No HTTP client available. Install one of: httr2, httr, or curl.",
       call. = FALSE)
}


#' Send a chat to the QDB LLM and return the assistant's text reply.
#'
#' @param messages  list of lists, each with `role` ("system"/"user"/
#'                   "assistant") and `content` (character). The full
#'                   conversation to send.
#' @param cfg       result of llm_assistant_config(); if NULL, read fresh.
#' @return character scalar — the assistant's reply text. On error, returns
#'   a string starting with "[assistant error]" so the UI can show it
#'   without crashing.
llm_chat <- function(messages, cfg = NULL) {
  if (is.null(cfg)) cfg <- llm_assistant_config()
  if (!isTRUE(cfg$enabled)) {
    return("[assistant error] The assistant is disabled in config.yml (set assistant.enabled: true).")
  }

  api_key <- Sys.getenv(cfg$api_key_env, unset = "")
  payload <- list(
    model       = cfg$model,
    messages    = messages,
    temperature = cfg$temperature,
    top_p       = cfg$top_p,
    max_tokens  = cfg$max_tokens
  )

  resp <- tryCatch(
    .llm_post(cfg$endpoint, payload,
              api_key    = if (nzchar(api_key)) api_key else NULL,
              timeout    = cfg$timeout_seconds,
              verify_ssl = cfg$verify_ssl),
    error = function(e) {
      structure(list(error = conditionMessage(e)), class = "llm_error")
    }
  )

  if (inherits(resp, "llm_error")) {
    return(paste0("[assistant error] ", resp$error,
                  "\n\nCheck that the endpoint (", cfg$endpoint,
                  ") is reachable from the app host and that ",
                  "assistant settings in config.yml are correct."))
  }

  # OpenAI-compatible shape: resp$choices[[1]]$message$content
  out <- tryCatch(
    resp$choices[[1]]$message$content,
    error = function(e) NULL
  )
  if (is.null(out) || !nzchar(out)) {
    return(paste0("[assistant error] The endpoint responded but no message ",
                  "content was found. Raw keys: ",
                  paste(names(resp), collapse = ", ")))
  }
  out
}


#' Quick connectivity probe used by the UI status badge. Returns a list with
#' `ok` (logical) and `detail` (character).
llm_health_check <- function(cfg = NULL) {
  if (is.null(cfg)) cfg <- llm_assistant_config()
  if (!isTRUE(cfg$enabled)) {
    return(list(ok = FALSE, detail = "Assistant disabled in config.yml"))
  }
  reply <- llm_chat(
    list(
      list(role = "system", content = "Reply with the single word: OK"),
      list(role = "user",   content = "ping")
    ),
    cfg = cfg
  )
  if (startsWith(reply, "[assistant error]")) {
    return(list(ok = FALSE, detail = sub("^\\[assistant error\\]\\s*", "", reply)))
  }
  list(ok = TRUE, detail = sprintf("Connected to %s", cfg$model))
}


# ---- small helpers ---------------------------------------------------------

#' Trim a string to n chars with an ellipsis.
.llm_trim <- function(x, n) {
  x <- as.character(x)
  if (nchar(x) <= n) return(x)
  paste0(substr(x, 1, n), " …")
}

# `%||%` is defined elsewhere in the package (io_helpers.R). Provide a local
# fallback in case this file is sourced standalone in a test.
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
