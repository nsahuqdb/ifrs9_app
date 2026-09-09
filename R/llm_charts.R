# =============================================================================
# R/llm_charts.R
#
# Renders the `chart_data` block produced by the analytics tools (see
# R/llm_tools.R) into a base64-encoded PNG data URI, so the assistant can
# show charts inline in the chat transcript without any client-side JS
# charting library. Uses base R graphics (always available) — no ggplot /
# plotly dependency required.
#
# chart_data shape:
#   list(
#     type       = "bar" | "grouped_bar" | "line",
#     title      = "...",
#     categories = c("Stage 1","Stage 2", ...),     # x-axis labels
#     series     = list(list(name="Q1", values=c(...)),
#                       list(name="Q2", values=c(...)))
#   )
# =============================================================================

#' Render a chart_data spec to a PNG data URI (character) or NULL on failure.
render_chart_data <- function(chart_data, width = 720, height = 420) {
  if (is.null(chart_data)) return(NULL)
  if (!requireNamespace("grDevices", quietly = TRUE)) return(NULL)

  cats <- chart_data$categories %||% character(0)
  series <- chart_data$series %||% list()
  if (length(cats) == 0 || length(series) == 0) return(NULL)

  # Cap categories so the chart stays legible
  max_cat <- 25L
  if (length(cats) > max_cat) {
    cats <- cats[seq_len(max_cat)]
    series <- lapply(series, function(s) {
      s$values <- s$values[seq_len(max_cat)]; s
    })
  }

  type  <- chart_data$type %||% "bar"
  title <- chart_data$title %||% ""
  s_names <- vapply(series, function(s) s$name %||% "series", character(1))
  # Build a numeric matrix: rows = series, cols = categories
  mat <- tryCatch(
    do.call(rbind, lapply(series, function(s) {
      v <- suppressWarnings(as.numeric(s$values))
      length(v) <- length(cats)   # pad/truncate to category count
      v[is.na(v)] <- 0
      v
    })),
    error = function(e) NULL)
  if (is.null(mat)) return(NULL)

  # QDB palette
  pal <- c("#5b1f6e", "#1f6e5b", "#b8860b", "#1f4e6e", "#9e2a2b", "#6e6e1f")

  tmp <- tempfile(fileext = ".png")
  ok <- tryCatch({
    grDevices::png(tmp, width = width, height = height, res = 110)
    on.exit(grDevices::dev.off(), add = TRUE)
    op <- graphics::par(mar = c(8, 5, 3, 1), xpd = NA)
    on.exit(graphics::par(op), add = TRUE)

    if (type == "line") {
      ymax <- max(mat, na.rm = TRUE); ymin <- min(0, min(mat, na.rm = TRUE))
      graphics::plot(NA, xlim = c(1, length(cats)), ylim = c(ymin, ymax * 1.05),
                     xaxt = "n", xlab = "", ylab = "", main = title)
      graphics::axis(1, at = seq_along(cats), labels = cats, las = 2,
                     cex.axis = 0.7)
      for (i in seq_len(nrow(mat))) {
        graphics::lines(seq_along(cats), mat[i, ], col = pal[(i - 1) %% length(pal) + 1],
                        lwd = 2, type = "o", pch = 19, cex = 0.6)
      }
      if (nrow(mat) > 1)
        graphics::legend("topright", legend = s_names, col = pal[seq_len(nrow(mat))],
                         lwd = 2, bty = "n", cex = 0.8)
    } else {
      beside <- (type == "grouped_bar") || nrow(mat) > 1
      bp <- graphics::barplot(mat, beside = beside, names.arg = cats,
                              col = pal[seq_len(nrow(mat))], las = 2,
                              cex.names = 0.7, main = title, border = NA)
      if (nrow(mat) > 1)
        graphics::legend("topright", legend = s_names,
                         fill = pal[seq_len(nrow(mat))], bty = "n", cex = 0.8)
    }
    TRUE
  }, error = function(e) FALSE)

  if (!isTRUE(ok) || !file.exists(tmp)) return(NULL)
  raw <- readBin(tmp, "raw", file.info(tmp)$size)
  unlink(tmp)
  b64 <- tryCatch(jsonlite::base64_enc(raw), error = function(e) NULL)
  if (is.null(b64)) {
    # fallback to base R base64 if available
    b64 <- tryCatch(.b64_encode(raw), error = function(e) NULL)
  }
  if (is.null(b64)) return(NULL)
  paste0("data:image/png;base64,", b64)
}

#' Minimal base64 encoder fallback (used only if jsonlite::base64_enc absent).
.b64_encode <- function(raw) {
  if (requireNamespace("base64enc", quietly = TRUE))
    return(base64enc::base64encode(raw))
  # last resort: openssl
  if (requireNamespace("openssl", quietly = TRUE))
    return(openssl::base64_encode(raw))
  stop("no base64 encoder available")
}

if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
