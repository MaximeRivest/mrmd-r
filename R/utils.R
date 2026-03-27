#' Utility Functions
#'
#' Miscellaneous utility functions and remaining handlers.

#' Handle POST /is_complete
#' @keywords internal
handle_is_complete <- function(body) {
  code <- body$code %||% ""

  # Try to parse the code
  result <- tryCatch({
    parse(text = code)
    list(status = "complete", indent = "")
  }, error = function(e) {
    msg <- conditionMessage(e)

    # Check for incomplete code patterns
    if (grepl("unexpected end of input", msg, ignore.case = TRUE)) {
      list(status = "incomplete", indent = "  ")
    } else if (grepl("unexpected", msg, ignore.case = TRUE)) {
      list(status = "invalid", indent = "")
    } else {
      list(status = "unknown", indent = "")
    }
  })

  result
}

#' Handle POST /format
#' @keywords internal
handle_format <- function(body) {
  code <- body$code %||% ""

  # R doesn't have a built-in formatter like black/prettier

  # Could integrate styler package if available

  if (requireNamespace("styler", quietly = TRUE)) {
    formatted <- tryCatch({
      styler::style_text(code)
    }, error = function(e) code)

    formatted_str <- paste(formatted, collapse = "\n")
    list(
      formatted = formatted_str,
      changed = formatted_str != code
    )
  } else {
    list(
      formatted = code,
      changed = FALSE,
      error = "Formatting not available (install 'styler' package)"
    )
  }
}

#' Handle POST /history
#' @keywords internal
handle_history <- function(body) {
  if (!isTRUE(.mrp_env$is_worker)) {
    runtime <- get_runtime()
    touch_runtime()
    return(worker_dispatch_sync(runtime, "handle_history_local", body = body))
  }

  handle_history_local(body)
}

#' Handle POST /history inside the worker
#' @keywords internal
handle_history_local <- function(body) {
  runtime <- get_runtime()

  n <- body$n %||% 20L
  pattern <- body$pattern %||% NULL
  before <- body$before %||% NULL

  n <- suppressWarnings(as.integer(n))
  if (is.na(n) || n <= 0L) {
    n <- 20L
  }

  entries <- runtime$history
  if (length(entries) == 0) {
    return(list(entries = list(), hasMore = FALSE))
  }

  if (!is.null(before)) {
    before <- suppressWarnings(as.integer(before))
    if (!is.na(before)) {
      entries <- Filter(function(entry) {
        isTRUE(entry$historyIndex < before)
      }, entries)
    }
  }

  if (!is.null(pattern) && nzchar(pattern)) {
    rx <- mrp_glob_to_regex(pattern)
    entries <- Filter(function(entry) {
      code <- entry$code %||% ""
      isTRUE(grepl(rx, code, perl = TRUE))
    }, entries)
  }

  total <- length(entries)
  if (total == 0) {
    return(list(entries = list(), hasMore = FALSE))
  }

  start <- max(1L, total - n + 1L)
  list(
    entries = entries[start:total],
    hasMore = start > 1L
  )
}

#' Convert a simple glob pattern to a regex
#' @keywords internal
mrp_glob_to_regex <- function(pattern) {
  chars <- strsplit(pattern %||% "", "", fixed = TRUE)[[1]]
  escaped <- vapply(chars, function(ch) {
    if (identical(ch, "*")) {
      ".*"
    } else if (identical(ch, "?")) {
      "."
    } else if (grepl("[][{}()+^$.|\\\\]", ch, perl = TRUE)) {
      paste0("\\", ch)
    } else {
      ch
    }
  }, character(1))

  paste0("(?s)^", paste(escaped, collapse = ""), "$")
}

#' Handle GET /assets/{path}
#' @keywords internal
handle_assets <- function(asset_path) {
  # Construct full path
  # Assets are stored relative to the current working directory's _assets folder
  full_path <- file.path(.mrp_env$cwd, "_assets", asset_path)

  if (!file.exists(full_path)) {
    return(list(error = "Asset not found", status = 404L))
  }

  # Determine MIME type
  ext <- tolower(tools::file_ext(full_path))
  mime_types <- list(
    png = "image/png",
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    gif = "image/gif",
    svg = "image/svg+xml",
    pdf = "application/pdf",
    html = "text/html",
    json = "application/json"
  )

  mime_type <- mime_types[[ext]] %||% "application/octet-stream"

  # Read file
  content <- tryCatch({
    readBin(full_path, "raw", file.info(full_path)$size)
  }, error = function(e) NULL)

  if (is.null(content)) {
    return(list(error = "Failed to read asset", status = 500L))
  }

  # Return raw response (httpuv handles this differently)
  list(
    status = 200L,
    headers = list("Content-Type" = mime_type),
    body = content,
    raw = TRUE
  )
}

#' Parse command line arguments
#' @keywords internal
parse_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  defaults <- list(
    host = "127.0.0.1",
    port = 8001,
    cwd = getwd()
  )

  i <- 1
  while (i <= length(args)) {
    arg <- args[i]

    if (arg == "--host" && i < length(args)) {
      defaults$host <- args[i + 1]
      i <- i + 2
    } else if (arg == "--port" && i < length(args)) {
      defaults$port <- as.integer(args[i + 1])
      i <- i + 2
    } else if (arg == "--cwd" && i < length(args)) {
      defaults$cwd <- args[i + 1]
      i <- i + 2
    } else if (arg == "--help" || arg == "-h") {
      cat("Usage: mrmd-r [options]\n\n")
      cat("Options:\n")
      cat("  --host HOST    Host to bind to (default: 127.0.0.1)\n")
      cat("  --port PORT    Port to bind to (default: 8001)\n")
      cat("  --cwd PATH     Working directory (default: current directory)\n")
      cat("  --help, -h     Show this help message\n")
      quit(save = "no", status = 0)
    } else {
      i <- i + 1
    }
  }

  defaults
}

#' Main entry point (called from CLI script)
#' @export
main <- function() {
  args <- parse_args()
  start_server(host = args$host, port = args$port, cwd = args$cwd, blocking = TRUE)
}
