#' Runtime State Management
#'
#' Manages the single R runtime namespace.
#' One server process = one namespace. Start another process for another namespace.

#' Initialize the runtime state
#' @keywords internal
init_runtime <- function() {
  # Create isolated environment with globalenv as parent
  # This allows access to base R functions while isolating user variables
  user_env <- new.env(parent = globalenv())
  user_env$.mrmd_cwd <- .mrp_env$cwd

  history_file <- file.path(.mrp_env$cwd, ".Rhistory")
  history_state <- load_history_file(history_file)

  runtime <- new.env(parent = emptyenv())
  runtime$env <- user_env
  runtime$execution_count <- 0L
  runtime$history <- history_state$entries
  runtime$history_file <- history_file
  runtime$history_next_index <- history_state$next_index
  runtime$created <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  runtime$last_activity <- runtime$created
  runtime$current_exec_id <- NULL
  runtime$interrupted <- FALSE

  .mrp_env$runtime <- runtime
}

#' Get the runtime state, initializing if needed
#' @return Runtime state environment
#' @keywords internal
get_runtime <- function() {
  if (is.null(.mrp_env$runtime)) {
    init_runtime()
  }
  .mrp_env$runtime
}

#' Reset the runtime namespace (clear all user variables)
#' @keywords internal
reset_runtime <- function() {
  init_runtime()
}

#' Update runtime activity timestamp
#' @keywords internal
touch_runtime <- function() {
  runtime <- get_runtime()
  runtime$last_activity <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' Load persisted history from .Rhistory
#' @keywords internal
load_history_file <- function(path) {
  if (!file.exists(path)) {
    return(list(entries = list(), next_index = 1L))
  }

  lines <- tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character(0)
  )

  entries <- list()
  next_index <- 1L
  i <- 1L

  while (i <= length(lines)) {
    line <- lines[[i]]

    if (startsWith(line, "# MRMD-HISTORY-START ")) {
      history_index <- suppressWarnings(as.integer(sub("^# MRMD-HISTORY-START ", "", line)))
      if (is.na(history_index)) {
        history_index <- next_index
      }

      i <- i + 1L
      block <- character(0)
      while (i <= length(lines) && !identical(lines[[i]], "# MRMD-HISTORY-END")) {
        block <- c(block, lines[[i]])
        i <- i + 1L
      }

      entries[[length(entries) + 1L]] <- list(
        historyIndex = history_index,
        code = paste(block, collapse = "\n")
      )
      next_index <- max(next_index, history_index + 1L)

      if (i <= length(lines) && identical(lines[[i]], "# MRMD-HISTORY-END")) {
        i <- i + 1L
      }
      if (i <= length(lines) && identical(lines[[i]], "")) {
        i <- i + 1L
      }
      next
    }

    if (nzchar(line)) {
      entries[[length(entries) + 1L]] <- list(
        historyIndex = next_index,
        code = line
      )
      next_index <- next_index + 1L
    }

    i <- i + 1L
  }

  list(entries = entries, next_index = next_index)
}

#' Append one history entry to .Rhistory
#' @keywords internal
append_history_entry <- function(path, history_index, code) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  code <- gsub("\r\n?", "\n", code %||% "")
  needs_block <- grepl("\n", code, fixed = TRUE) ||
    startsWith(code, "# MRMD-HISTORY-START ") ||
    identical(code, "# MRMD-HISTORY-END")

  if (needs_block) {
    payload <- paste0(
      "# MRMD-HISTORY-START ", history_index, "\n",
      code,
      "\n# MRMD-HISTORY-END\n\n"
    )
  } else {
    payload <- paste0(code, "\n")
  }

  cat(payload, file = path, append = TRUE, sep = "")
}
