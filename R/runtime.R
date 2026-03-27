#' Runtime State Management
#'
#' Manages the single R runtime namespace.
#' The HTTP server runs in the main process and delegates stateful runtime
#' operations to a persistent background R worker so `/interrupt` can be
#' serviced while code is executing.

#' Initialize the runtime state
#' @keywords internal
init_runtime <- function() {
  if (isTRUE(.mrp_env$is_worker)) {
    init_runtime_local()
  } else {
    init_runtime_parent()
  }
}

#' Initialize the parent-side runtime state
#' @keywords internal
init_runtime_parent <- function() {
  old_runtime <- .mrp_env$runtime
  if (!is.null(old_runtime) && !is.null(old_runtime$session)) {
    stop_worker_session(old_runtime$session)
  }

  runtime <- new.env(parent = emptyenv())
  runtime$created <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  runtime$last_activity <- runtime$created
  runtime$session <- create_worker_session(.mrp_env$cwd)

  .mrp_env$runtime <- runtime
}

#' Initialize the worker-side runtime state
#' @keywords internal
init_runtime_local <- function() {
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
  runtime <- .mrp_env$runtime

  if (is.null(runtime)) {
    init_runtime()
    return(.mrp_env$runtime)
  }

  if (!isTRUE(.mrp_env$is_worker)) {
    session <- runtime$session
    if (is.null(session) || identical(session$get_state(), "finished") || !session$is_alive()) {
      init_runtime_parent()
      return(.mrp_env$runtime)
    }
  }

  runtime
}

#' Reset the runtime namespace (clear all user variables)
#' @keywords internal
reset_runtime <- function() {
  if (isTRUE(.mrp_env$is_worker)) {
    init_runtime_local()
  } else {
    runtime <- get_runtime()
    worker_dispatch_sync(runtime, "handle_reset_local")
    runtime$last_activity <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }
}

#' Update runtime activity timestamp
#' @keywords internal
touch_runtime <- function() {
  runtime <- get_runtime()
  runtime$last_activity <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' Detect the package root for sourcing worker code in development mode
#' @keywords internal
get_package_root <- function() {
  from_option <- getOption("mrmdr.package_root", NULL)
  if (!is.null(from_option) && file.exists(file.path(from_option, "DESCRIPTION"))) {
    return(normalizePath(from_option, mustWork = FALSE))
  }

  from_pkg <- tryCatch(find.package("mrmdr"), error = function(e) NULL)
  if (!is.null(from_pkg) && nzchar(from_pkg)) {
    return(normalizePath(from_pkg, mustWork = FALSE))
  }

  NULL
}

#' Start the persistent worker session
#' @keywords internal
create_worker_session <- function(cwd) {
  session <- callr::r_session$new(wait = TRUE)
  init_worker_session(session, cwd)
  session
}

#' Initialize the child R session with mrmd-r handlers
#' @keywords internal
init_worker_session <- function(session, cwd) {
  package_root <- get_package_root()

  session$run(function(package_root, cwd) {
    source_paths <- c(
      file.path(package_root, "R", "server.R"),
      file.path(package_root, "R", "runtime.R"),
      file.path(package_root, "R", "execute.R"),
      file.path(package_root, "R", "complete.R"),
      file.path(package_root, "R", "variables.R"),
      file.path(package_root, "R", "utils.R")
    )

    if (!is.null(package_root) && all(file.exists(source_paths))) {
      for (path in source_paths) {
        sys.source(path, envir = .GlobalEnv)
      }
    } else {
      suppressPackageStartupMessages(library(mrmdr))
    }

    .mrp_env$is_worker <- TRUE
    .mrp_env$server <- NULL
    .mrp_env$cwd <- normalizePath(cwd, mustWork = FALSE)
    .mrp_env$r_version <- paste(R.version$major, R.version$minor, sep = ".")
    .mrp_env$r_executable <- file.path(R.home("bin"), "R")
    .mrp_env$pending_inputs <- list()
    .mrp_env$input_values <- list()

    .mrp_env$.dispatch_handler <- function(handler_name, args = list()) {
      resolver <- function(name) {
        if (exists(name, envir = .GlobalEnv, inherits = FALSE)) {
          return(get(name, envir = .GlobalEnv, inherits = FALSE))
        }
        getFromNamespace(name, "mrmdr")
      }

      do.call(resolver(handler_name), args)
    }

    init_runtime_local()
    TRUE
  }, args = list(package_root, cwd))
}

#' Stop the worker session if it exists
#' @keywords internal
stop_worker_session <- function(session) {
  if (is.null(session)) {
    return(invisible(FALSE))
  }

  tryCatch(
    session$close(),
    error = function(e) NULL
  )

  invisible(TRUE)
}

#' Run a handler synchronously in the worker session
#' @keywords internal
worker_dispatch_sync <- function(runtime, handler_name, ...) {
  args <- list(...)
  runtime$session$run(
    function(handler_name, args) {
      .mrp_env$.dispatch_handler(handler_name, args)
    },
    args = list(handler_name, args)
  )
}

#' Start a handler asynchronously in the worker session
#' @keywords internal
worker_dispatch_async <- function(runtime, handler_name, ...) {
  args <- list(...)
  runtime$session$call(
    function(handler_name, args) {
      .mrp_env$.dispatch_handler(handler_name, args)
    },
    args = list(handler_name, args)
  )
}

#' Wait for an async worker call to complete
#' @keywords internal
worker_wait <- function(runtime, pump_http = FALSE, poll_ms = 25L) {
  repeat {
    status <- runtime$session$poll_process(poll_ms)

    if (identical(status, "ready")) {
      break
    }

    if (pump_http) {
      tryCatch(later::run_now(timeoutSecs = 0), error = function(e) NULL)
      Sys.sleep(0.001)
    }
  }

  worker_read(runtime)
}

#' Read the completed result of an async worker call
#' @keywords internal
worker_read <- function(runtime) {
  event <- runtime$session$read()

  if (is.null(event)) {
    stop("Worker did not produce a result")
  }

  code <- suppressWarnings(as.integer(event$code %||% NA_integer_))

  if (!identical(code, 200L)) {
    stop(event$message %||% "Worker session failed")
  }

  if (!is.null(event$error)) {
    err <- conditionMessage(event$error)
    stop(if (nzchar(err)) err else "Worker handler failed")
  }

  event$result
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
