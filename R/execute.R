#' Code Execution
#'
#' Execute R code with full output capture using the evaluate package.

#' Handle POST /execute
#' @keywords internal
handle_execute <- function(body) {
  if (isTRUE(.mrp_env$is_worker)) {
    return(handle_execute_local(body))
  }

  runtime <- get_runtime()
  worker_dispatch_async(runtime, "handle_execute_local", body = body)
  touch_runtime()
  worker_wait(runtime, pump_http = TRUE)
}

#' Handle POST /execute inside the worker
#' @keywords internal
handle_execute_local <- function(body) {
  code <- body$code %||% ""
  store_history <- body$storeHistory %||% TRUE
  exec_id <- body$execId %||% sprintf("exec-%s", as.numeric(Sys.time()))

  # Default asset_dir to {cwd}/_assets if not provided
  asset_dir <- body$assetDir
  if (is.null(asset_dir) || nchar(asset_dir) == 0) {
    asset_dir <- file.path(.mrp_env$cwd, "_assets")
  }

  runtime <- get_runtime()
  execute_code(runtime, code, store_history, exec_id, asset_dir)
}

#' Handle POST /execute/stream (SSE)
#' @keywords internal
handle_execute_stream <- function(body, cors_headers) {
  code <- body$code %||% ""
  exec_id <- body$execId %||% sprintf("exec-%s", as.numeric(Sys.time()))
  runtime <- get_runtime()

  # Return SSE response
  # httpuv doesn't have native SSE support, so we use a custom approach
  # We execute in the worker, then return SSE-compatible events.

  headers <- c(cors_headers, list(
    "Content-Type" = "text/event-stream",
    "Cache-Control" = "no-cache",
    "Connection" = "keep-alive"
  ))

  # Build SSE response body
  events <- list()

  # Start event
  events <- c(events, format_sse_event("start", list(
    execId = exec_id,
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )))

  worker_dispatch_async(runtime, "handle_execute_local", body = body)
  result <- worker_wait(runtime, pump_http = TRUE)

  if (nzchar(result$stdout %||% "")) {
    events <- c(events, format_sse_event("stdout", list(
      content = result$stdout,
      accumulated = result$stdout
    )))
  }

  if (nzchar(result$stderr %||% "")) {
    events <- c(events, format_sse_event("stderr", list(
      content = result$stderr,
      accumulated = result$stderr
    )))
  }

  if (length(result$displayData %||% list()) > 0) {
    for (display in result$displayData) {
      events <- c(events, format_sse_event("display", list(
        data = display$data %||% list(),
        metadata = display$metadata %||% list()
      )))
    }
  }

  if (length(result$assets %||% list()) > 0) {
    for (asset in result$assets) {
      events <- c(events, format_sse_event("asset", asset))
    }
  }

  if (isTRUE(result$success)) {
    events <- c(events, format_sse_event("result", result))
  } else {
    events <- c(events, format_sse_event("error", result$error %||% list()))
  }

  events <- c(events, format_sse_event("done", list()))
  touch_runtime()

  list(
    status = 200L,
    headers = headers,
    body = paste(events, collapse = "")
  )
}

#' Format an SSE event
#' @keywords internal
format_sse_event <- function(event_type, data) {
  json_data <- if (is.list(data) && length(data) == 0) {
    "{}"
  } else {
    jsonlite::toJSON(data, auto_unbox = TRUE, null = "null")
  }

  sprintf("event: %s\ndata: %s\n\n", event_type, json_data)
}

#' Execute code and return result
#' @keywords internal
execute_code <- function(runtime, code, store_history, exec_id, asset_dir) {
  execute_code_streaming(runtime, code, store_history, exec_id, asset_dir,
    on_output = NULL, on_display = NULL, on_asset = NULL)
}

#' Execute code with streaming callbacks
#' @keywords internal
execute_code_streaming <- function(runtime, code, store_history, exec_id, asset_dir,
                                   on_output = NULL, on_display = NULL, on_asset = NULL) {
  start_time <- Sys.time()
  runtime$current_exec_id <- exec_id
  runtime$interrupted <- FALSE

  # Prepare asset directory
  if (!is.null(asset_dir) && nchar(asset_dir) > 0) {
    if (!dir.exists(asset_dir)) {
      dir.create(asset_dir, recursive = TRUE)
    }
  }

  # Set up graphics device for plot capture
  plot_files <- character(0)
  plot_counter <- 0

  # Custom plot hook for evaluate
  plot_hook <- function(plot, hash) {
    if (!is.null(asset_dir) && nchar(asset_dir) > 0) {
      plot_counter <<- plot_counter + 1
      filename <- sprintf("plot-%s-%d.png", exec_id, plot_counter)
      filepath <- file.path(asset_dir, filename)

      # Save the plot
      png(filepath, width = 800, height = 600, res = 96)
      print(plot)
      dev.off()

      # For streaming mode: emit SSE event (editor will handle insertion)
      # For non-streaming mode: add to plot_files for final result
      if (!is.null(on_asset)) {
        on_asset(list(
          path = filepath,
          url = sprintf("/mrp/v1/assets/%s", filename),
          mimeType = "image/png",
          assetType = "image",
          size = file.info(filepath)$size
        ))
      } else {
        # Non-streaming: add to plot_files for inclusion in final assets
        plot_files <<- c(plot_files, filepath)
      }
    }
    invisible(NULL)
  }

  # Capture output
  stdout_acc <- ""
  stderr_acc <- ""
  display_data <- list()
  assets <- list()

  # Use evaluate to run code
  result_value <- NULL
  error_info <- NULL
  eval_result <- NULL

  # Change to runtime working directory
  old_wd <- getwd()
  setwd(runtime$env$.mrmd_cwd %||% .mrp_env$cwd)
  on.exit(setwd(old_wd), add = TRUE)

  tryCatch({
    # Evaluate the code
    eval_result <- evaluate::evaluate(
      code,
      envir = runtime$env,
      stop_on_error = 0,  # Don't stop on error, capture it
      keep_warning = TRUE,
      keep_message = TRUE,
      output_handler = evaluate::new_output_handler(
        text = function(x) {
          stdout_acc <<- paste0(stdout_acc, x)
          if (!is.null(on_output)) {
            on_output("stdout", x, stdout_acc)
          }
        },
        graphics = plot_hook,
        message = function(x) {
          msg <- conditionMessage(x)
          stderr_acc <<- paste0(stderr_acc, msg, "\n")
          if (!is.null(on_output)) {
            on_output("stderr", paste0(msg, "\n"), stderr_acc)
          }
        },
        warning = function(x) {
          msg <- paste0("Warning: ", conditionMessage(x), "\n")
          stderr_acc <<- paste0(stderr_acc, msg)
          if (!is.null(on_output)) {
            on_output("stderr", msg, stderr_acc)
          }
        },
        # Don't set error handler - let evaluate capture errors in result list
        value = function(x, visible) {
          if (visible) {
            result_value <<- x

            # Check if this is a ggplot object - needs special handling
            if (inherits(x, "ggplot") || inherits(x, "gg")) {
              # Save ggplot as asset
              if (!is.null(asset_dir) && nchar(asset_dir) > 0) {
                plot_counter <<- plot_counter + 1
                filename <- sprintf("plot-%s-%d.png", exec_id, plot_counter)
                filepath <- file.path(asset_dir, filename)

                tryCatch({
                  # Try ggsave if ggplot2 is available (it should be since we have a ggplot object)
                  if (requireNamespace("ggplot2", quietly = TRUE)) {
                    ggplot2::ggsave(filepath, plot = x, width = 8, height = 6, dpi = 96)
                  } else {
                    # Fallback: use png() device
                    png(filepath, width = 800, height = 600, res = 96)
                    print(x)
                    dev.off()
                  }

                  # For streaming mode: emit SSE event (editor will handle insertion)
                  # For non-streaming mode: add to plot_files for final result
                  if (!is.null(on_asset)) {
                    on_asset(list(
                      path = filepath,
                      url = sprintf("/mrp/v1/assets/%s", filename),
                      mimeType = "image/png",
                      assetType = "image",
                      size = file.info(filepath)$size
                    ))
                  } else {
                    # Non-streaming: add to plot_files for inclusion in final assets
                    plot_files <<- c(plot_files, filepath)
                  }
                }, error = function(e) {
                  # Fallback: print error but don't crash
                  stderr_acc <<- paste0(stderr_acc, "Warning: Could not save ggplot: ", e$message, "\n")
                })
              }
            } else {
              # Print non-ggplot value to stdout as R would
              output <- capture.output(print(x))
              output_str <- paste(output, collapse = "\n")
              if (nchar(output_str) > 0) {
                stdout_acc <<- paste0(stdout_acc, output_str, "\n")
                if (!is.null(on_output)) {
                  on_output("stdout", paste0(output_str, "\n"), stdout_acc)
                }
              }
            }
          }
        }
      )
    )
  }, interrupt = function(e) {
    error_info <<- list(
      type = "Interrupt",
      message = "Interrupted",
      traceback = list(),
      line = NULL,
      column = NULL
    )
  }, error = function(e) {
    error_info <<- list(
      type = class(e)[1],
      message = conditionMessage(e),
      traceback = list(),
      line = NULL,
      column = NULL
    )
  })

  # Check for errors in evaluation result (outside tryCatch)
  if (!is.null(eval_result) && is.null(error_info)) {
    for (item in eval_result) {
      if (inherits(item, "error")) {
        error_info <- list(
          type = class(item)[1],
          message = conditionMessage(item),
          traceback = if (!is.null(item$call)) {
            list(deparse(item$call))
          } else {
            list()
          },
          line = NULL,
          column = NULL
        )
        break
      }
    }
  }

  # Update runtime state
  if (store_history) {
    runtime$execution_count <- runtime$execution_count + 1L

    history_index <- runtime$history_next_index %||% 1L
    entry <- list(
      historyIndex = history_index,
      code = code
    )

    runtime$history[[length(runtime$history) + 1L]] <- entry
    runtime$history_next_index <- history_index + 1L

    tryCatch(
      append_history_entry(runtime$history_file, history_index, code),
      error = function(e) NULL
    )
  }
  touch_runtime()
  runtime$current_exec_id <- NULL

  # Build asset list from plot files
  for (filepath in plot_files) {
    filename <- basename(filepath)
    assets <- c(assets, list(list(
      path = filepath,
      url = sprintf("/mrp/v1/assets/%s", filename),
      mimeType = "image/png",
      assetType = "image",
      size = file.info(filepath)$size
    )))
  }

  # Calculate duration
  duration <- as.integer(difftime(Sys.time(), start_time, units = "secs") * 1000)

  # Format result value
  result_str <- NULL
  if (!is.null(result_value) && is.null(error_info)) {
    result_str <- tryCatch(
      paste(capture.output(print(result_value)), collapse = "\n"),
      error = function(e) NULL
    )
  }

  list(
    success = is.null(error_info),
    stdout = stdout_acc,
    stderr = stderr_acc,
    result = result_str,
    error = error_info,
    displayData = display_data,
    assets = assets,
    executionCount = runtime$execution_count,
    duration = duration
  )
}

#' Handle POST /input
#' @keywords internal
handle_input <- function(body) {
  exec_id <- body$exec_id
  text <- body$text %||% ""

  if (is.null(exec_id)) {
    return(list(accepted = FALSE, error = "exec_id is required", status = 400L))
  }

  # Store input for pending request
  if (exec_id %in% names(.mrp_env$pending_inputs)) {
    .mrp_env$input_values[[exec_id]] <- text
    .mrp_env$pending_inputs[[exec_id]] <- TRUE
    return(list(accepted = TRUE))
  }

  list(accepted = FALSE, error = "No pending input request")
}

#' Handle POST /input/cancel
#' @keywords internal
handle_input_cancel <- function(body) {
  exec_id <- body$exec_id

  if (is.null(exec_id)) {
    return(list(cancelled = FALSE, error = "exec_id is required", status = 400L))
  }

  if (exec_id %in% names(.mrp_env$pending_inputs)) {
    .mrp_env$pending_inputs[[exec_id]] <- NULL
    .mrp_env$input_values[[exec_id]] <- NULL
    return(list(cancelled = TRUE))
  }

  list(cancelled = FALSE, error = "No pending input request")
}

#' Handle POST /interrupt
#' @keywords internal
handle_interrupt <- function(body) {
  runtime <- get_runtime()
  session <- runtime$session

  if (is.null(session) || !session$is_alive() || !identical(session$get_state(), "busy")) {
    return(list(interrupted = FALSE))
  }

  interrupted <- tryCatch(
    isTRUE(session$interrupt()),
    error = function(e) FALSE
  )

  list(interrupted = interrupted)
}
