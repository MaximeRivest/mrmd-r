#' MRP Server for R
#'
#' HTTP server implementing the MRP (MRMD Runtime Protocol) for R.
#'
#' @import httpuv
#' @import jsonlite
#' @import later
#' @importFrom evaluate evaluate

# Global state
.mrp_env <- new.env(parent = emptyenv())
.mrp_env$runtime <- NULL
.mrp_env$pending_inputs <- list()
.mrp_env$input_values <- list()

#' Start the MRP server
#'
#' @param host Host to bind to (default: "127.0.0.1")
#' @param port Port to bind to (default: 8001)
#' @param cwd Working directory for the R runtime (default: getwd())
#' @param blocking If TRUE, block until server is stopped (default: TRUE)
#' @export
start_server <- function(host = "127.0.0.1", port = 8001, cwd = getwd(), blocking = TRUE) {
  .mrp_env$cwd <- normalizePath(cwd, mustWork = FALSE)
  .mrp_env$r_version <- paste(R.version$major, R.version$minor, sep = ".")
  .mrp_env$r_executable <- file.path(R.home("bin"), "R")

  message(sprintf("Starting mrmd-r server..."))
  message(sprintf("  Host: %s", host))
  message(sprintf("  Port: %d", port))
  message(sprintf("  Working directory: %s", .mrp_env$cwd))
  message(sprintf("  URL: http://%s:%d/mrp/v1/capabilities", host, port))

  app <- list(
    call = function(req) {
      route_request(req)
    }
  )

  server <- httpuv::startServer(host, port, app)
  .mrp_env$server <- server

  # Initialize runtime
  init_runtime()

  message("\nServer started. Press Ctrl+C to stop.")

  if (blocking) {
    on.exit({
      message("\nShutting down...")
      httpuv::stopServer(server)
    })

    # Run the event loop
    while (TRUE) {
      later::run_now(timeoutSecs = 1)
      Sys.sleep(0.01)
    }
  }

  invisible(server)
}

#' Stop the MRP server
#' @export
stop_server <- function() {
  if (!is.null(.mrp_env$server)) {
    httpuv::stopServer(.mrp_env$server)
    .mrp_env$server <- NULL
    .mrp_env$runtime <- NULL
    message("Server stopped.")
  }
}

#' Route incoming HTTP requests
#' @keywords internal
route_request <- function(req) {
  path <- req$PATH_INFO
  method <- req$REQUEST_METHOD

  # Parse JSON body for POST requests
  body <- NULL
  if (method == "POST" && !is.null(req$rook.input)) {
    body_raw <- req$rook.input$read_lines()
    if (length(body_raw) > 0 && nchar(body_raw) > 0) {
      body <- tryCatch(
        jsonlite::fromJSON(body_raw, simplifyVector = FALSE),
        error = function(e) list()
      )
    }
  }

  # CORS headers for all responses
  cors_headers <- list(
    "Access-Control-Allow-Origin" = "*",
    "Access-Control-Allow-Methods" = "GET, POST, DELETE, OPTIONS",
    "Access-Control-Allow-Headers" = "Content-Type"
  )

  # Handle CORS preflight

  if (method == "OPTIONS") {
    return(list(
      status = 204L,
      headers = cors_headers,
      body = ""
    ))
  }

  # Route to handlers
  result <- tryCatch({
    if (path == "/mrp/v1/capabilities" && method == "GET") {
      handle_capabilities()
    } else if (path == "/mrp/v1/reset" && method == "POST") {
      handle_reset()
    } else if (path == "/mrp/v1/execute" && method == "POST") {
      handle_execute(body)
    } else if (path == "/mrp/v1/execute/stream" && method == "POST") {
      return(handle_execute_stream(body, cors_headers))
    } else if (path == "/mrp/v1/input" && method == "POST") {
      handle_input(body)
    } else if (path == "/mrp/v1/input/cancel" && method == "POST") {
      handle_input_cancel(body)
    } else if (path == "/mrp/v1/interrupt" && method == "POST") {
      handle_interrupt(body)
    } else if (path == "/mrp/v1/complete" && method == "POST") {
      handle_complete(body)
    } else if (path == "/mrp/v1/inspect" && method == "POST") {
      handle_inspect(body)
    } else if (path == "/mrp/v1/hover" && method == "POST") {
      handle_hover(body)
    } else if (path == "/mrp/v1/variables" && method == "POST") {
      handle_variables(body)
    } else if (grepl("^/mrp/v1/variables/", path) && method == "POST") {
      var_name <- sub("^/mrp/v1/variables/", "", path)
      handle_variable_detail(var_name, body)
    } else if (path == "/mrp/v1/is_complete" && method == "POST") {
      handle_is_complete(body)
    } else if (path == "/mrp/v1/format" && method == "POST") {
      handle_format(body)
    } else if (path == "/mrp/v1/history" && method == "POST") {
      handle_history(body)
    } else if (grepl("^/mrp/v1/assets/", path) && method == "GET") {
      asset_path <- sub("^/mrp/v1/assets/", "", path)
      handle_assets(asset_path)
    } else {
      list(error = "Not found", status = 404L)
    }
  }, error = function(e) {
    list(error = conditionMessage(e), status = 500L)
  })

  # Build response
  status <- if (!is.null(result$status)) result$status else 200L
  result$status <- NULL

  # Check if this is a raw binary response (e.g., assets)
  if (isTRUE(result$raw)) {
    # Return binary data directly
    headers <- c(cors_headers, result$headers)
    return(list(
      status = status,
      headers = headers,
      body = result$body
    ))
  }

  # Default: JSON response
  headers <- c(cors_headers, list("Content-Type" = "application/json"))

  list(
    status = status,
    headers = headers,
    body = jsonlite::toJSON(result, auto_unbox = TRUE, null = "null")
  )
}

#' Handle GET /capabilities
#' @keywords internal
handle_capabilities <- function() {
  list(
    runtime = "r",
    version = .mrp_env$r_version,
    languages = list("r", "R", "rlang"),
    features = list(
      execute = TRUE,
      executeStream = TRUE,
      interrupt = TRUE,
      complete = TRUE,
      inspect = TRUE,
      hover = TRUE,
      variables = TRUE,
      variableExpand = TRUE,
      reset = TRUE,
      isComplete = TRUE,
      format = FALSE,
      history = TRUE,
      assets = TRUE
    ),
    lspFallback = NULL,
    environment = list(
      cwd = .mrp_env$cwd,
      executable = .mrp_env$r_executable,
      shell = NULL
    )
  )
}

#' Handle POST /reset
#' @keywords internal
handle_reset <- function() {
  reset_runtime()
  list(success = TRUE)
}

# Null-coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a
