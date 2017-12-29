#' buildApp
#'
#' Build a mattR app object for serving by httpuv
#'
#' This will set the variable in the appState environment if it is not set.
#'
#' @param config List containing the configuration for this app.
#' @param routes The routes used in this app.
#' @param appState The initial state of the app.
#' @return The app object
#' @export
#'
#' @examples
#' \dontrun{
#' buildApp()
#' }
buildApp <- function(routes, appState = new.env()) {
  if (!"debug" %in% names(appState)) {
    appState[["debug"]] <- FALSE
  }

  app <- list(
    call = function(request) {
      if (appState[["debug"]]) { # nocov start
        message(paste(request[["REQUEST_METHOD"]], "request on URL:",
                      request[["PATH_INFO"]])) # nocov end
      }

      resp <- tryCatch(
        getResponse(setupResponse(request, routes), request),
        error = function(e) {
          errorResponse(e[["message"]])
      })
      if (!"status" %in% names(resp) || is.null(resp[["status"]])) {
        warning("Response without status, adding status 200")
        resp[["status"]] <- 200L
      }

      if (appState[["debug"]]) { # nocov start
        message(paste("Response for", request[["REQUEST_METHOD"]],
                      "request on URL:", request[["PATH_INFO"]], "has status",
                      resp[["status"]])) # nocov end
      }

      resp
    },
    onWSOpen = function(ws) { # nocov start
      ws$onMessage(function(binary, message) {
        ws$send(message)
      }) # nocov end
    }
  )

  app
}

banner <- function(debug, host, port) {
  if (debug) {
    message("* debug is on.\n") # nocov
  }
  message("* R Version: ",
          paste0(R.version[["major"]], ".", R.version[["minor"]]), "\n",
          "* Started at ", Sys.time())
  message(paste0("* Listening on tcp://", host, ":", port))
}

#' Run a server for testing mattR apps
#'
#' @param daemonized Whether to start the server daemonized.
#' @return The httpuv handle to the server process.
#' @export
#'
#' @examples
#' \dontrun{
#' runTestServer()
#' }
runTestServer <- function(daemonized = FALSE) {
  config <- mattR::configure()
  debug <- getConfigOrDefault(config, "debug", FALSE)

  host <- mattR::getConfigOrDefault(config, "host", "0.0.0.0")
  port <- as.numeric(mattR::getConfigOrDefault(config, "port",
                                               sample(1025:(2^16 - 1), 1)))

  appState <- initFromFile(config)
  routes <- getRoutesFromFile(appState)
  app <- buildApp(routes, appState)

  startTestServer(app, host, port, daemonized, appState[["debug"]])
}

#' startTestServer
#'
#' Run a test server based on the given settings.
#'
#' @param app A rook app.
#' @param host The host to bind on.
#' @param port The port to listen on.
#' @param daemonized Whether to start the server daemonized.
#' @param debug Whether debug is activated.
#' @export
startTestServer <- function(
  app,
  host = "0.0.0.0", port = 8080L,
  daemonized = FALSE, debug = FALSE
) {
  banner(debug, host, port)
  # Closing the handle twice using httpuv::stopDaemonizedServer will crash R.
  # Therefore handle management is not entrusted to the user and done by mattR
  # and the handle is never returned.
  if (daemonized) {
    .pkgenv[["handle"]] <- httpuv::startDaemonizedServer(host, port, app)

  } else { # nocov start
    message("Use Ctrl-C to stop\n")
    httpuv::runServer(host, port, app) # nocov end
    on.exit(runExitHandlers())
  }
  invisible()
}

addExitHandler <- function(FUN) {
  .pkgenv[["exitHandlers"]] <- c(.pkgenv[["exitHandlers"]], FUN)
}

runExitHandlers <- function() {
  for (handler in .pkgenv[["exitHandlers"]]) {
    handler()
  }
}

#' isMattRRunning
#'
#' Check whether a daemonized server is running.
#'
#' @return Whether the server is running.
#' @export
#'
#' @examples
#' isMattRRunning()
isMattRRunning <- function() {
  !is.null(.pkgenv[["handle"]])
}

#' getHandle
#'
#' Retrieve the handle of the server.
getHandle <- function() {
  .pkgenv[["handle"]]
}

#' stopDaemonizedServer
#'
#' Stop a daemonized server
#'
#' @return \code{invisible()}
#'
#' @import httpuv
#'
#' @export
#'
#' @examples
#' \dontrun{
#' stopDaemonizedServer()
#' }
stopDaemonizedServer <- function() {
  httpuv::stopDaemonizedServer(getHandle())
  runExitHandlers()
  # What to do when stopping fails?
  .pkgenv[["handle"]] <- NULL
  invisible()
}

