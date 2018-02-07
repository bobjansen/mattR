#' buildApp
#'
#' Build a mattR app object for serving by httpuv
#'
#' This will set the variable in the appState environment if it is not set.
#'
#' @param routes The routes used in this app.
#' @param modules List of optional modules.
#' @param appState The initial state of the app.
#' @return The app object
#' @export
#'
#' @examples
#' \dontrun{
#' buildApp()
#' }
buildApp <- function(routes, modules = c(), appState = new.env()) {
  if (!"mattR_debug" %in% names(appState)) {
    appState[["mattR_debug"]] <- FALSE
  }

  appState[["modules"]] <- modules
  appState[["routes"]] <- routes

  with(appState, {
    for (module in modules) {
      routes <- module(routes, appState)
    }

    app <- list(
      call = function(request) {
        if (mattR_debug) { # nocov start
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

        if (mattR_debug) { # nocov start
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
  })
}

banner <- function(host, port, mattR_debug = FALSE) {
  if (mattR_debug) {
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
#' @param debug Whether to run in debug mode.
#' @return The httpuv handle to the server process.
#' @export
#'
#' @examples
#' \dontrun{
#' runTestServer()
#' }
runTestServer <- function(daemonized = FALSE, debug = NULL) {
  config <- mattR::configure()

  host <- mattR::getConfigOrDefault(config, "host", "0.0.0.0")
  port <- as.numeric(mattR::getConfigOrDefault(config, "port",
                                               sample(1025:(2^16 - 1), 1)))

  appState <- initFromFile(config, debug)
  routes <- getRoutesFromFile(appState)
  app <- buildApp(routes, getConfigOrDefault(config, "modules", c()), appState)

  startTestServer(app, appState, host, port, daemonized)
}

#' startTestServer
#'
#' Run a test server based on the given settings.
#'
#' @param app A rook app.
#' @param appState State of the app.
#' @param host The host to bind on.
#' @param port The port to listen on.
#' @param daemonized Whether to start the server daemonized.
#' @export
startTestServer <- function(
  app, appState = new.env(),
  host = "0.0.0.0", port = 8080L,
  daemonized = FALSE
) {
  if (!"mattR_debug" %in% names(appState)) {
    appState[["mattR_debug"]] <- FALSE
  }

  appState[["host"]] <- host
  appState[["port"]] <- port
  appState[["app"]] <- app
  appState[["daemonized"]] <- daemonized

  with(appState, {
    banner(host, port, mattR_debug)
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
  })
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

