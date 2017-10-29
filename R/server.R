#' Build a mattR app object for serving by httpuv
#'
#' @param config List containing the configuration for this server
#' @return The app object
#' @export
#'
#' @examples
#' \dontrun{
#' buildApp()
#' }
buildApp <- function(config) {

  debug <- getConfigOrDefault(config, "debug", FALSE)

  app <- list(
    call = function(request) {
      if (debug) { # nocov start
        message(paste(request[["REQUEST_METHOD"]], "request on URL:",
                      request[["PATH_INFO"]])) # nocov end
      }

      resp <- getResponse(setupResponse(request), request)
      if (!"status" %in% names(resp) || is.null(resp[["status"]])) {
        warning("Response without status, adding status 200")
        resp[["status"]] <- 200L
      }

      if (debug) { # nocov start
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

  if (mattR::getConfigOrDefault(config, "debug", FALSE)) {
    message("* debug is on.\n") # nocov
  }

  message("* R Version:",
          paste0(R.version[["major"]], ".", R.version[["minor"]]),
          "\n")

  host <- "0.0.0.0"
  port <- as.numeric(mattR::getConfigOrDefault(config, "port",
                                               sample(1025:(2^16 - 1), 1)))

  message(paste0("* Listening on tcp://", host, ":", port, "\n"))

  app <- buildApp(config)

  if (daemonized) {
    .pkgenv[["handle"]] <- httpuv::startDaemonizedServer(host, port, app)
  } else { # nocov start
    message("Use Ctrl-C to stop\n")
    httpuv::runServer(host, port, app) # nocov end
    on.exit(runExitHandlers())
  }

  # Closing the handle twice using httpuv::stopDaemonizedServer will crash R.
  # Therefore handle management is done by mattR and the handle is never returned.
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

