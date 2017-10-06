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
      if (debug) {
        print(paste(request[["REQUEST_METHOD"]], "request on URL:",
                    request[["PATH_INFO"]]))
      }

      resp <- getResponse(setupResponse(request), request)

      if (debug) {
        print(paste("Response for", request[["REQUEST_METHOD"]],
                    "request on URL:", request[["PATH_INFO"]], "has status",
                    resp[["status"]]))
      }

      resp
    },
    onWSOpen = function(ws) {
      ws$onMessage(function(binary, message) {
        ws$send(message)
      })
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
    cat("* debug is on.\n")
  }

  cat("* R Version:",
      paste0(R.version[["major"]], ".", R.version[["minor"]]),
      "\n")

  host <- "0.0.0.0"
  port <- as.numeric(mattR::getConfigOrDefault(config, "port",
                                               sample(1025:(2^16 - 1), 1)))

  cat(paste0("* Listening on tcp://", host, ":", port, "\n"))

  app <- buildApp(config)

  if (daemonized) {
    .pkgenv[["handle"]] <- httpuv::startDaemonizedServer(host, port, app)
  } else {
    cat("Use Ctrl-C to stop\n")
    httpuv::runServer(host, port, app)
  }

  # Closing the handle twice using httpuv::stopDaemonizedServer will crash R.
  # Therefore handle management is done by mattR and the handle is never returned.
  invisible()
}

#' isRunning
#'
#' Check whether a daemonized server is running.
#'
#' @return Whether the server is running.
#' @export
#'
#' @examples
#' isRunning()
isRunning <- function() {
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
  httpuv::stopDaemonizedServer(.pkgenv[["handle"]])
  # What to do when stopping fails?
  .pkgenv[["handle"]] <- NULL
  invisible()
}

