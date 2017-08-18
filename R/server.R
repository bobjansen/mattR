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

  routes <- getRoutes(debug)

  app <- list(
    call = function(request) {
      if (debug) {
        print(paste(request$REQUEST_METHOD, "request on URL:",
                    request$PATH_INFO))
      }

      response <- matchRoutes(routes, request)

      if (debug) {
        print(paste("Response for", request$REQUEST_METHOD, "request on URL:",
                    request$PATH_INFO, "has status", response$status))
      }

      response
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
#' @return The httpuv handle to the server process
#' @export
#'
#' @examples
#' \dontrun{
#' runTestServer()
#' }
runTestServer <- function() {
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

  cat("Use Ctrl-C to stop\n")
  httpuv::runServer(host, port, app)
}


#' Kill a process by the given token.
#'
#' @param token Token of the process to kill.
#'
#' @return TRUE on success, FALSE on failure.
#' @export
killByToken <- function(token) {
  procs <- system2("ps", args = "aux", stdout = TRUE)

  lines <- grep(paste("mattR --args", token), procs)
  if (length(lines) > 1) {
    stop("Multiple processes found with supposedly unique token")
  } else if (length(lines) == 0) {
    warning(paste("No process found for token:", token))
    FALSE
  } else {
    system2("kill", args = strsplit(procs[[lines[1]]], "\\s+")[[1]][[2]])
    TRUE
  }
}

#' startServerProcess
#'
#' Start a server as an external process, killable by killByToken(output)
#'
#' @param logDir The default logging directory.
#' @return The probably unique token of the process.
#' @export
#'
#' @examples
#' \dontrun{
#' startServerProcess()
#' }
startServerProcess <- function(logDir = getwd()) {
  scriptName <- system.file("scripts", "mattR", package = "mattR")

  token <- as.character(sample(1:1e6, 1))

  system2(scriptName,
          wait = FALSE,
          args = token,
          stdout = file.path(logDir, "stdout.log"),
          stderr = file.path(logDir, "stderr.log"))

  token
}
