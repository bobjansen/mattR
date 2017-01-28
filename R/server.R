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

  routes <- mattR::createRoutes(
    c("/index.html", mattR::staticView("/opt/code/mattR/inst/static/", "/")),
    c("/static/*", mattR::staticView("/opt/code/mattR/inst/static/", "/static"))
  )

  app <- list(
    call = function(request) {
      debug <- getConfigOrDefault(config, "debug", FALSE)
      if (debug) {
        print(paste(request$REQUEST_METHOD, "request on URL:",
                    request$PATH_INFO))
      }
      matchRoutes(routes, request)
    },
    onWSOpen = function(ws) {
      ws$onMessage(function(binary, message) {
        ws$send(message)
      })
    }
  )

  app
}

handleRequest <- function(request, debug = FALSE) {
  content <- if (request$PATH_INFO == "/") {
    "HOME"
  } else if (startsWith(request$PATH_INFO, "/static/")) {
    fileName <- system.file(request$PATH_INFO, package = "mattR")
    if (file.exists(fileName)) {
      readChar(fileName, file.info(fileName)$size)
    } else {
      "404"
    }
  } else {
    "NOT HOME"
  }

  list(
    status = 200L,
    headers = list(
      # Invalid, but the browser is pretty smart.
      'Content-Type' = ''
    ),
    body = paste(
      sep = "\r\n",
      content, ifelse(debug, request$PATH_INFO, "")
    )
  )
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

  host <- "0.0.0.0"
  port <- as.numeric(mattR::getConfigOrDefault(config, "port",
                                               sample(1025:(2^16 - 1), 1)))

  app <- buildApp(config)

  print(paste("Starting app on:", paste0(host, ":", port)))
  httpuv::runServer(host, port, app)
}


#' Kill a process by the given token.
#'
#' @param token Token of the process to kill.
#'
#' @return TRUE on success, FALSE on failure.
#' @export
#'
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

#' Start a server as an external process, killable by killByToken(output)
#'
#' @return The probably unique token of the process.
#' @export
#'
#' @examples
#' \dontrun{
#' startServerProcess()
#' }
startServerProcess <- function() {
  scriptName <- system.file("scripts", "mattR", package = "mattR")

  token <- as.character(sample(1:1e6, 1))

  system2(scriptName, wait = FALSE, args = token, stdout = "/opt/code/mattR/stdout.log", stderr = "/opt/code/mattR/stderr.log")

  token
}
