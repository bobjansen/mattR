#' Build a mattR app object for serving by httpuv
#'
#' @return The app object
#' @export
#'
#' @examples
#' \dontrun{
#' buildApp()
#' }
buildApp <- function() {
  app <- list(
    call = function(req) {
      list(
        status = 200L,
        headers = list(
          'Content-Type' = 'text/html'
        ),
        body = paste(
          sep = "\r\n",
          "foo"
        )
      )
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
  config <- mattR::config()

  host <- "0.0.0.0"
  if (!"port" %in% names(config)) {
    port <- sample(1025:(2^16 - 1), 1)
  } else {
    port <- as.numeric(config[["port"]])
  }

  app <- mattR::buildApp()

  print(paste("Starting app on:", paste0(host, ":", port)))
  httpuv::runServer(host, port, app)
}
