#' Build a mattR app object for serving by httpuv
#'
#' @return The app object
#' @export
#'
#' @examples
#' \dontrun{
#' buildApp()
#' }
buildApp <- function(config) {

  app <- list(
    call = function(req) {
      debug <- getConfigOrDefault(config, "debug", FALSE)
      if (debug) {
        print(paste(req$REQUEST_METHOD, "request on URL:", req$PATH_INFO))
      }
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
  config <- mattR::configure()

  host <- "0.0.0.0"
  port <- as.numeric(getConfigOrDefault(config, "port",
                                        sample(1025:(2^16 - 1), 1)))

  app <- buildApp(config)

  print(paste("Starting app on:", paste0(host, ":", port)))
  httpuv::runServer(host, port, app)
}
