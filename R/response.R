response <- function(body = NULL, statusCode = NULL, contentType = '') {
  structure(
    # Rook requires taht Content-Type is set.
    list(body = body,
         headers = list('Content-Type' = contentType),
         status = statusCode),
    class = "response"
  )
}

notFoundResponse <- function() {
  structure(
    list(
      status = 404L,
      headers = list(
        'Content-Type' = 'text/plain'
      ),
      body = paste(
        sep = "\r\n",
        "404 Page Not Found"
      )
    ),
    class = "response"
  )
}

#' Set up response
#'
#' Set up an empty response to be handled by the attached middlewares.
#'
#' @return An empty request with middlewares attached.
setupResponse <- function(req) {
  config <- configure()

  resp <- response()
  resp[["_middlewares"]] <- config[["middlewares"]]
  resp[["_middlewares_index"]] <- 0L

  debug <- getConfigOrDefault(config, "debug", FALSE)
  routes <- getRoutes(debug)

  resp[["_middlewares"]] <- c(resp[["_middlewares"]],
                              function(resp, req) {
                                matchRoutes(routes, resp, req)
                              })

  resp
}

