response <- function(body = NULL, statusCode = NULL, contentType = '') {
  structure(
    # Rook requires that Content-Type is set.
    list(body = body,
         headers = list('Content-Type' = contentType),
         status = statusCode),
    class = "response"
  )
}

#' notFoundResponse
#'
#' Response for objects (URL's) that can't be found.
#'
#' @param An optional message to show to the user.
#'
#' @export
notFoundResponse <- function(message = "") {
  structure(
    list(
      status = 404L,
      headers = list(
        'Content-Type' = 'text/plain'
      ),
      body = paste(
        sep = "\r\n",
        "404 Page Not Found",
        message
      )
    ),
    class = "response"
  )
}

#' errorResponse
#'
#' Response for requests that generate an error.
#'
#' @param An optional message to show to the user.
#'
#' @export
errorResponse <- function(message = "") {
  structure(
    list(
      status = 500L,
      headers = list(
        'Content-Type' = 'text/plain'
      ),
      body = paste(
        sep = "\r\n",
        "500 Internal server error",
        message
      )
    ),
    class = "response"
  )
}

#' Set up response
#'
#' Set up an empty response to be handled by the attached middlewares.
#'
#' @param req The request triggering the response setup.
#' @param routes The configured routes.
#'
#' @return An empty request with middlewares attached.
setupResponse <- function(req, routes) {
  config <- configure()
  resp <- response()
  resp[["_middlewares"]] <- config[["middlewares"]]
  resp[["_middlewares_index"]] <- 0L
  resp[["_middlewares"]] <- c(resp[["_middlewares"]],
                              function(resp, req) {
                                matchRoutes(routes, resp, req)
                              })
  resp
}

