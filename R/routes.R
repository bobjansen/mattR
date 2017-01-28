#' Create routes from a list of urls and handlers
#' @param ... A list of c(url, handler)'s
#'
#' @export
#'
#' @examples
#' \dontrun{
#' createRoutes(
#'    c("index.html", function(...) 1),
#'    c("contact.html", function(...) 2)
#'}
createRoutes <- function(...) {
  routes <- list(...)
  lapply(routes, function(route) {
    createRoute(route[[1]], route[[2]])
  })
}

matchRequest <- function(request, pattern) {
  regexpr(pattern, request$PATH_INFO) == 1
}

createRoute <- function(path, handler) {
  function(request) {
    if (matchRequest(request, path)) {
      response <- handler(request)
      if (is.null(response)) {
        stop("Handler for", path, "returned NULL, should be response object.")
      } else {
        response
      }
    } else {
      NULL
    }
  }
}

matchRoutes <- function(routes, request) {
  for (route in routes) {
    response <- route(request)
    if (!is.null(response)) {
      return(response)
    }
  }
  notFoundResponse()
}

