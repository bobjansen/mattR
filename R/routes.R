#' Create routes from a list of urls and handlers
#' @param ... A list of c(url, handler)'s
#' @export
#' \dontrun{
#' createRoutes(
#'    c("index.html", function(...) 1),
#'    c("contact.html", function(...) 2)
#' }
createRoutes <- function(...) {
  routes <- list(...)
  lapply(routes, function(route) {
    url(route[[1]], route[[2]])
  })
}

matchRequest <- function(request, pattern) {
  regexpr(pattern, request$PATH_INFO) == 1
}

url <- function(path, handler) {
  function(request) {
    if (matchRequest(request, path)) {
      response <- handler(request, path)
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
  NULL
}
