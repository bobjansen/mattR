#' createRoutes
#'
#' Create routes from a list of urls and handlers
#'
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

#' Return a function that dispatches to `handler()` if `path` matches.
#' @param path URL path.
#' @param handler Handler for this request given the path.
#' @return A response if the path is matched, NULL if the path is not matched.
createRoute <- function(path, handler) {
  function(request) {
    if (matchRequest(request, path)) {
      response <- handler(request)
      if (is.null(response)) {
        stop("Handler for ", path,
             " returned NULL, should be response object.")
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

getRoutes <- function(debug = FALSE) {
  # The file specified by routesPath should create a `routes` variable.
  # Defining beforehand prevents a note being issued by `R CMD check`.
  routes <- NULL
  routesPath <- file.path(getwd(), "routes.R")
  if (file.exists(routesPath)) {
    source(routesPath, local = TRUE)
  } else {
    source(system.file("defaults", "routes.R", package = "mattR"), local = TRUE)
  }

  if (debug) {
    cat(paste0("Path of routes file is: ", routesPath, "\n"))
    #print(routes)
  }

  if (is.null(routes)) {
    stop("The routes.R file should define a variable 'routes'.")
  }
  routes
}

