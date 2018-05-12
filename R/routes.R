matchRequest <- function(request, pattern) {
  regMatch <- regexpr(pattern, request[["PATH_INFO"]], perl = TRUE)
  if (regMatch == 1) {
    regMatch
  } else {
    NULL
  }
}

matchRoutes <- function(routes, resp, req) {
  for (route in routes) {
    regMatch <- matchRequest(req, route[[1]])
    if (!is.null(regMatch)) {
      req[["RegExpMatch"]] <- regMatch
      return(route[[2]](resp, req))
    }
  }
  notFoundResponse("Page Not Found")
}

getRoutesFromFile <- function(appState) {
  # The file specified by routesPath should create a `routes` variable.
  # Defining beforehand prevents a note being issued by `R CMD check`.
  with(appState, {
    routes <- NULL
    routesPath <- file.path(getwd(), "routes.R")
    if (file.exists(routesPath)) {
      source(routesPath, local = TRUE) # nocov
    } else {
      source(system.file("defaults", "routes.R", package = "mattR"),
             local = TRUE)
    }

    if (appState[["mattR_debug"]]) { # nocov start
      message(paste("Path of the user routes file would be:",
                    routesPath)) # nocov end
    }

    if (is.null(routes)) {
      stop("The routes.R file should define a variable 'routes'.") # nocov
    }
    routes
  })
}

