#' Respond with a static file
#'
#' @param staticDir Location of the static files on the server
#' @param urlPath URL path to the static files
#'
#' @return A response object or NULL if the file is not found.
#' @export
#'
#' @examples
#' \dontrun{
#' staticFun <- staticView("/var/www/static/", "/static/")
#' }
staticView <- function(staticDir, urlPath) {
  function(request) {
    requestPath <- request[["PATH_INFO"]]

    # Ensure that urlPath is a prefix for the requested path.
    if (!startsWith(requestPath, urlPath)) {
      return()
    }

    staticResourceSubPath <- substring(requestPath, nchar(urlPath) + 1)

    if (staticResourceSubPath == "") {
      staticResourceSubPath <- "index.html"
    }
    fileName <- file.path(staticDir, staticResourceSubPath)

    if (file.exists(fileName)) {
      create200Response(readChar(fileName, file.info(fileName)$size))
    } else {
      NULL
    }
  }
}

#' Respond with the result of FUN.
#'
#' @param FUN Function that generates the response as text.
#'
#' @return A response object.
#' @export
#'
#' @import shiny
#'
#' @examples
#' genericView(function() "Hello World!")
genericView <- function(FUN) {
  function(request) {
    params <- if ("QUERY_STRING" %in% names(request)) {
      shiny::parseQueryString(request[["QUERY_STRING"]])
    } else {
      list()
    }
    create200Response(FUN(params))
  }
}

#' Respond with a rendered template
#'
#' @param templateFile Filepath of a whisker template
#' @param data named list or environment with variables that will be used during
#'  rendering
#'
#' @return A response object with as body the rendered template
#' @export
#'
#' @examples
#' templateView(system.file("static", "index.html", package = "mattR"),
#'              list(title = "foo", text = "bar"))
templateView <- function(templateFile, data) {
  if (!file.exists(templateFile)) {
    stop("File '", templateFile, "' doesn't exist")
  }

  function(request) {
    create200Response(renderTemplate(templateFile, data))
  }
}

#' Render a template given data
#'
#' @param templateFile Filepath of a whisker template
#' @param data named list or environment with variables that will be used during
#'  rendering
#'
#' @return the rendered template
#' @export
#'
#' @examples
#' renderTemplate(system.file("static", "index.html", package = "mattR"),
#'                list(title = "foo", text = "bar"))
renderTemplate <- function(templateFile, data) {
  template <- readChar(templateFile, file.info(templateFile)$size)
  whisker::whisker.render(template, data)
}
