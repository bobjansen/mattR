#' Guess the Content-Type for a file or filepath
#'
#' @param filepath Filepath of which to guess the Content-Type.
#'
#' @return The guess of the Content-Type.
guessContentTypeFromFilePath <- function(filepath) {
  if (endsWith(filepath, 'html') || endsWith(filepath, 'htm')) {
    'text/html'
  } else if (endsWith(filepath, 'css')) {
    'text/css'
  } else {
    ''
  }
}

#' Respond with a static file
#'
#' @param staticDir Location of the static files on the server.
#' @param urlPath URL path to the static files.
#'
#' @return A response object or NULL if the file is not found.
#' @export
#'
#' @examples
#' \dontrun{
#' staticFun <- staticView("/var/www/static/", "/static/")
#' }
staticView <- function(staticDir, urlPath) {
  function(resp, request) {
    requestPath <- request[["PATH_INFO"]]

    # Ensure that urlPath is a prefix for the requested path.
    if (!startsWith(requestPath, urlPath)) {
      return()
    }

    staticResourceSubPath <- substring(requestPath, nchar(urlPath) + 1)

    if (staticResourceSubPath == "") {
      staticResourceSubPath <- "index.html"
      if (!file.exists(file.path(staticDir, staticResourceSubPath))) {
        staticResourceSubPath <- "index.htm"
      }
    }
    fileName <- file.path(staticDir, staticResourceSubPath)

    contentType <- guessContentTypeFromFilePath(staticResourceSubPath)

    if (file.exists(fileName)) {
      resp[["body"]] <- paste0(resp[["body"]],
                               readChar(fileName,
                                        file.info(fileName)[["size"]]))
      resp[["headers"]][["Content-Type"]] <- contentType
      resp[["status"]] <- 200L
      resp
    } else {
      notFoundResponse(paste("File", fileName, "not found."))
    }
  }
}

#' Respond with the result of FUN
#'
#' @param FUN Function that generates the response as text.
#'
#' @return A response object.
#' @export
#'
#' @import shiny
#' @importFrom utils modifyList
#'
#' @examples
#' genericView(function() "Hello World!")
genericView <- function(FUN) {
  function(resp, request) {
    params <- if ("QUERY_STRING" %in% names(request)) {
      shiny::parseQueryString(request[["QUERY_STRING"]])
    } else {
      list()
    }
    params <- if (request[["REQUEST_METHOD"]] == "POST") {
      postParams <- request[["rook.input"]]$read_lines()
      if (length(postParams) > 0) {
        modifyList(params, shiny::parseQueryString(postParams))
      } else {
        params
      }
    } else {
      params
    }

    FUN(resp, request, params)
  }
}

#' Respond with a rendered template
#'
#' @param templateFile Filepath of a whisker template
#' @param data named list or environment with variables that will be used during
#' rendering
#'
#' @return A response object with as body the rendered template.
#' @export
#'
#' @examples
#' templateView(system.file("static", "index.html", package = "mattR"),
#'              list(title = "foo", text = "bar"))
templateView <- function(templateFile, data) {
  if (!file.exists(templateFile)) {
    stop("File '", templateFile, "' doesn't exist")
  }

  contentType <- guessContentTypeFromFilePath(templateFile)

  function(resp, request) {
    resp[["body"]] <- paste0(resp[["body"]],
                             renderTemplate(templateFile, data))
    resp[["headers"]][["Content-Type"]] <- contentType
    resp[["status"]] <- 200L
    resp
  }
}

#' Render a template given data
#'
#' @param templateFile Filepath of a whisker template.
#' @param data Named list or environment with variables that will be used during
#' rendering.
#'
#' @return The rendered template.
#' @export
#'
#' @examples
#' renderTemplate(system.file("static", "index.html", package = "mattR"),
#'                list(title = "foo", text = "bar"))
renderTemplate <- function(templateFile, data) {
  template <- readChar(templateFile, file.info(templateFile)$size)
  whisker::whisker.render(template, data)
}

