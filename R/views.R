#' Return static files
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
    requestPath <- request$PATH_INFO

    # Ensure that urlPath is a prefix for the requested path.
    if (!startsWith(requestPath, urlPath)) {
      return(NULL)
    }

    staticResourceSubPath <- substring(requestPath, nchar(urlPath) + 1)

    fileName <- file.path(staticDir, staticResourceSubPath)

    if (file.exists(fileName)) {
      create200Response(readChar(fileName, file.info(fileName)$size))
    } else {
      NULL
    }
  }
}

create200Response <- function(...) {
  response(..., statusCode = 200)
}

response <- function(body, statusCode) {
  structure(
    list(body = body,
         statusCode = statusCode),
    class = "response"
  )
}