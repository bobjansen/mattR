#' Extract Parameters from a request
#'
#' Extract the parameters from a request (GET, POST and from the URL).
#'
#' @param request The request object from which to extract the parameters.
#' @return A list of extracted parameters.
#'
#' @import shiny
#' @importFrom utils modifyList hasName
#' @export
extractParameters <- function(request) {
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

  params <- if (
    hasName(request, "RegExpMatch") &&
      hasName(attributes(request[["RegExpMatch"]]), "capture.names")
  ) {
    m <- request[["RegExpMatch"]]
    urlNamedParams <- substring(
      request[["PATH_INFO"]],
      attr(m, "capture.start"),
      attr(m, "capture.start") + attr(m, "capture.length") - 1)
    names(urlNamedParams) <- attr(m, "capture.names")
    modifyList(params, split(unname(urlNamedParams), names(urlNamedParams)))
  } else {
    params
  }

  params
}

