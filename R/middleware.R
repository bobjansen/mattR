#' Get a response
#'
#' Handle the middlewares, as is done in Django.
#'
#' @param resp The response object moving through the middlewares.
#' @param req The request for which the response is being created.
#'
#' @importFrom methods is
#'
#' @export
getResponse <- function(resp, req) {
  if (resp[["_middlewares_index"]] < length(resp[["_middlewares"]])) {
    resp[["_middlewares_index"]] <- resp[["_middlewares_index"]] + 1L
    resp <- resp[["_middlewares"]][[resp[["_middlewares_index"]]]](resp, req)
    if (!is(resp, 'response')) {
      stop("A middleware should always return a response.")
    }
  }
  resp
}

