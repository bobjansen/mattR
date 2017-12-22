#' parseCookie
#'
#' Parse a HTTP cookie
#'
#' @param http_cookie The cookie as a string.
#'
#' @return The cookie parsed to a list. The empty list is returned when an error
#' or warning occurs and a \code{message()} is generated.
#' @export
#'
#' @examples
#' parseCookie("foo=bar")
parseCookie <- function(http_cookie) {
  cookie <- list()
  tryCatch({
    cookiePairs <- trimws(strsplit(http_cookie, ";")[[1]])
    for (cookiePair in cookiePairs) {
      if (length(cookiePair) > 0) {
        parts <- strsplit(cookiePair, "=")[[1]]
        if (length(parts) == 2) {
          cookie[[parts[[1]]]] <- parts[[2]]
        } else if (substr(cookiePair, nchar(cookiePair), nchar(cookiePair))
                   == "=") {
          cookie[[parts[[1]]]] <- ""
        } else {
          # E.g. "foo=bar; baz"
          warning("Seems invalid")
        }
      }
    }
    cookie
  }, warning = function(e) {
    message("Received malformed cookie.")
    list()
  }, error = function(e) {
    message("Received malformed cookie.")
    list()
  })
}

