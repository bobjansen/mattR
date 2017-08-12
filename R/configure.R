#' App configuration file
#'
#' @return the configuration
#' @export
#'
#' @examples
#' \dontrun{
#' configure()
#' }
configure <- function() {
  list(
    port = 8080,
    debug = TRUE
  )
}

#' Get a value from the config or return the default
#'
#' @param config The configuration list
#' @param key The key to look for
#' @param default The default
#'
#' @return The key if found or the default
#' @export
#'
#' @examples
#' getConfigOrDefault(configure(), "port", 8888)
getConfigOrDefault <- function(config, key, default) {
  if (key %in% names(config)) {
    config[[key]]
  } else {
    default
  }
}
