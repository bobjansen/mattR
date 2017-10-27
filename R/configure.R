#' configure
#'
#' Load the default configuration and the configuration defined in the
#' \code{config} file in the working directory.
#'
#' The default configuration is retrieved from \code{defaults/config.R} in the
#' package. Then settings found in the variable named \code{config} in
#' \code{config.R} in the working directory is used to overwrite to optionally
#' overwrite settings.
#'
#' @return the configuration
#' @export
#'
#' @examples
#' \dontrun{
#' configure()
#' }
configure <- function() {
  config <- packageConfig <- NULL
  source(system.file("defaults", "config.R", package = "mattR"), local = TRUE)
  appConfigFile <- file.path(getwd(), "config.R")
  if (file.exists(appConfigFile)) { # nocov start
    source(appConfigFile, local = TRUE)
    modifyList(packageConfig, config)
    # nocov end
  } else {
    packageConfig
  }
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
