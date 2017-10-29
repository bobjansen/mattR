#' Create a skeleton for the app
#' @param path Target path for skeleton creation
#' @export
#'
#' @return \code{TRUE} on success, \code{FALSE} otherwise.
#'
#' @examples
#' \dontrun{
#' buildApp()
#' }
skeleton <- function(path = getwd()) {
  if (length(list.files(path)) +
      length(list.dirs(path, recursive = FALSE)) > 0
  ) {
    message("The directory is not empty, exiting.")
    FALSE
  } else {
    fromPath <- file.path(system.file(package = "mattR"), "skeleton")
    file.copy(file.path(fromPath, "routes.R"), path)
    file.copy(file.path(fromPath, "manage"), path)
    TRUE
  }
}

#' Display the help message
#' @param ... Ignored, just show the default help message.
#' @export
#'
#' @examples
#' \dontrun{
#' helpMessage()
#' }
helpMessage <- function(...) {
  usageMessage() # nocov
}

#' Display the usage message
#' @export
#'
#' @examples
#' \dontrun{
#' usageMessage()
#' }
usageMessage <- function() {
  message("Start a server using './manage runServer'.\n") # nocov
}

