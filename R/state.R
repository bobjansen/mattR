initFromFile <- function(debug = FALSE) {
  appState <- NULL
  initPath <- file.path(getwd(), "init.R")
  if (file.exists(initPath)) {
    source(initPath, local = TRUE) # nocov
    if (is.null(appState)) {
      stop("The init.R script didn't create an appState variable.")
    }
  } else {
    source(system.file("defaults", "init.R", package = "mattR"), local = TRUE)
    if (is.null(appState)) {
      stop("The default init.R script didn't create an appState variable.")
    }
  }

  if (debug) { # nocov start
    message(paste("Path of the user init file would be:",
                  initPath)) # nocov end
  }

  appState
}

