initFromFile <- function(config) {
  appState <- new.env()

  initPath <- file.path(getwd(), "init.R")
  if (file.exists(initPath)) {
    sys.source(initPath, envir = appState) # nocov
    if (is.null(appState)) {
      stop("The init.R script didn't create an appState variable.")
    }
  } else {
    sys.source(system.file("defaults", "init.R", package = "mattR"),
               envir = appState)
    if (is.null(appState)) {
      stop("The default init.R script didn't create an appState variable.")
    }
  }

  appState[["debug"]] <- getConfigOrDefault(config, "debug", FALSE)

  if (appState[["debug"]]) { # nocov start
    message(paste("Path of the user init file would be:",
                  initPath)) # nocov end
  }

  appState
}
