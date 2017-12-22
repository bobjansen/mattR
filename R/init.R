.pkgenv <- new.env(parent = emptyenv())

onLoad <- function(libname, pkgname) {
  # nocov start
  .pkgenv[["handle"]] <- NULL
  .pkgenv[["exitHandlers"]] <- c()
  # nocov end
}

