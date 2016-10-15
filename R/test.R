killByToken <- function(token) {
  procs <- system2("ps", args = "aux", stdout = TRUE)

  lines <- grep(paste("mattR --args", token), procs)
  if (length(lines) > 1) {
    stop("Multiple processes found with supposedly unique token")
  } else if (length(lines) == 0) {
    warning(paste("No process found for token:", token))
  } else {
    system2("kill", args = strsplit(procs[[lines[1]]], "\\s+")[[1]][[2]])
  }
}

startServerProcess <- function() {
  scriptName <- system.file("scripts", "mattR", package = "mattR")

  token <- as.character(sample(1:1e6, 1))

  system2(scriptName, wait = FALSE, args = token)

  token
}
