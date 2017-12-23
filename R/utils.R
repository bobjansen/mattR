#' fileUrl
#'
#' Return a URL for a file to be sent to the client. the file will be base64
#' encoded and embedded in the URL.
#'
#' Taken from
#' https://github.com/rstudio/shiny/blob/e30fac02eddbb7cd510d448e385c559c82009a24/R/shiny.R
#'
#' @param file The file name
#' @param contentType The content type
#' @export
fileUrl <- function(
  file, contentType = 'application/octet-stream'
) { # nocov start
  bytes <- file.info(file)[["size"]]
  if (is.na(bytes)) {
    return()
  }

  fileData <- readBin(file, 'raw', n = bytes)
  b64 <- httpuv::rawToBase64(fileData)
  paste0('data:', contentType, ';base64,', b64) # nocov end
}
