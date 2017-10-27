nonBlockingGet <- function(url) {
    fileName <- tempfile()
  system2("curl", args = url, wait = FALSE, stdout = fileName, stderr = FALSE)

    fileSize <- file.info(fileName)[["size"]]
    while (is.na(fileSize) || fileSize == 0) {
          Sys.sleep(0.001)
        fileSize <- file.info(fileName)[["size"]]
          }

      readChar(fileName, file.info(fileName)[["size"]])
}

