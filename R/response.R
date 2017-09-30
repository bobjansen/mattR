response <- function(body = NULL, statusCode = NULL, contentType = '') {
  structure(
    # Rook requires taht Content-Type is set.
    list(body = body,
         headers = list('Content-Type' = contentType),
         status = statusCode),
    class = "response"
  )
}

notFoundResponse <- function() {
  structure(
    list(
      status = 404L,
      headers = list(
        'Content-Type' = 'text/plain'
      ),
      body = paste(
        sep = "\r\n",
        "404 Page Not Found"
      )
    ),
    class = "response"
  )
}
