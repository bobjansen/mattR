create200Response <- function(...) {
  response(..., statusCode = 200L)
}

response <- function(body, statusCode) {
  structure(
    list(body = body,
         headers = list('Content-Type' = ''),
         status = statusCode),
    class = "response"
  )
}

notFoundResponse <- function() {
  list(
    status = 404L,
    headers = list(
      # Invalid, but the browser is pretty smart.
      'Content-Type' = ''
    ),
    body = paste(
      sep = "\r\n",
      404L
    )
  )
}
