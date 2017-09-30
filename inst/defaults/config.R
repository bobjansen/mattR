packageConfig <- list(
  port = 8080,
  debug = TRUE,
  dbConnection = NULL,
  middlewares = c(
                  function(resp, req) {
                    resp <- getResponse(resp, req)
                    resp[["headers"]][["X-SERVER"]] <- "mattR"
                    resp
                  })
)
