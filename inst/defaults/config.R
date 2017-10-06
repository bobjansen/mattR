packageConfig <- list(
  port = 8080,
  debug = TRUE,
  dbConnection = setupDatabase(),
  middlewares = c(
                  sessionMiddleware,
                  function(resp, req) {
                    resp <- getResponse(resp, req)
                    resp[["headers"]][["X-SERVER"]] <- "mattR"
                    resp
                  })
)
