library(mattR)

routes <- list(
  c("^/$", templateView(system.file("static", "index.html", package = "mattR"),
                               list(title = "Title", text = "text"))),
  c("/static/*", staticView(system.file("static", package = "mattR"),
                            "/static")),
  c("/params", genericView(function(resp, req, params) {
    if (length(params) > 0) {
      resp[["body"]] <- paste0(names(params), ": ", params, collapse = ", ")
    } else {
      resp[["body"]] <- "No params given"
    }
    resp[["status"]] <- 200L
    resp
  }))
)
