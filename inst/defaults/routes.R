library(mattR)

routes <- createRoutes(
  c("^/$", templateView(system.file("static", "index.html", package = "mattR"),
                               list(title = "Title", text = "text"))),
  c("/static/*", staticView(system.file("static", package = "mattR"),
                            "/static")),
  c("/params", genericView(function(params) {
    if (length(params) > 0) {
      paste0(names(params), ": ", params, collapse = ", ")
    } else {
      "No params given"
    }
  }))
)
