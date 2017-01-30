library(mattR)

routes <- mattR::createRoutes(
  c("^/$", mattR::templateView(system.file("static", "index.html", package = "mattR"),
                               list(title = "Title", text = "text"))),
  c("/static/*", mattR::staticView(system.file("static", package = "mattR"), "/static"))
)
