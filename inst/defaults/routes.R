routes <- mattR::createRoutes(
  c("^/$", mattR::staticView("/opt/code/mattR/inst/static/", "/")),
  c("/static/*", mattR::staticView("/opt/code/mattR/inst/static/", "/static"))
)
