# Put your routes here.

routes <- createRoutes(c("^*/", genericView(function(params) {
  "Hello World!"
})))
