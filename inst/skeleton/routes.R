# Put your routes here.

routes <- list(c("^*/", genericView(function(resp, request, params) {
  resp[["body"]] <- "Hello World!"
  resp
})))
