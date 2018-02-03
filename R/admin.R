#' mattR_admin
#'
#' Provide an admin interface.
#' @param routes The original set of routes.
#' @param appState The state of the app.
#'
#' @export
mattR_admin <- function(routes, appState) {
  con <- appState[["dbConnection"]]

  adminMainPage <- function() {
    if (is.null(con)) {
      text <- "No database available"
      tables <- list(table = c())
    } else {
      text <- "Tables available"
      tables <- whisker::iteratelist(DBI::dbListTables(con))
    }

    templateView(system.file("static", "admin", "index.html",
                             package = "mattR"),
                 list(title = "mattR Admin", text = text, tables = tables))
  }

  showMainPage <- function() {
    templateView(system.file("static", "admin", "index.html",
                             package = "mattR"),
                 list(title = "mattR Admin", text = "Edit"))
  }

  c(
    list(c("^/admin$", adminMainPage())),
    list(c("^/admin/show.*$", showMainPage())),
    routes)
}

