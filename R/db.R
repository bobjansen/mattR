#' createConnection
#'
#' Create a connection to an in-memory SQLite database.
#'
#' @return Connection to the database.
#' @export
#' @import DBI
#' @import RSQLite
#'
#' @examples
#' \dontrun{
#' createConnection()
#' }
createConnection <- function() {
  DBI::dbConnect(RSQLite::SQLite(), ":memory:")
}

#' setupDatabase
#'
#' Setup a database with a simple USERS table.
#'
#' @return A connection to the database
#' @export
#' @import DBI
#'
#' @examples
#' \dontrun{
#' setupDatabase()
#' }
setupDatabase <- function() {
  con <- createConnection()
  sql <- "CREATE TABLE USERS (username TEXT primary key, password TEXT)"
  res <- DBI::dbSendQuery(con, sql)
  DBI::dbClearResult(res)
  con
}
