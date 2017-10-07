#' createConnection
#'
#' Create a connection to an in-memory SQLite database.
#'
#' @param dsn DSN of the connection.
#' @return Connection to the database.
#' @import DBI
#' @import RSQLite
#'
#' @export
#'
#' @examples
#' \dontrun{
#' createConnection()
#' }
createConnection <- function(dsn = ":memory:") {
  DBI::dbConnect(RSQLite::SQLite(), dsn)
}

#' setupDatabase
#'
#' Setup a database with a simple USERS table.
#'
#' @param dsn DSN of the connection.
#'
#' @return A connection to the database
#' @import DBI
#' @export
#'
#' @examples
#' \dontrun{
#' setupDatabase()
#' }
  sqlUsers <- "CREATE TABLE USERS (username TEXT primary key, password TEXT)"
setupDatabase <- function(dsn = ":memory:") {
  con <- createConnection(dsn)
  res <- DBI::dbSendQuery(con, sqlUsers)
  DBI::dbClearResult(res)

  # The table structure is taken from a standard Django install.
  sqlSession <- 'CREATE TABLE IF NOT EXISTS "SESSION"
    ("session_key" varchar(40) NOT NULL PRIMARY KEY,
     "session_data" text NOT NULL,
     "creation_date" datetime NOT NULL);'
  sqlSessionIndex <- 'CREATE INDEX "django_session_expire_date_index" ON
    "session" ("expire_date");'
  res <- DBI::dbSendQuery(con, sqlSession)
  DBI::dbClearResult(res)
  res <- DBI::dbSendQuery(con, sqlSessionIndex)
  DBI::dbClearResult(res)
  con
}
