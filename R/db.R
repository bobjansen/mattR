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
  sqlUsers <- "CREATE TABLE USERS (username TEXT primary key, password TEXT)"
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
