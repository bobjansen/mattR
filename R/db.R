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
setupDatabase <- function(dsn = ":memory:") {
  con <- createConnection(dsn)
  addExitHandler(function () {
    if (DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
    }
  })
  sqlUsers <- "CREATE TABLE IF NOT EXISTS 'USERS'
    (user_id integer primary key,
     username TEXT,
     password TEXT)"
  sqlUserIndex <- 'CREATE INDEX IF NOT EXISTS
    "mattR_users_username_unique_index" ON
    "users" ("username")';
  res <- DBI::dbSendQuery(con, sqlUsers)
  DBI::dbClearResult(res)
  res <- DBI::dbSendQuery(con, sqlUserIndex)
  DBI::dbClearResult(res)

  # The table structure is taken from a standard Django install.
  sqlSession <- 'CREATE TABLE IF NOT EXISTS "SESSION"
    ("session_key" varchar(40) NOT NULL PRIMARY KEY,
     "session_data" text NOT NULL,
     "creation_date" datetime NOT NULL);'
  sqlSessionIndex <- 'CREATE INDEX IF NOT EXISTS
    "mattR_session_expire_date_index" ON
    "session" ("expire_date");'
  res <- DBI::dbSendQuery(con, sqlSession)
  DBI::dbClearResult(res)
  res <- DBI::dbSendQuery(con, sqlSessionIndex)
  DBI::dbClearResult(res)
  con
}

