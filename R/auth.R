#' checkPassword
#'
#' Check whether the password for the user matches the hashed (using scrypt)
#' password in the database.
#'
#' @param con Database connection
#' @param username Username
#' @param password Password for the user
#'
#' @export
#' @import DBI
#' @import scrypt
#' @examples
#' \dontrun{
#' checkUserCredentials(con, "foo", "42")
#' }
checkUserCredentials <- function(con, username, password) {
  sql <- "SELECT password FROM USERS where username = ?username;"
  query <- DBI::sqlInterpolate(con, sql, username = username)
  scrypt::verifyPassword(DBI::dbGetQuery(con, query)[["password"]], password)
}

#' createUser
#'
#' Create a user in the database. The password is hashed using scrypt.
#'
#' @param con Database connection
#' @param username Username
#' @param password Password for the user
#'
#' @export
#' @import DBI
#' @import scrypt
#' @examples
#' \dontrun{
#' createUser(con, "foo", "42")
#' }
createUser <- function(con, username, password) {
  sql <- "INSERT INTO USERS (username, password) VALUES (?username, ?password)"
  hashed <- scrypt::hashPassword(password)
  query <- DBI::sqlInterpolate(con, sql, username = username, password = hashed)
  DBI::dbExecute(con, query)
}

