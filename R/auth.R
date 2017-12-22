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
  if (is.null(username)) {
    username <- ""
  }
  if (is.null(password)) {
    password <- ""
  }

  sql <- "SELECT user_id, password FROM USERS where username = ?username;"
  query <- DBI::sqlInterpolate(con, sql, username = username)
  res <- DBI::dbGetQuery(con, query)
  hashedPass <- res[["password"]]
  if (length(hashedPass) == 0) {
    # scrypt::hashPassword("")
    hashedPass <- "c2NyeXB0ABAAAAAIAAAAAfLfQ+kvzWKSpI4modXxrBN6cGVo94VdMQHyeuW4b6Q5+ZvV1/TPzMVnbGf4EF7BDpkFrCuDTJm9XlSls/v6N1fOrSkRGQ4AkFSYHqYEjVro"
    scrypt::verifyPassword(hashedPass, password)
    NULL
  } else {
    if (scrypt::verifyPassword(hashedPass, password)) {
      res[["user_id"]]
    } else {
      NULL
    }
  }
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

#' existsUser
#'
#' Check whether a users exists
#'
#' @param con Database connection
#' @param username Username
#'
#' @export
#' @import DBI
#' @examples
#' \dontrun{
#' createUser(con, "foo", "42")
#' }
existsUser <- function(con, username) {
  sql <- "SELECT count(1) FROM USERS WHERE username = ?username"
  query <- DBI::sqlInterpolate(con, sql, username = username)
  res <- DBI::dbGetQuery(con, query)
  res[[1]] >= 1
}

