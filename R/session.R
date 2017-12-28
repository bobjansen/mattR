#' sessionMiddleware
#'
#' Read the cookie and create a session id.
#'
#' @param resp The response that is being build.
#' @param req The request.
#'
#' @export
sessionMiddleware <- function(resp, req) {
  resp[["_cookies"]] <- list()

  if (!is.null(req[["HTTP_COOKIE"]])) {
    resp[["_cookies"]] <- modifyList(resp[["_cookies"]],
                                     parseCookie(req[["HTTP_COOKIE"]]))
  }

  if (!"sessionid" %in% names(resp[["_cookies"]])) {
    sessionid = getRandomString(40)
    resp[["headers"]][["Set-Cookie"]] <- paste("sessionid",
                                               sessionid,
                                               sep = "=")
    storeSessionId(getConfigOrDefault(configure(),
                                      "dbConnection", NULL),
                   sessionid)
  }

  getResponse(resp, req)
}

#' Store a session id to the database
#'
#' @param con A connection to the database.
#' @param sessionKey The session key for current request.
#'
#' @return The result of the DB query saving to the database.
storeSessionId <- function(con, sessionKey) {
  sql <- "INSERT INTO SESSION (session_key, session_data, creation_date)
    VALUES (?sessionKey, '', datetime())"
  query <- DBI::sqlInterpolate(con, sql, sessionKey = sessionKey)
  DBI::dbExecute(con, query)
}

#' getSessionData
#'
#' @param con Connection to database.
#' @param sessionKey The session key for which to get session data.
#'
#' @return The session data.
#' @export
#'
#' @examples
#' \dontrun{
#' getSessionData(con, "123")
#' }
getSessionData <- function(con, sessionKey) {
  if (!is.null(sessionKey)) {
    sql <- "SELECT session_data FROM SESSION where session_key = ?sessionKey;"
    query <- DBI::sqlInterpolate(con,
                                 sql,
                                 sessionKey = sessionKey)
    DBI::dbGetQuery(con, query)[["session_data"]]
  } else {
    NULL
  }
}

#' setSessionData
#'
#' @param con Connection to database.
#' @param sessionKey Session key to set session data for.
#' @param sessionData The session data to set.
#'
#' @return The session data.
#' @export
#'
#' @examples
#' \dontrun{
#' setSessionData(con, "123", "foo=bar")
#' }
setSessionData <- function(con, sessionKey, sessionData) {
  sql <- "UPDATE SESSION set session_data = ?sessionData
    where session_key = ?sessionKey;"
  query <- DBI::sqlInterpolate(con,
                               sql,
                               sessionKey = sessionKey,
                               sessionData = sessionData)
  rows_affected <- DBI::dbExecute(con, query)

  if (rows_affected == 1) {
    invisible(sessionData)
  } else if (rows_affected > 1) {
    stop("Duplicate session keys?")
  } else {
    warning("No session key found, did you forget to call storeSessionId()?")
  }
}

