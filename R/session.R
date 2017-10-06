sessionMiddleware <- function(resp, req) {
  resp[["_cookies"]] <- list()
  for (cookiePair in trimws(strsplit(req[["HTTP_COOKIE"]], ";")[[1]])) {
    parts <- strsplit(cookiePair, "=")[[1]]
    resp[["_cookies"]][[parts[[1]]]] <- parts[[2]]
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

getSessionData <- function(con, sessionKey) {
  sql <- "SELECT session_data FROM SESSION where session_key = ?sessionKey;"
  query <- DBI::sqlInterpolate(con,
                               sql,
                               sessionKey = sessionKey)
  DBI::dbGetQuery(con, query)
}

setSessionData <- function(con, sessionKey, sessionData) {
  sql <- "UPDATE SESSION set session_data = ?sessionData
    where session_key = ?sessionKey;"
  query <- DBI::sqlInterpolate(con,
                               sql,
                               sessionKey = sessionKey,
                               sessionData = sessionData)
  res <- DBI::dbSendQuery(con, query)
  DBI::dbClearResult(res)
}

