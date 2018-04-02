context("Session")

test_that("Creating a session works", {
  con <- setupDatabase()
  on.exit(DBI::dbDisconnect(con))
  sessionKey <- "ABCDE"
  storeSessionId(con, sessionKey)
  sessionData <- "foo=bar"
  retSessionData <- setSessionData(con, sessionKey, sessionData)

  expect_equal(sessionData, retSessionData)
  expect_equal(sessionData, getSessionData(con, sessionKey))
})

