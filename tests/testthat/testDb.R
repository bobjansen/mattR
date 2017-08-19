test_that("Connecting to the database works", {
  con <- createConnection()
  on.exit(DBI::dbDisconnect(con))
  expect_true(is(con, "SQLiteConnection"))
})

test_that("Setting up the database works", {
  con <- setupDatabase()
  on.exit(DBI::dbDisconnect(con))
  expect_equal(DBI::dbListTables(con), c("USERS"))
})

test_that("Creating a user works", {
  con <- setupDatabase()
  on.exit(DBI::dbDisconnect(con))
  createUser(con, "user", "42")
  res <- DBI::dbGetQuery(con, "SELECT count(1) FROM USERS;")
  expect_equal(res[[1]], 1)
})

test_that("Check valid user credentials work", {
  con <- setupDatabase()
  on.exit(DBI::dbDisconnect(con))
  createUser(con, "user", "42")
  expect_true(checkUserCredentials(con, "user", "42"))
})
