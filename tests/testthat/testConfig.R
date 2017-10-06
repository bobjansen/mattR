test_that("Configure returns a list", {
  config <- configure()

  expect_equal(class(config), "list")
})

test_that("Configure returns a list with port and debug", {
  config <- configure()

  expect_true("port" %in% names(config))
  expect_true("debug" %in% names(config))
})

test_that("Defaults are given when key is not found", {
  config <- configure()
  on.exit(DBI::dbDisconnect(config[["dbConnection"]]))

  expect_equal(getConfigOrDefault(config, "NOTACONFIGOPTION", "foo"), "foo")
})

test_that("The correct value is given when it exists", {
  config <- configure()
  on.exit(DBI::dbDisconnect(config[["dbConnection"]]))

  expect_equal(getConfigOrDefault(config, "port", "foo"), config[["port"]])
})
