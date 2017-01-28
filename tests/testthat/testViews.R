test_that("response has a body", {
  resp <- response("foo", 200)

  expect_true("body" %in% names(resp))
})

test_that("response has a status", {
  resp <- response("foo", 200)

  expect_true("status" %in% names(resp))
})

test_that("response has headers", {
  resp <- response("foo", 200)

  expect_true("headers" %in% names(resp))
})

test_that("response created by create200Response has status 200", {
  resp <- create200Response("foo")

  expect_equal(resp$status, 200)
})

