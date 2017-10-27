test_that("Setting up a response works", {
  req <- list()
  resp <- setupResponse(req)

  expect_is(resp, "response")

  expect_true("body" %in% names(resp))
  expect_true(is.null(resp[["body"]]))
  expect_true("status" %in% names(resp))
  expect_true(is.null(resp[["status"]]))
  expect_true("headers" %in% names(resp))
  expect_true("Content-Type" %in% names(resp[["headers"]]))
  expect_equal(resp[["headers"]][["Content-Type"]], "")
  expect_true("_middlewares" %in% names(resp))
  expect_true("_middlewares_index" %in% names(resp))
})



