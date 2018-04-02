context("Response")

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

test_that("Error responses are created correctly", {
  errResp <- errorResponse()
  expect_true(all(c("status", "body", "headers") %in% names(errResp)))
  expect_true(errResp[["status"]] >= 500 && errResp[["status"]] < 600)
  expect_true(grepl("500 Internal server error", errResp[["body"]]))
  expect_true("Content-Type" %in% names(errResp[["headers"]]))
  expect_equal(errResp[["headers"]][["Content-Type"]], "text/plain")

  errResp2 <- errorResponse("Custom Message")
  expect_true(all(c("status", "body", "headers") %in% names(errResp2)))
  expect_true(errResp[["status"]] >= 500 && errResp2[["status"]] < 600)
  expect_true(grepl("Custom Message", errResp2[["body"]]))
  expect_true("Content-Type" %in% names(errResp[["headers"]]))
  expect_equal(errResp[["headers"]][["Content-Type"]], "text/plain")
})

