library(RCurl)

token <- startServerProcess()

Sys.sleep(1)

test_that("Get works", {
  expect_equal(RCurl::getURL("127.0.01:8080"), "foo")
})

killByToken(token)
