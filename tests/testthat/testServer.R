# library(mattR)
# library(RCurl)
# library(testthat)
# token <- startServerProcess()
#
# Sys.sleep(1)
#
# test_that("Get works", {
#   expect_equal(RCurl::getURL("127.0.0.1:8080"), "HOME\r\n")
# })
#
# killByToken(token)
