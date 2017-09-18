test_that("getRandomString creates strings of the proper length", {
  expect_equal(nchar(getRandomString(16)), 16)
})
test_that("getRandomString errors on lenghts not dividable by 4", {
  expect_error(nchar(getRandomString(1)))
  expect_error(nchar(getRandomString(15)))
})

