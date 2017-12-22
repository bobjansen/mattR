test_that("Parsing a single valued cookie works", {
  parsedCookie <- parseCookie("foo=bar")
  expect_equal(parsedCookie, list(foo = "bar"))
})

test_that("Parsing a multi valued cookie works", {
  parsedCookie <- parseCookie("foo=bar;baz=quux")
  expect_equal(parsedCookie, list(foo = "bar", baz = "quux"))

  parsedCookie <- parseCookie("foo=bar; baz=quux")
  expect_equal(parsedCookie,
               list(foo = "bar", baz = "quux"),
               label = "With extra space")

  parsedCookie <- parseCookie("foo=bar; baz=")
  expect_equal(parsedCookie,
               list(foo = "bar", baz = ""),
               label = "With empty value")

  expect_message(parseCookie("foo=bar; baz"))
})

