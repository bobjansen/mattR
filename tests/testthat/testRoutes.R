test_that("Simple matching works", {
  request <- list(PATH_INFO = "/")
  expect_true(matchRequest(request, "/"))

  request$PATH_INFO <- "/static"
  expect_true(matchRequest(request, "/static"))
  expect_false(matchRequest(request, "/index"))

})

test_that("Pattern matching works", {
  routes <- createRoutes(
    c('^articles/2003/$', function(...) "views.special_case_2003"),
    c('^articles/([0-9]{4})/$', function(...) "views.year_archive"),
    c('^articles/([0-9]{4})/([0-9]{2})/$', function(...) "views.month_archive"),
    c('^articles/([0-9]{4})/([0-9]{2})/([0-9]+)/$', function(...) "views.article_detail")
  )

  expect_equal(matchRoutes(routes, list(PATH_INFO = "articles/2003/")), "views.special_case_2003")
  expect_equal(matchRoutes(routes, list(PATH_INFO = "articles/1999/")), "views.year_archive")
  expect_equal(matchRoutes(routes, list(PATH_INFO = "articles/1999/12/")), "views.month_archive")
  expect_equal(matchRoutes(routes, list(PATH_INFO = "articles/1999/12/1/")), "views.article_detail")
})

test_that("createRoutes creates an iterable of closures", {
  routes <- createRoutes(
    c("index.html", function(...) 1),
    c("contact.html", function(...) 2)
  )

  expect_true(all(sapply(routes, is.function)))
})

test_that("Routes are matched and return a value", {
  routes <- createRoutes(
    c("index.html", function(...) 1),
    c("contact.html", function(...) 2)
  )

  expect_equal(matchRoutes(routes, list(PATH_INFO = "index.html")), 1)
  expect_equal(matchRoutes(routes, list(PATH_INFO = "contact.html")), 2)
})

test_that("Unmatched routes return NULL", {
  routes <- createRoutes(
    c("index.html", function(...) 1),
    c("contact.html", function(...) 2)
  )

  expect_equal(matchRoutes(routes, list(PATH_INFO = "foo")), NULL)
})

