context("Routes")

test_that("Simple matching works", {
  request <- list(PATH_INFO = "/")
  expect_true(matchRequest(request, "/") == 1L)

  request$PATH_INFO <- "/static"
  expect_true(matchRequest(request, "/static") == 1L)
  expect_null(matchRequest(request, "/index"))

})

test_that("Pattern matching works", {
  routes <- list(
    c('^articles/2003/$', function(...) "views.special_case_2003"),
    c('^articles/([0-9]{4})/$', function(...) "views.year_archive"),
    c('^articles/([0-9]{4})/([0-9]{2})/$', function(...) "views.month_archive"),
    c('^articles/([0-9]{4})/([0-9]{2})/([0-9]+)/$', function(...) "views.article_detail")
  )

  expect_equal(matchRoutes(routes, NULL,
                           list(PATH_INFO = "articles/2003/")),
               "views.special_case_2003")
  expect_equal(matchRoutes(routes, NULL,
                           list(PATH_INFO = "articles/1999/")),
               "views.year_archive")
  expect_equal(matchRoutes(routes, NULL,
                           list(PATH_INFO = "articles/1999/12/")),
               "views.month_archive")
  expect_equal(matchRoutes(routes, NULL,
                           list(PATH_INFO = "articles/1999/12/1/")),
               "views.article_detail")
})

test_that("Routes are matched and return a value", {
  routes <- list(
    c("index.html", function(...) 1),
    c("contact.html", function(...) 2)
  )

  expect_equal(matchRoutes(routes, NULL,
                           list(PATH_INFO = "index.html")), 1)
  expect_equal(matchRoutes(routes, NULL,
                           list(PATH_INFO = "contact.html")), 2)
})

test_that("Unmatched routes return NULL", {
  routes <- list(
    c("index.html", function(...) 1),
    c("contact.html", function(...) 2)
  )

  responseObject <- mattR:::matchRoutes(routes, NULL,
                                        list(PATH_INFO = "foo"))

  expect_equal(responseObject$status, 404L)
})

test_that("Returning a null object results in error", {
  routes <- list(
    c("index.html", function(...) NULL)
  )

  expect_error(matchRoutes(routes, list(PATH_INFO = "index.html")))
})

test_that("Params are extracted from a named url", {
  routes <- list(
    c("^blogs/(?<title>[A-z]+)/$", genericView(
      function(resp, req, params) params[["title"]]))
  )
  expect_equal(matchRoutes(routes, NULL,
                           list(PATH_INFO = "blogs/foo/",
                                REQUEST_METHOD = "GET")),
               "foo")
  expect_equal(matchRoutes(routes, NULL,
                           list(PATH_INFO = "blogs/FOO/",
                                REQUEST_METHOD = "GET")),
               "FOO")

  routes <- list(
    c("^blogs/(?<title>[A-z]+)/comment/(?<number>[0-9]+)/$", genericView(
      function(resp, req, params)
        paste(params[["title"]], params[["number"]]))))
  expect_equal(matchRoutes(routes, NULL,
                           list(PATH_INFO = "blogs/foo/comment/2/",
                                REQUEST_METHOD = "GET")),
               "foo 2")
})
