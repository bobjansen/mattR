context("Views")

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

test_that("Static View returns the proper file", {
  request <- list(PATH_INFO = 'static/index.html')
  view <- staticView(system.file("static", package = "mattR"),
                     "static")
  resp <- view(list(), request)

  expect_true("body" %in% names(resp))
  # Check that text from the template itself is present.
  expect_true(grepl("1.css", resp[["body"]], fixed = TRUE))

  # This is the raw template so unsubsituted values are expected.
  expect_true(grepl("{{title}}", resp[["body"]], fixed = TRUE))
  expect_true(grepl("{{text}}", resp[["body"]], fixed = TRUE))

  expect_true("status" %in% names(resp))
  expect_true(resp[["status"]] == 200)
})

test_that("Static View returns the index file when it's not given", {
  request <- list(PATH_INFO = 'static')
  view <- staticView(system.file("static", package = "mattR"),
                     "static")
  resp <- view(list(), request)

  expect_true("body" %in% names(resp))
  # Check that text from the template itself is present.
  expect_true(grepl("1.css", resp[["body"]], fixed = TRUE))

  # This is the raw template so unsubsituted values are expected.
  expect_true(grepl("{{title}}", resp[["body"]], fixed = TRUE))
  expect_true(grepl("{{text}}", resp[["body"]], fixed = TRUE))

  expect_true("status" %in% names(resp))
  expect_true(resp[["status"]] == 200)
})

test_that("Static View returns NULL when the prefix is not used.", {
  request <- list(PATH_INFO = 'index.html')
  view <- staticView(system.file("static", package = "mattR"),
                     "static")
  resp <- view(NULL, request)

  expect_equal(resp, NULL)
})

test_that("Static View returns NULL when the file doesn't exist.", {
  request <- list(PATH_INFO = 'static/index2.html')
  view <- staticView(system.file("static", package = "mattR"),
                     "static")
  resp <- view(NULL, request)

  expect_equal(resp[['status']], 404L)
})

test_that("Template View stops on missing template", {
  expect_error(templateView('NON_EXISTENT_FILE'))
})

test_that("Template View returns the proper template", {
  request <- list()
  view <- templateView(system.file("static", "index.html", package = "mattR"),
                       list(title = "foo", text = "bar"))
  resp <- view(list(), request)

  expect_true("body" %in% names(resp))
  # Check that text from the template itself is present.
  expect_true(grepl("1.css", resp[["body"]], fixed = TRUE))

  # Check that the templated values are present.
  expect_true(grepl("foo", resp[["body"]], fixed = TRUE))
  expect_true(grepl("bar", resp[["body"]], fixed = TRUE))

  expect_true("status" %in% names(resp))
  expect_true(resp[["status"]] == 200L)
})

test_that("Static genericView returns a function creating response", {
  responseText = "Hello World!"
  view <- genericView(function(resp, request, params) {
                        resp[["body"]] <- responseText
                        resp[["status"]] <- 200L
                        resp
                       })

  resp <- view(structure(list(), class = "response"),
               list(REQUEST_METHOD = "GET",
                    QUERY_STRING = ""))

  expect_true(is(resp, "response"))

  expect_true("body" %in% names(resp))
  expect_true(resp[["body"]] == responseText)

  expect_true("status" %in% names(resp))
  expect_true(resp[["status"]] == 200L)
})

test_that("Dynamic genericView returns a function creating response", {
  responseText = "Hello World!"
  view <- genericView(function(resp, req, params) {
                        resp[["body"]] <- params[["Text"]]
                        resp[["status"]] <- 200L
                        resp
                     })

  resp <- view(structure(list(), class = "response"),
               list(REQUEST_METHOD = "GET",
                    QUERY_STRING = paste0("?Text=", responseText)))

  expect_true(is(resp, "response"))
  expect_true("body" %in% names(resp))
  expect_true(resp[["body"]] == responseText)
  expect_true("status" %in% names(resp))
  expect_true(resp[["status"]] == 200L)
})

test_that("Dynamic views with no params don't crash", {
  responseText = "Hello World!"
  view <- genericView(function(resp, req, params) {
                        resp[["body"]] <- responseText
                        resp[["status"]] <- 200L
                        resp
                     })

  resp <- view(structure(list(), class = "response"),
               list(REQUEST_METHOD = "GET"))

  expect_true(is(resp, "response"))
  expect_true("body" %in% names(resp))
  expect_true(resp[["body"]] == responseText)
  expect_true("status" %in% names(resp))
  expect_true(resp[["status"]] == 200L)
})

test_that("Dynamic POST views with no params don't crash", {
  responseText <- "Hello World!"
  view <- genericView(function(resp, req, params) {
                        resp[["body"]] <- responseText
                        resp[["status"]] <- 200L
                        resp
                     })

  resp <- view(structure(list(), class = "response"),
               list(REQUEST_METHOD = "POST",
                    rook.input = list(read_lines = function() "")))

  expect_true(is(resp, "response"))
  expect_true("body" %in% names(resp))
  expect_true(resp[["body"]] == responseText)
  expect_true("status" %in% names(resp))
  expect_true(resp[["status"]] == 200L)
})

test_that("Dynamic POST views with params are handled", {
  responseText <- "Hello World!"
  view <- genericView(function(resp, req, params) {
                        resp[["body"]] <- responseText
                        resp[["status"]] <- 200L
                        resp
                     })

  resp <- view(structure(list(), class = "response"),
               list(REQUEST_METHOD = "POST",
                    QUERY_STRING = paste0("?Text=", responseText),
                    rook.input = list(read_lines = function() "")))


  expect_true(is(resp, "response"))
  expect_true("body" %in% names(resp))
  expect_true(resp[["body"]] == responseText)
  expect_true("status" %in% names(resp))
  expect_true(resp[["status"]] == 200L)
})

test_that("Content-Type guessing works", {
  expect_equal(guessContentTypeFromFilePath("index.html"), "text/html")
  expect_equal(guessContentTypeFromFilePath("index.htm"), "text/html")
  expect_equal(guessContentTypeFromFilePath("style.css"), "text/css")
  expect_equal(guessContentTypeFromFilePath("foo"), "")
})
