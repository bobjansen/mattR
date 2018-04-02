context("Server")

test_that("Get works", {
  runTestServer(daemonized = TRUE)
  on.exit(stopDaemonizedServer())

  for (x in 1:10) {
    if (isMattRRunning()) {
      break
    }
    Sys.sleep(0.1)
  }
  if (!isMattRRunning()) {
    warning("Server could not be started in one second.")
  }

  expect_equal(nonBlockingGet("localhost:8080/params"), "No params given")
})

