context("Manage")

test_that("Skeleton creates a skeleton", {
  # Prepare a directory for installation.
  installDir <- file.path(tempdir(), "skeleton")
  on.exit(unlink(installDir, recursive = TRUE))
  dir.create(installDir)

  # Check that creation works.
  didCreate <- skeleton(path = installDir)

  expect_true(didCreate)

  # List of all installed files.
  expect_true(file.exists(file.path(installDir, "routes.R")))
  expect_true(file.exists(file.path(installDir, "manage")))
})

test_that("Skeleton doesn't create a file in a non-empty dir", {
  # Prepare a directory for installation.
  installDir <- file.path(tempdir(), "skeleton")
  on.exit(unlink(installDir, recursive = TRUE))
  dir.create(installDir)

  # Check that creation works.
  skeleton(path = installDir)
  functionOutput <- capture_messages(didCreate <- skeleton(path = installDir))

  expect_equal(functionOutput, "The directory is not empty, exiting.\n")
  expect_false(didCreate)
})
