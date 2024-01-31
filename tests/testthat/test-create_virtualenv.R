test_that("Raise an error when python isn't available", {
  expect_error(create_virtualenv(NULL), "No python found on this computer.")
})
test_that("create a virtual environment", {
  skip_if_no_python()
  path <- create_virtualenv()
  expect_true(is.character(path))
})

test_that("path exists", {
  skip_if_no_python()
  path <- create_virtualenv()
  expect_true(all(file.exists(path)))
})
