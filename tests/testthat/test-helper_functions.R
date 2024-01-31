test_that("Data_check returns the proper responses", {
  lm_data <- data_gen_lm(10)
  lm_train <- data_gen_lm(10)

  # Formula argument
  expect_no_error(data_check(Y ~ ., lm_data, lm_train))
  expect_error(data_check("Help", lm_data), "The formula object")

  # Data argument
  expect_error(data_check(Y ~ ., "data"),
               "Data must be inheritted from data.frame")

  # Train Data argument
  expect_error(data_check(Y ~ ., lm_data, "train"),
               "Data must be inheritted from data.frame")
})

test_that("Integer_check raises the correct messages", {
  expect_no_error(integer_check(3))
  expect_error(integer_check(1.5), "Value provided is not an integer")
})

test_that("Numeric_check raises the correct messages", {
  expect_no_error(numeric_check(3))
  expect_error(numeric_check("1.5"), "Value provided is not a numeric value")
})

test_that("Matrix_check raises the correct messages", {
  lm_data <- data_gen_lm(10)
  model_mat <- model.matrix(Y ~ ., data = lm_data)
  expect_no_error(matrix_check(model_mat))
  expect_error(matrix_check(lm_data), "Not a matrix object")
})

test_that("Boolean_check raises the correct messages", {
  expect_no_error(boolean_check(TRUE))
  expect_no_error(boolean_check(FALSE))
  expect_error(boolean_check("TRUE"))
  expect_error(boolean_check(10))
})
