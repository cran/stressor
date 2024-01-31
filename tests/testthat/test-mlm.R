test_that("mlm_regressor fits regression models and is reproducible", {
  skip_if_no_python()
  set.seed(43421)
  lm_data <- data_gen_lm(25)
  mlm_regressor <- mlm_regressor(Y ~ ., lm_data, sort_v = 'RMSE', seed = 43421)
  initial_pred <- round(mlm_regressor$pred_accuracy$RMSE, 2)
  expect_equal(length(mlm_regressor), 2)
  expect_equal(length(mlm_regressor$models), 18)
  expect_equal(is.data.frame(mlm_regressor$pred_accuracy), TRUE)
  expect_s3_class(mlm_regressor, "mlm_stressor")
  expect_s3_class(mlm_regressor, "regressor")
  expect_equal(initial_pred, c(0.97, 1.03, 1.03, 1.04, 1.07, 1.42, 1.43, 1.55,
                               1.67, 1.80, 1.97, 1.97, 2.20, 2.31, 2.31, 2.40,
                               2.60, 2.60))
})

test_that("mlm_classification fits classification models and is reproducible", {
  skip_if_no_python()
  set.seed(43421)
  binary_resp <- sample(c(0, 1), 50, replace = TRUE)
  sine_class <- data_gen_sine(50)
  sine_class$Y <- binary_resp
  mlm_class <- mlm_classification(Y ~ ., sine_class, sort_v = 'Accuracy',
                                  seed = 43421)
  initial_pred <- round(mlm_class$pred_accuracy$Accuracy, 2)
  expect_equal(length(mlm_class), 2)
  expect_equal(length(mlm_class$models), 14)
  expect_equal(is.data.frame(mlm_class$pred_accuracy), TRUE)
  expect_s3_class(mlm_class, "mlm_stressor")
  expect_s3_class(mlm_class, "classifier")
  expect_equal(initial_pred, c(0.62, 0.60, 0.58, 0.58, 0.57, 0.56, 0.55, 0.53,
                               0.53, 0.52, 0.52, 0.52, 0.52, 0.47))
})
