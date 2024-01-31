test_that("CV preserves through the number folds specified for LM", {
  lm_data <- data_gen_lm(25)
  model <- lm(Y ~ ., lm_data)
  lm_cv <- cv(model, lm_data, 5)
  expect_equal(nrow(lm_data), length(lm_cv))
})

test_that("CV preserves through the number folds specified for reg_sine", {
  sine_data <- data_gen_sine(25)
  model <- reg_sine(Y ~ ., sine_data)
  sine_cv <- cv(model, sine_data, 5)
  expect_equal(nrow(sine_data), length(sine_cv))
})

test_that("CV preserves through the number folds specified for reg_asym", {
  asym_data <- data_gen_asym(25)
  model <- reg_asym(Y ~ ., asym_data)
  asym_cv <- cv(model, asym_data, 5)
  expect_equal(nrow(asym_data), length(asym_cv))
})

test_that("CV preserves through the number folds specified for mlm_stressor", {
  skip_if_no_python()

  # Regression
  sine_data <- data_gen_sine(50)
  model <- mlm_regressor(Y ~ ., sine_data)
  mlm_cv <- cv(model, sine_data, 5)
  for (i  in ncol(mlm_cv)) {
    expect_equal(nrow(sine_data), length(mlm_cv[, i]))
  }

  # Classification
  binary_resp <- sample(c(0, 1), 50, replace = TRUE)
  sine_class <- data_gen_sine(50)
  sine_class$Y <- binary_resp
  model <- mlm_classification(Y ~ ., sine_class)
  mlm_class_cv <- cv(model, sine_class, 5)
  for (i  in ncol(mlm_class_cv)) {
    expect_equal(nrow(sine_class), length(mlm_class_cv[, i]))
  }
})
