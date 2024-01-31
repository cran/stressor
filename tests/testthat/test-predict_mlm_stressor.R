test_that("predict mlm_stressor returns the correct number", {
  skip_if_no_python()
  set.seed(43421)
  lm_data <- data_gen_lm(50)
  index <- seq(1, 25, 1)
  lm_train <- lm_data[index, ]
  lm_test <- lm_data[-index, ]
  mlm_lm <- mlm_regressor(Y ~ ., lm_train, seed = 43421)
  pred <- predict(mlm_lm, lm_test)
  expect_equal(length(mlm_lm), 2)
  expect_true(inherits(pred, "data.frame"))
  expect_equal(dim(pred), c(25, 18))
  expect_true(!is.null(pred))
})
