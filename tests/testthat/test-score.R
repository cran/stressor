test_that("score_regression returns the correct results", {
  set.seed(43421)
  lm_data <- data_gen_lm(1000)
  indices <- split_data_prob(lm_data, .2)
  train <- lm_data[!indices,]
  test <- lm_data[indices,]
  model <- lm(Y ~ ., train)
  pred_lm <- predict(model, test)
  res_lm <- round(score(test$Y, pred_lm), 4)
  names(res_lm) <- NULL
  expect_equal(length(res_lm), 6)
  expect_equal(res_lm, c(0.9895, 0.7838, 0.9791, 0.8288, 0.1414, 1.3953))
  expect_true(all(is.numeric(res_lm)))
})

test_that("score_classification returns the correct results", {
  set.seed(43421)
  lm_data <- data_gen_lm(1000)
  lm_data$Y <- sample(c(0, 1), 1000, replace = TRUE, prob = c(.6, .4))
  indices <- split_data_prob(lm_data, .2)
  train <- lm_data[!indices,]
  test <- lm_data[indices,]
  pred <- sample(c(0, 1), nrow(test), replace = TRUE)
  res_pred <- round(score(test$Y, pred), 4)
  names(res_pred) <- NULL
  expect_equal(length(res_pred), 7)
  expect_equal(res_pred, c(0.5591, 0.5599, 0.5455, 0.5934, 0.5684, 0.1199,
                           0.1195))
  expect_true(all(is.numeric(res_pred)))
})
