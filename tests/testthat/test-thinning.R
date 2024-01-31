test_that("thinning returns the correct number of RMSE values", {
  n <- 1000
  lm_data <- data_gen_lm(n)
  lm_model <- lm(Y ~ ., lm_data)
  thin_amt <- seq(.95, .05, -0.05)
  thin_results <- thinning(lm_model, lm_data)
  expect_equal(length(thin_results$RMSE), 19)
  expect_equal(length(thin_results$predictions), 19)
  for (i in seq_len(length(thin_amt))) {
    expect_equal(length(thin_results$predictions[[i]]), thin_amt[i] * n)
  }
})
