test_that("correct output of predict asymptotic", {
  set.seed(43421)
  test_data <- data_gen_asym(4, weight_mat = matrix(rep(1, 8),
                                                    nrow = 2, ncol = 4),
                             resp_sd = 0)
  test_obj <- reg_asym(Y ~ ., test_data)
  test_pred <- predict(test_obj, test_data[, -1])
  expect_setequal(round(test_pred, 2), c(-0.36, -0.11, -0.10, -0.45))
  expect_equal(length(test_pred), 4)

  # 1-dimensional case
  test_one <- data_gen_sine(4, weight_mat = matrix(c(1, 1, 0),
                                                   nrow = 3, ncol = 1))
  test_one_obj <- reg_sine(Y ~ ., test_one)
  expect_equal(length(predict(test_one_obj, test_one)), 4)
})
