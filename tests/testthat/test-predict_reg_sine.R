test_that("correct output of predict sine", {
  set.seed(43421)
  test_data <- data_gen_sine(4, weight_mat = matrix(rep(c(1, 1, 0), 4),
                                                    nrow = 3, ncol = 4),
                             resp_sd = 0)
  test_obj <- reg_sine(Y ~ ., test_data)
  test_pred <- predict(test_obj, test_data[, -1])
  expect_equal(unname(round(test_pred, 2)), c(2.11, 0.12, -0.29, 1.06))

  # 1-dimensional case
  test_one <- data_gen_sine(4, weight_mat = matrix(c(1, 1, 0),
                                                   nrow = 3, ncol = 1))
  test_one_obj <- reg_sine(Y ~ ., test_one)
  expect_equal(length(predict(test_one_obj, test_one)), 4)
})
