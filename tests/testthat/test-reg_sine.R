test_that("check to see reg_sine outputs correctly", {
  set.seed(43421)
  test_data <- data_gen_sine(4, weight_mat = matrix(rep(c(1, 1, 0), 4),
                                                    nrow = 3, ncol = 4),
                             resp_sd = 0)
  test_obj <- reg_sine(Y ~ ., test_data)
  expect_equal(class(test_obj), "reg_sine")
  expect_equal(length(test_obj$par), 13)
})

# Check for y_hat
