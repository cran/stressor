test_that("check to see reg_asym outputs correctly", {
  set.seed(43421)
  test_data <- data_gen_asym(4, weight_mat = matrix(rep(1, 8),
                                                    nrow = 2, ncol = 4),
                             resp_sd = 0)
  test_obj <- reg_asym(Y ~ ., test_data)
  expect_equal(class(test_obj), "reg_asym")
  expect_equal(length(test_obj$par), 9)
})
