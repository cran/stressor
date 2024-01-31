test_that("correct output from data_gen_sine", {
  set.seed(43421)
  test_data <- data_gen_sine(4, weight_mat = matrix(rep(c(1,1,0),2), ncol = 2,
                                                    nrow = 3), resp_sd = 0)
  mat <- matrix(c(1.57, 1.69, -1.34, 1.38,
                  0.95, 1.48, -0.35, 2.29,
                  0.86, 0.77, -1.45, 0.68),
                ncol = 3, nrow = 4)
  expect_equal(round(test_data$Y, 2), mat[, 1])
  expect_equal(round(test_data$V1, 2), mat[, 2])
  expect_equal(round(test_data$V2, 2), mat[, 3])
  expect_equal(nrow(test_data), 4)
  expect_equal(ncol(test_data), 3)
})
