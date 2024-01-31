test_that("correct output from data_gen_lm", {
  set.seed(43421)
  test_data <- data_gen_lm(4, weight_vec = c(1, 2, .5, 3), resp_sd = 0)
  mat <- matrix(c(8.97,  0.95,  0.86,  1.18,  1.91,
                  -0.85,  1.48,  0.77, -0.63, -1.19,
                  -7.46, -0.35, -1.45,  1.39, -1.64,
                  0.64,  2.29,  0.68,  0.57, -1.10),
                ncol = 5, nrow = 4, byrow = TRUE)
  expect_equal(round(test_data$Y, 2), mat[, 1])
  expect_equal(round(test_data$V1, 2), mat[, 2])
  expect_equal(round(test_data$V2, 2), mat[, 3])
  expect_equal(nrow(test_data), 4)
  expect_equal(ncol(test_data), 5)
})
