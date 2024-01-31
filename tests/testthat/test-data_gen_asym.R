test_that("correct output from data_gen_asym", {
  set.seed(43421)
  test_data <- data_gen_asym(4, weight_mat = matrix(rep(1, 4), ncol = 2,
                                                    nrow = 2), resp_sd = 0)
  mat <- matrix(c(-0.01539, -0.00872, -0.00003, -0.36638,
                  9.54178, 5.13422, 10.72029, 4.07060,
                  4.17886, 5.86805, 11.38766, 1.05179),
                ncol = 3, nrow = 4)
  expect_equal(round(test_data$Y, 5), mat[, 1])
  expect_equal(round(test_data$V1, 5), mat[, 2])
  expect_equal(round(test_data$V2, 5), mat[, 3])
  expect_equal(nrow(test_data), 4)
  expect_equal(ncol(test_data), 3)
})
