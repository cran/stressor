test_that("A vector of the same length of distances is returned", {
  lm_data <- data_gen_lm(10)
  distances <- dist_cent(Y ~ ., lm_data)
  expect_equal(nrow(lm_data), length(distances))
  expect_true(all(is.numeric(distances)))
})
