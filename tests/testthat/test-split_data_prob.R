test_that("Returns a logical vector, the correct length, and throws error", {
  lm_data <- data_gen_lm(10)
  expect_equal(length(split_data_prob(lm_data, .8)), 10)
  expect_type(split_data_prob(lm_data, .8), "logical")
  expect_error(split_data_prob(lm_data, 2),
               "Training size must be between 0 and 1.")
  expect_equal(sum(split_data_prob(lm_data, 1)), 10)
  expect_equal(sum(split_data_prob(lm_data, 0)), 0)
})
