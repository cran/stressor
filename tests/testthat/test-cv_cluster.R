test_that("Correct number of groups created and number of data points
          preserved", {
  data_lm <- data_gen_lm(50)
  groups <- create_groups(Y ~ ., data_lm, n_folds = 5, k_mult = 5)
  expect_equal(length(groups), 50)
  expect_equal(length(unique(groups)), 5)
  expect_true(is.integer(groups))
})
