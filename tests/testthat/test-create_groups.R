test_that("groups are created and rows are preserved", {
  data_lm <- data_gen_lm(25)
  groups <- create_groups(Y ~ ., data_lm, n_folds = 5)
  expect_equal(length(groups), 25)
  expect_equal(length(unique(groups)), 5)
  expect_true(is.integer(groups))
})
