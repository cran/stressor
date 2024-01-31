#' @describeIn cv Cross-Validation for mlm_stressor
#' @importFrom stats formula
#' @examplesIf python_avail()
#'  lm_test <- data_gen_lm(20)
#'  create_virtualenv()
#'  mlm_lm <- mlm_regressor(Y ~ ., lm_test)
#'  cv(mlm_lm, lm_test, n_folds = 2)
#' @export
cv.mlm_stressor <- function(object, data, n_folds = 10, k_mult = NULL,
                            repl = FALSE, grouping_formula = NULL) {
  data_check(formula(object), data)
  integer_check(n_folds)
  groups <- create_groups(formula(object), data, n_folds, k_mult, repl,
                          grouping_formula)
  predictions <- cv_core(object, data, groups)
  predictions
}



