#' @describeIn cv Cross-Validation for lm
#' @examples
#'  # lm example
#'  lm_test <- data_gen_lm(20)
#'  lm <- lm(Y ~ ., lm_test)
#'  cv(lm, lm_test, n_folds = 2)
#'
#' @importFrom stats formula
#' @export
cv.lm <- function(object, data, n_folds = 10, k_mult = NULL, repl = FALSE,
                  grouping_formula = NULL) {
  data_check(formula(object), data)
  integer_check(n_folds)
  groups <- create_groups(formula(object), data, n_folds, k_mult, repl,
                          grouping_formula)
  predictions <- cv_core(object, data, groups)
  predictions
}
