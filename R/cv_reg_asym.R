#' @describeIn cv Cross-Validation for reg_asym
#' @importFrom stats formula
#' @examples
#'  # Asymptotic example
#'  asym_data <- data_gen_asym(10)
#'  asym_fit <- reg_asym(Y ~ ., asym_data)
#'  cv(asym_fit, asym_data, n_folds = 2)
#'
#' @export
cv.reg_asym <- function(object, data, n_folds = 10, k_mult = NULL,
                        repl = FALSE, grouping_formula = NULL) {
  data_check(formula(object), data)
  integer_check(n_folds)
  groups <- create_groups(formula(object), data, n_folds, k_mult, repl,
                          grouping_formula)
  predictions <- cv_core(object, data, groups)
  predictions
}
