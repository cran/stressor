#' @describeIn cv Cross-Validation for reg_sine
#' @importFrom stats formula
#' @examples
#'  # Sine example
#'  sine_data <- data_gen_sine(10)
#'  sine_fit <- reg_sine(Y ~ ., sine_data)
#'  cv(sine_fit, sine_data, n_folds = 2)
#' @export
cv.reg_sine <- function(object, data, n_folds = 10, k_mult = NULL,
                        repl = FALSE, grouping_formula = NULL) {
  data_check(formula(object), data)
  integer_check(n_folds)
  groups <- create_groups(formula(object), data, n_folds, k_mult, repl,
                          grouping_formula)
  predictions <- cv_core(object, data, groups)
  predictions
}
