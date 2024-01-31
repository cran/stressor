#' Cross Validation
#'
#' This is the core of cross-validation- both standard and using k-mean groups.
#'  This method is called by other cv methods of classes.
#' @param object One of the four objects that is accepted: mlm_stressor,
#'   reg_sine, reg_asym, or lm.
#' @param data A data.frame object that contains all the entries to be
#'  cross-validated on.
#' @param n_folds An integer value for the number of folds defaulted to 10. If
#'   NULL, it will run LOO cross-validation.
#' @param k_mult Used to specify if k-means clustering is to be used, defaulted
#'   to NULL.
#' @param repl A Boolean value defaulted to `FALSE`, change to `TRUE` when
#'   replicates need to be included in the same group.
#' @param grouping_formula A formula object that specifies how the groups will
#'   be gathered.
#' @return If the object is of class mlm_stressor, then a data.frame will be
#'   returned. Otherwise, a vector of the predictions will be returned.
#' @export
cv <- function(object, data, n_folds = 10, k_mult = NULL, repl = FALSE,
               grouping_formula = NULL) {
  data_check(formula(object), data)
  integer_check(n_folds)
  UseMethod("cv")
}

