#' @rdname predict
#' @examples
#'  # Asymptotic Examples
#'  asym_data <- data_gen_asym(10)
#'  asym_fit <- reg_asym(Y ~ ., asym_data)
#'  predict(asym_fit, asym_data)
#' @importFrom stats delete.response
#' @export
predict.reg_asym <- function(object, newdata, ...) {
  par <- object$par
  formula <- delete.response(formula(object))
  X <- model.matrix(formula, data = newdata)[, -1]
  if (!is.element("matrix", class(X))) {
    X <- as.matrix(X, ncol = 1)
  }
  y_pred <- asym_yhat(par, X)
  y_pred
}
