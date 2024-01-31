#' @rdname predict
#' @examples
#'  # Sinusoidal Examples
#'  sine_data <- data_gen_sine(10)
#'  sine_fit <- reg_sine(Y ~ ., sine_data)
#'  predict(sine_fit, sine_data)
#' @importFrom stats delete.response
#' @export
predict.reg_sine <- function(object, newdata, ...) {
  par <- object$par
  formula <- delete.response(formula(object))
  X <- model.matrix(formula, data = newdata)[, -1]
  if (!is.element("matrix", class(X))) {
    X <- as.matrix(X, ncol = 1)
  }
  y_pred <- sine_yhat(par, X)
  y_pred
}
