#' Asymptotic Regression
#'
#' A simple example of asymptotic regression that is in the form of
#'  \eqn{y = -e^{-x}} and is the sum of multiple of these exponential
#'  functions with a common intercept term.
#' @param formula A formula object to describe the relationship.
#' @param data The response and predictor variables.
#' @param method The method that is passed to the optim function. By default, it
#'  is the BFGS method which uses a gradient.
#' @param init_guess The initial parameter guesses for the optim function. By
#'  default, it is all ones.
#' @param ... Additional arguments passed to the optim function.
#' @return A "reg_asym" object is returned which contains the results from the
#'  optim function that was returned.
#' @importFrom stats model.frame terms
#' @examples
#'  asym_data <- data_gen_asym(10)
#'  reg_asym(Y ~ ., asym_data)
#' @export
reg_asym <- function(formula, data, method = "BFGS",
                     init_guess = rep(1, ncol(data) * 2 - 1), ...) {
  # Regression Function for Asymptotic
  # Y = a - b * e^(-c * X_1) - d * e^(-f * X_2) - ... - g * e^(-h * X_n)
  temp <- model.frame(formula = formula, data = data)
  X <- temp[, -1]
  Y <- temp[, 1]
  obj <- asym_optimize(init_guess, X, Y, method = method, ...)
  attr(obj, "formula") <- terms(formula, data = data)
  class(obj) <- "reg_asym"
  obj
}

#' @title Asymptotic Function for the Optim Function
#' @description It returns the loss of the additive asymptotic function for the
#'   \link[stats]{optim} function.
#' @inheritParams asym_yhat
#' @param Y A vector of the observed results used to calculate the loss
#'   function.
#' @return A numeric value representing the error with the current parameter
#'   estimates. Evaluates the \eqn{\sum_{i = 1}^n y_i - \hat{y_i}}, which is the
#'   mean squared error.
#' @details This function is solely for the purpose of optim function.
#' @noRd
asym_function <- function(estimated, X, Y) {
  Y_pred <- asym_yhat(estimated, X)
  error <- sum((Y - Y_pred)^2, na.rm=TRUE)
  error
}

#' @title Gradient Asymptotic Function used for the Optim Function
#' @description This is the gradient function used for the \link[stats]{optim}
#'  for the `"BFGS"` optimization of optim.
#' @inheritParams asym_function
#' @return The gradient of the loss with the current parameter estimates.
#' @noRd
asym_gradient <- function(estimated, X, Y) {
  Y_pred <- asym_yhat(estimated, X)
  vec_2 <- vector(length = length(estimated))
  est_mat <- matrix(estimated[-length(estimated)], nrow = 2, ncol = ncol(X))
  for (i in seq_len(ncol(X))) {
    prin <- est_mat[1, i]
    rate <- est_mat[2, i]
    vec_2[2 * i - 1] <- sum(exp(-rate * X[, i]))
    vec_2[2 * i] <- sum(-prin * X[, i] * exp(-rate * X[, i]))
  }
  vec_2[length(estimated)] <- -1 * nrow(X)
  vec_2 <- sum(2 * (Y - Y_pred)) * vec_2
  vec_2
}

#' @title Predictions for the Additive Asymptotic Model
#' @description Fits the Additive Sinusoidal model with the current coefficients
#'   and the current predictor variables.
#' @param estimated A vector of the current guesses on the coefficients of the
#'   model.
#' @param X A matrix of the predictor variables.
#' @return A vector of predictions from the model.
#' @details When you want predictions from the model use `asym_yhat`. If you
#'  want the loss then use the `asym_func`, as `optim` requires that function
#'  returns the loss value.
#' @noRd
asym_yhat <- function(estimated, X) {
  vec_2 <- X
  est_mat <- matrix(estimated[-length(estimated)], nrow = 2, ncol = ncol(X))
  for (i in seq_len(ncol(X))) {
    prin <- est_mat[1, i]
    rate <- est_mat[2, i]
    vec_2[, i] <- -prin * exp(-rate * X[, i])
  }
  Y_pred <- estimated[length(estimated)] + rowSums(vec_2)
  Y_pred
}

#' @title Optim function for the Additive Asymptotic Regression Model
#' @description Utilizes the [stats::optim()] function to perform the
#'   optimization of the curve.
#' @param init_guess The initial parameter guesses for the optim function. By
#'  default, it is all ones.
#' @param X A matrix of the predictor space.
#' @param Y A vector of the observed results used to calculate the Loss
#'   function.
#' @param method The method to be used. See method in
#'   \code{\link[stats]{optim}}.
#' @param ... Additional arguments passed to the \code{\link[stats]{optim}}
#'   function.
#' @inherit stats::optim return
#' @importFrom stats optim
#' @noRd
asym_optimize <- function(init_guess, X, Y, method, ...) {
  opt <- optim(init_guess, fn = asym_function, gr = asym_gradient,
               method = method, X, Y, ...)
}
