#' Sinusoidal Regression
#'
#' A simple example of sinusoidal regression that is in the form of
#'  \eqn{y = asin(b(x - c))} and is the sum of of multiple of these sine
#'  functions with a common intercept term.
#' @param formula A formula object to describe the relationship.
#' @param data The response and predictor variables.
#' @param method The method that is passed to the optim function. By default, it
#'  is the BFGS method which uses a gradient.
#' @param init_guess The initial parameter guesses for the optim function. By
#'  default, it is all ones.
#' @param ... Additional arguments passed to the optim function.
#' @return A "reg_sine" object is returned which contains the results from the
#'  optim function that was returned.
#' @importFrom stats model.frame terms
#' @examples
#'  sine_data <- data_gen_sine(10)
#'  reg_sine(Y ~ ., sine_data)
#' @export
reg_sine <- function(formula, data, method = "BFGS",
                     init_guess = rep(1, ncol(data) * 3 - 2), ...){
  temp <- model.frame(formula = formula, data = data)
  Y <- temp[, 1]
  X <- as.matrix(temp[, -1])

  obj <- sine_optimize(init_guess, X, Y, method, ...)
  attr(obj, "formula") <- terms(formula, data = data)
  class(obj) <- "reg_sine"
  obj
}

#' @title Sine function for the Optim Function
#' @description It returns the loss of the additive sinusoidal function for the
#'   \link[stats]{optim()} function.
#' @inheritParams sine_yhat
#' @param Y A vector of the observed results used to calculate the loss
#'   function.
#' @return A numeric value representing the error with the current parameter
#'   estimates.
#' @details This function is solely for the purpose of optim function.
#' @noRd
sine_function <- function(estimated, X, Y) {
  Y_pred <- sine_yhat(estimated, X)
  error <- sum((Y - Y_pred)^2, na.rm=TRUE)
  error
}

#' @title Predictions for the Additive Sinusoidal Model
#' @description Fits the Additive Sinusoidal model with the current coefficients
#'   and the current predictor space.
#' @param estimated A vector of the current guesses on the coefficients of the
#'   model.
#' @param X A matrix of the predictor variables.
#' @return A vector of predictions from the model.
#' @details When you want predictions from the model, use `sine_yhat`. If you
#'  want the loss then use the `sine_func` as `optim` requires that function
#'  returns the loss value.
#' @noRd
sine_yhat <- function(estimated, X) {
  vec_2 <- X
  cols <- ncol(X)
  #print(cols)
  if (is.null(cols)) {
    Y_pred <- NA
  }
  else if (cols == 1){
  amp <- estimated[1]
  per <- estimated[2]
  shift <- estimated[3]
  b_0 <- estimated[4]
  Y_pred <- amp * sin(per * (X - shift)) + b_0
  Y_pred <- t(Y_pred)
  } else {
      par <- estimated[-length(estimated)]
      est_mat <- matrix(par, nrow = 3, ncol = cols)
      for (i in seq_len(cols)) {
        amp <- est_mat[1, i]
        per <- est_mat[2, i]
        shift <- est_mat[3, i]
        vec_2[, i] <- amp * sin(per * (X[, i] - shift))
      }
      Y_pred <- rowSums(vec_2) + estimated[length(estimated)]
  }
  Y_pred
}

#' @title Gradient Sine Function used for Optim
#' @description This is the gradient function used for the \link[stats]{optim()}
#'  for the `"BFGS"` optimization of optim.
#' @inheritParams sine_function
#' @return The gradient of the loss with the current parameter estimates.
#' @noRd
sine_gradient <- function(estimated, X, Y) {
  vec_2 <- vector("numeric", length = length(estimated))
  Y_pred <- sine_yhat(estimated, X)
  est_mat <- matrix(estimated[-length(estimated)], nrow = 3, ncol = ncol(X))
  for (i in seq_len(ncol(est_mat))) {
    amp <- est_mat[1, i]
    per <- est_mat[2, i]
    shft <- est_mat[3, i]
    vec_2[3 * i - 2] <- sum((-1) * sin(per * (X[, i] - shft)))
    vec_2[3 * i - 1] <- sum((-1) * amp * (X[, i] - shft) *
                              cos(per * (X[, i] - shft)))
    vec_2[3 * i] <- sum(amp * per * cos(per * (X[, i] - shft)))
  }
  vec_2[length(estimated)] <- -1 * nrow(X)
  vec_2 <- sum(2 * (Y - Y_pred)) * vec_2
  vec_2
}

#' @title Optim function for the Additive Sinusoidal Regression Model
#' @description Utilizes the \link[stats]{optim()} function to perform the
#'   optimization of the curve.
#' @param init_guess The initial parameter guesses for the optim function. By
#'  default, it is all ones.
#' @param X A matrix of the predictor variables.
#' @param Y A vector of the observed results used to calculate the Loss
#'   function.
#' @param method The method to be used. See method in
#'   \link[stats]{optim}.
#' @param ... Additional arguments passed to the \link[stats]{optim} function.
#' @inherit stats::optim return
#' @importFrom stats optim
#' @noRd
sine_optimize <- function(init_guess, X, Y, method, ...) {
  opt <- optim(init_guess, fn = sine_function,
               gr = sine_gradient, method = method, X, Y, ...)
}
