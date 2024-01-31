#' Data Generation for Linear Regression
#'
#' Creates a synthetic data set for an additive linear model. See details for
#'  clarification.
#' @param n The number of observations for each parameter.
#' @param weight_vec The parameter coefficients where each entry represents the
#'  coefficients for the additive linear model.
#' @param y_int The y-intercept term of the additive model.
#' @param resp_sd The standard deviation of the epsilon term to be added for
#'  noise.
#' @param ... Additional arguments that are not currently implemented.
#' @return A data.frame object with the n rows and the response variable with
#'  the number of parameters being equal to the number of columns from the
#'  weight matrix.
#' @details
#'  Observations are generated from the following model:
#'   \deqn{y = \sum_{i = 1}^n \alpha_i\cdot x_i + y_{int}}
#'   Where `n` is the number of parameters to be used and the \eqn{\alpha_i}'s
#'   are the weights associated with each \eqn{x_i}. With the \eqn{y_{int}}
#'   being where it crosses the y-axis.
#' @examples
#'  # Generates 10 observations
#'  lm_data <- data_gen_lm(10)
#'  lm_data
#' @importFrom stats rnorm
#' @export
data_gen_lm <- function(n, weight_vec = rep(1, 5), y_int = 0,
                        resp_sd = 1, ...) {
  # User input check
  integer_check(n)
  numeric_check(weight_vec)
  numeric_check(y_int)
  numeric_check(resp_sd)

  # This chunk makes the predictor variables
  vec_1 <- rnorm(n * length(weight_vec))
  vec_1 <- matrix(vec_1, nrow = n, ncol = length(weight_vec))

  colnames(vec_1) <- paste0(rep("V", length(weight_vec)),
                            seq(length(weight_vec)))
  # Create the Response Variable with the specified weights
  eps <- rnorm(n, sd = resp_sd)
  Y <- as.vector(t(weight_vec) %*% t(vec_1)) + y_int + eps
  vec_1 <- cbind(Y, vec_1)
  as.data.frame(vec_1)
}
