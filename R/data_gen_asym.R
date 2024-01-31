#' Data Generation Asymptotic
#'
#' Creates a synthetic data set for an additive asymptotic model. See the details
#'  section for clarification.
#' @param n The number of observations for each parameter.
#' @param weight_mat The parameter coefficients, where each column represents
#'  the coefficients and is two rows as each additive equation contains two
#'  parameters. Defaulted to be 10 random numbers from the log-normal
#'  distribution. The second row of the matrix needs to be positive.
#' @param y_int The y-intercept term of the additive model.
#' @param resp_sd The standard deviation of the epsilon term to be added for
#'  noise.
#' @param window Used to determine for any given X variable to get you within
#'  distance to capture the asymptotic behavior.
#' @param ... Additional arguments that are not currently implemented.
#' @return A data.frame object with the n rows and the response variable with
#'  the number of parameters being equal to the number of columns from the
#'  weight matrix.
#' @details
#'  Observations are generated from the following model:
#'   \deqn{y = \sum_{i = 1}^n -\alpha_ie^{-\beta_i \cdot x_i} + y_{int}}
#'   Where `n` is the number of parameters to be used, \eqn{\alpha_i}'s
#'   are the scaling parameter and the \eqn{\beta_i}'s are the weights
#'   associated with each \eqn{x_i}. With the \eqn{y_{int}} being where it
#'   crosses the y-axis.
#' @examples
#'  # Generates 10 observations
#'  asym_data <- data_gen_asym(10)
#'  asym_data
#' @importFrom stats rlnorm runif rnorm
#' @export
data_gen_asym <- function(n, weight_mat = matrix(rlnorm(10), nrow = 2,
                                                 ncol = 5),
                          y_int = 0, resp_sd = 1, window = .00001, ...) {
  # User input checks
  integer_check(n)
  matrix_check(weight_mat)
  numeric_check(y_int)
  numeric_check(resp_sd)
  numeric_check(window)

  # Produce the Predictor variables
  vec_1 <- matrix(0, nrow = n, ncol = ncol(weight_mat))
  # Include the window parameter to capture
  for (i in seq_len(ncol(weight_mat))) {
    max_range <- -log(window) / weight_mat[2, i]
    vec_1[, i] <- runif(n, min = 0, max = max_range)
  }
  colnames(vec_1) <- paste0(rep("V", ncol(weight_mat)),
                            seq(ncol(weight_mat)))

  # Produce the response variable with noise
  eps <- rnorm(n, sd = resp_sd)
  vec_2 <- vec_1
  for (i in seq_len(ncol(weight_mat))) {
    prin <- weight_mat[1, i]
    rate <- weight_mat[2, i]
    vec_2[, i] <- -prin * exp(-rate * vec_1[, i])
  }
  Y <- y_int + rowSums(vec_2) + eps
  vec_1 <- cbind(Y, vec_1)
  as.data.frame(vec_1)
}
