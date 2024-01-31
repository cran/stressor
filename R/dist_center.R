#' Distance to Center
#'
#' Calculates the distance from center of the matrix of predictor variables
#'  using a euclidean distance, or the average of all x-dimensions.
#' @param formula A formula object.
#' @param data A data.frame object.
#' @return A vector of distances from the center.
#' @details
#'  Formula used to calculate the center point:
#'  \deqn{\bar{x} = \frac{1}{N}\sum_{j = 1}^N x_{ij}}
#'  Where \strong{\eqn{\bar{x}}} is a vector of the center of the x-dimensions,
#'  \eqn{N} is the number of rows in the matrix, and \eqn{x_{ij}} is the
#'  \eqn{i,j^{th}} entry in the matrix.
#' @examples
#'   data <- data_gen_lm(10)
#'   dist <- dist_cent(Y ~ ., data)
#'   dist
#' @importFrom stats model.matrix model.response model.frame
#' @export
dist_cent <- function(formula, data){
  t_data <- model.matrix(formula, data = data)[, -1]
  t_response <- model.response(model.frame(formula, data = data))
  scaled_data <- scale(t_data)
  center <- apply(scaled_data, 2, mean)
  dist <- vector("numeric", length = nrow(data))
  dist <- apply(scaled_data, 1, function(x) {
    euclid <- euclid_dist(center, x)
    euclid
  })
  dist
}

euclid_dist <- function(x, y) {
  sqrt(sum((x - y)^2))
}
