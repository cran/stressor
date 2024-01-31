#' Create Train Index Set
#'
#' This function takes in a data.frame object and the training size and returns
#'   a logical vector indicating which entries to include.
#' @param data A data.frame object used to determine the length of the vector.
#' @param test_prop A numeric that is between zero and one that represents the
#' proportion of observations to be included in the test data.
#' @return A logical vector is returned that is the same length as the number of
#'   rows of the data.
#' @examples
#'   lm_data <- data_gen_lm(10)
#'   indices <- split_data_prob(lm_data, .8)
#'   train <- lm_data[indices, ]
#'   test <- lm_data[!indices, ]
#' @export
split_data_prob <- function(data, test_prop) {
  numeric_check(test_prop)
  if (!(test_prop >= 0 && test_prop <= 1)) {
    stop("Training size must be between 0 and 1.")
  }
  sample(c(TRUE, FALSE), nrow(data), replace = TRUE,
         prob = c(test_prop, 1 - test_prop))
}
