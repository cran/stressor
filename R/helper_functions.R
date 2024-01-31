#' Data and Formula Helper Check Function
#'
#' Checks that the formula is a formula object, data is data.frame object, and
#'  if all the variables in the formula call are in the data.frame.
#' @param formula A formula object.
#' @param data A data.frame object.
#' @param train_data A data.frame object. Some methods include two data.frames
#'  as inputs.
#' @returns Stops the function from continuing its output by displaying an
#'  error message or nothing is returned.
#' @examples
#'  lm_data <- data_gen_lm(10)
#'  data_check(Y ~ ., lm_data)
#' @noRd
data_check <- function(formula, data, train_data = NULL) {
  if (!inherits(formula, "formula")) {
    stop("The formula object must be of class formula and in the form
         'Y ~ x1 + x2 + ...'")
  }
  if (!is.null(train_data)){
    if (!all(inherits(data, "data.frame"),
             inherits(train_data, "data.frame"))) {
      stop("Data must be inheritted from data.frame or be a data.frame.")
    }
  } else {
    if (!inherits(data, "data.frame")) {
      stop("Data must be inheritted from data.frame or be a data.frame.")
    }
  }
}

#' @title Integer Check Function
#' @description A helper function to check if value is an integer.
#' @param integer An integer value.
#' @return Allows the function to run or returns a user friendly error message.
#' @examples
#'  integer_check(1)
#' @noRd
integer_check <- function(integer) {
  if (integer %% 1 != 0) {
    stop("Value provided is not an integer.")
  }
}

#' @title Numeric Check Function
#' @description A helper function to check if value is an numeric.
#' @param numeric A numeric value.
#' @return Allows the function to run or returns a user friendly error message.
#' @examples
#'  numeric_check(1.5)
#' @noRd
numeric_check <- function(numeric) {
  if (!is.numeric(numeric)) {
    stop("Value provided is not a numeric value")
  }
}

#' @title Matrix Check Function
#' @description A helper function to check if the matrix is properly formatted.
#' @param matrix A matrix.
#' @return Allows the function to run or returns a user friendly error message.
#'  mat <- matrix(rep(1, 4), nrow = 2)
#'  matrix_check(mat)
#' @noRd
matrix_check <- function(matrix) {
  if (!inherits(matrix, "matrix")) {
    stop("Not a matrix object.")
  }
  if (is.null(dim(matrix))) {
    stop("Not a valid matrix.")
  }
}

#' @title Boolean Check Function
#' @description A helper function to check if value is a Boolean value.
#' @param boolean A Boolean value.
#' @return Allows the function to run or returns a user friendly error message.
#' @examples
#'  boolean_check(TRUE)
#' @noRd
boolean_check <- function(boolean) {
  if (!is.logical(boolean)) {
    stop("Value provided is not a Boolean value.")
  }
}

#' @title Check if `Python` is Available
#' @description A function that allows examples to run when appropriate.
#' @return A Boolean value is returned.
#' @examples
#'  python_avail()
#' @export
python_avail <- function() {
  reticulate::py_available()
}
