#' Cross Validation Function
#'
#' This is the machinery to run cross validation. It subsets the test and train
#'  set based on the groups it receives.
#' @param object Currently `"reg_sine", "reg_asym", "lm", "mlm_stressor"`
#'   objects are accepted.
#' @param data A data.frame object that has the same formula that was fitted on
#'   the data.
#' @param t_groups The groups for cross validation: standard cross validation,
#'   LOO cross_validation, or spatial cross validation.
#' @param ... Additional arguments that are passed to the predict function.
#' @return Either a vector of predictions for `"reg_sine", "reg_asym", "lm"` and
#'   a data frame for `"mlm_stressor"`.
#' @inherit cv examples
#' @importFrom stats predict formula lm
cv_core <- function(object, data, t_groups, ...) {
  data_check(formula(object), data)
  curr_methods <- c("reg_sine", "reg_asym", "lm", "mlm_stressor")
  method <- class(object)[1]
  if (!is.element(method, curr_methods)){
    stop("Not a supported method at this time!")
  }
  if (method == "mlm_stressor") {
    predictions <- matrix(0, nrow = nrow(data),
                          ncol = length(object$models))
  } else {
    predictions <- vector("numeric", length = nrow(data))
  }
  for (i in seq_len(max(t_groups))) {
    test_index <- which(i == t_groups)
    train <- data[-test_index, ]
    test <- data[test_index, ]
    if (method == "mlm_stressor") {
      classification = FALSE
      if (class(object)[2] == "classifier") {classification = TRUE}
      predictions[test_index, ] <- mlm_refit(object, train, test,
                                             classification)
    } else if (method == "reg_sine") {
      predictions[test_index] <- predict(reg_sine(formula(object),
                                                       data = train), test, ...)
    } else if (method == "reg_asym") {
      predictions[test_index] <- predict(reg_asym(formula(object),
                                                       data = train), test, ...)
    } else if (method == "lm"){
      predictions[test_index] <- predict(lm(formula(object),
                                                 data = train), test, ...)
    } else {
      stop("Current method is unsupported at this time.")
    }
  }
  if (method == "mlm_stressor") {
    colnames(predictions) <- row.names(object$pred_accuracy)
    predictions <- as.data.frame(predictions)
  }
  predictions
}
