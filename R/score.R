#' Score Function for Metrics
#'
#' A score function takes the observed and predicted values and returns a
#'   vector or data.frame of the various metrics that are reported from `PyCaret`.
#'   For regression, the following metrics are available: `RMSE`, `MAE`, `MSE`,
#'   `R2`, `RMSLE`, and `MAPE`. For classification, the following metrics are
#'   available:`Accuracy`, `AUC`, `Recall`, `Prec.`, `F1`, `MCC`, and `Kappa`.
#' @param observed A vector of the observed results.
#' @param predicted A data.frame or vector object that is the same number of
#'   rows or length as the length of observed values.
#' @inheritDotParams score_classification metrics
#' @inheritDotParams score_regression metrics
#' @return A matrix with the various metrics reported.
#' @examples
#' lm_data <- data_gen_lm(100)
#' indices <- split_data_prob(lm_data, .2)
#' train <- lm_data[!indices,]
#' test <- lm_data[indices,]
#' model <- lm(Y ~ ., train)
#' pred_lm <- predict(model, test)
#' score(test$Y, pred_lm)
#' @export
score <- function(observed, predicted, ...) {
  if (length(unique(observed)) < 5 || is.factor(observed)) {
    result <- score_classification(observed, predicted, ...)
  } else {
    result <- score_regression(observed, predicted, ...)
  }
  result
}
