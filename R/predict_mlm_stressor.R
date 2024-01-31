#' @rdname predict
#' @examplesIf python_avail()
#'  # mlm_stressor example
#'  lm_test <- data_gen_lm(20)
#'  create_virtualenv()
#'  mlm_lm <- mlm_regressor(Y ~ ., lm_test)
#'  predict(mlm_lm, lm_test)
#' @export
predict.mlm_stressor <- function(object, newdata, train_data = NULL, ...) {
  data_check(formula(object), newdata, train_data)
  classification = FALSE
  if (class(object)[2] == "classifier") {classification = TRUE}
  pred <- mlm_refit(object, train_data, test_data = newdata, classification)
  pred <- as.data.frame(pred)
  pred
}
