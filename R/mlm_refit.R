#' Refit Machine Learning Models
#'
#' Refits models fitted in the \link[stressor]{mlm_init}, and returns the
#'  predictions.
#' @param mlm_object A `"mlm_stressor"` object.
#' @param train_data A data.frame object used for refitting excludes the test
#'   data. Can be `NULL` to allow for predictions to be used on the current
#'   model.
#' @param test_data A data.frame object used for predictions.
#' @param classification A Boolean value used to represent if classification
#'   methods need to be used to refit the data.
#' @return A matrix with the predictions of the various machine learning
#'   methods.
#' @examplesIf python_avail()
#'  lm_train <- data_gen_lm(20)
#'  train_idx <- sample.int(20, 5)
#'  train <- lm_train[train_idx, ]
#'  test <- lm_train[-train_idx, ]
#'  create_virtualenv()
#'  mlm_lm <- mlm_regressor(Y ~ ., lm_train)
#'  mlm_refit(mlm_lm, train, test, classification = FALSE)
#' @export
mlm_refit <- function(mlm_object, train_data, test_data,
                      classification = FALSE) {
  if (classification) {
    file <- system.file("python", "refit_class.py", package = "stressor")
  } else {
    file <- system.file("python", "refit.py", package = "stressor")
  }

  # Check the input
  data_check(formula(mlm_object), test_data, train_data)

  test_data <- model.frame(formula(mlm_object), test_data)

  prediction_mlm <- matrix(0, nrow = nrow(test_data),
                           ncol = length(mlm_object$models))

  modelnames <- row.names(mlm_object$pred_accuracy)
  colnames(prediction_mlm) <- modelnames
  #prediction_mlm <- vector(mode = "list", length = 18)

  if (!is.null(train_data)) {
    # Re-position if necessary the data
    train_data <- model.frame(formula(mlm_object), train_data)

    # For the python file to execute
    refit_mlm_X <- as.matrix(train_data[, -1])
    refit_mlm_y <- as.matrix(train_data[, 1])
    refit_mlm_test <- as.matrix(test_data[, -1])
    for (i in seq_len(length(mlm_object$models))) {
      refit_mlm_temp <- mlm_object$models[[i]]
      reticulate::source_python(file)
      prediction_mlm[, i] <- refit(refit_mlm_X, refit_mlm_y, refit_mlm_test,
                                   refit_mlm_temp)
    }
  } else {
    # Import PyCaret Predict
    if (classification){
      reg <- reticulate::import("pycaret.classification")
    } else {
      reg <- reticulate::import("pycaret.regression")
    }
    # Predicting generally
    for (i in seq_len(length(mlm_object$models))){
      model <- mlm_object$models[[i]]
      #prediction_mlm[[i]] <- reg$predict_model(model,test_data)
      prediction_mlm[, i] <- reg$predict_model(model,
                                               test_data)[, "prediction_label"]
    }
  }
  prediction_mlm
}

