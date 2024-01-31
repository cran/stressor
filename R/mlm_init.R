#' Compare Machine Learning Models
#'
#' Through the \href{https://pycaret.gitbook.io/docs/get-started/quickstart}{PyCaret}
#'   module from `python`, this function fits many machine
#'   learning models simultaneously without requiring any `python`
#'   programming on the part of the user. This is the core function to fitting
#'   the initial models. This function is the backbone to fitting all the models.
#' @param formula The regression formula or classification formula. This formula
#'  should be linear.
#' @param train_data A data.frame object that includes data to be trained on.
#' @param fit_models A character vector with all the possible Machine Learning
#'   regressors that are currently being fit. The user may specify a subset of
#'   them using a character vector.
#'   \tabular{rl}{
#'     ada \tab AdaBoost Regressor \cr
#'     br \tab Bayesian Ridge \cr
#'     dt \tab Decision Tree Regressor \cr
#'     dummy \tab Dummy Regressor \cr
#'     en \tab Elastic Net \cr
#'     et \tab Extra Trees Regressor \cr
#'     gbr \tab Gradient Boosting Regressor \cr
#'     huber \tab Huber Regressor \cr
#'     knn \tab K Neighbors Regressor \cr
#'     lar \tab Least Angle Regression \cr
#'     lasso \tab Lasso Regression \cr
#'     lightgbm \tab Light Gradient Boosting Machine \cr
#'     llar \tab Lasso Least Angle Regression \cr
#'     lr \tab Linear Regression \cr
#'     omp \tab Orthogonal Matching Pursuit \cr
#'     par \tab Passive Aggressive Regressor \cr
#'     rf \tab Random Forest Regressor\cr
#'     ridge \tab Ridge Regression
#'   }
#'  If classification is set to `TRUE`, these models can be used depending on user.
#'  These are the default values for classification:
#'   \tabular{rl}{
#'     ada \tab AdaBoost Classifier \cr
#'     dt \tab Decision Tree Classifier\cr
#'     dummy \tab Dummy Classifier\cr
#'     et \tab Extra Trees Classifier \cr
#'     gbc \tab Gradient Boosting Classifier\cr
#'     knn \tab K Neighbors Classifier\cr
#'     lda \tab Linear Discriminant Analysis\cr
#'     lightgbm \tab Light Gradient Boosting Machine\cr
#'     lr \tab Logistic Regression\cr
#'     nb \tab Naive Bayes \cr
#'     qda \tab Quadratic Discriminant Analysis\cr
#'     rf \tab Random Forest Classifier\cr
#'     ridge \tab Ridge Classifier\cr
#'     svm \tab SVM - Linear Kernel
#'   }
#' @param sort_v A character vector indicating what to sort the tuned models on.
#'  Default value is `NULL`.
#' @param n_models A defaulted integer to return the maximum number of models.
#' @param classification A Boolean value tag to indicate if classification
#'   methods should be used.
#' @param seed An integer value to set the seed of the python environment.
#'   Default value is set to `NULL`.
#' @param ... Additional arguments passed to the setup function in `PyCaret`.
#' @return A list object that contains all the fitted models and the CV
#'   predictive accuracy. With a class attribute of `"mlm_stressor"`.
#' @details The formula should be linear. However, that does not imply a linear
#'   fit. The formula is a convenient way to separate predictor
#'   variables from explanatory variables.
#'
#'   `PyCaret` is a `python` module where machine learning models can be fitted with
#'   little coding by the user. The pipeline that `PyCaret` uses has a
#'   setup function to parameterize the data that is easy for all the models to
#'   fit on. Then compare models function is executed which fits all the models
#'   that are currently available. This process takes less than five minutes for
#'   data.frame objects that are less than 10,000 rows.
#' @inherit mlm_regressor examples
#' @importFrom stats model.frame terms
mlm_init <- function(formula, train_data, fit_models, sort_v = NULL,
                     n_models = 9999, classification = FALSE,
                     seed = NULL, ...) {
  # Declaring Constants
  regress_models <- c('ada', 'br', 'dt', 'dummy', 'en', 'et', 'gbr', 'huber',
                      'knn', 'lar', 'lasso', 'lightgbm', 'llar', 'lr', 'omp',
                      'par', 'rf', 'ridge')
  class_models <- c('ada', 'dt', 'dummy', 'et', 'gbc', 'knn', 'lda', 'lightgbm',
                    'lr', 'nb', 'qda', 'rf', 'ridge', 'svm')
  regress_sort <- c('MAE', 'MSE', 'RMSE', 'R2', 'RMSLE', 'MAPE')
  class_sort <- c('Accuracy', 'AUC', 'Recall', 'Precision', 'F1', 'Kappa',
                  'MCC')
  # Function starts here
  data <- model.frame(formula = formula, data = train_data)
  vv <- attr(terms(formula(train_data)), which = "variables")
  rr <- as.character(vv[[2]]) # The response variable name
  if (classification) {
    if (is.null(sort_v) || length(sort_v) > 1) {
      sort_v <- 'Accuracy'
    }
    if (!(is.element(sort_v, class_sort))) {
      stop("The current sorting method is not supported.")
    }
    if (!all(is.element(fit_models, class_models))) {
      stop("The current models are not supported.")
    }
    message("Importing Pycaret Classification")
    reg <- reticulate::import("pycaret.classification")
  } else {
    if (is.null(sort_v) || length(sort_v) > 1) {
      sort_v <- 'RMSE'
    }
    if (!(is.element(sort_v, regress_sort))) {
      stop("The current sorting method is not supported.")
    }
    if (!all(is.element(fit_models, regress_models))) {
      stop("The current models are not supported.")
    }
    message("Importing Pycaret Regression")
    reg <- reticulate::import("pycaret.regression")
  }
  message("Setting up the data for fitting models.")
  if (!is.null(seed)) {
    exp_reg <- reg$setup(data = train_data, target = rr, n_jobs = as.integer(1),
                         session_id = as.integer(seed), system_log = FALSE,
                         memory = FALSE, ...)
    reg$set_config('seed', as.integer(seed))
  } else {
    exp_reg <- reg$setup(data = train_data, target = rr, system_log = FALSE,
                         n_jobs = as.integer(1), memory = FALSE, ...)
  }

  message("Fitting Machine Learning Models")

  models <- reg$compare_models(include = fit_models,
                               sort = sort_v,
                               n_select = as.integer(n_models))
  accuracy_measures <- reg$pull()

  obj <- list(models = models, pred_accuracy = accuracy_measures)
  # Possibly change to a formula/data type
  attr(obj, "formula") <- terms(formula, data = data)
  class(obj) <- "mlm_stressor"
  obj
}
