#' Fit Machine Learning Regressor Models
#'
#' Through the \href{https://pycaret.gitbook.io/docs/get-started/quickstart#regression}{PyCaret}
#'   module from `python`, this function fits many machine
#'   learning models simultaneously with without requiring any `python`
#'   programming on the part of the user. This function is
#'   specifically designed for the regression models.
#' @param formula A linear formula object.
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
#' @param sort_v A character vector indicating what to sort the tuned models on.
#' @param n_models An integer value defaulted to a large integer value to
#'   return all possible models.
#' @param seed An integer value to set the seed of the `python` environment.
#'   Default value is set to `NULL`.
#' @param ... Additional arguments passed onto \link[stressor]{mlm_init}.
#' @return A list object where the first entry is the models fitted and the
#'   second is the initial predictive accuracy on the random test data. Returns
#'   as two classes `"mlm_stressor"` and `"regressor"`.
#' @examplesIf python_avail()
#'  lm_test <- data_gen_lm(20)
#'  create_virtualenv()
#'  mlm_lm <- mlm_regressor(Y ~ ., lm_test)
#' @inherit mlm_classification details
#' @export
mlm_regressor <- function(formula, train_data,
                          fit_models = c('ada', 'et', 'lightgbm','gbr',
                                         'lr', 'rf', 'ridge', 'knn', 'dt',
                                         'dummy', 'lar', 'br', 'huber', 'omp',
                                         'lasso', 'en', 'llar', 'par'),
                          sort_v = c('MAE', 'MSE', 'RMSE', 'R2', 'RMSLE',
                                     'MAPE'),
                          n_models = 9999, seed = NULL, ...) {
  # Throw in a data_check/formula_check
  data_check(formula, train_data)
  integer_check(n_models)
  obj <- mlm_init(formula, train_data, fit_models, sort_v, n_models,
                  seed = seed, ...)
  class(obj) <- c(class(obj), "regressor")
  obj
}
