#' Fit Machine Learning Classification Models
#'
#' Through the \href{https://pycaret.gitbook.io/docs/get-started/quickstart#classification}{PyCaret}
#'   module from `python`, this function fits many machine
#'   learning models simultaneously without requiring any `python`
#'   programming on the part of the user. This function is specifically
#'   designed for the classification models fitted by `PyCaret`.
#' @param formula The classification formula, as a formula object.
#' @param train_data A data.frame object that includes data to be trained on.
#' @param fit_models A character vector with all the possible Machine Learning
#'   classifiers that are currently being fit, the user may specify a subset of
#'   them using a character vector.
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
#' @param n_models An integer value defaulted to a large integer value to
#'   return all possible models.
#' @param seed An integer value to set the seed of the `python` environment.
#'   Default value is set to `NULL`.
#' @param ... Additional arguments passed onto \link[stressor]{mlm_init}.
#' @return A list object where the first entry is the models fitted and the
#'   second is the initial predictive accuracy on the random test data. Returns
#'   as two classes `"mlm_stressor"` and `"classifier"`.
#' @details
#'  `PyCaret` is a `python` module where machine learning models can be fitted with
#'   little coding by the user. The pipeline that `PyCaret` uses is a
#'   setup function to parameterize the data that is easy for all the models to
#'   fit on. Then the compare models function is executed, which fits all the models
#'   that are currently available. This process takes less than five minutes for
#'   data.frame objects that are less than 10,000 rows.
#' @examplesIf python_avail()
#'  lm_test <- data_gen_lm(20)
#'  binary_response <- sample(c(0, 1), 20, replace = TRUE)
#'  lm_test$Y <- binary_response
#'  mlm_class <- mlm_classification(Y ~ ., lm_test)
#' @export
mlm_classification <- function(formula, train_data,
                               fit_models = c('ada', 'et', 'lightgbm','dummy',
                                              'lr', 'rf', 'ridge', 'knn', 'dt',
                                              'gbc', 'svm', 'lda', 'nb', 'qda'),
                               sort_v = c('Accuracy', 'AUC', 'Recall',
                                          'Precision', 'F1', 'Kappa', 'MCC'),
                               n_models = 9999, seed = NULL, ...) {
  obj <- mlm_init(formula, train_data, fit_models, sort_v, n_models,
                  classification = TRUE, seed = seed, ...)
  class(obj) <- c(class(obj), "classifier")
  obj
}


