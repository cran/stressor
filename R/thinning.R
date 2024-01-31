#' Thinning Algorithm for Models with Predict Function
#'
#' Fits various train size and test sizes.
#' @param model A model that is currently of class type "reg_sine", "reg_asym",
#'   "lm", or "mlm_stressor".
#' @param data A data frame with all the data.
#' @param max A numeric value in (0, 1] and greater than `min`,
#'  defaulted to .95.
#' @param min A numeric value in (0, 1) and less than `max`, defaulted to .05.
#' @param iter A numeric value to indicate the step size, defaulted to .05.
#' @param classification A Boolean value defaulted `FALSE`, used for
#'   `mlm_classification`.
#' @return A list of objects, where the first element is the RMSE values at each
#'   iteration and the second element is the predictions.
#' @examples
#'  lm_data <- data_gen_lm(1000)
#'  lm_model <- lm(Y ~ ., lm_data)
#'  thin_results <- thinning(lm_model, lm_data)
#' @export
thinning <- function(model, data, max = .95, min = .05, iter = .05,
                     classification = FALSE) {
  train_size <- seq(min, max, iter)
  curr_methods <- c("reg_sine", "reg_asym", "lm", "mlm_stressor", "randomForest.formula")
  method <- class(model)[1]
  data <- model.frame(formula = formula(model), data = data)
  vv <- attr(terms(formula(data)), which = "variables")
  rr <- as.character(vv[[2]])
  if (!is.element(method, curr_methods)){
    stop("Not a supported method at this time!")
  }
  predictions <- vector("list", length = length(train_size))
  if (method == "mlm_stressor") {
    pred_rmse <- matrix(0, nrow = length(model$models),
                        ncol = length(train_size))
  } else {
    pred_rmse <- vector("numeric", length = length(train_size))
  }
  for (i in seq_len(length(train_size))) {
    train_index <- sample(1:nrow(data), train_size[i] * nrow(data))
    test <- data[-train_index, ]
    train <- data[train_index, ]
    if (method == "mlm_stressor") {
      classification = FALSE
      if (class(model)[2] == "classifier") {classification = TRUE}
      predictions[[i]] <- mlm_refit(model, train, test,
                                             classification)
    } else if (method == "reg_sine") {
      predictions[[i]] <- predict(reg_sine(formula(model),
                                                  data = train), test)
    } else if (method == "reg_asym") {
      predictions[[i]] <- predict(reg_asym(formula(model),
                                                  data = train), test)
    } else if (method == "lm"){
      predictions[[i]] <- predict(lm(formula(model),
                                            data = train), test)
    } else {
      stop("Current method is unsupported at this time.")
    }
    if (classification) {
      if (method == "mlm_stressor") {
        for (j in seq_len(ncol(predictions[[i]]))){
          if (!all(suppressWarnings(is.na(as.integer(predictions[[i]][, j]))))) {
            predictions[[i]][, j] <- as.integer(predictions[[i]][, j])
          }
          pred_rmse[j, i] <- sum(test[, rr] == predictions[[i]][, j]) /
            nrow(test)
        }
      } else {
        pred_rmse[i] <- sum(test[, rr] == predictions[[i]]) / nrow(test)
      }
    } else {
      if (method == "mlm_stressor") {
        pred_rmse[, i] <- score(test[, rr], predictions[[i]], metrics = "RMSE")

      } else {
        pred_rmse[i] <- score(test[, rr], predictions[[i]], metrics = "RMSE")
      }
    }

  }
  if (classification) {
    rtn_list <- list(PCC = pred_rmse, predictions = predictions)
  } else {
    rtn_list <- list(RMSE = pred_rmse, predictions = predictions)
  }
  rtn_list
}
