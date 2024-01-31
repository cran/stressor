#' Root Mean Squarred Error (RMSE)
#'
#' A function to calculate the RMSE.
#' @inheritParams score
rmse <- function(predicted, observed) {
  sqrt(mean((observed - predicted)^2))
}

mae <- function(predicted, observed) {
  mean(abs(observed - predicted))
}
mse <- function(predicted, observed) {
  mean((observed - predicted)^2)
}
r2 <- function(predicted, observed) {
  1 - (sum((observed - predicted)^2) / (sum((observed - mean(observed))^2)))
}
rmsle <- function(predicted, observed) {
  make_pos <- max(abs(c(predicted, observed)))
  sqrt(mean((log((1 + make_pos + predicted) / (1 + make_pos + observed)))^2,
            na.rm = TRUE))
}

mape <- function(predicted, observed) {
  mean(abs(observed - predicted) / abs(observed))
}


#' @title Score Function for Regression
#' @description This function takes the observed and predicted values and
#'  computes metrics that are found in `PyCaret` such as: `RMSE`, `MAE`, `MSE`,
#'  `R2`, `RMSLE`, and `MAPE`.
#' @inheritParams score_classification
#' @inherit score_classification return
score_regression <- function(observed, predicted,
                             metrics = c('RMSE', 'MAE', 'MSE', 'R2', 'RMSLE',
                                         'MAPE')) {
  metric_const <- c('RMSE', 'MAE', 'MSE', 'R2', 'RMSLE', 'MAPE')
  if (is.null(metrics)) {
    stop("A metric needs to be defined.")
  }

  if (!all(is.element(metrics, metric_const))) {
    stop("Metric is not currently implemented.")
  }
  if (is.null(ncol(predicted))) {
    results <- vector(length = length(metrics))
    results <- sapply(metrics, function(x) {
      if (x == "RMSE") {
        res <- rmse(predicted, observed)
      } else if (x == "MAE") {
        res <- mae(predicted, observed)
      } else if (x == "MSE") {
        res <- mse(predicted, observed)
      } else if (x == "R2") {
        res <- r2(predicted, observed)
      } else if (x == "RMSLE") {
        res <- rmsle(predicted, observed)
      } else if (x == "MAPE") {
        res <- mape(predicted, observed)
      } else {
        stop("Metric is not implemented.")
      }
      res
    })
    names(results) <- metrics
  } else {
    results <- matrix(0, nrow = ncol(predicted), ncol = length(metrics))
    for (i in seq_len(length(metrics))) {
      met <- metrics[i]
      results[, i] <- apply(predicted, 2, function(x) {
        if (met == "RMSE") {
          res <- rmse(x, observed)
        } else if (met == "MAE") {
          res <- mae(x, observed)
        } else if (met == "MSE") {
          res <- mse(x, observed)
        } else if (met == "R2") {
          res <- r2(x, observed)
        } else if (met == "RMSLE") {
          res <- rmsle(x, observed)
        } else if (met == "MAPE") {
          res <- mape(x, observed)
        } else {
          stop("Metric is not implemented.")
        }
      })
    }
    row.names(results) <- colnames(predicted)
    colnames(results) <- metrics
  }
  results
}

