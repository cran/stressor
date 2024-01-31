confusion_matrix <- function(predicted, observed) {
  tab <- table(observed, predicted)
  if (ncol(tab) < 2 || nrow(tab) < 2) {
    if (ncol(tab) < 2) {
      temp <- matrix(0, nrow = 2, ncol = 2)
      if (colnames(tab) == "1") {
        temp[, 1] <- tab[, 1]
      } else {
        temp[, 2] <- tab[, 1]
      }
    } else {
      temp <- matrix(0, nrow = 2, ncol = 2)
      if (row.names(tab) == "1") {
        temp[1, ] <- tab[1, ]
      } else {
        temp[2, ] <- tab[1, ]
      }
    }
    tab <- as.table(temp)
  }
  tab
}

auc <- function(observed, predicted) {
  # Non-infinite values
  ind <- is.finite(observed) & is.finite(predicted)
  obs_u <- observed[ind]
  pred_u <- predicted[ind]
  ones <- sum(obs_u)
  total <- length(obs_u)
  auc <- (mean(rank(pred_u)[obs_u == 1]) - (ones + 1)/2)/(total - ones)
  auc
}

#' @title Kappa function
#' @description
#' A function to calculate the Kappa of binary classification.
#' @param confusion_matrix A matrix or table that is the confusion matrix.
#' @return A numeric value representing the kappa value.
kappa_class <- function(confusion_matrix) {
  tot <- sum(confusion_matrix)
  per_cor <- (sum(confusion_matrix[1, ]) / tot) *
    (sum(confusion_matrix[, 1]) / tot)
  per_inc <- (sum(confusion_matrix[2, ]) / tot) *
    (sum(confusion_matrix[, 2]) / tot)
  per_err <- per_cor + per_inc
  per_opp <- (sum(diag(confusion_matrix))) / tot
  (per_opp - per_err) / (1 - per_err)
}

accuracy <- function(confusion_matrix) {
  sum(diag(confusion_matrix)) / sum(confusion_matrix)
}

recall <- function(confusion_matrix) {
  confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
}

precision <- function(confusion_matrix) {
  confusion_matrix[1, 1] / sum(confusion_matrix[, 1])
}

f1 <- function(confusion_matrix) {
  rec <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
  pre <- confusion_matrix[1, 1] / sum(confusion_matrix[, 1])
  (2 * rec * pre) / (rec + pre)
}

mcc <- function(confusion_matrix) {
  tot <- sum(confusion_matrix)
  pos_FN <- sum(confusion_matrix[1, ]) / tot
  pos_FP <- sum(confusion_matrix[, 1]) / tot
  denominator <- sqrt((pos_FN * pos_FP * (1 - pos_FN) * (1 - pos_FP)))
  mcc <- ((confusion_matrix[1, 1] / tot) - (pos_FN * pos_FP)) / denominator
}

#' @title Score Function for Binary Classification
#'
#' @description This function takes the observed and predicted values and computes metrics
#'  that are found in `PyCaret` such as: `Accuracy`, `AUC`, `Recall`, `Prec.`,
#'  `F1`, `MCC`, and `Kappa`.
#' @param observed A vector of the observed results.
#' @param predicted  A data.frame or vector object that is the same number of
#'   rows or length as the length of observed values.
#' @param metrics A character vector of the metrics to be fitted. This is
#'   defaulted to be the metrics from `PyCaret`.
#' @return A vector or data.frame of the methods and metrics.
score_classification <- function(observed, predicted,
                                 metrics = c('Accuracy', 'AUC', 'Recall',
                                             'Prec.', 'F1', 'MCC',
                                             'Kappa')) {
  metric_const <- c('Accuracy', 'AUC', 'Recall',
                    'Prec.', 'F1', 'MCC',
                    'Kappa')
  binary <- length(unique(observed)) == 2

  if (is.null(metrics)) {
    stop("A metric needs to be defined.")
  }

  if (!all(is.element(metrics, metric_const))) {
    stop("Metric is not currently implemented.")
  }
  if (is.null(ncol(predicted))) {
    confusion_mat <- confusion_matrix(predicted, observed)
    results <- vector(length = length(metrics))
    results <- sapply(metrics, function(x) {
      if (x == "AUC") {
        res <- auc(predicted, observed)
      } else if (x == "Accuracy") {
        res <- accuracy(confusion_mat)
      } else if (x == "Kappa") {
        res <- kappa_class(confusion_mat)
      } else if (x == "MCC") {
        res <- mcc(confusion_mat)
      } else if (x == "Recall") {
        res <- recall(confusion_mat)
      } else if (x == "Prec.") {
        res <- precision(confusion_mat)
      } else if (x == "F1") {
        res <- f1(confusion_mat)
      } else {
        stop("Method not implemented.")
      }
      res
    })
    names(results) <- metrics
  } else {
    confusion_mat <- apply(predicted, 2, confusion_matrix, observed,
                           simplify = FALSE)
    results <- matrix(0, nrow = ncol(predicted), ncol = length(metrics))
    for (i in seq_len(length(metrics))) {
      met <- metrics[i]
      if (met == "auc") {
        results[, i] <- apply(predicted, 2, function(x) {auc(observed, x)})
      } else {
        results[, i] <- sapply(confusion_mat, function(x) {
          if (met == "Accuracy") {
            res <- accuracy(x)
          } else if (met == "Kappa") {
            res <- kappa_class(x)
          } else if (met == "MCC") {
            res <- mcc(x)
          } else if (met == "Recall") {
            res <- recall(x)
          } else if (met == "Prec.") {
            res <- precision(x)
          } else if (met == "F1") {
            res <- f1(x)
          } else {
            stop("Method not implemented.")
          }
        })
      }
    }
    colnames(results) <- metrics
    row.names(results) <- colnames(predicted)
  }
  results
}
