## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(ggplot2)

## ----setup--------------------------------------------------------------------
library(stressor)

## -----------------------------------------------------------------------------
set.seed(43421)
lm_data <- data_gen_lm(500, weight_vec = rep(1, 5), y_int = 0, resp_sd = 1)
head(lm_data)

## ----echo = FALSE, warning=FALSE, fig.align='center', fig.height=5, fig.width = 5----
# Data Verification
simulation <- function(n, eps, weight_mat, label, test_size = 100, 
                       seed = 43421) {
  pred_accuracy <- matrix(0, nrow = length(eps), ncol = length(n))
  conv_mat <- matrix(0, nrow = length(eps), ncol = length(n))
  set.seed(seed)
  for (i in seq_len(ncol(pred_accuracy))) {
    for (j in seq_len(nrow(pred_accuracy))) {
      temp <- data_gen_lm(n[i] + test_size, weight_mat = weight_mat, 
                          resp_sd = eps[j])
      test <- sample(nrow(temp), test_size)
      data <- temp[-test,]
      test <- temp[test, ]
      test_y <- test[, 1]
      test <- test[, -1]
      obj <- lm(Y ~ ., data = data)
      pred <- predict(obj, test)
      pred_accuracy[j, i] <- sqrt(sum((test_y - pred)^2) / test_size)
    }
  }
  pred_accuracy <- as.data.frame(pred_accuracy)
  colnames(pred_accuracy) <- label
  sim_list <- list(pred_accuracy, conv_mat)
  return(sim_list)
}
eps <- seq(from = .1, to = 1, by = .1)
n <- c(100, 300, 500, 700, 900, 1000) # add in 5000
lab <- c("n = 100", "n = 300", "n = 500", "n = 700",
         "n = 900", "n = 1000")
weight_vec <- c(1, 3, 4, 5, 7)
lm_accuracy_res <- simulation(n, eps, weight_vec, lab)
lm_accuracy <- lm_accuracy_res[[1]]

eps2 <- rep(seq(.1, 1, .1), 6)
lm_results <- as.data.frame(eps2)
rmse <- c(lm_accuracy$`n = 100`, lm_accuracy$`n = 300`,
          lm_accuracy$`n = 500`, lm_accuracy$`n = 700`,
          lm_accuracy$`n = 900`, lm_accuracy$`n = 1000`)
lm_results$rmse <- rmse
lm_results$groups <- c(rep("n = 100", 10), rep("n = 300", 10),
                       rep("n = 500", 10), rep("n = 700", 10),
                       rep("n = 900", 10), rep("n = 1000", 10))
lm_results$groups <- factor(lm_results$groups, levels = lab)

ggplot(lm_results, aes(x = eps2, y = rmse)) +
  geom_point() +
  geom_line(aes(x = eps2, y = eps2), color = "red") +
  scale_x_continuous(name = "eps", breaks = seq(0, 1, by = .2), 
                     limits = c(0.0, 1.05)) +
  scale_y_continuous(breaks = seq(0, 1.2, by = .2), limits = c(0, 1.2)) +
  facet_wrap(~ groups, nrow = 2) +
  ggtitle("Linear Model Validation") +
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 45))

## ----eval=FALSE---------------------------------------------------------------
#  create_virtualenv()

## ----eval = FALSE-------------------------------------------------------------
#  set.seed(43421)
#  lm_data <- data_gen_lm(1000)
#  # Split the data into a 80/20 split
#  indices <- split_data_prob(lm_data, .8)
#  train <- lm_data[indices, ]
#  test <- lm_data[!indices, ]
#  # Tune the models
#  mlm_lm <- mlm_regressor(Y ~ ., lm_data, sort_v = 'RMSE', seed = 43421)

## ----echo=FALSE---------------------------------------------------------------
pred <- readRDS(file = "pred_lm.rds")
cv <- readRDS(file = "mlm_lm_cv.rds")
mlm_vignette <- list(pred_accuracy = pred, mlm_lm_cv = cv)
mlm_score <- as.data.frame(readRDS("mlm_test.rds"))
top_RMSE <- min(mlm_score$rmse)
name_RMSE <- mlm_vignette$pred_accuracy$Model[which(mlm_score$rmse == top_RMSE)]

## ----eval = FALSE-------------------------------------------------------------
#  mlm_lm$pred_accuracy

## ----echo = FALSE-------------------------------------------------------------
mlm_vignette$pred_accuracy

## ----eval = FALSE-------------------------------------------------------------
#  pred_lm <- predict(mlm_lm, test)
#  score(test$Y, pred_lm)

## ----echo = FALSE-------------------------------------------------------------
mlm_score

## -----------------------------------------------------------------------------
test_index <- split_data_prob(lm_data, .2)
test <- lm_data[test_index, ]
train <- lm_data[!test_index, ]
lm_test <- lm(Y ~ ., train)
lm_pred <- predict(lm_test, newdata = test)
lm_score <- score(test$Y, lm_pred)
lm_score

## ----eval = FALSE-------------------------------------------------------------
#  mlm_cv <- cv(mlm_lm, lm_data, n_folds = 10)

## -----------------------------------------------------------------------------
lm_cv <- cv(lm_test, lm_data, n_folds = 10)

## ----echo = FALSE-------------------------------------------------------------
mlm_cv <- mlm_vignette$mlm_lm_cv

## -----------------------------------------------------------------------------
score(lm_data$Y, mlm_cv)
score(lm_data$Y, lm_cv)

## ----eval=FALSE---------------------------------------------------------------
#  data(boston)
#  mlm_boston <- mlm_regressor(cmedv ~ ., boston)
#  mlm_boston$pred_accuracy

## ----echo=FALSE---------------------------------------------------------------
data(boston)
mlm_boston_pred_accuracy <- readRDS(file = "pred.rds")
mlm_boston_pred_accuracy

## ----eval=FALSE---------------------------------------------------------------
#  mlm_boston_cv <- cv(mlm_boston, boston, n_folds = 10)
#  mlm_boston_score <- score(boston$cmedv, mlm_boston_cv)
#  mlm_boston_score

## ----echo=FALSE---------------------------------------------------------------
cv_data <- readRDS(file = "cv.rds")
mlm_boston_score <- score(boston$cmedv, cv_data)
mlm_boston_score

## ----eval=FALSE---------------------------------------------------------------
#  mlm_boston_clust_cv <- cv(mlm_boston, boston, n_folds = 10, k_mult = 5)
#  mlm_boston_clust_score <- score(boston$cmedv, mlm_boston_clust_cv)
#  mlm_boston_clust_score

## ----echo=FALSE---------------------------------------------------------------
clus_data <- readRDS(file = "cluster.rds")
boston_clust_score <- score(boston$cmedv, clus_data)
boston_clust_score

