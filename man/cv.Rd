% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cv.R, R/cv_lm.R, R/cv_mlm_stressor.R,
%   R/cv_reg_asym.R, R/cv_reg_sine.R
\name{cv}
\alias{cv}
\alias{cv.lm}
\alias{cv.mlm_stressor}
\alias{cv.reg_asym}
\alias{cv.reg_sine}
\title{Cross Validation}
\usage{
cv(
  object,
  data,
  n_folds = 10,
  k_mult = NULL,
  repl = FALSE,
  grouping_formula = NULL
)

\method{cv}{lm}(
  object,
  data,
  n_folds = 10,
  k_mult = NULL,
  repl = FALSE,
  grouping_formula = NULL
)

\method{cv}{mlm_stressor}(
  object,
  data,
  n_folds = 10,
  k_mult = NULL,
  repl = FALSE,
  grouping_formula = NULL
)

\method{cv}{reg_asym}(
  object,
  data,
  n_folds = 10,
  k_mult = NULL,
  repl = FALSE,
  grouping_formula = NULL
)

\method{cv}{reg_sine}(
  object,
  data,
  n_folds = 10,
  k_mult = NULL,
  repl = FALSE,
  grouping_formula = NULL
)
}
\arguments{
\item{object}{One of the four objects that is accepted: mlm_stressor,
reg_sine, reg_asym, or lm.}

\item{data}{A data.frame object that contains all the entries to be
cross-validated on.}

\item{n_folds}{An integer value for the number of folds defaulted to 10. If
NULL, it will run LOO cross-validation.}

\item{k_mult}{Used to specify if k-means clustering is to be used, defaulted
to NULL.}

\item{repl}{A Boolean value defaulted to `FALSE`, change to `TRUE` when
replicates need to be included in the same group.}

\item{grouping_formula}{A formula object that specifies how the groups will
be gathered.}
}
\value{
If the object is of class mlm_stressor, then a data.frame will be
  returned. Otherwise, a vector of the predictions will be returned.
}
\description{
This is the core of cross-validation- both standard and using k-mean groups.
 This method is called by other cv methods of classes.
}
\section{Methods (by class)}{
\itemize{
\item \code{cv(lm)}: Cross-Validation for lm

\item \code{cv(mlm_stressor)}: Cross-Validation for mlm_stressor

\item \code{cv(reg_asym)}: Cross-Validation for reg_asym

\item \code{cv(reg_sine)}: Cross-Validation for reg_sine

}}
\examples{
 # lm example
 lm_test <- data_gen_lm(20)
 lm <- lm(Y ~ ., lm_test)
 cv(lm, lm_test, n_folds = 2)

\dontshow{if (python_avail()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
 lm_test <- data_gen_lm(20)
 create_virtualenv()
 mlm_lm <- mlm_regressor(Y ~ ., lm_test)
 cv(mlm_lm, lm_test, n_folds = 2)
\dontshow{\}) # examplesIf}
 # Asymptotic example
 asym_data <- data_gen_asym(10)
 asym_fit <- reg_asym(Y ~ ., asym_data)
 cv(asym_fit, asym_data, n_folds = 2)

 # Sine example
 sine_data <- data_gen_sine(10)
 sine_fit <- reg_sine(Y ~ ., sine_data)
 cv(sine_fit, sine_data, n_folds = 2)
}
