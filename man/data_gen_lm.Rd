% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_gen_lm.R
\name{data_gen_lm}
\alias{data_gen_lm}
\title{Data Generation for Linear Regression}
\usage{
data_gen_lm(n, weight_vec = rep(1, 5), y_int = 0, resp_sd = 1, ...)
}
\arguments{
\item{n}{The number of observations for each parameter.}

\item{weight_vec}{The parameter coefficients where each entry represents the
coefficients for the additive linear model.}

\item{y_int}{The y-intercept term of the additive model.}

\item{resp_sd}{The standard deviation of the epsilon term to be added for
noise.}

\item{...}{Additional arguments that are not currently implemented.}
}
\value{
A data.frame object with the n rows and the response variable with
 the number of parameters being equal to the number of columns from the
 weight matrix.
}
\description{
Creates a synthetic data set for an additive linear model. See details for
 clarification.
}
\details{
Observations are generated from the following model:
  \deqn{y = \sum_{i = 1}^n \alpha_i\cdot x_i + y_{int}}
  Where `n` is the number of parameters to be used and the \eqn{\alpha_i}'s
  are the weights associated with each \eqn{x_i}. With the \eqn{y_{int}}
  being where it crosses the y-axis.
}
\examples{
 # Generates 10 observations
 lm_data <- data_gen_lm(10)
 lm_data
}
