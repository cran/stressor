% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reg_sine.R
\name{reg_sine}
\alias{reg_sine}
\title{Sinusoidal Regression}
\usage{
reg_sine(
  formula,
  data,
  method = "BFGS",
  init_guess = rep(1, ncol(data) * 3 - 2),
  ...
)
}
\arguments{
\item{formula}{A formula object to describe the relationship.}

\item{data}{The response and predictor variables.}

\item{method}{The method that is passed to the optim function. By default, it
is the BFGS method which uses a gradient.}

\item{init_guess}{The initial parameter guesses for the optim function. By
default, it is all ones.}

\item{...}{Additional arguments passed to the optim function.}
}
\value{
A "reg_sine" object is returned which contains the results from the
 optim function that was returned.
}
\description{
A simple example of sinusoidal regression that is in the form of
 \eqn{y = asin(b(x - c))} and is the sum of of multiple of these sine
 functions with a common intercept term.
}
\examples{
 sine_data <- data_gen_sine(10)
 reg_sine(Y ~ ., sine_data)
}
