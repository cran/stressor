% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_virtualenv.R
\name{create_virtualenv}
\alias{create_virtualenv}
\title{Create `Python` Virtual Environment}
\usage{
create_virtualenv(python = Sys.which("python"), delete_env = FALSE)
}
\arguments{
\item{python}{Defaults to your install of `python`. We prefer version 3.8.10.
This is assuming that you installed python from python.org. Currently `Anaconda`
installations of python are not implemented.}

\item{delete_env}{Boolean value to indicate if the environments need to be
deleted.}
}
\value{
A message indicating which environment is being used.
}
\description{
Allows the user to create a stressor `python` environment with `PyCaret`
  installed in the environment. This function assumes that
  you have properly installed `python`. We recommend version 3.8.10. It uses
  existing stressor environments.
}
\details{
To install `python`, it is recommended using `python` version 3.8.10 from
 \href{https://www.python.org/downloads/release/python-3810/}{python.org}.
 This is the same version recommended by `PyCaret`, as it is the most stable.
 Users have reported troubles using the `Anaconda` distribution of `python`.

 For MacOS and Linux Users note that in order to run this package, `LightGBM`
 package on python requires the install of an additional compiler `cmake` and
 the `libomp` (Open Multi-Processing interface). Troubleshoot link from the
 `LightGBM`documentation
 \href{https://lightgbm.readthedocs.io/en/latest/Installation-Guide.html}{here}.
}
\section{Troubleshoot}{

 If `python` is not being found properly, trying setting the
  `RETICULATE_PYTHON` to blank string. Also ensure that you do not have other
  `python` objects in your environment.

 Also note that on some instances that a warning message may be displayed as
   to which version of `python` is being used.
}

\examples{
\dontshow{if (python_avail()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
 create_virtualenv()
\dontshow{\}) # examplesIf}
}
