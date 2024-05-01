#' Create `Python` Virtual Environment
#'
#' Allows the user to create a stressor `python` environment with `PyCaret`
#'   installed in the environment. This function assumes that
#'   you have properly installed `python`. We recommend version 3.8.10. It uses
#'   existing stressor environments.
#' @param python Defaults to your install of `python`. We prefer version 3.8.10.
#'  This is assuming that you installed python from python.org. Currently `Anaconda`
#'  installations of python are not implemented.
#' @param delete_env Boolean value to indicate if the environments need to be
#'  deleted.
#' @return A message indicating which environment is being used.
#' @details
#'  To install `python`, it is recommended using `python` version 3.8.10 from
#'  \href{https://www.python.org/downloads/release/python-3810/}{python.org}.
#'  This is the same version recommended by `PyCaret`, as it is the most stable.
#'  Users have reported troubles using the `Anaconda` distribution of `python`.
#'
#'  For MacOS and Linux Users note that in order to run this package, `LightGBM`
#'  package on python requires the install of an additional compiler `cmake` and
#'  the `libomp` (Open Multi-Processing interface). Troubleshoot link from the
#'  `LightGBM`documentation
#'  \href{https://lightgbm.readthedocs.io/en/latest/Installation-Guide.html}{here}.
#' @section Troubleshoot:
#'  If `python` is not being found properly, trying setting the
#'   `RETICULATE_PYTHON` to blank string. Also ensure that you do not have other
#'   `python` objects in your environment.
#'
#'  Also note that on some instances that a warning message may be displayed as
#'    to which version of `python` is being used.
#' @examplesIf python_avail()
#'  create_virtualenv()
#' @export
create_virtualenv <- function(python = Sys.which('python'),
                              delete_env = FALSE) {
  if (Sys.getenv("RETICULATE_PYTHON") != "") {
    python = Sys.getenv("RETICULATE_PYTHON")
  }
  if (is.null(python)) {
    stop("No python found on this computer. If installed try testing your
         `Sys.which(\"python\")` argument. May need to add `python` to your
         path variable.")
  }
  current_virtualenv <- reticulate::virtualenv_list()
  stressor_env <- grep("stressor", current_virtualenv)
  active_env <- vector(mode = "logical", length = length(stressor_env))
  for (i in seq_len(length(current_virtualenv))) {
    check <- stressor_env[i]
    active_env[i] <- reticulate::virtualenv_exists(current_virtualenv[check])
  }
  if ((length(stressor_env) == 0 || sum(active_env) == 0)
           && delete_env == FALSE) {
    # Create the new environment
    time <- gsub("\\.", "", gsub(":", "", gsub(" ", "", Sys.time())))

    reticulate::virtualenv_create(paste0("stressor-env", time),
                                  python = python)
    message(paste("Created Virtual Environment:",
                  paste0("stressor-env", time)))
    message("Installing pycaret")
    reticulate::py_install(packages = c("pycaret==3.2.0", "joblib==1.3.0"),
                           envname = paste0("stressor-env", time))
    # Possible Parallelization
    # reticulate::py_install("fugue",
    #                       envname = paste0("stressor-env", time))
    message("Installed pycaret")
    reticulate::use_virtualenv(paste0("stressor-env", time))
    message(paste("Using Virtual Environment:",
                  paste0("stressor-env", time)))
  } else if (length(stressor_env) > 0 && delete_env == FALSE) {
    # Use the most recent stressor env
    if (active_env[stressor_env[length(stressor_env)]]) {
      use <- stressor_env[length(stressor_env)]
      message(paste("Using Virtual Environment:",
                  current_virtualenv[use]))
      reticulate::use_virtualenv(current_virtualenv[use], required = TRUE)
    }
  } else if (length(stressor_env) > 0 && delete_env == TRUE) {
    # Remove the stressor env
    stressor_env <- grep("stressor", current_virtualenv)
    if (length(stressor_env) > 0) {
      for (i in seq_len(length(stressor_env))) {
        rem_env <- stressor_env[i]
        if (active_env[rem_env]) {
          message(paste("Removing Virtual Environmnet:",
                        current_virtualenv[rem_env]))
          reticulate::virtualenv_remove(current_virtualenv[rem_env],
                                        confirm = FALSE)
        }
      }
    }
  } else {
    stop("ERROR:: No Virtual Environments exist!")
  }
}
