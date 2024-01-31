skip_if_no_python <- function() {
  have_python <- reticulate::py_module_available("pycaret")
  if (!have_python) {
    skip("Python is not available")
  }
}
