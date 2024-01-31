from pycaret.classification import *
def refit(x_train, y_train, x_test, model):
  refit_mlm_new_model = model.fit(x_train, y_train)
  predictions = refit_mlm_new_model.predict(x_test)
  return predictions
