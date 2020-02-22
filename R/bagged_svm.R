#' Wrapper to add a `baggedSVM` engine to the parsnip svm_rbf model
#' specification
#'
#' @return NULL
#' @export
add_baggedSVM_engine <- function() {
  parsnip::set_model_engine("svm_rbf", "classification", "baggedSVM")
  parsnip::set_model_engine("svm_rbf", "regression", "baggedSVM")
  parsnip::set_dependency("svm_rbf", "baggedSVM", "parsnip")
  
  # model args
  parsnip::set_model_arg(
    model = "svm_rbf",
    eng = "baggedSVM",
    parsnip = "cost",
    original = "cost",
    func = list(pkg = "dials", fun = "cost"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "svm_rbf",
    eng = "baggedSVM",
    parsnip = "rbf_sigma",
    original = "rbf_sigma",
    func = list(pkg = "dials", fun = "rbf_sigma"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "svm_rbf",
    eng = "baggedSVM",
    parsnip = "margin",
    original = "margin",
    func = list(pkg = "dials", fun = "margin"),
    has_submodel = FALSE
  )
  
  # fit methods
  parsnip::set_fit(
    model = "svm_rbf",
    eng = "baggedSVM",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(fun = "baggedSVM_train"),
      defaults = list(mode = "regression")
    )
  )
  parsnip::set_fit(
    model = "svm_rbf",
    eng = "baggedSVM",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(fun = "baggedSVM_train"),
      defaults = list(mode = "classification")
    )
  )
  
  # predict methods
  parsnip::set_pred(
    model = "svm_rbf",
    eng = "baggedSVM",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "baggedSVM_predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "numeric"
      )
    )
  )
  parsnip::set_pred(
    model = "svm_rbf",
    eng = "baggedSVM",
    mode = "regression",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "baggedSVM_predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "numeric"
      )
    )
  )
  parsnip::set_pred(
    model = "svm_rbf",
    eng = "baggedSVM",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = function(x, object) object$lvl[x],
      func = c(fun = "baggedSVM_predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "class"
      )
    )
  )
  parsnip::set_pred(
    model = "svm_rbf",
    eng = "baggedSVM",
    mode = "classification",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "baggedSVM_predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "class"
      )
    )
  )
}

#' Training wrapper for baggedSVM
#'
#' @param formula A formula object.
#' @param data A data.frame of training data.
#' @param mode A character specifying "classification" or "regression".
#' @param cost The dials::cost parmaeters.
#' @param rbf_sigma The dials::rbf_sigma parameter.
#' @param margin The dials::margin parameter.
#' @param nfolds An engine argument used to set the number of folds for bagging.
#' @param ... Currently unused.
#'
#' @return A fitted parsnip model specification.
#' @export
baggedSVM_train <-
  function(formula,
           data,
           mode,
           cost = 1,
           rbf_sigma = 0.05,
           margin = 0.1,
           ...) {
    
    others <- list(...)
    nfolds <- parallel::detectCores()
    future::plan(future::multiprocess)
    
    estimator <-
      parsnip::svm_rbf(
        mode = mode,
        cost = cost,
        rbf_sigma = rbf_sigma,
        margin = margin
      ) %>%
      parsnip::set_engine("kernlab")
    
    # create partitions
    folds <- rsample::vfold_cv(data, v = nfolds)
    
    # fit partitions
    folds$models <-
      furrr::future_map(
        folds$splits,
        ~ parsnip::fit(
          object = estimator,
          formula = formula,
          data = rsample::assessment(.x)
        ), .options = furrr::future_options(packages = c("parsnip", "kernlab"))
      )
    
    future::plan(future::sequential)
    
    folds$models
}


#' Prediction wrapper for baggedSVM
#'
#' @param object A fitted model
#' @param newdata A data.frame
#' @param type A character specifying the prediction type, either "class",
#'   "numeric"
#' @param ... Currently unused
#'
#' @return Prediction results
#' @export
baggedSVM_predict <- function(object, newdata, type, ...) {
  
  others <- list(...)
  
  workers <- length(object)
  future::plan(future::multiprocess)
  
  if (type == "class") {
    # majority voting of classes  
    preds <- furrr::future_map_dfc(
      object, ~ predict(.x, newdata, type = type),
      options = furrr::future_options(packages = c("kernlab"))
    )
    preds <- map_int(apply(preds, 1, table), ~ which.max(.x))

  } else if (type == "numeric") {
    # averaging of numeric predictions
    preds <- furrr::future_map_dfc(
      object, ~ predict(.x, newdata, type = type),
      options = furrr::future_options(packages = c("kernlab"))
    )
    preds <- apply(preds, 1, mean)
  
  } else if (type %in% c("prob", "conf_int", "pred_int")) {
    # averaging of class probabilities
    preds <- furrr::future_map(
      object, ~ predict(.x, newdata, type = type),
      options = furrr::future_options(packages = c("kernlab"))
    )
    
    preds <- abind::abind(preds, along = 3)
    preds <- apply(preds, c(1, 2), mean)
    preds <- as_tibble(preds)
  }
  
  future::plan(future::sequential)
  
  preds
}
