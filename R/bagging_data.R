#' Wrapper to add a bagging engine to parsnip
#'
#' @return NULL
#' @export
make_bagging <- function() {
  
  parsnip::set_new_model("bagging")
  
  parsnip::set_model_mode(model = "bagging", mode = "classification")
  parsnip::set_model_mode(model = "bagging", mode = "regression")
  
  parsnip::set_model_engine(model = "bagging", mode = "classification", eng = "parsnipExtra")
  parsnip::set_model_engine(model = "bagging", mode = "regression", eng = "parsnipExtra")
  parsnip::set_dependency("bagging", "parsnipExtra", "parsnipExtra")
  
  # model args
  parsnip::set_model_arg(
    model = "bagging",
    eng = "parsnipExtra",
    parsnip = "trees",
    original = "trees",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "bagging",
    eng = "parsnipExtra",
    parsnip = "mtry",
    original = "mtry",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "bagging",
    eng = "parsnipExtra",
    parsnip = "sample_prop",
    original = "sample_prop",
    func = list(pkg = "dials", fun = "sample_prop"),
    has_submodel = FALSE
  )
  
  # fit methods
  parsnip::set_fit(
    model = "bagging",
    eng = "parsnipExtra",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(fun = "bagging_train"),
      defaults = list(
        mode = "regression", 
        .opts = furrr::future_options()
      )
    )
  )
  parsnip::set_fit(
    model = "bagging",
    eng = "parsnipExtra",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(fun = "bagging_train"),
      defaults = list(
        mode = "classification",
        .opts = furrr::future_options()
      )
    )
  )
  
  # predict methods
  parsnip::set_pred(
    model = "bagging",
    eng = "parsnipExtra",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "bagging_predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "numeric"
      )
    )
  )
  parsnip::set_pred(
    model = "bagging",
    eng = "parsnipExtra",
    mode = "regression",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "bagging_predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "numeric"
      )
    )
  )
  parsnip::set_pred(
    model = "bagging",
    eng = "parsnipExtra",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = function(x, object) object$lvl[x],
      func = c(fun = "bagging_predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "class"
      )
    )
  )
  parsnip::set_pred(
    model = "bagging",
    eng = "parsnipExtra",
    mode = "classification",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "bagging_predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "class"
      )
    )
  )
}

#' Training method for bagging
#'
#' @param formula A formula object.
#' @param data A data.frame of training data.
#' @param mode A character specifying "classification" or "regression".
#' @param mtry The dials::mtry parameters.
#' @param trees The dials::trees parameter.
#' @param sample_prop The dials::sample_prop parameter.
#' @param model The base model specification to use in the bagging ensemble.
#' @param .opt Additional future_options to use for multiprocessing.
#' @param ... Currently unused.
#'
#' @return A fitted parsnip model specification.
#' @export
bagging_train <-
  function(formula,
           data,
           mode,
           mtry = NULL,
           trees = 10,
           sample_prop = 0.67,
           model = NULL,
           .opts = furrr::future_options(),
           ...) {
    
    others <- list(...)

    # create partitions
    resamples <- rsample::mc_cv(data, prop = sample_prop, times = trees)
    
    # fit partitions
    resamples$models <-
      furrr::future_map(
        resamples$splits,
        ~ parsnip::fit(
          object = model,
          formula = formula,
          data = rsample::assessment(.x)
        ), 
        .options = .opts
      )
    
    
    fitted_models <- resamples$models
    names(fitted_models) <- paste("bag", seq_len(length(fitted_models)), sep = "_")
    fitted_models$.opts <- .opts
    
    fitted_models
  }


#' Prediction method for bagging
#'
#' @param object A fitted model
#' @param newdata A data.frame
#' @param type A character specifying the prediction type, either "class",
#'   "numeric"
#' @param ... Currently unused
#'
#' @return Prediction results
#' @export
bagging_predict <- function(object, newdata, type, ...) {
  
  others <- list(...)
  
  .opts = object$.opts
  object <- object[names(object) != ".opts"]
  
  if (type == "class") {
    # majority voting of classes  
    preds <- furrr::future_map_dfc(
      object, ~ predict(.x, newdata, type = type),
      .options = .opts
    )
    preds <- furrr::future_map_int(apply(preds, 1, table), ~ which.max(.x))
    
  } else if (type == "numeric") {
    # averaging of numeric predictions
    preds <- furrr::future_map_dfc(
      object, ~ predict(.x, newdata, type = type),
      .options = .opts
    )
    preds <- apply(preds, 1, mean)
    
  } else if (type %in% c("prob", "conf_int", "pred_int")) {
    # averaging of class probabilities
    preds <- furrr::future_map(
      object, ~ predict(.x, newdata, type = type),
      .options = .opts
    )
    
    preds <- abind::abind(preds, along = 3)
    preds <- apply(preds, c(1, 2), mean)
    preds <- as_tibble(preds)
  }
  
  preds
}