#' Wrapper to add the `rknn` engine to the parsnip `nearest_neighbor` model
#' specification
#'
#' @return NULL
#' @export
add_rknn_engine <- function() {
  parsnip::set_model_engine("nearest_neighbor", "classification", "rknn")
  parsnip::set_model_engine("nearest_neighbor", "regression", "rknn")
  parsnip::set_dependency("nearest_neighbor", "rknn", "rknn")
  
  parsnip::set_model_arg(
    model = "nearest_neighbor",
    eng = "rknn",
    parsnip = "neighbors",
    original = "k",
    func = list(pkg = "dials", fun = "neighbors"),
    has_submodel = FALSE
  )
  parsnip::set_fit(
    model = "nearest_neighbor",
    eng = "rknn",
    mode = "regression",
    value = list(
      interface = "matrix",
      protect = c("x", "y"),
      func = c(fun = "rknn_train"),
      defaults = list()
    )
  )
  parsnip::set_fit(
    model = "nearest_neighbor",
    eng = "rknn",
    mode = "classification",
    value = list(
      interface = "matrix",
      protect = c("x", "y"),
      func = c(fun = "rknn_train"),
      defaults = list()
    )
  )
  parsnip::set_encoding(
    model = "nearest_neighbor",
    eng = "rknn",
    mode = "classification",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )
  parsnip::set_encoding(
    model = "nearest_neighbor",
    eng = "rknn",
    mode = "regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )
  parsnip::set_pred(
    model = "nearest_neighbor",
    eng = "rknn",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "rknn_pred"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )
  )
  parsnip::set_pred(
    model = "nearest_neighbor",
    eng = "rknn",
    mode = "regression",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "rknn_pred"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )
  )
  parsnip::set_pred(
    model = "nearest_neighbor",
    eng = "rknn",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "rknn_pred"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )
  )
  parsnip::set_pred(
    model = "nearest_neighbor",
    eng = "rknn",
    mode = "classification",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "rknn_pred"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data))
    )
  )
}

#' Nearest neighbors using rknn
#'
#' `rknn_train` is a wrapper for `rknn` fast nearest neighbor models
#'
#' @param x a data frame or matrix of predictors.
#' @param y a vector (factor or numeric) or matrix (numeric) of outcome data.
#' @param k a vector (integer) of the number of neighbours to consider.
#' @param r an integer specifying the number of KNN models containing random
#'   feature subsets. Default = 500.
#' @param mtry an integer with the number of features to randomly select per KNN
#'   model. Default is `trunc(sqrt(ncol(x)))`.
#' @param seed an integer, default is NULL.
#' @param ... additional arguments to pass to FNN, currently unused.
#'
#' @return list containing the FNN call
#' @export
rknn_train <- function(x, y = NULL, k = 1, r = 500, mtry = trunc(sqrt(ncol(x))), 
                       seed = NULL, ...) {
  if (is.numeric(y)) {
    main_args <- list(
      data = rlang::enquo(x),
      y = rlang::enquo(y),
      k = k,
      r = r,
      mtry = mtry,
      newdata = x,
      seed = seed
    )
    
    call <- parsnip:::make_call(fun = "rknnReg", ns = "rknn", main_args)
    
  } else {
    main_args <- list(
      data = rlang::enquo(x),
      y = rlang::enquo(y),
      k = k,
      r = r,
      mtry = mtry,
      newdata = x,
      seed = seed
    )
    
    call <- parsnip:::make_call(fun = "rknn", ns = "rknn", main_args)
  }
  
  list(call = call)
}


#' Nearest neighbors prediction using rknn
#'
#' `rknn_pred` is a wrapper for `rknn` fast nearest neighbor models
#'
#' @param object parsnip model spec.
#' @param newdata data.frame or matrix of training data.
#' @param ... additional arguments to pass to FNN, currently unused.
#'
#' @return data.frame containing the predicted results.
#' @export
rknn_pred <- function(object, newdata, ...) {
  # modify the call for prediction
  object$call$newdata <- newdata
  res <- rlang::eval_tidy(object$call)
  res[["pred"]]
}
