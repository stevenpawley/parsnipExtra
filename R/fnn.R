#' Wrapper to add the `FNN` engine to the parsnip `nearest_neighbor` model
#' specification
#'
#' @return NULL
#' @export
add_fnn_engine <- function() {
  
  parsnip::set_model_engine("nearest_neighbor", "classification", "FNN")
  parsnip::set_model_engine("nearest_neighbor", "regression", "FNN")
  parsnip::set_dependency("nearest_neighbor", "FNN", "FNN")
  
  parsnip::set_model_arg(
    model = "nearest_neighbor",
    eng = "FNN",
    parsnip = "neighbors",
    original = "k",
    func = list(pkg = "dials", fun = "neighbors"),
    has_submodel = FALSE
  )
  parsnip::set_fit(
    model = "nearest_neighbor",
    eng = "FNN",
    mode = "regression",
    value = list(
      interface = "matrix",
      protect = c("x", "y"),
      func = c(fun = "fnn_train"),
      defaults = list()
    )
  )
  parsnip::set_fit(
    model = "nearest_neighbor",
    eng = "FNN",
    mode = "classification",
    value = list(
      interface = "matrix",
      protect = c("x", "y"),
      func = c(fun = "fnn_train"),
      defaults = list()
    )
  )
  parsnip::set_encoding(
    model = "nearest_neighbor",
    eng = "FNN",
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
    eng = "FNN",
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
    eng = "FNN",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "fnn_pred"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )
  )
  parsnip::set_pred(
    model = "nearest_neighbor",
    eng = "FNN",
    mode = "regression",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "fnn_pred"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )
  )
  parsnip::set_pred(
    model = "nearest_neighbor",
    eng = "FNN",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "fnn_pred"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )
  )
  parsnip::set_pred(
    model = "nearest_neighbor",
    eng = "FNN",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = function(result, object) tibble::as_tibble(result),
      func = c(fun = "fnn_pred"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          prob = TRUE
        )
    )
  )
  parsnip::set_pred(
    model = "nearest_neighbor",
    eng = "FNN",
    mode = "classification",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "fnn_pred"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data))
    )
  )
}

#' Nearest neighbors using FNN
#'
#' `fnn_train` is a wrapper for `FNN` fast nearest neighbor models
#'
#' @param x a data frame or matrix of predictors.
#' @param y a vector (factor or numeric) or matrix (numeric) of outcome data.
#' @param k a vector (integer) of the number of neighbours to consider.
#' @param algorithm character, one of c("kd_tree", "cover_tree", "brute"),
#'   default = "kd_tree"
#' @param ... additional arguments to pass to FNN, currently unused.
#'
#' @return list containing the FNN call
#' @export
fnn_train <- function(x, y = NULL, k = 1, algorithm = "kd_tree", ...) {
  
  if (is.numeric(y)) {
    
    main_args <- list(
      train = rlang::enquo(x),
      y = rlang::enquo(y),
      k = k,
      algorithm = algorithm
    )
    
    call <- parsnip:::make_call(fun = "knn.reg", ns = "FNN", main_args)
    rlang::eval_tidy(call, env = rlang::current_env())
  
  } else {
    main_args <- list(
      train = rlang::enquo(x),
      cl = rlang::enquo(y),
      k = k,
      algorithm = algorithm
    )
    
    call <- parsnip:::make_call(fun = "knn", ns = "FNN", main_args)
    
    list(call = call)
  }
}


#' Nearest neighbors prediction using FNN
#'
#' `fnn_pred` is a wrapper for `FNN` fast nearest neighbor models
#'
#' @param object parsnip model spec.
#' @param newdata data.frame or matrix of training data.
#' @param prob logical return predicted probability of the winning class,
#'   default = FALSE.
#' @param ... additional arguments to pass to FNN, currently unused.
#'
#' @return data.frame containing the predicted results.
#' @export
fnn_pred <- function(object, newdata, prob = FALSE, ...) {
  
  # modify the call for prediction
  object$call$test <- newdata
  
  # regression result
  if ("y" %in% names(object$call)) {
    res <- rlang::eval_tidy(object$call)
    res <- res$pred
    
    # classification result
  } else {
    object$call$prob <- prob
    lvl <- levels(rlang::eval_tidy(object$call$cl))
    res <- rlang::eval_tidy(object$call)
    
    # probability for winning class
    if (prob == FALSE) {
      attributes(res) <- NULL
      res <- factor(lvl[res], levels = lvl)
    } else {
      res <- attr(res, "prob")
    }
  }
  
  res
}
