#' Wrapper to add the `FNN` engine to the parsnip `nearest_neighbor` model
#' specification
#'
#' @return NULL
#' @export
#' @importFrom parsnip set_model_engine set_dependency set_model_arg set_fit
#' set_pred
add_fnn_engine <- function() {

  set_model_engine("nearest_neighbor", "classification", "FNN")
  set_model_engine("nearest_neighbor", "regression", "FNN")
  set_dependency("nearest_neighbor", "FNN", "FNN")
  
  set_model_arg(
    model = "nearest_neighbor",
    eng = "FNN",
    parsnip = "neighbors",
    original = "k",
    func = list(pkg = "dials", fun = "neighbors"),
    has_submodel = FALSE
  )
  
  set_fit(
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
  
  set_fit(
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
  
  set_pred(
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
  
  set_pred(
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
  
  set_pred(
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
  
  set_pred(
    model = "nearest_neighbor",
    eng = "FNN",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = function(result, object) as_tibble(result),
      func = c(fun = "fnn_pred"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          prob = TRUE
        )
    )
  )
  
  set_pred(
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
#' @param x a data frame or matrix of predictors
#' @param y a vector (factor or numeric) or matrix (numeric) of outcome data.
#' @param k a vector (integer) of the number of neighbours to consider.
#' @param algorithm character, one of c("kd_tree", "cover_tree", "brute"), default = "kd_tree"
#' @param ... additional arguments to pass to FNN, currently unused
#'
#' @return list containing the FNN call
#' @export
#' @importFrom rlang enquo call2 eval_tidy
fnn_train <- function(x, y = NULL, k = 1, algorithm = "kd_tree", ...) {
  
  # regression
  if (is.numeric(y)) {
    fun <- "knn.reg"
    main_args <- list(
      train = enquo(x),
      y = enquo(y),
      k = k,
      algorithm = algorithm)
    call <- parsnip:::make_call(fun = fun, ns = "FNN", main_args)
    eval_tidy(call, env = current_env())
    
    # for classification return unevaluated call because FNN:knn
    # trains and predicts in same call
  } else {
    fun <- "knn"
    main_args <- list(
      train = enquo(x),
      cl = enquo(y),
      k = k,
      algorithm = algorithm)
    call <- parsnip:::make_call(fun = fun, ns = "FNN", main_args)
    list(call = call)
  }
}


#' Nearest neighbors prediction using FNN
#'
#' `fnn_pred` is a wrapper for `FNN` fast nearest neighbor models
#'
#' @param object parsnip model spec
#' @param newdata data.frame or matrix of training data
#' @param prob logical return predicted probability of the winning class, default = FALSE
#' @param ... additional arguments to pass to FNN, currently unused
#'
#' @return data.frame containing the predicted rsults
#' @export
#' @importFrom rlang eval_tidy
fnn_pred <- function(object, newdata, prob = FALSE, ...) {
  
  # modify the call for prediction
  object$call$test <- newdata
  
  # regression result
  if ("y" %in% names(object$call)) {
    res <- eval_tidy(object$call)
    res <- res$pred
    
    # classification result
  } else {
    object$call$prob <- prob
    lvl <- levels(eval_tidy(object$call$cl))
    res <- eval_tidy(object$call)
    
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