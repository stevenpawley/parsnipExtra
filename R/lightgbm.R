add_lightgbm_engine <- function() {
  parsnip::set_model_engine("boost_tree", "classification", "lightgbm")
  parsnip::set_model_engine("boost_tree", "regression", "lightgbm")
  parsnip::set_dependency("boost_tree", "lightgbm", "lightgbm")
  
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "tree_depth",
    original = "max_depth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "trees",
    original = "nrounds",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "learn_rate",
    original = "learning_rate",
    func = list(pkg = "dials", fun = "learn_rate"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "mtry",
    original = "feature_fraction",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "min_n",
    original = "min_data_in_leaf",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "loss_reduction",
    original = "min_gain_to_split",
    func = list(pkg = "dials", fun = "loss_reduction"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "sample_size",
    original = "bagging_fraction",
    func = list(pkg = "dials", fun = "sample_size"),
    has_submodel = FALSE
  )
  parsnip::set_fit(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "regression",
    value = list(
      interface = "matrix",
      protect = c("x", "y"),
      func = c(fun = "lgbm_train"),
      defaults = list(num_threads = 0, verbose = -1)
    )
  )
  parsnip::set_encoding(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "regression",
    options = list(predictor_indicators = TRUE)
  )
  parsnip::set_pred(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = function(x, object) if (inherits(x, "lgb.Dataset")) as.matrix(x) else x,
      post = NULL,
      func = c(fun = "predict"),
      args = list(object = quote(object$fit), data = quote(new_data))
    )
  )
  parsnip::set_pred(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "regression",
    type = "raw",
    value = list(
      pre = function(x, object) if (inherits(x, "lgb.Dataset")) as.matrix(x) else x,
      post = NULL,
      func = c(fun = "predict"),
      args = list(object = quote(object$fit), data = quote(new_data))
    )
  )
  parsnip::set_fit(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "classification",
    value = list(
      interface = "matrix",
      protect = c("x", "y"),
      func = c(fun = "lgbm_train"),
      defaults = list(num_threads = 0, verbose = -1)
    )
  )
  parsnip::set_encoding(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "classification",
    options = list(predictor_indicators = TRUE)
  )
  parsnip::set_pred(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "classification",
    type = "class",
    value = list(
      pre = function(x, object) if (inherits(x, "lgb.Dataset")) as.matrix(x) else x,
      post = function(x, object) {
        if (is.vector(x)) {
          x <- ifelse(x >= 0.5, object$lvl[2], object$lvl[1])
        } else {
          x <- object$lvl[apply(x, 1, which.max)]
        }
        x
      },
      func = c(fun = "predict"),
      args = list(object = quote(object$fit), data = quote(new_data))
    )
  )
  parsnip::set_pred(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "classification",
    type = "prob",
    value = list(
      pre = function(x, object) if (inherits(x, "lgb.Dataset")) as.matrix(x) else x,
      post = function(x, object) {
        if (is.vector(x)) {
          x <- tibble(v1 = 1 - x, v2 = x)
        } else {
          x <- as_tibble(x)
        }
        colnames(x) <- object$lvl
        x
      },
      func = c(fun = "predict"),
      args = list(object = quote(object$fit), data = quote(new_data))
    )
  )
  parsnip::set_pred(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "classification",
    type = "raw",
    value = list(
      pre = function(x, object) if (inherits(x, "lgb.Dataset")) as.matrix(x) else x,
      post = NULL,
      func = c(fun = "predict"),
      args = list(object = quote(object$fit), data = quote(new_data))
    )
  )
}

#' Boosted trees via lightgbm
#'
#' `lgbm_train` is a wrapper for `xgboost` tree-based models
#'  where all of the model arguments are in the main function.
#'
#' @param x A data frame or matrix of predictors
#' @param y A vector (factor or numeric) or matrix (numeric) of outcome data.
#' @param max_depth An integer for the maximum depth of the tree.
#' @param nrounds An integer for the number of boosting iterations.
#' @param learning_rate A numeric value between zero and one to control the learning rate.
#' @param feature_fraction Subsampling proportion of columns.
#' @param min_data_in_leaf A numeric value for the minimum sum of instance
#'  weights needed in a child to continue to split.
#' @param min_gain_to_split A number for the minimum loss reduction required to make a
#'  further partition on a leaf node of the tree
#' @param bagging_fraction Subsampling proportion of rows.
#' @param validation A positive number. If on `[0, 1)` the value, `validation`
#' is a random proportion of data in `x` and `y` that are used for performance
#' assessment and potential early stopping. If 1 or greater, it is the _number_
#' of training set samples use for these purposes.
#' @param early_stop An integer or `NULL`. If not `NULL`, it is the number of
#' training iterations without improvement before stopping. If `validation` is
#' used, performance is base on the validation set; otherwise the training set
#' is used.
#' @param ... Other options to pass to `lgbm.train`.
#' @return A fitted `lightgbm` object.
#' @keywords internal
#' @export
lgbm_train <-
  function(x,
           y,
           max_depth = 6,
           nrounds = 15,
           learning_rate  = 0.3,
           feature_fraction = 1,
           min_data_in_leaf = 20,
           bagging_fraction = 1,
           min_gain_to_split = 0,
           validation = 0,
           early_stop = NULL,
           ...) {
    others <- list(...)
    
    if (!is.numeric(validation) || validation < 0 || validation >= 1) {
      rlang::abort("`validation` should be on [0, 1).")
    }
    if (!is.null(early_stop)) {
      if (early_stop <= 1) {
        rlang::abort(paste0("`early_stop` should be on [2, ",  nrounds, ")."))
      } else if (early_stop >= nrounds) {
        early_stop <- nrounds - 1
        rlang::warn(paste0("`early_stop` was reduced to ", early_stop, "."))
      }
    }
    
    if (!"num_leaves" %in% names(others)) {
      num_leaves <- 2 ^ max_depth
    }
    
    if (is.numeric(y)) {
      objective <- "regression"
      
    } else {
      lvl <- levels(y)
      y <- as.numeric(y) - 1
      objective <- ifelse(length(lvl) == 2, "cross_entropy", "multiclass")
    }
    
    if (is.data.frame(x))
      x <- as.matrix(x)
    
    n <- nrow(x)
    p <- ncol(x)
    
    if (!inherits(x, "lgb.Dataset")) {
      
      if (validation > 0) {
        idx <- sample(1:n, size = floor(n * validation) + 1)
        wlist <-
          list(lightgbm::lgb.Dataset(x[-idx, ], label = y[-idx]))
        x <- lightgbm::lgb.Dataset(x[idx, ], label = y[idx])
      }
      
      x <- lightgbm::lgb.Dataset(x, label = y)
      wlist <- list(training = x)
      
    } else {
      lightgbm::setinfo(x, "label", y)
    }
    
    if (bagging_fraction > 1) {
      bagging_fraction <- bagging_fraction / n
    }
    if (bagging_fraction > 1) {
      bagging_fraction <- 1
    }
    if (bagging_fraction < 1) {
      bagging_fraction <- 1
    }
    
    if (feature_fraction > 1) {
      feature_fraction <- feature_fraction / p
    }
    if (feature_fraction > 1) {
      feature_fraction <- 1
    }
    
    arg_list <- list(
      learning_rate = learning_rate,
      max_depth = max_depth,
      num_leaves = num_leaves,
      feature_fraction = feature_fraction,
      min_data_in_leaf = min_data_in_leaf,
      bagging_fraction = bagging_fraction,
      min_gain_to_split = min_gain_to_split
    )
    
    main_args <- list(
      data = quote(x),
      valids = quote(wlist),
      params = arg_list,
      nrounds = nrounds,
      obj = objective,
      early_stopping_rounds = early_stop
    )
    
    call <- parsnip:::make_call(fun = "lightgbm", ns = "lightgbm", main_args)
    
    # override or add some other args
    others <- list(...)
    others <-
      others[!(names(others) %in% c("data", "nrounds", names(arg_list)))]
    
    if (length(others) > 0) {
      call <- rlang::call_modify(call,!!!others)
    }
    
    eval_tidy(call, env = current_env())
  }
