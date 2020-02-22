#' Wrapper to add regression kriging model specification
#'
#' @return NULL
#' @export
add_kriging_rk <- function() {
  
  # define new model
  parsnip::set_new_model(
    model = "kriging_rk"
  )
  parsnip::set_model_mode(
    model = "kriging_rk", 
    mode = "regression"
  )
  parsnip::set_model_engine(
    model = "kriging_rk", 
    mode = "regression", 
    eng = "gstat"
  )
  parsnip::set_dependency(model = "kriging_rk", eng = "gstat", pkg = "gstat")
  
  # define model arguments
  parsnip::set_model_arg(
    model = "kriging_rk",
    eng = "gstat",
    parsnip = "neighbors",
    original = "nmax",
    func = list(pkg = "dials", fun = "neighbors"),
    has_submodel = TRUE
  )
  
  # fit functions
  parsnip::set_fit(
    model = "kriging_rk",
    eng = "gstat",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(fun = "kriging_train"),
      defaults = list()
    )
  )
  
  # prediction functions
  parsnip::set_pred(
    model = "kriging_rk",
    eng = "gstat",
    mode = "regression",
    type = "numeric",
    value =   list(
      pre = NULL,
      post = NULL,
      func = c(fun = "kriging_predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "numeric",
        nmax = quote(object$fit$nmax)
      )
    )
  )
  parsnip::set_pred(
    model = "kriging_rk",
    eng = "gstat",
    mode = "regression",
    type = "conf_int",
    value =   list(
      pre = NULL,
      post = NULL,
      func = c(fun = "kriging_predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "conf_int",
        nmax = quote(object$fit$nmax)
      )
    )
  )
  
}


#' Regression kriging model specification
#'
#' @param mode A character specifying the mode of the model. Only 'regression'
#'   is currently implemented.
#' @param neighbors An integer specifying the maximum number of points to use in
#'   neighborhood.
#'
#' @return NULL
#' @export
kriging_rk <- function(mode = "regression", neighbors = NULL) {
  
  # Check for correct mode
  if (mode  != "regression") {
    stop("`mode` should be 'regression'", call. = FALSE)
  }
  
  args <- list(
    neighbors = rlang::enquo(neighbors)
  )
  
  out <- list(args = args, eng_args = NULL,
              mode = mode, method = NULL, engine = NULL)
  
  class(out) <- parsnip::make_classes("kriging_rk")
  out
  
  # parsnip::new_model_spec(
  #   "kriging_rk",
  #   args = args,
  #   eng_args = NULL,
  #   mode = mode,
  #   method = NULL,
  #   engine = NULL
  # )
}


#' Print generic function method for regression kriging model
#'
#' @param x A model object.
#' @param ... Not currently used.
#'
#' @return NULL
#' @export
print.kriging_rk <- function(x, ...) {
  cat("Regression Kriging Model Specification (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)
  
  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }
  
  invisible(x)
}


#' Method to update a regression kriging model specification
#'
#' @param object A model_spec.
#' @param parameters The parameters for the model
#' @param neighbors An integer specifying the maximum number of points to use in
#'   neighborhood.
#' @param fresh A logical.
#' @param ... Currently unused.
#'
#' @export
update.kriging_rk <-
  function(object,
           parameters = NULL,
           neighbors = NULL,
           fresh = FALSE, ...) {
    
    parsnip::update_dot_check(...)
    
    if (!is.null(parameters)) {
      parameters <- parsnip::check_final_param(parameters)
    }
    
    args <- list(
      neighbors = rlang::enquo(neighbors),
      nscore = rlang::enquo(nscore)
    )
    
    args <- parsnip::update_main_parameters(args, parameters)
    
    if (fresh) {
      object$args <- args
      
    } else {
      null_args <- purrr::map_lgl(args, parsnip::null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
    }
    
    parsnip::new_model_spec(
      "kriging_rk",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }


#' Regression Kriging
#'
#' @param formula A formula object.
#' @param data An data.frame object that includes "x" and "y" columns.
#' @param nmax Integer, number of neighbors to use in prediction.
#' @param nscore A logical, whether to transform the training data using a normal
#'   score transform.
#' @param ... Other arguments to pass onto the automap::autofitVariogram
#'   function.
#'
#' @return kriging_rk class object
#' @export
kriging_train <- function(formula, data, nmax, nscore = FALSE, ...) {
  
  # select response variable
  response <- as.character(rlang::f_lhs(formula))
  
  # nscore
  if (isTRUE(nscore)) {
    trans <- nscore(data[[response]])
    data[[response]] <- trans$nscore
  } else {
    trans <- NULL
  }
  
  # select only arguments used for variogram fitting
  args <- rlang::list2(...)

  automap_args <- c(
    "model",
    "kappa",
    "fix.values",
    "verbose",
    "GLS.model",
    "start_vals",
    "miscFitOptions"
  )
  
  if (any(!names(args) %in% automap_args)) {
    fit_args <- args[names(args) %in% automap_args]
  } else {
    fit_args <- args
  }
  
  # regression
  regr_trend_model <- stats::lm(formula = formula, data = data)
  data$trend <- stats::predict(regr_trend_model, data)
  data$residual <- stats::residuals(regr_trend_model)
  
  # autofit variogram on residuals
  data <- data[, names(data) != response]
  data <- sf::st_as_sf(data, coords = c("x", "y"))
  
  fit_call <- rlang::quo(
    rlang::exec(
      automap::autofitVariogram,
      formula = residual ~ 1,
      input_data = methods::as(data, "Spatial"),
      !!!fit_args)
    )
  
  vgm_model_fit <- rlang::eval_tidy(fit_call)
  
  structure(
    list(
      lm = regr_trend_model,
      vgm = vgm_model_fit,
      data = data,
      trans = trans,
      nmax = nmax
    ),
    class = "kriging_rk"
  )
}


#' Predict function for regression kriging
#'
#' @param object A kriging_rk class object.
#' @param new_data An sf object or a SpatialGridDataFrame.
#' @param type A character with either "numeric" or "conf_int".
#' @param nmax Integer, number of closest points to use in prediction.
#' @param ... Other arguments passed to gstat::krige function.
#'
#' @return
#' @export
kriging_predict <- function(object, new_data, type, nmax, ...) {
  
  # predict trend from lm model
  new_data$trend <- stats::predict(object$lm, new_data)

  # convert to spatial object
  new_data <- sf::st_as_sf(new_data, coords = c("x", "y"))

  args <- list(
    formula = residual ~ 1,
    locations = quo(object$data),
    newdata = quo(new_data),
    model = object$vgm$var_model,
    beta = 0,
    debug.level = 0,
    nmax = nmax
  )
  
  # kriging prediction
  call <- parsnip:::make_call("krige", ns = "gstat", args = args)
  pred_krige <- rlang::eval_tidy(call)
  
  if (!is.null(object$trans)) {
    pred_krige$var1.pred <-
      backtr(pred_krige$var1.pred, object$trans, draw = FALSE)
    pred_krige$var1.var <-
      backtr(pred_krige$var1.var, object$trans, draw = FALSE)
  }
  
  # return predictions
  if (type == "numeric") {
    output <- new_data$trend + pred_krige$var1.pred
    
  } else if (type == "conf_int") {
    prediction <- new_data$trend + pred_krige$var1.pred
    output <- tibble::tibble(
      .pred_lower = prediction - (sqrt(pred_krige$var1.var) * 1.96),
      .pred_upper = prediction + (sqrt(pred_krige$var1.var) * 1.96)
    )
  }
  
  output
}


#' `multi_predict`` method for krige_rk
#'
#' @param object A model_spec.
#' @param new_data A data.frame.
#' @param type A character specifying either "numeric" or "conf_int".
#' @param neighbors An integer specifying number of closest points to use in
#' prediction.
#' @param ... Currently unused.
#'
#' @return A tibble with the prediction results.
#' @export
#' @importFrom parsnip multi_predict
multi_predict._kriging_rk <-
  function(object, new_data, type = "numeric", neighbors, ...) {

    args <- rlang::list2(...)

    if (any(names(rlang::enquos(...)) == "newdata"))
      stop("Did you mean to use `new_data` instead of `newdata`?", call. = FALSE)

    pred <- kriging_predict(object$fit, new_data, type, nmax = neighbors)

    if (type == "numeric") {
      output = tibble::tibble(neighbors = neighbors, .pred = pred)
    } else if (type == "conf_int") {
      output = pred
      output$neighbors <- neighbors
    }

    output
  }
