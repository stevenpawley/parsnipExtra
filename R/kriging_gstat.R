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
  
  # define model arguments and their mapping to parsnip nomenclature
  parsnip::set_model_arg(
    model = "kriging_rk",
    eng = "gstat",
    parsnip = "neighbors",
    original = "nmax",
    func = list(pkg = "dials", fun = "neighbors"),
    has_submodel = TRUE
  )
  
  # fit function for regression
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
  
  # prediction function for regression
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
        type = "numeric"
      )
    )
  )
  
  # prediction function for conf_int
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
        type = "conf_int"
      )
    )
  )
  
}


#' Regression kriging model specification
#'
#' @param mode character, only 'regression' is currently implemented
#' @param neighbors integer, maximum number of points to use in neighborhood
#'
#' @return NULL
#' @export
kriging_rk <- function(mode = "regression", neighbors = NULL) {
  
  args <- list(neighbors = rlang::enquo(neighbors))
  
  parsnip::new_model_spec(
    "kriging_rk",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}


#' Print generic function method for regression kriging model
#'
#' @param x A model object
#' @param ... Not currently used
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
      neighbors = rlang::enquo(neighbors)
    )
    
    args <- parsnip::update_main_parameters(args, parameters)
    
    if (fresh) {
      object$args <- args
      
    } else {
      null_args <- purrr::map_lgl(args, null_value)
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
#' @param object formula
#' @param input_data sf object containing sfc points features
#' @param ... arguments to pass onto the automap::autofitVariogram function
#'
#' @return kriging_rk class object
#' @export
kriging_train <- function(formula, data, ...) {
  
  # select response variable
  response <- as.character(rlang::f_lhs(formula))
  
  # select only arguments used for variogram fitting
  args <- list2(...)

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
  
  fit_call <- rlang::quo(
    rlang::exec(
      automap::autofitVariogram,
      formula = residual ~ 1,
      input_data = as(data, "Spatial"),
      !!!fit_args)
    )
  
  vgm_model_fit <- rlang::eval_tidy(fit_call)
  
  # save arguments used for predict function for later
  krige_args <- c(
    "nmin",
    "nmax",
    "omax",
    "maxdist",
    "block",
    "nsim",
    "indicators",
    "na.action",
    "idp"
  )
  
  if (any(names(args) %in% krige_args)) {
    predict_args <- args[names(args) %in% krige_args]
  } else {
    predict_args <- list()
  }
  
  structure(
    list(
      lm = regr_trend_model,
      vgm = vgm_model_fit,
      data = data,
      predict_args = predict_args
    ),
    class = "kriging_rk"
  )
}


#' Predict function for regression kriging
#'
#' @param object kriging_rk class object
#' @param newdata sf object, or SpatialGridDataFrame
#' @param ... other arguments passed to gstat::krige function
#'
#' @return
#' @export
kriging_predict <- function(object, new_data, type, ...) {
  
  # predict trend from lm model
  new_data$trend <- stats::predict(object$lm, new_data)
  
  # ensure crs is the same between locations and newdata
  new_data <- sf::st_set_crs(new_data, st_crs(object$data))
  
  # kriging prediction
  call <- rlang::quo(
    rlang::exec(
      gstat::krige,
      formula = residual ~ 1,
      locations = object$data,
      newdata = new_data,
      model = object$vgm$var_model,
      beta = 0,
      debug.level = 0,
      !!!object$predict_args
    )
  )
  
  pred_krige <- rlang::eval_tidy(call)
  
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


#' Multi_predict method for regression kriging model specification
#'
#' @param object A model object
#' @param new_data sf object containing new_data to predict
#' @param type character, 'numeric' or 'conf_int'
#' @param neighbors integer, maximum number of closest points to use during
#' prediction
#' @param ... Currently unused 
#'
#' @return
#' @export
multi_predict._kriging_rk <- 
  function(object, new_data, type = "numeric", neighbors = 16, ...) {
    
    args <- list2(...)

    if (any(names(enquos(...)) == "newdata"))
      stop("Did you mean to use `new_data` instead of `newdata`?", call. = FALSE)
    
    pred <- kriging_predict(object$fit, new_data, type, neighbors = neighbors)
    
    if (type == "numeric") {
      output = tibble::tibble(neighbors = neighbors, .pred = pred)
    } else if (type == "conf_int") {
      output = pred
      output$neighbors <- neighbors
    }
    
    output
  }