# library(parsnip)
# library(sf)
# library(tidyverse)
# library(gstat)
# library(automap)
# library(rlang)
# 
# data(meuse.all)
# meuse.sf <- st_as_sf(meuse.all, coords = c("x", "y"))
# 
# krige_regr <- kriging(mode = "regression", neighbors = 16) %>%
#   set_engine("gstat")
# fitted <- fit(krige_regr, cadmium ~ elev + dist.m, meuse.sf)
# predict(fitted, meuse.sf, type = "numeric")
# predict(fitted, meuse.sf, type = "conf_int")



# define new model
set_new_model("kriging")
set_model_mode(model = "kriging", mode = "regression")
set_model_engine(
  model = "kriging", 
  mode = "regression", 
  eng = "gstat"
)

# define model arguments and their mapping to parsnip nomenclature
set_model_arg(
  model = "kriging",
  eng = "gstat",
  parsnip = "neighbors",
  original = "nmax",
  func = list(pkg = "dials", fun = "neighbors"),
  has_submodel = TRUE
)

# create model function
kriging <- function(mode = "regression",  neighbors = NULL) {
  
  # Check for correct mode
  if (mode != "regression")
    stop("`mode` should be 'regression'", call. = FALSE)
    
  # Capture the arguments in quosures
  args <- list(
    neighbors = rlang::enquo(neighbors)
    )
    
  # Save some empty slots for future parts of the specification
  out <- list(
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
    
  # set classes in the correct order
  class(out) <- make_classes("kriging")
  out
  }

# fit function for regression
set_fit(
  model = "kriging",
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
set_pred(
  model = "kriging",
  eng = "gstat",
  mode = "regression",
  type = "numeric",
  value =   list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict.regr_krige"),
    args = list(
      object = quote(object$fit),
      new_data = quote(new_data),
      type = "numeric",
      nmax = quote(object$spec$args$neighbors)
    )
  )
)

# prediction function for conf_int
set_pred(
  model = "kriging",
  eng = "gstat",
  mode = "regression",
  type = "conf_int",
  value =   list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict.regr_krige"),
    args = list(
      object = quote(object$fit),
      new_data = quote(new_data),
      type = "conf_int",
      nmax = quote(object$spec$args$neighbors)
    )
  )
)


#' Regression Kriging
#'
#' @param object formula
#' @param input_data sf object containing sfc points features
#' @param ... arguments to pass onto the automap::autofitVariogram function
#'
#' @return regr_krige class object
#' @export
#' @importFrom rlang list2 f_lhs exec
#' @importFrom automap autofitVariogram
#' @importFrom stats lm residuals predict
kriging_train <- function(formula, data, ...) {
  
  # select response variable
  response <- as.character(f_lhs(formula))
  
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
  
  if (any(names(args) != automap_args))
    args <- args[!which(!names(args) %in% automap_args)]
  
  # regression
  regr_trend_model <- lm(formula = formula, data = data)
  data$trend <- predict(regr_trend_model, data)
  data$residual <- residuals(regr_trend_model)
  
  # autofit variogram on residuals
  data <- data[, names(data) != response]
  
  vgm_model_fit <- exec(
    autofitVariogram,
    formula = residual ~ 1,
    input_data = as(data, "Spatial"),
    !!!args)
  
  structure(
    list(lm = regr_trend_model, vgm = vgm_model_fit, data = data),
    class = "regr_krige")
}


#' Predict function for regression kriging
#'
#' @param object regr_krige class object
#' @param newdata sf object, or SpatialGridDataFrame
#' @param ... other arguments passed to gstat::krige function
#'
#' @return
#' @export
#' @importFrom sf st_set_crs st_crs st_drop_geometry
#' @importFrom gstat krige
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr rename
#' @importFrom purrr map
predict.regr_krige <- function(object, new_data, type, ...) {
  args <- list2(...)
  args <- map(args, eval_tidy)

  # predict trend from lm model
  new_data$trend <- predict(object$lm, new_data)
  
  # ensure crs is the same between locations and newdata
  new_data <- st_set_crs(new_data, st_crs(object$data))
  
  # kriging prediction
  call <- quo(
    exec(
      krige,
      formula = residual ~ 1,
      locations = object$data,
      newdata = new_data,
      model = object$vgm$var_model,
      beta = 0,
      debug.level = 0,
      !!!args
    )
  )
  
  pred_krige <- eval_tidy(call)
  
  # return predictions
  if (type == "numeric") {
    output <- new_data$trend + pred_krige$var1.pred
    
  } else if (type == "conf_int") {
    
    prediction <- new_data$trend + pred_krige$var1.pred
    
    output <- tibble(
      .pred_lower = prediction - pred_krige$var1.var,
      .pred_upper = prediction + pred_krige$var1.var
      )
  }
  
  output
}


multi_predict._kriging <- 
  function(object, new_data, type = "numeric", neighbors = 16, ...) {
  
  if (any(names(enquos(...)) == "newdata"))
    stop("Did you mean to use `new_data` instead of `newdata`?", call. = FALSE)
  
  predict(object, new_data, type, neighbors)
  }
