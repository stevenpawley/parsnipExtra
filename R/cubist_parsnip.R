library(parsnip)
library(rlang)

cubist_forest_modes <- c("regression", "unknown")

cubist_forest_engines <- data.frame(
  Cubist    = c(TRUE, TRUE),
  row.names = c( "regression", "unknown")
)

cubist_forest_arg_key <- data.frame(
  Cubist    =  c("trees", "neighbors"),
  row.names =  c("trees", "neighbors"),
  stringsAsFactors = FALSE
)

#' Cubist regression trees model specification
#'
#' @param mode character, one of c("classification", "regression")
#' @param trees integer, number of boosting iterations
#' @param neighbors integer, number of neighbors
#'
#' @return model_spec object
#' @export
cubist_forest <- function(mode = "unknown", trees = 1, neighbors = 0) {
    
    args <- list(
      trees  = enquo(trees),
      neighbors  = enquo(neighbors)
    )
    
    parsnip:::new_model_spec(
      "cubist_forest",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

cubist_forest_Cubist_data <- list(libs = "Cubist")

cubist_forest_Cubist_data$fit <-
  list(
    interface = "matrix",
    protect = c("control", "weights"),
    func = c(pkg = "Cubist", fun = "cubist"),
    defaults = list()
  )

cubist_forest_Cubist_data$numeric = list(
  pre = NULL,
  post = NULL,
  func = c(fun = "predict"),
  args =
    list(
      object = expr(object$fit),
      newdata = expr(new_data)
    )
)

translate.cubist_forest <- function(x, engine = x$engine, ...) {

  if (is.null(engine)) {
    message("Used `engine = 'Cubist'` for translation.")
    engine <- "Cubist"
  }

  x <- parsnip:::translate.default(x, engine, ...)
  arg_vals <- x$method$fit$args
  arg_names <- names(arg_vals)

  if (engine == "Cubist") {
    if (any(arg_names == "trees")) {
      arg_vals$committees <- quo(!!arg_vals$trees)
      arg_vals$trees <- NULL
    }
  }
  x$method$fit$args <- arg_vals
  x
}


update.cubist_forest <-
  function(object,
           trees = NULL, 
           neighbors = NULL, ...) {
    
    parsnip:::update_dot_check(...)
    
    args <- list(
      trees = enquo(trees),
      neighbors = enquo(neighbors)
    )
    
    null_args <- map_lgl(args, parsnip:::null_value)
    if (any(null_args))
      args <- args[!null_args]
    if (length(args) > 0)
      object$args[names(args)] <- args
    
    parsnip:::new_model_spec(
      "cubist_forest",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }