#' General interface for bagging models
#'
#' @param mode A single character string for the type of model.
#' @param base_model The parsnip model specification to use as the base model in
#'   the bagging ensemble.
#' @param mtry The number of predictors that will be randomly sampled for each
#'   model in the bagging ensemble.
#' @param trees The number of bagging models in the ensemble.
#' @param sample_prop The number or proportion of samples to use in each bagging
#'   model.
#' @param model A parsnip `model_spec` object for use in the bagging ensemble.
#'
#' @return A model_spec
#' @export
bagging <-
  function(mode = "classification",
           base_model = NULL,
           mtry = NULL,
           trees = 10,
           sample_prop = 0.67) {
    
    args <- list(
      base_model = rlang::enquo(base_model),
      mtry = rlang::enquo(mtry),
      trees = rlang::enquo(trees),
      sample_prop = rlang::enquo(sample_prop)
    )
    
    parsnip::new_model_spec(
      "bagging",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

#' @export
print.bagging <- function(x, ...) {
  cat("Bagging Model Specification (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)
  
  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }
  
  invisible(x)
}

#' @param object A bagging model specification.
#' @param base_model The parsnip model specification to use as the base model in
#'   the bagging ensemble.
#' @param mtry The number of predictors that will be randomly sampled for each
#'   model in the bagging ensemble.
#' @param trees The number of bagging models in the ensemble.
#' @param sample_prop The proportion of samples to use in each bagging
#'   model.
#' @param ... Not used for `update()`.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#' @export
update.bagging <-
  function(object,
           base_model = NULL,
           mtry = NULL,
           trees = NULL,
           sample_size = NULL,
           fresh = FALSE, ...) {
    update_dot_check(...)
    
    args <- list(
      base_model = enquo(base_model),
      mtry = enquo(mtry),
      trees = enquo(trees),
      sample_prop = enquo(sample_prop)
    )
    
    if (fresh) {
      object$args <- args
    } else {
      null_args <- map_lgl(args, null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
    }
    
    new_model_spec(
      "bagging",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }
