svm_liquid_modes <- c("classification", "regression", "unknown")

svm_liquid_engines <- data.frame(
  liquidSVM = c(TRUE, TRUE, TRUE),
  row.names = c("cost", "rbf_sigma", "margin")
)

svm_liquid_arg_key <- data.frame(
  liquidSVM =  c("C", "sigma", "epsilon"),
  row.names =  c("C", "sigma", "epsilon")
)

#' liquidSVM model
#'
#' @param mode character, one of c("classification", "regression")
#' @param cost A positive number for the cost of predicting a sample within
#'  or on the wrong side of the margin
#' @param rbf_sigma A positive number for radial basis function.
#' @param margin A positive number for the epsilon in the SVM insensitive
#'   loss function (regression only)
#'   
#' @return model_spec object
#' @export
#' @importFrom rlang enquo
svm_liquid <- function(mode = "unknown", cost = NULL, rbf_sigma = NULL, margin = NULL) {
    
    args <- list(
      cost   = enquo(cost),
      rbf_sigma  = enquo(rbf_sigma),
      margin = enquo(margin)
    )
    
    parsnip:::new_model_spec(
      "svm_liquid",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }


svm_liquid_liquidSVM_data <- list(
    libs = "liquidSVM",
    fit = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "liquidSVM", fun = "svm"),
      defaults = list(folds = 5)
    ),
    numeric = list(
      pre = NULL,
      post = function(results, object) results,
      func = c(pkg = NULL, fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    ),
    class = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = NULL, fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    ),
    classprob = list(
      pre = function(x, object) {
        if (object$fit$predict.prob == FALSE)
          stop("`svm` model does not appear to use class probabilities. Was ",
               "the model fit with `predict.prob = TRUE`?",
               call. = FALSE)
        x
      },
      post = function(result, object) {
        res <- tibble::as_tibble(result)
        names(res) <- object$lvl
        res
      },
      func = c(pkg = NULL, fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          predict.prob = TRUE
        )
    ),
    raw = list(
      pre = NULL,
      func = c(pkg = NULL, fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    )
  )


#' @export
#' @importFrom rlang quo !! expr
translate.svm_liquid <- function(x, engine = x$engine, ...) {
  x <- parsnip:::translate.default(x, engine = engine, ...)
  
  # slightly cleaner code using
  arg_vals <- x$method$fit$args
  arg_names <- names(arg_vals)
  
  # add checks to error trap or change things for this method
  if (x$engine == "kernlab") {
    
    # unless otherwise specified, classification models predict probabilities
    if (x$mode == "classification" && !any(arg_names == "prob.model"))
      arg_vals$prob.model <- TRUE
    if (x$mode == "classification" && any(arg_names == "epsilon"))
      arg_vals$epsilon <- NULL
    
    # convert sigma and scale to a `kpar` argument.
    if (any(arg_names == "sigma")) {
      kpar <- expr(list())
      kpar$sigma <- arg_vals$sigma
      arg_vals$sigma <- NULL
      arg_vals$kpar <- kpar
    }
    
  }
  
  if (x$engine == "liquidSVM") {
    # convert parameter arguments
    if (any(arg_names == "sigma")) {
      arg_vals$gammas <- quo(1 / !!arg_vals$sigma)
      arg_vals$sigma <- NULL
    }
    
    if (any(arg_names == "C")) {
      arg_vals$lambdas <- arg_vals$C
      arg_vals$C <- NULL
    }
    
  }
  
  x$method$fit$args <- arg_vals
  
  # worried about people using this to modify the specification
  x
}


#' @export
#' @importFrom rlang enquo
#' @importFrom purrr map_lgl
update.svm_liquid <- function(object, cost = NULL, rbf_sigma = NULL, margin = NULL, fresh = FALSE, ...) {
    parsnip:::update_dot_check(...)
    
    args <- list(
      cost   = enquo(cost),
      rbf_sigma  = enquo(rbf_sigma),
      margin  = enquo(margin)
    )
    
    if (fresh) {
      object$args <- args
    } else {
      null_args <- map_lgl(args, parsnip:::null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
    }
    
    parsnip:::new_model_spec(
      "svm_rbf",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }