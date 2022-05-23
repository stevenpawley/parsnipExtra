#' Wrapper to add the `mboost` engine to the parsnip `gen_additive_mod` model
#' specification
#'
#' @return NULL
#' @export
add_mboost_engine <- function() {
  parsnip::set_model_engine("gen_additive_mod", "classification", "mboost")
  parsnip::set_model_engine("gen_additive_mod", "regression", "mboost")
  parsnip::set_dependency("gen_additive_mod", "mboost", "mboost")
  
  parsnip::set_fit(
    model = "gen_additive_mod",
    eng = "mboost",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "mboost", fun = "gamboost"),
      defaults = list(family = mboost::Gaussian(),
                      baselearner = "bbs",
                      control = mboost::boost_control(mstop = 100))
    )
  )
  parsnip::set_fit(
    model = "gen_additive_mod",
    eng = "mboost",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "mboost", fun = "gamboost"),
      defaults = list(family = mboost::Binomial(type = "adaboost", link = "logit"),
                      baselearner = "bbs",
                      control = mboost::boost_control(mstop = 100))
    )
  )
  parsnip::set_encoding(
    model = "gen_additive_mod",
    eng = "mboost",
    mode = "classification",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )
  parsnip::set_encoding(
    model = "gen_additive_mod",
    eng = "mboost",
    mode = "regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )
  parsnip::set_pred(
    model = "gen_additive_mod",
    eng = "mboost",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = function(x, object) as.numeric(x),
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )
  )
  parsnip::set_pred(
    model = "gen_additive_mod",
    eng = "mboost",
    mode = "regression",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )
  )
  parsnip::set_pred(
    model = "gen_additive_mod",
    eng = "mboost",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = function(x, object) object$lvl[apply(x, 1, which.max)],
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "response"
      )
    )
  )
  parsnip::set_pred(
    model = "gen_additive_mod",
    eng = "mboost",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = function(x, object) {
        x <- tibble::tibble(v1 = as.numeric(x), v2 = 1 - as.numeric(x))
        setNames(x, object$lvl)
      },
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "response"
        )
    )
  )
  parsnip::set_pred(
    model = "gen_additive_mod",
    eng = "mboost",
    mode = "classification",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "link"
      )
    )
  )
}
