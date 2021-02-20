#' Wrapper to add the `neuralnet` engine to the parsnip `mlp` model
#' specification
#'
#' @return NULL
#' @export
add_neuralnet_engine <- function() {
  
  parsnip::set_model_engine("mlp", "classification", "neuralnet")
  parsnip::set_model_engine("mlp", "regression", "neuralnet")
  parsnip::set_dependency("mlp", "neuralnet", "neuralnet")
  
  parsnip::set_model_arg(
    model = "mlp",
    eng = "neuralnet",
    parsnip = "hidden_units",
    original = "hidden",
    func = list(pkg = "dials", fun = "hidden_units"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "mlp",
    eng = "neuralnet",
    parsnip = "epochs",
    original = "rep",
    func = list(pkg = "dials", fun = "epochs"),
    has_submodel = FALSE
  )
  parsnip::set_fit(
    model = "mlp",
    eng = "neuralnet",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "neuralnet", fun = "neuralnet"),
      defaults = list()
    )
  )
  parsnip::set_fit(
    model = "mlp",
    eng = "neuralnet",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "neuralnet", fun = "neuralnet"),
      defaults = list()
    )
  )
  parsnip::set_encoding(
    model = "mlp",
    eng = "neuralnet",
    mode = "classification",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )
  parsnip::set_encoding(
    model = "mlp",
    eng = "neuralnet",
    mode = "regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )
  parsnip::set_pred(
    model = "mlp",
    eng = "neuralnet",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = function(x, object) object$lvl[apply(x, 1, which.max)],
      func = c(pkg = "stats", fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )
  )
  parsnip::set_pred(
    model = "mlp",
    eng = "neuralnet",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = function(x, object) {
        preds <- tibble::as_tibble(x)
        names(preds) <- object$lvl
        preds
      },
      func = c(pkg = "stats", fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        prob = TRUE
      )
    )
  )
  parsnip::set_pred(
    model = "mlp",
    eng = "neuralnet",
    mode = "classification",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "stats", fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )
  )
  parsnip::set_pred(
    model = "mlp",
    eng = "neuralnet",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = function(x, object) as.numeric(x),
      func = c(pkg = "stats", fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )
  )
  parsnip::set_pred(
    model = "mlp",
    eng = "neuralnet",
    mode = "regression",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "stats", fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )
  )
}