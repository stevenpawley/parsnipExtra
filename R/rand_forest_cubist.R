#' Wrapper to add the `Cubist` engine to the parsnip `rand_forest` models
#' specification
#'
#' @return NULL
#' @export
#' @importFrom parsnip set_model_engine set_dependency set_model_arg set_fit
#' set_pred
add_cubist_engine <- function() {
  set_model_engine("rand_forest", "regression", "Cubist")
  set_dependency("rand_forest", "Cubist", "Cubist")
  
  # declare main arguments
  set_model_arg(
    model = "rand_forest",
    eng = "Cubist",
    parsnip = "trees",
    original = "committees",
    func = list(pkg = "Cubist", fun = "cubist"),
    has_submodel = FALSE
  )
  
  # add fit module
  set_fit(
    model = "rand_forest",
    eng = "Cubist",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "Cubist", fun = "cubist"),
      defaults = list()
    )
  )
  
  set_pred(
    model = "rand_forest",
    eng = "Cubist",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "numeric"
        )
    )
  )
}
