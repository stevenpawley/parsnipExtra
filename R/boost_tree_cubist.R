#' Wrapper to add the `Cubist` engine to the parsnip `boost_tree` models
#' specification
#'
#' @return NULL
#' @export
#' @importFrom parsnip set_model_engine set_dependency set_model_arg set_fit
#' set_pred
add_cubist_engine <- function() {
  set_model_engine("boost_tree", "regression", "Cubist")
  set_dependency("boost_tree", "Cubist", "Cubist")
  
  # declare main arguments
  set_model_arg(
    model = "boost_tree",
    eng = "Cubist",
    parsnip = "trees",
    original = "committees",
    func = list(pkg = "Cubist", fun = "cubist"),
    has_submodel = FALSE
  )
  
  # add fit module
  set_fit(
    model = "boost_tree",
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
    model = "boost_tree",
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
