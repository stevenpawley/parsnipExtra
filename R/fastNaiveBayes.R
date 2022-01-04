#' Wrapper to add the `fastNaiveBayes` engine to the parsnip `naive_Bayes` model
#' specification in the parsnip package
#'
#' @return NULL
#' @export
#' @importFrom parsnip naive_Bayes
add_fastNaiveBayes_engine <- function() {
  
  parsnip::set_model_engine("naive_Bayes", "classification", "fastNaiveBayes")
  parsnip::set_dependency("naive_Bayes", "fastNaiveBayes", "fastNaiveBayes")
  
  parsnip::set_model_arg(
    model = "naive_Bayes",
    eng = "fastNaiveBayes",
    parsnip = "Laplace",
    original = "laplace",
    func = list(pkg = "dials", fun = "Laplace"),
    has_submodel = FALSE
  )
  parsnip::set_fit(
    model = "naive_Bayes",
    eng = "fastNaiveBayes",
    mode = "classification",
    value = list(
      interface = "matrix",
      protect = c("x", "y"),
      func = c(pkg = "fastNaiveBayes", fun = "fastNaiveBayes"),
      defaults = list()
    )
  )
  parsnip::set_encoding(
    model = "naive_Bayes",
    eng = "fastNaiveBayes",
    mode = "classification",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )
  parsnip::set_encoding(
    model = "naive_Bayes",
    eng = "fastNaiveBayes",
    mode = "regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )
  parsnip::set_pred(
    model = "naive_Bayes",
    eng = "fastNaiveBayes",
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
    model = "naive_Bayes",
    eng = "fastNaiveBayes",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "stats", fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "class"
      )
    )
  )
  parsnip::set_pred(
    model = "naive_Bayes",
    eng = "fastNaiveBayes",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = function(result, object) tibble::as_tibble(result),
      func = c(pkg = "stats", fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "raw"
        )
    )
  )
}
