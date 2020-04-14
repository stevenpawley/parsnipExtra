#' Saves a liquidSVM model that is contained within a parsnip model
#' specification or a workflows object
#'
#' @param wrkflow A parsnip `model_fit` or workflow object based on the
#'   liquidSVM engine.
#' @param filename The file path to save the model.
#' @param ... Currently unused.
#'
#' @return None
write_liquid <- function(object, filename, ...) {
  UseMethod("write_liquid", object)
}


#' Reads a liquidSVM model and inserts it back into a workflow or parsnip model
#' specification that is based on the liquidSVM engine.
#'
#' @param wrkflow A workflow object or a parsnip `model_fit` that is based on
#'   the liquidSVM engine.
#' @param filename The file path to the saved model.
#' @param ... Currently unused.
#'
#' @return A workflow object with the restored liquidSVM fit.
read_liquid <- function(object, filename, ...) {
  UseMethod("read_liquid", object)
}


#' @export
write_liquid.workflow <- function(object, filename) {
  fit <- workflows::pull_workflow_fit(object)
  liquidSVM::write.liquidSVM(fit$fit, filename)
}


#' @export
write_liquid.model_fit <- function(object, filename) {
  fit <- object$fit
  liquidSVM::write.liquidSVM(fit, filename)
}


#' @export
read_liquid.workflow <- function(object, filename) {
  liquid_model <- liquidSVM::read.liquidSVM(filename)
  object$fit$fit$fit <- liquid_model
  
  object
}


#' @export
read_liquid.model_fit <- function(object, filename) {
  liquid_model <- liquidSVM::read.liquidSVM(filename)
  object$fit <- liquid_model
  
  object
}