.onLoad <- function(libname, pkgname) {
  add_fnn_engine()
  add_rknn_engine()
  add_neuralnet_engine()
  add_fastNaiveBayes_engine()
  add_mboost_engine()
}
