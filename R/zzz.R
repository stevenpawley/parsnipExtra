.onLoad <- function(libname, pkgname) {
  add_fnn_engine()
  add_liquidSVM_engine()
  add_fastNaiveBayes_engine()
  add_neuralnet_engine()
}
