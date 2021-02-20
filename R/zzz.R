.onLoad <- function(libname, pkgname) {
  
  if (any(loadedNamespaces() == "discrim")) {
    add_fastNaiveBayes_engine()
  }
  
  add_fnn_engine()
  add_neuralnet_engine()
}
