.onLoad <- function(libname, pkgname) {
  add_fnn_engine()
  add_neuralnet_engine()
  
  if (any(loadedNamespaces() == "discrim"))
    add_fastNaiveBayes_engine()
}
