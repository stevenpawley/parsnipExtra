.onLoad <- function(libname, pkgname) {
  add_fnn_engine()
  add_liquidSVM_engine()
  add_kriging_rk()
  add_fastNaiveBayes_engine()
}

