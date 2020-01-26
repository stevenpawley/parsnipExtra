library(testthat)
library(rlang)
library(sf)

test_that('krige_rk execution', {
  data(meuse.all)
  meuse.sf <- st_as_sf(meuse.all, coords = c("x", "y"))
  
  krige_regr <- kriging_rk(mode = "regression", neighbors = 16) %>%
    set_engine("gstat", fix.values = c(2, NA, NA))
  fitted <- fit(krige_regr, cadmium ~ elev + dist.m, meuse.sf)
  predict(fitted, meuse.sf, type = "numeric")
  predict(fitted, meuse.sf, type = "conf_int")
  multi_predict(fitted, new_data = meuse.sf, neighbors = 4)
  update(krige_regr, neighbors = 8)

})
