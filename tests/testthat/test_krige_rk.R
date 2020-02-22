library(testthat)
library(rlang)
library(parsnip)

test_that('krige_rk execution', {
  
  skip_if_not_installed("gstat")
  skip_if_not_installed("automap")
  skip_if_not_installed("sf")
  
  library(sf)
  library(gstat)
  library(automap)
  
  data(meuse.all)
  
  # test fitting
  krige_regr <-
    kriging_rk(mode = "regression",
               neighbors = 16) %>%
    set_engine("gstat", fix.values = c(2, NA, NA))
  
  fitted <- fit(krige_regr, cadmium ~ elev + dist.m + x + y, meuse.all)
  expect_s3_class(fitted$fit, "kriging_rk")
  
  # test numeric prediction
  preds_numeric <- predict(fitted, meuse.all, type = "numeric")
  expect_s3_class(preds_numeric, "tbl_df")
  expect_equal(names(preds_numeric), ".pred")
  expect_equal(preds_numeric$.pred[1:4], c(11.7, 8.6, 6.5, 2.6))
  
  # test conf_int prediction
  preds_conf_int <- predict(fitted, meuse.all, type = "conf_int")
  expect_s3_class(preds_conf_int, "tbl_df")
  expect_equal(names(preds_conf_int), c(".pred_lower", ".pred_upper"))
  
  # test multi_predict numeric
  preds_multi_numeric <-
    parsnip::multi_predict(fitted, new_data = meuse.all, neighbors = -1)
  expect_s3_class(preds_multi_numeric, "tbl_df")
  expect_equal(names(preds_multi_numeric), c("neighbors", ".pred"))

  # test multi_predict conf_int
  preds_multi_conf_int <- parsnip::multi_predict(fitted, new_data = meuse.all, type = "conf_int", neighbors = 4)
  expect_s3_class(preds_multi_conf_int, "tbl_df")
  expect_equal(names(preds_multi_conf_int), c(".pred_lower", ".pred_upper", "neighbors"))
  
  # test update
  updated_model <- update(krige_regr, neighbors = 8)
  expect_equal(eval_tidy(updated_model$args$neighbors), 8)
})
