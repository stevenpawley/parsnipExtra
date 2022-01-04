library(testthat)
library(rlang)
library(tibble)

data(iris)
ctrl <- control_parsnip(catch = FALSE)

test_that('rknn regression', {
  skip_if_not_installed("rknn")
  
  # regression - xy interface
  reg_xy <- nearest_neighbor(neighbors = 8) %>% 
    set_engine("rknn", seed = 1234) %>%
    set_mode("regression") %>%
    fit_xy(control = ctrl, x = iris[-c(1, 5)], y = iris$Sepal.Length)
  reg_xy_pred <- predict(reg_xy, iris[-c(1, 5)])
  
  reg_rknn_pred <- rknn::rknnReg(
    data = iris[-c(1, 5)],
    y = iris$Sepal.Length,
    k = 8,
    newdata = iris[-c(1, 5)],
    seed = 1234
  )
  expect_equal(reg_rknn_pred[["pred"]], reg_xy_pred[[1]])
})


test_that("rknn classification", {
  # nominal - xy interface
  cls_xy <- nearest_neighbor(neighbors = 8) %>% 
    set_engine("rknn", seed = 1234) %>%
    set_mode("classification") %>%
    fit_xy(control = ctrl, x = iris[-5], y = iris$Species)
  
  cls_xy_pred <- predict(cls_xy, iris[-5])
  
  cls_rknn_pred <- rknn::rknn(
    data = iris[-5],
    y = iris$Species,
    k = 8,
    newdata = iris[-5],
    seed = 1234
  )
  
  cls_fnn_pred <- tibble(.pred_class = cls_rknn_pred[["pred"]])
  expect_equal(cls_fnn_pred, cls_xy_pred)
})
