library(testthat)
library(rlang)
library(tibble)

data(iris)
ctrl <- control_parsnip(catch = FALSE)

test_that('FNN execution', {
  skip_if_not_installed("FNN")
  
  # continuous
  # expect no error
  expect_error(
    nearest_neighbor(neighbors = 8) %>% 
      set_engine("FNN") %>%
      set_mode("regression") %>%
      fit_xy(x = iris[-c(1, 5)], y = iris$Sepal.Length),
    regexp = NA
  )
  
  # nominal
  # expect no error
  expect_error(
    nearest_neighbor(neighbors = 8) %>% 
      set_engine("FNN") %>%
      set_mode("classification") %>%
      fit_xy(control = ctrl, x = iris[-5], y = iris$Species),
    regexp = NA
  )
  
  expect_error(
    nearest_neighbor(neighbors = 8) %>% 
      set_engine("FNN") %>%
      set_mode("regression") %>%
      fit(as.formula(Species ~ term), data = iris, control = ctrl)
  )
  
})


test_that('FNN regression', {
  skip_if_not_installed("FNN")
  
  # regression - xy interface
  reg_xy <- nearest_neighbor(neighbors = 8) %>% 
    set_engine("FNN") %>%
    set_mode("regression") %>%
    fit_xy(control = ctrl, x = iris[-c(1, 5)], y = iris$Sepal.Length)
  
  reg_xy_pred <- predict(reg_xy, iris[-c(1, 5)])

  reg_fnn_pred <- FNN::knn.reg(
    train = iris[-c(1, 5)],
    y = iris$Sepal.Length,
    k = 8,
    test = iris[-c(1, 5)]
  )
  reg_fnn_pred <- reg_fnn_pred[["pred"]]
  expect_equal(reg_fnn_pred, reg_xy_pred[[1]])
  
  # continuous - formula interface
  reg_form <- nearest_neighbor(neighbors = 8) %>% 
    set_engine("FNN") %>%
    set_mode("regression") %>%
    fit(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
  reg_form_pred <- predict(reg_form, new_data = iris)
  
  expect_equal(reg_form_pred[[1]], reg_fnn_pred)
})


test_that("FNN classification", {
  # nominal - xy interface
  cls_xy <- nearest_neighbor(neighbors = 8) %>% 
    set_engine("FNN") %>%
    set_mode("classification") %>%
    fit_xy(control = ctrl, x = iris[-5], y = iris$Species)
  
  cls_xy_pred <- predict(cls_xy, iris[-5])
  
  cls_fnn_pred <- FNN::knn(
    train = iris[-5],
    cl = iris$Species,
    k = 8,
    test = iris[-5]
  )
  
  lvl <- levels(iris$Species)
  attributes(cls_fnn_pred) <- NULL
  cls_fnn_pred <- factor(lvl[cls_fnn_pred], levels = lvl)
  
  cls_fnn_pred <- tibble(.pred_class = cls_fnn_pred)
  expect_equal(cls_fnn_pred, cls_xy_pred)
  
})


test_that("FNN class probabilities", {
  # nominal - xy interface
  cls_xy <- nearest_neighbor(neighbors = 8) %>% 
    set_engine("FNN") %>%
    set_mode("classification") %>%
    fit_xy(control = ctrl, x = iris[-5], y = iris$Species)
  
  cls_xy_prob <- predict(cls_xy, iris[-5], type = "prob")
  
  cls_fnn_prob <- FNN::knn(
    train = iris[-5],
    cl = iris$Species,
    k = 8,
    test = iris[-5],
    prob = TRUE
  )
  
  # FNN only returns the class probability for the winning class
  # compare what is calculated in the wrapper to FNNs winning probability
  probs_df <- tibble(
    .pred_class = factor(cls_fnn_prob),
    .fnn_prob = attr(cls_fnn_prob, "prob"),
    .parsnip_prob = apply(cls_xy_prob, 1, function(row) row[which.max(row)])
  )
  
  expect_equal(probs_df$.fnn_prob, probs_df$.parsnip_prob)
  
})