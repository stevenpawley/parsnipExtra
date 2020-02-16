library(testthat)
library(parsnip)
library(discrim)
library(fastNaiveBayes)

iris_df <- tibble::as_tibble(iris)

fast_nb <- fastNaiveBayes(x = iris_df[, 1:4], y = iris_df[[5]])
pkg_classes <- predict(fast_nb, newdata = iris_df[, 1:4], type = "class")
pkg_probs <- predict(fast_nb, newdata = iris_df[, 1:4], type = "raw")

test_that('fastNaiveBayes execution', {
  
  nb <- naive_Bayes() %>%
    set_engine("fastNaiveBayes")
  
  mod <- nb %>% fit(Species ~., iris_df)
  parsnip_classes <- predict(mod, new_data = iris_df)
  parsnip_probs <- predict(mod, new_data = iris_df, type = "prob")
  parsnip_probs_mat <- as.matrix(parsnip_probs)
  colnames(parsnip_probs_mat) <- levels(iris_df$Species)
  
  expect_equal(pkg_classes, parsnip_classes$.pred_class)
  expect_equal(pkg_probs, parsnip_probs_mat)
  })


