library(testthat)
library(parsnip)
library(rlang)
library(tibble)
library(magrittr)

# rescale iris data
iris_df <- iris
iris_df[, 1:4] <- scale(iris_df[, 1:4])


test_that('neuralnet execution', {
  
  skip_if_not_installed("neuralnet")
  library(neuralnet)
  
  # neuralnet classification model
  set.seed(1234)
  nn_model <- neuralnet(
    formula = Species ~ .,
    data = iris_df,
    hidden = 1, 
    rep = 1
  )
  nn_probs <- predict(nn_model, iris_df)
  nn_probs <- tibble::as_tibble(as.data.frame(nn_probs))
  names(nn_probs) <- paste0(".pred_", levels(iris_df$Species))
  
  # parsnip classification model
  clf <- mlp(mode = "classification", hidden_units = 1, epochs = 1) %>%
    set_engine("neuralnet")
  set.seed(1234)
  clf_fit <- fit(clf, Species ~., iris_df)
  
  # test models are equal
  testthat::expect_equal(nn_model$net.result, clf_fit$fit$net.result)
  
})