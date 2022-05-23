library(modeldata)
data("wa_churn")

test_that("test gamboost classification", {
  engine_fit <-
    mboost::gamboost(
      churn ~ .,
      wa_churn,
      family = mboost::Binomial(link = "logit"),
      baselearner = "bols",
      control = mboost::boost_control()
    )
  
  engine_preds <- predict(engine_fit, newdata = wa_churn, type = "response")
  
  mod <- gen_additive_mod() %>% 
    set_mode("classification") %>% 
    set_engine("mboost")
  
  mod_fit <- fit(mod, churn ~ ., data = wa_churn)
  mod_preds <- predict(mod_fit, new_data = wa_churn, type = "prob")
  
  testthat::expect_equal(as.numeric(engine_preds), mod_preds[[1]])
  testthat::expect_named(mod_preds, c(".pred_Yes", ".pred_No"))
})


test_that("test gamboost regression", {
  engine_fit <-
    mboost::gamboost(
      Petal.Length ~ .,
      iris,
      family = mboost::Gaussian(),
      baselearner = "bols",
      control = mboost::boost_control()
    )
  
  engine_preds <- predict(engine_fit, newdata = iris)
  
  mod <- gen_additive_mod() %>% 
    set_mode("regression") %>% 
    set_engine("mboost")
  
  mod_fit <- fit(mod, Petal.Length ~ ., data = iris)
  mod_preds <- predict(mod_fit, new_data = iris)
  
  testthat::expect_equal(as.numeric(engine_preds), mod_preds[[1]])
  testthat::expect_named(mod_preds, c(".pred"))
})
