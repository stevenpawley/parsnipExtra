test_that("test gamboost classification", {
  iris_binary <- iris[iris$Species %in% c("setosa", "versicolor"), ]
  iris_binary$Species <- droplevels(iris_binary$Species)

  engine_fit <-
    mboost::gamboost(
      Species ~ .,
      iris_binary,
      family = mboost::Binomial(link = "logit"),
      baselearner = "bbs",
      control = mboost::boost_control()
    )
  
  engine_preds <- predict(engine_fit, newdata = iris_binary, type = "response")
  
  mod <- gen_additive_mod() %>% 
    set_mode("classification") %>% 
    set_engine("mboost")
  
  mod_fit <- fit(mod, Species ~ ., data = iris_binary)
  mod_preds <- predict(mod_fit, new_data = iris_binary, type = "prob")
  
  testthat::expect_equal(as.numeric(engine_preds), mod_preds[[1]])
  testthat::expect_named(mod_preds, c(".pred_setosa", ".pred_versicolor"))
})


test_that("test gamboost regression", {
  f <- Petal.Length ~ Sepal.Length + Sepal.Width + Petal.Width
  
  engine_fit <-
    mboost::gamboost(
      f,
      iris,
      family = mboost::Gaussian(),
      baselearner = "bbs",
      control = mboost::boost_control()
    )
  
  engine_preds <- predict(engine_fit, newdata = iris)
  
  mod <- gen_additive_mod() %>% 
    set_mode("regression") %>% 
    set_engine("mboost")
  
  mod_fit <- fit(mod, f, data = iris)
  mod_preds <- predict(mod_fit, new_data = iris)
  
  testthat::expect_equal(as.numeric(engine_preds), mod_preds[[1]])
  testthat::expect_named(mod_preds, c(".pred"))
})
