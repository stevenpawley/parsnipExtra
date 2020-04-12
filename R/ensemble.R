#' General interface for a stacked ensemble of models
#'
#' @param mode A single character string for the type of model.
#' @return A model_spec
#' @export
ensemble <- function(mode = "unknown") {
    
  args <- list()
  
  parsnip::new_model_spec(
    "ensemble",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}


#' Wrapper to add a bagging engine to parsnip
#'
#' @return NULL
#' @export
make_ensemble <- function() {
  
  # create new model
  parsnip::set_new_model("ensemble")
  
  # set model modes
  parsnip::set_model_mode(model = "ensemble", mode = "classification")
  parsnip::set_model_mode(model = "ensemble", mode = "regression")
  
  # set engine for each mode
  parsnip::set_model_engine(model = "ensemble", mode = "classification", eng = "parsnipExtra")
  parsnip::set_model_engine(model = "ensemble", mode = "regression", eng = "parsnipExtra")
  
  # set package dependencies
  parsnip::set_dependency("ensemble", "parsnipExtra", "parsnipExtra")
  
  # define fit methods
  parsnip::set_fit(
    model = "ensemble",
    eng = "parsnipExtra",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(fun = "ensemble_train"),
      defaults = list(
        mode = "regression"
      )
    )
  )
  parsnip::set_fit(
    model = "ensemble",
    eng = "parsnipExtra",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(fun = "ensemble_train"),
      defaults = list(
        mode = "classification"
      )
    )
  )
  
  # define prediction methods
  parsnip::set_pred(
    model = "ensemble",
    eng = "parsnipExtra",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "ensemble_predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "numeric"
      )
    )
  )

  parsnip::set_pred(
    model = "ensemble",
    eng = "parsnipExtra",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "ensemble_predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "class"
      )
    )
  )
}



#' Training method for a stacked ensemble
#'
#' @param formula A formula
#' @param data A data.frame to train on
#' @param mode Either "regression" or "classification"
#' @param base_workflows A named list of workflows to use as base models.
#' @param meta_model A parsnip model specification to use as the meta model.
#' @param cv A rsample resamples object. Default uses 5-fold cross validation.
#' @param ... Other arguments currently unused.
#'
#' @return A model_fit object
#' @export
ensemble_train <-
  function(formula,
           data,
           mode,
           base_workflows,
           meta_model,
           cv = purrr::partial(rsample::vfold_cv, v = 5),
           ...) {
    
    # get workflow names
    model_names <- names(base_workflows)
    pred_names <- paste(model_names, "preds", sep = "_")
    target_variable <- formula %>%
      rlang::f_lhs() %>%
      as.character()
    
    # create resamples
    folds <- cv(data = data)
    
    # fit models on resamples and store model as column
    for (i in seq_len(length(base_workflows)))
      folds <- folds %>%
        mutate(!!model_names[[i]] := map(folds$splits, ~ base_workflows[[i]] %>% fit(data = analysis(.x))))
    
    # make out-of-fold predictions
    for (i in seq_len(length(base_workflows)))
      folds <- folds %>%
        mutate(
          !!pred_names[[i]] := map2(
            folds[[model_names[i]]], 
            folds$splits, 
            ~ predict(.x, new_data = assessment(.y)))
        )
    
    # fit base models on full training dataset
    fitted_base_models <- map(base_workflows, ~ fit(.x, data = data))
    
    # gather out of fold predictions
    oof_preds <- map(folds[pred_names], bind_rows) %>%
      bind_cols() %>% set_names(pred_names)
    
    oof_preds <- bind_cols(oof_preds, map(folds$splits, ~ assessment(.x)[target_variable]) %>% bind_rows())

    # train meta model on out-of-fold predictions
    meta_fit <- meta_model %>% fit(formula, oof_preds)

    structure(
      list(
        base_models = fitted_base_models,
        meta_model = meta_fit
      ),
      class = "ensemble"
    )
  }


#' Prediction method for ensembles
#'
#' @param object A model_fit object
#' @param newdata A data.frame
#' @param type Either "regression" or "classification"
#' @param ... Other arguments currently unused
#'
#' @return A tibble
#' @export
ensemble_predict <- function(object, newdata, type = NULL, ...) {

  others <- list(...)
  
  # predictions for base models
  base_model_preds <- map_dfc(object$base_models, ~ predict(.x, new_data = newdata))
  pred_names <- paste(names(object$base_models), "preds", sep = "_")
  base_model_preds <- set_names(base_model_preds, pred_names)
  
  # pass base model predictions to meta model for prediction
  meta_model_pred <- predict(object$meta_model, new_data = base_model_preds)
  meta_model_pred
}
