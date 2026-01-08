


#=================#
#    Packages     #
#=================#

library(tidyverse)
library(tidymodels)
library(gt)
library(ranger)
library(brulee)
library(pins)
library(vetiver)
library(plumber)
library(conflicted)
library(caret)



###--- Importe jeu de données 
cardio_impute <- read.csv("models_Python/data_final.csv")

cardio_impute$gluc <- as.factor(cardio_impute$gluc)
cardio_impute$cardio <- as.factor(cardio_impute$cardio)

#===========================#
#    Prepare & split data   # 
#===========================#

## set the seed for reproducibility 
set.seed(12345)

## split the data into train ans test sets stratified by sex 
cardio_split <- initial_split(cardio_impute, strata = cardio)
cardio_train <- training(cardio_split)
cardio_test <- testing(cardio_split)


## create folds for cross validation 
cardio_cv <- vfold_cv(cardio_train, v=10)


#=====================#
#    Create recipe    #
#######################

## recipe process 
cardio_rec <- 
  recipe(cardio~., data = cardio_train) |> 
  step_YeoJohnson(all_numeric_predictors()) |> 
  step_dummy(gluc) |> 
  step_normalize(all_numeric_predictors())



#===============================#
#  Specify models with parsnip  #
#===============================#

## Logistic Regression
glm_spec <-
  logistic_reg(penalty = 1) |> 
  set_engine("glm")


## Random Forest 
tree_spec <- 
  rand_forest(min_n = tune(), mtry = tune(), trees = 500) |> 
  set_engine("ranger") |> 
  set_mode("classification")


## Neural Network with torch 
mlp_brulee_spec <- 
  mlp(
    hidden_units = tune(), 
    epochs = tune(), 
    penalty = tune(), 
    learn_rate = tune()
  ) |> 
  set_engine("brulee") |> 
  set_mode("classification")




#=========================================#
#    Fit models & tune hyperparameters    # 
#=========================================#
## use bayes optimization for hyperparameters tuning 
bayes_control <- control_bayes(
  no_improve = 10L, 
  time_limit = 30, 
  save_pred = TRUE, 
  verbose = TRUE
)



#=====================================
# Fit models & tune hyperparameters  #
#====================================#
## Use workflowsets 
workflow_set <-
  workflow_set(
    preproc = list(cardio_rec), 
    models = list(glm = glm_spec, 
                  tree = tree_spec, 
                  torch = mlp_brulee_spec)
  ) |> 
  workflow_map("tune_bayes", 
               iter = 10L, 
               resamples = cardio_cv, 
               control = bayes_control)


#==========================#
#   Compare Model results  #
#==========================#

# Results models 
 rank_results(workflow_set, 
             rank_metric = "roc_auc", 
             select_best = TRUE) |> gt()


best_model_id <- "recipe_torch"


## plotting performance 
workflow_set |>  autoplot(metric = c("accuracy", "roc_auc"))



#================#
#   Finalize fit #
#================#

## Select best model 
best_fit <- 
  workflow_set |> 
  extract_workflow_set_result(best_model_id) |> 
  select_best(metric = "accuracy")



## Workflow for best model 
final_workflow <- 
  workflow_set |> 
  extract_workflow(best_model_id) |> 
  finalize_workflow(best_fit)

## fit final model with all data 
final_fit <- 
  final_workflow |> 
  last_fit(penguin_split)


#===================#
# Final fit metrics #
#===================#
final_fit |> 
  collect_metrics() |> 
  gt()


final_fit |> 
  collect_predictions() |> 
  roc_curve(sex, .pred_female) |> 
  autoplot()
