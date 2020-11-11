# 12 Using Recipes with train ---------------------------------------------
# 12.1 Why Should you learn this?
# 12.1.1 More versatile tools for preprocessing data
# 12.1.2 Using additional data to measure performance

# 12.2 An Example
library(caret)
library(recipes)
library(dplyr)
library(QSARdata)

data(AquaticTox)
tox <- AquaticTox_moe2D
ncol(tox)

## Add the outcome variable to the data frame
tox$Activity <- AquaticTox_Outcome$Activity

tox <- tox %>%
  select(-Molecule) %>%
  ## Suppose the easy of manufacturability is 
  ## related to the molecular weight of the compound
  mutate(manufacturability  = 1/moe2D_Weight) %>%
  mutate(manufacturability = manufacturability/sum(manufacturability))

# compute the RMSE using weights 
model_stats <- function(data, lev = NULL, model = NULL) {
  
  stats <- defaultSummary(data, lev = lev, model = model)
  
  wt_rmse <- function (pred, obs, wts, na.rm = TRUE) 
    sqrt(weighted.mean((pred - obs)^2, wts, na.rm = na.rm))
  
  res <- wt_rmse(pred = data$pred,
                 obs = data$obs, 
                 wts = data$manufacturability)
  c(wRMSE = res, stats)
}

# create a recipe incrementally
tox_recipe <- recipe(Activity ~ ., data = tox) %>%
  add_role(manufacturability, new_role = "performance var")

tox_recipe
tox_recipe <- tox_recipe %>% step_nzv(all_predictors())
tox_recipe

tox_recipe <- tox_recipe %>% 
  step_pca(contains("VSA"), prefix = "surf_area_",  threshold = .95) 

tox_recipe <- tox_recipe %>% 
  step_corr(all_predictors(), -starts_with("surf_area_"), threshold = .90)

tox_recipe <- tox_recipe %>% 
  step_center(all_predictors()) %>%
  step_scale(all_predictors())
tox_recipe

#  fit a SVM model and pick the tuning parameters that minimize 
# the weighted RMSE value
tox_ctrl <- trainControl(method = "cv", summaryFunction = model_stats)
set.seed(888)
tox_svm <- train(tox_recipe, tox,
                 method = "svmRadial", 
                 metric = "wRMSE",
                 maximize = FALSE,
                 tuneLength = 10,
                 trControl = tox_ctrl)
tox_svm

## originally:
ncol(tox) - 2

## after the recipe was executed:
predictors(tox_svm)

tox_svm$recipe
