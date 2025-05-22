# ./R/ml_utils.R

# Load required libraries
library(recipes)
library(caret)
library(dplyr)

# Create preprocessing recipe (centering and scaling)
make_preprocessing_recipe <- function(train_data) {
  recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, data = train_data) %>%
    step_center(all_numeric(), -all_outcomes()) %>%
    step_scale(all_numeric(), -all_outcomes())
}

# Fit linear regression model using caret
fit_linear_model <- function(train_data) {
  train(
    form = GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F,
    data = train_data,
    trControl = trainControl(method = "none"),
    method = "lm"
  )
}

# Fit KNN model using caret and preprocessing recipe
fit_knn_model <- function(train_data, recipe_obj) {
  train(
    recipe_obj,
    data = train_data,
    method = "knn",
    trControl = trainControl(method = "none")
  )
}
