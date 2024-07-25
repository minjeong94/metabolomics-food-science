library(xgboost)
library(tidyverse)
library(haven)
library(purrr)
library(caret)

# Set working directory
setwd("C:/Users/Desktop")

color <- read_csv("color.csv")

# Create dummy variables for "Storage_2"
color <- color %>% 
  mutate(storage = ifelse(Storage_2 == "Storage", 1, 0))


data <- color
outcome <- "L"  

XGBoost_model <- function(data, outcome)
{
  sample_rows <- sample(1:nrow(data), size = 25, replace = FALSE)
  test_data <- data[sample_rows,]
  train_data <- data[-sample_rows,]
  
  # Subset the outcome column using the column name
  train_Y <- as.matrix(train_data[[outcome]])
  test_Y  <- as.matrix(test_data[[outcome]])
  
  # Select predictor columns for training and testing data
  train_X <- train_data[,c(9:ncol(train_data))]  # Include the dummy variables here
  test_X <- test_data[,c(9:ncol(test_data))]  # Include the dummy variables here
  
  # Standardize the predictor variables (after selecting them)
  train_X <- scale(train_X)
  test_X <- scale(test_X)
  
  # Convert to matrix
  train_X <- as.matrix(train_X)
  test_X <- as.matrix(test_X) 
  
  # Create a train control object for cross-validation
  train_control <- trainControl(
    method = "cv",  # Use cross-validation
    number = 10,     # Number of folds
    verboseIter = TRUE  # Print progress
  )

  # Initialize variables to track the best model and RMSE
  best_model <- NULL
  best_rmse <- Inf
  
  
  # Define a grid of hyperparameters to search
  hyperparameters <- expand.grid(
    eta = c(0.01, 0.1, 0.2),  # Different values of eta
    nrounds = c(500, 1000)  # Different values of nrounds
  )
  
  # Perform the grid search
  for (i in 1:nrow(hyperparameters)) {
    cat("Training model with eta =", hyperparameters$eta[i], "and nrounds =", hyperparameters$nrounds[i], "\n")
    
    xgb_model <- xgboost(
      data = train_X,
      label = train_Y,
      objective = "reg:squarederror",
      nrounds = hyperparameters$nrounds[i],
      eta = hyperparameters$eta[i],
      max_depth = 2,
      early_stopping_rounds = 10,
      verbose = 0  # Set verbose to 0 for no progress print
    )
    
    # Make predictions on the test set
    prediction <- predict(xgb_model, test_X)
    
    # Calculate RMSE
    rmse <- sqrt(mean((test_Y - prediction)^2))
    
    cat("RMSE for the current model:", rmse, "\n")
    
    if (rmse < best_rmse) {
      best_model <- xgb_model
      best_rmse <- rmse
      best_eta <- hyperparameters$eta[i]
      best_nrounds <- hyperparameters$nrounds[i]
    }
  }
  
  cat("Optimal eta:", best_eta, "\n")
  cat("Optimal nrounds:", best_nrounds, "\n")
  cat("Best RMSE:", best_rmse, "\n")
  
   # Define the XGBoost model
  xgb_model <- xgboost(
    data = train_X, 
    label = train_Y, 
    objective = "reg:squarederror",  # Regression task
    nrounds = best_nrounds,  # Number of boosting rounds
    eta = best_eta,
    max_depth = 2,
    early_stopping_rounds = 10,  # Stop if no improvement in 10 rounds
    verbose = 1  # Print progress
  )
  
  # Extract feature importance scores
  feature_importance <- xgb.importance(model = xgb_model)
  
  
  # Make predictions
  prediction <- predict(xgb_model, test_X)
  
  mse <- mean((test_Y - prediction)^2)  # Calculate MSE
  rmse <- sqrt(mse)  # Calculate RMSE from MSE
  mse_sd <- sd((test_Y - prediction)^2)
  
  # Calculate the correlation between predicted and observed values
  correlation <- cor(prediction, test_Y)
  
  return(list(tibble(rmse = rmse, mse = mse, mse_sd = mse_sd, correlation = correlation ), importance = feature_importance))
}

# 10-fold cross validation
XGBoost <- map(1:10, ~ XGBoost_model(color, outcome)) 

# Correlation and other features dataset----------------------------
XGBoost_features <- bind_rows(lapply(XGBoost, function(sublist) sublist[[1]]))


# List of feature importance scores for each cross-validation run
Xgboost_vars <- lapply(XGBoost, function(sublist) sublist$importance)


Xgboost_gain <- Xgboost_vars %>% 
  reduce(left_join, by = "Feature") %>% 
  select(Feature, starts_with("Gain"))
