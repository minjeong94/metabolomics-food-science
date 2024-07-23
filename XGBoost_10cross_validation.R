library(xgboost)
library(tidyverse)
library(haven)
library(purrr)

setwd("C:/Users/mk41285/Documents/Pecan_storage_color/R studio")

color <- read_csv("color.csv")

# Create dummy variables for "Storage_2"
color <- color %>% 
  mutate(storage = ifelse(Storage_2 == "Storage", 1, 0))


data <- color
outcome <- "L"  

XGBoost_model <- function(data, outcome, size = 50)
{
  sample_rows <- sample(1:nrow(data), size = size, replace = FALSE)
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
  
  # Define the XGBoost model
  xgb_model <- xgboost(
    data = train_X, 
    label = train_Y, 
    objective = "reg:squarederror",  # Regression task
    nrounds = 1000,  # Number of boosting rounds
    eta = 0.1,
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

sample_size = c(50:200)



for(i in sample_size) {
  
}

# Replicate 10 times
XGBoost <- map(1:10, ~ XGBoost_model(color, outcome)) 

# Correlation and other features dataset----------------------------
XGBoost_features <- bind_rows(lapply(XGBoost, function(sublist) sublist[[1]]))

XGBoost_features

# List of feature importance scores for each cross-validation run
Xgboost_vars <- lapply(XGBoost, function(sublist) sublist$importance)

# Assuming df_list is your list of dataframes
# and key_var is the common key variable
Xgboost_gain <- Xgboost_vars %>% 
  reduce(left_join, by = "Feature") %>% 
  select(Feature, starts_with("Gain"))

Xgboost_cover <- Xgboost_vars %>% 
  reduce(left_join, by = "Feature") %>% 
  select(Feature, starts_with("Cover"))


Xgboost_frequency <- Xgboost_vars %>% 
  reduce(left_join, by = "Feature") %>% 
  select(Feature, starts_with("Frequency"))


# Export the data into xlsx
library(writexl)
library(openxlsx)

write_xlsx(XGBoost_features, path = "1018_XGBoost_features_b.xlsx")
write.xlsx(XGBoost_features, file = "1018_XGBoost_features_b.xlsx")

write_xlsx(Xgboost_gain, path = "1016_gain_Xgboost_b.xlsx")
write.xlsx(Xgboost_gain, file = "1016_gain_Xgboost_b.xlsx")

write_xlsx(Xgboost_cover, path = "cover_Xgboost_L.xlsx")
write.xlsx(Xgboost_cover, file = "cover_Xgboost_L.xlsx")

write_xlsx(Xgboost_frequency, path = "frequency_Xgboost_L.xlsx")
write.xlsx(Xgboost_frequency, file = "frequency_Xgboost_L.xlsx")

