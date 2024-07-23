# Load the randomForest library if not already loaded
library(tidyverse)
library(haven)
library(purrr)
library(randomForest)

setwd("C:/Users/mk41285/Documents/Pecan_storage_color/R studio")

color <- read_csv("color.csv")

# Create dummy variables for "Storage_2"
color <- color %>% 
  mutate(storage = ifelse(Storage_2 == "Storage", 1, 0))


data <- color
outcome <- "b"  

# Function to run Random Forest regression
RandomForest_model <- function(data, outcome) {
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
  
  
  # Define the Random Forest model
  rf_model <- randomForest(
    x = train_X, 
    y = train_Y, 
    ntree = 100,  # Number of trees in the forest
    mtry = sqrt(ncol(train_X)),  # Number of predictors to sample at each split
    importance = TRUE  # Compute variable importance
  )
  # Access feature importances
  importances <- importance(rf_model)
  
  # Make predictions
  prediction <- predict(rf_model, newdata = test_X)
  
  mse <- mean((test_Y - prediction)^2)  # Calculate MSE
  rmse <- sqrt(mse)  # Calculate RMSE from MSE
  mse_sd <- sd((test_Y - prediction)^2)
  
  # Calculate the correlation between predicted and observed values
  correlation <- cor(prediction, test_Y)
  
  return(list(tibble(rmse = rmse, mse = mse, mse_sd = mse_sd, correlation = correlation), importance = importances))
}

# Replicate 10 times
RandomForest <- map(1:10, ~ RandomForest_model(color, outcome))

# Correlation and other features dataset----------------------------
rf_features <- bind_rows(lapply(RandomForest, function(sublist) sublist[[1]]))


rf_features

# List of feature importance scores for each cross-validation run
rf_importance <- lapply(RandomForest, function(sublist) sublist$importance)

library(Matrix)

# Step 1: Convert sparse matrices to dense matrices
import_matrices <- lapply(rf_importance, as.matrix)
importance_dt <- tibble(chemical = rownames(import_matrices[[1]]), bind_cols(import_matrices))


# Assuming df_list is your list of dataframes
# and key_var is the common key variable
rf_IncMSE <- importance_dt %>% 
  select(chemical, starts_with("%IncMSE"))

rf_IncNodePurity <- importance_dt %>% 
  select(chemical, starts_with("IncNodePurity"))

# Export the data into xlsx
library(writexl)
library(openxlsx)

write_xlsx(rf_features, path = "1018_rf_features_b.xlsx")
write.xlsx(rf_features, file = "1018_rf_features_b.xlsx")

write_xlsx(rf_IncMSE, path = "1016_rf_IncMSE_b.xlsx")
write.xlsx(rf_IncMSE, file = "1016_rf_IncMSE_b.xlsx")

write_xlsx(rf_IncNodePurityr, path = "rf_IncNodePurity_L.xlsx")
write.xlsx(rf_IncNodePurity, file = "rf_IncNodePurity_L.xlsx")
