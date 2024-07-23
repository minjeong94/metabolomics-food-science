library(glmnet)
library(tidyverse)
library(tidylog)
library(haven)
library(purrr)

setwd("C:/Users/Desktop")

# Create dummy variables for "Storage_2": Stored samepls are "1" and control group samples are "0"
color <- read_csv("color.csv") %>% 
  mutate(storage = ifelse(Storage_2 == "Storage", 1, 0))

data = color
outcome  <- "L"  

linear_model <- function(data, outcome)
{
  sample_rows <- sample(1:nrow(data), size = 25, replace = FALSE)
  test_data <- data[sample_rows,]
  train_data <- data[-sample_rows,]
  
  # Subset the outcome column using the column name
  train_Y <- as.matrix(train_data[[outcome]])
  test_Y  <- as.matrix(test_data[[outcome]])
  
  # Select predictor columns for training and testing data
  train_X <- tibble(train_data[,c(9:ncol(train_data))])
  test_X <- tibble(test_data[,c(9:ncol(test_data))])
  
  # Standardize the predictor variables (after selecting them)
  train_X <- scale(train_X)
  test_X <- scale(test_X)
  
  # Convert to data frames
  train_X <- as.data.frame(train_X)
  test_X <- as.data.frame(test_X)
  
  # Fit a linear regression model
  linear_reg <- lm(train_Y ~ . - 1, data = train_X)
  
  prediction <- predict(linear_reg, newdata = test_X)
  mse <- mean((test_Y - prediction)^2)  # Calculate MSE
  rmse <- sqrt(mse)  # Calculate RMSE from MSE
  mse_sd <- sd((test_Y - prediction)^2)
  r_sq <- summary(linear_reg)$r.squared  # Get R-squared value
  correlation <- cor(prediction, test_Y)  # Calculate the correlation between predicted and observed values: Accuracy
  
  # Extract coefficients
  coefficients <- coef(linear_reg)
  
  return(list(tibble(rmse = rmse, mse = mse, mse_sd = mse_sd, correlation = correlation, r_sq = r_sq), coefficients = coefficients)
    )
  }

# Replicate 10 times: 10-fold cross validation
linear_mse_L <- map(1:10, ~ linear_model(color, outcome)) 

# Correlation and other features dataset----------------------------
linear_features <- bind_rows(lapply(linear_mse_L, function(sublist) sublist[[1]]))
linear_features

# Coefficient dataset-----------------------------------------------
linear_coefs <- lapply(linear_mse_L, function(sublist) sublist$coefficients)

# Load the required library for matrix operations
library(Matrix)

# Step 1: Convert sparse matrices to dense matrices
linear_coef_matrices <- lapply(linear_coefs, as.matrix)
linear_coefs_dt <- tibble(chemical = rownames(linear_coef_matrices[[1]]), bind_cols(linear_coef_matrices))

print(linear_coefs_dt, n = 100)

# Export the data into xlsx
library(writexl)
library(openxlsx)

write_xlsx(linear_features, path = "linear_features.xlsx")
write.xlsx(linear_features, file = "linear_features.xlsx")

write_xlsx(linear_coefs_dt, path = "linear_coefs.xlsx")
write.xlsx(linear_coefs_dt, file = "linear_coefs.xlsx")



