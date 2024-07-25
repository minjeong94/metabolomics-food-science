library(BGLR)
library(caret)
library(tidyverse)
library(haven)
library(purrr)
library(BGLR)

# Set working directory
setwd("C:/Users/Desktop")

color <- read_csv("color.csv")

# Create dummy variables for "Storage_2"
color <- color %>% 
  mutate(storage = ifelse(Storage_2 == "Storage", 1, 0))

data <- color
outcome <- "L"  

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

# Set the number of folds
num_folds <- 10

# Create a data frame to store results
results <- data.frame(a = numeric(num_folds), b = numeric(num_folds), RMSE = numeric(num_folds))

# Split the data into folds
folds <- createFolds(1:nrow(train_Y), k = num_folds)

# Perform cross-validation
for (fold in 1:num_folds) {
  # Get the training and testing indices for this fold
  train_indices <- unlist(folds[-fold])
  test_indices <- folds[[fold]]
  
  # Split the data into training and testing sets
  train_X_fold <- train_X[train_indices, ]
  train_Y_fold <- train_Y[train_indices]
  test_X_fold <- train_X[test_indices, ]
  test_Y_fold <- train_Y[test_indices]
  
  # Fit the Bayesian regression model for different values of a and b
  for (a_value in seq(0.1, 10, by = 0.1)) {
    for (b_value in seq(0.1, 10, by = 0.1)) {
      model <- BGLR(Y = train_Y_fold, X = train_X_fold, a = a_value, b = b_value, nIter = 1000, burnIn = 500, response_type = "gaussian")
      
      # Make predictions
      predictions <- predict(model, Xnew = test_X_fold)
      
      # Calculate RMSE
      rmse <- sqrt(mean((test_Y_fold - predictions)^2))
      
      # Store results
      results[fold, ] <- c(a = a_value, b = b_value, RMSE = rmse)
    }
  }
}

# Find the a and b values that minimize RMSE
optimal_a <- results[which.min(results$RMSE), "a"]
optimal_b <- results[which.min(results$RMSE), "b"]

# Print the optimal values
cat("Optimal a:", optimal_a, "\n")
cat("Optimal b:", optimal_b, "\n")
