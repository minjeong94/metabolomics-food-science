# Load the randomForest library if not already loaded
library(tidyverse)
library(haven)
library(purrr)
library(randomForest)

# Example data
setwd("C:/Users/Desktop")

color <- read_csv("color.csv")

# Create dummy variables for "Storage_2"
color <- color %>% 
  mutate(storage = ifelse(Storage_2 == "Storage", 1, 0))


data <- color
outcome <- "L"  

# Function to run Random Forest regression with cross-validation
RandomForest_CV <- function(data, outcome, ntree_range) 
  {
  # Initialize a list to store cross-validation results
  cv_results <- list()
  
  for (ntree in ntree_range) {
    # Initialize a vector to store results for the current ntree
    cv_result <- c()
    
    # Perform k-fold cross-validation (adjust k as needed)
    k <- 10  # You can change the number of folds as desired
    
    for (fold in 1:k) {
      # Split the data into training and validation folds
      set.seed(123)  # Set a random seed for reproducibility
      sample_rows <- sample(1:nrow(data), size = 25, replace = FALSE)
      validation_data <- data[sample_rows, ]
      training_data <- data[-sample_rows, ]
      
      # Subset the outcome column using the column name
      train_Y <- as.matrix(training_data[[outcome]])
      valid_Y <- as.matrix(validation_data[[outcome]])
      
      # Select predictor columns for training and validation data
      train_X <- training_data[, c(9:ncol(training_data))]  # Include the dummy variables here
      valid_X <- validation_data[, c(9:ncol(validation_data))]  # Include the dummy variables here
      
      # Standardize the predictor variables (after selecting them)
      train_X <- scale(train_X)
      valid_X <- scale(valid_X)
      
      # Convert to matrix
      train_X <- as.matrix(train_X)
      valid_X <- as.matrix(valid_X)
      
      # Define the Random Forest model
      rf_model <- randomForest(
        x = train_X, 
        y = train_Y, 
        ntree = ntree,  # Use the current ntree value from the loop
        mtry = sqrt(ncol(train_X)),  # Number of predictors to sample at each split
      )
      
      # Make predictions on the validation fold
      prediction <- predict(rf_model, newdata = valid_X)
      
      # Calculate and store RMSE for this fold
      mse <- mean((valid_Y - prediction)^2)  # Calculate MSE
      rmse <- sqrt(mse)
      cv_result <- c(cv_result, rmse)
    }
    
    # Calculate the mean RMSE across all folds for the current ntree value
    mean_rmse <- mean(cv_result)
    
    # Store the mean RMSE and ntree value in the results list
    cv_results[[as.character(ntree)]] <- mean_rmse
  }
  
  # Find the ntree value with the lowest mean RMSE
  best_ntree <- as.integer(names(which.min(cv_results)))
  
  return(best_ntree)
}

# Define a range of ntree values to test
ntree_range <- seq(10, 200, by = 5)

# Find the optimal ntree using cross-validation
best_ntree <- RandomForest_CV(data, outcome, ntree_range)

# Print the best ntree value
cat("Best ntree:", best_ntree, "\n")


# Replicate 10 times
RandomForest <- map(1:10, ~ RandomForest_model(color, outcome))

# Correlation and other features dataset----------------------------
rf_features <- bind_rows(lapply(RandomForest, function(sublist) sublist[[1]]))
