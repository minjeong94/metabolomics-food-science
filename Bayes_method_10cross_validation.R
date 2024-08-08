library(BGLR)
library(tidyverse)
library(tidylog)
library(haven)
library(purrr)

# Create dummy variables for "Storage_2": Stored samepls are "1" and control group samples are "0"
color <- read_csv("color.csv") %>% 
  mutate(storage = ifelse(Storage_2 == "Storage", 1, 0))

data = color
outcome  <- "L"  

bayes_a_model <- function(data, outcome)
{
  sample_rows <- sample(1:nrow(data), size = 25, replace = FALSE)
  test_data <- data[sample_rows,]
  train_data <- data
  
  # Subset the outcome column using the column name
  train_Y <- as.matrix(train_data[[outcome]])
  train_Y[sample_rows,] <- NA
  test_Y  <- as.matrix(test_data[[outcome]])
  
  # Select predictor columns for training and testing data
  train_X <- glimpse(tibble(train_data[,c(9:ncol(train_data))]))
  test_X <- tibble(test_data[,c(9:ncol(test_data))])
  
  # Convert to model.matrix
  train_X <- model.matrix(~ . - 1, data = train_X)
  test_X <- model.matrix(~ . - 1, data = test_X)
  
  # Define ETA for Bayes A_ Here you can change it into BayesB or BayesC
  ETA <- list(
    list(X = train_X[,ncol(train_X)], model = "FIXED"),  
    list(X = train_X[,-ncol(train_X)], model = "BayesA"))

  bays_a_fit <- BGLR(train_Y, ETA = ETA)  

  prediction <- bays_a_fit$yHat[sample_rows]
  mse <- mean((test_Y - prediction)^2)  # Calculate MSE
  rmse <- sqrt(mse)  # Calculate RMSE from MSE
  mse_sd <- sd((test_Y - prediction)^2)
  bays_a_fit$ETA
  
  # Calculate the correlation between predicted and observed values: Accuracy
  correlation <- cor(prediction, test_Y)
  
  coefs <- c(bays_a_fit$ETA[[2]]$b, storage = bays_a_fit$ETA[[1]]$b)
  
  
  return(list(tibble(rmse = rmse, mse = mse, mse_sd = mse_sd, r_sq = r_sq, correlation = correlation), coefs = coefs))
}


# Replicate 10 times: 10-fold cross validation
bayes_a_mse_L <- map(1:10, ~ bayes_a_model(color, outcome)) 


bayes_a_features <- bind_rows(lapply(bayes_a_mse_L, function(sublist) sublist[[1]]))

bayes_a_features


# Coefficient dataset-----------------------------------------------


bayes_a_coefs <- lapply(bayes_a_mse_L, function(sublist) sublist$coefs)

# Load the required library for matrix operations
library(Matrix)

# Step 1: Convert sparse matrices to dense matrices
bayes_a_matrices <- lapply(bayes_a_coefs, as.matrix)
bayes_a_dt <- tibble(chemical = rownames(bayes_a_matrices[[1]]), bind_cols(bayes_a_matrices))

print(bayes_a_dt, n = 100)
