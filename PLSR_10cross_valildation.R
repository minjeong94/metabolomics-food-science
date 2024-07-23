library(pls)
library(tidyverse)
library(tidylog)
library(haven)
library(purrr)

setwd("C:/Users/mk41285/Documents/Pecan_storage_color/R studio")

color <- read_csv("color.csv") %>% 
  mutate(storage = ifelse(Storage_2 == "Storage", 1, 0))

data = color
outcome  <- "L"  

plsr_model <- function(data, outcome)
{
  sample_rows <- sample(1:nrow(data), size = 25, replace = FALSE)
  test_data <- data[sample_rows,]
  train_data <- data[-sample_rows,]
  
  # Subset the outcome column using the column name
  train_Y <- as.vector(train_data[[outcome]])
  test_Y <- as.vector(test_data[[outcome]])
  
  # Select predictor columns for training and testing data
  train_X <- train_data[, 9:ncol(train_data)]
  test_X <- test_data[, 9:ncol(test_data)]
  
  # Standardize the predictor variables (after selecting them)
  train_X <- scale(train_X)
  test_X <- scale(test_X)
  
  # Fit a PLSR model
  plsr_reg <- plsr(train_Y ~ ., data = as.data.frame(train_X), ncomp = 1)
  
  # Access loadings for the first component (replace 1 with the component of interest)
  loadings_component1 <- plsr_reg$loadings[, 1]
  
  # Predict on the test data
  prediction <- predict(plsr_reg, newdata = test_X)
  mse <- mean((test_Y - prediction)^2)  # Calculate MSE
  rmse <- sqrt(mse)  # Calculate RMSE from MSE
  mse_sd <- sd((test_Y - prediction)^2)
  #r_sq <- summary(plsr_reg)$adjr2  # Get adjusted R-squared value
  correlation <- cor(prediction, test_Y)  # Calculate the correlation between predicted and observed values
  
  return(list(tibble(rmse = rmse, mse = mse, mse_sd = mse_sd, correlation = correlation), loadings = loadings_component1))
}

# Replicate 10 times
plsr_mse_L <- map(1:10, ~ plsr_model(color, outcome))


# Correlation and other features dataset----------------------------
plsr_features <- bind_rows(lapply(plsr_mse_L, function(sublist) sublist[[1]]))
plsr_features

# Coefficient dataset-----------------------------------------------


plsr_loadings <- lapply(plsr_mse_L, function(sublist) sublist$loadings)

# Load the required library for matrix operations
library(Matrix)


# Step 1: Convert sparse matrices to dense matrices
loading_matrices <- lapply(plsr_loadings, as.matrix)
loadings_dt <- tibble(chemical = rownames(loading_matrices[[1]]), bind_cols(loading_matrices))

print(loadings_dt, n = 100)


# Export the data into xlsx
library(writexl)
library(openxlsx)

write_xlsx(plsr_features, path = "1019_plsr_features_L.xlsx")
write.xlsx(plsr_features, file = "1019_plsr_features_L.xlsx")

write_xlsx(loadings_dt, path = "1016_plsr_load_b.xlsx")
write.xlsx(loadings_dt, file = "1016_plsr_load_b.xlsx")


