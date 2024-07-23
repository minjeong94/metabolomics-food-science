library(glmnet)
library(tidyverse)
library(tidylog)
library(haven)
library(purrr)

setwd("C:/Users/mk41285/Documents/Pecan_storage_color/R studio")

color <- read_csv("color.csv")
data = color
outcome  <- "L"  

color <- color %>% 
  mutate(storage = ifelse(Storage_2 == "Storage", 1, 0))

lasso_model <- function(data, outcome)
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
  
  # Convert to model.matrix
  train_X <- model.matrix(~ . - 1, data = train_X)
  test_X <- model.matrix(~ . - 1, data = test_X)
  
  cvfit <- cv.glmnet(
    train_X,
    train_Y,
    type.measure = "mse",  # Use "mse" for Mean Squared Error
    nfolds = 10,
    alpha = 0.999
  )
  
  lambda_min <- cvfit$lambda.min
  
  lasso_reg <- glmnet(train_X, train_Y, lambda = lambda_min)
  prediction <- predict(lasso_reg, newx = test_X)
  mse <- mean((test_Y - prediction)^2)  # Calculate MSE
  rmse <- sqrt(mse)  # Calculate RMSE from MSE
  mse_sd <- sd((test_Y - prediction)^2)
  r_sq <- 1 - sum((test_Y - prediction)^2)/sum((test_Y - mean(test_Y))^2)
  
  # Calculate the correlation between predicted and observed values
  correlation <- cor(prediction, test_Y)
  
  return(list(tibble(rmse = rmse, mse = mse, mse_sd = mse_sd, correlation = correlation, r_sq = r_sq), betas = lasso_reg$beta))
}

# Replicate 10 times
lasso_mse_L <- map(1:10, ~ lasso_model(color, outcome)) 

# Correlation and other features dataset----------------------------
lasso_features <- bind_rows(lapply(lasso_mse_L, function(sublist) sublist[[1]]))

lasso_features

# Coefficient dataset-----------------------------------------------
lasso_coefs <- lapply(lasso_mse_L, function(sublist) sublist$betas)

# Load the required library for matrix operations
library(Matrix)

# Step 1: Convert sparse matrices to dense matrices
dense_matrices <- lapply(lasso_coefs, as.matrix)
beta_dt <- tibble(chemical = rownames(dense_matrices[[1]]), bind_cols(dense_matrices))

print(beta_dt, n = 100)


# Export the data into xlsx
library(writexl)
library(openxlsx)

write_xlsx(lasso_features, path = "1021_lasso_features_L.xlsx")
write.xlsx(lasso_features, file = "1021_lasso_features_L.xlsx")

write_xlsx(beta_dt, path = "1016_lasso_coef_b.xlsx")
write.xlsx(beta_dt, file = "1016_lasso_coef_b.xlsx")



