library(e1071)  # Load the e1071 package
library(tidyverse)
library(tidylog)
library(haven)
library(purrr)

# Load your data and set the working directory
setwd("C:/Users/mk41285/Documents/Pecan_storage_color/R studio")

color <- read_csv("color.csv") %>% 
  mutate(storage = ifelse(Storage_2 == "Storage", 1, 0))

data = color
outcome  <- "L"  

# Function to perform Support Vector Regression (SVR)
svr_model <- function(data, outcome) {  
 sample_rows <- sample(1:nrow(data), size = 25, replace = FALSE)
 test_data <- data[sample_rows, ]
 train_data <- data[-sample_rows, ]

 # Subset the outcome column using the column name
 train_Y <- as.vector(train_data[[outcome]])
 test_Y <- as.vector(test_data[[outcome]])

 # Select predictor columns for training and testing data
 train_X <- train_data[, c(9:ncol(train_data))]
 test_X <- test_data[, c(9:ncol(test_data))]

 # Train the SVR model
 svr_model <- svm(train_Y ~ ., data = train_X, kernel = "radial", cost = 1)
 
 # Make predictions on the test data
 prediction <- predict(svr_model, newdata = test_X)

 # Calculate Mean Squared Error (MSE)
 mse <- mean((test_Y - prediction)^2)
 rmse <- sqrt(mse)
 mse_sd <- sd((test_Y - prediction)^2)

 # Calculate R-squared (R^2)
 ss_total <- sum((test_Y - mean(test_Y))^2)
 r_sq <- 1 - mse / ss_total


  # Calculate the correlation between predicted and observed values
 correlation <- cor(prediction, test_Y)
 # Extract coefficients
 #coefficients <- coef(svr_model)
 
  return(list(tibble(rmse = rmse, mse = mse, mse_sd = mse_sd, correlation = correlation, r_sq = r_sq), coefficients = coefficients))
}

# Replicate 10 times
svr_results <- map(1:10, ~svr_model(color, outcome))

svm_features <- bind_rows(lapply(svr_results, function(sublist) sublist[[1]]))
svm_features


# Coefficient dataset-----------------------------------------------


svm_coefs <- lapply(svr_results, function(sublist) sublist$coefficients)

# Load the required library for matrix operations
library(Matrix)

# Step 1: Convert sparse matrices to dense matrices
svm_matrices <- lapply(svm_coefs, as.matrix)
svm_coefs_dt <- tibble(chemical = rownames(svm_matrices[[1]]), bind_cols(svm_matrices))

print(svm_coefs_dt, n = 100)

# Export the data into xlsx
library(writexl)
library(openxlsx)

write_xlsx(svm_features, path = "1121_svmRad_features_L.xlsx")
write.xlsx(svm_features, file = "1121_svmRad_features_L.xlsx")

write_xlsx(svm_coefs_dt, path = "1016_svmLin_coefs_dt_b.xlsx")
write.xlsx(svm_coefs_dt, file = "1016_svmLin_coefs_dt_b.xlsx")


