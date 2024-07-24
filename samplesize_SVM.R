library(e1071)  # Load the e1071 package
library(tidyverse)
library(tidylog)
library(haven)
library(purrr)

# Load your data and set the working directory
setwd("C:/Users/mk41285/Documents/Pecan_storage_color/R studio")


# Create dummy variables for "Storage_2"
color <- read_csv("color.csv") %>% 
  mutate(storage = ifelse(Storage_2 == "Storage", 1, 0))

data = color
outcome  <- "b"  

# Function to perform Support Vector Regression (SVR)
svr_model <- function(data, outcome, s = 50)
{
  test_rows <- sample(1:nrow(data), size = 35, replace = FALSE)
  test_data <- data[test_rows,]
  train_data <- data[-test_rows,]
  sample_rows <- sample(1:nrow(train_data), size = s, replace = FALSE)
  train_data <- train_data[sample_rows,]
  
  # Subset the outcome column using the column name
  train_Y <- as.vector(train_data[[outcome]])
  test_Y <- as.vector(test_data[[outcome]])
  
  # Select predictor columns for training and testing data
  train_X <- train_data[, c(9:ncol(train_data))]
  test_X <- test_data[, c(9:ncol(test_data))]
  
  # Train the SVR model
  svr_model <- svm(train_Y ~ ., data = train_X, kernel = "linear", cost = 1)
  
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

  return(list(rmse = rmse, mse = mse, mse_sd = mse_sd, correlation = correlation, r_sq = r_sq, size = s))
}

#---------------Sample Size--------------------------------------------------------------------------------
sample_size = c(50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150, 155, 160, 165, 170, 175, 180)
SVmodel_total <- tibble(rmse = numeric(), correlation = numeric(), size = numeric())

for(i in sample_size) {
  for(j in 1:10) {
    SVmodel_total = bind_rows(
      SVmodel_total, 
      tibble(size = i, 
             correlation = svr_model(data, outcome, s = i)[[1]]$correlation,
             rmse = svr_model(data, outcome, s = i)[[1]]$rmse)
    )
  }
}

#RMSE and Correlation between prediction and observation
SVmodel_total <- SVmodel_total %>% 
  group_by(size) %>% 
  summarise(correlation = mean(correlation), rmse = mean(rmse))

# Export the data into xlsx
library(writexl)
library(openxlsx)

write_xlsx(SVmodel_total, path = "samplesize_SVM.xlsx")
write.xlsx(SVmodel_total, file = "samplesize_SVM.xlsx")



