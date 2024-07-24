# Load the randomForest library if not already loaded
library(tidyverse)
library(haven)
library(purrr)
library(randomForest)

setwd("C:/Users/Desktop")

color <- read_csv("color.csv")

# Create dummy variables for "Storage_2"
color <- color %>% 
  mutate(storage = ifelse(Storage_2 == "Storage", 1, 0))


data <- color
outcome <- "b"  

# Function to run Random Forest regression
RandomForest_model <- function(data, outcome, s = 50) 
  {
  test_rows <- sample(1:nrow(data), size = 35, replace = FALSE)
  test_data <- data[test_rows,]
  train_data <- data[-test_rows,]
  sample_rows <- sample(1:nrow(train_data), size = s, replace = FALSE)
  train_data <- train_data[sample_rows,]
  
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

  # Make predictions
  prediction <- predict(rf_model, newdata = test_X)
  
  mse <- mean((test_Y - prediction)^2)  # Calculate MSE
  rmse <- sqrt(mse)  # Calculate RMSE from MSE
  mse_sd <- sd((test_Y - prediction)^2)
  
  # Calculate the correlation between predicted and observed values
  correlation <- cor(prediction, test_Y)
  
  return(list(rmse = rmse, mse = mse, mse_sd = mse_sd, correlation = correlation, size = s))
}

#---------------Sample size--------------------------------------------------------------------------------
sample_size = c(50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150, 155, 160, 165, 170, 175, 180)
rfmodel_total <- tibble(rmse = numeric(), correlation = numeric(), size = numeric())

for(i in sample_size) {
  for(j in 1:10) {
    rfmodel_total = bind_rows(
      rfmodel_total, 
      tibble(size = i, 
             correlation = RandomForest_model(data, outcome, s = i)[[1]]$correlation,
             rmse = RandomForest_model(data, outcome, s = i)[[1]]$rmse)
    )
  }
}

rfmodel_total <- rfmodel_total %>% 
  group_by(size) %>% 
  summarise(correlation = mean(correlation), rmse = mean(rmse))

# Export the data into xlsx
library(writexl)
library(openxlsx)

write_xlsx(rfmodel_total, path = "samplesize_rf.xlsx")
write.xlsx(rfmodel_total, file = "samplesize_rf.xlsx")


