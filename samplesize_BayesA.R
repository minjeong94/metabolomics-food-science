library(BGLR)
library(tidyverse)
library(tidylog)
library(haven)
library(purrr)

setwd("C:/Users/Desktop")


# Create dummy variables for "Storage_2"
color <- read_csv("color.csv") %>% 
  mutate(storage = ifelse(Storage_2 == "Storage", 1, 0))

data = color
outcome  <- "b"  

bayes_a_model <- function(data, outcome, s = 180) 
{
  sample_rows <- sample(1:nrow(data), size = 35, replace = FALSE)
  data$index = 1:nrow(data)
  
  test_data <- data[sample_rows,]
  
  train_data <- data[-sample_rows,]
  train_data <- train_data[1:s,]
  train_data  <- rbind(train_data, test_data)
  train_data[[outcome]] <- ifelse(train_data$index %in% sample_rows, NA, train_data[[outcome]])

  # Subset the outcome column using the column name
  train_Y <- as.matrix(train_data[[outcome]])
  test_Y  <- as.matrix(test_data[[outcome]])

  # Select predictor columns for training and testing data
  train_X <- glimpse(tibble(train_data[,c(9:ncol(train_data))]))
  test_X <- tibble(test_data[,c(9:ncol(test_data))])
  
  # Convert to model.matrix
  train_X <- model.matrix(~ . - 1, data = train_X)
  test_X <- model.matrix(~ . - 1, data = test_X)
  
  # Define ETA for Bayes A
  ETA <- list(
    list(X = train_X[,ncol(train_X)], model = "FIXED"),  
    list(X = train_X[,-ncol(train_X)], model = "BayesA")) 
  
  bays_a_fit <-BGLR(train_Y, ETA = ETA, nIter = 8000,
                    burnIn = 500)  
  
  train_data$index = 1:length(train_data[[outcome]])
  
  NA_rows = train_data$index[is.na(train_data[[outcome]])]
  
  prediction <- bays_a_fit$yHat[NA_rows]
  mse <- mean((test_Y - prediction)^2)  # Calculate MSE
  rmse <- sqrt(mse)  # Calculate RMSE from MSE
  # Calculate the correlation between predicted and observed values
  correlation <- cor(prediction,  test_Y)
  
  
  return(list(tibble(rmse = rmse, mse = mse, correlation = correlation, size = s)))
}

#---------------Sample size--------------------------------------------------------------------------------
sample_size = c(50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150, 155, 160, 165, 170, 175, 180)
Bay_A_model_total <- tibble(rmse = numeric(), correlation = numeric(), size = numeric())

for(i in sample_size) {
  for(j in 1:10) {
    Bay_A_model_total = bind_rows(
      Bay_A_model_total, 
      tibble(size = i, 
             correlation = bayes_a_model(data, outcome, s = i)[[1]]$correlation,
             rmse = bayes_a_model(data, outcome, s = i)[[1]]$rmse)
    )
  }
}

#RMSE and Correlation between prediction and observation
Bay_A_model_total <- Bay_A_model_total %>% 
  group_by(size) %>% 
  summarise(correlation = mean(correlation), rmse = mean(rmse))

# Export the data into xlsx
library(writexl)
library(openxlsx)

write_xlsx(Bay_A_model_total, path = "samplesize_Bay_A.xlsx")
write.xlsx(Bay_A_model_total, file = "samplesize_Bay_A.xlsx")
