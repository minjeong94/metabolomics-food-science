library(glmnet)
library(tidyverse)
library(tidylog)
library(haven)
library(purrr)

setwd("C:/Users/Desktop")

color <- read_csv("color.csv")
data = color
outcome  <- "L"  


# Create dummy variables for "Storage_2"
color <- color %>% 
  mutate(storage = ifelse(Storage_2 == "Storage", 1, 0))

lasso_model(color, outcome)
lasso_model <- function(data, outcome, s = 50)
{
  test_rows <- sample(1:nrow(data), size = 25, replace = FALSE)
  test_data <- data[test_rows,]
  train_data <- data[-test_rows,]
  sample_rows <- sample(1:nrow(train_data), size = s, replace = FALSE)
  train_data <- train_data[sample_rows,]
  
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
    alpha = 0.999 #alpha = 0.0001 ridge regression / 0.5 Elastic net regression
  )
  
  lambda_min <- cvfit$lambda.min
  
  lasso_reg <- glmnet(train_X, train_Y, lambda = lambda_min)
  prediction <- predict(lasso_reg, newx = test_X)
  mse <- mean((test_Y - prediction)^2)  # Calculate MSE
  rmse <- sqrt(mse)  # Calculate RMSE from MSE
  mse_sd <- sd((test_Y - prediction)^2)

  # Calculate the correlation between predicted and observed values
  correlation <- cor(prediction, test_Y)
  
  return(tibble(rmse = rmse, mse = mse, mse_sd = mse_sd, correlation = correlation, size = s))
}


#-------------------Sample Size--------------------------------------
sample_size = c(50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150, 155, 160, 165, 170, 175, 180, 185, 190)
lasso_total <- tibble(rmse = numeric(), correlation = numeric(), size = numeric())

models = list()
for(i in 1:10) {
  models[[i]] = lapply(sample_size, function(x) {
    lasso_model(color, outcome, s = x)})
}

models = bind_rows(models)

#RMSE and Correlation between prediction and observation
models = models %>% 
  group_by(size) %>% 
  summarise(rmse = mean(rmse), correlation = mean(correlation))

plot(models$size, models$rmse)

# Export the data into xlsx
library(writexl)
library(openxlsx)

write_xlsx(models, path = "samplesize_lasso.xlsx")
write.xlsx(models, file = "samplesize_lasso.xlsx")
