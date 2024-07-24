# Install and load necessary packages
library(caret)
library(glmnet)
library(e1071)
library(tidyverse)

# Example data

setwd("C:/Users/Desktop")

color <- read_csv("color.csv")

# Create dummy variables for "Storage_2"
color <- color %>% 
  mutate(storage = ifelse(Storage_2 == "Storage", 1, 0))

outcome <- "L"  
Red <-"a"
Yellow <-"b"

# x variables
chemicals_X <- tibble(color[,c(9:ncol(color))])

# Convert to model.matrix
chemicals_X <- model.matrix(~ . - 1, data = chemicals_X)

light_Y <- as.matrix(color[[outcome]])#as.matrix(data[[outcome]])
Red_Y <-as.matrix(color[[Red]])
Yellow_Y <- as.matrix(color[[Yellow]])


# Define a control object for the feature selection----------------------------------------------
ctrl_svm <- rfeControl(functions = svmFuncs, method = "cv", number = 10)

# Perform RFE with SVM
svm_rfe_result <- rfe(chemicals_X, light_Y, sizes = 1:36, rfeControl = ctrl_svm)
svm_rfe_result_a <- rfe(chemicals_X, Red_Y, sizes = 1:36, rfeControl = ctrl_svm)
svm_rfe_result_b <- rfe(chemicals_X, Yellow_Y, sizes = 1:36, rfeControl = ctrl_svm)

#Export the results----------------------------------------------------------------------------
SVM_RFE_results_L <- svm_rfe_result$results
SVM_RFE_variables_L <- svm_rfe_result$variables

SVM_RFE_results_a <- svm_rfe_result_a$results
SVM_RFE_variables_a <- svm_rfe_result_a$variables

SVM_RFE_results_b <- svm_rfe_result_b$results
SVM_RFE_variables_b <- svm_rfe_result_b$variables

library(writexl)
library(openxlsx)

write_xlsx(SVM_RFE_results_b , path = "RFE_SVM_result.xlsx")
write.xlsx(SVM_RFE_results_b , file = "RFE_SVM_result.xlsx")

write_xlsx(SVM_RFE_variables_b, path = "RFE_SVM_variabels.xlsx")
write.xlsx(SVM_RFE_variables_b, file = "RFE_SVM_variabels.xlsx")

#Customize the function for SVM----------------------------------------------------------------
svmFuncs = list()

svmFuncs$summary = function (data, lev = NULL, model = NULL) {
  if (is.character(data$obs)) 
    data$obs <- factor(data$obs, levels = lev)
  postResample(data[, "pred"], data[, "obs"])
}


svmFuncs$fit = function(x,y, first, last, ...) {
  tmp <- if (is.data.frame(x)) 
    x
  else as.data.frame(x, stringsAsFactors = TRUE)
  tmp$y <- y
  svm(y ~ ., data = tmp, kernel = "linear", cost = 1)
}


svmFuncs$pred = function (object, x) {
  predict(object, x)
}


svmFuncs$rank =function (object, x, y) {
  coefs <- abs(coef(object))
  coefs <- coefs[names(coefs) != "(Intercept)"]
  coefs[is.na(coefs)] <- 0
  vimp <- data.frame(Overall = unname(coefs), var = names(coefs))
  rownames(vimp) <- names(coefs)
  vimp <- vimp[order(vimp$Overall, decreasing = TRUE), , drop = FALSE]
  vimp
}

svmFuncs$selectSize = function (x, metric, maximize) {
  best <- if (maximize) 
    which.max(x[, metric])
  else which.min(x[, metric])
  min(x[best, "Variables"])
}

svmFuncs$selectVar = function (y, size) {
  finalImp <- plyr::ddply(y[, c("Overall", "var")], ~var, function(x) mean(x$Overall, 
                                                                       na.rm = TRUE))
  names(finalImp)[2] <- "Overall"
  finalImp <- finalImp[order(finalImp$Overall, decreasing = TRUE), 
  ]
  as.character(finalImp$var[1:size])
}




