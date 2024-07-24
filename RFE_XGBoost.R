library(xgboost)
library(tidyverse)
library(haven)
library(purrr)
library(caret)
library(e1071)

setwd("C:/Users/Desktop")

color <- read_csv("color.csv")

# Create dummy variables for "Storage_2"
color <- color %>% 
  mutate(storage = ifelse(Storage_2 == "Storage", 1, 0))

data <- color
light <- "L"  
Red <-"a"
Yellow <-"b"


Light_Y <- as.matrix(data[[light]])
Red_Y <-as.matrix(data[[Red]])
Yellow_Y <- as.matrix(data[[Yellow]])

chemicals_X <- data[,c(9:ncol(data))]  # Include the dummy variables here

scale_chemicals_X <- scale(chemicals_X)

scale_chemicals_X <- as.matrix(scale_chemicals_X)


# Define a control object for the feature selection (10-fold cross validation)-----------------------------------
ctrl_XGB <- rfeControl(functions = xgbFuncs, method = "cv", number = 10)

# Perform RFE with XGBoost
result_xgb <- rfe(x = scale_chemicals_X, y = Light_Y, sizes = 1:36, rfeControl = ctrl_XGB, method = "xgbLinear")
result_xgb_a <- rfe(x = scale_chemicals_X, y = Red_Y, sizes = 1:36, rfeControl = ctrl_XGB, method = "xgbLinear")
result_xgb_b <- rfe(x = scale_chemicals_X, y = Yellow_Y, sizes = 1:36, rfeControl = ctrl_XGB, method = "xgbLinear")


#Export the results----------------------------------------------------------------------------
XGB_RFE_results_L<- result_xgb$results
XGB_RFE_variables_L<- result_xgb$variables

XGB_RFE_results_a<- result_xgb_a$results
XGB_RFE_variables_a<- result_xgb_a$variables

XGB_RFE_results_b<- result_xgb_b$results
XGB_RFE_variables_b<- result_xgb_b$variables

library(writexl)
library(openxlsx)

write_xlsx(XGB_RFE_results_b, path = "RFE_XGB_result.xlsx")
write.xlsx(XGB_RFE_results_b, file = "RFE_XGB_result.xlsx")

write_xlsx(XGB_RFE_variables_b, path = "RFE_XGB_variabels.xlsx")
write.xlsx(XGB_RFE_variables_b, file = "RFE_XGB_variabels.xlsx")

# Customize the controlFunc--------------------------------------------------------------------------
xgbFuncs = list()
xgbFuncs$fit = function(x,y, first, last, ...) {
  temp = if (is.data.frame(x))
    x
  else as.data.frame(x, astringAsFactors = TRUE)
  xgboost(
    data = x, 
    label = y, 
    objective = "reg:squarederror",  # Regression task
    nrounds = 1000,  # Number of boosting rounds
    eta = 0.1,
    max_depth = 2,
    early_stopping_rounds = 10,  # Stop if no improvement in 10 rounds
    verbose = 0  # Print progress
  )
}

xgbFuncs$pred = function (object, x) {
    x = as.matrix(x)
    predict(object, x)
}


xgbFuncs$rank =function (object, x, y) {
  print("SHIBAL-1")  
  coefs <- abs(xgb.importance(model = object)$Gain)
    coefs[is.na(coefs)] <- 0
    vimp <- data.frame(Overall = coefs, var = xgb.importance(model = object)$Feature)
    rownames(vimp) <- xgb.importance(model = object)$Feature
    vimp <- vimp[order(vimp$Overall, decreasing = TRUE), , drop = FALSE]
    vimp
}

xgbFuncs$selectSize = function (x, metric, maximize) {
        best <- if (maximize) 
        which.max(x[, metric])
    else which.min(x[, metric])
    min(x[best, "Variables"])
}


xgbFuncs$selectVar = function (y, size) {
  finalImp <- plyr::ddply(y[, c("Overall", "var")],  ~var, function(x) mean(x$Overall, 
        na.rm = TRUE))
    names(finalImp)[2] <- "Overall"
    finalImp <- finalImp[order(finalImp$Overall, decreasing = TRUE), 
        ]
    as.character(finalImp$var[1:size])
}

xgbFuncs$summary = function (data, lev = NULL, model = NULL) {
  if (is.character(data$obs)) 
    data$obs <- factor(data$obs, levels = lev)
  postResample(data[, "pred"], data[, "obs"])
}

