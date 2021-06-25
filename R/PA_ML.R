library(xlsx)
library(mgcv)
library(readxl)
library(tidyverse)
library(neuralnet)
file_shared <- readxl::read_excel("D:/Data to share with sir and jonathan/daily/data.xlsx",
                                  sheet = 1) %>%
  select("BAM" = `BAM-PM2.5`, "PA" = `PA-PM2.5`, "PA_RH" = `PA-RH`) %>%
  mutate_if(is.character, as.numeric) 

nn <- neuralnet(BAM ~ PA + PA_RH + PA_Temp + BC, data = file_shared, hidden = 3, act.fct = "logistic",
             linear.output = FALSE)

# write.xlsx(file_shared, "D:/Data to share with sir and jonathan/daily/GAM_imput_file.xlsx", sheetName = "Without_BC_Hourly_Corrected", 
#            col.names = TRUE, append = TRUE)

file_shared_daily <- readxl::read_excel("D:/Data to share with sir and jonathan/daily/GAM_imput_file.xlsx",
                                        sheet = 2) %>%
  select("BAM" = `BAM-PM2.5`, "PA" = `PA-PM2.5`, "PA_RH" = `PA-RH`, "PA_Temp" = `PA-Temperature`, BC) %>%
  mutate_if(is.character, as.numeric) 


# write.xlsx(file_shared_daily, "D:/Data to share with sir and jonathan/daily/GAM_imput_file.xlsx", sheetName = "Without_BC_Daily_Corrected", 
#            col.names = TRUE, append = TRUE)

# https://www.kdnuggets.com/2017/03/building-regression-models-support-vector-regression.html

library(e1071)
library(tidyverse)

file_shared_na <- file_shared %>%
  drop_na()

# prepare the model
svm_model <- svm(BAM ~ PA + PA_RH + PA_Temp + BC, file_shared_na) # Radius Basis Function (RBF) kernel
#Predict using model
file_shared_na$predicted_PA <- predict(svm_model, file_shared_na)

# Plot the model predicted and expected
plot(file_shared_na$BAM)
points(file_shared_na$predicted_PA, col = "pink", pch = 16)

rmsq <- sqrt(mean(((file_shared_na$predicted_PA - file_shared_na$BAM) ^ 2), na.rm = TRUE))
nrmse_mean <- ((rmsq / mean(file_shared_na$BAM, na.rm = TRUE))) * 100 

W <- t(svm_model$coefs) %*% svm_model$SV

# ? cost parameter to avoid overfit and error 
# SVR technique relies on kernel functions to construct the model. 
# The commonly used kernel functions are: a) Linear, b) Polynomial, c) Sigmoid and d) Radial Basis. 
# While implementing SVR technique, the user needs to select the appropriate kernel function.  
# The selection of kernel function is a tricky and requires optimization techniques for the best selection.
# svm function in R considers maximum allowed error (??i) to be 0.1.
# In order to avoid over-fitting, the svm SVR function allows us to penalize the regression through cost function. 
# The SVR technique is flexible in terms of maximum allowed error and penalty cost.
# This process of searching for the best model is called tuning of SVR model.
# The tune function evaluates performance of 1100 models (100*11) i.e. for every 
# combination of maximum allowable error (0 , 0.1 , . . . . . 1) and cost parameter (1 , 2 , 3 , 4 , 5 , . . . . . 100).

# Tuning SVR model by varying values of maximum allowable error and cost parameter

# Takes a lot of time, any chance to reduce the time taken
# Tune the SVM model
tune_model_svm <- tune(svm, BAM ~ PA + PA_RH + PA_Temp + BC, data = file_shared_na, 
                       ranges = list(elsilon = seq(0, 1, 0.1), cost = 1:100))

# Print optimum value of parameters
print(tune_model_svm)

# Plot the perfrormance of SVM Regression model
plot(tune_model_svm)

# The best model is the one with lowest MSE. 
# The darker the region the lower the MSE, which means better the model.

# Find out the best model
best_svm_model <- tune_model_svm$best.model

# Predict Y using best model
file_shared_na$Predicted_PA_best <- predict(BstModel, file_shared_na)

# Calculate RMSE of the best model 
rmsq <- sqrt(mean(((file_shared_na$Predicted_PA_best - file_shared_na$BAM) ^ 2), na.rm = TRUE))
nrmse_mean <- ((rmsq / mean(file_shared_na$BAM, na.rm = TRUE))) * 100 

# Calculate parameters of the Best SVR model
# Find value of W
W <- t(best_svm_model$coefs) %*% best_svm_model$SV
# Find value of b
b <- best_svm_model$rho