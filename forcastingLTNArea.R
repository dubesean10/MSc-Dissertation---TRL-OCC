##install packages

library(tidyverse)
library(forecast)
library(ggplot2)
library(rio)
library(tseries)
library(prophet)

# create ts objects for each type of count to predict what will happen in the rest of 2023/2024
head(hourlyLTNArea)
tsCarArea <- ts(hourlyLTNArea$avg_car_count, frequency = 24*12, start = c(2019, 1), end = c(2023,7))
tsPedArea <- ts(hourlyLTNArea$avg_pedestrian_count, frequency = 24*12, start = c(2019, 1), end = c(2023,7))
tsCycArea <- ts(hourlyLTNArea$avg_cyclist_count, frequency = 24*12, start = c(2019, 1), end = c(2023,7))

par(mfrow = c(3,1))
plot(tsCarArea)
plot(tsPedArea)
plot(tsCycArea)

#create arima models
arimaCarArea <- auto.arima(tsCarArea, stepwise = FALSE, parallel = TRUE)
arimaPedArea <- auto.arima(tsPedArea, stepwise = FALSE, parallel = TRUE)
arimaCycArea <- auto.arima(tsCycArea, stepwise = FALSE, parallel = TRUE)

#forecast

forecastCarArea <- forecast(arimaCarArea, h = 535)
forecastPedArea <- forecast(arimaPedArea, h = 535)
forecastCycArea <- forecast(arimaCycArea, h = 535)

#residuals

carResid <- residuals(arimaCarArea)
cycResid <- residuals(arimaCycArea)
pedResid <- residuals(arimaPedArea)

## ACF 

par(mfrow = c(3,1))
acf(carResid, main = "LTN Area Car ACF")
acf(cycResid, main = "LTN Area Cyclists ACF")
acf(pedResid, main = "LTN Area Pedestrians ACF")

#plot 2023/2024 forecast

par(mfrow = c(3,1))
plot(forecastCarArea, main = "Forecast from ARIMA (1,0,1)(0,1,0) for Car Counts in LTN Area", ylab = "Count", xlab = "Year")
plot(forecastCycArea, main = "Forecast from ARIMA (1,0,3)(0,1,0) for Cyclist Counts in LTN Area", ylab = "Count", xlab = "Year")
plot(forecastPedArea, main = "Forecast from ARIMA (1,0,0)(0,1,0) for Pedestrian Counts in LTN Area", ylab = "Count", xlab = "Year")

par(mfrow = c(3,1))
plot(postForecastCar, main = "Forecast from ARIMA (1,0,1)(0,1,0) for Car Counts in LTN Area", ylab = "Count", xlab = "Year")
plot(postForecastPed, main = "Forecast from ARIMA (1,0,3)(0,1,0) for Cyclist Counts in LTN Area", ylab = "Count", xlab = "Year")
plot(postForecastCar, main = "Forecast from ARIMA (1,0,0)(0,1,0) for Pedestrian Counts in LTN Area", ylab = "Count", xlab = "Year")


accuracy(forecastCarArea)
accuracy(forecastCycArea)
accuracy(forecastPedArea)

### CROSS VALIDATOIN

# Perform cross-validation for ARIMA models
num_folds <- 5  # Specify the number of folds for cross-validation

# Cross-validation for Car Area
cv_results_car <- tsCV(tsCarArea, forecastfunction = forecast, h = 336, method = "arima", cv = num_folds)

# Cross-validation for Pedestrian Area
cv_results_ped <- tsCV(tsPedArea, forecastfunction = forecast, h = 336, method = "arima", cv = num_fold)

# Cross-validation for Cyclist Area
cv_results_cyc <- tsCV(tsCycArea, forecastfunction = forecast, h = 336, method = "arima", cv = num_folds)

# Calculate mean cross-validation errors for each model
mean_cv_error_car <- mean(cv_results_car, na.rm = TRUE)
mean_cv_error_ped <- mean(cv_results_ped, na.rm = TRUE)
mean_cv_error_cyc <- mean(cv_results_cyc, na.rm = TRUE)

# Calculate the mean of the cross-validated forecast errors for Car Area
mean_cv_error_car <- mean(cv_results_car, na.rm = TRUE)

# Print the mean cross-validation errors
cat("Mean CV Error for Car Area:", mean_cv_error_car, "\n")
cat("Mean CV Error for Pedestrian Area:", mean_cv_error_ped, "\n")
cat("Mean CV Error for Cyclist Area:", mean_cv_error_cyc, "\n")






