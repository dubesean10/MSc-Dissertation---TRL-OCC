#install packages

library(tidyverse)
library(forecast)
library(ggplot2)
library(rio)
library(tseries)

#filter data for each region 

iffleyLTN <- boundaryLTNs %>% 
  filter(LTNregion == "Iffley ") %>% 
  mutate(year = format(date, "%Y"), 
         month = format(date, "%m"),
         hour = factor(substr(time_from, 1, 2), levels = sprintf("%02d", 0:23))) %>% 
  mutate(month = as.numeric(month)) %>% 
  mutate(hour = as.numeric(hour)) %>% 
  group_by(year, month, hour) %>%
  summarise(avg_pedestrian_count = mean(pedestrian),
            avg_cyclist_count = mean(cyclist),
            avg_car_count = mean(car))

morrellLTN <- boundaryLTNs %>% 
  filter(LTNregion == "Morrell") %>% 
  mutate(year = format(date, "%Y"), 
         month = format(date, "%m"),
         hour = factor(substr(time_from, 1, 2), levels = sprintf("%02d", 0:23))) %>% 
  mutate(month = as.numeric(month)) %>% 
  mutate(hour = as.numeric(hour)) %>% 
  group_by(year, month, hour) %>%
  summarise(avg_pedestrian_count = mean(pedestrian),
            avg_cyclist_count = mean(cyclist),
            avg_car_count = mean(car))

cowleyLTN <- boundaryLTNs %>%
  filter(LTNregion == "Cowley Road East") %>% 
  mutate(year = format(date, "%Y"), 
         month = format(date, "%m"),
         hour = factor(substr(time_from, 1, 2), levels = sprintf("%02d", 0:23))) %>% 
  mutate(month = as.numeric(month)) %>% 
  mutate(hour = as.numeric(hour)) %>% 
  group_by(year, month, hour) %>%
  summarise(avg_pedestrian_count = mean(pedestrian),
            avg_cyclist_count = mean(cyclist),
            avg_car_count = mean(car))

stclementsLTN <- boundaryLTNs %>% 
  filter(LTNregion == "St Clements") %>% 
  mutate(year = format(date, "%Y"), 
         month = format(date, "%m"),
         hour = factor(substr(time_from, 1, 2), levels = sprintf("%02d", 0:23))) %>% 
  mutate(month = as.numeric(month)) %>% 
  mutate(hour = as.numeric(hour)) %>% 
  group_by(year, month, hour) %>%
  summarise(avg_pedestrian_count = mean(pedestrian),
            avg_cyclist_count = mean(cyclist),
            avg_car_count = mean(car))

## create TS for each counts for each region 23/24 forecasts

iffley_pedesTS <- ts(iffleyLTN$avg_pedestrian_count, frequency = 24*12, start = c(2019, 1), end = c(2023,7))
iffley_cycTS <- ts(iffleyLTN$avg_cyclist_count, frequency = 24*12, start = c(2019, 1), end = c(2023,7))
iffley_carTS <- ts(iffleyLTN$avg_car_count, frequency = 24*12, start = c(2019, 1), end = c(2023,7))

cowley_pedesTS <- ts(cowleyLTN$avg_pedestrian_count, frequency = 24*12, start = c(2019, 1), end = c(2023,7))
cowley_cycTS <- ts(cowleyLTN$avg_cyclist_count, frequency = 24*12, start = c(2019, 1), end = c(2023,7))
cowley_carTS <- ts(cowleyLTN$avg_car_count, frequency = 24*12, start = c(2019, 1), end = c(2023,7))

morrell_pedesTS <- ts(morrellLTN$avg_pedestrian_count, frequency = 24*12, start = c(2019, 1), end = c(2023,7))
morrell_cycTS <- ts(morrellLTN$avg_cyclist_count, frequency = 24*12, start = c(2019, 1), end = c(2023,7))
morrell_carTS <- ts(morrellLTN$avg_car_count, frequency = 24*12, start = c(2019, 1), end = c(2023,7))

stclements_pedesTS <- ts(stclementsLTN$avg_pedestrian_count, frequency = 24*12, start = c(2019, 1), end = c(2023,7))
stclements_cycTS <- ts(stclementsLTN$avg_cyclist_count, frequency = 24*12, start = c(2019, 1), end = c(2023,7))
stclements_carTS <- ts(stclementsLTN$avg_car_count, frequency = 24*12, start = c(2019, 1), end = c(2023,7))

## create arima models 

arimaIffleyPedes <- auto.arima(iffley_pedesTS, stepwise = FALSE, parallel = TRUE)
arimaIffleyCyc <- auto.arima(iffley_cycTS, stepwise = FALSE, parallel = TRUE)
arimaIffleyCar <- auto.arima(iffley_carTS, stepwise = FALSE, parallel = TRUE)
arimaMorrellPedes <- auto.arima(morrell_pedesTS, stepwise = FALSE, parallel = TRUE)
arimaMorrellCyc <- auto.arima(morrell_cycTS, stepwise = FALSE, parallel = TRUE)
arimaMorrellCar <- auto.arima(morrell_carTS, stepwise = FALSE, parallel = TRUE)
arimaCowleyPedes <- auto.arima(cowley_pedesTS, stepwise = FALSE, parallel = TRUE)
arimaCowleyCyc <- auto.arima(cowley_cycTS, stepwise = FALSE, parallel = TRUE)
arimaCowleyCar <- auto.arima(cowley_carTS, stepwise = FALSE, parallel = TRUE)
arimaSTCPedes <- auto.arima(stclements_pedesTS, stepwise = FALSE, parallel = TRUE)
arimaSTCCyc <- auto.arima(stclements_cycTS, stepwise = FALSE, parallel = TRUE)
arimaSTCCar <- auto.arima(stclements_carTS, stepwise = FALSE, parallel = TRUE)

summary(arimaCowleyCyc, "report")

## summary outputs
summary(arimaIffleyPedes)
summary(arimaIffleyCar)
summary(arimaMorrellPedes)
summary(arimaMorrellCar)
summary(arimaMorrellCyc)
summary(arimaCowleyPedes)
summary(arimaCowleyCyc)
summary(arimaCowleyCar)
summary(arimaSTCPedes)
summary(arimaSTCCyc)
summary(arimaSTCCar)

# create forecasts

forecastIffleyPedes <- forecast(arimaIffleyPedes, h = 535)
#forecastIffleyCyc <- forecast(arimaIffleyCyc, h = 365)
forecastIffleyCar <- forecast(arimaIffleyCar, h = 535)
forecastMorrellPedes <- forecast(arimaMorrellPedes, h = 535)
forecastMorrellCyc <- forecast(arimaMorrellCyc, h = 535)
forecastMorrellCar <- forecast(arimaMorrellCar, h = 535)
forecastCowleyPedes <- forecast(arimaCowleyPedes, h = 535)
forecastCowleyCyc <- forecast(arimaCowleyCyc, h = 535)
forecastCowleyCar <- forecast(arimaCowleyCar, h = 535)
forecastSTCPedes <- forecast(arimaSTCPedes, h = 535)
forecastSTCCyc <- forecast(arimaSTCCyc, h = 535)
forecastSTCCar <- forecast(arimaSTCCar, h = 535)

forecast2IffleyPed <- forecast(arima2IffleyPed, h = 335)
forecast2IffleyCar <- forecast(arima2IffleyCar, h = 335)
forecast2IffleyCyc <- forecast(arima2IffleyCyc, h = 335)
forecast2MorrelPed <- forecast(arima2MorrelPed, h = 335)
forecast2MorrelCyc <- forecast(arima2MorrelCyc, h = 335)
forecast2MorrelCar <- forecast(arima2MorrelCar, h = 335)
forecast2CowleyCar <- forecast(arima2CowleyCar, h = 335)
forecast2CowleyPed <- forecast(arima2CowleyPed, h = 335)
forecast2CowleyCyc <- forecast(arima2CowleyCyc, h = 335)
forecast2STCPed <- forecast(arima2STCPed, h = 335)
forecast2STCCar <- forecast(arima2STCcar, h = 335)
forecast2STCCyc <- forecast(arima2STCCyc, h = 335)


# Plotting the 2023/2024 forecasts

par(mfrow = c(2,1))
plot(forecastIffleyPedes, main = "Forecast from ARIMA (3,1,2)(0,1,0) for Pedestrian Counts at Iffley Road LTN Boundary", ylab = "Count", xlab = "Year")
#plot(forecastIffleyCyc) ## MODEL NOT RUNNING TAKING TOO LONG
plot(forecastIffleyCar, main = "Forecast from ARIMA (1,1,0)(0,1,0) for Car Counts at Iffley Road LTN Boundary", ylab = "Count", xlab = "Year")

par(mfrow = c(3,1))
plot(forecastMorrellPedes, main = "Forecast from ARIMA (3,1,2)(0,1,0) for Pedestrian Counts at Morrell Avenue LTN Boundary", ylab = "Count", xlab = "Year")
plot(forecastMorrellCyc, main = "Forecast from ARIMA (0,1,3)(0,1,0) for Cyclist Counts at Morrell Avenue LTN Boundary", ylab = "Count", xlab = "Year")
plot(forecastMorrellCar, main = "Forecast from ARIMA (1,1,2)(0,1,0) for Car Counts at Morrell Avenue LTN Boundary", ylab = "Count", xlab = "Year")


par(mfrow = c(3,1))
plot(forecastCowleyPedes, main = "Forecast from ARIMA (0,1,5) for Pedestrian Counts at Cowley Road LTN Boundary", ylab = "Count", xlab = "Year")
plot(forecastCowleyCyc, main = "Forecast from ARIMA (1,0,3)(0,1,0) for Cyclist Counts at Cowley Road LTN Boundary", ylab = "Count", xlab = "Year")
plot(forecastCowleyCar, main = "Forecast from ARIMA (3,1,2)(0,1,0) for Car Counts at Cowley Road LTN Boundary", ylab = "Count", xlab = "Year")

par(mfrow = c(3,1))
plot(forecastSTCPedes, main = "Forecast from ARIMA (3,1,2)(0,1,0) for Pedestrian Counts at St Clements LTN Boundary", ylab = "Count", xlab = "Year")
plot(forecastSTCCyc, main = "Forecast from ARIMA (1,1,3)(0,1,0) for Cyclist Counts at St Clements LTN Boundary", ylab = "Count", xlab = "Year")
plot(forecastSTCCar, main = "Forecast from ARIMA (1,1,0)(0,1,0) for Car Counts at St Clements LTN Boundary", ylab = "Count", xlab = "Year")

## plot post implementation forecasts

par(mfrow = c(3,1))
plot(forecast2IffleyPed, main = "Forecast from ARIMA (2,1,3) for Pedestrian Counts at Iffley Road LTN Boundary", ylab = "Count", xlab = "Year")
plot(forecast2IffleyCyc, main = "Forecast from ARIMA (2,0,2) for Cyclist Counts at Iffley Road LTN Boundary", ylab = "Count", xlab = "Year")
plot(forecast2IffleyCar, main = "Forecast from ARIMA (4,0,1) for Car Counts at Iffley Road LTN Boundary", ylab = "Count", xlab = "Year")

par(mfrow = c(3,1))
plot(forecast2MorrelPed, main = "Forecast from ARIMA (2,0,2) for Pedestrian Counts at Morrell Avenue LTN Boundary", ylab = "Count", xlab = "Year")
plot(forecast2MorrelCar, main = "Forecast from ARIMA (2,0,2) for Car Counts at Morrell Avenue LTN Boundary", ylab = "Count", xlab = "Year")
plot(forecast2MorrelCyc, main = "Forecast from ARIMA (2,0,3) for Cyclist Counts at Morrell Avenue LTN Boundary", ylab = "Count", xlab = "Year")

par(mfrow = c(3,1))
plot(forecast2CowleyPed, main = "Forecast from ARIMA (2,1,3) for Pedestrian Counts at Cowley Road LTN Boundary", ylab = "Count", xlab = "Year")
plot(forecast2CowleyCar, main = "Forecast from ARIMA (4,0,1) for Car Counts at Morrell Avenue LTN Boundary", ylab = "Count", xlab = "Year")
plot(forecast2CowleyCyc, main = "Forecast from ARIMA (1,1,3) for Cyclist Counts at Morrell Avenue LTN Boundary", ylab = "Count", xlab = "Year")

par(mfrow = c(3,1))
plot(forecast2STCPed, main = "Forecast from ARIMA (4,0,1) for Pedestrian Counts at St Clemenets LTN Boundary", ylab = "Count", xlab = "Year")
plot(forecast2STCCar, main = "Forecast from ARIMA (5,0,0) for Car Counts at St Clemenets LTN Boundary", ylab = "Count", xlab = "Year")
plot(forecast2STCCyc, main = "Forecast from ARIMA (4,0,1) for Cyclist Counts at St Clemenets LTN Boundary", ylab = "Count", xlab = "Year")
