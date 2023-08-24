library(tidyverse)
library(forecast)
library(ggplot2)
library(lubridate)
library(janitor)
library(zoo)
library(CausalImpact)

# set pre and post period 
pre.period <- c(as.numeric(as.yearmon("2019-01")), as.numeric(as.yearmon("2022-05")))
post.period <- c(as.numeric(as.yearmon("2022-06")), as.numeric(as.yearmon("2023-06")))

#create tS objects
pedesAreaTS <- ts(hourlyLTNArea$avg_pedestrian_count, start = c(2019, 1), end = c(2023, 7), frequency = 24*12)
cycAreaTS <- ts(hourlyLTNArea$avg_cyclist_count, start = c(2019, 1), end = c(2023, 7), frequency = 24*12)
carAreaTS <- ts(hourlyLTNArea$avg_car_count, start = c(2019, 1), end = c(2023, 7), frequency = 24*12)

#create CI model
modelPedArea <- CausalImpact(zoo(pedesAreaTS), pre.period, post.period)
modelCycArea <- CausalImpact(zoo(cycAreaTS), pre.period, post.period)
modelCarArea <- CausalImpact(zoo(carAreaTS), pre.period, post.period)

#make plots
plot(modelPedArea) +
  labs(title = "Causal Impact Analysis for Pedestrian Counts in LTN Area",
       caption = "The blue dashed line represents the predicted values from the Causal Impact model. The black solid line represents the observed values")
plot(modelCycArea)  +
  labs(title = "Causal Impact Analysis for Cyclist Counts in LTN Area",
       caption = "The blue dashed line represents the predicted values from the Causal Impact model. The black solid line represents the observed values")
plot(modelCarArea) +
  labs(title = "Causal Impact Analysis for Car Counts in LTN Area",
       caption = "The blue dashed line represents the predicted values from the Causal Impact model. The black solid line represents the observed values")