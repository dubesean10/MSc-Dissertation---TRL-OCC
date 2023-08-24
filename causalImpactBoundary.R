## install packages 
library(tidyverse)
library(forecast)
library(ggplot2)
library(lubridate)
library(janitor)
library(zoo)
library(CausalImpact)

#set pre and post period 

pre.period <- c(as.numeric(as.yearmon("2019-01")), as.numeric(as.yearmon("2022-05")))
post.period <- c(as.numeric(as.yearmon("2022-06")), as.numeric(as.yearmon("2023-06")))

# filter for different location

iffley_CI <- hourlyLTNboundary %>% filter(LTNregion == "Iffley ")
morrel_CI <- hourlyLTNboundary %>%  filter(LTNregion == "Morrell")
cowley_CI <- hourlyLTNboundary %>%  filter(LTNregion == "Cowley Road East")
stclements_CI <- hourlyLTNboundary %>%  filter(LTNregion == "St Clements")

## IFFLEY

iffleyPedTS <- ts(iffley_CI$avg_pedestrian_count, start = c(2019, 1), end = c(2023, 7), frequency = 24*12)
iffleyCycTS <- ts(iffley_CI$avg_cyclist_count, start = c(2019, 1), end = c(2023, 7), frequency = 24*12)
iffleyCarTS <- ts(iffley_CI$avg_car_count, start = c(2019, 1), end = c(2023, 7), frequency = 24*12)

### create Impact model 

iffleyPedImpact <- CausalImpact(zoo(iffleyPedTS), pre.period, post.period)
iffleyCycImpact <- CausalImpact(zoo(iffleyCycTS), pre.period, post.period)
iffleyCarImpact <- CausalImpact(zoo(iffleyCarTS), pre.period, post.period)

## plot for Iffley
iffley_pedes_plot <- plot(iffleyPedImpact) +
  labs(title = "Causal Impact Analysis for Pedestrian Counts at Iffley Road LTN Boundary",
       caption = "The blue dashed line represents the predicted values from the Causal Impact model. The black solid line represents the observed values")

iffley_cyclist_plot <- plot(iffleyCycImpact) +
  labs(title = "Causal Impact Analysis for Cyclist Counts at Iffley Road LTN Boundary",
       caption = "The blue dashed line represents the predicted values from the Causal Impact model. The black solid line represents the observed values")

iffley_car_plot <- plot(iffleyCycImpact) +
  labs(title = "Causal Impact Analysis for Car Counts at Iffley Road LTN Boundary",
       caption = "The blue dashed line represents the predicted values from the Causal Impact model. The black solid line represents the observed values")

#summary 

summary(iffleyPedImpact, "report")
summary(iffleyCycImpact, "report")
summary(iffleyCarImpact, "report")

## MORRELL

morrellPedTS <- ts(morrel_CI$avg_pedestrian_count, start = c(2019, 1), end = c(2023, 7), frequency = 24*12)
morrellCycTS <- ts(morrel_CI$avg_cyclist_count, start = c(2019, 1), end = c(2023, 7), frequency = 24*12)
morrellCarTS <- ts(morrel_CI$avg_car_count, start = c(2019, 1), end = c(2023, 7), frequency = 24*12)

## create Impact models

impactMorrellPed <- CausalImpact(zoo(morrellPedTS), pre.period, post.period)
impactMorrellCyc <- CausalImpact(zoo(morrellCycTS), pre.period, post.period)
impactMorrellCar <- CausalImpact(zoo(morrellCarTS), pre.period, post.period)

## plot 
morrell_pedes_plot <- plot(impactMorrellPed) +
  labs(title = "Causal Impact Analysis for Pedestrian Counts at Morrell Avenue LTN Boundary",
       caption = "The blue dashed line represents the predicted values from the Causal Impact model. The black solid line represents the observed values")

morrell_cyclist_plot <- plot(impactMorrellCyc) +
  labs(title = "Causal Impact Analysis for Cyclist Counts at Morrell Avenue LTN Boundary",
       caption = "The blue dashed line represents the predicted values from the Causal Impact model. The black solid line represents the observed values")

morrell_car_plot <- plot(impactMorrellCar) +
  labs(title = "Causal Impact Analysis for Car Counts at Morrell Avenue LTN Boundary",
       caption = "The blue dashed line represents the predicted values from the Causal Impact model. The black solid line represents the observed values")

## summary 

summary(impactMorrellPed, "report")
summary(impactMorrellCyc, "report")
summary(impactMorrellCar, "report")

## COWLEY

cowleyPedTS <- ts(cowley_CI$avg_pedestrian_count, start = c(2019, 1), end = c(2023, 7), frequency = 24*12)
cowleyCycTS <- ts(cowley_CI$avg_cyclist_count, start = c(2019, 1), end = c(2023, 7), frequency = 24*12)
cowleyCarTS <- ts(cowley_CI$avg_car_count, start = c(2019, 1), end = c(2023, 7), frequency = 24*12)

## create Impact Model

impactCowleyPed <- CausalImpact(zoo(cowleyPedTS), pre.period, post.period)
impactCowleyCyc <- CausalImpact(zoo(cowleyCycTS), pre.period, post.period)
impactCowleyCar <- CausalImpact(zoo(cowleyCarTS), pre.period, post.period)

##plot 

cowley_ped_plot <- plot(impactCowleyPed) +
  labs(title = "Causal Impact Analysis for Pedestrian Counts at Cowley Road LTN Boundary",
       caption = "The blue dashed line represents the predicted values from the Causal Impact model. The black solid line represents the observed values")

cowley_cyc_plot <- plot(impactCowleyCyc) +
  labs(title = "Causal Impact Analysis for Cyclist Counts at Cowley Road LTN Boundary",
       caption = "The blue dashed line represents the predicted values from the Causal Impact model. The black solid line represents the observed values")

cowley_car_plot <- plot(impactCowleyCar) +
  labs(title = "Causal Impact Analysis for Car Counts at Cowley Road LTN Boundary",
       caption = "The blue dashed line represents the predicted values from the Causal Impact model. The black solid line represents the observed values")

#summary

summary(impactCowleyPed, "report")
summary(impactCowleyCyc, "report")
summary(impactCowleyCar, "report")

# ST CLEMENTS 

stcPedTS <- ts(stclements_CI$avg_pedestrian_count, start = c(2019, 1), end = c(2023, 7), frequency = 24*12)
stcCycTS <- ts(stclements_CI$avg_cyclist_count, start = c(2019, 1), end = c(2023, 7), frequency = 24*12)
stcCarTS <- ts(stclements_CI$avg_car_count, start = c(2019, 1), end = c(2023, 7), frequency = 24*12)

## create impact model

impactSTCPed <- CausalImpact(zoo(stcPedTS), pre.period, post.period)
impactSTCCyc <- CausalImpact(zoo(stcCycTS), pre.period, post.period)
impactSTCCar <- CausalImpact(zoo(stcCarTS), pre.period, post.period)

#plot

stc_ped_plot <- plot(impactSTCPed) +
  labs(title = "Causal Impact Analysis for Pedestrian Counts at St Clements LTN Boundary",
       caption = "The blue dashed line represents the predicted values from the Causal Impact model. The black solid line represents the observed values")

stc_cyc_plot <- plot(impactSTCCyc) +
  labs(title = "Causal Impact Analysis for Cyclist Counts at St ClementsLTN Boundary",
       caption = "The blue dashed line represents the predicted values from the Causal Impact model. The black solid line represents the observed values")

stc_car_plot <- plot(impactSTCCar) +
  labs(title = "Causal Impact Analysis for Car Counts at St Clements LTN Boundary",
       caption = "The blue dashed line represents the predicted values from the Causal Impact model. The black solid line represents the observed values")

#summary 

summary(impactSTCPed, "report")
summary(impactSTCCyc, "report")
summary(impactSTCCar, "report")
