## LOAD PACKAGES
library(tidyverse)
library(ggplot2)
library(changepoint)
library(hms)

### ---------------------------------------------------------------------------
# Comparison of Hourly Pedestrian / Cyclist / Car Counts at the different LTN locations in East Oxford
### ---------------------------------------------------------------------------

# Load Data
 
#boundaryLTNs <- load("/Users/seandube/Desktop/TRL Disseration HUB/Data/TRL R DATA FILES/boundaryLTNs.RData")
#ltnArea <- load("/Users/seandube/Desktop/TRL Disseration HUB/Data/TRL R DATA FILES/ltnArea.RData")

## prepare data
newBoundary <- boundaryLTNs %>% 
  mutate(year = format(date, "%Y"), 
         month = format(date, "%m"),
         hour = factor(substr(time_from, 1, 2), levels = sprintf("%02d", 0:23))) %>% 
  mutate(month = as.numeric(month)) %>% 
  select(date, year, month, hour, cyclist, pedestrian, cyclist, LTNregion)

hourlyLTNboundary <- boundaryLTNs %>% 
  mutate(year = format(date, "%Y"), 
         month = format(date, "%m"),
         hour = factor(substr(time_from, 1, 2), levels = sprintf("%02d", 0:23))) %>% 
  mutate(month = as.numeric(month)) %>% 
  group_by(year, month, hour, LTNregion) %>%
  summarise(avg_pedestrian_count = mean(pedestrian),
            avg_cyclist_count = mean(cyclist),
            avg_car_count = mean(car))

#plot pedestrians

hourlyLTNboundarypedestrian <- ggplot(hourlyLTNboundary, aes(x = hour, y = avg_pedestrian_count, color = year)) +
  geom_smooth(aes(group = year)) + 
  facet_wrap(~ LTNregion, nrow = 2, scales = "free_y") +
  labs(x = "Hour", y = "Average Pedestrian Count", title = "Comparison of Hourly Pedestrian Counts by Region", 
       subtitle = "This plot represents the total average number of pedestrian counts per hour by Region for all Viva City sensors at LTN boundary roads",
       caption = "Data Range: Jan 1, 2019 = July 14, 2023")  +
  scale_color_manual(values = c("blue", "red", "green", "purple",  "orange"))

# plot cyclists

hourlyLTNboundarycyclist <- ggplot(hourlyLTNboundary, aes(x = hour, y = avg_cyclist_count, color = year)) +
  geom_smooth(aes(group = year)) + 
  facet_wrap(~ LTNregion, nrow = 2, scales = "free_y") +
  labs(x = "Hour", y = "Average Cyclist Count", title = "Comparison of Hourly Cyclist Counts by Region", 
       subtitle = "This plot represents the total average number of cyclist counts per hour by Region for all Viva City sensors at LTN boundary roads",
       caption = "Data Range: Jan 1, 2019 = July 14, 2023")  +
  scale_color_manual(values = c("blue", "red", "green", "purple",  "orange"))

# plot cars

hourlyLTNboundarycar<- ggplot(hourlyLTNboundary, aes(x = hour, y = avg_car_count, color = year)) +
  geom_smooth(aes(group = year)) + 
  facet_wrap(~ LTNregion, nrow = 2, scales = "free_y") +
  labs(x = "Hour", y = "Average CarCount", title = "Comparison of Hourly Car Counts by Region", 
       subtitle = "This plot represents the total average number of carcounts per hour by Region for all Viva City sensors at LTN boundary roads",
       caption = "Data Range: Jan 1, 2019 = July 14, 2023")  +
  scale_color_manual(values = c("blue", "red", "green", "purple",  "orange"))

### ---------------------------------------------------------------------------
    # Comparison of Monthly Pedestrian / Cyclist / Car Counts at the different LTN locations in East Oxford
### ---------------------------------------------------------------------------

# plot pedestrians

monthlyLTNboundarypedestrian <- ggplot(hourlyLTNboundary, aes(factor(month), y = avg_pedestrian_count, color = year)) +
  geom_smooth(aes(group = year)) +
  facet_wrap(~ LTNregion, nrow = 2, scales = "free_y") +
  labs(x = "Month", y = "Average Pedestrian Count", title = "Comparison of Monthly Pedestrian Counts by Region", 
       subtitle = "This plot represents the total average number of pedestrian counts per month by Region for all Viva City sensors at LTN boundary roads",
       caption = "Data Range: Jan 1, 2019 = July 14, 2023")  +
  scale_color_manual(values = c("blue", "red", "green", "purple",  "orange"))

# plot cyclists

monthlyLTNboundarycyclist <- ggplot(hourlyLTNboundary, aes(factor(month), y = avg_cyclist_count, color = year)) +
  geom_smooth(aes(group = year)) +
  facet_wrap(~ LTNregion, nrow = 2, scales = "free_y") +
  labs(x = "Month", y = "Average Cyclist Count",
       title = "Comparison of Monthly Cyclist Counts by Region", 
       subtitle = "This plot represents the total average number of cyclist counts per month by Region for all Viva City sensors at LTN boundary roads",
       caption = "Data Range: Jan 1, 2019 = July 14, 2023")  +
  scale_color_manual(values = c("blue", "red", "green", "purple",  "orange"))

# plot cars 

monthlyLTNboundarycar <- ggplot(hourlyLTNboundary, aes(factor(month), y = avg_car_count, color = year)) +
  geom_smooth(aes(group = year)) +
  facet_wrap(~ LTNregion, nrow = 2, scales = "free_y") +
  labs(x = "Month", y = "Average Car Count", title = "Comparison of Monthly Car Counts by Region", 
       subtitle = "This plot represents the total average number of car counts per month by Region for all Viva City sensors at LTN boundary roads",
       caption = "Data Range: Jan 1, 2019 = July 14, 2023")  +
  scale_color_manual(values = c("blue", "red", "green", "purple",  "orange"))

### ---------------------------------------------------------------------------
# Comparison of Hourly Pedestrian / Cyclist / Car Counts at LTN Areas in East Oxford
### ---------------------------------------------------------------------------

# filter for LTN Area

hourlyLTNArea <- ltnArea %>% 
  mutate(year = format(date, "%Y"), 
         month = format(date, "%m"),
         hour = factor(substr(time_from, 1, 2), levels = sprintf("%02d", 0:23))) %>% 
  mutate(month = as.numeric(month)) %>% 
  group_by(year, month, hour) %>%
  summarise(avg_pedestrian_count = mean(pedestrian),
            avg_cyclist_count = mean(cyclist),
            avg_car_count = mean(car))

# plot pedestrian

hourlyLTNpedestrian <- ggplot(hourlyLTNArea, aes(x = hour, y = avg_pedestrian_count, color = year)) +
  geom_smooth(aes(group = year)) +
  labs(x = "Hour", y = "Average Pedestrian Count", title = "Comparison of Hourly Pedestrian Counts", 
       subtitle = "This plot represents the total average number of pedestrian counts per hour for all Viva City sensors in LTN Areas",
       caption = "Data Range: Jan 1, 2019 = July 14, 2023")  +
  scale_color_manual(values = c("blue", "red", "green", "purple",  "orange"))

# plot cyclist

hourlyLTNcyclist <- ggplot(hourlyLTNArea, aes(x = hour, y = avg_cyclist_count, color = year)) +
  geom_smooth(aes(group = year)) +
  labs(x = "Hour", y = "Average Cyclist Count", title = "Comparison of Hourly Cyclist Counts", 
       subtitle = "This plot represents the total average number of cyclist counts per hour for all Viva City sensors in LTN Areas",
       caption = "Data Range: Jan 1, 2019 = July 14, 2023")  +
  scale_color_manual(values = c("blue", "red", "green", "purple",  "orange"))

# plot car

hourlyLTNcar <- ggplot(hourlyLTNArea, aes(x = hour, y = avg_car_count, color = year)) +
  geom_smooth(aes(group = year)) +
  labs(x = "Hour", y = "Average Car Count", title = "Comparison of Hourly Car Counts", 
       subtitle = "This plot represents the total average number of car counts per hour for all Viva City sensors in LTN Areas",
       caption = "Data Range: Jan 1, 2019 = July 14, 2023")  +
  scale_color_manual(values = c("blue", "red", "green", "purple",  "orange"))

### ---------------------------------------------------------------------------
# Comparison of Monthly Pedestrian / Cyclist / Car Counts at LTN Areas in East Oxford
### ---------------------------------------------------------------------------

monthlyLTNpedestrian <- ggplot(hourlyLTNArea, aes(factor(month), y = avg_pedestrian_count, color = year)) +
  geom_smooth(aes(group = year)) +
  labs(x = "Month", y = "Average Pedestrian Count", title = "Comparison of Monthly Pedestrian Counts", 
       subtitle = "This plot represents the total average number of pedestrian counts per month for all Viva City sensors in LTN Areas",
       caption = "Data Range: Jan 1, 2019 = July 14, 2023")  +
  scale_color_manual(values = c("blue", "red", "green", "purple",  "orange"))

monthlyLTNcyclist <- ggplot(hourlyLTNArea, aes(factor(month), y = avg_cyclist_count, color = year)) +
  geom_smooth(aes(group = year)) +
  labs(x = "Month", y = "Average Cyclist Count", title = "Comparison of Monthly Cyclist Counts", 
       subtitle = "This plot represents the total average number of cyclist counts per month for all Viva City sensors in LTN Areas",
       caption = "Data Range: Jan 1, 2019 = July 14, 2023")  +
  scale_color_manual(values = c("blue", "red", "green", "purple",  "orange"))

monthlyLTNcar <- ggplot(hourlyLTNArea, aes(factor(month), y = avg_car_count, color = year)) +
  geom_smooth(aes(group = year)) +
  labs(x = "Month", y = "Average Car Count", title = "Comparison of Monthly Car Counts", 
       subtitle = "This plot represents the total average number of car counts per month for all Viva City sensors in LTN Areas",
       caption = "Data Range: Jan 1, 2019 = July 14, 2023")  +
  scale_color_manual(values = c("blue", "red", "green", "purple",  "orange"))




