#install packages
library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)

## Lood Data
viva1923 <- read.csv("/Users/seandube/Desktop/TRL Disseration HUB/Data/Traffic Related/Viva CIty/viva1923.csv")

## WRANGLE
newViva1923 <- viva1923 %>% 
  clean_names() %>% 
  na.omit() 

specific_char <- ","
char_position <- regexpr(specific_char, newViva1923$location)

finalViva <- newViva1923 %>% 
  mutate(date = as_date(substring(time_from, 1, 10))) %>% 
  mutate(time_from = substring(time_from, 12, 16)) %>% 
  mutate(time_to = substring(time_to, 12, 16)) %>% 
  mutate(location = str_extract(location, paste0("(?<=\\", "[", ").*?(?=\\", "]", ")"))) %>% 
  mutate(location = gsub("\\[", "", location)) %>% 
  mutate(latitude = sub(".*,", "", location)) %>% 
  mutate(longitude = sub(",.*", "", location)) %>% 
  select(date, time_from, time_to, countline_name, latitude, longitude, cyclist, motorbike, car, pedestrian, taxi, van, minibus, bus)
## save 
save(finalViva, file = "/Users/seandube/Desktop/TRL Disseration HUB/Data/TRL R DATA FILES/finalViva.RData")

##----------------------------------------------------------------        
# filter for Viva City sensors locations for east oxford in LTN area and boundarys
##------------------------------------------------------------------

## MORRELL AND DIVINITY BOUNDARY ROADS

morrell1923 <- finalViva[str_detect(finalViva$countline_name, "morrell"),]
warneford1923 <- finalViva[str_detect(finalViva$countline_name, "warneford"),]

divinityBoundary1923 <- rbind(morrell1923, warneford1923) %>% 
  mutate(LTNregion = "Morrell")

## ST CLEMENTS BOUNDARY ROAD

stclements1923 <- finalViva[str_detect(finalViva$countline_name, "Cowley_road_north"), ] %>% 
  mutate(LTNregion = "St Clements")

## LEOPOLD X ST CLEMENTS 

leopold1923 <- finalViva[str_detect(finalViva$countline_name, "OX44_Cowley_Rd_East"), ] %>% 
  mutate(LTNregion = "Cowley Road East")

# IFFLEY BOUNDARY ROAD

iffley1923 <- finalViva[str_detect(finalViva$countline_name, "Iffley_Rd_North"), ] %>% 
  mutate(LTNregion = "Iffley ")

### CREATE BOUNDARY LTN DF AND SAVE 

boundaryLTNs <- rbind(divinityBoundary1923, stclements1923, leopold1923, iffley1923)
save(boundaryLTNs, file = "/Users/seandube/Desktop/TRL Disseration HUB/Data/TRL R DATA FILES/boundaryLTNs.RData")

# SENSOR AT DIVINITY ROAD IN THE LTN AREA 

LeopoldXDivinity <- finalViva[str_detect(finalViva$countline_name, "Leopold"), ] 
divinity <- finalViva[str_detect(finalViva$countline_name, "Divinity"), ]

ltnArea <- rbind(LeopoldXDivinity, divinity)
save(ltnArea, file = "/Users/seandube/Desktop/TRL Disseration HUB/Data/TRL R DATA FILES/ltnArea.RData")

#finalViva <- load("/Users/seandube/Desktop/TRL Disseration HUB/Data/TRL R DATA FILES/finalViva.RData")
#boundaryLTNs <- load("/Users/seandube/Desktop/TRL Disseration HUB/Data/TRL R DATA FILES/boundaryLTNs.RData")
#ltnArea <- load("/Users/seandube/Desktop/TRL Disseration HUB/Data/TRL R DATA FILES/ltnArea.RData")
 
