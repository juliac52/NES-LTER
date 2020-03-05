library(tidyverse)
library(readxl)
EcoMon_Plankton_Data_v3_1_2_ <- read_excel("~/Desktop/NES-LTER/Zoop/EcoMon_Plankton_Data_v3_1 (2).xlsx", + sheet = "Data")
#import raw data file 

EcoMon <- EcoMon_Plankton_Data_v3_1_2_

install.packages("data.table")
library(data.table)
nes_lat <- setDT(EcoMon)[!(lat %between% c(0, 39.5) | lat %between% c(41.4, 50))]
#only include latitudes between 39.5 and 41.4 
range(nes_lat$lat)
#view range of latitudes to make sure it worked 
nes <- setDT(nes_lat)[!(lon %between% c(-69.5, 0) | lon %between% c(-76,-71.5))]
#only include longitudes between 69.5 and 71.5 
range(nes$lat)
range(nes$lon)
#view nes (northeast us shelf) lat and lon to make sure it worked 

