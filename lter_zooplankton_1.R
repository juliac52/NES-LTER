setwd("/Users/juliacox/Desktop/NES-LTER/Zoop")
library(tidyverse)
library(readxl)
EcoMon_Plankton_Data_v3_1_2_ <- read_excel("EcoMon_Plankton_Data_v3_1 (2).xlsx", sheet = "Data")
#import raw data file 

EcoMon <- EcoMon_Plankton_Data_v3_1_2_

install.packages("data.table")
library(data.table)
nes_lat <- setDT(EcoMon)[!(lat %between% c(0, 39.5) | lat %between% c(41.33, 50))]
#only include latitudes between 39.5 and 41.4 
range(nes_lat$lat)
#view range of latitudes to make sure it worked 
nes <- setDT(nes_lat)[!(lon %between% c(-69, 0) | lon %between% c(-76,-72))]
#only include longitudes between 69.5 and 71.5 
range(nes$lat)
range(nes$lon)
#view nes (northeast us shelf) lat and lon to make sure it worked 

nes_zoo_wide <- nes[,c("cruise_name","date","station","zoo_gear","ich_gear","lat","lon","time","depth","sfc_temp","sfc_salt","btm_temp","btm_salt","volume_1m2","calfin_10m2","ctyp_10m2","pseudo_10m2","tlong_10m2","larvaceans_10m2","para_10m2")]
#make a vector with desired columns 
#select only the six taxa of interest (calanus finmarchicus, centropages typicus, 
#pseudocalanus, paracalanus, temora longicornis, and larvacians)

library(tidyr)
nes_zoo_long <- gather(nes_zoo_wide, taxa, organisms_per_10m2, calfin_10m2:para_10m2, factor_key=TRUE)
#convert to long format 
install.packages("Amelia")
library("Amelia")
missmap(nes_zoo_long)
#see what's missing in data with missingness map 
plot(nes_zoo_wide$date, type = 'o', pch = '|', ylab = '')
#attempt to see what dates are missing, didn't work 

nes_zoo_wide_ymd <- nes_zoo_wide %>% separate(date, sep ="-", into = c("year", "month", "day"))
nes_zoo_long_ymd <- nes_zoo_long %>% separate(date, sep ="-", into = c("year", "month", "day"))
#separate date into year, month, and day 

range(nes_zoo_long$sfc_temp, na.rm = TRUE)

range(nes_zoo_long$date)

nes_zoo_wide_ymd$date <- nes_zoo_wide$date
nes_zoo_long_ymd$date <- nes_zoo_long$date
#add date column to ymd dataframes 

View(nes_zoo_long_ymd)

ctyp <- subset(nes_zoo_long_ymd, taxa=="ctyp_10m2")
calfin <- subset(nes_zoo_long_ymd, taxa=="calfin_10m2")
larv <- subset(nes_zoo_long_ymd, taxa=="larvaceans_10m2")
#make subset dataframes with only one taxa 

lat10 <- subset(nes_zoo_long_ymd, bins=="[41.147-41.33")
lat9 <- subset(nes_zoo_long_ymd, bins=="[40.964-41.147)")
lat8 <- subset(nes_zoo_long_ymd, bins=="[40.781-40.964)")
lat7 <- subset(nes_zoo_long_ymd, bins=="[40.598-40.781)")
lat6 <- subset(nes_zoo_long_ymd, bins=="[40.415-40.598)")
lat5 <- subset(nes_zoo_long_ymd, bins=="[40.232-40.415)")
lat4 <- subset(nes_zoo_long_ymd, bins=="[40.049-40.232)")
lat3 <- subset(nes_zoo_long_ymd, bins=="[39.866-40.049)")
lat2 <- subset(nes_zoo_long_ymd, bins=="[39.683-39.866)")

View(nes_zoo_long_ymd)
