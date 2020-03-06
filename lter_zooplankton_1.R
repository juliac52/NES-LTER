setwd("/Users/juliacox/Desktop/NES-LTER/Zoop")
library(tidyverse)
library(readxl)
EcoMon_Plankton_Data_v3_1_2_ <- read_excel("EcoMon_Plankton_Data_v3_1 (2).xlsx", sheet = "Data")
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
#separate date into year, month, and day 


library(ggplot2)
ggplot(nes_zoo_wide, aes(x=date, y=calfin_10m2)) + geom_smooth() #Calanus finmarchicus 

getwd()


