# set up cut-off values 
breaks <- c(39.5,39.683,39.866,40.049,40.232,40.415,40.598,40.781,40.964,41.147,41.33)
# specify interval/bin labels
tags <- c("[39.5-39.683", "[39.683-39.866)", "[39.866-40.049)", "[40.049-40.232)", "[40.232-40.415)", "[40.415-40.598)","[40.598-40.781)", "[40.781-40.964)","[40.964-41.147)","[41.147-41.33")
# bucketing values into bins
group_tags_long <- cut(nes_zoo_long$lat, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)
# inspect bins
summary(group_tags)

group_tags_wide <- cut(nes_zoo_wide$lat, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags, mean)

nes_zoo_long$bins <- cut(nes_zoo_long$lat, breaks=breaks, labels=tags) 
#add column to nes_zoo_long_ymd denoting latitudinal bins 

#-----------------------------------------------------------------------

group_tags_long_ymd <- cut(nes_zoo_long_ymd$lat, 
                       breaks=breaks, 
                       include.lowest=TRUE, 
                       right=FALSE, 
                       labels=tags)

nes_zoo_long_ymd$bins <- cut(nes_zoo_long_ymd$lat, breaks=breaks, labels=tags) 
#-----------------------------------------------------------------------
#Regime shift binning:
#convert year column to numeric 
nes_zoo_long_ymd$year <- as.numeric(nes_zoo_long_ymd$year)
#break dataframe into PC1 and PC2 regime shift chunks 
breaksPC1 <- c(1976,1988,1994,2002,2015)
breaksPC2 <- c(1976,1996,2015)

tagsPC1 <- c("1977-1988","1989-1994","1995-2002","2003-2015")
tagsPC2 <- c("1977-1996","1997-2015")
#bucketing values into bins 
group_tagsPC1_long <- cut(nes_zoo_long_ymd$year, 
                       breaks=breaksPC1, 
                       include.lowest=FALSE, 
                       right=FALSE, 
                       labels=tagsPC1)

group_tagsPC2_long <- cut(nes_zoo_long_ymd$year,
                          breaks = breaksPC2,
                          include.lowest = FALSE,
                          right = FALSE,
                          labels = tagsPC2)
#add new columns to dataframe 
nes_zoo_long_ymd$PC1_year_bins <- cut(nes_zoo_long_ymd$year, breaks=breaksPC1, labels=tagsPC1) 
nes_zoo_long_ymd$PC2_year_bins <- cut(nes_zoo_long_ymd$year, breaks=breaksPC2, labels=tagsPC2) 

#subset dataframe based on regime shifts
sub_77_88 <- subset(nes_zoo_long_ymd, PC1_year_bins == "1977-1988")
sub_89_94 <- subset(nes_zoo_long_ymd, PC1_year_bins == "1989-1994")
sub_95_02 <- subset(nes_zoo_long_ymd, PC1_year_bins == "1995-2002")
sub_03_15 <- subset(nes_zoo_long_ymd, PC1_year_bins == "2003-2015")
#
sub_77_96 <- subset(nes_zoo_long_ymd, PC1_year_bins == "1977-1996")
sub_97_15 <- subset(nes_zoo_long_ymd, PC1_year_bins == "1997-2015")
#-------------------------------------------------------------------
#PCA data frame restructuring 
#find annual averages
yearly_averages <- aggregate(nes_zoo_wide_ymd[, 17:22], list(nes_zoo_wide_ymd$year), mean, na.rm=TRUE)
#rename columns 
library(dplyr)
yearly_averages <- yearly_averages %>% 
                    rename(Year = Group.1,
                           "C.finmarchicus"=calfin_10m2,
                           "C.typicus"=ctyp_10m2,
                           "Pseudocalanus"=pseudo_10m2,
                           "T.longicornis"=tlong_10m2,
                           "Larvaceans"=larvaceans_10m2,
                           "Paracalanus"=para_10m2)
yearly_averages$Year <- as.numeric(yearly_averages$Year)
#Break into spring and fall
nes_zoo_wide_ymd$month <- as.numeric(nes_zoo_wide_ymd$month)
#spring (Feb-April)
springnes <- subset(nes_zoo_wide_ymd, month>=2 & month<5)
#fall data (Sep-Nov)
fallnes <- subset(nes_zoo_wide_ymd, month>=9 & month<12)
#---------------------------------------------------------
#Seasonal PCA
#find annual averages for spring and fall
spring_yearly_averages <- aggregate(springnes[, 17:22], list(springnes$year), mean, na.rm=TRUE)
fall_yearly_averages <- aggregate(fallnes[, 17:22], list(fallnes$year), mean, na.rm=TRUE)

#rename columns 
library(dplyr)
spring_yearly_averages <- spring_yearly_averages %>% 
  rename(Year = Group.1,
         "C.finmarchicus"=calfin_10m2,
         "C.typicus"=ctyp_10m2,
         "Pseudocalanus"=pseudo_10m2,
         "T.longicornis"=tlong_10m2,
         "Larvaceans"=larvaceans_10m2,
         "Paracalanus"=para_10m2)
fall_yearly_averages <- fall_yearly_averages %>% 
  rename(Year = Group.1,
         "C.finmarchicus"=calfin_10m2,
         "C.typicus"=ctyp_10m2,
         "Pseudocalanus"=pseudo_10m2,
         "T.longicornis"=tlong_10m2,
         "Larvaceans"=larvaceans_10m2,
         "Paracalanus"=para_10m2)

spring_yearly_averages$Year <- as.numeric(spring_yearly_averages$Year)
fall_yearly_averages$Year <- as.numeric(fall_yearly_averages$Year)
