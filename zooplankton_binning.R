# set up cut-off values 
breaks <- c(39.5,39.683,39.866,40.049,40.232,40.415,40.598,40.781,40.964,41.147,41.33)
# specify interval/bin labels
tags <- c("[39.5-39.683)", "[39.683-39.866)", "[39.866-40.049)", "[40.049-40.232)", "[40.232-40.415)", "[40.415-40.598)","[40.598-40.781)", "[40.781-40.964)","[40.964-41.147)","[41.147-41.33")
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

################################################################################################
#Bin into 2D bins by latitude and longitude 
library(ash)
#nbin <- c(8.5665,14.9915)
nbin<-c(8,8)
ab <- matrix( c(-5,-5,5,5), 2, 2) 
bins <- bin2(cbind(nes_zoo_long$lat,nes_zoo_long$lon),nbin,ab)
bins2D <- ash2(bins)
image(bins$x,bins$y)
#1. Create 2D bins - some sort of loop to choose values 
# range(nes_zoo_long$lat):39.6167 41.3283, 1.7116 degree range
# range(nes_zoo_long$lon):-71.9983 -69.0017, 2.9966 degree range
# 10 bins in the y axis, 0.17116 square degree bins, 16.91 bins in the x axis (2.9966/0.17116)
#2. Assign data to these bins 
GeoBin <- 	data.frame(
  bin_name = c(1:10), # cruise_name probably not needed 
  LatN = c(30,40,50,0,0,0,0,0,0,0), 
  LonW = c(60,70,80,0,0,0,0,0,0,0),
  LatS = c(90,91,92,0,0,0,0,0,0,0),
  LonE = c(0,0,0,0,0,0,0,0,0,0)
)
print(GeoBin)          # print the whole example data frame
print(GeoBin[3,2])     # just one element, indexing or slicing
#  GeoBin[column, row] 
print(GeoBin$LatN[3]) # same element but with column reference

for (i in GeoBin$bin_name) {
  print(i)
}
for (i in c(1:10)) {
  print(GeoBin$LonW[i])
}
for (bin in GeoBin$LatS) {
  print(bin)
}

GeoBin <- 	data.frame(
  bin_name = c(1:170), 
  LatN = c(0), 
  LonW = c(0),
  LatS = c(0),
  LonE = c(0)
)
#start at 39.6167 for LatN and add 0.17116 170 times
latNstart <- 39.6167
lonWstart <- -71.9983

LonW = lonWstart
for (i in GeoBin$bin_name) {
  LatN = latNstart
  for () {
    GeoBin$LatN[i] = LatN
    LatN = LatN + 0.17116
  }
  LonW = LonW + 0.17116 
}
               
#example of binning zooplankton 
Gridded_Zoops<-EcoMon %>% 
  mutate(binlon = cut(lon, seq(from = min(-72), to = max(-69), by = .25), include.lowest = T, right = F),
         binlat = cut(lat, seq(from = min(39.5), to = max(41.5), by = .25), include.lowest = T, right = F)) %>% 
  group_by(Year, binlat= forcats::fct_explicit_na(binlat), binlon=forcats::fct_explicit_na(binlon), .drop=FALSE) %>% 
  summarise(calfin_100m3_binned = mean(calfin_100m3, na.rm=TRUE), ctyp_100m3_binned=mean(ctyp_100m3, na.rm=TRUE), pseudo_100m3_binned=mean(pseudo_100m3, na.rm=TRUE), hyper_100m3_binned=mean(hyper_100m3, na.rm=TRUE),
            poly_100m3_binned=mean(poly_100m3, na.rm=TRUE), mlucens_100m3_binned=mean(mlucens_100m3, na.rm=TRUE), larvaceans_100m3_binned=mean(larvaceans_100m3, na.rm=TRUE),
            ammspp_100m3_binned=mean(ammspp_100m3, na.rm=TRUE), tlong_100m3_binned=mean(tlong_100m3, na.rm=TRUE))
