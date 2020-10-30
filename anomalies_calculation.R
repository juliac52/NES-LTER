#Preprocessing for NES EcoMon data 
library(vegan)
library(stats)
library(mgcv)
library(reshape2)
library(readxl)
library(lubridate)
library(sp)
library(maptools)
library(marmap)
library(rgeos)
#this is using Ryan Morse's code from EcoMon 

#Fix date, time
#use for binning into 1 degree bins for removal of undersampled bins
#ASSIGN EPU based on GPS data
## load shapefiles from EDAB EPU analysis ## not available here

library(sf)
library(sp)
library(rgdal)
library(maps)
library(dplyr)

#Okay now we have which ones are within the GOM 
#reload data frame in order to match 
setwd("/Users/juliacox/Desktop/NES-LTER/Zoop")
ZPD<-read.csv("NES.csv", header=TRUE)
ZPD <- ZPD[ZPD$lat>=39.5000 & ZPD$lat<41.33001 & ZPD$lon>=(-72.0000) & ZPD$lon<(-69.00001),]
#get rid of the numbering column
ZPD<-ZPD[,2:290]
#convert date column to a date object 
dt=as_date(ZPD$date, origin = "1899-12-30") 
DOY=yday(dt) #day of year
month=as.numeric(format(dt, '%m')) #extracting month values and convert month to numeric 
year=as.numeric(format(dt, '%Y')) #extracting year values and convert year to numeric 
ZPD$year=year
ZPD$month=month
ZPD$dt=dt

ZPD$DOY=DOY
ZPD$day=as.numeric(format(dt, '%d')) #*
ZPD$lat2=ceiling(ZPD$lat) #use for binning into 1 degree bins for removal of undersampled bins
ZPD$lon2=floor(ZPD$lon)#???why ceiling and floor on lat and lon? 

ZPD$epu<-"NES" #???why
#now back to following the code from Ryan Morse 

nms=data.frame(colnames(ZPD)) # column names of orginal data
# limit data set to zooplankton from 1977 on
#make sure that the biovolume of the zooplankton is numeric 
ZPD$volume_100m3<-as.numeric(ZPD$volume_100m3)
#change the last set to seq(14, 104) for values in 10m2
ZPDb=ZPD[,c(seq(1,14,1), seq(290,297,1), seq(106,197,1))] # check to make sure these are correct against 'nms' if data source changes!!!
#what are these rows? why are these particular rows being checked? shouldn't be hardcoded???

ZPDb=ZPDb[order(ZPDb$date),]
ZPDb=ZPDb[which(ZPDb$year > 1976),] # remove NA data in years prior to 1977
# Select only taxa present in yearly data > x percent of samples
X=10 # percent criteria to use as minimum percent in samples
ZPDa=ZPDb
ZPDa=ZPDa[!is.na(ZPDa$zoo_gear),] # Remove NA in zooplankton rows
# Reduce to taxa occurrance > x percent in samples
p.a=ZPDa[,23:114]
#???again, what are these arbitrary columns

#just used ctyp as an arbitray column to get rid of NA rows because all zoops show as NA 
#in NA rows
p.a<-p.a[!is.na(p.a$ctyp_100m3),]
#also remove the stupid NaN they left in the volume...
p.a<-p.a[!is.na(as.numeric(as.character(p.a$volume_100m3))),]
p.a[p.a > 0]=1 # presence/absence
count=colSums(p.a) #list of non-zero density values
pct=(count/dim(ZPDa)[1])*100 #calculating percentage of non-zero observations
crit=which(pct>X) #take all greater than X%. crit is just header names and column numbers
ZPDa=ZPDa[c(1:22,crit+22)]#???
#all runs smoothly so far.... 
cruises=unique(ZPDa$cruise_name) #removes redundant rows
for (i in 1:length(cruises)){
  ZPDa$medmonth[ZPDa$cruise_name == cruises[i]] = median(ZPDa$DOY[ZPDa$cruise_name == cruises[i]])
}#takes median day of the year cruise happened on and adds it to new column ZPDa$medmonth
ZPDa$bmm=NA
ZPDa$bmm[which(as.integer(ZPDa$medmonth) %in% seq(0,59))]=1
ZPDa$bmm[which(as.integer(ZPDa$medmonth) %in% seq(60,120))]=3
ZPDa$bmm[which(as.integer(ZPDa$medmonth) %in% seq(121,181))]=5
ZPDa$bmm[which(as.integer(ZPDa$medmonth) %in% seq(182,243))]=7
ZPDa$bmm[which(as.integer(ZPDa$medmonth) %in% seq(244,366))]=9

ZPDa<-ZPDa[!is.na(ZPDa$bmm),]

ZPDa[,14]=as.numeric(ZPDa[,14])
ZPDa[,23]=as.numeric(ZPDa[,23])
ZPDsave=ZPDa #from above routine, Yearly (all data) 

SEASON='Yearly'
ZPDa=ZPDsave
# LOG transform data using ZPDa from above (select season first)
test=log10(ZPDa[,23:62]+1) #choose columns with zooplankton data
ZPDlog=ZPDa
ZPDlog[,23:62]=test
nm=matrix(colnames(ZPDlog))

area='NES'
#ZPDlog$epu<-as.numeric(ZPDlog$epu)
#ZPDlog$EPU<-NULL
#for (i in 1:20642) {ZPDlog$EPU<-ifelse(ZPDlog$epu[i]==2, ZPDlog$EPU[i]=="GOM", ZPDlog$EPU[i]=="NA")}

gom.yr.spln=data.frame()
for (i in 23:62){
  num=i
  name=nm[num,1]
  mean.loc.x=aggregate(ZPDlog[which(ZPDlog$epu==area),num], by=list(ZPDlog$bmm[which(ZPDlog$epu==area)]), FUN=mean, na.rm=T)
  func = splinefun(mean.loc.x[,1], y=mean.loc.x[,2], method="natural",  ties = mean)
  x.daily=func(seq(1, 12, 0.0302)) #365 days
  gom.yr.spln=rbind(gom.yr.spln,x.daily)
}
gom.yr.spln=t(gom.yr.spln)
rownames(gom.yr.spln)=seq(1:365); colnames(gom.yr.spln)=nm[23:62,1]


gom.anom=ZPDlog[which(ZPDlog$epu=='NES'),]
gom.anom.b=data.frame(matrix(NA, nrow = dim(gom.anom)[1], ncol = dim(gom.anom)[2]))
for (i in 1:dim(gom.anom)[1]){
  gom.anom.b[i,23:62]=gom.anom[i,23:62]-gom.yr.spln[which(gom.anom$DOY[i]==rownames(gom.yr.spln)),]
}
gom.yr.anom=aggregate(gom.anom.b, by=list(gom.anom$year), FUN=mean, na.rm=T); rownames(gom.yr.anom)=gom.yr.anom[,1]; gom.yr.anom[,1]=NULL; colnames(gom.yr.anom)=colnames(gom.anom)


lgtx=c(25) #column for Calanus finmarchicus
dataTlg=gom.yr.anom[,lgtx]
GOM_Cal_Anom<-NULL
GOM_Cal_Anom$Cal_Anom<-dataTlg
GOM_Cal_Anom$Year<-row.names(gom.yr.anom)
lgtx=c(25,35) #column for Calanus finmarchicus and metridia lucens
dataTlg=gom.yr.anom[,lgtx]
Cal=gom.yr.anom[,25]
Met=gom.yr.anom[,35]
GOM_Cal_Anom$Cal_Met_anom<-rowMeans(dataTlg)
GOM_Cal_Anom$Cal_minus_Met<-Cal-Met
GOM_Cal_Anom$Cal_Plus_Met<-Cal+Met
GOM_Cal_Anom$Spring_Zoops_Dens<-gom.yr.anom[,23]
smtx=c(26,24,28,27)
GOM_Cal_Anom$CSI<-Cal-rowMeans(gom.yr.anom[,smtx])

GOM_Cal_Anom<-as.data.frame(GOM_Cal_Anom)

