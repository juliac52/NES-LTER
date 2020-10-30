library(ggplot2)
calfin_plot <- ggplot(nes_zoo_wide, aes(x=date, y=calfin_10m2)) + geom_smooth() #Calanus finmarchicus 
ctyp_plot <- ggplot(nes_zoo_wide, aes(x=date, y=ctyp_10m2)) + geom_smooth() #Centropoges typicus 
ggplot(nes_zoo_wide, aes(x=date, y=pseudo_10m2)) + geom_smooth() 
ggplot(nes_zoo_wide, aes(x=date, y=tlong_10m2)) + geom_smooth() 
ggplot(nes_zoo_wide, aes(x=date, y=larvaceans_10m2)) + geom_smooth() 
ggplot(nes_zoo_wide, aes(x=date, y=para_10m2)) + geom_smooth()  
#simple plots for taxa of interest 

zooplot_combine <- ggplot(data=nes_zoo_long,
                    aes(x=date, y=organisms_per_10m2, colour=taxa))  + geom_smooth() +
                    theme_classic(base_size = 15) +labs(y= expression("Organisms per 10m"^{2}), x = "Date") 
#make time series plot of all six taxa 

zooplot_combine +  scale_colour_discrete(name="Taxa",
                      breaks=c("calfin_10m2", "ctyp_10m2", "pseudo_10m2","tlong_10m2","larvaceans_10m2","para_10m2"),
                      labels=c("C. finmarchicus", "C. typicus", "Pseudocalanus sp.","T. longicornis","Larvaceans","Paracalanus sp."))
#edit legend

ggplot(data=nes_zoo_long,
  aes(x=date, y=organisms_per_10m2, colour=taxa))  + geom_density2d() +
  theme_classic(base_size = 15) +labs(y= expression("Organisms per 10m"^{2}), x = "Date") 
#add superscript to axis label

ggplot(data = as_tibble(group_tags_wide), mapping = aes(x=nes_zoo_wide$calfin_10m2)) + 
  geom_bar(fill="bisque",color="white",alpha=0.7) + 
  stat_count(geom="text", aes(label=sprintf("%.4f",..count../length(group_tags))), vjust=-0.5) +
  labs(x='Zooplankton per 10m2') +
  theme_minimal() 
#tried to make binned spatial plot, didn't work 

ggplot(data=calfin,
       aes(x=date, y=organisms_per_10m2, colour=bins)) + geom_smooth(method = loess, span = 0.1, data = subset (nes_zoo_long,taxa == "tlong_10m2"), stat = "identity")+
  theme_classic(base_size = 15) +labs(y= expression("Organisms per 10m"^{2}), x = "Date") 
#tried plotting taxa densities by latitudinal bins, data seems to be too spiky

ggplot(data=nes_zoo_long,
      aes(x=bins, y=organisms_per_10m2, fill=taxa)) + geom_bar(stat = "identity")+
  theme_classic(base_size = 15) +labs(y= expression("Organisms per 10m"^{2}), x = "Latitude") 
#rough offshore onshore stacked bar plot of taxa densities 

#onshore offshore stacked bar plots of taxa densities by regime 
ggplot(data=sub_77_88,
       aes(x=bins, y=organisms_per_10m2, fill=taxa)) + geom_bar(stat = "identity")+
  theme_classic(base_size = 15) +labs(y= expression("Organisms per 10m"^{2}), x = "Latitude") +
  coord_flip() + scale_x_discrete(limits = rev(levels(sub_77_88$lat)))
ggplot(data=sub_89_94,
       aes(x=bins, y=organisms_per_10m2, fill=taxa)) + geom_bar(stat = "identity")+
  theme_classic(base_size = 15) +labs(y= expression("Organisms per 10m"^{2}), x = "Latitude") +
  coord_flip() + scale_x_discrete(limits = rev(levels(sub_89_94$lat)))
ggplot(data=sub_95_02,
       aes(x=bins, y=organisms_per_10m2, fill=taxa)) + geom_bar(stat = "identity")+
  theme_classic(base_size = 15) +labs(y= expression("Organisms per 10m"^{2}), x = "Latitude") +
  coord_flip() + scale_x_discrete(limits = rev(levels(sub_89_94$lat)))
ggplot(data=sub_03_15,
       aes(x=bins, y=organisms_per_10m2, fill=taxa)) + geom_bar(stat = "identity")+
  theme_classic(base_size = 15) +labs(y= expression("Organisms per 10m"^{2}), x = "Latitude", title = "2003-2015") +
  coord_flip() + scale_x_discrete(limits = rev(levels(sub_89_94$lat))) 

#rough plot of temperature vs. taxa density 
ggplot(data=calfin,
       aes(x=sfc_temp, y=organisms_per_10m2, colour=taxa)) + geom_count(stat = "identity")+
  theme_classic(base_size = 15) +labs(y= expression("Organisms per 10m"^{2}), x = "Surface Temp") 

#plot of one taxa's density by month over time series 
ggplot(data=larv,
       aes(x=year, y=organisms_per_10m2, colour=month)) + geom_bar(stat = "identity")+
  theme_classic(base_size = 15) +labs(y= expression("Organisms per 10m"^{2}), x = "Date") 

ggplot(data=lat5,
       aes(x=date, y=organisms_per_10m2, colour=taxa))  + geom_smooth() +
  theme_classic(base_size = 15) +labs(y= expression("Organisms per 10m"^{2}), x = "Date")


