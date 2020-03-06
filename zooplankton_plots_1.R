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

