library(ggplot2)
calfin_plot <- ggplot(nes_zoo_wide, aes(x=date, y=calfin_10m2)) + geom_smooth() #Calanus finmarchicus 
ctyp_plot <- ggplot(nes_zoo_wide, aes(x=date, y=ctyp_10m2)) + geom_smooth() #Centropoges typicus 
ggplot(nes_zoo_wide, aes(x=date, y=pseudo_10m2)) + geom_smooth() 
ggplot(nes_zoo_wide, aes(x=date, y=tlong_10m2)) + geom_smooth() 
ggplot(nes_zoo_wide, aes(x=date, y=larvaceans_10m2)) + geom_smooth() 
ggplot(nes_zoo_wide, aes(x=date, y=para_10m2)) + geom_smooth()  
