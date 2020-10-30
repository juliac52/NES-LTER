#PCA of yearly averages
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
nes.pca <- prcomp(~ C.typicus + C.finmarchicus + Pseudocalanus + T.longicornis + Larvaceans + Paracalanus, data = yearly_averages, scale = TRUE)
biplot(nes.pca, xlabs = yearly_averages$Year, cex = 0.4, main = "Zooplankton PCA (All Year)")

#PCA of spring and fall
nes.pca.spring <- prcomp(~ C.typicus + C.finmarchicus + Pseudocalanus + T.longicornis + Larvaceans + Paracalanus, data = spring_yearly_averages, scale = TRUE)
biplot(nes.pca.spring, xlabs = spring_yearly_averages$Year, cex = 0.4, main = "Zooplankton PCA (Spring)")

nes.pca.fall <- prcomp(~ C.typicus + C.finmarchicus + Pseudocalanus + T.longicornis + Larvaceans + Paracalanus, data = fall_yearly_averages, scale = TRUE)
biplot(nes.pca.fall, xlabs = fall_yearly_averages$Year, cex = 0.4, main = "Zooplankton PCA (Fall)")

#make data frame with PCA results
pca <- data.frame(nes.pca$x)
pca$Year <- 1977:2015
#separate positive and negative PC values 
posval1 <- pca[pca$PC1>0,]                                        
negval1 <- pca[pca$PC1<0,]                                        
posval2 <- pca[pca$PC2>0,]                                        
negval2 <- pca[pca$PC2<0,] 
#make plots showing regime shifts for PC1 and PC2
plot(pca$Year, pca$PC1, type = "n", main = "Zooplankton Regime Shifts PC1", ylab = "PC1", xlab = "year")
points(posval1$Year, posval1$PC1, lwd = 9, type = "h", col = "purple4")
points(negval1$Year, negval1$PC1, lwd = 9, type = "h", col = "slategray3")

plot(pca$Year, pca$PC2, type = "n", main = "Zooplankton Regime Shifts PC2", ylab = "PC2", xlab = "year")
points(posval2$Year, posval2$PC2, lwd = 9, type = "h", col = "purple4")
points(negval2$Year, negval2$PC2, lwd = 9, type = "h", col = "slategray3")

