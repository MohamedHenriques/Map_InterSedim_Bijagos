setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

packs<-c("RStoolbox","raster","ggplot2","rgdal")
lapply(packs,require,character.only=T)



## Load S2 image 
S2_20200105_a<-stack("D:/Work/FCUL/Doutoramento/Capitulos/Mapping_intertidal_sediments/SNAP/S2/Resampled/subset_S2B_MSIL2A_20200105T112349_N0213_R037_T28PCT_20200105T123330_resampled.tif")
S2_20200105_b<-stack("D:/Work/FCUL/Doutoramento/Capitulos/Mapping_intertidal_sediments/SNAP/S2/Resampled/subset_S2B_MSIL2A_20200105T112349_N0213_R037_T28PDT_20200105T123330_resampled.tif")

##Check image
names(S2_20200105_a)<-c("B02","B03","B04","B05","B06","B07","B08","B08a","B09","B11","B12")
names(S2_20200105_a)

names(S2_20200105_b)<-c("B02","B03","B04","B05","B06","B07","B08","B08a","B09","B11","B12")
names(S2_20200105_b)

ggRGB(S2_20200105_a,3,2,1,stretch = "lin")
ggRGB(S2_20200105_b,3,2,1,stretch = "lin")

###Poligonos areas de trabalho
Formosa<-

