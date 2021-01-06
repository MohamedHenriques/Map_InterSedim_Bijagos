setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

packs<-c("sf","beepr","RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis","data.table","reshape2")
lapply(packs,require,character.only=T)

## Load exposure model
bat<-raster("D:/Work/FCUL/Doutoramento/Joao_Belo/Exposure_model/tempo_exposicao_Bijagos.tif")
crs(bat)

plot(bat)
