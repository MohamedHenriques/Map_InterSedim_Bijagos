setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

packs<-c("RStoolbox","raster","ggplot2","rgdal")
lapply(packs,require,character.only=T)



## Load S2 images 
S2_20200105_a<-stack("D:/Work/FCUL/Doutoramento/Capitulos/Mapping_intertidal_sediments/SNAP/S2/Resampled/subset_S2B_MSIL2A_20200105T112349_N0213_R037_T28PCT_20200105T123330_resampled.tif")
S2_20200105_b<-stack("D:/Work/FCUL/Doutoramento/Capitulos/Mapping_intertidal_sediments/SNAP/S2/Resampled/subset_S2B_MSIL2A_20200105T112349_N0213_R037_T28PDT_20200105T123330_resampled.tif")

##Check images
names(S2_20200105_a)<-c("B02","B03","B04","B05","B06","B07","B08","B08a","B09","B11","B12")
names(S2_20200105_a)

names(S2_20200105_b)<-c("B02","B03","B04","B05","B06","B07","B08","B08a","B09","B11","B12")
names(S2_20200105_b)

plotRGB(S2_20200105_a,3,2,1,stretch = "lin")
plotRGB(S2_20200105_b,3,2,1,stretch = "lin")

##Load S1 SAR images




###Poligonos areas de trabalho
gnb<-readOGR(dsn="Shapefiles/GNB",layer="gnb_poly")
plot(gnb, col="grey25")

Urok<-readOGR(dsn="Shapefiles/Urok_shape",layer="Urok_shapes")
plot(Urok, col="red")

Bub<-readOGR(dsn="Shapefiles/Bubaque_shape",layer="Bubaque_shape")
plot(Bub, add=T,col="green")

CanhGa<-readOGR(dsn="Shapefiles/Canhabaque_Galinhas_shape",layer="Canhabaque_Galinhas")
plot(CanhGa,add=T,col="blue")

Bolama<-readOGR(dsn="Shapefiles/Bolama_shape",layer="Bolama_shape")
plot(Bolama, col="red", add=T)

#Crop for each study area
## S2 a
### Urok
S2a_Urok<-crop(S2_20200105_a,Urok)
S2a_Urok
plotRGB(S2a_Urok,3,2,1,stretch = "lin")

#writeRaster(S2a_Urok,"Data_out/S2a_Urok.tif",format="GTiff",overwrite=F)


###Bubaque
S2a_Bub<-crop(S2_20200105_a,Bub)
S2a_Bub
plotRGB(S2a_Bub,3,2,1,stretch = "lin")

#writeRaster(S2a_Bub,"Data_out/S2a_Bub.tif",format="GTiff",overwrite=F)

## S2 b
S2b_Urok<-crop(S2_20200105_b,Urok)
S2b_Urok
plotRGB(S2b_Urok,3,2,1,stretch = "lin")

#writeRaster(S2b_Urok,"Data_out/S2b_Urok.tif",format="GTiff",overwrite=F)


###Canhabaque galinhas
S2b_CanhGa<-crop(S2_20200105_b,CanhGa)
S2b_CanhGa
plotRGB(S2b_CanhGa,3,2,1,stretch = "lin")

#writeRaster(S2b_CanhGa,"Data_out/S2b_CanhGa.tif",format="GTiff",overwrite=F)

###Bolama
S2b_Bolama<-crop(S2_20200105_b,Bolama)
S2b_Bolama
plotRGB(S2b_Bolama,3,2,1,stretch = "lin")

#writeRaster(S2b_Bolama,"Data_out/S2b_Bolama.tif",format="GTiff",overwrite=F)

# Load bathymetry map for mask
bat<-raster("D:/Work/FCUL/Doutoramento/Capitulos/Mapping_intertidal_sediments/Jcatalao/bijagos_batim.tif")
plot(bat)


# Mask out every study area

## Mask Urok
mask_urok<-crop(bat,Urok)
plotRGB(S2a_Urok,3,2,1,stretch = "lin")
plot(mask_urok,add=T)

ndwi_Urok<-(S2a_Urok$B03-S2a_Urok$B08)/(S2a_Urok$B03+S2a_Urok$B08)
plot(ndwi_Urok)
hist(ndwi_Urok, n=1000, freq=T)
axis(1)

table(ndwi_Urok@data@values)


