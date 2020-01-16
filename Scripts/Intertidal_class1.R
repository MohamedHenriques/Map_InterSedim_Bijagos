setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

library(rgdal)
library(RStoolbox)
library(raster)
library(randomForest)
library(ggplot2)
library(rgeos)
library(maptools)
library(cluster)
library(rgdal)
library(sentinel2)
library(raster)


#Prepara imagens satelite

##List files for sentinel 2a image import
files<-dir(path="D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Sat_img/s2/S2A_MSIL2A_20190316T112111_N0211_R037_T28PCT_20190316T140635.SAFE",pattern=glob2rx("*_B*10m*.jp2*"),recursive=T,full.names = T)

## Import images with native 10m resolution
S2_10<-stack(files)
str(S2_10)

##Check image
projection(S2_10)
nlayers(S2_10)
names(S2_10)
names(S2_10)<-c("B02_10","B03_10","B04_10","B08_10") #Change image names

plotRGB(S2_10,3,2,1,stretch="lin")

## Import images resampled to 10m with SNAP
S2_resamp<-stack("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Sat_img/s2/resampled/S2A_MSIL2A_20190316T1121_resampled10_subset5_6_7_8a_9_11_12.tif")

## Check image
projection(S2_resamp)
nlayers(S2_resamp)
names(S2_resamp)
names(S2_resamp)<-c("B05_10","B06_10","B07_10","B08a_10","B09_10","B11_10","B12_10")

## Merge all bands together
S2_20190316<-stack(S2_10,S2_resamp)
S2_20190316


## load batimetry raster from catalao 
batim<-raster("D:/Work/FCUL/Doutoramento/Capitulos/Mapping_intertidal_sediments/Jcatalao/bijagos_batim.tif")
batim
ggR(batim)

## check the extents of the two layers -- if they are different crop both datasets
if (extent(S2_20190316) == extent(batim)){
  print("Extents are the same, no need to crop")
} else {
  # calculate overlap between the two datasets
  overlap <- intersect(extent(S2_20190316), extent(batim))
  # now let's crop both datasets to the overlap region
  S2_20190316_c <- crop(S2_20190316, overlap)
  batim_c <- crop(batim, overlap)
  if (extent(S2_20190316_c) == extent(batim_c)){
    print("Extents are different, data cropped and solved")
  } else {
    # resample batim using sat image
    batim_r<-resample(batim_c,S2_20190316_c, method="bilinear")
    if (extent(S2_20190316_c)==extent(batim_r)){
      print("Cropping failed. Batim resampled, solved. Use batim_r to mask S2_20190316_c")
    } else {
      print("Failed, try another thing")
    }
  }
  
}

S2_20190316_crop<-crop(S2_20190316,batim)
plotRGB(S2_20190316_crop,3,2,1,stretch="lin")
extent(S2_20190316_crop)==extent(batim))



batim_r
ggR(batim_r)
ggRGB(S2_20190316,3,2,1,stretch="lin")
plotRGB(S2_20190316_c,3,2,1,stretch="lin")

S2_20190316_cm<-mask(S2_20190316_c,batim_r)

Adon_b<-readOGR("./Shapefiles/Adonga_bancos/poligono1.shp")
  
