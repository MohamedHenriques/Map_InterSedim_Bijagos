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
library(rasterVis)



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

S2_20190316_c
ggRGB(S2_20190316_c,3,2,1,stretch="lin")

batim_r
ggR(batim_r)

## Mask land and water
S2_20190316_cm<-mask(S2_20190316_c,batim_r)

S2_20190316_cm
ggR(S2_20190316_cm)
ggRGB(S2_20190316_cm,3,2,1,stretch="lin")


# Adonga study area

## Load poligon made in QGIS
Adon_b<-readOGR("./Shapefiles/Adonga_bancos/poligono1.shp")

GNB<-readOGR("./Shapefiles/GNB/gnb_poly.shp")

plot(GNB)
plot(Adon_b, add=T, col="red")

## Crop study area with that poligon
S2_20190316_ad<-crop(S2_20190316_cm,Adon_b)
ggRGB(S2_20190316_ad,3,2,1,stretch="lin")

# Load sentinel 1 image 20190310 in decibels

S1_VV_db<-stack("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Sat_img/s1/S1A_IW_GRDH_1SDV_20190310T1917_Orb_TN_Bdr_Cal_TC_Catalao_Sigma0_VV_db.tif")
S1_VV_db

names(S1_VV_db)<-c("B1","B2","B3","B4")
ggR(S1_VV_db[[1]])
hist(S1_VV_db[[1]], breaks=100)
rasterVis::levelplot(S1_VV_db)
rasterVis::densityplot(S1_VV_db)
splom(S1_VV_db)


S1_VH_db<-stack("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Sat_img/s1/S1A_IW_GRDH_1SDV_20190310T1917_Orb_TN_Bdr_Cal_TC_Catalao_Sigma0_VH_db.tif")
S1_VH_db  

names(S1_VH_db)<-c("B1","B2","B3","B4")
rasterVis::levelplot(S1_VH_db)
rasterVis::densityplot(S1_VH_db)

extent(S1_VV_db) == extent(S1_VH_db)

S1_VV_VH<-S1_VV_db$B1/S1_VH_db$B1
S1_VV_VH
rasterVis::levelplot(S1_VV_VH)