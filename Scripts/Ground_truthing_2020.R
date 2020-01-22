setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

packs<-c("RStoolbox","raster","ggplot2","rgdal","viridis")
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

## Mask Urok with bathymetry
mask_urok<-crop(bat,Urok)
plotRGB(S2a_Urok,3,2,1,stretch = "lin")
plot(mask_urok)



## check the extents of the two layers -- if they are different crop both datasets
if (extent(S2a_Urok) == extent(mask_urok)){
  print("Extents are the same, no need to crop")
} else {
  # calculate overlap between the two datasets
  overlap <- intersect(extent(S2a_Urok), extent(mask_urok))
  # now let's crop both datasets to the overlap region
  S2a_Urok_c <- crop(S2a_Urok, overlap)
  mask_urok_c <- crop(mask_urok, overlap)
  if (extent(S2a_Urok_c) == extent(mask_urok_c)){
    print("Extents are different, data cropped and solved")
  } else {
    # resample batim using sat image
    mask_urok_r<-resample(mask_urok_c,S2a_Urok_c,method="bilinear")
    if (extent(S2a_Urok_c)==extent(mask_urok_r)){
      print("Cropping failed. Batim resampled, solved. Use mask_urok_r to mask S2a_Urok_c")
    } else {
      print("Failed, better try fishing")
    }
  }
  
}
Intertidal_urok<-mask(S2a_Urok_c,mask_urok_r) ### This mask is no good, has too much water
plotRGB(Intertidal_urok,3,2,1,stretch="lin")


##
sa_a<-c(S2a_Urok,S2a_Bub)
sa_b<-c(S2b_CanhGa,S2b_Bolama)
names_a<-c("Urok","Bub")
names_b<-c("CanhGa","Bolama")

## Mask with NDWI - this is better for now

###Urok
ndwi_Urok<-(S2a_Urok$B03-S2a_Urok$B08)/(S2a_Urok$B03+S2a_Urok$B08)
plot(ndwi_Urok)
hist(ndwi_Urok, n=1000, freq=T,axes=F)
axis(1,at=c(seq(-0.8,0.8,0.05)))
axis(2)

ndwi_Urok_F<-cut(ndwi_Urok,breaks=c(-2,-.30,.20,2))
plot(ndwi_Urok_F)
table(ndwi_Urok_F@data@values)

mask_urok1<-ndwi_Urok_F==2
plot(mask_urok1)
mask_urok1[mask_urok1==0]<-NA
plot(mask_urok1,colNA=1)

Intertidal_urok1<-S2a_Urok*mask_urok1
plotRGB(Intertidal_urok1,3,2,1,stretch = "lin")

###Bub
ndwi_Bub<-(S2a_Bub$B03-S2a_Bub$B08)/(S2a_Bub$B03+S2a_Bub$B08)
plot(ndwi_Bub)
hist(ndwi_Bub, n=1000, freq=T,axes=F)
axis(1,at=c(seq(-0.8,0.8,0.05)))
axis(2)

ndwi_Bub_F<-cut(ndwi_Bub,breaks=c(-2,-.425,.15,2))
plot(ndwi_Bub_F)
table(ndwi_Bub_F@data@values)

mask_Bub1<-ndwi_Bub_F==2
plot(mask_Bub1)
mask_Bub1[mask_Bub1==0]<-NA
plot(mask_Bub1,colNA=1)

Intertidal_Bub1<-S2a_Bub*mask_Bub1
plotRGB(Intertidal_Bub1,3,2,1,stretch = "lin")

###CanhGa
ndwi_CanhGa<-(S2b_CanhGa$B03-S2b_CanhGa$B08)/(S2b_CanhGa$B03+S2b_CanhGa$B08)
plot(ndwi_CanhGa)
hist(ndwi_CanhGa, n=1000, freq=T,axes=F)
axis(1,at=c(seq(-0.8,0.8,0.05)))
axis(2)

ndwi_CanhGa_F<-cut(ndwi_CanhGa,breaks=c(-2,-.30,.20,2))
plot(ndwi_CanhGa_F)
table(ndwi_CanhGa_F@data@values)

mask_CanhGa1<-ndwi_CanhGa_F==2
plot(mask_CanhGa1)
mask_CanhGa1[mask_CanhGa1==0]<-NA
plot(mask_CanhGa1,colNA=1)

Intertidal_CanhGa1<-S2b_CanhGa*mask_CanhGa1
plotRGB(Intertidal_CanhGa1,3,2,1,stretch = "lin")

###Bolama
ndwi_Bolama<-(S2b_Bolama$B03-S2b_Bolama$B08)/(S2b_Bolama$B03+S2b_Bolama$B08)
plot(ndwi_Bolama)
hist(ndwi_Bolama, n=1000, freq=T,axes=F)
axis(1,at=c(seq(-0.8,0.8,0.05)))
axis(2)

ndwi_Bolama_F<-cut(ndwi_Bolama,breaks=c(-2,-.35,.20,2))
plot(ndwi_Bolama_F)
table(ndwi_Bolama_F@data@values)

mask_Bolama1<-ndwi_Bolama_F==2
plot(mask_Bolama1)
mask_Bolama1[mask_Bolama1==0]<-NA
plot(mask_Bolama1,colNA=1)

Intertidal_Bolama1<-S2b_Bolama*mask_Bolama1
plotRGB(Intertidal_Bolama1,3,2,1,stretch = "lin")
