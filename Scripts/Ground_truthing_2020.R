setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

packs<-c("RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis")
lapply(packs,require,character.only=T)

## Load S2 images 
S2_20200105_a<-stack("D:/Work/FCUL/Doutoramento/Capitulos/Mapping_intertidal_sediments/SNAP/S2/Resampled/subset_S2B_MSIL2A_20200105T112349_N0213_R037_T28PCT_20200105T123330_resampled.tif")
S2_20200105_b<-stack("D:/Work/FCUL/Doutoramento/Capitulos/Mapping_intertidal_sediments/SNAP/S2/Resampled/subset_S2B_MSIL2A_20200105T112349_N0213_R037_T28PDT_20200105T123330_resampled.tif")

##Check images
names(S2_20200105_a)<-c("B02","B03","B04","B05","B06","B07","B08","B08a","B09","B11","B12")
names(S2_20200105_a)

names(S2_20200105_b)<-c("B02","B03","B04","B05","B06","B07","B08","B08a","B09","B11","B12")
names(S2_20200105_b)

#plotRGB(S2_20200105_a,3,2,1,stretch = "lin")
#plotRGB(S2_20200105_b,3,2,1,stretch = "lin")

##Load S1 SAR images

# import radar images preprocessadas em SNAP. Realizamos orbit file correction, 
#thermal and border noise removal/reduction, radiometric calibration, Speckle noise filtering,
#Terrain correction e conversion to decibels

## Load VV and VH s1 image
S1_VH_VV<-stack("D:/Work/FCUL/Doutoramento/Capitulos/Mapping_intertidal_sediments/SNAP/S1/20200116_1917/S1A_IW_GRDH_1SDV_20200116T191708_20200116T191733_030830_038983_2062_Orb_NR_Cal_Spk_TC_db.tif")
S1_VH_VV
#plot(S1_VH_VV$S1A_IW_GRDH_1SDV_20200116T191708_20200116T191733_030830_038983_2062_Orb_NR_Cal_Spk_TC_db.1)

## change names of layers (I know the order because of the way I preprocessed it in SNAP)
names(S1_VH_VV)<-c("S1_VH","S1_VV")

# bathymetry map
bat<-raster("D:/Work/FCUL/Doutoramento/Capitulos/Mapping_intertidal_sediments/Jcatalao/bijagos_batim.tif")
#plot(bat)

#Poligonos areas de trabalho
gnb<-readOGR(dsn="Shapefiles/GNB",layer="gnb_poly")
#plot(gnb, col="grey25")

Urok<-readOGR(dsn="Shapefiles/Urok_shape",layer="Urok_shapes")
#plot(Urok, col="red")

Bub<-readOGR(dsn="Shapefiles/Bubaque_shape",layer="Bubaque_shape")
#plot(Bub, add=T,col="green")

CanhGa<-readOGR(dsn="Shapefiles/Canhabaque_Galinhas_shape",layer="Canhabaque_Galinhas")
#plot(CanhGa,add=T,col="blue")

Bolama<-readOGR(dsn="Shapefiles/Bolama_shape",layer="Bolama_shape")
#plot(Bolama, col="red", add=T)

#Crop for each study area
## S2 a
### Urok
S2a_Urok<-crop(S2_20200105_a,Urok)
S2a_Urok
#plotRGB(S2a_Urok,3,2,1,stretch = "lin")

#writeRaster(S2a_Urok,"Data_out/S2a_Urok.tif",format="GTiff",overwrite=F)

## radar image
###Urok
VV_VH_Urok<-crop(S1_VH_VV,Urok)
#plot(VV_VH_Urok)

## batimetry
### Urok
bat_Urok<-crop(bat,Urok)
#plot(bat_Urok)

###Bubaque
S2a_Bub<-crop(S2_20200105_a,Bub)
S2a_Bub
#plotRGB(S2a_Bub,3,2,1,stretch = "lin")

#writeRaster(S2a_Bub,"Data_out/S2a_Bub.tif",format="GTiff",overwrite=F)

## radar
### Bubaque
VV_VH_Bub<-crop(S1_VH_VV,Bub)
#plot(VV_VH_Bub)

## bathymetry
bat_Bub<-crop(bat,Bub)
#plot(bat_Bub)

## S2 b
S2b_Urok<-crop(S2_20200105_b,Urok)
S2b_Urok
#plotRGB(S2b_Urok,3,2,1,stretch = "lin")

### Join a and b images
#S2a_S2b_Urok<-mosaic(S2a_Urok,S2b_Urok,fun=min)
S2a_S2b_Urok<-merge(S2b_Urok,S2a_Urok,overlap=T)
#plotRGB(S2a_S2b_Urok,3,2,1,stretch="lin")
names(S2a_S2b_Urok)<-c("B02","B03","B04","B05","B06","B07","B08","B08a","B09","B11","B12")

writeRaster(S2a_S2b_Urok,"Data_out/S2a_S2b_Urok.tif",format="GTiff",overwrite=F)


## S2 b
S2b_Bub<-crop(S2_20200105_b,Bub)
S2b_Bub
#plotRGB(S2b_Bub,3,2,1,stretch = "lin")

### Join a and b images
#S2a_S2b_Bub<-mosaic(S2a_Bub,S2b_Bub,fun=min)
S2a_S2b_Bub<-merge(S2b_Bub,S2a_Bub,overlap=T)
#plotRGB(S2a_S2b_Bub,3,2,1,stretch="lin")
names(S2a_S2b_Bub)<-c("B02","B03","B04","B05","B06","B07","B08","B08a","B09","B11","B12")

writeRaster(S2a_S2b_Bub,"Data_out/S2a_S2b_Bub.tif",format="GTiff",overwrite=F)

###Canhabaque galinhas
S2b_CanhGa<-crop(S2_20200105_b,CanhGa)
S2b_CanhGa
#plotRGB(S2b_CanhGa,3,2,1,stretch = "lin")

#writeRaster(S2b_CanhGa,"Data_out/S2b_CanhGa.tif",format="GTiff",overwrite=F)

###### Can't do this yet because of the extent of the s1 image. will dela with that later on SNAP
### CanhGa
#VH_CanhGa<-crop(VH,CanhGa)
#VV_CanhGa<-crop(VV,CanhGa)
#plot(VH_CanhGa)
#plot(VV_CanhGa)

###Bolama
S2b_Bolama<-crop(S2_20200105_b,Bolama)
S2b_Bolama
#plotRGB(S2b_Bolama,3,2,1,stretch = "lin")

#writeRaster(S2b_Bolama,"Data_out/S2b_Bolama.tif",format="GTiff",overwrite=F)

###Bolama
#VH_Bolama<-crop(VH,Bolama)
#VV_Bolama<-crop(VV,Bolama)
#plot(VH_Bolama)
#plot(VV_Bolama)




## Mask with NDWI - this is better for now, after will try other index or new catalao way

###Urok
ndwi_Urok<-(S2a_S2b_Urok$B03-S2a_S2b_Urok$B08)/(S2a_S2b_Urok$B03+S2a_S2b_Urok$B08)
#plot(ndwi_Urok)
hist(ndwi_Urok, n=1000, freq=T,axes=F)
axis(1,at=c(seq(-0.8,0.8,0.05)))
axis(2)

ndwi_Urok_F<-cut(ndwi_Urok,breaks=c(-2,-.30,.20,2))
#plot(ndwi_Urok_F)
#table(ndwi_Urok_F@data@values)

mask_urok<-ndwi_Urok_F==2
#plot(mask_urok)
mask_urok[mask_urok==0]<-NA
#plot(mask_urok,colNA=1)

Intertidal_urok<-S2a_S2b_Urok*mask_urok
plotRGB(Intertidal_urok,3,2,1,stretch = "lin")







## Mask radar

## check the extents of the two layers -- if they are different crop both datasets
if (extent(VV_VH_Urok) == extent(mask_urok)){
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








## Mask bathymetry
### Urok
#### resample mask to allow use with bat image
mask_urok_bat_r<-resample(mask_urok,bat_Urok,method="bilinear")
#plot(mask_urok_bat_r,col=magma(2),colNA=1)
Intertidal_bat_urok<-bat_Urok*mask_urok_bat_r
#plot(Intertidal_bat_urok)

###Bub
ndwi_Bub<-(S2a_S2b_Bub$B03-S2a_S2b_Bub$B08)/(S2a_S2b_Bub$B03+S2a_S2b_Bub$B08)
#plot(ndwi_Bub)
hist(ndwi_Bub, n=1000, freq=T,axes=F)
axis(1,at=c(seq(-0.8,0.8,0.05)))
axis(2)

ndwi_Bub_F<-cut(ndwi_Bub,breaks=c(-2,-.425,.125,2))
#plot(ndwi_Bub_F)
#table(ndwi_Bub_F@data@values)

mask_Bub<-ndwi_Bub_F==2
#plot(mask_Bub)
mask_Bub[mask_Bub==0]<-NA
#plot(mask_Bub,colNA=1)

Intertidal_Bub<-S2a_S2b_Bub*mask_Bub
#plotRGB(Intertidal_Bub,3,2,1,stretch = "lin")

## bathymetry
### Bubaque
#### resample mask to allow use with bat image
mask_Bub_bat_r<-resample(mask_Bub,bat_Bub,method="bilinear")
#plot(mask_Bub_bat_r,col=magma(2),colNA=1)
Intertidal_bat_Bub<-bat_Bub*mask_Bub_bat_r
#plot(Intertidal_bat_Bub)


###CanhGa
ndwi_CanhGa<-(S2b_CanhGa$B03-S2b_CanhGa$B08)/(S2b_CanhGa$B03+S2b_CanhGa$B08)
#plot(ndwi_CanhGa)
#hist(ndwi_CanhGa, n=1000, freq=T,axes=F)
#axis(1,at=c(seq(-0.8,0.8,0.05)))
#axis(2)

ndwi_CanhGa_F<-cut(ndwi_CanhGa,breaks=c(-2,-.30,.20,2))
#plot(ndwi_CanhGa_F)
#table(ndwi_CanhGa_F@data@values)

mask_CanhGa<-ndwi_CanhGa_F==2
#plot(mask_CanhGa)
mask_CanhGa[mask_CanhGa==0]<-NA
#plot(mask_CanhGa,colNA=1)

Intertidal_CanhGa<-S2b_CanhGa*mask_CanhGa
#plotRGB(Intertidal_CanhGa,3,2,1,stretch = "lin")

###Bolama
ndwi_Bolama<-(S2b_Bolama$B03-S2b_Bolama$B08)/(S2b_Bolama$B03+S2b_Bolama$B08)
#plot(ndwi_Bolama)
#hist(ndwi_Bolama, n=1000, freq=T,axes=F)
#axis(1,at=c(seq(-0.8,0.8,0.05)))
#axis(2)

ndwi_Bolama_F<-cut(ndwi_Bolama,breaks=c(-2,-.35,.20,2))
#plot(ndwi_Bolama_F)
#table(ndwi_Bolama_F@data@values)

mask_Bolama<-ndwi_Bolama_F==2
#plot(mask_Bolama)
mask_Bolama[mask_Bolama==0]<-NA
#plot(mask_Bolama,colNA=1)

Intertidal_Bolama<-S2b_Bolama*mask_Bolama
#plotRGB(Intertidal_Bolama,3,2,1,stretch = "lin")






# stack s2, s1 and bat images
##Urok
all_Urok<-stack(Intertidal_urok,Intertidal_bat_urok)




# PCA

## S2a
beginCluster()
PCA_S2a_Urok<-rasterPCA(Intertidal_urok1,nSamples=NULL,nComp=nlayers(Intertidal_urok1),maskCheck=T)
endCluster()

summary(PCA_S2a_Urok$model)
loadings(PCA_S2a_Urok$model)
my.palette <- colorspace::rainbow_hcl(100)

plotRGB(PCA_S2a_Urok$map,r=1,g=2,b=3,axes=T,colNA="black",stretch="lin",main="PCA S2a Urok 20200105",col=magma(50))
writeRaster(PCA_S2a_Urok$map,"Data_out/PCA_Urok_S2a_20200105.tif",format="GTiff",overwrite=F)

## S2a
beginCluster()
PCA_S2a_Bub<-rasterPCA(Intertidal_Bub,nSamples=NULL,nComp=nlayers(Intertidal_Bub),maskCheck=T)
endCluster()

summary(PCA_S2a_Bub$model)
#my.palette <- colorspace::rainbow_hcl(100)

plotRGB(PCA_S2a_Bub$map,r=1,g=2,b=3,axes=T,colNA="black",stretch="lin",main="PCA S2a Bub 20200105",col=magma(20))
writeRaster(PCA_S2a_Bub$map,"Data_out/PCA_Bub_S2a_20200105.tif",format="GTiff",overwrite=F)


## S2b
beginCluster()
PCA_S2b_CanhGa<-rasterPCA(Intertidal_CanhGa,nSamples=NULL,nComp=nlayers(Intertidal_CanhGa),maskCheck=T)
endCluster()

summary(PCA_S2b_CanhGa$model)
#my.palette <- colorspace::rainbow_hcl(100)

plotRGB(PCA_S2b_CanhGa$map,r=1,g=2,b=3,axes=T,colNA="black",stretch="lin",main="PCA S2b CanhGa 20200105",col=magma(20))
writeRaster(PCA_S2b_CanhGa$map,"Data_out/PCA_CanhGa_S2b_20200105.tif",format="GTiff",overwrite=F)


## S2b
beginCluster()
PCA_S2b_Bolama<-rasterPCA(Intertidal_Bolama,nSamples=NULL,nComp=nlayers(Intertidal_Bolama),maskCheck=T)
endCluster()

summary(PCA_S2b_Bolama$model)
#my.palette <- colorspace::rainbow_hcl(100)

plotRGB(PCA_S2b_Bolama$map,r=1,g=2,b=3,axes=T,colNA="black",stretch="lin",main="PCA S2b Bolama 20200105",col=magma(20))
writeRaster(PCA_S2b_Bolama$map,"Data_out/PCA_Bolama_S2b_20200105.tif",format="GTiff",overwrite=F)

# Unsupervised classification

## kmeans classification
### S2a Urok
values1_S2a_Urok<-getValues(Intertidal_urok1)
i_S2a_Urok<-which(!is.na(values1_S2a_Urok))
values1_S2a_Urok<-na.omit(values1_S2a_Urok)
head(values1_S2a_Urok)
tail(values1_S2a_Urok)

beginCluster()

#### kmeans classification Urok 7 classes
E_S2a_Urok<-kmeans(values1_S2a_Urok, 7, iter.max = 1000, nstart = 25)
kmeans_raster_S2a_Urok<-raster(Intertidal_urok1)
kmeans_raster_S2a_Urok[i_S2a_Urok]<-E_S2a_Urok$cluster

endCluster()

plot(kmeans_raster_S2a_Urok,col=my.palette,main="kmeans 7 S2a Urok 20200105",colNA="black")
writeRaster(kmeans_raster_S2a_Urok,"Data_out/kmeans_7_S2a_Urok_20200105.tif",format="GTiff",overwrite=F)

#### kmeans classification Urok 4 classes
E_S2a_Urok<-kmeans(values1_S2a_Urok, 4, iter.max = 1000, nstart = 25)
kmeans_raster_S2a_Urok<-raster(Intertidal_urok1)
kmeans_raster_S2a_Urok[i_S2a_Urok]<-E_S2a_Urok$cluster

endCluster()

plot(kmeans_raster_S2a_Urok,col=my.palette,main="kmeans 4 S2a Urok 20200105",colNA="black")
writeRaster(kmeans_raster_S2a_Urok,"Data_out/kmeans_4_S2a_Urok_20200105.tif",format="GTiff",overwrite=F)

### S2a Bub
values1_S2a_Bub<-getValues(Intertidal_Bub)
i_S2a_Bub<-which(!is.na(values1_S2a_Bub))
values1_S2a_Bub<-na.omit(values1_S2a_Bub)
head(values1_S2a_Bub)
tail(values1_S2a_Bub)

beginCluster()

#### kmeans classification Bub 7 classes
E_S2a_Bub<-kmeans(values1_S2a_Bub, 7, iter.max = 1000, nstart = 25)
kmeans_raster_S2a_Bub<-raster(Intertidal_Bub)
kmeans_raster_S2a_Bub[i_S2a_Bub]<-E_S2a_Bub$cluster

endCluster()

plot(kmeans_raster_S2a_Bub,col=my.palette,main="kmeans 7 S2a Bub 20200105",colNA="black")

writeRaster(kmeans_raster_S2a_Bub,"Data_out/kmeans_7_S2a_Bub_20200105.tif",format="GTiff",overwrite=F)


#### kmeans classification Bub 4 classes
E_S2a_Bub<-kmeans(values1_S2a_Bub, 4, iter.max = 1000, nstart = 25)
kmeans_raster_S2a_Bub<-raster(Intertidal_Bub)
kmeans_raster_S2a_Bub[i_S2a_Bub]<-E_S2a_Bub$cluster

endCluster()

plot(kmeans_raster_S2a_Bub,col=my.palette,main="kmeans 4 S2a Bub 20200105",colNA="black")

writeRaster(kmeans_raster_S2a_Bub,"Data_out/kmeans_4_S2a_Bub_20200105.tif",format="GTiff",overwrite=F)


### S2b CanhGa
values1_S2b_CanhGa<-getValues(Intertidal_CanhGa)
i_S2b_CanhGa<-which(!is.na(values1_S2b_CanhGa))
values1_S2b_CanhGa<-na.omit(values1_S2b_CanhGa)
head(values1_S2b_CanhGa)
tail(values1_S2b_CanhGa)

beginCluster()

#### kmeans classification CanhGa 7 classes
E_S2b_CanhGa<-kmeans(values1_S2b_CanhGa, 7, iter.max = 1000, nstart = 25)
kmeans_raster_S2b_CanhGa<-raster(Intertidal_CanhGa)
kmeans_raster_S2b_CanhGa[i_S2b_CanhGa]<-E_S2b_CanhGa$cluster

endCluster()

plot(kmeans_raster_S2b_CanhGa,col=my.palette,main="kmeans 7 S2b CanhGa 20200105",colNA="black")

writeRaster(kmeans_raster_S2b_CanhGa,"Data_out/kmeans_7_S2b_CanhGa_20200105.tif",format="GTiff",overwrite=F)

beginCluster()

#### kmeans classification CanhGa 4 classes
E_S2b_CanhGa<-kmeans(values1_S2b_CanhGa, 4, iter.max = 1000, nstart = 25)
kmeans_raster_S2b_CanhGa<-raster(Intertidal_CanhGa)
kmeans_raster_S2b_CanhGa[i_S2b_CanhGa]<-E_S2b_CanhGa$cluster

endCluster()

plot(kmeans_raster_S2b_CanhGa,col=my.palette,main="kmeans 4 S2b CanhGa 20200105",colNA="black")

writeRaster(kmeans_raster_S2b_CanhGa,"Data_out/kmeans_4_S2b_CanhGa_20200105.tif",format="GTiff",overwrite=F)



### S2b Bolama
values1_S2b_Bolama<-getValues(Intertidal_Bolama)
i_S2b_Bolama<-which(!is.na(values1_S2b_Bolama))
values1_S2b_Bolama<-na.omit(values1_S2b_Bolama)
head(values1_S2b_Bolama)
tail(values1_S2b_Bolama)

beginCluster()

#### kmeans classification Bolama 7 classes
E_S2b_Bolama<-kmeans(values1_S2b_Bolama, 7, iter.max = 1000, nstart = 25)
kmeans_raster_S2b_Bolama<-raster(Intertidal_Bolama)
kmeans_raster_S2b_Bolama[i_S2b_Bolama]<-E_S2b_Bolama$cluster

endCluster()

plot(kmeans_raster_S2b_Bolama,col=my.palette,main="kmeans 7 S2b Bolama 20200105",colNA="black")

writeRaster(kmeans_raster_S2b_Bolama,"Data_out/kmeans_7_S2b_Bolama_20200105.tif",format="GTiff",overwrite=F)

#### kmeans classification Bolama 4 classes
E_S2b_Bolama<-kmeans(values1_S2b_Bolama, 4, iter.max = 1000, nstart = 25)
kmeans_raster_S2b_Bolama<-raster(Intertidal_Bolama)
kmeans_raster_S2b_Bolama[i_S2b_Bolama]<-E_S2b_Bolama$cluster

endCluster()

plot(kmeans_raster_S2b_Bolama,col=my.palette,main="kmeans 4 S2b Bolama 20200105",colNA="black")

writeRaster(kmeans_raster_S2b_Bolama,"Data_out/kmeans_4_S2b_Bolama_20200105.tif",format="GTiff",overwrite=F)


## clara classification Urok 7 classes
beginCluster()
clus_S2a_Urok<-clara(values1_S2a_Urok,7,samples=1000,metric="manhattan",pamLike=T)
clara_raster_S2a_Urok<-raster(Intertidal_urok1)
clara_raster_S2a_Urok[i_S2a_Urok]<-clus_S2a_Urok$clustering
endCluster()

plot(clara_raster_S2a_Urok,col=my.palette,colNA="black",main="clara 7 urok")
writeRaster(clara_raster_S2a_Urok,"Data_out/clara_7_Urok_20200105.tif",format="GTiff",overwrite=F)

## clara classification Bubaque 7 classes
beginCluster()
clus_S2a_Bub<-clara(values1_S2a_Bub,7,samples=1000,metric="manhattan",pamLike=T)
clara_raster_S2a_Bub<-raster(Intertidal_Bub1)
clara_raster_S2a_Bub[i_S2a_Bub]<-clus_S2a_Bub$clustering
endCluster()

plot(clara_raster_S2a_Bub,col=my.palette,colNA="black",main="clara 7 Bub")
writeRaster(clara_raster_S2a_Bub,"Data_out/clara_7_Bub_20200105.tif",format="GTiff",overwrite=F)

## clara classification CanhGa 7 classes
beginCluster()
clus_S2b_CanhGa<-clara(values1_S2b_CanhGa,7,samples=1000,metric="manhattan",pamLike=T)
clara_raster_S2b_CanhGa<-raster(Intertidal_CanhGa)
clara_raster_S2b_CanhGa[i_S2b_CanhGa]<-clus_S2b_CanhGa$clustering
endCluster()

plot(clara_raster_S2b_CanhGa,col=my.palette,colNA="black",main="clara 7 CanhGa")
writeRaster(clara_raster_S2b_CanhGa,"Data_out/clara_7_CanhGa_20200105.tif",format="GTiff",overwrite=F)

## clara classification Bolama 7 classes
beginCluster()
clus_S2b_Bolama<-clara(values1_S2b_Bolama,7,samples=1000,metric="manhattan",pamLike=T)
clara_raster_S2b_Bolama<-raster(Intertidal_Bolama1)
clara_raster_S2b_Bolama[i_S2b_Bolama]<-clus_S2b_Bolama$clustering
endCluster()

plot(clara_raster_S2b_Bolama,col=my.palette,colNA="black",main="clara 7 Bolama")
writeRaster(clara_raster_S2b_Bolama,"Data_out/clara_7_Bolama_20200105.tif",format="GTiff",overwrite=F)


## Random Forest Urok 7 classes

### unsupervised randomForest classification using kmeans
vx_S2a_Urok<-values1_S2a_Urok[sample(nrow(values1_S2a_Urok),500),]
randfor_S2a_Urok=randomForest(vx_S2a_Urok)
randfor_prox_S2a_Urok<-randomForest(vx_S2a_Urok,ntree = 1000,proximity=T)$proximity
E_randfor_S2a_Urok<-kmeans(randfor_prox_S2a_Urok,7,iter.max=500,nstart=25)
randfor_S2a_Urok<-randomForest(vx_S2a_Urok,as.factor(E_randfor_S2a_Urok$cluster),ntree=500)
randfor_raster_S2a_Urok<-predict(Intertidal_urok1,randfor_S2a_Urok)

plot(randfor_raster_S2a_Urok,col=my.palette,colNA="black",main="randfor_7_Urok_20200105")
writeRaster(randfor_raster_S2a_Urok, "Data_out/randfor_7_Urok_20200105.tif",format="GTiff",overwrite=F)


## Random Forest Bub 7 classes

### unsupervised randomForest classification using kmeans
vx_S2a_Bub<-values1_S2a_Bub[sample(nrow(values1_S2a_Bub),500),]
randfor_S2a_Bub=randomForest(vx_S2a_Bub)
randfor_prox_S2a_Bub<-randomForest(vx_S2a_Bub,ntree = 1000,proximity=T)$proximity
E_randfor_S2a_Bub<-kmeans(randfor_prox_S2a_Bub,7,iter.max=500,nstart=25)
randfor_S2a_Bub<-randomForest(vx_S2a_Bub,as.factor(E_randfor_S2a_Bub$cluster),ntree=500)
randfor_raster_S2a_Bub<-predict(Intertidal_Bub,randfor_S2a_Bub)

plot(randfor_raster_S2a_Bub,col=my.palette,colNA="black",main="randfor_7_Bub_20200105")
writeRaster(randfor_raster_S2a_Bub, "Data_out/randfor_7_Bub_20200105.tif",format="GTiff",overwrite=F)


## Random Forest CanhaGa 7 classes

### unsupervised randomForest classification using kmeans
vx_S2b_CanhGa<-values1_S2b_CanhGa[sample(nrow(values1_S2b_CanhGa),500),]
randfor_S2b_CanhGa=randomForest(vx_S2b_CanhGa)
randfor_prox_S2b_CanhGa<-randomForest(vx_S2b_CanhGa,ntree = 1000,proximity=T)$proximity
E_randfor_S2b_CanhGa<-kmeans(randfor_prox_S2b_CanhGa,7,iter.max=500,nstart=25)
randfor_S2b_CanhGa<-randomForest(vx_S2b_CanhGa,as.factor(E_randfor_S2b_CanhGa$cluster),ntree=500)
randfor_raster_S2b_CanhGa<-predict(Intertidal_CanhGa,randfor_S2b_CanhGa)

plot(randfor_raster_S2b_CanhGa,col=my.palette,colNA="black",main="randfor_7_CanhGa_20200105")
writeRaster(randfor_raster_S2b_CanhGa, "Data_out/randfor_7_CanhGa_20200105.tif",format="GTiff",overwrite=F)


## Random Forest Bolama 7 classes

### unsupervised randomForest classification using kmeans
vx_S2b_Bolama<-values1_S2b_Bolama[sample(nrow(values1_S2b_Bolama),500),]
randfor_S2b_Bolama=randomForest(vx_S2b_Bolama)
randfor_prox_S2b_Bolama<-randomForest(vx_S2b_Bolama,ntree = 1000,proximity=T)$proximity
E_randfor_S2b_Bolama<-kmeans(randfor_prox_S2b_Bolama,7,iter.max=500,nstart=25)
randfor_S2b_Bolama<-randomForest(vx_S2b_Bolama,as.factor(E_randfor_S2b_Bolama$cluster),ntree=500)
randfor_raster_S2b_Bolama<-predict(Intertidal_Bolama,randfor_S2b_Bolama)

plot(randfor_raster_S2b_Bolama,col=my.palette,colNA="black",main="randfor_7_Bolama_20200105")
writeRaster(randfor_raster_S2b_Bolama, "Data_out/randfor_7_Bolama_20200105.tif",format="GTiff",overwrite=F)


## Comparing the 4 algorithms

## Urok
class_stack_Urok<-stack(kmeans_raster_S2a_Urok,clara_raster_S2a_Urok,randfor_raster_S2a_Urok)
names(class_stack_Urok)<-c("kmeans Urok","clara Urok","randomForest Urok")
plot(class_stack_Urok,colNA="black",col=magma(50))

## Bub
class_stack_Bub<-stack(kmeans_raster_S2a_Bub,clara_raster_S2a_Bub,randfor_raster_S2a_Bub)
names(class_stack_Bub)<-c("kmeans Bub","clara Bub","randomForest Bub")
plot(class_stack_Bub,colNA="black",col=magma(50))

## CanhGa
class_stack_CanhGa<-stack(kmeans_raster_S2b_CanhGa,clara_raster_S2b_CanhGa,randfor_raster_S2b_CanhGa)
names(class_stack_CanhGa)<-c("kmeans CanhGa","clara CanhGa","randomForest CanhGa")
plot(class_stack_CanhGa,colNA="grey45",col=magma(50))

## Bolama
class_stack_Bolama<-stack(kmeans_raster_S2b_Bolama,clara_raster_S2b_Bolama,randfor_raster_S2b_Bolama)
names(class_stack_Bolama)<-c("kmeans Bolama","clara Bolama","randomForest Bolama")
plot(class_stack_Bolama,colNA="black",col=magma(50))





