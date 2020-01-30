setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

packs<-c("RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis")
lapply(packs,require,character.only=T)

my.palette<-colorspace::rainbow_hcl(100)

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
S1_VH_VV<-stack("D:/Work/FCUL/Doutoramento/Capitulos/Mapping_intertidal_sediments/SNAP/S1/20200116_1917/subset_0_of_S1A_IW_GRDH_1SDV_20200116T191708_20200116T191733_030830_038983_2062_Orb_NR_Cal_Spk_TC_VV_VH_db.tif")
S1_VH_VV

## change names of layers (I know the order because of the way I preprocessed it in SNAP)
names(S1_VH_VV)<-c("S1_VH","S1_VV")
ggR(S1_VH_VV$S1_VH)
ggR(S1_VH_VV$S1_VV)

# bathymetry map
bat<-raster("D:/Work/FCUL/Doutoramento/Capitulos/Mapping_intertidal_sediments/Jcatalao/bijagos_batim.tif")
#plot(bat)

#Poligonos areas de trabalho
#gnb<-readOGR(dsn="Shapefiles/GNB",layer="gnb_poly")
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

## S2 b
S2b_Urok<-crop(S2_20200105_b,Urok)
S2b_Urok
#plotRGB(S2b_Urok,3,2,1,stretch = "lin")

### Join a and b images
#S2a_S2b_Urok<-mosaic(S2a_Urok,S2b_Urok,fun=min)
S2a_S2b_Urok<-merge(S2b_Urok,S2a_Urok,overlap=T)
#plotRGB(S2a_S2b_Urok,3,2,1,stretch="lin")
names(S2a_S2b_Urok)<-c("B02","B03","B04","B05","B06","B07","B08","B08a","B09","B11","B12")

writeRaster(S2a_S2b_Urok,"Data_out/SatImg_StudyArea/S2a_S2b_Urok.tif",format="GTiff",overwrite=F)

## radar image
###Urok
VH_VV_Urok<-crop(S1_VH_VV,Urok)
#ggR(VV_VH_Urok,2)
#hist(VV_VH_Urok,breaks=100,axes=T)
writeRaster(VH_VV_Urok,"Data_out/SatImg_StudyArea/VH_VV_Urok.tif",format="GTiff",overwrite=F)

## batimetry
### Urok
bat_Urok<-crop(bat,Urok)
#plot(bat_Urok)
writeRaster(bat_Urok,"Data_out/SatImg_StudyArea/bat_Urok.tif",format="GTiff",overwrite=F)

###Bubaque
S2a_Bub<-crop(S2_20200105_a,Bub)
S2a_Bub
#plotRGB(S2a_Bub,3,2,1,stretch = "lin")
#writeRaster(S2a_Bub,"Data_out/S2a_Bub.tif",format="GTiff",overwrite=F)

## S2 b
S2b_Bub<-crop(S2_20200105_b,Bub)
S2b_Bub
#plotRGB(S2b_Bub,3,2,1,stretch = "lin")

### Join a and b images
#S2a_S2b_Bub<-mosaic(S2a_Bub,S2b_Bub,fun=min)
S2a_S2b_Bub<-merge(S2b_Bub,S2a_Bub,overlap=T)
#plotRGB(S2a_S2b_Bub,3,2,1,stretch="lin")
names(S2a_S2b_Bub)<-c("B02","B03","B04","B05","B06","B07","B08","B08a","B09","B11","B12")

writeRaster(S2a_S2b_Bub,"Data_out/SatImg_StudyArea/S2a_S2b_Bub.tif",format="GTiff",overwrite=F)

## radar
### Bubaque
VH_VV_Bub<-crop(S1_VH_VV,Bub)
#ggR(VV_VH_Bub,2)
writeRaster(VH_VV_Bub,"Data_out/SatImg_StudyArea/VH_VV_Bub.tif",format="GTiff",overwrite=F)

## bathymetry
bat_Bub<-crop(bat,Bub)
#plot(bat_Bub)
writeRaster(bat_Bub,"Data_out/SatImg_StudyArea/bat_Bub.tif",format="GTiff",overwrite=F)

###Canhabaque galinhas
S2b_CanhGa<-crop(S2_20200105_b,CanhGa)
S2b_CanhGa
#plotRGB(S2b_CanhGa,3,2,1,stretch = "lin")

writeRaster(S2b_CanhGa,"Data_out/SatImg_StudyArea/S2b_CanhGa.tif",format="GTiff",overwrite=F)

## radar
### Canhabaque galinhas 
VH_VV_CanhGa<-crop(S1_VH_VV,CanhGa) ## Esta porção aindaq tem terra e água, vai elimar-se com a mascara
#ggR(VV_VH_CanhGa,2)
writeRaster(VH_VV_CanhGa,"Data_out/SatImg_StudyArea/VH_VV_CanhGa.tif",format="GTiff",overwrite=F)


###Bolama
S2b_Bolama<-crop(S2_20200105_b,Bolama)
S2b_Bolama
#plotRGB(S2b_Bolama,3,2,1,stretch = "lin")

writeRaster(S2b_Bolama,"Data_out/SatImg_StudyArea/S2b_Bolama.tif",format="GTiff",overwrite=F)

## radar
### Bolama 
VH_VV_Bolama<-crop(S1_VH_VV,Bolama) ## Esta porção aindaq tem terra e água, vai elimar-se com a mascara
#ggR(VV_VH_Bolama,2)

writeRaster(VH_VV_Bolama,"Data_out/SatImg_StudyArea/VH_VV_Bolama.tif",format="GTiff",overwrite=F)


## Mask with NDWI - this is better for now, after will try other index or new catalao way

###Urok
ndwi_Urok<-(S2a_S2b_Urok$B03-S2a_S2b_Urok$B08)/(S2a_S2b_Urok$B03+S2a_S2b_Urok$B08)
#plot(ndwi_Urok)
#hist(ndwi_Urok, n=1000, freq=T,axes=F)
#axis(1,at=c(seq(-0.8,0.8,0.05)))
#axis(2)

ndwi_Urok_F<-cut(ndwi_Urok,breaks=c(-2,-.30,.20,2))
#plot(ndwi_Urok_F)
#table(ndwi_Urok_F@data@values)

mask_urok<-ndwi_Urok_F==2
#plot(mask_urok)
mask_urok[mask_urok==0]<-NA
#plot(mask_urok,colNA=1)

Intertidal_urok<-S2a_S2b_Urok*mask_urok
plotRGB(Intertidal_urok,3,2,1,stretch = "lin")

writeRaster(Intertidal_urok,"Data_out/Intertidal/Intertidal_urok.tif",format="GTiff",overwrite=F)

## Mask radar

## check the extents of the two layers -- if they are different crop both datasets
if (extent(VH_VV_Urok) == extent(mask_urok)){
  print("Extents are the same, no need to intersect, multiplied them")
  SAR_Urok<-VH_VV_Urok*mask_urok
} else {
  # calculate overlap between the two datasets
  overlap <- intersect(extent(VH_VV_Urok), extent(mask_urok))
  # now let's crop both datasets to the overlap region
  SAR_Urok_c <- crop(VH_VV_Urok, overlap)
  mask_urok_c <- crop(mask_urok, overlap)
  if (extent(SAR_Urok_c) == extent(mask_urok_c)){
    print("Extents are different, data cropped and solved")
  } else {
    # resample batim using sat image
    mask_urok_r<-resample(mask_urok,VH_VV_Urok,method="bilinear")
    if (extent(VH_VV_Urok)==extent(mask_urok_r)){
      print("Cropping failed. Batim resampled, solved. Used mask_urok_r to mask VH_VV_Urok")
      SAR_Urok<-VH_VV_Urok*mask_urok_r
    } else {
      print("Failed, better try fishing")
    }
  }
}

#plot(SAR_Urok,2)
writeRaster(SAR_Urok,"Data_out/Intertidal/SAR_Urok.tif",format="GTiff",overwrite=F)

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
#hist(ndwi_Bub, n=1000, freq=T,axes=F)
#axis(1,at=c(seq(-0.8,0.8,0.05)))
#axis(2)

ndwi_Bub_F<-cut(ndwi_Bub,breaks=c(-2,-.425,.125,2))
#plot(ndwi_Bub_F)
#table(ndwi_Bub_F@data@values)

mask_Bub<-ndwi_Bub_F==2
#plot(mask_Bub)
mask_Bub[mask_Bub==0]<-NA
#plot(mask_Bub,colNA=1)

Intertidal_Bub<-S2a_S2b_Bub*mask_Bub
#plotRGB(Intertidal_Bub,3,2,1,stretch = "lin")
writeRaster(Intertidal_Bub,"Data_out/Intertidal/Intertidal_Bub.tif",format="GTiff",overwrite=F)

## radar

## check the extents of the two layers -- if they are different crop both datasets
if (extent(VH_VV_Bub) == extent(mask_Bub)){
  print("Extents are the same, no need to intersect, multiplied them")
  SAR_Bub<-VH_VV_Bub*mask_Bub
} else {
  # calculate overlap between the two datasets
  overlap <- intersect(extent(VH_VV_Bub), extent(mask_Bub))
  # now let's crop both datasets to the overlap region
  SAR_Bub_c <- crop(VH_VV_Bub, overlap)
  mask_Bub_c <- crop(mask_Bub, overlap)
  if (extent(SAR_Bub_c) == extent(mask_Bub_c)){
    print("Extents are different, data cropped and solved")
  } else {
    # resample batim using sat image
    mask_Bub_r<-resample(mask_Bub,VH_VV_Bub,method="bilinear")
    if (extent(VH_VV_Bub)==extent(mask_Bub_r)){
      print("Cropping failed. Batim resampled, solved. Used mask_Bub_r to mask VH_VV_Bub")
      SAR_Bub<-VH_VV_Bub*mask_Bub_r
    } else {
      print("Failed, better try fishing")
    }
  }
}

#plot(SAR_Bub,2)
writeRaster(SAR_Bub,"Data_out/Intertidal/SAR_Bub.tif",format="GTiff",overwrite=F)


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
writeRaster(Intertidal_CanhGa,"Data_out/Intertidal/Intertidal_CanhGa.tif",format="GTiff",overwrite=F)

## radar

## check the extents of the two layers -- if they are different crop both datasets
if (extent(VH_VV_CanhGa) == extent(mask_CanhGa)){
  print("Extents are the same, no need to intersect, multiplied them")
  SAR_CanhGa<-VH_VV_CanhGa*mask_CanhGa
} else {
  # calculate overlap between the two datasets
  overlap <- intersect(extent(VH_VV_CanhGa), extent(mask_CanhGa))
  # now let's crop both datasets to the overlap region
  SAR_CanhGa_c <- crop(VH_VV_CanhGa, overlap)
  mask_CanhGa_c <- crop(mask_CanhGa, overlap)
  if (extent(SAR_CanhGa_c) == extent(mask_CanhGa_c)){
    print("Extents are different, data cropped and solved")
  } else {
    # resample batim using sat image
    mask_CanhGa_r<-resample(mask_CanhGa,VH_VV_CanhGa,method="bilinear")
    if (extent(VH_VV_CanhGa)==extent(mask_CanhGa_r)){
      print("Cropping failed. Batim resampled, solved. Used mask_CanhGa_r to mask VH_VV_CanhGa")
      SAR_CanhGa<-VH_VV_CanhGa*mask_CanhGa_r
    } else {
      print("Failed, better try fishing")
    }
  }
}

#plot(SAR_CanhGa,2)
writeRaster(SAR_CanhGa,"Data_out/Intertidal/SAR_CanhGa.tif",format="GTiff",overwrite=F)

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
writeRaster(Intertidal_Bolama,"Data_out/Intertidal/Intertidal_Bolama.tif",format="GTiff",overwrite=F)

## radar

## check the extents of the two layers -- if they are different crop both datasets
if (extent(VH_VV_Bolama) == extent(mask_Bolama)){
  print("Extents are the same, no need to intersect, multiplied them")
  SAR_Bolama<-VH_VV_Bolama*mask_Bolama
} else {
  # calculate overlap between the two datasets
  overlap <- intersect(extent(VH_VV_Bolama), extent(mask_Bolama))
  # now let's crop both datasets to the overlap region
  SAR_Bolama_c <- crop(VH_VV_Bolama, overlap)
  mask_Bolama_c <- crop(mask_Bolama, overlap)
  if (extent(SAR_Bolama_c) == extent(mask_Bolama_c)){
    print("Extents are different, data cropped and solved")
  } else {
    # resample batim using sat image
    mask_Bolama_r<-resample(mask_Bolama,VH_VV_Bolama,method="bilinear")
    if (extent(VH_VV_Bolama)==extent(mask_Bolama_r)){
      print("Cropping failed. Batim resampled, solved. Used mask_Bolama_r to mask VH_VV_Bolama")
      SAR_Bolama<-VH_VV_Bolama*mask_Bolama_r
    } else {
      print("Failed, better try fishing")
    }
  }
}

#plot(SAR_Bolama,1)
writeRaster(SAR_Bolama,"Data_out/Intertidal/SAR_Bolama.tif",format="GTiff",overwrite=F)



# stack s2, s1 and bat images
##Urok
all_Urok<-stack(Intertidal_urok,resample(SAR_Urok,Intertidal_urok),resample(Intertidal_bat_urok,Intertidal_urok))
names(all_Urok)<-c(names(S2a_S2b_Urok),names(VH_VV_Urok),names(bat_Urok))
writeRaster(all_Urok,"Data_out/Stack/stack_all_Urok.tif",format="GTiff",overwrite=F)

##Bub
all_Bub<-stack(Intertidal_Bub,resample(SAR_Bub,Intertidal_Bub),resample(Intertidal_bat_Bub,Intertidal_Bub))
names(all_Bub)<-c(names(S2a_S2b_Bub),names(VH_VV_Bub),names(bat_Bub))
writeRaster(all_Bub,"Data_out/Stack/stack_all_Bub.tif",format="GTiff",overwrite=F)

##CanhGa
all_CanhGa<-stack(Intertidal_CanhGa,resample(SAR_CanhGa,Intertidal_CanhGa))
names(all_CanhGa)<-c(names(S2b_CanhGa),names(VH_VV_CanhGa))
writeRaster(all_CanhGa,"Data_out/Stack/stack_all_CanhGa.tif",format="GTiff",overwrite=F)

##Bolama
all_Bolama<-stack(Intertidal_Bolama,resample(SAR_Bolama,Intertidal_Bolama))
names(all_Bolama)<-c(names(S2b_Bolama),names(VH_VV_Bolama))
writeRaster(all_Bolama,"Data_out/Stack/stack_all_Bolama.tif",format="GTiff",overwrite=F)


# PCA

##Urok
### just with S2
beginCluster()
PCA_Urok_S2<-rasterPCA(Intertidal_urok,nSamples=NULL,nComp=nlayers(Intertidal_urok),maskCheck=T)
#endCluster()

summary(PCA_Urok_S2$model)
loadings(PCA_Urok_S2$model)

#plotRGB(PCA_Urok_S2$map,r=1,g=2,b=3,axes=T,colNA="black",stretch="lin",main="PCA Urok S2 20200105",col=magma(50))
writeRaster(PCA_Urok_S2$map,"Data_out/PCA/PCA_Urok_S2_20200105.tif",format="GTiff",overwrite=F)


### just with S1
#beginCluster()
PCA_Urok_S1<-rasterPCA(SAR_Urok,nSamples=NULL,nComp=nlayers(SAR_Urok),maskCheck=T)
#endCluster()

summary(PCA_Urok_S1$model)
loadings(PCA_Urok_S1$model)

plotRGB(PCA_Urok_S1$map,r=1,g=2,b=1,axes=T,colNA="black",stretch="lin",main="PCA Urok S1 20200116",col=magma(20))
writeRaster(PCA_Urok_S1$map,"Data_out/PCA/PCA_Urok_S1_20200116.tif",format="GTiff",overwrite=F)

### just with all
#beginCluster()
PCA_Urok_all<-rasterPCA(all_Urok,nSamples=NULL,nComp=nlayers(all_Urok),maskCheck=T)
#endCluster()

summary(PCA_Urok_all$model)
loadings(PCA_Urok_all$model)

#plotRGB(PCA_Urok_all$map,r=1,g=2,b=3,axes=T,colNA="black",stretch="lin",main="PCA Urok all 20200105",col=magma(50))
writeRaster(PCA_Urok_all$map,"Data_out/PCA/PCA_Urok_all_20200105-0116-2018.tif",format="GTiff",overwrite=F)


##Bub
### just with S2
#beginCluster()
PCA_Bub_S2<-rasterPCA(Intertidal_Bub,nSamples=NULL,nComp=nlayers(Intertidal_Bub),maskCheck=T)
#endCluster()

summary(PCA_Bub_S2$model)
loadings(PCA_Bub_S2$model)

#plotRGB(PCA_Bub_S2$map,r=1,g=2,b=3,axes=T,colNA="black",stretch="lin",main="PCA Bub S2 20200105",col=magma(50))
writeRaster(PCA_Bub_S2$map,"Data_out/PCA/PCA_Bub_S2_20200105.tif",format="GTiff",overwrite=F)


### just with S1
#beginCluster()
PCA_Bub_S1<-rasterPCA(SAR_Bub,nSamples=NULL,nComp=nlayers(SAR_Bub),maskCheck=T)
#endCluster()

summary(PCA_Bub_S1$model)
loadings(PCA_Bub_S1$model)

#plotRGB(PCA_Bub_S1$map,r=1,g=2,b=3,axes=T,colNA="black",stretch="lin",main="PCA Bub S1 20200116",col=magma(50))
writeRaster(PCA_Bub_S1$map,"Data_out/PCA/PCA_Bub_S1_20200116.tif",format="GTiff",overwrite=F)

### just with all
#beginCluster()
PCA_Bub_all<-rasterPCA(all_Bub,nSamples=NULL,nComp=nlayers(all_Bub),maskCheck=T)
#endCluster()

summary(PCA_Bub_all$model)
loadings(PCA_Bub_all$model)

#plotRGB(PCA_Bub_all$map,r=1,g=2,b=3,axes=T,colNA="black",stretch="lin",main="PCA Bub all 20200105",col=magma(50))
writeRaster(PCA_Bub_all$map,"Data_out/PCA/PCA_Bub_all_20200105-0116-2018.tif",format="GTiff",overwrite=F)

##CanhGa
### just with S2
#beginCluster()
PCA_CanhGa_S2<-rasterPCA(Intertidal_CanhGa,nSamples=NULL,nComp=nlayers(Intertidal_CanhGa),maskCheck=T)
#endCluster()

summary(PCA_CanhGa_S2$model)
loadings(PCA_CanhGa_S2$model)

#plotRGB(PCA_CanhGa_S2$map,r=1,g=2,b=3,axes=T,colNA="black",stretch="lin",main="PCA CanhGa S2 20200105",col=magma(50))
writeRaster(PCA_CanhGa_S2$map,"Data_out/PCA/PCA_CanhGa_S2_20200105.tif",format="GTiff",overwrite=F)


### just with S1
#beginCluster()
PCA_CanhGa_S1<-rasterPCA(SAR_CanhGa,nSamples=NULL,nComp=nlayers(SAR_CanhGa),maskCheck=T)
#endCluster()

summary(PCA_CanhGa_S1$model)
loadings(PCA_CanhGa_S1$model)

#plotRGB(PCA_CanhGa_S1$map,r=1,g=2,b=3,axes=T,colNA="black",stretch="lin",main="PCA CanhGa S1 20200116",col=magma(50))
writeRaster(PCA_CanhGa_S1$map,"Data_out/PCA/PCA_CanhGa_S1_20200116.tif",format="GTiff",overwrite=F)

### just with all
#beginCluster()
PCA_CanhGa_all<-rasterPCA(all_CanhGa,nSamples=NULL,nComp=nlayers(all_CanhGa),maskCheck=T)
#endCluster()

summary(PCA_CanhGa_all$model)
loadings(PCA_CanhGa_all$model)

#plotRGB(PCA_CanhGa_all$map,r=1,g=2,b=3,axes=T,colNA="black",stretch="lin",main="PCA CanhGa all 20200105",col=magma(50))
writeRaster(PCA_CanhGa_all$map,"Data_out/PCA/PCA_CanhGa_all_20200105-0116-2018.tif",format="GTiff",overwrite=F)


##Bolama
### just with S2
#beginCluster()
PCA_Bolama_S2<-rasterPCA(Intertidal_Bolama,nSamples=NULL,nComp=nlayers(Intertidal_Bolama),maskCheck=T)
#endCluster()

summary(PCA_Bolama_S2$model)
loadings(PCA_Bolama_S2$model)

#plotRGB(PCA_Bolama_S2$map,r=1,g=2,b=3,axes=T,colNA="black",stretch="lin",main="PCA Bolama S2 20200105",col=magma(50))
writeRaster(PCA_Bolama_S2$map,"Data_out/PCA/PCA_Bolama_S2_20200105.tif",format="GTiff",overwrite=F)


### just with S1
#beginCluster()
PCA_Bolama_S1<-rasterPCA(SAR_Bolama,nSamples=NULL,nComp=nlayers(SAR_Bolama),maskCheck=T)
#endCluster()

summary(PCA_Bolama_S1$model)
loadings(PCA_Bolama_S1$model)

#plotRGB(PCA_Bolama_S1$map,r=1,g=2,b=3,axes=T,colNA="black",stretch="lin",main="PCA Bolama S1 20200116",col=magma(50))
writeRaster(PCA_Bolama_S1$map,"Data_out/PCA/PCA_Bolama_S1_20200116.tif",format="GTiff",overwrite=F)

### just with all
#beginCluster()
PCA_Bolama_all<-rasterPCA(all_Bolama,nSamples=NULL,nComp=nlayers(all_Bolama),maskCheck=T)
endCluster()

summary(PCA_Bolama_all$model)
loadings(PCA_Bolama_all$model)

plotRGB(PCA_Bolama_all$map,r=1,g=2,b=3,axes=T,colNA="black",stretch="lin",main="PCA Bolama all 20200105",col=magma(50))
writeRaster(PCA_Bolama_all$map,"Data_out/PCA/PCA_Bolama_all_20200105-0116-2018.tif",format="GTiff",overwrite=F)



# Unsupervised classification

## kmeans classification
### S2a Urok
values1_S2a_Urok<-getValues(stack("./Data_out/Intertidal/Intertidal_urok.tif")) ## Extract values from each band of S2 image, then scale the values as we don’t want the k-means algorithm to depend to an arbitrary variable unit
i_S2a_Urok<-which(!is.na(values1_S2a_Urok)) ## k-means can't deal with missing values, so create an vector with the positions without NA
values1_S2a_Urok<-scale(na.omit(values1_S2a_Urok)) ## omit NA values from data
head(values1_S2a_Urok)
tail(values1_S2a_Urok)

#beginCluster()

#### kmeans classification Urok 10 classes
set.seed(1)
E_S2a_Urok<-kmeans(values1_S2a_Urok,10,iter.max=1000,nstart=25,algorithm="MacQueen")
E_S2a_Urok
E_S2a_Urok$ifault
kmeans_raster_S2a_Urok<-raster(stack("./Data_out/Intertidal/Intertidal_urok.tif"))
kmeans_raster_S2a_Urok[i_S2a_Urok]<-E_S2a_Urok$cluster

#endCluster()

plot(kmeans_raster_S2a_Urok,col=magma(10),main="kmeans 10 S2a Urok 20200105",colNA="lightskyblue")
writeRaster(kmeans_raster_S2a_Urok,"Data_out/Kmeans/kmeans_10_S2a_Urok_20200105.tif",format="GTiff",overwrite=T)


### SAR Urok
values1_SAR_Urok<-getValues(stack("./Data_out/Intertidal/SAR_Urok.tif")) ## Extract values from each band of S2 image, then scale the values as we don’t want the k-means algorithm to depend to an arbitrary variable unit
i_SAR_Urok<-which(!is.na(values1_SAR_Urok)) ## k-means can't deal with missing values, so create an vector with the positions without NA
values1_SAR_Urok<-scale(na.omit(values1_SAR_Urok)) ## omit NA values from data
head(values1_SAR_Urok)
tail(values1_SAR_Urok)

beginCluster()

#### kmeans classification Urok 10 classes
set.seed(2)
E_SAR_Urok<-kmeans(values1_SAR_Urok,5,iter.max=1000,nstart=25,algorithm="MacQueen")
E_SAR_Urok
E_SAR_Urok$ifault
kmeans_raster_SAR_Urok<-raster(stack("./Data_out/Intertidal/SAR_Urok.tif"))
kmeans_raster_SAR_Urok[i_SAR_Urok]<-E_SAR_Urok$cluster

endCluster()

plot(kmeans_raster_SAR_Urok,col=magma(5),main="kmeans 5 SAR Urok 20200116",colNA="lightskyblue")
writeRaster(kmeans_raster_SAR_Urok,"Data_out/Kmeans/kmeans_5_SAR_Urok_20200116.tif",format="GTiff",overwrite=F)


# Create NDWI and stacks fot wet classification
all_Urok<-stack("./Data_out/Stack/stack_all_Urok.tif")
names(all_Urok)<-c(paste("B",c(2:8,"8a",9,11:12),sep=""),"VH","VV","bat")
#plot(all_Urok)


NDWI_1<-(all_Urok$B8-all_Urok$B12)/(all_Urok$B8+all_Urok$B12)
plot(NDWI_1)
NDWI_2<-(all_Urok$B3-all_Urok$B8)/(all_Urok$B3+all_Urok$B8)
plot(NDWI_2)
mNDWI<-(all_Urok$B3-all_Urok$B11)/(all_Urok$B3+all_Urok$B11)
plot(mNDWI)

U<-stack(all_Urok@layers[c(2,7,10,11:13)])
wet_Urok<-stack(U,NDWI_1,NDWI_2,mNDWI)
plot(wet_Urok)
names(wet_Urok)<-c(names(U),"NDWI_1","NDWI_2","mNDWI")

## kmeans classification
### wet Urok
values1_wet_Urok<-getValues(wet_Urok) ## Extract values from each band of S2 image, then scale the values as we don’t want the k-means algorithm to depend to an arbitrary variable unit
i_wet_Urok<-which(!is.na(values1_wet_Urok)) ## k-means can't deal with missing values, so create an vector with the positions without NA
values1_wet_Urok<-scale(na.omit(values1_wet_Urok)) ## omit NA values from data
head(values1_wet_Urok)
tail(values1_wet_Urok)

beginCluster()

#### kmeans classification Urok 10 classes
set.seed(3)
E_wet_Urok<-kmeans(values1_wet_Urok,3,iter.max=1000,nstart=25,algorithm="Hartigan-Wong")
E_wet_Urok
E_wet_Urok$ifault
kmeans_raster_wet_Urok<-raster(wet_Urok)
kmeans_raster_wet_Urok[i_wet_Urok]<-E_wet_Urok$cluster

endCluster()

plot(kmeans_raster_wet_Urok,col=magma(3),main="kmeans 3 wet Urok 20200105",colNA="lightskyblue")
writeRaster(kmeans_raster_wet_Urok,"Data_out/Kmeans/kmeans_3_wet_Urok_20200116.tif",format="GTiff",overwrite=F)


## kmeans classification
### all Urok

all_Urok1<-scale(all_Urok)


values1_all_Urok<-getValues(all_Urok1[[c(1:13)]]) ## Extract values from each band
i_all_Urok<-which(!is.na(values1_all_Urok)) ## k-means can't deal with missing values, so create an vector with the positions without NA
values1_all_Urok<-na.omit(values1_all_Urok) ## omit NA values from data
head(values1_all_Urok)
tail(values1_all_Urok)

#beginCluster()

#### kmeans classification Urok 10 classes
set.seed(3)
E_all_Urok<-kmeans(values1_all_Urok,10,iter.max=1000,nstart=25,algorithm="Hartigan-Wong")
E_all_Urok
E_all_Urok$ifault
kmeans_raster_all_Urok<-raster(all_Urok1)
kmeans_raster_all_Urok[i_all_Urok]<-E_all_Urok$cluster

#endCluster()

plot(kmeans_raster_all_Urok,col=magma(10),main="kmeans 10 all Urok 20200105",colNA="lightskyblue")
writeRaster(kmeans_raster_all_Urok,"Data_out/Kmeans/kmeans_10_all_Urok_20200116.tif",format="GTiff",overwrite=F)


##Bubaque

### S2a Bub
values1_S2a_Bub<-getValues(stack("./Data_out/Intertidal/Intertidal_Bub.tif")) ## Extract values from each band of S2 image, then scale the values as we don’t want the k-means algorithm to depend to an arbitrary variable unit
i_S2a_Bub<-which(!is.na(values1_S2a_Bub)) ## k-means can't deal with missing values, so create an vector with the positions without NA
values1_S2a_Bub<-scale(na.omit(values1_S2a_Bub)) ## omit NA values from data
head(values1_S2a_Bub)
tail(values1_S2a_Bub)

beginCluster()

#### kmeans classification Bub 10 classes
set.seed(4)
E_S2a_Bub<-kmeans(values1_S2a_Bub,10,iter.max=1000,nstart=25,algorithm="Hartigan-Wong")
E_S2a_Bub
E_S2a_Bub$ifault
kmeans_raster_S2a_Bub<-raster(stack("./Data_out/Intertidal/Intertidal_Bub.tif"))
kmeans_raster_S2a_Bub[i_S2a_Bub]<-E_S2a_Bub$cluster

endCluster()

plot(kmeans_raster_S2a_Bub,col=magma(10),main="kmeans 10 S2a Bub 20200105",colNA="lightskyblue")
writeRaster(kmeans_raster_S2a_Bub,"Data_out/Kmeans/kmeans_10_S2a_Bub_20200105.tif",format="GTiff",overwrite=F)


### SAR Bub
values1_SAR_Bub<-getValues(stack("./Data_out/Intertidal/SAR_Bub.tif")) ## Extract values from each band of S2 image, then scale the values as we don’t want the k-means algorithm to depend to an arbitrary variable unit
i_SAR_Bub<-which(!is.na(values1_SAR_Bub)) ## k-means can't deal with missing values, so create an vector with the positions without NA
values1_SAR_Bub<-scale(na.omit(values1_SAR_Bub)) ## omit NA values from data
head(values1_SAR_Bub)
tail(values1_SAR_Bub)

beginCluster()

#### kmeans classification Bub 10 classes
set.seed(5)
E_SAR_Bub<-kmeans(values1_SAR_Bub,8,iter.max=1000,nstart=25,algorithm="Hartigan-Wong")
E_SAR_Bub
E_SAR_Bub$ifault
kmeans_raster_SAR_Bub<-raster(stack("./Data_out/Intertidal/SAR_Bub.tif"))
kmeans_raster_SAR_Bub[i_SAR_Bub]<-E_SAR_Bub$cluster

endCluster()

plot(kmeans_raster_SAR_Bub,col=magma(8),main="kmeans 8 SAR Bub 20200116",colNA="lightskyblue")
writeRaster(kmeans_raster_SAR_Bub,"Data_out/Kmeans/kmeans_8_SAR_Bub_20200116.tif",format="GTiff",overwrite=F)


### all Bub

all_Bub<-stack("./Data_out/Stack/stack_all_Bub.tif")
names(all_Bub)<-c(paste("B",c(2:8,"8a",9,11:12),sep=""),"VH","VV","bat")
plot(all_Bub)


NDWI_1<-(all_Bub$B8-all_Bub$B12)/(all_Bub$B8+all_Bub$B12)
plot(NDWI_1)
NDWI_2<-(all_Bub$B3-all_Bub$B8)/(all_Bub$B3+all_Bub$B8)
plot(NDWI_2)
mNDWI<-(all_Bub$B3-all_Bub$B11)/(all_Bub$B3+all_Bub$B11)
plot(mNDWI)

U<-stack(all_Bub@layers[c(2,7,10,11:13)])
wet_Bub<-stack(U,NDWI_1,NDWI_2,mNDWI)
plot(wet_Bub)
names(wet_Bub)<-c(names(U),"NDWI_1","NDWI_2","mNDWI")

## kmeans classification
### wet Bub

wet_Bub1<-scale(wet_Bub)

values1_wet_Bub<-getValues(wet_Bub1) ## Extract values from each band of S2 image, then scale the values as we don’t want the k-means algorithm to depend to an arbitrary variable unit
i_wet_Bub<-which(!is.na(values1_wet_Bub)) ## k-means can't deal with missing values, so create an vector with the positions without NA
values1_wet_Bub<-na.omit(values1_wet_Bub) ## omit NA values from data
head(values1_wet_Bub)
tail(values1_wet_Bub)

beginCluster()

#### kmeans classification Bub 10 classes
set.seed(6)
E_wet_Bub<-kmeans(values1_wet_Bub,4,iter.max=1000,nstart=25,algorithm="Hartigan-Wong")
E_wet_Bub
E_wet_Bub$ifault
kmeans_raster_wet_Bub<-raster(wet_Bub1)
kmeans_raster_wet_Bub[i_wet_Bub]<-E_wet_Bub$cluster

endCluster()

plot(kmeans_raster_wet_Bub,col=magma(4),main="kmeans 4 wet Bub 20200105",colNA="lightskyblue")
writeRaster(kmeans_raster_wet_Bub,"Data_out/Kmeans/kmeans_4_wet_Bub_20200116.tif",format="GTiff",overwrite=F)


















### all Bub

all_Bub1<-scale(all_Bub)

values1_all_Bub<-getValues(all_Bub1) ## Extract values from each band of S2 image, then scale the values as we don’t want the k-means algorithm to depend to an arbitrary variable unit
i_all_Bub<-which(!is.na(values1_all_Bub)) ## k-means can't deal with missing values, so create an vector with the positions without NA
values1_all_Bub<-na.omit(values1_all_Bub) ## omit NA values from data
head(values1_all_Bub)
tail(values1_all_Bub)

beginCluster()

#### kmeans classification Bub 10 classes
set.seed(6)
E_all_Bub<-kmeans(values1_all_Bub,10,iter.max=1000,nstart=25,algorithm="Hartigan-Wong")
E_all_Bub
E_all_Bub$ifault
kmeans_raster_all_Bub<-raster(all_Bub1)
kmeans_raster_all_Bub[i_all_Bub]<-E_all_Bub$cluster

endCluster()

plot(kmeans_raster_all_Bub,col=magma(10),main="kmeans 10 all Bub 20200116",colNA="lightskyblue")
writeRaster(kmeans_raster_all_Bub,"Data_out/Kmeans/kmeans_10_all_Bub_20200116.tif",format="GTiff",overwrite=F)



##Canhabaque Galinhas

### S2a CanhGa
values1_S2a_CanhGa<-scale(getValues(raster("./Data_out/Intertidal/Intertidal_CanhGa.tif"))) ## Extract values from each band of S2 image, then scale the values as we don’t want the k-means algorithm to depend to an arbitrary variable unit
i_S2a_CanhGa<-which(!is.na(values1_S2a_CanhGa)) ## k-means can't deal with missing values, so create an vector with the positions without NA
values1_S2a_CanhGa<-na.omit(values1_S2a_CanhGa) ## omit NA values from data
head(values1_S2a_CanhGa)
tail(values1_S2a_CanhGa)

beginCluster()

#### kmeans classification CanhGa 10 classes
set.seed(7)
E_S2a_CanhGa<-kmeans(values1_S2a_CanhGa,10,iter.max=1000,nstart=25,algorithm="Hartigan-Wong")
E_S2a_CanhGa
E_S2a_CanhGa$ifault
kmeans_raster_S2a_CanhGa<-raster(raster("./Data_out/Intertidal/Intertidal_CanhGa.tif"))
kmeans_raster_S2a_CanhGa[i_S2a_CanhGa]<-E_S2a_CanhGa$cluster

endCluster()

plot(kmeans_raster_S2a_CanhGa,col=magma(10),main="kmeans 10 S2a CanhGa 20200105",colNA="lightskyblue")
writeRaster(kmeans_raster_S2a_CanhGa,"Data_out/Kmeans/kmeans_10_S2a_CanhGa_20200105.tif",format="GTiff",overwrite=F)


### SAR CanhGa
values1_SAR_CanhGa<-scale(getValues(raster("./Data_out/Intertidal/SAR_CanhGa.tif"))) ## Extract values from each band of S2 image, then scale the values as we don’t want the k-means algorithm to depend to an arbitrary variable unit
i_SAR_CanhGa<-which(!is.na(values1_SAR_CanhGa)) ## k-means can't deal with missing values, so create an vector with the positions without NA
values1_SAR_CanhGa<-na.omit(values1_SAR_CanhGa) ## omit NA values from data
head(values1_SAR_CanhGa)
tail(values1_SAR_CanhGa)

beginCluster()

#### kmeans classification CanhGa 10 classes
set.seed(8)
E_SAR_CanhGa<-kmeans(values1_SAR_CanhGa,5,iter.max=1000,nstart=25,algorithm="Hartigan-Wong")
E_SAR_CanhGa
E_SAR_CanhGa$ifault
kmeans_raster_SAR_CanhGa<-raster(raster("./Data_out/Intertidal/SAR_CanhGa.tif"))
kmeans_raster_SAR_CanhGa[i_SAR_CanhGa]<-E_SAR_CanhGa$cluster

endCluster()

plot(kmeans_raster_SAR_CanhGa,col=magma(5),main="kmeans 5 SAR CanhGa 20200116",colNA="lightskyblue")
writeRaster(kmeans_raster_SAR_CanhGa,"Data_out/Kmeans/kmeans_5_SAR_CanhGa_20200116.tif",format="GTiff",overwrite=F)


### all CanhGa
values1_all_CanhGa<-scale(getValues(raster("./Data_out/Stack/stack_all_CanhGa.tif"))) ## Extract values from each band of S2 image, then scale the values as we don’t want the k-means algorithm to depend to an arbitrary variable unit
i_all_CanhGa<-which(!is.na(values1_all_CanhGa)) ## k-means can't deal with missing values, so create an vector with the positions without NA
values1_all_CanhGa<-na.omit(values1_all_CanhGa) ## omit NA values from data
head(values1_all_CanhGa)
tail(values1_all_CanhGa)

beginCluster()

#### kmeans classification CanhGa 10 classes
set.seed(9)
E_all_CanhGa<-kmeans(values1_all_CanhGa,7,iter.max=1000,nstart=25,algorithm="Hartigan-Wong")
E_all_CanhGa
E_all_CanhGa$ifault
kmeans_raster_all_CanhGa<-raster(raster("./Data_out/Stack/stack_all_CanhGa.tif"))
kmeans_raster_all_CanhGa[i_all_CanhGa]<-E_all_CanhGa$cluster

endCluster()

plot(kmeans_raster_all_CanhGa,col=magma(7),main="kmeans 7 all CanhGa 20200116",colNA="lightskyblue")
writeRaster(kmeans_raster_all_CanhGa,"Data_out/Kmeans/kmeans_7_all_CanhGa_20200116.tif",format="GTiff",overwrite=F)



##Bolama

### S2a Bolama
values1_S2a_Bolama<-scale(getValues(raster("./Data_out/Intertidal/Intertidal_Bolama.tif"))) ## Extract values from each band of S2 image, then scale the values as we don’t want the k-means algorithm to depend to an arbitrary variable unit
i_S2a_Bolama<-which(!is.na(values1_S2a_Bolama)) ## k-means can't deal with missing values, so create an vector with the positions without NA
values1_S2a_Bolama<-na.omit(values1_S2a_Bolama) ## omit NA values from data
head(values1_S2a_Bolama)
tail(values1_S2a_Bolama)

beginCluster()

#### kmeans classification Bolama 10 classes
set.seed(7)
E_S2a_Bolama<-kmeans(values1_S2a_Bolama,10,iter.max=1000,nstart=25,algorithm="Hartigan-Wong")
E_S2a_Bolama
E_S2a_Bolama$ifault
kmeans_raster_S2a_Bolama<-raster(raster("./Data_out/Intertidal/Intertidal_Bolama.tif"))
kmeans_raster_S2a_Bolama[i_S2a_Bolama]<-E_S2a_Bolama$cluster

endCluster()

plot(kmeans_raster_S2a_Bolama,col=magma(10),main="kmeans 10 S2a Bolama 20200105",colNA="lightskyblue")
writeRaster(kmeans_raster_S2a_Bolama,"Data_out/Kmeans/kmeans_10_S2a_Bolama_20200105.tif",format="GTiff",overwrite=F)


### SAR Bolama
values1_SAR_Bolama<-scale(getValues(raster("./Data_out/Intertidal/SAR_Bolama.tif"))) ## Extract values from each band of S2 image, then scale the values as we don’t want the k-means algorithm to depend to an arbitrary variable unit
i_SAR_Bolama<-which(!is.na(values1_SAR_Bolama)) ## k-means can't deal with missing values, so create an vector with the positions without NA
values1_SAR_Bolama<-na.omit(values1_SAR_Bolama) ## omit NA values from data
head(values1_SAR_Bolama)
tail(values1_SAR_Bolama)

beginCluster()

#### kmeans classification Bolama 10 classes
set.seed(8)
E_SAR_Bolama<-kmeans(values1_SAR_Bolama,5,iter.max=1000,nstart=25,algorithm="Hartigan-Wong")
E_SAR_Bolama
E_SAR_Bolama$ifault
kmeans_raster_SAR_Bolama<-raster(raster("./Data_out/Intertidal/SAR_Bolama.tif"))
kmeans_raster_SAR_Bolama[i_SAR_Bolama]<-E_SAR_Bolama$cluster

endCluster()

plot(kmeans_raster_SAR_Bolama,col=magma(5),main="kmeans 5 SAR Bolama 20200116",colNA="lightskyblue")
writeRaster(kmeans_raster_SAR_Bolama,"Data_out/Kmeans/kmeans_5_SAR_Bolama_20200116.tif",format="GTiff",overwrite=F)


### all Bolama
values1_all_Bolama<-scale(getValues(raster("./Data_out/Stack/stack_all_Bolama.tif"))) ## Extract values from each band of S2 image, then scale the values as we don’t want the k-means algorithm to depend to an arbitrary variable unit
i_all_Bolama<-which(!is.na(values1_all_Bolama)) ## k-means can't deal with missing values, so create an vector with the positions without NA
values1_all_Bolama<-na.omit(values1_all_Bolama) ## omit NA values from data
head(values1_all_Bolama)
tail(values1_all_Bolama)

beginCluster()

#### kmeans classification Bolama 10 classes
set.seed(9)
E_all_Bolama<-kmeans(values1_all_Bolama,7,iter.max=1000,nstart=25,algorithm="Hartigan-Wong")
E_all_Bolama
E_all_Bolama$ifault
kmeans_raster_all_Bolama<-raster(raster("./Data_out/Stack/stack_all_Bolama.tif"))
kmeans_raster_all_Bolama[i_all_Bolama]<-E_all_Bolama$cluster

endCluster()

plot(kmeans_raster_all_Bolama,col=magma(7),main="kmeans 7 all Bolama 20200116",colNA="lightskyblue")
writeRaster(kmeans_raster_all_Bolama,"Data_out/Kmeans/kmeans_7_all_Bolama_20200116.tif",format="GTiff",overwrite=F)










