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
library(RColorBrewer)
library(viridis)
library(ggfortify)
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)

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

## Load VV s1 image
S1_VV_db<-stack("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Sat_img/s1/S1A_IW_GRDH_1SDV_20190310T1917_Orb_TN_Bdr_Cal_Spk_TC_Catalao_Sigma0_VV_db.tif")
S1_VV_db

names(S1_VV_db)<-c("B1","B2","B3","B4")## change names of layers
ggR(S1_VV_db[[1]])
hist(S1_VV_db[[1]], breaks=100)
rasterVis::levelplot(S1_VV_db)
rasterVis::densityplot(S1_VV_db)
splom(S1_VV_db)

## Keep just one of the layers: layers 1,2 and 3 are the same, layer 4 is wierd
VV<-S1_VV_db$B1
ggR(batim_r)
extent(VV)==extent(batim_r)

## crop batim with adonga polygon to mask s1 sat image
batim_ad<-crop(batim,Adon_b)
plot(batim_ad)

## resample batimetry to downsize to st img size 
batim_VV<-resample(batim_ad,VV, method="bilinear")
extent(VV)==extent(batim_VV)
VV_m<-mask(VV,batim_VV)
ggR(VV_m)
levelplot(VV_m)

## Load VH band
S1_VH_db<-stack("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Sat_img/s1/S1A_IW_GRDH_1SDV_20190310T1917_Orb_TN_Bdr_Cal_Spk_TC_Catalao_Sigma0_VH_db.tif")
S1_VH_db  

names(S1_VH_db)<-c("B1","B2","B3","B4") ## change names of layers

VH<-S1_VH_db$B1
ggR(VH)

## resample batimetry to downsize to st img size 
batim_VH<-resample(batim_ad,VH, method="bilinear")
extent(VH)==extent(batim_VH)
ggR(batim_VH)
ggR(VH)

VH_m<-mask(VH,batim_VH)
ggR(VH_m)
levelplot(VH_m)

## Create a layer with the ratio between VV and VH

extent(VV_m) == extent(VH_m) #Objects have the same extent

VV_VH<-VH_m/VV_m
VV_VH
levelplot(VV_VH)


## stack the 2 polarisation images
radar<-stack(VV_m,VH_m)
ggRGB(radar,1,2,1,stretch = "lin")
radar_r<-resample(radar,S2_20190316_ad) ###resample radar images to size down/up to S2 images
all<-stack(radar_r,S2_20190316_ad) ### stack all the satellite images

# preform a PCA on sentinel images

## All sat bands
beginCluster()
### Raster PCA
allPCA<-rasterPCA(all,nComp=nlayers(all),maskCheck=T,scale=T)

### PCA just for biplot
all_s<-sampleRandom(all,200000) # sample 200000 random points of the raster values of each of the bands of the stack
allPCA1<-prcomp(all_s,scale=T) # preform a PCA over the random sample

#ggbiplot(allPCA1,obs.scale=1,var.scale=1,labels=row.names(all_s),alpha=.2,circle=F,var.axes=T,varname.size=5,ellipse=T)
#allPCA1p<-predict(all,allPCA1,index=1:6)
#### Biplot of the random sample PCA
autoplot(allPCA1,loadings=T,loadings.colour ="blue",loadings.label=T,loadings.label.size=6,loadings.label.colour="red",alpha=.2)

endCluster()
summary(allPCA$model)
loadings(allPCA$model)

#my.palette <- colorspace::rainbow_hcl(100)
plot(allPCA$map$PC1,main="PCA Adonga",col=magma(50))
plotRGB(allPCA$map,r=1,g=2,b=3,axes=F,colNA="black", stretch="lin",main="PCA all", col=magma(20))

## ONly S2 PCA
beginCluster()

S2_s<-sampleRandom(S2_20190316_ad,200000)
S2PCA1<-prcomp(S2_s,scale=T)

autoplot(S2PCA1,loadings=T,loadings.colour ="blue",loadings.label=T,loadings.label.size=6,loadings.label.colour="red",alpha=.2)

S2PCA<-rasterPCA(scale(S2_20190316_ad),nComp=nlayers(S2_20190316_ad),maskCheck=T)
endCluster()
summary(S2PCA$model)
loadings(S2PCA$model)

plot(S2PCA$map$PC1*-1, main="S2 PCA Adonga", col=magma(50))
plotRGB(S2PCA$map,r=1,g=2,b=3,axes=T,colNA="black", stretch="lin",main="PCA S2", col=my.palette)

## Only S1 PCA
beginCluster()

S1_s<-sampleRandom(radar,200000)
S1PCA1<-prcomp(S1_s,scale=F)

autoplot(S1PCA1,loadings=T,loadings.colour ="blue",loadings.label=T,loadings.label.size=6,loadings.label.colour="red",alpha=.2)

radarPCA<-rasterPCA(radar_r,nComp=nlayers(radar_r),maskCheck=T)
endCluster()

summary(radarPCA$model)
loadings(radarPCA$model)

plot(radarPCA$map$PC1, main="S1 PCA Adonga", col=magma(20))
plotRGB(radarPCA$map,r=1,g=2,b=1,axes=T,colNA="black", stretch="lin",main="PCA radar", col=my.palette)

# Criar base de dados com valores do raster

## Importar poligonos de ground truthing
### shapes de treino
treino<-readOGR("./Shapefiles/Areas_treino/t1.shp")
plot(treino,add=T,col="red")
treino_df<-as(treino,"data.frame")
write.table(treino_df,"./Data_out/treino_df.csv",row.names = F,sep=";")
treino@data

### shapes de validacao
validacao<-readOGR("./Shapefiles/Areas_validacao/v1.shp")
plot(validacao,add=T,col="green")

## Unite validation and training shapes
GT<-union(treino,validacao)
plot(GT, add=T,col="blue")

## create a new column with de ID of each polygon
GT@data$RID<-as.factor(c(as.character(GT@data$name.1[!is.na(GT@data$name.1)]),as.character(GT@data$name.2[!is.na(GT@data$name.2)])))
GT_df<-as(GT,"data.frame")

## Extract raster pixel values 
v<-extract(all,GT,df=F,method="bilinear")

## Add results to polygons data, since it will be ordered with the same order of the polygons
GT@data<-data.frame(GT@data,do.call(rbind, lapply(v, colMeans)))
DF<-as(GT,"data.frame")

#write.table(DF,"./Data_out/DF.csv",sep=";",row.names = F)

#unsupervised classification for radar

values1_Adongabancos_1<-getValues(radar)
i_Adongabancos_1<-which(!is.na(values1_Adongabancos_1))
values1_Adongabancos_1<-na.omit(values1_Adongabancos_1)
head(values1_Adongabancos_1)
tail(values1_Adongabancos_1)

beginCluster()
## kmeans classification adongabancos
E_Adongabancos_1<-kmeans(values1_Adongabancos_1, 5, iter.max = 500, nstart = 25)
kmeans_raster_Adongabancos_1 <- raster(radar)
kmeans_raster_Adongabancos_1[i_Adongabancos_1]<-E_Adongabancos_1$cluster

endCluster()

plot(kmeans_raster_Adongabancos_1, col=magma(5), main="kmeans_radar_Adongabancos_1", colNA="grey30")
#writeRaster(kmeans_raster_Adongabancos_1, "./products/kmeans_adongabancos_161218.tif")







