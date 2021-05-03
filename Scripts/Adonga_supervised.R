setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

packs<-c("RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis")
lapply(packs,require,character.only=T)

#Preparar imagens satelite S2 20190316

##List files for sentinel 2a image import
files<-dir(path="D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Sat_img/s2/S2A_MSIL2A_20190316T112111_N0211_R037_T28PCT_20190316T140635.SAFE",pattern=glob2rx("*_B*10m*.jp2*"),recursive=T,full.names = T)

## Import images with native 10m resolution
S2<-stack(files)
S2
names(S2)
names(S2)<-c("B02","B03","B04","B08") #Change image names

plotRGB(S2,3,2,1,stretch="lin")

## Import images resampled to 10m with SNAP
S2_resamp<-stack("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Sat_img/s2/resampled/S2A_MSIL2A_20190316T1121_resampled10_subset5_6_7_8a_9_11_12.tif")
names(S2_resamp)<-c("B05","B06","B07","B08a","B09","B11","B12")

## Merge all S2 bands together
S2_f<-stack(S2[[1:3]],S2_resamp[[1:3]],S2[[4]],S2_resamp[[4:nlayers(S2_resamp)]])
S2_f


# Load sentinel 1 image 20190310 in decibels

## Load VV s1 image
S1_VV_db<-stack("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Sat_img/s1/S1A_IW_GRDH_1SDV_20190310T1917_Orb_TN_Bdr_Cal_Spk_TC_Catalao_Sigma0_VV_db.tif")
S1_VV_db


# bathymetry map
bat<-raster("D:/Work/FCUL/Doutoramento/Capitulos/Mapping_intertidal_sediments/Jcatalao/bijagos_batim.tif")
plot(bat)


#Crop to adonga area

##Load poligon made in QGIS
Adon<-readOGR("./Shapefiles/Adonga_bancos/poligono1.shp")
plot(Adon, add=T, col="red")

## crop S2 image
S2_Ad<-crop(S2_f,Adon)
ggRGB(S2_Ad,3,2,1,stretch="lin")

## crop bat
bat_Ad<-crop(bat,Adon)
plot(bat_Ad)

## crop S1 image
S1_Ad<-crop(S1,Adon)
plot(S1_Ad)

# MAsk out land and water using bat
S2_Inter<-mask(S2_Ad,bat_Ad)

# Stack all bands together

if(extent(S2_Ad)==extent(bat_Ad)==extent(S1_Ad)){
  all_Ad<-stack(S2_Ad,S1_Ad,bat_Ad)
  print("extents match, just stacked it")
} else {
  S1_Ad_r<-resample(S1_Ad,S2_Ad)
  bat_Ad_r<-resample(bat_Ad,S2_Ad)
  if(extent(S2_Ad_r)==extent(bat_Ad_r)==extent(S1_Ad_r)){
    all_Ad<-stack(S2_Ad,S1_Ad_r,bat_Ad_r)
    print("extents don't match, resampled and solved")
  } else{
    print("Shit went south. Try again")
  }
}


## check the extents of the two layers -- if they are different crop both datasets
if (extent(all_Ad) == extent(bat_Ad)){
  print("Extents are the same, no need to crop")
  all_Inter<-mask(all_Ad,bat_Ad)
} else {
  # calculate overlap between the two datasets
  overlap<-intersect(extent(all_Ad),extent(bat_Ad))
  # now let's crop both datasets to the overlap region
  all_Ad_c<-crop(all_Ad,overlap)
  bat_Ad_c<-crop(bat_Ad,overlap)
  if (extent(all_Ad_c) == extent(bat_Ad_c)){
    print("Extents are different, data cropped and solved")
    all_Inter<-mask(all_Ad_c,bat_Ad_c)
  } else {
    # resample bat using sat image
    bat_Ad_r<-resample(bat_Ad,all_Ad,method="bilinear")
    if (extent(all_Ad)==extent(bat_Ad_r)){
      print("Cropping failed. bat resampled, solved.")
      all_Inter<-mask(all_Ad,bat_Ad_r)
    } else {
      print("Failed, try another thing")
    }
  }
  
}


# Load training polygons

### shapes de treino
treino<-readOGR("./Shapefiles/Areas_treino/t1.shp")
plot(treino,add=F,col="red")
treino_df<-as(treino,"data.frame")
treino@data

### shapes de validacao
validacao<-readOGR("./Shapefiles/Areas_validacao/v1.shp")
plot(validacao,add=T,col="green")
