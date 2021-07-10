rm(list=ls())
graphics.off()
OS <- .Platform$OS.type
if (OS == "windows"){
  setwd("C:/Doutoramento1/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos") # Windows file path
  print(paste("working on",OS,getwd()))
} else if (OS == "unix"){
  setwd("/Users/MohamedHenriques/Work/R/Map_InterSedim_Bijagos") # MAC file path
  print(paste("working on",OS,getwd()))
} else {
  print("ERROR: OS could not be identified")
}

packs<-c("sf","beepr","RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis","data.table","reshape2")
npacks <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(npacks)) install.packages(npacks)
#install_github("vqv/ggbiplot")
lapply(packs,require,character.only=T)

## Load exposure model
bat_bub<-raster("C:/Doutoramento1/DEM_JP_JB/Bubaque/dem_0_5.tif")
crs(bat_bub)

plot(bat_bub)

bat_bol<-raster("C:/Doutoramento1/DEM_JP_JB/Bolama/dem_cut_104_469.tif")
plot(bat_bol,add=F)

##clean bolama mask
pol_bol<-readOGR("Data_in/shapefiles/bolama_cut.shp")
crs(pol_bol)
pol_bol1<-spTransform(pol_bol,crs(bat_bol))
plot(pol_bol1,add=T,col="red")

bat_bol1<-mask(bat_bol,pol_bol1,inverse=T)
plot(bat_bol1)

### merge both bathymetries
plot(bat_bub)
plot(bat_bol1,add=T)

beginCluster()
bat<-merge(bat_bub,bat_bol1,overlap=T)
#bat<-mosaic(bat_bub,bat_bol1,fun=mean)
endCluster()
plot(bat)

## clean bat
pol_land<-readOGR("Data_in/shapefiles/land_clean.shp")
crs(pol_land)
pol_land1<-spTransform(pol_land,crs(bat))
bat1<-mask(bat,pol_land1,inverse=T)
plot(bat1)

##cut to AOI
x<-drawExtent()
bat2<-crop(bat1,x)
plot(bat2)

writeRaster(bat2,"Data_out/DEM/Final_DEM/Final_DEM_nodelay.tif",format="GTiff",overwrite=F)

##Load S1 image
s1_20200128<-brick("D:/Work/FCUL/Doutoramento/Capitulos/Mapping_intertidal_sediments/SNAP/S1/20200128_1917/S1A_IW_GRDH_1SDV_20200128T191708_20200128T191733_031005_038FAD_B2A7_Orb_TNR_BN_Cal_Spk_TC_DEM05.tif")
plot(s1_20200128)

### Remember to repeat S1 processing in SNAP including the all mask this time
S1_c<-crop(s1_20200128,bat1)
plot(S1_c)
S1_VH<-S1_c$S1A_IW_GRDH_1SDV_20200128T191708_20200128T191733_031005_038FAD_B2A7_Orb_TNR_BN_Cal_Spk_TC_DEM05.1
S1_VV<-S1_c$S1A_IW_GRDH_1SDV_20200128T191708_20200128T191733_031005_038FAD_B2A7_Orb_TNR_BN_Cal_Spk_TC_DEM05.2
plot(S1_VV)
plot(S1_VH)

###Calculate hight of water at acquisition time for S1 sat image
hHW<-4.2
hLW<-1.0
Tsat<-(19*60+17)
TLW<-(18*60+32)
THW<-(24*60+54)

hsat_S1<-hHW-((hHW-hLW)*(cos((pi*(Tsat-TLW)/(THW-TLW)))+1))/2
hsat_S1

##Load S2 images 20200204
path<-"C:/Doutoramento1/Capitulos/Mapping_intertidal_sediments/Satellite_images/Sentinel2/Resampled/Bubaque/im_20200204/2A"
files<-list.files(path=path,pattern=".tif",full.names = T)
S2_20200204<-stack(files)
#S2_20200204<-raster(paste(path,files[1],sep="/"))
#system.time(
  #for(i in 2:length(files)) {
  #S2_20200204<-stack(S2_20200204,paste(path,files[i],sep="/"))
#}
#)

plot(S2_20200204)

###Calculate height of water at acquisition time for S2 sat image
hHW<-3.5
hLW<-1.5
Tsat<-(11*60+22)
TLW<-(11*60+58)
THW<-(05*60+45)

hsat_S2<-hHW-((hHW-hLW)*(cos((pi*(Tsat-TLW)/(THW-TLW)))+1))/2
hsat_S2

##### Filter out pixels in DEM to use in the mask based on the hight of sat images - using as reference the hight of S2 image
plot(bat2)
bat3<-bat2>1.55
plot(bat3)

bat3[bat3==0]<-NA
#bat3[bat3>0]<-1
plot(bat3)

##Export final mask
writeRaster(bat3,"Data_out/mask/final_mask_20210710.tif")


