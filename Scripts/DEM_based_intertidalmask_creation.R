setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

packs<-c("sf","beepr","RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis","data.table","reshape2")
lapply(packs,require,character.only=T)

## Load exposure model
bat<-raster("D:/Work/FCUL/Doutoramento/Digital elevation and exposure time model/dem_0_5.tif")
crs(bat)

plot(bat)

##Load S1 image
s1_20200128<-brick("D:/Work/FCUL/Doutoramento/Capitulos/Mapping_intertidal_sediments/SNAP/S1/20200128_1917/S1A_IW_GRDH_1SDV_20200128T191708_20200128T191733_031005_038FAD_B2A7_Orb_TNR_BN_Cal_Spk_TC_DEM05.tif")
plot(s1_20200128)

S1_c<-crop(s1_20200128,bat)
plot(S1_c)
S1_VH<-S1_c$S1A_IW_GRDH_1SDV_20200128T191708_20200128T191733_031005_038FAD_B2A7_Orb_TNR_BN_Cal_Spk_TC_DEM05.1
S1_VV<-S1_c$S1A_IW_GRDH_1SDV_20200128T191708_20200128T191733_031005_038FAD_B2A7_Orb_TNR_BN_Cal_Spk_TC_DEM05.2
plot(S1_VV)

###Calculate hight of water at acquisition time for S1 sat image
hHW<-4.2
hLW<-1.0
Tsat<-(19*60+17)
TLW<-(18*60+32)
THW<-(24*60+54)

hsat<-hHW-((hHW-hLW)*(cos((pi*(Tsat-TLW)/(THW-TLW)))+1))/2
hsat

##Load S2 images 20200204
path<-"D:/Work/FCUL/Doutoramento/Capitulos/Mapping_intertidal_sediments/Satellite_images/Sentinel2/Resampled/Bubaque/im_20200204/2A"
files<-list.files(path=path,pattern=".tif")
S2_20200204<-raster(paste(path,files[1],sep="/"))
system.time(
  for(i in 2:length(files)) {
  S2_20200204<-stack(S2_20200204,paste(path,files[i],sep="/"))
}
)

S2_20200204
beep()

plot(S2_20200204)

###Calculate hight of water at acquisition time for S2 sat image
hHW<-3.5
hLW<-1.5
Tsat<-(11*60+22)
TLW<-(11*60+58)
THW<-(05*60+45)

hsat<-hHW-((hHW-hLW)*(cos((pi*(Tsat-TLW)/(THW-TLW)))+1))/2
hsat

##### Filter out pixels in DEM to use in the mask based on the hight of sat images - using as reference the hight of S2 image
bat
bat1<-bat>1.55
bat1
plot(bat1)

bat1[bat1==0]<-NA
plot(bat1)

##Export final mask
writeRaster(bat1,"Data_out/mask/final_mask_20210113.tif")


