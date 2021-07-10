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

## Load polygons with info on concluded sediment so far 20201113
#system.time(GT<-readOGR("Data_out/Polygons/Poly_GT_Gra_ended_20201126.shp"))
GT<-readOGR("Data_out/Polygons/Poly_GT_Gra_ended_20210701.shp") # created in script Granulometry_test.R
#plot(GT, add=F, col="red")
##Load GT polygons
#GT_c1<-readOGR("Data_out/Polygons/GT_c1.shp") ##created in script Data_cleanup_SUp_Class
## Load exposure model
#bat_bub<-raster("C:/Doutoramento1/Digital elevation and exposure time model/dem_104_469.tif")
#crs(bat_bub)
#plot(bat_bub)

#bat_bol<-raster("rasters_in/mask_int_02.tif")
#plot(bat_bol,add=F)
#plot(GT,col="red",add=T)

### Load intertidal mask (created in script DEM_based_intertidalmask_creation)
intmask<-raster("Data_out/mask/final_mask_20210710.tif")
plot(intmask,colNA=1)

## Mask bathymetry
### Urok
#### resample mask to allow use with bat image
#mask_urok_bat_r<-resample(mask_urok,bat_Urok,method="bilinear")
#plot(mask_urok_bat_r,col=magma(2),colNA=1)
#Intertidal_bat_urok<-bat_Urok*mask_urok_bat_r

### Crop GT to keep only points in the available scene
GT_c<-crop(GT,intmask)
#plot(GT_c, add=T, col="red")


## Load other images
###Load S1 image
s1_20200128<-stack("D:/Work/FCUL/Doutoramento/Capitulos/Mapping_intertidal_sediments/SNAP/S1/20200128_1917/S1A_IW_GRDH_1SDV_20200128T191708_20200128T191733_031005_038FAD_B2A7_Orb_TNR_BN_Cal_Spk_TC_DEM05.tif")
#plot(s1_20200128)

S1_c<-crop(s1_20200128,intmask)
names(S1_c)<-c("S1_20200128_VH","S1_20200128_VV")
#plot(S1_c)


##Load S2 images 20200204
path_bub<-"C:/Doutoramento1/Capitulos/Mapping_intertidal_sediments/Satellite_images/Sentinel2/Resampled/Bubaque/im_20200204/2A"
files_bub<-list.files(path=path_bub,pattern=".tif",full.names = T)
S2_20200204_bub<-stack(files_bub)
#S2_20200204<-raster(paste(path,files[1],sep="/"))

#for(i in 2:length(files)) {
    #S2_20200204<-stack(S2_20200204,paste(path,files[i],sep="/"))
  #}

names(S2_20200204_bub)
#plot(S2_20200204_bub)

path_bol<-"C:/Doutoramento1/Capitulos/Mapping_intertidal_sediments/Satellite_images/Sentinel2/Resampled/Bolama/im_20200204/2A"
files_bol<-list.files(path=path_bol,pattern=".tif",full.names = T)
S2_20200204_bol<-stack(files_bol)
names(S2_20200204_bol)
#plot(S2_20200204_bol)

## Convert to true reflectances
beginCluster()
S2_20200204_bub_t<- S2_20200204_bub/10000
endCluster()
S2_20200204_bub_t
plot(S2_20200204_bub_t)

beginCluster()
S2_20200204_bol_t<- S2_20200204_bol/10000
endCluster()
S2_20200204_bol_t
plot(S2_20200204_bol_t)

### unite the 2 sentinel2 scenes
S2_20200204_tot<-merge(S2_20200204_bub_t,S2_20200204_bol_t,overlap=T)
plot(S2_20200204_tot)

names(S2_20200204_bub_t)
names(S2_20200204_bol_t)

names(S2_20200204_tot)<-names(S2_20200204_bub_t)

beginCluster(6)
S2_20200204_tot_c<-crop(S2_20200204_tot,intmask)
endCluster()
plot(S2_20200204_tot_c)


## Check extent of images to prepare to stack all together

#extent(bat)==extent(intmask)
extent(S2_20200204_tot_c)==extent(intmask)
extent(S1_c)==extent(intmask)

## Resample sat image to enable stacking
beginCluster(6)
S1_cr<-resample(S1_c,intmask,method="bilinear")

#plot(S1_cr)
#S2_20200204_tot_r<-resample(S2_20200204_tot,intmask,method="bilinear")
#plot(S1_cr)
endCluster()
#extent(S2_20200204_tr)==extent(intmask)
extent(S1_cr)==extent(intmask)
plot(S1_cr)

##Stack images
all<-stack(S2_20200204_tot_c,S1_cr)
#plot(all)

## mask intertidal area
beginCluster(6)
all_m<-mask(all,intmask)
endCluster()
beep(2)
plot(all_m)

##Create index layers
NDWI<-(all_m$B03_20200204-all_m$B08_20200204)/(all_m$B03_20200204+all_m$B08_20200204)
mNDWI<-(all_m$B03_20200204-all_m$B11_20200204)/(all_m$B03_20200204+all_m$B11_20200204)
NDMI<-(all_m$B08_20200204-all_m$B11_20200204)/(all_m$B08_20200204+all_m$B11_20200204)
NDMI1<-(all_m$B08A_20200204-all_m$B11_20200204)/(all_m$B08A_20200204+all_m$B11_20200204)
NDVI<-(all_m$B08_20200204-all_m$B04_20200204)/(all_m$B08_20200204+all_m$B04_20200204)
RVI<-(4*all_m$S1_20200128_VH)/(all_m$S1_20200128_VV+all_m$S1_20200128_VH)
VH_VV<-(all_m$S1_20200128_VH)/(all_m$S1_20200128_VV)
MSAVI2<-(2*all_m$B08_20200204+1-sqrt((2*all_m$B08_20200204+1)^2-8*(all_m$B08_20200204-all_m$B04_20200204)))/2
intensity<-1/30.5*(all_m$B02_20200204+all_m$B03_20200204+all_m$B04_20200204)
#visible_multi<-all_m$B02_20200204*all_m$B03_20200204*all_m$B04_20200204
rededge_multi<-all_m$B05_20200204*all_m$B06_20200204*all_m$B07_20200204
iv_multi<-all_m$B08_20200204*all_m$B08A_20200204
beep(3)

#plot(NDWI)
#plot(mNDWI)
#plot(NDMI)
#plot(NDMI1)
#plot(NDVI)
#plot(RVI)
#plot(VH_VV)
#plot(MSAVI2)



sat1<-stack(all_m,NDWI,mNDWI,NDMI,NDMI1,NDVI,RVI,VH_VV,MSAVI2,intensity,rededge_multi,iv_multi)
sat<-sat1[[-9]] ##remove band 9, cirrus cloud, not relevant
names(sat)[13:23]<-c("NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","MSAVI2","intensity","rededge_multi","iv_multi")



#plot(sat)
writeRaster(sat,"Data_out/Stack/Final_stack1.tif",format="GTiff",overwrite=F)
writeRaster(sat,"Data_out/Stack/Final_stack1.grd",format="raster",overwrite=F)

