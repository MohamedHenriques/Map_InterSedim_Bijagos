setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

packs<-c("sf","beepr","RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis","data.table","reshape2")
lapply(packs,require,character.only=T)

## Load polygons with info on concluded sediment so far 20201113
#system.time(GT<-readOGR("Data_out/Polygons/Poly_GT_Gra_ended_20201126.shp"))
GT<-readOGR("Data_out/Polygons/Poly_GT_Gra_ended_20210113.shp") # much faster this way
#plot(GT, add=F, col="red")

## Load exposure model
bat<-raster("D:/Work/FCUL/Doutoramento/Digital elevation and exposure time model/dem_104_469.tif")
#crs(bat)
#plot(bat)


#########Jump this, since we're aiming to do this for all the area now ##############################################

###Cut bat and GT polygons for Urok area
Urok<-readOGR(dsn="Shapefiles/Urok_shape",layer="Urok_shapes")
crs(Urok)<-crs(bat)
#plot(Urok, add=T, col="red")

bat_Urok<-crop(bat,Urok)
#plot(bat_Urok)

GT_Urok<-crop(GT,Urok)
plot(GT_Urok, col="red", add=F)
######################################################################################################################
### Load intertidal mask (created in script DEM_based_intertidalmask_creation)
intmask<-raster("Data_out/mask/final_mask_20210113.tif")
#plot(intmask,colNA=1)

## Mask bathymetry
### Urok
#### resample mask to allow use with bat image
#mask_urok_bat_r<-resample(mask_urok,bat_Urok,method="bilinear")
#plot(mask_urok_bat_r,col=magma(2),colNA=1)
#Intertidal_bat_urok<-bat_Urok*mask_urok_bat_r

### Crop GT to keep only points in the available scene
GT_c<-crop(GT,intmask)
#plot(GT_c)


## Load other images
###Load S1 image
s1_20200128<-stack("D:/Work/FCUL/Doutoramento/Capitulos/Mapping_intertidal_sediments/SNAP/S1/20200128_1917/S1A_IW_GRDH_1SDV_20200128T191708_20200128T191733_031005_038FAD_B2A7_Orb_TNR_BN_Cal_Spk_TC_DEM05.tif")
#plot(s1_20200128)

S1_c<-crop(s1_20200128,intmask)
names(S1_c)<-c("S1_20200128_VH","S1_20200128_VV")
#plot(S1_c)


##Load S2 images 20200204
path<-"D:/Work/FCUL/Doutoramento/Capitulos/Mapping_intertidal_sediments/Satellite_images/Sentinel2/Resampled/Bubaque/im_20200204/2A"
files<-list.files(path=path,pattern=".tif")
S2_20200204<-raster(paste(path,files[1],sep="/"))

for(i in 2:length(files)) {
    S2_20200204<-stack(S2_20200204,paste(path,files[i],sep="/"))
  }

S2_20200204
names(S2_20200204)
#beep()

#plot(S2_20200204)

## Check extebt of images to prepare tp stack all together

extent(bat)==extent(intmask)
extent(S2_20200204)==extent(intmask)
extent(S1_c)==extent(intmask)

## Resample sat image to enable stacking
S1_cr<-resample(S1_c,intmask,method="bilinear")
#plot(S1_cr)
extent(S1_cr)==extent(intmask)

##Stack images
all<-stack(S2_20200204,S1_cr,bat)
#plot(all)

## mask intertidal area
beginCluster(7)
all_m<-mask(all,intmask)
endCluster()
beep()

##Create index layers
NDWI<-(all_m$B03_20200204-all_m$B08_20200204)/(all_m$B03_20200204+all_m$B08_20200204)
mNDWI<-(all_m$B03_20200204-all_m$B11_20200204)/(all_m$B03_20200204+all_m$B11_20200204)
NDMI<-(all_m$B08_20200204-all_m$B11_20200204)/(all_m$B08_20200204+all_m$B11_20200204)
NDMI1<-(all_m$B08A_20200204-all_m$B11_20200204)/(all_m$B08A_20200204+all_m$B11_20200204)
beep()

plot(NDWI)
plot(mNDWI)
plot(NDMI)
plot(NDMI1)

all_m1<-stack(all_m,NDWI,mNDWI,NDMI,NDMI1)
names(all_m1)[15:18]<-c("NDWI","mNDWI","NDMI","NDMI1")

#plot(all_m1)
writeRaster(all_m1,"Data_out/Stack/Final_stack.tif",format="GTiff",overwrite=F)

## Extract values 
beginCluster(7)
system.time(DF<-extract(all_m,GT_c,cellnumbers=T,df=F,factors=T,nl=15,na.rm=T))
beep()
endCluster()



##Remove points that do not fall into Urok area
#names(DF)<-seq_along(DF)
#DF1<-Filter(Negate(is.null), DF)
Point<-seq_along(DF)

###Unpack list and ppend it to a data frame
n<-c(colnames(DF[[1]]),colnames(GT_c@data))
m<-matrix(data=NA,nrow=1,ncol=length(n))
colnames(m)<-n

for(i in Point) {
  m<-rbind(m,cbind(DF[i],GT_c@data[i,]))
  print(paste(i,"done"))
}

m1<-m[-1,] ###remove first row of NAs

##Check for pixels that fall outside intertidal masked area
unique(m1[is.na(m1$B02_20200204),"Point"])

write.table(m1,"Data_out/db/GraVSSat_db_20210114.csv",row.names=F,sep=";")


