setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

packs<-c("sf","beepr","RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis","data.table","reshape2")
lapply(packs,require,character.only=T)

## Load polygons with info on concluded sediment so far 20201113
#system.time(GT<-readOGR("Data_out/Polygons/Poly_GT_Gra_ended_20201126.shp"))
GT<-readOGR("Data_out/Polygons/Poly_GT_Gra_ended_20201126.shp") # much faster this way

plot(GT)


## Load exposure model
bat<-raster("D:/Work/FCUL/Doutoramento/Joao_Belo/Exposure_model/tempo_exposicao_Bijagos.tif")
crs(bat)

#plot(bat)

###Cut bat and GT polygons for Urok area
Urok<-readOGR(dsn="Shapefiles/Urok_shape",layer="Urok_shapes")
crs(Urok)<-crs(bat)
#plot(Urok, add=T, col="red")

bat_Urok<-crop(bat,Urok)
#plot(bat_Urok)

GT_Urok<-crop(GT,Urok)
plot(GT_Urok, col="red", add=F)


## Mask bathymetry
### Urok
#### resample mask to allow use with bat image
#mask_urok_bat_r<-resample(mask_urok,bat_Urok,method="bilinear")
#plot(mask_urok_bat_r,col=magma(2),colNA=1)
#Intertidal_bat_urok<-bat_Urok*mask_urok_bat_r


## Load other images
all_Urok<-stack("Data_out/Stack/stack_all_Urok.tif")
names(all_Urok)
nam<-c("B02","B03","B04","B05","B06","B07","B08","B08a","B09","B11","B12","S1_VH","S1_VV","bat_Urok1")
names(all_Urok)<-nam
plot(all_Urok)

### Resample new bat to enable to be stacked to the rest of the images
extent(bat_Urok)==extent(all_Urok[[14]])
bat_Urok2<-resample(bat_Urok,all_Urok[[14]])
extent(bat_Urok2)==extent(all_Urok[[14]])
plot(bat_Urok2)

all_Urok1<-stack(all_Urok,bat_Urok2)
plot(all_Urok1[[2]])
plot(GT_Urok,col="red",add=T)


## Extract values only for Urok
beginCluster(7)
system.time(DF<-extract(all_Urok1,GT_Urok,cellnumbers=T,df=F,factors=T,nl=15,na.rm=T))
beep()
endCluster()

##Remove points that do not fall into Urok area
#names(DF)<-seq_along(DF)
#DF1<-Filter(Negate(is.null), DF)
Point<-seq_along(DF)


n<-c(colnames(DF[[1]]),colnames(GT_Urok@data))
m<-matrix(data=NA,nrow=1,ncol=length(n))
colnames(m)<-n

for(i in Point) {
  m<-rbind(m,cbind(DF[i],GT_Urok@data[i,]))
  print(paste(i,"done"))
}

m1<-m[-1,] ###remove first row of NAs

unique(m1$Point)

write.table(m1,"Data_out/db/GraVSSat_db_20201221.csv",row.names=F,sep=";")


