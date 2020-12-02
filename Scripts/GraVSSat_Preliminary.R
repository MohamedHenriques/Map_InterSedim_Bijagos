setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

packs<-c("RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis","xlsx","reshape2")
lapply(packs,require,character.only=T)

## Load polygons with info on concluded sediment so far 20201113
GT<-readOGR("Data_out/Polygons/Poly_GT_Gra_ended_20201126.shp")

plot(GT,col="red")

## Load exposure model
bat<-raster("D:/Work/FCUL/Doutoramento/Joao_Belo/Exposure_model/tempo_exposicao_Bijagos.tif")
plot(bat)

###Cut bat for Urok area
Urok<-readOGR(dsn="Shapefiles/Urok_shape",layer="Urok_shapes")
plot(Urok,add=T,col="red")

bat_Urok<-crop(bat,Urok)
plot(bat_Urok)


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


## Extract values 

DF<-extract(all_Urok1,GT,cellnumbers=T,df=F,factors=T,nl=15,na.rm=T)

##Because point 76 falls out of sat image range, I only use Urok area
DF1<-DF[-c(76,191:228)]
GT1<-GT[-c(76,191:228),]

n<-c(colnames(DF1[[1]]),colnames(GT1@data))
m<-matrix(data=NA,nrow=1,ncol=length(n))
colnames(m)<-n

for(i in 1:length(DF1)) {
  m<-rbind(m,cbind(DF1[i],GT1@data[i,]))
  print(paste(i,"done"))
}

m1<-m[-1,] ###remove first row of NAs

unique(m1$Point)

write.table(m1,"Data_out/db/GraVSSat_db_20201115.csv",row.names=F,sep=";")
