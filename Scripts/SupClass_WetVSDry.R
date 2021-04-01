setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

packs<-c("randomForest","caret","sf","beepr","RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis","data.table","reshape2")
lapply(packs,require,character.only=T)

## Load sat img

sat<-stack("Data_out/Stack/Final_stack.tif") ##created in script GraVSSat_Preliminary
names(sat)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204","B08A_20200204",
              "B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","DEM","NDWI","mNDWI","NDMI","NDMI1")

##Load GT polygons
GT_c1<-readOGR("Data_out/Polygons/GT_c1.shp") ##created in script Data_cleanup_SUp_Class




###########################################################################
##########################################################################
###create threshold with water cover

DF[,WD:=ifelse(c_water<20,"dry","wet")] # this is the mark that provides the highest accuracy and kappa
DF[,table(WD)]


##Split data in training + validation using caret balanced splitting

set.seed(10)
trainIndex_WD <- createDataPartition(DF$WD, p = .7, 
                                     list = FALSE, 
                                     times = 1)
head(trainIndex_WD)

L0_train_WD<-DF[trainIndex_WD]
L0_val_WD<-DF[-trainIndex_WD]


###Introduce new columns on training and validation polygons
L0_train1_WD<-L0_train_WD[,.(Class_11,Class_22,Class_33,WD,Point)]

GT_c_l0_t_WD<-merge(GT_c,L0_train1_WD,by="Point",all.x=F,all.y=T)

L0_val1_WD<-L0_val_WD[,.(Class_11,Class_22,Class_33,WD,Point)]

GT_c_l0_v_WD<-merge(GT_c,L0_val1_WD,by="Point",all.x=F,all.y=T)

### Supervised class with rstoolbox and rf
set.seed(11)
beginCluster(7)
SC1_WD<-superClass(img=sat,model="rf",trainData=GT_c_l0_t_WD,responseCol="WD",valData=GT_c_l0_v_WD,polygonBasedCV=F,predict=T,
                   predType="raw",filename="Data_out/Class_map/WetVSDry.tif")
endCluster()
beep(3)

plot(SC1_WD$map, colNA=1, main="WetVSDry, 20% cut")

SC1_WD$classMapping

WDmap<-SC1_WD$map
plot(WDmap[WDmap$WD==1])

###Isolating dry area
dry_mask<-WDmap==1
plot(dry_mask, colNA=1)

dry_mask[dry_mask==0]<-NA # turn wet area (coded zero) into NA
area_dry<-sum(dry_mask[dry_mask==1])*res(dry_mask)[1]^2*10^-6 #calculate dry area size in Km2

writeRaster(dry_mask,"Data_out/mask/dry_mask.tif")

###Isolating wet area
wet_mask<-WDmap==2
plot(wet_mask, colNA=1)

wet_mask[wet_mask==0]<-NA # turn wet area (coded zero) into NA
area_wet<-sum(wet_mask[wet_mask==1])*res(wet_mask)[1]^2*10^-6 #calculate wet area size in Km2

writeRaster(wet_mask,"Data_out/mask/wet_mask.tif")

