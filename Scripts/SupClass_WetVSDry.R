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
DF2<-data.table(GT_c1@data)
str(DF2)


##Split data in training + validation using caret balanced splitting: Use this for final validation
DF3<-data.table(DF2)
DF3[,covr_vrA:=as.character(covr_vr)][covr_vrA=="water_body",covr_vrA:="bare_sediment"]

set.seed(10)
trainIndex <- createDataPartition(DF3$covr_vrA, p = .7, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

L0_train<-DF3[trainIndex]
L0_train[,table(covr_vrA)]

L0_val<-DF3[-trainIndex]
L0_val[,table(covr_vrA)]


###Introduce new columns on training and validation polygons
#L0_train1<-L0_train[,.(covr_vr,Point)]

GT_c_l0_t<-merge(GT_c1,L0_train,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t,col="red")
#str(GT_c_l0_t@data)

#L0_val1<-L0_val[,.(covr_vr,Point)]

GT_c_l0_v<-merge(GT_c1,L0_val,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v)
#str(GT_c_l0_v@data)


#################################################
####################### WB ##########################

##Split data in training + validation using caret balanced splitting

set.seed(10)
trainIndex_WB <- createDataPartition(L0_train$WB, p = .7, 
                                     list = FALSE, 
                                     times = 1)
head(trainIndex_WB)

L0_train_WB<-DF3[trainIndex_WB]
L0_val_WB<-DF3[-trainIndex_WB]


###Introduce new columns on training and validation polygons
GT_c_l0_t_WB<-merge(GT_c1,L0_train_WB,by="Point",all.x=F,all.y=T)

GT_c_l0_v_WB<-merge(GT_c1,L0_val_WB,by="Point",all.x=F,all.y=T)

### Supervised class with rstoolbox and rf
set.seed(11)
beginCluster(7)
SC1_WB<-superClass(img=sat,model="rf",trainData=GT_c_l0_t_WB,responseCol="WB.y",valData=GT_c_l0_v_WB,polygonBasedCV=F,predict=T,
                   predType="raw",filename=NULL)
endCluster()
beep(3)
saveRSTBX(SC1_WB,"Data_out/models/SC1_WB",format="raster",overwrite=T)
SC1_WB$classMapping

pal<-c("green","red","blue","dark                                                                                                                                                              grey","lightgrey")

plot(SC1_WB$map, colNA=1,col=pal,main="WetVSDry, 20% cut")



WBmap<-SC1_WB$map
plot(WBmap[WBmap$WB==1])

###Isolating dry area
dry_mask<-WBmap==1
plot(dry_mask, colNA=1)

dry_mask[dry_mask==0]<-NA # turn wet area (coded zero) into NA
area_dry<-sum(dry_mask[dry_mask==1])*res(dry_mask)[1]^2*10^-6 #calculate dry area size in Km2

writeRaster(dry_mask,"Data_out/mask/dry_mask.tif")

###Isolating wet area
wet_mask<-WBmap==2
plot(wet_mask, colNA=1)

wet_mask[wet_mask==0]<-NA # turn wet area (coded zero) into NA
area_wet<-sum(wet_mask[wet_mask==1])*res(wet_mask)[1]^2*10^-6 #calculate wet area size in Km2

writeRaster(wet_mask,"Data_out/mask/wet_mask.tif")


###########################################################################
##########################################################################
###create threshold with water cover

DF3[,WD:=ifelse(c_water<20,"dry","wet")] # this is the mark that provides the highest accuracy and kappa
DF3[,table(WD)]


##Split data in training + validation using caret balanced splitting

set.seed(10)
trainIndex_WD <- createDataPartition(DF3$WD, p = .7, 
                                     list = FALSE, 
                                     times = 1)
head(trainIndex_WD)

L0_train_WD<-DF3[trainIndex_WD]
L0_val_WD<-DF3[-trainIndex_WD]


###Introduce new columns on training and validation polygons


GT_c_l0_t_WD<-merge(GT_c1,L0_train_WD,by="Point",all.x=F,all.y=T)

GT_c_l0_v_WD<-merge(GT_c1,L0_val_WD,by="Point",all.x=F,all.y=T)

### Supervised class with rstoolbox and rf
set.seed(11)
beginCluster(7)
SC1_WD<-superClass(img=sat,model="rf",trainData=GT_c_l0_t_WD,responseCol="WD",valData=GT_c_l0_v_WD,polygonBasedCV=F,predict=T,
                   predType="raw",filename="Data_out/Class_map/WetVSDry.tif")
endCluster()
beep(3)
saveRSTBX(SC1_WD,"Data_out/models/SC1_WD",format="raster")

#plot(SC1_WD$map, colNA=1, main="WetVSDry, 20% cut")
SC1_WD$classMapping

WDmap<-SC1_WD$map
#plot(WDmap[WDmap$WD==1])

###Isolating dry area
dry_mask<-WDmap==1
plot(dry_mask, colNA=1)

dry_mask[dry_mask==0]<-NA # turn wet area (coded zero) into NA
area_dry<-sum(dry_mask[dry_mask==1])*res(dry_mask)[1]^2*10^-6 #calculate dry area size in Km2

writeRaster(dry_mask,"Data_out/mask/dry_mask.tif",overwrite=T)

###Isolating wet area
wet_mask<-WDmap==2
#plot(wet_mask, colNA=1)

wet_mask[wet_mask==0]<-NA # turn wet area (coded zero) into NA
area_wet<-sum(wet_mask[wet_mask==1])*res(wet_mask)[1]^2*10^-6 #calculate wet area size in Km2

writeRaster(wet_mask,"Data_out/mask/wet_mask.tif",overwrite=T)

