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


packs<-c("foreach","doParallel","randomForest","caret","sf","beepr","RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis","data.table","reshape2")
npacks <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(npacks)) install.packages(npacks)
#install_github("vqv/ggbiplot")
lapply(packs,require,character.only=T)

p<-c("green","red","blue","grey50","lightgrey")

## Load sat img

sat<-stack("Data_out/Stack/Final_stack1.grd") ##created in script GraVSSat_Preliminary


PCA_WD<-readRSTBX("Data_out/PCA/PCA_WD,tif") ##created in script PCAs.R
PCA_map<-PCA_WD$map
plot(PCA_map[[1]])
summary(PCA_WD$model)


##Load GT polygons
GT_c1<-readOGR("Data_out/Polygons/GT_c1.shp") ##created in script Data_cleanup_SUp_Class
DF2<-data.table(GT_c1@data)
str(DF2)


##Split data in training + validation using caret balanced splitting: Use this for final validation
DF3<-data.table(DF2)
DF3[,table(cvr_vrA)]
#DF3[,covr_vrA:=as.character(covr_vr)][covr_vrA=="water_body",covr_vrA:="bare_sediment"]

DF4<-DF3[!(is.na(cvr_vrA))] ## remove 2 bad points with NA everywhere

DF4[,cvr_vrA6:=as.character(cvr_vrA)][cvr_vrA6=="bare_sediment",cvr_vrA6:="sediments"][cvr_vrA6=="uca",cvr_vrA6:="sediments"]
DF4[,.(table(cvr_vrA6))]

### Variable selection for water

sat0_WD<-sat[[c(9,13,11:12,21,14,16)]]
names(sat0_WD)

#################################################
####################### WD ##########################

##Split data in training + validation using caret balanced splitting

DF4[,.(table(WD))]
DF4[,.(table(WB))]

set.seed(10)
trainIndex_WD <- createDataPartition(DF4$WD, p = .8, 
                                     list = FALSE, 
                                     times = 1)

set.seed(10)
trainIndex_WB <- createDataPartition(DF4$WB, p = .8, 
                                     list = FALSE, 
                                     times = 1)
#head(trainIndex_WD)

L0_train_WD<-DF4[trainIndex_WD]
L0_val_WD<-DF4[-trainIndex_WD]

L0_train_WB<-DF4[trainIndex_WB]
L0_val_WB<-DF4[-trainIndex_WB]


###Introduce new columns on training and validation polygons
GT_c_l0_t_WD<-merge(GT_c1,L0_train_WD,by="Point",all.x=F,all.y=T)
GT_c_l0_t_WB<-merge(GT_c1,L0_train_WB,by="Point",all.x=F,all.y=T)

GT_c_l0_v_WD<-merge(GT_c1,L0_val_WD,by="Point",all.x=F,all.y=T)
GT_c_l0_v_WB<-merge(GT_c1,L0_val_WB,by="Point",all.x=F,all.y=T)

###create new treshold for Water
#GT_c_l0_t$WD20<-ifelse(GT_c_l0_t$c_water.x<20,"dry","wet")
#GT_c_l0_v$WD20<-ifelse(GT_c_l0_v$c_water.x<20,"dry","wet")

### Supervised class with rstoolbox and rf
set.seed(11)
beginCluster()
SC1_WD20<-superClass(img=sat0_WD,model="rf",trainData=GT_c_l0_t_WD,responseCol="WD.y",valData=GT_c_l0_v_WD,polygonBasedCV=T,predict=T,
                   predType="raw",filename=NULL)
endCluster()
beep(3)
saveRSTBX(SC1_WD20,"Data_out/models/SC1_WD20",format="raster",overwrite=T)
SC1_WD20$classMapping

writeRaster(SC1_WD20$map,"Data_out/class_tif/SC1_WD20.tif")

pWD<-c("red","lightblue")

plot(SC1_WD20$map, colNA=1,col=pWD,main="WetVSDry, 20% cut")



set.seed(11)
beginCluster()
SC1_WB30<-superClass(img=sat0_WD,model="rf",trainData=GT_c_l0_t_WB,responseCol="WB.y",valData=GT_c_l0_v_WB,polygonBasedCV=T,predict=T,
                     predType="raw",filename=NULL)
endCluster()
beep(3)
saveRSTBX(SC1_WB30,"Data_out/models/SC1_WB30",format="raster",overwrite=T)
SC1_WB30$classMapping

pWD<-c("red","lightblue")

plot(SC1_WB30$map, colNA=1,col=pWD,main="WetVSDry, 20% cut")


#WBmap<-SC1_WB$map
#plot(WBmap[WBmap$WB==1])

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

DF3[,WD20:=ifelse(c_water<20,"dry","wet")] # this is the mark that provides the highest accuracy and kappa
DF3[,table(WD20)]


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

