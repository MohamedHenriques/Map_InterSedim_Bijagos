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

packs<-c("colorspace","randomForest","caret","sf","beepr","RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis","data.table","reshape2")
npacks <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(npacks)) install.packages(npacks)
#install_github("vqv/ggbiplot")
lapply(packs,require,character.only=T)

## Load sat img
sat<-stack("Data_out/Stack/Final_stack.tif") ##created in script GraVSSat_Preliminary
names(sat)<-names(sat)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
                          "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
                          "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","intensity","iv_multi","rededge_multi","rededge_sum",
                          "visible_multi")

##Load GT polygons
GT_c1<-readOGR("Data_out/Polygons/GT_c1.shp") ##created in script Data_cleanup_SUp_Class
DF2<-data.table(GT_c1@data)
str(DF2)

######################################################################################################
#################### classification using all target habitat classes ################################

##Split data in training + validation using caret balanced splitting: Use this for final validation
DF3<-data.table(DF2)
DF3[,table(cvr_sd_f)]
DF4<-DF3[!(cvr_sd_f=="bare_sediment"),]
str(DF4)
DF4[,cvr_sd_f:=as.character(cvr_sd_f)]
DF4[,table(cvr_sd_f)]

set.seed(200)
trainIndex_F <- createDataPartition(DF4$cvr_sd_f, p = .7, 
                                    list = FALSE, 
                                    times = 1)
head(trainIndex_F)

L0_train_F<-DF4[trainIndex_F]
L0_train_F[,table(cvr_sd_f)]

L0_val_F<-DF4[-trainIndex_F]
L0_val_F[,table(cvr_sd_f)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_F<-merge(GT_c1,L0_train_F,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t,col="red")
str(GT_c_l0_t_F@data)

GT_c_l0_v_F<-merge(GT_c1,L0_val_F,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v)
str(GT_c_l0_v_F@data)

### Supervised class with rstoolbox and rf
set.seed(20)
beginCluster(7)
SC1_allclass<-superClass(img=sat,model="rf",trainData=GT_c_l0_t_F,responseCol="cvr_sd_f.y",valData=GT_c_l0_v_F,polygonBasedCV=F,predict=T,
                         predType="raw",filename=NULL)
endCluster()
beep(3)
saveRSTBX(SC1_allclass,"Data_out/models/SC1_allclass",format="raster")
SC1_allclass<-readRSTBX("Data_out/models/SC1_allclass.tif")
SC1_allclass$classMapping

plot(SC1_allclass$map, colNA=1, main="cover over wet")
writeRaster(SC1_allclass$map,"Data_out/models/SC1_allclass.tif")


########################################################################################
#######################################################################################
########## CLASSIFICATION OF ALL SEDIMENT CLASSES AFTER REMOVING COVER OVER ###################
##Load GT polygons
GT_c1<-readOGR("Data_out/Polygons/GT_c1.shp") ##created in script Data_cleanup_SUp_Class
DF2<-data.table(GT_c1@data)
str(DF2)


##Split data in training + validation using caret balanced splitting: Use this for final validation
DF3<-data.table(DF2)
DF3[,table(cvr_sd_f)]
DF4<-DF3[!(cvr_sd_f=="bare_sediment"),]
str(DF4)
DF4[,cvr_sd_f:=as.character(cvr_sd_f)]
DF4[,table(cvr_sd_f)]

DF5<-DF4[,grain_uca_bsed:=cvr_sd_f][!(grain_uca_bsed=="macroalgae"|grain_uca_bsed=="rock"|grain_uca_bsed=="shell")]
DF5[,table(grain_uca_bsed)]

set.seed(200)
trainIndex_F1 <- createDataPartition(DF5$grain_uca_bsed, p = .7, 
                                     list = FALSE, 
                                     times = 1)
head(trainIndex_F1)

L0_train_F1<-DF5[trainIndex_F1]
L0_train_F1[,table(grain_uca_bsed)]

L0_val_F1<-DF5[-trainIndex_F1]
L0_val_F1[,table(grain_uca_bsed)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_F1<-merge(GT_c1,L0_train_F1,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t,col="red")
str(GT_c_l0_t_F1@data)

GT_c_l0_v_F1<-merge(GT_c1,L0_val_F1,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v)
str(GT_c_l0_v_F1@data)

### Supervised class with rstoolbox and rf
set.seed(20)
beginCluster(7)
SC1_grainclass<-superClass(img=sat_bsed,model="rf",trainData=GT_c_l0_t_F1,responseCol="grain_uca_bsed",valData=GT_c_l0_v_F1,polygonBasedCV=F,predict=T,
                           predType="raw",filename=NULL)
endCluster()
beep(3)
saveRSTBX(SC1_grainclass,"Data_out/models/SC1_grainclass",format="raster")
SC1_grainclass<-readRSTBX("Data_out/models/SC1_grainclass.tif")
SC1_grainclass$classMapping

plot(SC1_grainclass$map, colNA=1, main="grain size")
#writeRaster(SC1_grainclass$map,"Data_out/models/SC1_grainclass.tif")



###########################################################################
####################################################################
####### Classification of grain size only for bare sediment and uca #####


##Load GT polygons
GT_c1<-readOGR("Data_out/Polygons/GT_c1.shp") ##created in script Data_cleanup_SUp_Class
DF2<-data.table(GT_c1@data)
str(DF2)


##Split data in training + validation using caret balanced splitting: Use this for final validation
DF3<-data.table(DF2)
DF3[,table(cvr_sd_f)]
DF4<-DF3[!(cvr_sd_f=="bare_sediment"),]
str(DF4)
DF4[,cvr_sd_f:=as.character(cvr_sd_f)]
DF4[,table(cvr_sd_f)]

DF5<-DF4[,grain_uca_bsed:=cvr_sd_f][!(grain_uca_bsed=="macroalgae"|grain_uca_bsed=="rock"|grain_uca_bsed=="shell")]
DF5[,table(grain_uca_bsed)]

DF6<-DF5[cvr_vrA=="bare_sediment"]
DF6[,table(grain_uca_bsed)]

set.seed(200)
trainIndex_Fbs <- createDataPartition(DF6$grain_uca_bsed, p = .7, 
                                      list = FALSE, 
                                      times = 1)
head(trainIndex_Fbs)

L0_train_Fbs<-DF6[trainIndex_Fbs]
L0_train_Fbs[,table(grain_uca_bsed)]

L0_val_Fbs<-DF6[-trainIndex_Fbs]
L0_val_Fbs[,table(grain_uca_bsed)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_Fbs<-merge(GT_c1,L0_train_Fbs,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t,col="red")
str(GT_c_l0_t_Fbs@data)

GT_c_l0_v_Fbs<-merge(GT_c1,L0_val_Fbs,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v)
str(GT_c_l0_v_Fbs@data)

### Supervised class with rstoolbox and rf

sat_bsed_uca<-stack("Data_out/Stack/sat_WD_all_valtot.tif") ##Created in script Level0_SupClass.R
bare_sediment_mask<-raster("Data_out/Habitat_classes/bare_sediment_mask_valtot.tif")

beginCluster(7)
sat_bsed<-mask(sat_bsed_uca,bare_sediment_mask)
writeRaster(sat_bsed,"Data_out/Stack/sat_bsed",driver="raster")
endCluster()
beep(3)

set.seed(20)
beginCluster(7)
SC1_grainclass_bs<-superClass(img=sat_bsed,model="rf",trainData=GT_c_l0_t_Fbs,responseCol="grain_uca_bsed",valData=GT_c_l0_v_Fbs,polygonBasedCV=F,predict=T,
                              predType="raw",filename=NULL)
endCluster()
beep(3)
saveRSTBX(SC1_grainclass_bs,"Data_out/models/SC1_grainclass_bs",format="raster",overwrite=F)
SC1_grainclass_bs<-readRSTBX("Data_out/models/SC1_grainclass_bs.tif")
SC1_grainclass_bs$classMapping

plot(SC1_grainclass_bs$map, colNA=1, main="grain size")
#writeRaster(SC1_grainclass_bs$map,"Data_out/models/SC1_grainclass_bs.tif")

################ for UCA now ##########

DF7<-DF5[cvr_vrA=="uca"]
DF7[,table(grain_uca_bsed)]

set.seed(200)
trainIndex_Fuca <- createDataPartition(DF7$grain_uca_bsed, p = .7, 
                                       list = FALSE, 
                                       times = 1)
head(trainIndex_Fuca)

L0_train_Fuca<-DF7[trainIndex_Fuca]
L0_train_Fuca[,table(grain_uca_bsed)]

L0_val_Fuca<-DF7[-trainIndex_Fuca]
L0_val_Fuca[,table(grain_uca_bsed)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_Fuca<-merge(GT_c1,L0_train_Fuca,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t,col="red")
#str(GT_c_l0_t_Fuca@data)

GT_c_l0_v_Fuca<-merge(GT_c1,L0_val_Fuca,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v)
#str(GT_c_l0_v_Fuca@data)

### Supervised class with rstoolbox and rf
sat_bsed_uca<-stack("Data_out/Stack/sat_WD_all_valtot.tif") ##Created in script Level0_SupClass.R
uca_mask<-raster("Data_out/Habitat_classes/uca_mask_valtot.tif") ##Created in script Level0_SupClass.R

beginCluster(7)
sat_uca<-mask(sat_bsed_uca,uca_mask)
writeRaster(sat_uca,"Data_out/Stack/sat_uca.tif",overwrite=T)
endCluster()
beep(3)

set.seed(20)
beginCluster(7)
SC1_grainclass_uca<-superClass(img=sat_uca,model="rf",trainData=GT_c_l0_t_Fuca,responseCol="grain_uca_bsed",valData=GT_c_l0_v_Fuca,polygonBasedCV=F,predict=T,
                               predType="raw",filename=NULL)
endCluster()
beep(3)
saveRSTBX(SC1_grainclass_uca,"Data_out/models/SC1_grainclass_uca",format="raster",overwrite=T)
SC1_grainclass_uca$classMapping

plot(SC1_grainclass_uca$map, colNA=1, main="grain size")
#writeRaster(SC1_grainclass_uca$map,"Data_out/models/SC1_grainclass_uca.tif")



#####################################################################################################
#####################################################################################################
################# Classification over dry and wet areas seperately #################################

### Sat image for classification of all areas
#dry_mask<-raster("Data_out/Habitat_classes/dry_mask_valtot.tif")
sat_dry<-mask(sat,dry_mask) # To use for clasification of all areas
writeRaster(sat_dry,"Data_out/Stack/sat_dry_valtot.tif",overwrite=T)
sat_dry<-raster("Data_out/Stack/sat_dry_valtot.tif")

#wet_mask<-raster("Data_out/Habitat_classes/wet_mask_valtot.tif")
sat_wet<-mask(sat,wet_mask) # To use for clasification of all areas
writeRaster(sat_wet,"Data_out/Stack/sat_wet_valtot.tif",overwrite=T)
beep(3)
sat_wet<-raster("Data_out/Stack/sat_wet_valtot.tif")


##Sat image for classification of areas combining bare sediment and uca
#dry_mask<-raster("Data_out/Habitat_classes/dry_mask_valtot.tif")
beginCluster(7)
sat_sed_dry<-mask(sat_bsed_uca,dry_mask) # To use for clasification of only areas of bare sediment and uca
writeRaster(sat_sed_dry,"Data_out/Stack/sat_sed_dry_valtot.tif",overwrite=T)
sat_sed_dry<-raster("Data_out/Stack/sat_sed_dry_valtot.tif")

#wet_mask<-raster("Data_out/Habitat_classes/wet_mask_valtot.tif")
sat_sed_wet<-mask(sat_bsed_uca,wet_mask) # To use for clasification of only areas of bare sediment and uca
writeRaster(sat_sed_wet,"Data_out/Stack/sat_sed_wet_valtot.tif",overwrite=T)
beep(3)
endCluster()
sat_sed_wet<-raster("Data_out/Stack/sat_sed_wet_valtot.tif")

## Sat image for classification of areas of bare sediment
#dry_mask<-raster("Data_out/Habitat_classes/dry_mask_valtot.tif")
#sat_bsed<-raster("Data_out/Stack/sat_bsed.tif")
beginCluster(7)
sat_bsed_dry<-mask(sat_bsed,dry_mask) # To use for clasification of only areas of bare sediment
writeRaster(sat_bsed_dry,"Data_out/Stack/sat_bsed_dry_valtot.tif",overwrite=T)
sat_bsed_dry<-raster("Data_out/Stack/sat_bsed_dry_valtot.tif")

#wet_mask<-raster("Data_out/Habitat_classes/wet_mask_valtot.tif")
sat_bsed_wet<-mask(sat_bsed,wet_mask) # To use for clasification of only areas of bare sediment
writeRaster(sat_bsed_wet,"Data_out/Stack/sat_bsed_wet_valtot.tif",overwrite=T)
beep(3)
endCluster()
sat_bsed_wet<-raster("Data_out/Stack/sat_bsed_wet_valtot.tif")

## Sat image for classification of areas of uca
#dry_mask<-raster("Data_out/Habitat_classes/dry_mask_valtot.tif")
sat_uca<-raster("Data_out/Stack/sat_uca.tif")
beginCluster(7)
sat_uca_dry<-mask(sat_uca,dry_mask) # To use for clasification of only areas of bare sediment
writeRaster(sat_uca_dry,"Data_out/Stack/sat_uca_dry_valtot.tif",overwrite=T)
sat_uca_dry<-raster("Data_out/Stack/sat_uca_dry_valtot.tif")

#wet_mask<-raster("Data_out/Habitat_classes/wet_mask_valtot.tif")
sat_uca_wet<-mask(sat_uca,wet_mask) # To use for clasification of only areas of bare sediment
writeRaster(sat_uca_wet,"Data_out/Stack/sat_uca_wet_valtot.tif",overwrite=T)
beep(3)
endCluster()
sat_uca_wet<-raster("Data_out/Stack/sat_uca_wet_valtot.tif")

############### DRY AREAS #########################
###################################################


### Supervised class with rstoolbox and rf
set.seed(20)
beginCluster(7)
SC1_grainclass_bs_dry<-superClass(img=sat_bsed_dry,model="rf",trainData=GT_c_l0_t_Fbs,responseCol="grain_uca_bsed",valData=GT_c_l0_v_Fbs,polygonBasedCV=F,predict=T,
                              predType="raw",filename=NULL)
endCluster()
beep(3)
saveRSTBX(SC1_grainclass_bs_dry,"Data_out/models/SC1_grainclass_bs_dry",format="raster",overwrite=T)
SC1_grainclass_bs_dry<-readRSTBX("Data_out/models/SC1_grainclass_bs_dry.tif")
SC1_grainclass_bs_dry$classMapping

p<-c("white","navajowhite4","lightgoldenrod1","lightgoldenrod","navajowhite2")
pal <- brewer.pal(5,"Accent")

plot(SC1_grainclass_bs_dry$map,col=pal, colNA=1, main="DRY AREAS - Bare sediment grain sizes")
#writeRaster(SC1_grainclass_bs$map,"Data_out/models/SC1_grainclass_bs.tif")

################ for UCA now ##########

### Supervised class with rstoolbox and rf
set.seed(20)
beginCluster(7)
SC1_grainclass_uca_dry<-superClass(img=sat_uca_dry,model="rf",trainData=GT_c_l0_t_Fuca,responseCol="grain_uca_bsed",valData=GT_c_l0_v_Fuca,polygonBasedCV=F,predict=T,
                               predType="raw",filename=NULL)
endCluster()
beep(3)
saveRSTBX(SC1_grainclass_uca_dry,"Data_out/models/SC1_grainclass_uca_dry",format="raster",overwrite=T)
SC1_grainclass_uca_dry<-readRSTBX("Data_out/models/SC1_grainclass_uca_dry.tif")
SC1_grainclass_uca_dry$classMapping

plot(SC1_grainclass_uca_dry$map, colNA=1, main="grain size")
#writeRaster(SC1_grainclass_uca_dry$map,"Data_out/models/SC1_grainclass_uca_dry.tif")


#######################################################################################
############################ WET AREAS ################################################

### Supervised class with rstoolbox and rf
set.seed(20)
beginCluster(7)
SC1_grainclass_bs_wet<-superClass(img=sat_bsed_wet,model="rf",trainData=GT_c_l0_t_Fbs,responseCol="grain_uca_bsed",valData=GT_c_l0_v_Fbs,polygonBasedCV=F,predict=T,
                                  predType="raw",filename=NULL)
endCluster()
beep(3)
saveRSTBX(SC1_grainclass_bs_wet,"Data_out/models/SC1_grainclass_bs_wet",format="raster",overwrite=T)
SC1_grainclass_bs_wet<-readRSTBX("Data_out/models/SC1_grainclass_bs_wet.tif")
SC1_grainclass_bs_wet$classMapping

plot(SC1_grainclass_bs_wet$map, colNA=1, main="grain size")
#writeRaster(SC1_grainclass_bs$map,"Data_out/models/SC1_grainclass_bs.tif")

################ for UCA now ##########

### Supervised class with rstoolbox and rf
set.seed(20)
beginCluster(7)
SC1_grainclass_uca_wet<-superClass(img=sat_uca_wet,model="rf",trainData=GT_c_l0_t_Fuca,responseCol="grain_uca_bsed",valData=GT_c_l0_v_Fuca,polygonBasedCV=F,predict=T,
                                   predType="raw",filename=NULL)
endCluster()
beep(3)
saveRSTBX(SC1_grainclass_uca_wet,"Data_out/models/SC1_grainclass_uca_wet",format="raster",overwrite=T)
SC1_grainclass_uca_wet<-readRSTBX("Data_out/models/SC1_grainclass_uca_wet.tif")
SC1_grainclass_uca_wet$classMapping

plot(SC1_grainclass_uca_wet$map, colNA=1, main="grain size")
#writeRaster(SC1_grainclass_uca_wet$map,"Data_out/models/SC1_grainclass_uca_wet.tif")



