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

#################################################################
##################################################################

#################### classification using all target habitat classes ################################

##Split data in training + validation using caret balanced splitting: Use this for final validation
DF3<-data.table(DF2)
DF3[,table(cvr_sd_g)]
DF4<-DF3[!(cvr_sd_g=="bare_sediment_NA"),]
str(DF4)
DF4[,cvr_sd_g:=as.character(cvr_sd_g)]
DF4[,table(cvr_sd_g)]
DF4[cvr_sd_g=="uca_NA",table(Sd_clss)]
DF5<-DF4[!(cvr_sd_g=="uca_NA"),]
DF5[,table(cvr_sd_g)]

set.seed(200)
trainIndex_G <- createDataPartition(DF5$cvr_sd_g, p = .7, 
                                    list = FALSE, 
                                    times = 1)
head(trainIndex_G)

L0_train_G<-DF5[trainIndex_G]
L0_train_G[,table(cvr_sd_g)]

L0_val_G<-DF5[-trainIndex_G]
L0_val_G[,table(cvr_sd_g)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_G<-merge(GT_c1,L0_train_G,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t,col="red")
str(GT_c_l0_t_G@data)

GT_c_l0_v_G<-merge(GT_c1,L0_val_G,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v)
str(GT_c_l0_v_G@data)

### Supervised class with rstoolbox and rf
set.seed(20)
beginCluster(7)
SC1_allclass_G<-superClass(img=sat,model="rf",trainData=GT_c_l0_t_G,responseCol="cvr_sd_g.y",valData=GT_c_l0_v_G,polygonBasedCV=F,predict=T,
                         predType="raw",filename=NULL)
endCluster()
beep(3)
saveRSTBX(SC1_allclass_G,"Data_out/models/SC1_allclass_G",format="raster")
SC1_allclass_G<-readRSTBX("Data_out/models/SC1_allclass_G.tif")
SC1_allclass_G$classMapping

plot(SC1_allclass$map, colNA=1, main="cover over wet")
writeRaster(SC1_allclass$map,"Data_out/models/SC1_allclass.tif")



########################################################################################
#######################################################################################
########## CLASSIFICATION OF ALL SEDIMENT CLASSES AFTER REMOVING COVER OVER ###################

DF6<-DF5[,grain_uca_bsed_g:=cvr_sd_g][!(grain_uca_bsed_g=="macroalgae"|grain_uca_bsed_g=="rock"|grain_uca_bsed_g=="shell")]
DF6[,table(grain_uca_bsed_g)]


##Split data in training + validation using caret balanced splitting: Use this for final validation
set.seed(200)
trainIndex_G1 <- createDataPartition(DF6$grain_uca_bsed_g, p = .7, 
                                     list = FALSE, 
                                     times = 1)
head(trainIndex_G1)

L0_train_G1<-DF6[trainIndex_G1]
L0_train_G1[,table(grain_uca_bsed_g)]

L0_val_G1<-DF6[-trainIndex_G1]
L0_val_G1[,table(grain_uca_bsed_g)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_G1<-merge(GT_c1,L0_train_G1,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t,col="red")
str(GT_c_l0_t_G1@data)

GT_c_l0_v_G1<-merge(GT_c1,L0_val_G1,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v)
str(GT_c_l0_v_G1@data)

### Supervised class with rstoolbox and rf
sat_bsed<-stack("Data_out/Stack/sat_bsed.gri")
sat_bsed_uca<-stack("Data_out/Stack/sat_WD_all_valtot.tif") ##Created in script Level0_SupClass.R

set.seed(20)
beginCluster(7)
SC1_grainclass_G<-superClass(img=sat_bsed_uca,model="rf",trainData=GT_c_l0_t_G1,responseCol="grain_uca_bsed_g",valData=GT_c_l0_v_G1,polygonBasedCV=F,predict=T,
                           predType="raw",filename=NULL)
endCluster()
beep(3)
saveRSTBX(SC1_grainclass_G,"Data_out/models/SC1_grainclass_G",format="raster")
SC1_grainclass_G<-readRSTBX("Data_out/models/SC1_grainclass_G.tif")
SC1_grainclass_G$classMapping

plot(SC1_grainclass_G$map, colNA=1, main="grain size")
#writeRaster(SC1_grainclass$map,"Data_out/models/SC1_grainclass.tif")


###########################################################################
####################################################################
####### Classification of grain size only for bare sediment and uca #####

DF7<-DF6[,grain_uca_bsed_g:=cvr_sd_g][!(grain_uca_bsed_g=="macroalgae"|grain_uca_bsed_g=="rock"|grain_uca_bsed_g=="shell")]
DF7[,table(grain_uca_bsed_g)]

DF8<-DF7[cvr_vrA=="bare_sediment"]
DF8[,table(grain_uca_bsed_g)]

set.seed(200)
trainIndex_Fbs_G <- createDataPartition(DF8$grain_uca_bsed_g, p = .7, 
                                      list = FALSE, 
                                      times = 1)
head(trainIndex_Fbs_G)

L0_train_Fbs_G<-DF8[trainIndex_Fbs_G]
L0_train_Fbs_G[,table(grain_uca_bsed_g)]

L0_val_Fbs_G<-DF8[-trainIndex_Fbs_G]
L0_val_Fbs_G[,table(grain_uca_bsed_g)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_Fbs_G<-merge(GT_c1,L0_train_Fbs_G,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t,col="red")
str(GT_c_l0_t_Fbs_G@data)

GT_c_l0_v_Fbs_G<-merge(GT_c1,L0_val_Fbs_G,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v)
str(GT_c_l0_v_Fbs_G@data)

### Supervised class with rstoolbox and rf

sat_bsed_uca<-stack("Data_out/Stack/sat_WD_all_valtot.tif") ##Created in script Level0_SupClass.R
bare_sediment_mask<-raster("Data_out/Habitat_classes/bare_sediment_mask_valtot.tif")

beginCluster(7)
sat_bsed<-mask(sat_bsed_uca,bare_sediment_mask)
writeRaster(sat_bsed,"Data_out/Stack/sat_bsed",driver="raster")
endCluster()
beep(3)

sat_bsed<-stack("Data_out/Stack/sat_bsed.grd") ##Crreated in script All_classes_classification_attempts.R

set.seed(20)
beginCluster(7)
SC1_grainclass_bs_G<-superClass(img=sat_bsed,model="rf",trainData=GT_c_l0_t_Fbs_G,responseCol="grain_uca_bsed_g",valData=GT_c_l0_v_Fbs_G,polygonBasedCV=F,predict=T,
                              predType="raw",filename=NULL)
endCluster()
beep(3)
saveRSTBX(SC1_grainclass_bs_G,"Data_out/models/SC1_grainclass_bs_G",format="raster",overwrite=F)
SC1_grainclass_bs_G<-readRSTBX("Data_out/models/SC1_grainclass_bs_G.tif")
SC1_grainclass_bs_G$classMapping

plot(SC1_grainclass_bs_G$map, colNA=1, main="grain size")
#writeRaster(SC1_grainclass_bs$map,"Data_out/models/SC1_grainclass_bs.tif")

################ for UCA now ##########

DF9<-DF7[cvr_vrA=="uca"]
DF9[,table(grain_uca_bsed_g)]

set.seed(200)
trainIndex_Fuca_G <- createDataPartition(DF9$grain_uca_bsed_g, p = .7, 
                                       list = FALSE, 
                                       times = 1)
head(trainIndex_Fuca_G)

L0_train_Fuca_G<-DF9[trainIndex_Fuca_G]
L0_train_Fuca_G[,table(grain_uca_bsed_g)]

L0_val_Fuca_G<-DF9[-trainIndex_Fuca_G]
L0_val_Fuca_G[,table(grain_uca_bsed_g)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_Fuca_G<-merge(GT_c1,L0_train_Fuca_G,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_G,col="red")
#str(GT_c_l0_t_Fuca_G@data)

GT_c_l0_v_Fuca_G<-merge(GT_c1,L0_val_Fuca_G,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_G)
#str(GT_c_l0_v_Fuca_G@data)


sat_uca<-stack("Data_out/Stack/sat_uca.tif")

set.seed(20)
beginCluster(7)
SC1_grainclass_uca_G<-superClass(img=sat_uca,model="rf",trainData=GT_c_l0_t_Fuca_G,responseCol="grain_uca_bsed_g",valData=GT_c_l0_v_Fuca_G,polygonBasedCV=F,predict=T,
                               predType="raw",filename=NULL)
endCluster()
beep(3)
saveRSTBX(SC1_grainclass_uca_G,"Data_out/models/SC1_grainclass_uca_G",format="raster",overwrite=T)
SC1_grainclass_uca_G$classMapping

plot(SC1_grainclass_uca_G$map, colNA=1, main="grain size")
#writeRaster(SC1_grainclass_uca_G$map,"Data_out/models/SC1_grainclass_uca_G.tif")




