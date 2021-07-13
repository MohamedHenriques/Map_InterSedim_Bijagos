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

packs<-c("randomForest","caret","sf","beepr","RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis","data.table","reshape2")
npacks <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(npacks)) install.packages(npacks)
#install_github("vqv/ggbiplot")
lapply(packs,require,character.only=T)

## Load sat img

sat<-stack("Data_out/Stack/Final_stack1.grd") ##created in script GraVSSat_Preliminary
names(sat)

##Load GT polygons
GT_c1<-readOGR("Data_out/Polygons/GT_c1.shp") ##created in script Data_cleanup_SUp_Class
#plot(GT_c1)

DF2<-data.table(GT_c1@data)
#str(DF2)
########################################

##Split data in training + validation using caret balanced splitting: Use this for final validation
DF3<-data.table(DF2)
DF3[,table(cvr_vrA)]
DF4<-DF3[!(is.na(cvr_vrA))] ## remove 2 bad points with NA everywhere

DF4[,cvr_vrA6:=as.character(cvr_vrA)][cvr_vrA6=="bare_sediment",cvr_vrA6:="sediments"][cvr_vrA6=="uca",cvr_vrA6:="sediments"]
DF4[,table(cvr_vrA6)]

set.seed(1)
trainIndex <- createDataPartition(DF4$cvr_vrA6, p = .7, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

L0_train<-DF4[trainIndex]
L0_train[,table(cvr_vrA6)]

L0_val<-DF4[-trainIndex]
L0_val[,table(cvr_vrA6)]

###Introduce new columns on training and validation polygons
GT_c_l0_t<-merge(GT_c1,L0_train,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t,col="red")
#str(GT_c_l0_t@data)

GT_c_l0_v<-merge(GT_c1,L0_val,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v)
#str(GT_c_l0_v@data)


### selection of bands to use
sat1<-sat[[-c(16:17,20,22:24)]]
names(sat1)

sat2<-sat[[-c(1:8,14,17,19)]]
names(sat2)

sat3<-subset(sat,c("B02_20200204","B03_20200204","B04_20200204","B08A_20200204","B11_20200204","S1_20200128_VH","S1_20200128_VV","Final_DEM_nodelay","MSAVI2","mNDWI","NDWI","rededge_multi","RVI","NDVI"))
names(sat3)

sat4<-subset(sat,c("B11_20200204","S1_20200128_VH","S1_20200128_VV","Final_DEM_nodelay","MSAVI2","NDMI","NDWI","intensity","rededge_multi","iv_multi","RVI","NDVI"))
names(sat4)

### PCAs
PCA_tot<-readRSTBX("Data_out/PCA/PCA_tot.tif")
summary(PCA_tot$model)
PCA_tot$model$loadings
PCA_tot_map<-PCA_tot$map

PCA_tot1<-readRSTBX("Data_out/PCA/PCA_tot1.tif")
summary(PCA_tot1$model)
PCA_tot1$model$loadings
PCA_tot1_map<-PCA_tot1$map

PCA_tot_sel1<-readRSTBX("Data_out/PCA/PCA_tot_sel1")
summary(PCA_tot_sel1$model)
PCA_tot_sel1$model$loadings
PCA_tot_sel1_map<-PCA_tot_sel1$map

PCA_tot_sel2<-readRSTBX("Data_out/PCA/PCA_tot_sel2")
summary(PCA_tot_sel2$model)
PCA_tot_sel2$model$loadings
PCA_tot_sel2_map<-PCA_tot_sel2$map

##########################################################################
##########################################################################
### Supervised class of cover over with rstoolbox and rf: Step 1
start<-Sys.time()

set.seed(12)
beginCluster(7)
SC1_1<-superClass(img=sat1[[1:10]],model="rf",trainData=GT_c_l0_t,responseCol="cvr_vrA6",valData=GT_c_l0_v,polygonBasedCV=F,predict=T,
                        predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif<-end-start


saveRSTBX(SC1_all,"Data_out/models/SC1_all",fomat="raster",overwrite=T)
SC1_all<-readRSTBX("Data_out/models/SC1_all")
saveRSTBX(SC1_1,"Data_out/models/SC1_1",fomat="raster",overwrite=T)
SC1_1<-readRSTBX("Data_out/models/SC1_1") ## First best
saveRSTBX(SC1_2,"Data_out/models/SC1_2",fomat="raster",overwrite=T)
SC1_2<-readRSTBX("Data_out/models/SC1_2")
saveRSTBX(SC1_3,"Data_out/models/SC1_3",fomat="raster",overwrite=T)
SC1_3<-readRSTBX("Data_out/models/SC1_3")
saveRSTBX(SC1_4,"Data_out/models/SC1_4",fomat="raster",overwrite=T)
SC1_4<-readRSTBX("Data_out/models/SC1_4") ## Second best

saveRSTBX(SC1_PCA_6,"Data_out/models/SC1_PCA_6",fomat="raster",overwrite=T)
SC1_PCA_6<-readRSTBX("Data_out/models/SC1_PCA_6")
saveRSTBX(SC1_PCA_10,"Data_out/models/SC1_PCA_10",fomat="raster",overwrite=T)
SC1_PCA_10<-readRSTBX("Data_out/models/SC1_PCA_10") ## Best PCA
saveRSTBX(SC1_PCA1_6,"Data_out/models/SC1_PCA1_6",fomat="raster",overwrite=T)
SC1_PCA1_6<-readRSTBX("Data_out/models/SC1_PCA1_6")
saveRSTBX(SC1_PCA1_10,"Data_out/models/SC1_PCA1_10",fomat="raster",overwrite=T)
SC1_PCA1_10<-readRSTBX("Data_out/models/SC1_PCA1_10")
saveRSTBX(SC1_PCA2_7,"Data_out/models/SC1_PCA2_7",fomat="raster",overwrite=T)
SC1_PCA2_7<-readRSTBX("Data_out/models/SC1_PCA2_7")
saveRSTBX(SC1_PCA2_9,"Data_out/models/SC1_PCA2_9",fomat="raster",overwrite=T)
SC1_PCA2_9<-readRSTBX("Data_out/models/SC1_PCA2_9")
saveRSTBX(SC1_PCA0_6,"Data_out/models/SC1_PCA0_6",fomat="raster",overwrite=T)
SC1_PCA0_6<-readRSTBX("Data_out/models/SC1_PCA0_6")
saveRSTBX(SC1_PCA0_10,"Data_out/models/SC1_PCA0_10",fomat="raster",overwrite=T)
SC1_PCA0_10<-readRSTBX("Data_out/models/SC1_PCA0_10")

SC1_1$model$finalModel$importance


SC1_1$classMapping
plot(SC1_1$map,colNA=1,col=c("green","red","lightgrey","blue"))
SC1_1_tif<-SC1_1$map
#writeRaster(SC1_tif,"Data_out/models/SC1_selvar.tif")

xx<-drawExtent()
adonga_t<-crop(SC1_1$map,xx)
plot(adonga_t, colNA=1,col=c("forestgreen","red","lightgrey","blue"))


###Isolating sediments area
seds_mask<-SC1_1_tif==3
seds_mask[seds_mask==0]<-NA # turn remaining area (coded zero) into NA
plot(seds_mask, colNA=1)
writeRaster(seds_mask,"Data_out/Habitat_classes/Level0/seds_mask_selvar.tif", overwrite=T)

### Saving the macro
mask_macro_t<-SC1_1_tif==1
mask_macro_t[mask_macro_t==0]<-NA # turn remaining area (coded zero) into NA
plot(mask_macro_t, colNA=1)
writeRaster(mask_macro_t,"Data_out/mask/mask_macro_t_selvar.tif", overwrite=T)

### Saving the rocks
mask_rocks_t<-SC1_1_tif==2
mask_rocks_t[mask_rocks_t==0]<-NA # turn remaining area (coded zero) into NA
plot(mask_rocks_t, colNA=1)
writeRaster(mask_rocks_t,"Data_out/mask/mask_rocks_t_selvar.tif", overwrite=T)

### Saving the shells
mask_shells_t<-SC1_1_tif==4
mask_shells_t[mask_shells_t==0]<-NA # turn remaining area (coded zero) into NA
plot(mask_shells_t, colNA=1)
writeRaster(mask_shells_t,"Data_out/mask/mask_shells_t_selvar.tif", overwrite=T)


area_seds<-sum(seds_mask[seds_mask==1])*res(seds_mask)[1]^2*10^-6 #calculate sediments area size in Km2
area_macro<-sum(mask_macro_t[mask_macro_t==1])*res(mask_macro_t)[1]^2*10^-6 #calculate sediments area size in Km2
area_rocks<-sum(mask_rocks_t[mask_rocks_t==1])*res(mask_rocks_t)[1]^2*10^-6 #calculate sediments area size in Km2
area_shells<-sum(mask_shells_t[mask_shells_t==1])*res(mask_shells_t)[1]^2*10^-6 #calculate sediments area size in Km2


###########################################################################
####################################################################################
################## Step 2 classification - uca vs bare sediment ###################################

DF4[,.(table(uca))]
DF4[,finos_class:=ifelse(mud<10,"sandy_010",ifelse(mud>=10&mud<=100,"muddy_10100",NA))]
DF4[!(is.na(mud)),table(finos_class)]
DF4[,Final_finos_class:=paste(cvr_vrA,finos_class,sep="_")][cvr_vrA=="macroalgae"|cvr_vrA=="rock"|cvr_vrA=="shell",Final_finos_class:=cvr_vrA]
DF4[,.(table(Final_finos_class))]

DF5<-DF4[!(Final_finos_class=="macroalgae"|Final_finos_class=="rock"|Final_finos_class=="shell")] ## database for exposed sed areas
DF5[,.(table(uca))]

### Training and validation data sets
set.seed(10)
trainIndex <- createDataPartition(DF5$uca, p = .70, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

L1_train_seds<-DF5[trainIndex]
L1_train_seds[,table(uca)]

L1_val_seds<-DF5[-trainIndex]
L1_val_seds[,table(uca)]

###Introduce new columns on training and validation polygons
GT_c_l1_t_seds<-merge(GT_c1,L1_train_seds,by="Point",all.x=F,all.y=T)

GT_c_l1_v_seds<-merge(GT_c1,L1_val_seds,by="Point",all.x=F,all.y=T)


### select variables
#seds_mask<-raster("Data_out/Habitat_classes/Level0/seds_mask_selvar.tif")
#beginCluster()
#sat_seds<-mask(sat,seds_mask)
#endCluster()
#writeRaster(sat_seds,"Data_out/Stack/sat_seds.grd",format="raster")
#plot(sat_seds[[1]])

sat_seds<-stack("Data_out/Stack/sat_seds.grd")
names(sat_seds)

sat1_seds<-sat_seds[[-c(16:17,20,22:24)]]
names(sat1_seds)

sat2_seds<-sat_seds[[-c(1:8,14,17,19)]]
names(sat2_seds)

sat3_seds<-subset(sat_seds,c("B02_20200204","B03_20200204","B04_20200204","B08A_20200204","B11_20200204","S1_20200128_VH","S1_20200128_VV","Final_DEM_nodelay","MSAVI2","mNDWI","NDWI","rededge_multi","RVI","NDVI"))
names(sat3_seds)

sat4_seds<-subset(sat_seds,c("B11_20200204","S1_20200128_VH","S1_20200128_VV","Final_DEM_nodelay","MSAVI2","NDMI","NDWI","intensity","rededge_multi","iv_multi","RVI","NDVI"))
names(sat4_seds)

###PCAs for step 2

PCA_tot<-readRSTBX("Data_out/PCA/PCA_tot.tif")
summary(PCA_tot$model)
PCA_tot$model$loadings
PCA_tot_map<-PCA_tot$map

PCA_tot1<-readRSTBX("Data_out/PCA/PCA_tot1.tif")
summary(PCA_tot1$model)
PCA_tot1$model$loadings
PCA_tot1_map<-PCA_tot1$map

PCA_tot_sel1<-readRSTBX("Data_out/PCA/PCA_tot_sel1")
summary(PCA_tot_sel1$model)
PCA_tot_sel1$model$loadings
PCA_tot_sel1_map<-PCA_tot_sel1$map

PCA_tot_sel2<-readRSTBX("Data_out/PCA/PCA_tot_sel2")
summary(PCA_tot_sel2$model)
PCA_tot_sel2$model$loadings
PCA_tot_sel2_map<-PCA_tot_sel2$map


######## Random forest class #########
start<-Sys.time()

set.seed(12)
beginCluster(7)
SC1_ucabs<-superClass(img=sat2_seds,model="rf",trainData=GT_c_l1_t_seds,responseCol="uca.y",valData=GT_c_l1_v_seds,polygonBasedCV=F,predict=T,
                     predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif<-end-start

saveRSTBX(SC1_ucabs,"Data_out/models/SC1_ucabs",fomat="raster",overwrite=T)
saveRSTBX(SC1_ucabs,"Data_out/models/SC1_ucabs_1",fomat="raster",overwrite=T)
SC1_ucabs<-readRSTBX("Data_out/models/SC1_ucabs")

SC1_ucabs$model$finalModel$importance

SC1_ucabs$classMapping
plot(SC1_ucabs$map,colNA=1,col=c("lightgrey","grey35"))
SC1_ucabs_tif<-SC1_ucabs$map
writeRaster(SC1_ucabs_tif,"Data_out/models/SC1_ucabs_selvar.tif")

xx<-drawExtent()
subs_t<-crop(SC1_ucabs$map,xx)
plot(subs_t, colNA=1,col=c("khaki1","grey35"))


###Isolating bare sediment area
baresed_mask<-SC1_ucabs_tif==1
baresed_mask[baresed_mask==0]<-NA # turn remaining area (coded zero) into NA
plot(baresed_mask, colNA=1)
writeRaster(baresed_mask,"Data_out/Habitat_classes/Level0/baresed_mask_selvar.tif", overwrite=T)

### isolating uca
uca_mask<-SC1_ucabs_tif==2
uca_mask[uca_mask==0]<-NA # turn remaining area (coded zero) into NA
plot(uca_mask, colNA=1)
writeRaster(uca_mask,"Data_out/mask/uca_mask_selvar.tif", overwrite=T)




#######################################################
#####################################################################
######## Classifing uca areas ##################

DF5[,.(table(uca))]

DF6<-DF5[!(Final_finos_class=="bare_sediment_NA"|Final_finos_class=="uca_NA")]
DF6[,.(table(Final_finos_class))]

DF6[,finos_grad:=ifelse(finos_class=="sandy_010",paste(finos_class,Sd_cls1,sep="_"),finos_class)]
DF6[,.(table(finos_grad))]
DF6[,Final_finos_grad:=ifelse(Final_finos_class=="bare_sediment_sandy_010"|Final_finos_class=="uca_sandy_010",paste(Final_finos_class,Sd_cls1,sep="_"),Final_finos_class)]
DF6[,.(table(Final_finos_grad))]
DF6[Final_finos_grad=="uca_sandy_010_Medium Sand",uca:="other"][Final_finos_grad=="uca_sandy_010_Medium Sand",Final_finos_grad:="bare_sediment_sandy_010_Medium Sand"]
DF6[,.(table(Final_finos_grad))]

DF6_uca<-DF6[uca=="uca"][,uca:=as.character(uca)] ##Database only for uca
DF6_uca[,table(uca)]
DF6_uca[,.(table(Final_finos_grad))]

### Devide data for validation (30% for validation)
set.seed(200)
trainIndex_mg1uca <- createDataPartition(DF6_uca$Final_finos_grad, p = .7, 
                                         list = FALSE, 
                                         times = 1)
head(trainIndex_mg1uca)

L0_train_mg1uca<-DF6_uca[trainIndex_mg1uca]
#L0_train_mg1uca[,table(Final_finos_grad)]

L0_val_mg1uca<-DF6_uca[-trainIndex_mg1uca]
#L0_val_mg1uca[,table(Final_finos_grad)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_mg1uca<-merge(GT_c1,L0_train_mg1uca,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_mg1a,col="red")
#str(GT_c_l0_t_mg1a@data)

GT_c_l0_v_mg1uca<-merge(GT_c1,L0_val_mg1uca,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_mg1a)
#str(GT_c_l0_v_mg1a@data)


### select variables
uca_mask<-raster("Data_out/mask/uca_mask_selvar.tif")
beginCluster()
sat_uca<-mask(sat,uca_mask)
endCluster()
writeRaster(sat_uca,"Data_out/Stack/sat_uca.grd",format="raster")
#plot(sat_uca[[1]])

sat_uca<-stack("Data_out/Stack/sat_uca.grd")
names(sat_uca)

sat1_uca<-sat_uca[[-c(16:17,20,22:24)]]
names(sat1_uca)

sat2_uca<-sat_uca[[-c(1:8,14,17,19)]]
names(sat2_uca)

sat3_uca<-subset(sat_uca,c("B02_20200204","B03_20200204","B04_20200204","B08A_20200204","B11_20200204","S1_20200128_VH","S1_20200128_VV","Final_DEM_nodelay","MSAVI2","mNDWI","NDWI","rededge_multi","RVI","NDVI"))
names(sat3_uca)

sat4_uca<-subset(sat_uca,c("B11_20200204","S1_20200128_VH","S1_20200128_VV","Final_DEM_nodelay","MSAVI2","NDMI","NDWI","intensity","rededge_multi","iv_multi","RVI","NDVI"))
names(sat4_uca)

###PCAs for step 2

PCA_tot<-readRSTBX("Data_out/PCA/PCA_tot.tif")
summary(PCA_tot$model)
PCA_tot$model$loadings
PCA_tot_map<-PCA_tot$map

PCA_tot1<-readRSTBX("Data_out/PCA/PCA_tot1.tif")
summary(PCA_tot1$model)
PCA_tot1$model$loadings
PCA_tot1_map<-PCA_tot1$map

PCA_tot_sel1<-readRSTBX("Data_out/PCA/PCA_tot_sel1")
summary(PCA_tot_sel1$model)
PCA_tot_sel1$model$loadings
PCA_tot_sel1_map<-PCA_tot_sel1$map

PCA_tot_sel2<-readRSTBX("Data_out/PCA/PCA_tot_sel2")
summary(PCA_tot_sel2$model)
PCA_tot_sel2$model$loadings
PCA_tot_sel2_map<-PCA_tot_sel2$map

### Random forest classification
start<-Sys.time()

set.seed(12)
beginCluster(7)
SC1_mg1uca<-superClass(img=sat_uca,model="rf",trainData=GT_c_l0_t_mg1uca,responseCol="Final_finos_grad",valData=GT_c_l0_v_mg1uca,polygonBasedCV=F,predict=T,
                       predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_mg1uca<-end-start

saveRSTBX(SC1_mg1uca,"Data_out/models/SC1_mg1uca",fomat="raster",overwrite=T)
SC1_mg1uca<-readRSTBX("Data_out/models/SC1_mg1uca")

SC1_mg1uca$model$finalModel$importance

SC1_mg1uca$classMapping
plot(SC1_mg1uca$map,colNA=1,col=c("cadetblue","lightgrey","cadetblue1"))
SC1_mg1uca_tif<-SC1_mg1uca$map
writeRaster(SC1_mg1uca_tif,"Data_out/models/SC1_mg1uca.tif")

xx<-drawExtent()
subs_t<-crop(SC1_mg1uca$map,xx)
plot(subs_t, colNA=1,col=c("cadetblue","lightgrey","cadetblue1"))











#######################################################
#####################################################################
######## Classifing bare sediment areas ##################

DF6_bs<-DF6[uca=="other"][,uca:=as.character(uca)] ##Database only for bs
DF6_bs[,table(uca)]
DF6_bs[,.(table(Final_finos_grad))]

### Devide data for validation (30% for validation)
set.seed(200)
trainIndex_mg1bs <- createDataPartition(DF6_bs$Final_finos_grad, p = .7, 
                                         list = FALSE, 
                                         times = 1)
head(trainIndex_mg1bs)

L0_train_mg1bs<-DF6_bs[trainIndex_mg1bs]
#L0_train_mg1bs[,table(Final_finos_grad)]

L0_val_mg1bs<-DF6_bs[-trainIndex_mg1bs]
#L0_val_mg1bs[,table(Final_finos_grad)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_mg1bs<-merge(GT_c1,L0_train_mg1bs,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_mg1a,col="red")
#str(GT_c_l0_t_mg1a@data)

GT_c_l0_v_mg1bs<-merge(GT_c1,L0_val_mg1bs,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_mg1a)
#str(GT_c_l0_v_mg1a@data)


### select variables
baresed_mask<-raster("Data_out/Habitat_classes/Level0/baresed_mask_selvar.tif")
beginCluster()
sat_bs<-mask(sat,baresed_mask)
endCluster()
writeRaster(sat_bs,"Data_out/Stack/sat_bs.grd",format="raster")
#plot(sat_bs[[1]])

sat_bs<-stack("Data_out/Stack/sat_bs.grd")
names(sat_bs)

sat1_bs<-sat_bs[[-c(16:17,20,22:24)]]
names(sat1_bs)

sat2_bs<-sat_bs[[-c(1:8,14,17,19)]]
names(sat2_bs)

sat3_bs<-subset(sat_bs,c("B02_20200204","B03_20200204","B04_20200204","B08A_20200204","B11_20200204","S1_20200128_VH","S1_20200128_VV","Final_DEM_nodelay","MSAVI2","mNDWI","NDWI","rededge_multi","RVI","NDVI"))
names(sat3_bs)

sat4_bs<-subset(sat_bs,c("B11_20200204","S1_20200128_VH","S1_20200128_VV","Final_DEM_nodelay","MSAVI2","NDMI","NDWI","intensity","rededge_multi","iv_multi","RVI","NDVI"))
names(sat4_bs)

###PCAs for step 2

PCA_tot<-readRSTBX("Data_out/PCA/PCA_tot.tif")
summary(PCA_tot$model)
PCA_tot$model$loadings
PCA_tot_map<-PCA_tot$map

PCA_tot1<-readRSTBX("Data_out/PCA/PCA_tot1.tif")
summary(PCA_tot1$model)
PCA_tot1$model$loadings
PCA_tot1_map<-PCA_tot1$map

PCA_tot_sel1<-readRSTBX("Data_out/PCA/PCA_tot_sel1")
summary(PCA_tot_sel1$model)
PCA_tot_sel1$model$loadings
PCA_tot_sel1_map<-PCA_tot_sel1$map

PCA_tot_sel2<-readRSTBX("Data_out/PCA/PCA_tot_sel2")
summary(PCA_tot_sel2$model)
PCA_tot_sel2$model$loadings
PCA_tot_sel2_map<-PCA_tot_sel2$map

### Random forest classification
start<-Sys.time()

set.seed(12)
beginCluster(7)
SC1_mg1bs<-superClass(img=sat2_bs,model="rf",trainData=GT_c_l0_t_mg1bs,responseCol="Final_finos_grad",valData=GT_c_l0_v_mg1bs,polygonBasedCV=F,predict=T,
                       predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_mg1bs<-end-start

saveRSTBX(SC1_mg1bs,"Data_out/models/SC1_mg1bs",fomat="raster",overwrite=T)
SC1_mg1bs<-readRSTBX("Data_out/models/SC1_mg1bs")

SC1_mg1bs$model$finalModel$importance

SC1_mg1bs$classMapping
plot(SC1_mg1bs$map,colNA=1,col=c("cadetblue","lightgrey","cadetblue1"))
SC1_mg1bs_tif<-SC1_mg1bs$map
writeRaster(SC1_mg1bs_tif,"Data_out/models/SC1_mg1bs.tif")

xx<-drawExtent()
subs_t<-crop(SC1_mg1bs$map,xx)
plot(subs_t, colNA=1,col=c("cadetblue","lightgrey","cadetblue1"))
