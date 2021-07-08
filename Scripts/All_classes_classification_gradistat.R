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
names(sat)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
                          "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
                          "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","MSAVI2","intensity","iv_multi","rededge_multi","rededge_sum",
                          "visible_multi")


##Load GT polygons
GT_c1<-readOGR("Data_out/Polygons/GT_c1.shp") ##created in script Data_cleanup_SUp_Class
DF2<-data.table(GT_c1@data)
str(DF2)
write.table(DF2,"Data_out/db/GT_c1.csv",row.names=F,sep=";")
#################################################################
##################################################################

#################### classification using all target habitat classes ################################

##Split data in training + validation using caret balanced splitting: Use this for final validation
DF3<-data.table(DF2)
DF3[,table(cvr_sd_g)]

## Sediment classes as defined by EU (2019)
DF3[,Grain_EU:=ifelse(mud<10,"sand_010",ifelse(mud>=10&mud<25,"muddy_sand_1025",ifelse(mud>=25&mud<60,"mixed_2560",ifelse(mud>=60&mud<=100,"muddy_60100",NA))))][,Final_grain_EU:=ifelse(cvr_vrA=="bare_sediment"|cvr_vrA=="uca",paste(cvr_vrA,Grain_EU,sep="_"),ifelse(cvr_vrA=="macroalgae","macroalgae",ifelse(cvr_vrA=="rock","rock",ifelse(cvr_vrA=="shell","shell",NA))))]
DF3[,Final_grain_EU1:=Final_grain_EU][Final_grain_EU=="bare_sediment_muddy_sand_1025",Final_grain_EU1:="bare_sediment_mixed_2560"][Final_grain_EU=="uca_muddy_sand_1025",Final_grain_EU1:="uca_mixed_2560"][Final_grain_EU1=="bare_sediment_mixed_2560",Final_grain_EU1:="bare_sediment_mixed_1060"][Final_grain_EU1=="uca_mixed_2560",Final_grain_EU1:="uca_mixed_1060"]
DF3[,table(Final_grain_EU1)]
DF3[,table(Final_grain_EU)]

DF3[,Grain_EU1:=ifelse(mud<10,"sand_010",ifelse(mud>=10&mud<25,"muddy_sand_1025",ifelse(mud>=25&mud<=100,"muddy_25100",NA)))][,Final_grain_EU2:=ifelse(cvr_vrA=="bare_sediment"|cvr_vrA=="uca",paste(cvr_vrA,Grain_EU1,sep="_"),ifelse(cvr_vrA=="macroalgae","macroalgae",ifelse(cvr_vrA=="rock","rock",ifelse(cvr_vrA=="shell","shell",NA))))]
DF3[,.(table(Final_grain_EU2))]

## Sediment class as used by Belo in MSc Thesis
DF3[,finos_class:=ifelse(mud<10,"sandy_010",ifelse(mud>=10&mud<=100,"muddy_10100",NA))]
DF3[!(is.na(mud)),table(finos_class)]
DF3[,Final_finos_class:=paste(cvr_vrA,finos_class,sep="_")][cvr_vrA=="macroalgae"|cvr_vrA=="rock"|cvr_vrA=="shell",Final_finos_class:=cvr_vrA]
DF3[,table(Final_finos_class)]

## Sediment classes as defined by Folk1970 and redrawn by Beninger2018
DF3[,FolkBeninger:=ifelse(mud<10,"sand_010",ifelse(mud>=10&mud<50,"muddy_sand_1050",ifelse(mud>=50&mud<90,"sandy_mud_5090",ifelse(mud>=90&mud<=100,"mud_90100",NA))))]
DF3[,table(FolkBeninger)]
DF3[,Final_FolkBeninger:=paste(cvr_vrA,FolkBeninger,sep="_")][cvr_vrA=="macroalgae"|cvr_vrA=="rock"|cvr_vrA=="shell",Final_FolkBeninger:=cvr_vrA]
DF3[,table(Final_FolkBeninger)]

## Sediment classes as defined by Folk1970 and redrawn by Beninger2018
DF3[,FolkBeninger1:=ifelse(mud<10,"sand_010",ifelse(mud>=10&mud<90,"mixed_1090",ifelse(mud>=90&mud<=100,"mud_90100",NA)))]
DF3[,table(FolkBeninger1)]
DF3[,Final_FolkBeninger1:=paste(cvr_vrA,FolkBeninger1,sep="_")][cvr_vrA=="macroalgae"|cvr_vrA=="rock"|cvr_vrA=="shell",Final_FolkBeninger1:=cvr_vrA]
DF3[,table(Final_FolkBeninger1)]

## Sediment classes as defined by Folk1970 and redrawn by Beninger2018
DF3[,FolkBeninger2:=ifelse(mud<50,"sandy_0_50",ifelse(mud>=50&mud<=100,"muddy_50_100",NA))]
DF3[,table(FolkBeninger2)]
DF3[,Final_FolkBeninger2:=paste(cvr_vrA,FolkBeninger2,sep="_")][cvr_vrA=="macroalgae"|cvr_vrA=="rock"|cvr_vrA=="shell",Final_FolkBeninger2:=cvr_vrA]
DF3[,table(Final_FolkBeninger2)]

## Sediment classes as defined by Folk1970 and redrawn by Beninger2018
DF3[,FolkBeninger3:=ifelse(mud<10,"sand_010",ifelse(mud>=10&mud<50,"muddy_sand_1050",ifelse(mud>=50&mud<=100,"muddy_50100",NA)))]
DF3[,table(FolkBeninger3)]
DF3[,Final_FolkBeninger3:=paste(cvr_vrA,FolkBeninger3,sep="_")][cvr_vrA=="macroalgae"|cvr_vrA=="rock"|cvr_vrA=="shell",Final_FolkBeninger3:=cvr_vrA]
DF3[,table(Final_FolkBeninger3)]

DF4<-DF3[!(is.na(cvr_vrA))] #remove bad data point with no data



DF4[,.(table(Final_finos_class))]
DF4[,.(table(Final_FolkBeninger))]
DF4[,.(table(Final_FolkBeninger1))]
DF4[,.(table(Final_FolkBeninger2))]
DF4[,.(table(Final_FolkBeninger3))]
DF4[,.(table(Final_grain_EU1))]
DF4[,.(table(Final_grain_EU))]

DF5<-DF4[!(is.na(mud)|cvr_vrA=="macroalgae"|cvr_vrA=="rock"|cvr_vrA=="shell")] ##remove data points for which we do not have measures of mud content

DF5[,.(table(Final_finos_class))]
DF5[,.(table(Final_FolkBeninger))]
DF5[,.(table(Final_FolkBeninger1))]
DF5[,.(table(Final_FolkBeninger2))]
DF5[,.(table(Final_FolkBeninger3))]
DF5[,.(table(Final_grain_EU1))]
DF5[,.(table(Final_grain_EU))]


################# Classification process ###################################

################
##########################
############################# Based on mud content

### Classify all classes at the same time

### Devide data for validation (30% for validation)
DF4_1<-DF4[!(Final_finos_class=="bare_sediment_NA"|Final_finos_class=="uca_NA")]

set.seed(200)
trainIndex_m1 <- createDataPartition(DF4_1$Final_finos_class, p = .7, 
                                    list = FALSE, 
                                    times = 1)
head(trainIndex_m1)

L0_train_m1<-DF4_1[trainIndex_m1]
L0_train_m1[,table(Final_finos_class)]

L0_val_m1<-DF4_1[-trainIndex_m1]
L0_val_m1[,table(Final_finos_class)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_m1<-merge(GT_c1,L0_train_m1,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_m1,col="red")
#str(GT_c_l0_t_m1@data)

GT_c_l0_v_m1<-merge(GT_c1,L0_val_m1,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_m1)
#str(GT_c_l0_v_m1@data)

### select variables
sat_m1_all<-sat[[-c(9,14)]]
names(sat_m1_all)



### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_m1<-superClass(img=sat_m1_all,model="rf",trainData=GT_c_l0_t_m1,responseCol="Final_finos_class",valData=GT_c_l0_v_m1,polygonBasedCV=F,predict=T,
                           predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_m1<-end-start

saveRSTBX(SC1_allclass_m1,"Data_out/models/SC1_allclass_m1",format="raster",overwrite=T)
SC1_allclass_m1<-readRSTBX("Data_out/models/SC1_allclass_m1.tif")

SC1_allclass_m1$model$finalModel$importance

plot(SC1_allclass_m1$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_m1$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_m1$map,d)
plot(d1,colNA=1,col=rainbow(7))

writeRaster(SC1_allclass_m1$map,"Data_out/class_tif/SC1_allclass_m1.tif",overwrite=F)


####Now with selection of variables
sat_m1_sel<-subset(sat_m1_all,c()) ## selected according to PCA and correlation matrix

### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_m1_sel<-superClass(img=sat_m1_sel,model="rf",trainData=GT_c_l0_t_m1,responseCol="finos_class",valData=GT_c_l0_v_m1,polygonBasedCV=F,predict=T,
                            predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_m1_sel<-end-start

saveRSTBX(SC1_allclass_m1_sel,"Data_out/models/SC1_allclass_m1_sel",format="raster",overwrite=T)
SC1_allclass_m1_sel<-readRSTBX("Data_out/models/SC1_allclass_m1_sel.tif")

SC1_allclass_m1_sel$model$finalModel$importance

plot(SC1_allclass_m1_sel$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_m1_sel$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_m1_sel$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_m1_sel$map,"Data_out/class_tif/SC1_allclass_m1_sel.tif")




### Classify ony sed classes (after removing macroalgae, rocks and shells)

### Devide data for validation (30% for validation)
set.seed(200)
trainIndex_m1a <- createDataPartition(DF5$Final_finos_class, p = .7, 
                                     list = FALSE, 
                                     times = 1)
head(trainIndex_m1a)

L0_train_m1a<-DF5[trainIndex_m1a]
#L0_train_m1a[,table(Final_finos_class)]

L0_val_m1a<-DF5[-trainIndex_m1a]
#L0_val_m1a[,table(Final_finos_class)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_m1a<-merge(GT_c1,L0_train_m1a,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_m1a,col="red")
#str(GT_c_l0_t_m1a@data)

GT_c_l0_v_m1a<-merge(GT_c1,L0_val_m1a,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_m1a)
#str(GT_c_l0_v_m1a@data)

### select variables
sat_seds<-stack("Data_out/Stack/sat_seds.tif")
names(sat_seds)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
                   "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
                   "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","MSAVI2","intensity","iv_multi","rededge_multi","rededge_sum",
                   "visible_multi")

sat_seds_m1a_all<-sat_seds[[-c(9,14)]]
names(sat_seds_m1a_all)



### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_m1a<-superClass(img=sat_seds_m1a_all,model="rf",trainData=GT_c_l0_t_m1a,responseCol="Final_finos_class",valData=GT_c_l0_v_m1a,polygonBasedCV=F,predict=T,
                            predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_m1a<-end-start

saveRSTBX(SC1_allclass_m1a,"Data_out/models/SC1_allclass_m1a",format="raster",overwrite=T)
SC1_allclass_m1a<-readRSTBX("Data_out/models/SC1_allclass_m1a.tif")

SC1_allclass_m1a$model$finalModel$importance

plot(SC1_allclass_m1a$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_m1a$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_m1a$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_m1a$map,"Data_out/class_tif/SC1_allclass_m1a.tif",overwrite=F)



####Now with selection of variables
sat_seds_m1a_sel<-subset(sat_seds_m1a_all,c()) ## selected according to PCA and correlation matrix

### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_m1a_sel<-superClass(img=sat_seds_m1a_sel,model="rf",trainData=GT_c_l0_t_m1a,responseCol="finos_class",valData=GT_c_l0_v_m1a,polygonBasedCV=F,predict=T,
                                predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_m1_sel<-end-start

saveRSTBX(SC1_allclass_m1a_sel,"Data_out/models/SC1_allclass_m1a_sel",format="raster",overwrite=T)
SC1_allclass_m1a_sel<-readRSTBX("Data_out/models/SC1_allclass_m1a_sel.tif")

plot(SC1_allclass_m1a_sel$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_m1a_sel$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_m1a_sel$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_m1a_sel$map,"Data_out/models/SC1_allclass_m1a_sel.tif")



############################################################################################################
#############################################################################################################
### Final_FolkBeninger


### Classify all classes at the same time

### Devide data for validation (30% for validation)
#DF4_1<-DF4[!(Final_finos_class=="bare_sediment_NA"|Final_finos_class=="uca_NA")]
DF4_1[,.(table(Final_FolkBeninger))]

set.seed(200)
trainIndex_m2 <- createDataPartition(DF4_1$Final_FolkBeninger, p = .7, 
                                     list = FALSE, 
                                     times = 1)
head(trainIndex_m2)

L0_train_m2<-DF4_1[trainIndex_m2]
L0_train_m2[,table(Final_FolkBeninger)]

L0_val_m2<-DF4_1[-trainIndex_m2]
L0_val_m2[,table(Final_FolkBeninger)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_m2<-merge(GT_c1,L0_train_m2,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_m2,col="red")
#str(GT_c_l0_t_m2@data)

GT_c_l0_v_m2<-merge(GT_c1,L0_val_m2,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_m2)
#str(GT_c_l0_v_m2@data)

### select variables
#sat_m1_all<-sat[[-c(9,14)]]
names(sat_m1_all)



### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_m2<-superClass(img=sat_m1_all,model="rf",trainData=GT_c_l0_t_m2,responseCol="Final_FolkBeninger",valData=GT_c_l0_v_m2,polygonBasedCV=F,predict=T,
                            predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_m2<-end-start

saveRSTBX(SC1_allclass_m2,"Data_out/models/SC1_allclass_m2",format="raster",overwrite=F)
SC1_allclass_m2<-readRSTBX("Data_out/models/SC1_allclass_m2.tif")

SC1_allclass_m2$model$finalModel$importance

plot(SC1_allclass_m2$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_m2$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_m2$map,d)
plot(d1,colNA=1,col=rainbow(7))

writeRaster(SC1_allclass_m2$map,"Data_out/class_tif/SC1_allclass_m2.tif",overwrite=F)



####Now with selection of variables
sat_m2_sel<-subset(sat_m2_all,c()) ## selected according to PCA and correlation matrix

### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_m2_sel<-superClass(img=sat_m2_sel,model="rf",trainData=GT_c_l0_t_m2,responseCol="Final_FolkBeninger",valData=GT_c_l0_v_m2,polygonBasedCV=F,predict=T,
                                predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_m2_sel<-end-start

saveRSTBX(SC1_allclass_m2_sel,"Data_out/models/SC1_allclass_m2_sel",format="raster",overwrite=T)
SC1_allclass_m2_sel<-readRSTBX("Data_out/models/SC1_allclass_m2_sel.tif")

SC1_allclass_m2_sel$model$finalModel$importance

plot(SC1_allclass_m2_sel$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_m2_sel$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_m2_sel$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_m2_sel$map,"Data_out/class_tif/SC1_allclass_m2_sel.tif")




############### Classify ony sed classes (after removing macroalgae, rocks and shells)
DF5_1<-DF5[!(DF5$Final_FolkBeninger=="uca_mud_90100")]

### Devide data for validation (30% for validation)
set.seed(200)
trainIndex_m2a <- createDataPartition(DF5_1$Final_FolkBeninger, p = .7, 
                                      list = FALSE, 
                                      times = 1)
head(trainIndex_m2a)

L0_train_m2a<-DF5_1[trainIndex_m2a]
#L0_train_m2a[,table(Final_FolkBeninger)]

L0_val_m2a<-DF5_1[-trainIndex_m2a]
#L0_val_m2a[,table(Final_FolkBeninger)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_m2a<-merge(GT_c1,L0_train_m2a,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_m2a,col="red")
#str(GT_c_l0_t_m2a@data)

GT_c_l0_v_m2a<-merge(GT_c1,L0_val_m2a,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_m2a)
#str(GT_c_l0_v_m2a@data)

### select variables
sat_seds<-stack("Data_out/Stack/sat_seds.tif")
names(sat_seds)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
                   "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
                   "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","MSAVI2","intensity","iv_multi","rededge_multi","rededge_sum",
                   "visible_multi")

#sat_seds_m1a_all<-sat_seds[[-c(9,14)]]
names(sat_seds_m1a_all)



### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_m2a<-superClass(img=sat_seds_m1a_all,model="rf",trainData=GT_c_l0_t_m2a,responseCol="Final_FolkBeninger",valData=GT_c_l0_v_m2a,polygonBasedCV=F,predict=T,
                             predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_m2a<-end-start

saveRSTBX(SC1_allclass_m2a,"Data_out/models/SC1_allclass_m2a",format="raster",overwrite=T)
SC1_allclass_m2a<-readRSTBX("Data_out/models/SC1_allclass_m2a.tif")

SC1_allclass_m2a$model$finalModel$importance

plot(SC1_allclass_m2a$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_m2a$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_m2a$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_m2a$map,"Data_out/class_tif/SC1_allclass_m2a.tif",overwrite=F)



####Now with selection of variables
sat_m2a_sel<-subset(sat_m1_all,c()) ## selected according to PCA and correlation matrix

### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_m2a_sel<-superClass(img=sat_m2a_sel,model="rf",trainData=GT_c_l0_t_m2a,responseCol="Final_FolkBeninger",valData=GT_c_l0_v_m2a,polygonBasedCV=F,predict=T,
                                predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_m2a_sel<-end-start

saveRSTBX(SC1_allclass_m2a_sel,"Data_out/models/SC1_allclass_m2a_sel",format="raster",overwrite=T)
SC1_allclass_m2a_sel<-readRSTBX("Data_out/models/SC1_allclass_m2a_sel.tif")

SC1_allclass_m2a_sel$model$finalModel$importance

plot(SC1_allclass_m2a_sel$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_m2a_sel$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_m2a_sel$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_m2a_sel$map,"Data_out/class_tif/SC1_allclass_m2a_sel.tif")









############################################################################################################
#############################################################################################################
### Final_FolkBeninger1


### Classify all classes at the same time

### Devide data for validation (30% for validation)
#DF4_1<-DF4[!(Final_finos_class=="bare_sediment_NA"|Final_finos_class=="uca_NA")]
DF4_2<-DF4_1[!(Final_FolkBeninger1=="uca_mud_90100")]
DF4_2[,table(Final_FolkBeninger1)]

set.seed(200)
trainIndex_m3 <- createDataPartition(DF4_2$Final_FolkBeninger1, p = .7, 
                                     list = FALSE, 
                                     times = 1)
head(trainIndex_m3)

L0_train_m3<-DF4_2[trainIndex_m3]
L0_train_m3[,table(Final_FolkBeninger1)]

L0_val_m3<-DF4_2[-trainIndex_m3]
L0_val_m3[,table(Final_FolkBeninger1)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_m3<-merge(GT_c1,L0_train_m3,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_m3,col="red")
#str(GT_c_l0_t_m3@data)

GT_c_l0_v_m3<-merge(GT_c1,L0_val_m3,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_m3)
#str(GT_c_l0_v_m3@data)

### select variables
#sat_m1_all<-sat[[-c(9,14)]]
#names(sat_m1_all)



### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_m3<-superClass(img=sat_m1_all,model="rf",trainData=GT_c_l0_t_m3,responseCol="Final_FolkBeninger1",valData=GT_c_l0_v_m3,polygonBasedCV=F,predict=T,
                            predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_m3<-end-start

saveRSTBX(SC1_allclass_m3,"Data_out/models/SC1_allclass_m3",format="raster",overwrite=F)
SC1_allclass_m3<-readRSTBX("Data_out/models/SC1_allclass_m3.tif")

SC1_allclass_m3$model$finalModel$importance

plot(SC1_allclass_m3$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_m3$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_m3$map,d)
plot(d1,colNA=1,col=rainbow(7))

writeRaster(SC1_allclass_m3$map,"Data_out/class_tif/SC1_allclass_m3.tif",overwrite=F)



####Now with selection of variables
sat_m3_sel<-subset(sat_m1_all,c()) ## selected according to PCA and correlation matrix

### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_m3_sel<-superClass(img=sat_m3_sel,model="rf",trainData=GT_c_l0_t_m3,responseCol="Final_FolkBeninger1",valData=GT_c_l0_v_m3,polygonBasedCV=F,predict=T,
                                predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_m3_sel<-end-start

saveRSTBX(SC1_allclass_m3_sel,"Data_out/models/SC1_allclass_m3_sel",format="raster",overwrite=T)
SC1_allclass_m3_sel<-readRSTBX("Data_out/models/SC1_allclass_m3_sel.tif")

SC1_allclass_m3_sel$model$finalModel$importance

plot(SC1_allclass_m3_sel$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_m3_sel$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_m3_sel$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_m3_sel$map,"Data_out/class_tif/SC1_allclass_m3_sel.tif")




############### Classify ony sed classes (after removing macroalgae, rocks and shells)
DF5[,table(Final_FolkBeninger1)]
DF5_1<-DF5[!(DF5$Final_FolkBeninger1=="uca_mud_90100")]


### Devide data for validation (30% for validation)
set.seed(200)
trainIndex_m3a <- createDataPartition(DF5_1$Final_FolkBeninger1, p = .7, 
                                      list = FALSE, 
                                      times = 1)
head(trainIndex_m3a)

L0_train_m3a<-DF5_1[trainIndex_m3a]
#L0_train_m3a[,table(Final_FolkBeninger1)]

L0_val_m3a<-DF5_1[-trainIndex_m3a]
#L0_val_m3a[,table(Final_FolkBeninger1)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_m3a<-merge(GT_c1,L0_train_m3a,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_m3a,col="red")
#str(GT_c_l0_t_m3a@data)

GT_c_l0_v_m3a<-merge(GT_c1,L0_val_m3a,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_m3a)
#str(GT_c_l0_v_m3a@data)

### select variables
sat_seds<-stack("Data_out/Stack/sat_seds.tif")
names(sat_seds)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
                   "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
                   "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","MSAVI2","intensity","iv_multi","rededge_multi","rededge_sum",
                   "visible_multi")

#sat_seds_m1a_all<-sat_seds[[-c(9,14)]]
names(sat_seds_m1a_all)



### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_m3a<-superClass(img=sat_seds_m1a_all,model="rf",trainData=GT_c_l0_t_m3a,responseCol="Final_FolkBeninger1",valData=GT_c_l0_v_m3a,polygonBasedCV=F,predict=T,
                             predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_m3a<-end-start

saveRSTBX(SC1_allclass_m3a,"Data_out/models/SC1_allclass_m3a",format="raster",overwrite=F)
SC1_allclass_m3a<-readRSTBX("Data_out/models/SC1_allclass_m3a.tif")

SC1_allclass_m3a$model$finalModel$importance

plot(SC1_allclass_m3a$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_m3a$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_m3a$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_m3a$map,"Data_out/class_tif/SC1_allclass_m3a.tif",overwrite=F)



####Now with selection of variables
sat_m3a_sel<-subset(sat_m1a_all,c()) ## selected according to PCA and correlation matrix

### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_m3a_sel<-superClass(img=sat_m3a_sel,model="rf",trainData=GT_c_l0_t_m3a,responseCol="Final_FolkBeninger1",valData=GT_c_l0_v_m3a,polygonBasedCV=F,predict=T,
                                 predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_m3a_sel<-end-start

saveRSTBX(SC1_allclass_m3a_sel,"Data_out/models/SC1_allclass_m3a_sel",format="raster",overwrite=T)
SC1_allclass_m3a_sel<-readRSTBX("Data_out/models/SC1_allclass_m3a_sel.tif")

SC1_allclass_m3a_sel$model$finalModel$importance

plot(SC1_allclass_m3a_sel$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_m3a_sel$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_m3a_sel$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_m3a_sel$map,"Data_out/class_tif/SC1_allclass_m3a_sel.tif")










############################################################################################################
#############################################################################################################
### Final_FolkBeninger2


### Classify all classes at the same time

### Devide data for validation (30% for validation)
#DF4_1<-DF4[!(Final_finos_class=="bare_sediment_NA"|Final_finos_class=="uca_NA")]
#DF4_2<-DF4_1[!(Final_FolkBeninger2=="uca_mud_90100")]
DF4_2[,table(Final_FolkBeninger2)]

set.seed(200)
trainIndex_m4 <- createDataPartition(DF4_2$Final_FolkBeninger2, p = .7, 
                                     list = FALSE, 
                                     times = 1)
head(trainIndex_m4)

L0_train_m4<-DF4_2[trainIndex_m4]
L0_train_m4[,table(Final_FolkBeninger2)]

L0_val_m4<-DF4_2[-trainIndex_m4]
L0_val_m4[,table(Final_FolkBeninger2)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_m4<-merge(GT_c1,L0_train_m4,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_m4,col="red")
#str(GT_c_l0_t_m4@data)

GT_c_l0_v_m4<-merge(GT_c1,L0_val_m4,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_m4)
#str(GT_c_l0_v_m4@data)

### select variables
#sat_m1_all<-sat[[-c(9,14)]]
#names(sat_m1_all)



### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_m4<-superClass(img=sat_m1_all,model="rf",trainData=GT_c_l0_t_m4,responseCol="Final_FolkBeninger2",valData=GT_c_l0_v_m4,polygonBasedCV=F,predict=T,
                            predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_m4<-end-start

saveRSTBX(SC1_allclass_m4,"Data_out/models/SC1_allclass_m4",format="raster",overwrite=F)
SC1_allclass_m4<-readRSTBX("Data_out/models/SC1_allclass_m4.tif")

SC1_allclass_m4$model$finalModel$importance

plot(SC1_allclass_m4$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_m4$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_m4$map,d)
plot(d1,colNA=1,col=rainbow(7))

writeRaster(SC1_allclass_m4$map,"Data_out/class_tif/SC1_allclass_m4.tif",overwrite=F)



####Now with selection of variables
sat_m4_sel<-subset(sat_m1_all,c()) ## selected according to PCA and correlation matrix

### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_m4_sel<-superClass(img=sat_m4_sel,model="rf",trainData=GT_c_l0_t_m4,responseCol="Final_FolkBeninger2",valData=GT_c_l0_v_m4,polygonBasedCV=F,predict=T,
                                predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_m4_sel<-end-start

saveRSTBX(SC1_allclass_m4_sel,"Data_out/models/SC1_allclass_m4_sel",format="raster",overwrite=T)
SC1_allclass_m4_sel<-readRSTBX("Data_out/models/SC1_allclass_m4_sel.tif")

SC1_allclass_m4_sel$model$finalModel$importance

plot(SC1_allclass_m4_sel$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_m4_sel$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_m4_sel$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_m4_sel$map,"Data_out/class_tif/SC1_allclass_m4_sel.tif")




############### Classify ony sed classes (after removing macroalgae, rocks and shells)

#DF5_1<-DF5[!(DF5$Final_FolkBeninger2=="uca_mud_90100")]
DF5[,table(Final_FolkBeninger2)]

### Devide data for validation (30% for validation)
set.seed(200)
trainIndex_m4a <- createDataPartition(DF5$Final_FolkBeninger2, p = .7, 
                                      list = FALSE, 
                                      times = 1)
head(trainIndex_m4a)

L0_train_m4a<-DF5[trainIndex_m4a]
#L0_train_m4a[,table(Final_FolkBeninger2)]

L0_val_m4a<-DF5[-trainIndex_m4a]
#L0_val_m4a[,table(Final_FolkBeninger2)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_m4a<-merge(GT_c1,L0_train_m4a,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_m4a,col="red")
#str(GT_c_l0_t_m4a@data)

GT_c_l0_v_m4a<-merge(GT_c1,L0_val_m4a,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_m4a)
#str(GT_c_l0_v_m4a@data)

### select variables
sat_seds<-stack("Data_out/Stack/sat_seds.tif")
names(sat_seds)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
                   "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
                   "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","MSAVI2","intensity","iv_multi","rededge_multi","rededge_sum",
                   "visible_multi")

#sat_seds_m1a_all<-sat_seds[[-c(9,14)]]
names(sat_seds_m1a_all)



### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_m4a<-superClass(img=sat_seds_m1a_all,model="rf",trainData=GT_c_l0_t_m4a,responseCol="Final_FolkBeninger2",valData=GT_c_l0_v_m4a,polygonBasedCV=F,predict=T,
                             predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_m4a<-end-start

saveRSTBX(SC1_allclass_m4a,"Data_out/models/SC1_allclass_m4a",format="raster",overwrite=F)
SC1_allclass_m4a<-readRSTBX("Data_out/models/SC1_allclass_m4a.tif")

SC1_allclass_m4a$model$finalModel$importance

plot(SC1_allclass_m4a$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_m4a$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_m4a$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_m4a$map,"Data_out/class_tif/SC1_allclass_m4a.tif",overwrite=T)



####Now with selection of variables
sat_m4a_sel<-subset(sat_m1a_all,c()) ## selected according to PCA and correlation matrix

### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_m4a_sel<-superClass(img=sat_m4a_sel,model="rf",trainData=GT_c_l0_t_m4a,responseCol="Final_FolkBeninger2",valData=GT_c_l0_v_m4a,polygonBasedCV=F,predict=T,
                                 predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_m4a_sel<-end-start

saveRSTBX(SC1_allclass_m4a_sel,"Data_out/models/SC1_allclass_m4a_sel",format="raster",overwrite=T)
SC1_allclass_m4a_sel<-readRSTBX("Data_out/models/SC1_allclass_m4a_sel.tif")

SC1_allclass_m4a_sel$model$finalModel$importance

plot(SC1_allclass_m4a_sel$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_m4a_sel$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_m4a_sel$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_m4a_sel$map,"Data_out/class_tif/SC1_allclass_m4a_sel.tif",overwrite=T)









############################################################################################################
#############################################################################################################
### Final_FolkBeninger3


### Classify all classes at the same time

### Devide data for validation (30% for validation)
#DF4_1<-DF4[!(Final_finos_class=="bare_sediment_NA"|Final_finos_class=="uca_NA")]
#DF4_2<-DF4_1[!(Final_FolkBeninger3=="uca_mud_90100")]
DF4_1[,table(Final_FolkBeninger3)]

set.seed(200)
trainIndex_m5 <- createDataPartition(DF4_1$Final_FolkBeninger3, p = .7, 
                                     list = FALSE, 
                                     times = 1)
head(trainIndex_m5)

L0_train_m5<-DF4_1[trainIndex_m5]
L0_train_m5[,table(Final_FolkBeninger3)]

L0_val_m5<-DF4_1[-trainIndex_m5]
L0_val_m5[,table(Final_FolkBeninger3)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_m5<-merge(GT_c1,L0_train_m5,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_m5,col="red")
#str(GT_c_l0_t_m5@data)

GT_c_l0_v_m5<-merge(GT_c1,L0_val_m5,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_m5)
#str(GT_c_l0_v_m5@data)

### select variables
#sat_m1_all<-sat[[-c(9,14)]]
#names(sat_m1_all)



### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_m5<-superClass(img=sat_m1_all,model="rf",trainData=GT_c_l0_t_m5,responseCol="Final_FolkBeninger3",valData=GT_c_l0_v_m5,polygonBasedCV=F,predict=T,
                            predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_m5<-end-start

saveRSTBX(SC1_allclass_m5,"Data_out/models/SC1_allclass_m5",format="raster",overwrite=F)
SC1_allclass_m5<-readRSTBX("Data_out/models/SC1_allclass_m5.tif")

SC1_allclass_m5$model$finalModel$importance

plot(SC1_allclass_m5$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_m5$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_m5$map,d)
plot(d1,colNA=1,col=rainbow(7))

writeRaster(SC1_allclass_m5$map,"Data_out/class_tif/SC1_allclass_m5.tif",overwrite=T)



####Now with selection of variables
sat_m5_sel<-subset(sat_m1_all,c()) ## selected according to PCA and correlation matrix

### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_m5_sel<-superClass(img=sat_m5_sel,model="rf",trainData=GT_c_l0_t_m5,responseCol="Final_FolkBeninger3",valData=GT_c_l0_v_m5,polygonBasedCV=F,predict=T,
                                predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_m5_sel<-end-start

saveRSTBX(SC1_allclass_m5_sel,"Data_out/models/SC1_allclass_m5_sel",format="raster",overwrite=T)
SC1_allclass_m5_sel<-readRSTBX("Data_out/models/SC1_allclass_m5_sel.tif")

SC1_allclass_m5_sel$model$finalModel$importance

plot(SC1_allclass_m5_sel$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_m5_sel$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_m5_sel$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_m5_sel$map,"Data_out/class_tif/SC1_allclass_m5_sel.tif")




############### Classify ony sed classes (after removing macroalgae, rocks and shells)

#DF5_1<-DF5[!(DF5$Final_FolkBeninger3=="uca_mud_90100")]
DF5[,table(Final_FolkBeninger3)]

### Devide data for validation (30% for validation)
set.seed(200)
trainIndex_m5a <- createDataPartition(DF5$Final_FolkBeninger3, p = .7, 
                                      list = FALSE, 
                                      times = 1)
head(trainIndex_m5a)

L0_train_m5a<-DF5[trainIndex_m5a]
#L0_train_m5a[,table(Final_FolkBeninger3)]

L0_val_m5a<-DF5[-trainIndex_m5a]
#L0_val_m5a[,table(Final_FolkBeninger3)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_m5a<-merge(GT_c1,L0_train_m5a,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_m5a,col="red")
#str(GT_c_l0_t_m5a@data)

GT_c_l0_v_m5a<-merge(GT_c1,L0_val_m5a,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_m5a)
#str(GT_c_l0_v_m5a@data)

### select variables
sat_seds<-stack("Data_out/Stack/sat_seds.tif")
names(sat_seds)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
                   "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
                   "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","MSAVI2","intensity","iv_multi","rededge_multi","rededge_sum",
                   "visible_multi")

#sat_seds_m1a_all<-sat_seds[[-c(9,14)]]
names(sat_seds_m1a_all)



### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_m5a<-superClass(img=sat_seds_m1a_all,model="rf",trainData=GT_c_l0_t_m5a,responseCol="Final_FolkBeninger3",valData=GT_c_l0_v_m5a,polygonBasedCV=F,predict=T,
                             predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_m5a<-end-start

saveRSTBX(SC1_allclass_m5a,"Data_out/models/SC1_allclass_m5a",format="raster",overwrite=F)
SC1_allclass_m5a<-readRSTBX("Data_out/models/SC1_allclass_m5a.tif")

SC1_allclass_m5a$model$finalModel$importance

plot(SC1_allclass_m5a$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_m5a$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_m5a$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_m5a$map,"Data_out/class_tif/SC1_allclass_m5a.tif",overwrite=F)



####Now with selection of variables
sat_m5a_sel<-subset(sat_m1a_all,c()) ## selected according to PCA and correlation matrix

### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_m5a_sel<-superClass(img=sat_m5a_sel,model="rf",trainData=GT_c_l0_t_m5a,responseCol="Final_FolkBeninger3",valData=GT_c_l0_v_m5a,polygonBasedCV=F,predict=T,
                                 predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_m5a_sel<-end-start

saveRSTBX(SC1_allclass_m5a_sel,"Data_out/models/SC1_allclass_m5a_sel",format="raster",overwrite=T)
SC1_allclass_m5a_sel<-readRSTBX("Data_out/models/SC1_allclass_m5a_sel.tif")

SC1_allclass_m5a_sel$model$finalModel$importance

plot(SC1_allclass_m5a_sel$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_m5a_sel$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_m5a_sel$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_m5a_sel$map,"Data_out/class_tif/SC1_allclass_m5a_sel.tif")











############################################################################################################
#############################################################################################################
### Final_grain_EU


### Classify all classes at the same time

### Devide data for validation (30% for validation)
#DF4_1<-DF4[!(Final_finos_class=="bare_sediment_NA"|Final_finos_class=="uca_NA")]
#DF4_2<-DF4_1[!(Final_grain_EU=="uca_mud_90100")]
DF4_1[,.(table(Final_grain_EU))]

set.seed(200)
trainIndex_e1 <- createDataPartition(DF4_1$Final_grain_EU, p = .65, 
                                     list = FALSE, 
                                     times = 1)
head(trainIndex_e1)

L0_train_e1<-DF4_1[trainIndex_e1]
L0_train_e1[,table(Final_grain_EU)]

L0_val_e1<-DF4_1[-trainIndex_e1]
L0_val_e1[,table(Final_grain_EU)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_e1<-merge(GT_c1,L0_train_e1,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_e1,col="red")
#str(GT_c_l0_t_e1@data)

GT_c_l0_v_e1<-merge(GT_c1,L0_val_e1,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_e1)
#str(GT_c_l0_v_e1@data)

### select variables
#sat_m1_all<-sat[[-c(9,14)]]
#names(sat_m1_all)



### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_e1<-superClass(img=sat_m1_all,model="rf",trainData=GT_c_l0_t_e1,responseCol="Final_grain_EU",valData=GT_c_l0_v_e1,polygonBasedCV=F,predict=T,
                            predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_e1<-end-start

saveRSTBX(SC1_allclass_e1,"Data_out/models/SC1_allclass_e1",format="raster",overwrite=F)
SC1_allclass_e1<-readRSTBX("Data_out/models/SC1_allclass_e1.tif")

SC1_allclass_e1$model$finalModel$importance

plot(SC1_allclass_e1$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_e1$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_e1$map,d)
plot(d1,colNA=1,col=rainbow(7))

writeRaster(SC1_allclass_e1$map,"Data_out/class_tif/SC1_allclass_e1.tif",overwrite=F)



####Now with selection of variables
sat_e1_sel<-subset(sat_m1_all,c()) ## selected according to PCA and correlation matrix

### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_e1_sel<-superClass(img=sat_e1_sel,model="rf",trainData=GT_c_l0_t_e1,responseCol="Final_grain_EU",valData=GT_c_l0_v_e1,polygonBasedCV=F,predict=T,
                                predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_e1_sel<-end-start

saveRSTBX(SC1_allclass_e1_sel,"Data_out/models/SC1_allclass_e1_sel",format="raster",overwrite=T)
SC1_allclass_e1_sel<-readRSTBX("Data_out/models/SC1_allclass_e1_sel.tif")

SC1_allclass_e1_sel$model$finalModel$importance

plot(SC1_allclass_e1_sel$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_e1_sel$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_e1_sel$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_e1_sel$map,"Data_out/class_tif/SC1_allclass_e1_sel.tif")




############### Classify ony sed classes (after removing macroalgae, rocks and shells)

#DF5_1<-DF5[!(DF5$Final_grain_EU=="uca_mud_90100")]
DF5[,.(table(Final_grain_EU))]

### Devide data for validation (30% for validation)
set.seed(200)
trainIndex_e1a <- createDataPartition(DF5$Final_grain_EU, p = .65, 
                                      list = FALSE, 
                                      times = 1)
head(trainIndex_e1a)

L0_train_e1a<-DF5[trainIndex_e1a]
#L0_train_e1a[,table(Final_grain_EU)]

L0_val_e1a<-DF5[-trainIndex_e1a]
#L0_val_e1a[,table(Final_grain_EU)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_e1a<-merge(GT_c1,L0_train_e1a,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_e1a,col="red")
#str(GT_c_l0_t_e1a@data)

GT_c_l0_v_e1a<-merge(GT_c1,L0_val_e1a,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_e1a)
#str(GT_c_l0_v_e1a@data)

### select variables
sat_seds<-stack("Data_out/Stack/sat_seds.tif")
names(sat_seds)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
                   "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
                   "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","MSAVI2","intensity","iv_multi","rededge_multi","rededge_sum",
                   "visible_multi")

#sat_seds_m1a_all<-sat_seds[[-c(9,14)]]
names(sat_seds_m1a_all)



### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_e1a<-superClass(img=sat_seds_m1a_all,model="rf",trainData=GT_c_l0_t_e1a,responseCol="Final_grain_EU",valData=GT_c_l0_v_e1a,polygonBasedCV=F,predict=T,
                             predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_e1a<-end-start

saveRSTBX(SC1_allclass_e1a,"Data_out/models/SC1_allclass_e1a",format="raster",overwrite=F)
SC1_allclass_e1a<-readRSTBX("Data_out/models/SC1_allclass_e1a.tif")

SC1_allclass_e1a$model$finalModel$importance

plot(SC1_allclass_e1a$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_e1a$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_e1a$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_e1a$map,"Data_out/class_tif/SC1_allclass_e1a.tif",overwrite=F)



####Now with selection of variables
sat_e1a_sel<-subset(sat_m1a_all,c()) ## selected according to PCA and correlation matrix

### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_e1a_sel<-superClass(img=sat_e1a_sel,model="rf",trainData=GT_c_l0_t_e1a,responseCol="Final_grain_EU",valData=GT_c_l0_v_e1a,polygonBasedCV=F,predict=T,
                                 predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_e1a_sel<-end-start

saveRSTBX(SC1_allclass_e1a_sel,"Data_out/models/SC1_allclass_e1a_sel",format="raster",overwrite=T)
SC1_allclass_e1a_sel<-readRSTBX("Data_out/models/SC1_allclass_e1a_sel.tif")

SC1_allclass_e1a_sel$model$finalModel$importance

plot(SC1_allclass_e1a_sel$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_e1a_sel$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_e1a_sel$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_e1a_sel$map,"Data_out/class_tif/SC1_allclass_e1a_sel.tif")









############################################################################################################
#############################################################################################################
### Final_grain_EU1


### Classify all classes at the same time

### Devide data for validation (30% for validation)
#DF4_1<-DF4[!(Final_finos_class=="bare_sediment_NA"|Final_finos_class=="uca_NA")]
#DF4_2<-DF4_1[!(Final_grain_EU1=="uca_mud_90100")]
DF4_1[,.(table(Final_grain_EU1))]

set.seed(200)
trainIndex_e2 <- createDataPartition(DF4_1$Final_grain_EU1, p = .65, 
                                     list = FALSE, 
                                     times = 1)
head(trainIndex_e2)

L0_train_e2<-DF4_1[trainIndex_e2]
L0_train_e2[,table(Final_grain_EU1)]

L0_val_e2<-DF4_1[-trainIndex_e2]
L0_val_e2[,table(Final_grain_EU1)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_e2<-merge(GT_c1,L0_train_e2,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_e2,col="red")
#str(GT_c_l0_t_e2@data)

GT_c_l0_v_e2<-merge(GT_c1,L0_val_e2,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_e2)
#str(GT_c_l0_v_e2@data)

### select variables
#sat_m1_all<-sat[[-c(9,14)]]
#names(sat_m1_all)



### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_e2<-superClass(img=sat_m1_all,model="rf",trainData=GT_c_l0_t_e2,responseCol="Final_grain_EU1",valData=GT_c_l0_v_e2,polygonBasedCV=F,predict=T,
                            predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_e2<-end-start

saveRSTBX(SC1_allclass_e2,"Data_out/models/SC1_allclass_e2",format="raster",overwrite=F)
SC1_allclass_e2<-readRSTBX("Data_out/models/SC1_allclass_e2.tif")

SC1_allclass_e2$model$finalModel$importance

plot(SC1_allclass_e2$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_e2$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_e2$map,d)
plot(d1,colNA=1,col=rainbow(7))

writeRaster(SC1_allclass_e2$map,"Data_out/class_tif/SC1_allclass_e2.tif",overwrite=F)



####Now with selection of variables
sat_e2_sel<-subset(sat_m1_all,c()) ## selected according to PCA and correlation matrix

### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_e2_sel<-superClass(img=sat_e2_sel,model="rf",trainData=GT_c_l0_t_e2,responseCol="Final_grain_EU1",valData=GT_c_l0_v_e2,polygonBasedCV=F,predict=T,
                                predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_e2_sel<-end-start

saveRSTBX(SC1_allclass_e2_sel,"Data_out/models/SC1_allclass_e2_sel",format="raster",overwrite=T)
SC1_allclass_e2_sel<-readRSTBX("Data_out/models/SC1_allclass_e2_sel.tif")

SC1_allclass_e2_sel$model$finalModel$importance

plot(SC1_allclass_e2_sel$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_e2_sel$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_e2_sel$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_e2_sel$map,"Data_out/class_tif/SC1_allclass_e2_sel.tif")




############### Classify ony sed classes (after removing macroalgae, rocks and shells)

#DF5_1<-DF5[!(DF5$Final_grain_EU1=="uca_mud_90100")]
DF5[,table(Final_grain_EU1)]

### Devide data for validation (30% for validation)
set.seed(200)
trainIndex_e2a <- createDataPartition(DF5$Final_grain_EU1, p = .65, 
                                      list = FALSE, 
                                      times = 1)
head(trainIndex_e2a)

L0_train_e2a<-DF5[trainIndex_e2a]
#L0_train_e2a[,table(Final_grain_EU1)]

L0_val_e2a<-DF5[-trainIndex_e2a]
#L0_val_e2a[,table(Final_grain_EU1)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_e2a<-merge(GT_c1,L0_train_e2a,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_e2a,col="red")
#str(GT_c_l0_t_e2a@data)

GT_c_l0_v_e2a<-merge(GT_c1,L0_val_e2a,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_e2a)
#str(GT_c_l0_v_e2a@data)

### select variables
sat_seds<-stack("Data_out/Stack/sat_seds.tif")
names(sat_seds)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
                   "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
                   "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","MSAVI2","intensity","iv_multi","rededge_multi","rededge_sum",
                   "visible_multi")

#sat_seds_m1a_all<-sat_seds[[-c(9,14)]]
names(sat_seds_m1a_all)



### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_e2a<-superClass(img=sat_seds_m1a_all,model="rf",trainData=GT_c_l0_t_e2a,responseCol="Final_grain_EU1",valData=GT_c_l0_v_e2a,polygonBasedCV=F,predict=T,
                             predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_e2a<-end-start

saveRSTBX(SC1_allclass_e2a,"Data_out/models/SC1_allclass_e2a",format="raster",overwrite=F)
SC1_allclass_e2a<-readRSTBX("Data_out/models/SC1_allclass_e2a.tif")

SC1_allclass_e2a$model$finalModel$importance

plot(SC1_allclass_e2a$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_e2a$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_e2a$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_e2a$map,"Data_out/class_tif/SC1_allclass_e2a.tif",overwrite=F)



####Now with selection of variables
sat_e2a_sel<-subset(sat_m1a_all,c()) ## selected according to PCA and correlation matrix

### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_e2a_sel<-superClass(img=sat_e2a_sel,model="rf",trainData=GT_c_l0_t_e2a,responseCol="Final_grain_EU1",valData=GT_c_l0_v_e2a,polygonBasedCV=F,predict=T,
                                 predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_e2a_sel<-end-start

saveRSTBX(SC1_allclass_e2a_sel,"Data_out/models/SC1_allclass_e2a_sel",format="raster",overwrite=T)
SC1_allclass_e2a_sel<-readRSTBX("Data_out/models/SC1_allclass_e2a_sel.tif")

SC1_allclass_e2a_sel$model$finalModel$importance

plot(SC1_allclass_e2a_sel$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_e2a_sel$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_e2a_sel$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_e2a_sel$map,"Data_out/class_tif/SC1_allclass_e2a_sel.tif")






############################################################################################################
#############################################################################################################
### Final_grain_EU2


### Classify all classes at the same time

### Devide data for validation (30% for validation)
#DF4_1<-DF4[!(Final_finos_class=="bare_sediment_NA"|Final_finos_class=="uca_NA")]
#DF4_2<-DF4_1[!(Final_grain_EU2=="uca_mud_90100")]
DF4[,.(table(Final_grain_EU2))]
DF4_1[,.(table(Final_grain_EU2))]

set.seed(200)
trainIndex_e3 <- createDataPartition(DF4_1$Final_grain_EU2, p = .70, 
                                     list = FALSE, 
                                     times = 1)
head(trainIndex_e3)

L0_train_e3<-DF4_1[trainIndex_e3]
L0_train_e3[,table(Final_grain_EU2)]

L0_val_e3<-DF4_1[-trainIndex_e3]
L0_val_e3[,table(Final_grain_EU2)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_e3<-merge(GT_c1,L0_train_e3,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_e3,col="red")
#str(GT_c_l0_t_e3@data)

GT_c_l0_v_e3<-merge(GT_c1,L0_val_e3,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_e3)
#str(GT_c_l0_v_e3@data)

### select variables
#sat_m1_all<-sat[[-c(9,14)]]
#names(sat_m1_all)



### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_e3<-superClass(img=sat_m1_all,model="rf",trainData=GT_c_l0_t_e3,responseCol="Final_grain_EU2",valData=GT_c_l0_v_e3,polygonBasedCV=F,predict=T,
                            predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_e3<-end-start

saveRSTBX(SC1_allclass_e3,"Data_out/models/SC1_allclass_e3",format="raster",overwrite=F)
SC1_allclass_e3<-readRSTBX("Data_out/models/SC1_allclass_e3.tif")

SC1_allclass_e3$model$finalModel$importance

plot(SC1_allclass_e3$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_e3$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_e3$map,d)
plot(d1,colNA=1,col=rainbow(7))

writeRaster(SC1_allclass_e3$map,"Data_out/class_tif/SC1_allclass_e3.tif",overwrite=F)



####Now with selection of variables
sat_e3_sel<-subset(sat_m1_all,c()) ## selected according to PCA and correlation matrix

### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_e3_sel<-superClass(img=sat_e3_sel,model="rf",trainData=GT_c_l0_t_e3,responseCol="Final_grain_EU2",valData=GT_c_l0_v_e3,polygonBasedCV=F,predict=T,
                                predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_e3_sel<-end-start

saveRSTBX(SC1_allclass_e3_sel,"Data_out/models/SC1_allclass_e3_sel",format="raster",overwrite=T)
SC1_allclass_e3_sel<-readRSTBX("Data_out/models/SC1_allclass_e3_sel.tif")

SC1_allclass_e3_sel$model$finalModel$importance

plot(SC1_allclass_e3_sel$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_e3_sel$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_e3_sel$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_e3_sel$map,"Data_out/class_tif/SC1_allclass_e3_sel.tif")




############### Classify ony sed classes (after removing macroalgae, rocks and shells)

#DF5_1<-DF5[!(DF5$Final_grain_EU2=="uca_mud_90100")]
DF5[,table(Final_grain_EU2)]

### Devide data for validation (30% for validation)
set.seed(200)
trainIndex_e3a <- createDataPartition(DF5$Final_grain_EU2, p = .70, 
                                      list = FALSE, 
                                      times = 1)
head(trainIndex_e3a)

L0_train_e3a<-DF5[trainIndex_e3a]
#L0_train_e3a[,table(Final_grain_EU2)]

L0_val_e3a<-DF5[-trainIndex_e3a]
#L0_val_e3a[,table(Final_grain_EU2)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_e3a<-merge(GT_c1,L0_train_e3a,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_e3a,col="red")
#str(GT_c_l0_t_e3a@data)

GT_c_l0_v_e3a<-merge(GT_c1,L0_val_e3a,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_e3a)
#str(GT_c_l0_v_e3a@data)

### select variables
sat_seds<-stack("Data_out/Stack/sat_seds.tif")
names(sat_seds)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
                   "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
                   "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","MSAVI2","intensity","iv_multi","rededge_multi","rededge_sum",
                   "visible_multi")

#sat_seds_m1a_all<-sat_seds[[-c(9,14)]]
names(sat_seds_m1a_all)



### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_e3a<-superClass(img=sat_seds_m1a_all,model="rf",trainData=GT_c_l0_t_e3a,responseCol="Final_grain_EU2",valData=GT_c_l0_v_e3a,polygonBasedCV=F,predict=T,
                             predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_e3a<-end-start

saveRSTBX(SC1_allclass_e3a,"Data_out/models/SC1_allclass_e3a",format="raster",overwrite=F)
SC1_allclass_e3a<-readRSTBX("Data_out/models/SC1_allclass_e3a.tif")

SC1_allclass_e3a$model$finalModel$importance

plot(SC1_allclass_e3a$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_e3a$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_e3a$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_e3a$map,"Data_out/class_tif/SC1_allclass_e3a.tif",overwrite=F)



####Now with selection of variables
sat_e3a_sel<-subset(sat_m1a_all,c()) ## selected according to PCA and correlation matrix

### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_e3a_sel<-superClass(img=sat_e3a_sel,model="rf",trainData=GT_c_l0_t_e3a,responseCol="Final_grain_EU2",valData=GT_c_l0_v_e3a,polygonBasedCV=F,predict=T,
                                 predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_e3a_sel<-end-start

saveRSTBX(SC1_allclass_e3a_sel,"Data_out/models/SC1_allclass_e3a_sel",format="raster",overwrite=T)
SC1_allclass_e3a_sel<-readRSTBX("Data_out/models/SC1_allclass_e3a_sel.tif")

SC1_allclass_e3a_sel$model$finalModel$importance

plot(SC1_allclass_e3a_sel$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_e3a_sel$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_e3a_sel$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_e3a_sel$map,"Data_out/class_tif/SC1_allclass_e3a_sel.tif")
















################### GRADISTAT ##########################################################



############################################################################################################
#############################################################################################################
### cvr_


### Classify all classes at the same time

### Devide data for validation (30% for validation)
#DF4_1<-DF4[!(Final_finos_class=="bare_sediment_NA"|Final_finos_class=="uca_NA")]
#DF4_2<-DF4_1[!(Final_grain_EU1=="uca_mud_90100")]
DF4_3<-DF4[!(cvr_sd_g=="bare_sediment_NA"|cvr_sd_g=="uca_NA"|cvr_sd_g=="uca_Medium Sand")][,cvr_sd_g:=as.character(cvr_sd_g)]
DF4_3[,.(table(cvr_sd_g))]

set.seed(200)
trainIndex_g1 <- createDataPartition(DF4_3$cvr_sd_g, p = .65, 
                                     list = FALSE, 
                                     times = 1)
head(trainIndex_g1)

L0_train_g1<-DF4_3[trainIndex_g1]
L0_train_g1[,table(cvr_sd_g)]

L0_val_g1<-DF4_3[-trainIndex_g1]
L0_val_g1[,table(cvr_sd_g)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_g1<-merge(GT_c1,L0_train_g1,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_g1,col="red")
#str(GT_c_l0_t_g1@data)

GT_c_l0_v_g1<-merge(GT_c1,L0_val_g1,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_g1)
#str(GT_c_l0_v_g1@data)

### select variables
#sat_m1_all<-sat[[-c(9,14)]]
#names(sat_m1_all)



### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_g1<-superClass(img=sat_m1_all,model="rf",trainData=GT_c_l0_t_g1,responseCol="cvr_sd_g.y",valData=GT_c_l0_v_g1,polygonBasedCV=F,predict=T,
                            predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_g1<-end-start

saveRSTBX(SC1_allclass_g1,"Data_out/models/SC1_allclass_g1",format="raster",overwrite=F)
SC1_allclass_g1<-readRSTBX("Data_out/models/SC1_allclass_g1.tif")

SC1_allclass_g1$model$finalModel$importance

plot(SC1_allclass_g1$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_g1$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_g1$map,d)
plot(d1,colNA=1,col=rainbow(7))

writeRaster(SC1_allclass_g1$map,"Data_out/class_tif/SC1_allclass_g1.tif",overwrite=F)



####Now with selection of variables
sat_g1_sel<-subset(sat_m1_all,c()) ## selected according to PCA and correlation matrix

### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_g1_sel<-superClass(img=sat_g1_sel,model="rf",trainData=GT_c_l0_t_g1,responseCol="cvr_sd_g",valData=GT_c_l0_v_g1,polygonBasedCV=F,predict=T,
                                predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_g1_sel<-end-start

saveRSTBX(SC1_allclass_g1_sel,"Data_out/models/SC1_allclass_g1_sel",format="raster",overwrite=T)
SC1_allclass_g1_sel<-readRSTBX("Data_out/models/SC1_allclass_g1_sel.tif")

SC1_allclass_g1_sel$model$finalModel$importance

plot(SC1_allclass_g1_sel$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_g1_sel$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_g1_sel$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_g1_sel$map,"Data_out/class_tif/SC1_allclass_g1_sel.tif")




############### Classify ony sed classes (after removing macroalgae, rocks and shells)

DF5_1<-DF5[!(cvr_sd_g=="bare_sediment_NA"|cvr_sd_g=="uca_Medium Sand"|cvr_sd_g=="uca_NA")]
DF5_1[,cvr_sd_g:=as.character(cvr_sd_g)]
DF5_1[,.(table(cvr_sd_g))]

### Devide data for validation (30% for validation)
set.seed(200)
trainIndex_g1a <- createDataPartition(DF5_1$cvr_sd_g, p = .65, 
                                      list = FALSE, 
                                      times = 1)
head(trainIndex_g1a)

L0_train_g1a<-DF5_1[trainIndex_g1a]
#L0_train_g1a[,table(cvr_sd_g)]

L0_val_g1a<-DF5_1[-trainIndex_g1a]
#L0_val_g1a[,table(cvr_sd_g)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_g1a<-merge(GT_c1,L0_train_g1a,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_g1a,col="red")
#str(GT_c_l0_t_g1a@data)

GT_c_l0_v_g1a<-merge(GT_c1,L0_val_g1a,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_g1a)
#str(GT_c_l0_v_g1a@data)

### select variables
sat_seds<-stack("Data_out/Stack/sat_seds.tif")
names(sat_seds)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
                   "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
                   "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","MSAVI2","intensity","iv_multi","rededge_multi","rededge_sum",
                   "visible_multi")

#sat_seds_m1a_all<-sat_seds[[-c(9,14)]]
names(sat_seds_m1a_all)



### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_g1a<-superClass(img=sat_seds_m1a_all,model="rf",trainData=GT_c_l0_t_g1a,responseCol="cvr_sd_g.y",valData=GT_c_l0_v_g1a,polygonBasedCV=F,predict=T,
                             predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_g1a<-end-start

saveRSTBX(SC1_allclass_g1a,"Data_out/models/SC1_allclass_g1a",format="raster",overwrite=F)
SC1_allclass_g1a<-readRSTBX("Data_out/models/SC1_allclass_g1a.tif")

SC1_allclass_g1a$model$finalModel$importance

plot(SC1_allclass_g1a$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_g1a$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_g1a$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_g1a$map,"Data_out/class_tif/SC1_allclass_g1a.tif",overwrite=F)



####Now with selection of variables
sat_g1a_sel<-subset(sat_m1a_all,c()) ## selected according to PCA and correlation matrix

### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_g1a_sel<-superClass(img=sat_g1a_sel,model="rf",trainData=GT_c_l0_t_g1a,responseCol="cvr_sd_g",valData=GT_c_l0_v_g1a,polygonBasedCV=F,predict=T,
                                 predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_g1a_sel<-end-start

saveRSTBX(SC1_allclass_g1a_sel,"Data_out/models/SC1_allclass_g1a_sel",format="raster",overwrite=T)
SC1_allclass_g1a_sel<-readRSTBX("Data_out/models/SC1_allclass_g1a_sel.tif")

SC1_allclass_g1a_sel$model$finalModel$importance

plot(SC1_allclass_g1a_sel$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_g1a_sel$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_g1a_sel$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_g1a_sel$map,"Data_out/class_tif/SC1_allclass_g1a_sel.tif")









###################  FINOS WITH GRADISTAT ##########################################################



############################################################################################################
#############################################################################################################


### Classify all classes at the same time

### Devide data for validation (30% for validation)
#DF4_1<-DF4[!(Final_finos_class=="bare_sediment_NA"|Final_finos_class=="uca_NA")]
DF4[,.(table(Final_finos_class))]
DF4_1[,.(table(finos_class))]
DF4_1[,finos_grad:=ifelse(finos_class=="sandy_010",paste(finos_class,Sd_cls1,sep="_"),finos_class)]
DF4_1[,.(table(finos_grad))]
DF4_1[,Final_finos_grad:=ifelse(Final_finos_class=="bare_sediment_sandy_010"|Final_finos_class=="uca_sandy_010",paste(Final_finos_class,Sd_cls1,sep="_"),Final_finos_class)]
DF4_1[,.(table(Final_finos_grad))]
DF4_4<-DF4_1[!(Final_finos_grad=="uca_sandy_010_Medium Sand")] ## Because we only have 2 polygons for now
DF4_4[,.(table(Final_finos_grad))]


set.seed(200)
trainIndex_mg1 <- createDataPartition(DF4_4$Final_finos_grad, p = .7, 
                                     list = FALSE, 
                                     times = 1)
head(trainIndex_mg1)

L0_train_mg1<-DF4_4[trainIndex_mg1]
L0_train_mg1[,table(Final_finos_grad)]

L0_val_mg1<-DF4_4[-trainIndex_mg1]
L0_val_mg1[,table(Final_finos_grad)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_mg1<-merge(GT_c1,L0_train_mg1,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_mg1,col="red")
#str(GT_c_l0_t_mg1@data)

GT_c_l0_v_mg1<-merge(GT_c1,L0_val_mg1,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_mg1)
#str(GT_c_l0_v_mg1@data)

### select variables
#sat_m1_all<-sat[[-c(9,14)]]
#names(sat_m1_all)



### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_mg1<-superClass(img=sat_m1_all,model="rf",trainData=GT_c_l0_t_mg1,responseCol="Final_finos_grad",valData=GT_c_l0_v_mg1,polygonBasedCV=F,predict=T,
                            predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_mg1<-end-start

saveRSTBX(SC1_allclass_mg1,"Data_out/models/SC1_allclass_mg1",format="raster",overwrite=F)
SC1_allclass_mg1<-readRSTBX("Data_out/models/SC1_allclass_mg1.tif")

SC1_allclass_mg1$model$finalModel$importance

plot(SC1_allclass_mg1$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_mg1$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_mg1$map,d)
plot(d1,colNA=1,col=rainbow(7))

writeRaster(SC1_allclass_mg1$map,"Data_out/class_tif/SC1_allclass_mg1.tif",overwrite=F)



####Now with selection of variables
sat_mg1_sel<-subset(sat_m1_all,c()) ## selected according to PCA and correlation matrix

### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_mg1_sel<-superClass(img=sat_mg1_sel,model="rf",trainData=GT_c_l0_t_mg1,responseCol="Final_finos_grad",valData=GT_c_l0_v_mg1,polygonBasedCV=F,predict=T,
                                predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_mg1_sel<-end-start

saveRSTBX(SC1_allclass_mg1_sel,"Data_out/models/SC1_allclass_mg1_sel",format="raster",overwrite=T)
SC1_allclass_mg1_sel<-readRSTBX("Data_out/models/SC1_allclass_mg1_sel.tif")

SC1_allclass_mg1_sel$model$finalModel$importance

plot(SC1_allclass_mg1_sel$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_mg1_sel$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_mg1_sel$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_mg1_sel$map,"Data_out/class_tif/SC1_allclass_mg1_sel.tif",overwrite=F)



####Now with PCA of variables
PCA_tot<-readRSTBX("Data_out/PCA/PCA_tot.tif")
summary(PCA_tot$model)
PCA_tot_map<-PCA_tot$map
names(PCA_tot_map)
PCA_mg1_tot1<-PCA_tot_map[[1:7]] ## PCA selection of bands
PCA_mg1_tot2<-PCA_tot_map[[1:11]]


### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_mg1_PCA<-superClass(img=PCA_mg1_tot2,model="rf",trainData=GT_c_l0_t_mg1,responseCol="Final_finos_grad",valData=GT_c_l0_v_mg1,polygonBasedCV=F,predict=T,
                                 predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_mg1_PCA<-end-start

saveRSTBX(SC1_allclass_mg1_PCA,"Data_out/models/SC1_allclass_mg1_PCA",format="raster",overwrite=F)
SC1_allclass_mg1_PCA<-readRSTBX("Data_out/models/SC1_allclass_mg1_PCA.tif")

SC1_allclass_mg1_PCA$model$finalModel$importance

plot(SC1_allclass_mg1_PCA$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_mg1_PCA$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_mg1_PCA$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_mg1_PCA$map,"Data_out/class_tif/SC1_allclass_mg1_PCA.tif",overwrite=F)






############### Classify ony sed classes (after removing macroalgae, rocks and shells)

DF5_1<-DF5[!(cvr_sd_g=="bare_sediment_NA"|cvr_sd_g=="uca_Medium Sand"|cvr_sd_g=="uca_NA")]

DF5[,.(table(finos_class))]
DF5[,finos_grad:=ifelse(finos_class=="sandy_010",paste(finos_class,Sd_cls1,sep="_"),finos_class)]
DF5[,.(table(finos_grad))]
DF5[,Final_finos_grad:=ifelse(Final_finos_class=="bare_sediment_sandy_010"|Final_finos_class=="uca_sandy_010",paste(Final_finos_class,Sd_cls1,sep="_"),Final_finos_class)]
DF5[,.(table(Final_finos_grad))]
DF5_4<-DF5[!(Final_finos_grad=="uca_sandy_010_Medium Sand")]
DF5_4[,.(table(Final_finos_grad))]

### Devide data for validation (30% for validation)
set.seed(200)
trainIndex_mg1a <- createDataPartition(DF5_4$Final_finos_grad, p = .7, 
                                      list = FALSE, 
                                      times = 1)
head(trainIndex_mg1a)

L0_train_mg1a<-DF5_4[trainIndex_mg1a]
#L0_train_mg1a[,table(cvr_sd_g)]

L0_val_mg1a<-DF5_4[-trainIndex_mg1a]
#L0_val_mg1a[,table(cvr_sd_g)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_mg1a<-merge(GT_c1,L0_train_mg1a,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_mg1a,col="red")
#str(GT_c_l0_t_mg1a@data)

GT_c_l0_v_mg1a<-merge(GT_c1,L0_val_mg1a,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_mg1a)
#str(GT_c_l0_v_mg1a@data)

### select variables
sat_seds<-stack("Data_out/Stack/sat_seds.tif")
names(sat_seds)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
                   "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
                   "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","MSAVI2","intensity","iv_multi","rededge_multi","rededge_sum",
                   "visible_multi")

#sat_seds_m1a_all<-sat_seds[[-c(9,14)]]
names(sat_seds_m1a_all)



### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_mg1a<-superClass(img=sat_seds_m1a_all,model="rf",trainData=GT_c_l0_t_mg1a,responseCol="Final_finos_grad",valData=GT_c_l0_v_mg1a,polygonBasedCV=F,predict=T,
                             predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_mg1a<-end-start

saveRSTBX(SC1_allclass_mg1a,"Data_out/models/SC1_allclass_mg1a",format="raster",overwrite=F)
SC1_allclass_mg1a<-readRSTBX("Data_out/models/SC1_allclass_mg1a.tif")

SC1_allclass_mg1a$model$finalModel$importance

plot(SC1_allclass_mg1a$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_mg1a$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_mg1a$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_mg1a$map,"Data_out/class_tif/SC1_allclass_mg1a.tif",overwrite=F)



####Now with selection of variables
sat_mg1a_sel<-subset(sat_m1a_all,c()) ## selected according to PCA and correlation matrix

### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_mg1a_sel<-superClass(img=sat_mg1a_sel,model="rf",trainData=GT_c_l0_t_mg1a,responseCol="Final_finos_grad",valData=GT_c_l0_v_mg1a,polygonBasedCV=F,predict=T,
                                 predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_mg1a_sel<-end-start

saveRSTBX(SC1_allclass_mg1a_sel,"Data_out/models/SC1_allclass_mg1a_sel",format="raster",overwrite=T)
SC1_allclass_mg1a_sel<-readRSTBX("Data_out/models/SC1_allclass_mg1a_sel.tif")

SC1_allclass_mg1a_sel$model$finalModel$importance

plot(SC1_allclass_mg1a_sel$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_mg1a_sel$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_mg1a_sel$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_mg1a_sel$map,"Data_out/class_tif/SC1_allclass_mg1a_sel.tif")




####Now with PCA of variables
PCA_tot<-readRSTBX("Data_out/PCA/PCA_tot.tif")
summary(PCA_tot$model)
PCA_tot_map<-PCA_tot$map
names(PCA_tot_map)
PCA_mg1_tot1<-PCA_tot_map[[1:7]] ## PCA selection of bands
PCA_mg1_tot2<-PCA_tot_map[[1:11]]

sat_seds<-stack("Data_out/Stack/sat_seds.tif")
names(sat_seds)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
                   "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
                   "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","MSAVI2","intensity","iv_multi","rededge_multi","rededge_sum",
                   "visible_multi")

beginCluster(7)
PCA_mg1_tot1_seds<-mask(PCA_mg1_tot1,sat_seds[[1]])
endCluster()
beep(3)
writeRaster(PCA_mg1_tot1_seds,"Data_out/PCA/PCA_mg1_tot1_seds.tif")
plot(PCA_mg1_tot1_seds[[1]],colNA=1)

beginCluster(7)
PCA_mg1_tot2_seds<-mask(PCA_mg1_tot2,sat_seds[[1]])
endCluster()
beep(3)
writeRaster(PCA_mg1_tot2_seds,"Data_out/PCA/PCA_mg1_tot2_seds.tif")
plot(PCA_mg1_tot2_seds[[1]],colNA=1)

### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_mg1a_PCA<-superClass(img=PCA_mg1_tot2_seds,model="rf",trainData=GT_c_l0_t_mg1a,responseCol="Final_finos_grad",valData=GT_c_l0_v_mg1a,polygonBasedCV=F,predict=T,
                                  predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_mg1a_PCA<-end-start

saveRSTBX(SC1_allclass_mg1a_PCA,"Data_out/models/SC1_allclass_mg1a_PCA",format="raster",overwrite=F)
SC1_allclass_mg1a_PCA<-readRSTBX("Data_out/models/SC1_allclass_mg1a_PCA.tif")

SC1_allclass_mg1a_PCA$model$finalModel$importance

plot(SC1_allclass_mg1a_PCA$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_mg1a_PCA$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_mg1a_PCA$map,d)
plot(d1,colNA=1,col=rainbow(15))

writeRaster(SC1_allclass_mg1a_PCA$map,"Data_out/class_tif/SC1_allclass_mg1a_PCA.tif")



############### Classify only UCA classes 

#DF5_1<-DF5[!(cvr_sd_g=="bare_sediment_NA"|cvr_sd_g=="uca_Medium Sand"|cvr_sd_g=="uca_NA")]

#DF5[,.(table(finos_class))]
#DF5[,finos_grad:=ifelse(finos_class=="sandy_010",paste(finos_class,Sd_cls1,sep="_"),finos_class)]
#DF5[,.(table(finos_grad))]
#DF5[,Final_finos_grad:=ifelse(Final_finos_class=="bare_sediment_sandy_010"|Final_finos_class=="uca_sandy_010",paste(Final_finos_class,Sd_cls1,sep="_"),Final_finos_class)]
#DF5[,.(table(Final_finos_grad))]
#DF5_4<-DF5[!(Final_finos_grad=="bare_sediment_sandy_010_NA"|Final_finos_grad=="uca_sandy_010_NA"|Final_finos_grad=="uca_sandy_010_Medium Sand")]
#DF5_4[,.(table(Final_finos_grad))]
DF5_4_uca<-DF5_4[uca=="uca"]
DF5_4_uca[,.(table(Final_finos_grad))]
DF5_4_uca1<-DF5_4_uca[!(Island=="Adonga")]

### Devide data for validation (30% for validation)
set.seed(200)
trainIndex_mg1uca <- createDataPartition(DF5_4_uca$Final_finos_grad, p = .7, 
                                       list = FALSE, 
                                       times = 1)
head(trainIndex_mg1uca)

L0_train_mg1uca<-DF5_4_uca[trainIndex_mg1uca]
#L0_train_mg1uca[,table(Final_finos_grad)]

L0_val_mg1uca<-DF5_4_uca[-trainIndex_mg1uca]
#L0_val_mg1uca[,table(Final_finos_grad)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_mg1uca<-merge(GT_c1,L0_train_mg1uca,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_mg1a,col="red")
#str(GT_c_l0_t_mg1a@data)

GT_c_l0_v_mg1uca<-merge(GT_c1,L0_val_mg1uca,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_mg1a)
#str(GT_c_l0_v_mg1a@data)


sat_uca<-stack("Data_out/Stack/sat_uca.tif")
names(sat_uca)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
                  "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
                  "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","MSAVI2","intensity","iv_multi","rededge_multi","rededge_sum",
                  "visible_multi")

sat_uca_all<-sat_uca[[-c(9,14)]]
names(sat_uca_all)

#### Select bands to use ####
#sat_uca1<-subset(sat_uca,c("S1_20200128_VH","S1_20200128_VV","B11_20200204","rededge_sum","intensity","dem_104_469","NDMI1","NDWI","mNDWI","MSAVI2","VH_VV"))

######## Random forest class #########
start<-Sys.time()

set.seed(12)
beginCluster(7)
SC1_mg1uca<-superClass(img=sat_uca_all,model="rf",trainData=GT_c_l0_t_mg1uca,responseCol="Final_finos_grad",valData=GT_c_l0_v_mg1uca,polygonBasedCV=F,predict=T,
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



####Now with PCA of variables
PCA_tot<-readRSTBX("Data_out/PCA/PCA_tot.tif")
summary(PCA_tot$model)
PCA_tot_map<-PCA_tot$map
names(PCA_tot_map)
PCA_mg1_tot1<-PCA_tot_map[[1:7]] ## PCA selection of bands
PCA_mg1_tot2<-PCA_tot_map[[1:11]]

sat_uca<-stack("Data_out/Stack/sat_uca.tif")
names(sat_uca)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
                  "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
                  "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","MSAVI2","intensity","iv_multi","rededge_multi","rededge_sum",
                  "visible_multi")

beginCluster(7)
PCA_mg1_tot1_uca<-mask(PCA_mg1_tot1,sat_uca[[1]])
endCluster()
beep(3)
writeRaster(PCA_mg1_tot1_uca,"Data_out/PCA/PCA_mg1_tot1_uca.tif")
plot(PCA_mg1_tot1_uca[[1]],colNA=1)

beginCluster(7)
PCA_mg1_tot2_uca<-mask(PCA_mg1_tot2,sat_uca[[1]])
endCluster()
beep(3)
writeRaster(PCA_mg1_tot2_uca,"Data_out/PCA/PCA_mg1_tot2_uca.tif")
plot(PCA_mg1_tot2_uca[[1]],colNA=1)


######## Random forest class #########
start<-Sys.time()

set.seed(12)
beginCluster(7)
SC1_mg1uca_PCA<-superClass(img=PCA_mg1_tot1_uca,model="rf",trainData=GT_c_l0_t_mg1uca,responseCol="Final_finos_grad",valData=GT_c_l0_v_mg1uca,polygonBasedCV=F,predict=T,
                       predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_mg1uca<-end-start

saveRSTBX(SC1_mg1uca_PCA,"Data_out/models/SC1_mg1uca_PCA",fomat="raster",overwrite=F)
SC1_mg1uca_PCA<-readRSTBX("Data_out/models/SC1_mg1uca_PCA")

SC1_mg1uca_PCA$model$finalModel$importance

SC1_mg1uca_PCA$classMapping
plot(SC1_mg1uca_PCA$map,colNA=1,col=c("cadetblue","lightgrey","cadetblue1"))
SC1_mg1uca_PCA_tif<-SC1_mg1uca_PCA$map
writeRaster(SC1_mg1uca_PCA_tif,"Data_out/models/SC1_mg1uca_PCA.tif")

xx<-drawExtent()
subs_t<-crop(SC1_mg1uca$map,xx)
plot(subs_t, colNA=1,col=c("cadetblue","lightgrey","cadetblue1"))





############### Classify only BARE SEDIMENT classes 

#DF5_1<-DF5[!(cvr_sd_g=="bare_sediment_NA"|cvr_sd_g=="uca_Medium Sand"|cvr_sd_g=="uca_NA")]

#DF5[,.(table(finos_class))]
#DF5[,finos_grad:=ifelse(finos_class=="sandy_010",paste(finos_class,Sd_cls1,sep="_"),finos_class)]
#DF5[,.(table(finos_grad))]
#DF5[,Final_finos_grad:=ifelse(Final_finos_class=="bare_sediment_sandy_010"|Final_finos_class=="uca_sandy_010",paste(Final_finos_class,Sd_cls1,sep="_"),Final_finos_class)]
#DF5[,.(table(Final_finos_grad))]
#DF5_4<-DF5[!(Final_finos_grad=="bare_sediment_sandy_010_NA"|Final_finos_grad=="uca_sandy_010_NA"|Final_finos_grad=="uca_sandy_010_Medium Sand")]
#DF5_4[,.(table(Final_finos_grad))]
DF5_4_bsed<-DF5_4[uca=="other"]
DF5_4_bsed[,.(table(Final_finos_grad))]
DF5_4_bsed1[!(Island=="Adonga")]

### Devide data for validation (30% for validation)
set.seed(200)
trainIndex_mg1bsed <- createDataPartition(DF5_4_bsed$Final_finos_grad, p = .7, 
                                         list = FALSE, 
                                         times = 1)
head(trainIndex_mg1bsed)

L0_train_mg1bsed<-DF5_4_bsed[trainIndex_mg1bsed]
#L0_train_mg1bsed[,table(Final_finos_grad)]

L0_val_mg1bsed<-DF5_4_bsed[-trainIndex_mg1bsed]
#L0_val_mg1bsed[,table(Final_finos_grad)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_mg1bsed<-merge(GT_c1,L0_train_mg1bsed,by="Point",all.x=F,all.y=T)

GT_c_l0_v_mg1bsed<-merge(GT_c1,L0_val_mg1bsed,by="Point",all.x=F,all.y=T)



sat_baresed<-stack("Data_out/Stack/sat_baresed.tif")
names(sat_baresed)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
                      "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
                      "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","MSAVI2","intensity","iv_multi","rededge_multi","rededge_sum",
                      "visible_multi")

sat_bsed_all<-sat_baresed[[-c(9,14)]]
names(sat_bsed_all)

#### Select bands to use ####
#sat_bsed1<-subset(sat_bsed,c("S1_20200128_VH","S1_20200128_VV","B11_20200204","rededge_sum","intensity","dem_104_469","NDMI1","NDWI","mNDWI","MSAVI2","VH_VV"))

######## Random forest class #########
start<-Sys.time()

set.seed(12)
beginCluster(7)
SC1_mg1bsed<-superClass(img=sat_bsed_all,model="rf",trainData=GT_c_l0_t_mg1bsed,responseCol="Final_finos_grad",valData=GT_c_l0_v_mg1bsed,polygonBasedCV=F,predict=T,
                       predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_mg1bsed<-end-start

saveRSTBX(SC1_mg1bsed,"Data_out/models/SC1_mg1bsed",fomat="raster",overwrite=T)
SC1_mg1bsed<-readRSTBX("Data_out/models/SC1_mg1bsed")

SC1_mg1bsed$model$finalModel$importance

SC1_mg1bsed$classMapping
plot(SC1_mg1bsed$map,colNA=1,col=c("cadetblue","lightgrey","cadetblue1"))
SC1_mg1bsed_tif<-SC1_mg1bsed$map
writeRaster(SC1_mg1bsed_tif,"Data_out/models/SC1_mg1bsed.tif")

xx<-drawExtent()
subs_t<-crop(SC1_mg1bsed$map,xx)
plot(subs_t, colNA=1,col=c("cadetblue","lightgrey","cadetblue1"))

































setkey(DF3,cvr_sd_g)
DF3["bare_sediment_NA"]

DF4<-DF3[!(cvr_sd_g=="bare_sediment_NA"|cvr_sd_g=="uca_NA"),] ##Removing data points for which there is no data on sediment grain size
DF4[,table(cvr_sd_g)]
str(DF4)

DF4[,cvr_sd_g:=as.character(cvr_sd_g)]
DF4[,table(cvr_sd_g)]
DF4[,table(WD)]

DF4[,all_g_WD:=paste(cvr_sd_g,WD,sep="_")]
DF4[,table(all_g_WD)]
DF4[all_g_WD=="shell_dry"|all_g_WD=="shell_wet",all_g_WD:="shell"]
DF4[all_g_WD=="rock_dry",all_g_WD:="rock"]

DF4[,Point:=as.character(Point)]
x<-DF4[,.(table(Point,all_g_WD))]
x1<-dcast.data.table(x,Point~all_g_WD,fun.aggregate = sum)
x1$tot<-apply(x1[,-1],1,sum)

DF4[,table(all_g_WD)]
DF5<-DF4[!all_g_WD=="uca_Medium Sand_dry"] ## remove class with only one data point
DF5[,table(all_g_WD)]

### Devide data for validation (30% for validation)
set.seed(200)
trainIndex_G <- createDataPartition(DF5$all_g_WD, p = .7, 
                                    list = FALSE, 
                                    times = 1)
head(trainIndex_G)

L0_train_G<-DF5[trainIndex_G]
L0_train_G[,table(all_g_WD)]

L0_val_G<-DF5[-trainIndex_G]
L0_val_G[,table(all_g_WD)]

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
SC1_allclass_G<-superClass(img=sat,model="rf",trainData=GT_c_l0_t_G,responseCol="all_g_WD",valData=GT_c_l0_v_G,polygonBasedCV=F,predict=T,
                         predType="raw",filename=NULL)
endCluster()
beep(3)
saveRSTBX(SC1_allclass_G,"Data_out/models/SC1_allclass_G",format="raster",overwrite=T)
SC1_allclass_G<-readRSTBX("Data_out/models/SC1_allclass_G.tif")
SC1_allclass_G$classMapping

plot(SC1_allclass_G$map, colNA=1, main="cover over wet")
d<-drawExtent()
d1<-crop(SC1_allclass_G$map,d)
plot(d1,colNA=1,col=rainbow(15))

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




