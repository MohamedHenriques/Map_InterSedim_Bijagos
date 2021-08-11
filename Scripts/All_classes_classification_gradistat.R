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
sat<-stack("Data_out/Stack/Final_stack1.grd") ##created in script Sat_image_stack.R
names(sat)

#ggRGB(sat[[1:3]],3,2,1, stretch = "lin")


##Load GT polygons
GT_c1<-readOGR("Data_out/Polygons/GT_c1.shp") ##created in script Data_cleanup_SUp_Class
DF2<-data.table(GT_c1@data)
str(DF2)
#write.table(DF2,"Data_out/db/GT_c1.csv",row.names=F,sep=";")
#################################################################
##################################################################

#################### classification using all target habitat classes ################################

##Split data in training + validation using caret balanced splitting: Use this for final validation
DF3<-data.table(DF2)
DF3[,.(table(cvr_sd_g))]

## Sediment classes as defined by EU (2019)
DF3[,Grain_EU:=ifelse(mud<10,"sand_010",ifelse(mud>=10&mud<25,"muddy_sand_1025",ifelse(mud>=25&mud<60,"mixed_2560",ifelse(mud>=60&mud<=100,"muddy_60100",NA))))][,Final_grain_EU:=ifelse(cvr_vrA=="bare_sediment"|cvr_vrA=="uca",paste(cvr_vrA,Grain_EU,sep="_"),ifelse(cvr_vrA=="macroalgae","macroalgae",ifelse(cvr_vrA=="rock","rock",ifelse(cvr_vrA=="shell","shell",NA))))]
DF3[,Final_grain_EU1:=Final_grain_EU][Final_grain_EU=="bare_sediment_muddy_sand_1025",Final_grain_EU1:="bare_sediment_mixed_2560"][Final_grain_EU=="uca_muddy_sand_1025",Final_grain_EU1:="uca_mixed_2560"][Final_grain_EU1=="bare_sediment_mixed_2560",Final_grain_EU1:="bare_sediment_mixed_1060"][Final_grain_EU1=="uca_mixed_2560",Final_grain_EU1:="uca_mixed_1060"]
DF3[,table(Final_grain_EU1)]
DF3[,table(Final_grain_EU)]

DF3[,Grain_EU1:=ifelse(mud<10,"sand_010",ifelse(mud>=10&mud<25,"muddy_sand_1025",ifelse(mud>=25&mud<=100,"muddy_25100",NA)))][,Final_grain_EU2:=ifelse(cvr_vrA=="bare_sediment"|cvr_vrA=="uca",paste(cvr_vrA,Grain_EU1,sep="_"),ifelse(cvr_vrA=="macroalgae","macroalgae",ifelse(cvr_vrA=="rock","rock",ifelse(cvr_vrA=="shell","shell",NA))))]
DF3[,.(table(Final_grain_EU2))]

## Sediment class as used by Belo in MSc Thesis
DF3[,finos_class:=ifelse(mud<10,"sandy_010",ifelse(mud>=10&mud<=100,"mixed_1075",NA))]
DF3[!(is.na(mud)),table(finos_class)]
DF3[,Final_finos_class:=paste(cvr_vrA,finos_class,sep="_")][cvr_vrA=="macroalgae"|cvr_vrA=="rock"|cvr_vrA=="shell",Final_finos_class:=cvr_vrA]
DF3[,.(table(Final_finos_class))]

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

###Median
DF3[,summary(D50_um_)]
DF3[,Median:=ifelse(D50_um_<=125,"less_125","more_125")]
DF3[,table(Median)]
DF3[,Final_median:=paste(cvr_vrA,Median,sep="_")][cvr_vrA=="macroalgae"|cvr_vrA=="rock"|cvr_vrA=="shell",Final_median:=cvr_vrA]
DF3[,.(table(Final_median))]

###Median1
DF3[,summary(D50_um_)]
DF3[,Median1:=ifelse(D50_um_<150,"less_150","more_150")]
DF3[,table(Median1)]
DF3[,Final_median1:=paste(cvr_vrA,Median1,sep="_")][cvr_vrA=="macroalgae"|cvr_vrA=="rock"|cvr_vrA=="shell",Final_median1:=cvr_vrA]
DF3[,.(table(Final_median1))]

##calculate area of polygons to use in createdatapartition
DF3[,polAreas:= sapply(slot(GT_c1, "polygons"), slot, "area")]
DF3[,summary(polAreas)]
0-2000,2500-5000,5000-18000,18000-
  
### Create size classes for polygon based on quantiles

cuts<-quantile(DF3[,polAreas], c(.75, .875, 1)) 

DF3[polAreas>cuts[2],.(table(Final_finos_class))]
DF3[polAreas>cuts[1]&polAreas<=cuts[2],.(table(Final_finos_class))]
DF3[polAreas<=cuts[1],.(table(Final_finos_class))]

DF3[,size_class:=ifelse(polAreas<=cuts[1],"A",
                        ifelse(polAreas>cuts[1]&polAreas<=cuts[2],"B",
                               ifelse(polAreas>cuts[2],"C",NA)))]

DF3[,.(table(size_class))]

# ggplot(DF3,aes(x=polAreas,fill=Final_finos_class))+
#   geom_histogram(binwidth = 250)
# 
# hist(DF3[,polAreas],breaks=500,col=1)
# abline(v=cuts,col="red",lwd=2)
# 
# hist(log(DF3[,polAreas]),breaks=500,col=1,xaxt="n")
# axis(1,at=seq(2,12,1),labels=round(exp(seq(2,12,1)),0))

DF3[is.na(cvr_vrA)]
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

### Divide data for validation (30% for validation)
DF4[,table(Final_finos_class)]
DF4[,.(table(size_class))]
DF4_1<-DF4[!(Final_finos_class=="bare_sediment_NA"|Final_finos_class=="uca_NA")]

DF4_1[,.(table(size_class))]
DF4_1[,.(table(Final_finos_class))]

DF4_1[,Final_finos_size:=paste(size_class,Final_finos_class,sep="_")]
DF4_1[,.(table(Final_finos_size))]

# DF4_1[,Final_finos_class1:=ifelse(Final_finos_class=="bare_sediment_muddy_10100"|Final_finos_class=="bare_sediment_sandy_010","bare_sediment",Final_finos_class)]
# DF4_1[,.(table(Final_finos_class1))]


set.seed(200)
trainIndex_m1 <- createDataPartition(DF4_1$Final_finos_size, p = .85, 
                                    list = FALSE, 
                                    times = 1)
head(trainIndex_m1)

L0_train_m1<-DF4_1[trainIndex_m1]
L0_train_m1[,table(Final_finos_class)]
L0_train_m1[,table(Final_finos_size)]

L0_val_m1<-DF4_1[-trainIndex_m1]
L0_val_m1[,table(Final_finos_class)]
L0_val_m1[,table(Final_finos_size)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_m1<-merge(GT_c1,L0_train_m1,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_m1,col="red")
#str(GT_c_l0_t_m1@data)
writeOGR(GT_c_l0_t_m1,"Data_out/Polygons",layer="GT_c_l0_t_m1",driver = "ESRI Shapefile",overwrite_layer = T)

GT_c_l0_v_m1<-merge(GT_c1,L0_val_m1,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_m1)
#str(GT_c_l0_v_m1@data)
writeOGR(GT_c_l0_v_m1,"Data_out/Polygons",layer="GT_c_l0_v_m1",driver = "ESRI Shapefile",overwrite_layer = T)

### Introduce new data into original polygon to try split validation data on the function itself
#GT_c1_class<-merge(GT_c1,DF4_1,by="Point",all.x=F,all.y=T)

### select variables
#sat_m1_all<-sat[[-c(9,14)]]
#names(sat_m1_all)


### selection of bands to use
sat0<-sat[[c(3,8,21,9,13,18,11:12,14,16,23)]]
names(sat0)

sat1<-sat[[-c(16:17,20,22:24)]]
names(sat1)

sat2<-sat[[-c(1:8,14,17,19)]]
names(sat2)

sat3<-subset(sat,c("B02_20200204","B03_20200204","B04_20200204","B08A_20200204","B11_20200204","S1_20200128_VH","S1_20200128_VV","Final_DEM_nodelay","MSAVI2","mNDWI","NDWI","rededge_mean","RVI","NDVI"))
names(sat3)

sat4<-subset(sat,c("B11_20200204","S1_20200128_VH","S1_20200128_VV","Final_DEM_nodelay","MSAVI2","NDMI","NDWI","intensity","rededge_mean","iv_div","RVI","NDVI"))
names(sat4)


### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster()
SC1_allclass_m1<-superClass(img=sat0,model="rf",trainData=GT_c_l0_t_m1,responseCol="Final_finos_class",valData=GT_c_l0_v_m1,trainPartition=NULL,polygonBasedCV=T,predict=T,
                           predType="raw",filename=NULL,tuneLength = 10,verbose = T,nSamples = 10000)
endCluster()
beep(3)
end<-Sys.time()
dif_m1<-end-start

saveRSTBX(SC1_allclass_m1,"Data_out/models/SC1_allclass_m1_85",format="raster",overwrite=T)
SC1_allclass_m1<-readRSTBX("Data_out/models/SC1_allclass_m1.tif")

SC1_allclass_m1$model$finalModel$importance

plot(SC1_allclass_m1$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_m1$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_m1$map,d)
plot(d1,colNA=1,col=rainbow(7))

writeRaster(SC1_allclass_m1$map,"Data_out/class_tif/SC1_allclass_m1_85.tif",overwrite=F)



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

table(GT_c1_class$Final_finos_class)
GT_c1_class_seds<-GT_c_l0_t_m1[!(GT_c_l0_t_m1$Final_finos_class=="macroalgae"|GT_c_l0_t_m1$Final_finos_class=="shell"|GT_c_l0_t_m1$Final_finos_class=="rock"),]
table(GT_c1_class_seds$Final_finos_class)
### select variables

sat_seds<-stack("Data_out/Stack/sat_seds.grd")
sat_seds_val<-stack("Data_out/Stack/sat_seds_val.grd")
sat1_seds<-sat_seds[[-c(16:17,20,22:24)]]
names(sat1_seds)

### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_m1a<-superClass(img=sat0_seds_val,model="rf",trainData=GT_c_l0_t_m1[GT_c_l0_t_m1$cvr_vrA.y=="uca"|GT_c_l0_t_m1$cvr_vrA.y=="bare_sediment",],responseCol="Final_finos_class",valData=GT_c_l0_v_m1[GT_c_l0_v_m1$cvr_vrA.y=="uca"|GT_c_l0_v_m1$cvr_vrA.y=="bare_sediment",],trainPartition=NULL,polygonBasedCV=T,predict=F,
                            predType="raw",filename=NULL,tuneLength = 10,verbose = T)
endCluster()
beep(3)
end<-Sys.time()
dif_m1<-end-start

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




################### GRADISTAT ##########################################################



############################################################################################################
#############################################################################################################
### cvr_


### Classify all classes at the same time

### Devide data for validation (30% for validation)
#DF4_1<-DF4[!(Final_finos_class=="bare_sediment_NA"|Final_finos_class=="uca_NA")]
#DF4_2<-DF4_1[!(Final_grain_EU1=="uca_mud_90100")]
DF4[,.(table(cvr_sd_g))]
DF4_3<-DF4[!(cvr_sd_g=="bare_sediment_NA"|cvr_sd_g=="uca_NA"|cvr_sd_g=="uca_Medium Sand")][,cvr_sd_g:=as.character(cvr_sd_g)]
DF4_3[,.(table(cvr_sd_g))]

set.seed(200)
trainIndex_g1 <- createDataPartition(DF4_3$cvr_sd_g, p = .8, 
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

### selection of bands to use

sat1<-sat[[-c(16:17,20,22:24)]]
names(sat1)

sat2<-sat[[-c(1:8,14,17,19)]]
names(sat2)

sat3<-subset(sat,c("B02_20200204","B03_20200204","B04_20200204","B08A_20200204","B11_20200204","S1_20200128_VH","S1_20200128_VV","Final_DEM_nodelay","MSAVI2","mNDWI","NDWI","rededge_multi","RVI","NDVI"))
names(sat3)

sat4<-subset(sat,c("B11_20200204","S1_20200128_VH","S1_20200128_VV","Final_DEM_nodelay","MSAVI2","NDMI","NDWI","intensity","rededge_multi","iv_multi","RVI","NDVI"))
names(sat4)

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


### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_g1<-superClass(img=sat0,model="rf",trainData=GT_c_l0_t_g1,responseCol="cvr_sd_g.y",valData=GT_c_l0_v_g1,polygonBasedCV=T,predict=T,
                            predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif_g1<-end-start

saveRSTBX(SC1_allclass_g1,"Data_out/models/SC1_allclass_g1",format="raster",overwrite=T)
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
trainIndex_g1a <- createDataPartition(DF5_1$cvr_sd_g, p = .7, 
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
SC1_allclass_g1a<-superClass(img=sat1,model="rf",trainData=GT_c_l0_t_g1a,responseCol="cvr_sd_g.y",valData=GT_c_l0_v_g1a,polygonBasedCV=F,predict=T,
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
DF4_1<-DF4[!(Final_finos_class=="bare_sediment_NA"|Final_finos_class=="uca_NA")]
DF4_1[,.(table(finos_class))]
DF4_1[,.(table(Final_finos_class))]
DF4_1[,finos_grad:=ifelse(finos_class=="sandy_010",paste(finos_class,Sd_cls1,sep="_"),finos_class)]
DF4_1[,.(table(finos_grad))]
DF4_1[,Final_finos_grad:=ifelse(Final_finos_class=="bare_sediment_sandy_010"|Final_finos_class=="uca_sandy_010",paste(Final_finos_class,Sd_cls1,sep="_"),Final_finos_class)]
DF4_1[,.(table(Final_finos_grad))]
DF4_1[Final_finos_grad=="uca_sandy_010_Medium Sand",Final_finos_grad:="uca_sandy_010_Fine Sand"] ## aggregate to fine sand because we only have 2 polygons
DF4_1[Final_finos_grad=="bare_sediment_sandy_010_Medium Sand",Final_finos_grad:="bare_sediment_sandy_010_Fine Sand"] ## aggregate to fine sand because we only have 2 polygons

#DF4_2<-DF4_1[!(Final_finos_grad=="uca_sandy_010_Medium Sand")] ## Because we only have 2 polygons for now
DF4_1[,.(table(Final_finos_grad))]

# DF4_1[,cvr_vrA6:=as.character(cvr_vrA)][cvr_vrA6=="bare_sediment",cvr_vrA6:="sediments"][cvr_vrA6=="uca",cvr_vrA6:="sediments"]
# DF4_1[,.(table(cvr_vrA6))]

DF4_1[,Final_fg_size:=paste(size_class,Final_finos_grad,sep="_")]
DF4_1[,.(table(Final_fg_size))]


set.seed(200)
trainIndex_mg1 <- createDataPartition(DF4_1$Final_fg_size, p = .85, 
                                     list = FALSE, 
                                     times = 1)
head(trainIndex_mg1)

L0_train_mg1<-DF4_1[trainIndex_mg1]
L0_train_mg1[,table(Final_finos_grad)]

L0_val_mg1<-DF4_1[-trainIndex_mg1]
L0_val_mg1[,table(Final_finos_grad)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_mg1<-merge(GT_c1,L0_train_mg1,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_mg1,col="red")
#str(GT_c_l0_t_mg1@data)

GT_c_l0_v_mg1<-merge(GT_c1,L0_val_mg1,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_mg1)
#str(GT_c_l0_v_mg1@data)

GT_class1<-merge(GT_c1,DF4_1,by="Point",all.x=F,all.y=T)

### select variables
#sat_m1_all<-sat[[-c(9,14)]]
#names(sat_m1_all)
### selection of bands to use
sat0<-sat[[c(3,8,21,9,13,18,11:12,14,16,23)]]
names(sat0)

sat1<-sat[[-c(16:17,20,22:24)]]
names(sat1)

sat1_1<-sat1[[-c(7:10,14:15)]]
names(sat1_1)

sat2<-sat[[-c(1:8,14,17,19)]]
names(sat2)

sat3<-subset(sat,c("B02_20200204","B03_20200204","B04_20200204","B08A_20200204","B11_20200204","S1_20200128_VH","S1_20200128_VV","Final_DEM_nodelay","MSAVI2","mNDWI","NDWI","rededge_mean","RVI","NDVI"))
names(sat3)

sat4<-subset(sat,c("B11_20200204","S1_20200128_VH","S1_20200128_VV","Final_DEM_nodelay","MSAVI2","NDMI","NDWI","intensity","rededge_mean","iv_div","RVI","NDVI"))
names(sat4)

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


### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(21)
beginCluster(7)
SC1_allclass_mg1<-superClass(img=sat1,model="rf",trainData=GT_c_l0_t_mg1,responseCol="Final_finos_grad",valData=GT_c_l0_v_mg1,trainPartition = NULL,polygonBasedCV=F,predict=F,
                            predType="raw",filename=NULL,verbose=T,tuneLength = 10)
endCluster()
beep(3)
end<-Sys.time()
dif_mg1<-end-start

saveRSTBX(SC1_allclass_mg1,"Data_out/models/SC1_allclass_mg1",format="raster",overwrite=T)
SC1_allclass_mg1<-readRSTBX("Data_out/models/SC1_allclass_mg1.tifS")

SC1_allclass_mg1$model$finalModel$importance

plot(SC1_allclass_mg1$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_mg1$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_mg1$map,d)
plot(d1,colNA=1,col=rainbow(7))

writeRaster(SC1_allclass_mg1$map,"Data_out/class_tif/SC1_allclass_mg1.tif",overwrite=T)




################################################################################
##################################################################################
#######ONLY SEDIMENT NOW #################

table(GT_class1$Final_finos_grad)
GT_class1_seds<-GT_class1[!(GT_class1$Final_finos_grad=="macroalgae"|GT_class1$Final_finos_grad=="shell"|GT_class1$Final_finos_grad=="rock"),]
table(GT_class1_seds$Final_finos_grad)

sat_seds<-stack("Data_out/Stack/sat_seds.grd")
sat1_seds<-sat_seds[[-c(16:17,20,22:24)]]
names(sat1_seds)


### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(21)
beginCluster(7)
SC1_allclass_mg1a<-superClass(img=sat1_seds,model="rf",trainData=GT_class1_seds,responseCol="Final_finos_grad",valData=NULL,trainPartition = .85,polygonBasedCV=F,predict=F,
                             predType="raw",filename=NULL,verbose=T,tuneLength = 10)
endCluster()
beep(3)
end<-Sys.time()
dif_mg1a<-end-start


###########################
################################# MEDIAN 125 ########################################

### Classify all classes at the same time

### Devide data for validation (30% for validation)
#DF4_1<-DF4[!(Final_median=="bare_sediment_NA"|Final_median=="uca_NA")]
DF4_1[,.(table(Final_median))]

DF4_1[,Final_med1_size:=paste(size_class,Final_median,sep="_")]
DF4_1[,.(table(Final_med1_size))]

set.seed(200)
trainIndex_m1 <- createDataPartition(DF4_1$Final_med1_size, p = .85, 
                                     list = FALSE, 
                                     times = 1)
head(trainIndex_m1)

L0_train_m1<-DF4_1[trainIndex_m1]
L0_train_m1[,table(Final_median)]

L0_val_m1<-DF4_1[-trainIndex_m1]
L0_val_m1[,table(Final_median)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_m1med<-merge(GT_c1,L0_train_m1,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_m1,col="red")
#str(GT_c_l0_t_m1@data)

GT_c_l0_v_m1med<-merge(GT_c1,L0_val_m1,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_m1)
#str(GT_c_l0_v_m1@data)

### select variables
#sat_m1_all<-sat[[-c(9,14)]]
#names(sat_m1_all)


### selection of bands to use
sat0<-sat[[c(3,8,21,9,13,18,11:12,14,16,23)]]
names(sat0)

sat1<-sat[[-c(16:17,20,22:24)]]
names(sat1)

sat2<-sat[[-c(1:8,14,17,19)]]
names(sat2)

sat3<-subset(sat,c("B02_20200204","B03_20200204","B04_20200204","B08A_20200204","B11_20200204","S1_20200128_VH","S1_20200128_VV","Final_DEM_nodelay","MSAVI2","mNDWI","NDWI","rededge_multi","RVI","NDVI"))
names(sat3)

sat4<-subset(sat,c("B11_20200204","S1_20200128_VH","S1_20200128_VV","Final_DEM_nodelay","MSAVI2","NDMI","NDWI","intensity","rededge_multi","iv_multi","RVI","NDVI"))
names(sat4)


### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_m1med<-superClass(img=sat1,model="rf",trainData=GT_c_l0_t_m1med,responseCol="Final_median",valData=GT_c_l0_v_m1med,polygonBasedCV=F,predict=F,
                            predType="raw",filename=NULL,tuneLength = 10,verbose = T)
endCluster()
beep(3)
end<-Sys.time()
dif_m1med<-end-start

saveRSTBX(SC1_allclass_m1,"Data_out/models/SC1_allclass_m1",format="raster",overwrite=T)
SC1_allclass_m1<-readRSTBX("Data_out/models/SC1_allclass_m1.tif")

SC1_allclass_m1$model$finalModel$importance

plot(SC1_allclass_m1$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_m1$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_m1$map,d)
plot(d1,colNA=1,col=rainbow(7))

writeRaster(SC1_allclass_m1$map,"Data_out/class_tif/SC1_allclass_m1.tif",overwrite=F)





###########################
################################# MEDIAN 150 ########################################

### Classify all classes at the same time

### Devide data for validation (30% for validation)
DF4_1[,.(table(Final_median1))]

set.seed(200)
trainIndex_m1 <- createDataPartition(DF4_1$Final_median1, p = .85, 
                                     list = FALSE, 
                                     times = 1)
head(trainIndex_m1)

L0_train_m1<-DF4_1[trainIndex_m1]
L0_train_m1[,table(Final_median1)]

L0_val_m1<-DF4_1[-trainIndex_m1]
L0_val_m1[,table(Final_median1)]

###Introduce new columns on training and validation polygons
GT_c_l0_t_m1med<-merge(GT_c1,L0_train_m1,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_m1,col="red")
#str(GT_c_l0_t_m1@data)

GT_c_l0_v_m1med<-merge(GT_c1,L0_val_m1,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_m1)
#str(GT_c_l0_v_m1@data)

### select variables
#sat_m1_all<-sat[[-c(9,14)]]
#names(sat_m1_all)


### selection of bands to use
sat0<-sat[[c(3,8,21,9,13,18,11:12,14,16,23)]]
names(sat0)

sat1<-sat[[-c(16:17,20,22:24)]]
names(sat1)

sat2<-sat[[-c(1:8,14,17,19)]]
names(sat2)

sat3<-subset(sat,c("B02_20200204","B03_20200204","B04_20200204","B08A_20200204","B11_20200204","S1_20200128_VH","S1_20200128_VV","Final_DEM_nodelay","MSAVI2","mNDWI","NDWI","rededge_multi","RVI","NDVI"))
names(sat3)

sat4<-subset(sat,c("B11_20200204","S1_20200128_VH","S1_20200128_VV","Final_DEM_nodelay","MSAVI2","NDMI","NDWI","intensity","rededge_multi","iv_multi","RVI","NDVI"))
names(sat4)


### Supervised class with rstoolbox and rf
start<-Sys.time()
set.seed(20)
beginCluster(7)
SC1_allclass_m1med<-superClass(img=sat,model="rf",trainData=GT_c_l0_t_m1med,responseCol="Final_median1",valData=GT_c_l0_v_m1med,polygonBasedCV=F,predict=F,
                               predType="raw",filename=NULL,tuneLength = 10,verbose = T)
endCluster()
beep(3)
end<-Sys.time()
dif_m1med<-end-start

saveRSTBX(SC1_allclass_m1,"Data_out/models/SC1_allclass_m1",format="raster",overwrite=T)
SC1_allclass_m1<-readRSTBX("Data_out/models/SC1_allclass_m1.tif")

SC1_allclass_m1$model$finalModel$importance

plot(SC1_allclass_m1$map, colNA=1, main="Finos class: muddy=>=10%; sandy<10%")

SC1_allclass_m1$classMapping
d<-drawExtent()
d1<-crop(SC1_allclass_m1$map,d)
plot(d1,colNA=1,col=rainbow(7))

writeRaster(SC1_allclass_m1$map,"Data_out/class_tif/SC1_allclass_m1.tif",overwrite=F)




