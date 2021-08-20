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

packs<-c("MASS","geodist","caTools","colorspace","randomForest","caret","sf","beepr","RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis","data.table","reshape2")
npacks <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(npacks)) install.packages(npacks)
#install_github("vqv/ggbiplot")
lapply(packs,require,character.only=T)

## Load sat img
sat<-stack("Data_out/Stack/Final_stack1.grd") ##created in script Sat_image_stack.R
names(sat)


## Load data created in sctipt extract_table.R
m1<-fread("Data_out/db/DF_extract_20210722.csv")
str(m1)

##remove bad points in db
m2<-m1[!(is.na(cvr_vrA))]

## Sediment class as used by Belo in MSc Thesis
m2[,finos_class:=ifelse(mud<10,"sandy_010",ifelse(mud>=10&mud<=100,"mixed_1075",NA))]
m2[!(is.na(mud)),table(finos_class)]
m2[,Final_finos_class:=paste(cvr_vrA,finos_class,sep="_")][cvr_vrA=="macroalgae"|cvr_vrA=="rock"|cvr_vrA=="shell",Final_finos_class:=cvr_vrA]
m2[,.(table(Final_finos_class))]

###Median
m2[,summary(D50_um_)]
m2[,Median:=ifelse(D50_um_<=125,"less_125","more_125")]
m2[,table(Median)]
m2[,Final_median:=paste(cvr_vrA,Median,sep="_")][cvr_vrA=="macroalgae"|cvr_vrA=="rock"|cvr_vrA=="shell",Final_median:=cvr_vrA]
m2[,.(table(Final_median))]

m2[,cvr_vrA6:=as.character(cvr_vrA)][cvr_vrA6=="bare_sediment",cvr_vrA6:="sediments"][cvr_vrA6=="uca",cvr_vrA6:="sediments"]
m2[,.(table(cvr_vrA6))]

m2[,.(table(cvr_sd_g))]
m2[,Final_finos_grad:=ifelse(Final_finos_class=="bare_sediment_sandy_010"|Final_finos_class=="uca_sandy_010",paste(Final_finos_class,Sd_cls1,sep="_"),Final_finos_class)]
m2[,.(table(Final_finos_grad))]

str(m2)

## Include coordinates of the pixels (will be used for data partition with spatial independence)
m2_df<-as.data.frame(m2)
m2_1<-as.data.table(cbind(m2_df, xyFromCell(sat,m2_df[,1])))
data.frame(names(m2_1))

m3<-m2_1[,c(1:26,59,66,68,70,77,79,81:85)]
names(m3)


################################################################################
#################################################################################
################ Finos + gradistat #############################################

##clean and adjust habitat classes
m3[,.(table(Final_finos_grad))]
m3[Final_finos_grad=="uca_sandy_010_Medium Sand",Final_finos_grad:="uca_sandy_010_Fine Sand"] ## aggregate to fine sand because we only have 2 polygons
m3[Final_finos_grad=="bare_sediment_sandy_010_Medium Sand",Final_finos_grad:="bare_sediment_sandy_010_Fine Sand"] ## aggregate to fine sand because we only have 2 polygons
m3_2<-m3[!(Final_finos_grad=="bare_sediment_NA"|Final_finos_grad=="uca_NA")]
m3_2[,.(table(Final_finos_grad))]

data.frame(names(m3_2))
m3_2_1<-m3_2[,c(1:25,26,35:37)]
m3_fg<-na.omit(m3_2_1)
m3_fg[,.(table(Final_finos_grad))]
#write.csv(m3_fg,"Data_out/m3_fg.csv",row.names=F)

sum(table(unique(m3$Point)))
sum(table(unique(m3_2$Point)))
sum(table(unique(m3_fg$Point)))


###Split data 
library(geosphere)
library(rgeos)


##Load GT polygons with database with sed id
GT_c1<-readOGR("Data_out/Polygons/GT_c1.shp") ##created in script Data_cleanup_SUp_Class
#plot(GT_c1)
crs(GT_c1)
GT_c2<-GT_c1
GT_c2<-spTransform(GT_c1,"+proj=longlat +datum=WGS84")
plot(GT_c2)

##Extract coordinates of polygons
# xy<-as.data.table(fortify(GT_c1))
# xy[,c(3:6):=NULL]

##calculate distance between all polygons
xy<-as.data.table(centroid(GT_c2))
names(xy)<-c("lon","lat")
#xy[,Point:=GT_c2$Point]

dist1<-as.data.table(distm(xy))
names(dist1)<-as.character(GT_c1$Point)

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(dist1), method="complete")

# define the distance threshold, in this case 40 m
d=1000

# define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
xy[,clust:=cutree(hc, h=d)]
xy[,.(table(clust))]

xy[,Point:=GT_c1$Point]

GT_c3<-merge(GT_c2,xy, by="Point")
GT_c3$clust
plot(GT_c3)
writeOGR(GT_c3,"Data_out/Polygons",layer="GT_c3_1",driver = "ESRI Shapefile",overwrite_layer = F)

##merge with extract database
m3_fg_c<-merge(m3_fg,GT_c3,by="Point",all.x=T)
str(m3_fg_c)

m3_fg_c[,.(table())]

# ## calculate distance between all points
# d<geodist(x=m3[,34:35],paired=F,sequential=F,measure="geodesic")
# str(d)
# 
# colnames(d)<-m3[,cell]
# d1$cell<-m3[,cell]
# write.csv(d,"Data_out/db/pixel_distance_matrix.csv",header=T)
# 
# r<-m3_fg[Final_finos_grad=="rock"]
# sum(table(unique(r$Point)))
# sp<-r[,.N,by=Point]
# sp1<-sp[order(N)]
# 
# t<-matrix(nrow=(nrow(r)*.7),ncol=ncol(r))
# names(t)<-names(r)
# 
# v<-matrix(nrow=(nrow(r)*.3),ncol=ncol(r))
# names(v)<-names(r)
# 
# t[1,]<-r[sample(1:nrow(r),1,replace=F)]
# r<-r[!cell==t[1,cell],]
# 
# prov<-r[sample(1:nrow(r),1,replace=F)]
# r<-r[!cell==t[2,cell],]
# 
# if(prov[,cell])

##Dealing with spatial independence (but will create imbalance in the number of pixels between train and validation db)
set.seed(1)
trainIndex_fg<-sample.split(m3_fg$Final_finos_grad,SplitRatio=.70,group=m3_fg$Point)
table(m3_fg$Final_finos_grad,trainIndex_fg)
split(trainIndex_fg,m3_fg$Point)

# set.seed(1)
# trainIndex_fg1 <- createDataPartition(m3_fg$Final_finos_grad, p = .7,list = FALSE,times = 1,group=m3_fg$Point)

head(trainIndex_fg)

train_fg<-m3_fg[trainIndex_fg]
train_fg[,.(table(Final_finos_grad))]

val_fg<-m3_fg[!trainIndex_fg]
val_fg[,.(table(Final_finos_grad))]

##Select variables
names(train_fg)
var_fg0<-names(train_fg)[c(3,8,9,11:12,13,14,16,18,21,23)]
var_fg1<-names(train_fg)[c(1:6,8,9,11:12,13,14,16,18,19,21)]
var_fg2<-names(train_fg)[c(9,11:12,13,15:16,18,19,20,21,22:24)]
var_fg3<-names(train_fg)[c(1:3,4,11:12,18,19,21)]

##############################################################################
#################################################################################

## Using For loop to identify the right mtry for model
a=c()
#i=5
for (i in 2:(length(var_fg0)-3)) {
  set.seed(2)
  print(paste("mtry=",i,sep=" "))
  rf_fg0<-randomForest(train_fg[,..var_fg0],y=train_fg[,as.factor(Final_finos_grad)],ntree=1000,mtry=i,replace=T,importance=T,localImp=FALSE,do.trace=100)
  pred_fg0<-predict(rf_fg0, newdata=val_fg[,..var_fg0],type="class")
  a[i-1] = mean(pred_fg0 == val_fg$Final_finos_grad)
}
a
plot(2:(length(var_fg0)-3),a)
mtry_fg0<-which(a==max(a))+1

##############################################################################################
################################################################################################

##Train and fit random forest with selected mtry
set.seed(2)
rf_fg0<-randomForest(train_fg[,..var_fg0],y=train_fg[,as.factor(Final_finos_grad)],ntree=1000,mtry=mtry_fg0,replace=T,importance=T,localImp=FALSE,do.trace=100)

##Validate with validation data set and confusion matrix
val_fg$pred_fg0<-predict(rf_fg0, newdata=val_fg[,..var_fg0],type="class")
mean(val_fg$pred_fg0 == val_fg$Final_finos_grad)                    
#cm_val_fg0<-table(val_fg$pred_fg0,val_fg$Final_finos_grad)

confusionMatrix(as.factor(val_fg$pred_fg0),as.factor(val_fg$Final_finos_grad))

##Predict training data and check validation and confusion matrix
train_fg$pred_fg0<-predict(rf_fg0, newdata=train_fg[,..var_fg0],type="class")
mean(train_fg$pred_fg0 == train_fg$Final_finos_grad)
#cm_train_fg0<-table(train_fg[,pred_fg0],train_fg[,Final_finos_grad])

## Importance of variables
#### The below functions show the drop in mean accuracy for each of the variables
importance(rf_fg0)
varImpPlot(rf_fg0, main= "Variable impoortance for classification using both mud content and mean greain size")




### Predict for sat image and export tif
##PCA variable selection
beginCluster()
pred_img_fg0 <- clusterR(sat[[var_fg0]], raster::predict, args = list(model = rf_fg0))
endCluster()
beep(3)
writeRaster(pred_img_fg0,"Data_out/class_tif/pred_img_fg_sel0_pol.tif",overwrite=F)

plot(pred_img_fg0,colNA=1)

d<-drawExtent()
d1<-crop(pred_img_fg0,d)
plot(d1,colNA=1,col=rainbow(10))


###Variable selection 1

beginCluster()
pred_img_fg1 <- clusterR(sat[[var_fg1]], raster::predict, args = list(model = rf_fg1))
endCluster()
beep(3)
writeRaster(pred_img_fg1,"Data_out/class_tif/pred_img_fg_sel1.tif",overwrite=T)

plot(pred_img_fg1,colNA=1)
d<-drawExtent()
d1<-crop(pred_img_fg1,d)
plot(d1,colNA=1,col=rainbow(10))


###Variable selection 2

beginCluster()
pred_img_fg2 <- clusterR(sat[[var_fg2]], raster::predict, args = list(model = rf_fg2))
endCluster()
beep(3)
writeRaster(pred_img_fg2,"Data_out/class_tif/pred_img_fg_sel2.tif",overwrite=T)

plot(pred_img_fg2,colNA=1)
d<-drawExtent()
d1<-crop(pred_img_fg2,d)
plot(d1,colNA=1,col=rainbow(10))

###Variable selection 3
beginCluster()
pred_img_fg3 <- clusterR(sat[[var_fg3]], raster::predict, args = list(model = rf_fg3))
endCluster()
beep(3)
writeRaster(pred_img_fg3,"Data_out/class_tif/pred_img_fg_sel3.tif",overwrite=T)

plot(pred_img_fg3,colNA=1)
d<-drawExtent()
d1<-crop(pred_img_fg3,d)
plot(d1,colNA=1,col=rainbow(10))




##########################################################################################
########################################################################################
############## CLASSES GRADISTAT ##########################################################

###Split data 
m3[,.(table(cvr_sd_g))]
m3_2[,.(table(cvr_sd_g))]

m3[cvr_sd_g=="uca_Medium Sand",cvr_sd_g:="uca_Fine Sand"] ## aggregate to fine sand because we only have 2 polygons
#m3[cvr_sd_g=="bare_sediment_sandy_010_Medium Sand",cvr_sd_g:="bare_sediment_sandy_010_Fine Sand"] ## aggregate to fine sand because we only have 2 polygons
m3_3<-m3[!(cvr_sd_g=="bare_sediment_NA"|cvr_sd_g=="uca_NA")]
m3_3[,.(table(cvr_sd_g))]
data.frame(names(m3_3))
m3_3_1<-m3_3[,c(2:25,26,29)]
m3_g<-na.omit(m3_3_1)
m3_g[,.(table(cvr_sd_g))]

##Dealing with spatial independence (but will create imbalance in the number of pixels between train and validation db)
set.seed(1)
trainIndex_g<-sample.split(m3_g$cvr_sd_g,SplitRatio=.70,group=m3_g$Point)
table(m3_g$cvr_sd_g,trainIndex_g)
split(trainIndex_g,m3_g$Point)

# set.seed(1)
# trainIndex_g1 <- createDataPartition(m3_g$cvr_sd_g, p = .7,list = FALSE,times = 1,group=m3_g$Point)

head(trainIndex_g)

train_g<-m3_g[trainIndex_g]
train_g[,.(table(cvr_sd_g))]

val_g<-m3_g[!trainIndex_g]
val_g[,.(table(cvr_sd_g))]

##Select variables
names(train_g)
var_g0<-names(train_g)[c(3,8,9,11:12,13,14,16,18,21,23)]
var_g1<-names(train_g)[c(1:6,8,9,11:12,13,14,16,18,19,21)]
var_g2<-names(train_g)[c(9,11:12,13,15:16,18,19,20,21,22:24)]
var_g3<-names(train_g)[c(1:3,4,11:12,18,19,21)]

##############################################################################
#################################################################################

## Using For loop to identify the right mtry for model
a=c()
#i=5
for (i in 2:(length(var_g0)-3)) {
  set.seed(2)
  print(paste("mtry=",i,sep=" "))
  rf_g0<-randomForest(train_g[,..var_g0],y=train_g[,as.factor(cvr_sd_g)],ntree=1000,mtry=i,replace=T,importance=T,localImp=FALSE,do.trace=100)
  pred_g0<-predict(rf_g0, newdata=val_g[,..var_g0],type="class")
  a[i-1] = mean(pred_g0 == val_g$cvr_sd_g)
}
a
plot(2:(length(var_g0)-3),a)
mtry_g0<-which(a==max(a))+1

##############################################################################################
################################################################################################

##Train and fit random forest with selected mtry
set.seed(2)
rf_g0<-randomForest(train_g[,..var_g0],y=train_g[,as.factor(cvr_sd_g)],ntree=1000,mtry=mtry_g0,replace=T,importance=T,localImp=F,do.trace=100)

##Validate with validation data set and confusion matrix
val_g$pred_g0<-predict(rf_g0, newdata=val_g[,..var_g0],type="class")
mean(val_g$pred_g0 == val_g$cvr_sd_g)         
#cm_val_g0<-table(val_g$pred_g0,val_g$cvr_sd_g)

confusionMatrix(as.factor(val_g$pred_g0),as.factor(val_g$cvr_sd_g))

##Predict training data and check validation and confusion matrix
train_g$pred_g0<-predict(rf_g0, newdata=train_g[,..var_g0],type="class")
mean(train_g$pred_g0 == train_g$cvr_sd_g)
#cm_train_g0<-table(train_g[,pred_g0],train_g[,cvr_sd_g])

## Importance of variables
#### The below functions show the drop in mean accuracy for each of the variables
importance(rf_g0)
varImpPlot(rf_g0)




### Predict for sat image and export tif
##PCA variable selection
beginCluster()
pred_img_g0 <- clusterR(sat[[var_g0]], raster::predict, args = list(model = rf_g0))
endCluster()
beep(3)
writeRaster(pred_img_g0,"Data_out/class_tif/pred_img_g_sel0.tif",overwrite=T)

plot(pred_img_g0,colNA=1)

d<-drawExtent()
d1<-crop(pred_img_g0,d)
plot(d1,colNA=1,col=rainbow(10))


###Variable selection 1

beginCluster()
pred_img_g1 <- clusterR(sat[[var_g1]], raster::predict, args = list(model = rf_g1))
endCluster()
beep(3)
writeRaster(pred_img_g1,"Data_out/class_tif/pred_img_g_sel1.tif",overwrite=T)

plot(pred_img_g1,colNA=1)
d<-drawExtent()
d1<-crop(pred_img_g1,d)
plot(d1,colNA=1,col=rainbow(10))


###Variable selection 2

beginCluster()
pred_img_g2 <- clusterR(sat[[var_g2]], raster::predict, args = list(model = rf_g2))
endCluster()
beep(3)
writeRaster(pred_img_g2,"Data_out/class_tif/pred_img_g_sel2.tif",overwrite=T)

plot(pred_img_g1,colNA=1)
d<-drawExtent()
d1<-crop(pred_img_g1,d)
plot(d1,colNA=1,col=rainbow(10))


###Variable selection 3

beginCluster()
pred_img_g3 <- clusterR(sat[[var_g3]], raster::predict, args = list(model = rf_g3))
endCluster()
beep(3)
writeRaster(pred_img_g3,"Data_out/class_tif/pred_img_g_sel3.tif",overwrite=T)

plot(pred_img_g1,colNA=1)
d<-drawExtent()
d1<-crop(pred_img_g1,d)
plot(d1,colNA=1,col=rainbow(10))





###############################################################################################################
#############################################################################################################
#################### Hierarchical ##########################################


################ Level 0 ##########################
###Split data 
# m3[,.(table(cvr_vrA))]
# m3_l0<-m3[,c(1:24,27)]
# m3_l0[,.(table(cvr_vrA))]
# table(is.na(m3_l0))
# m3_l0_1<-na.omit(m3_l0)
# m3_l0_1[,.(table(cvr_vrA))]
# 
# set.seed(1)
# trainIndex_l0 <- createDataPartition(m3_l0_1$cvr_vrA, p = .7, 
#                                      list = FALSE, 
#                                      times = 1)
# head(trainIndex_l0)
# 
# train_l0<-m3_l0_1[trainIndex_l0]
# train_l0[,.(table(cvr_vrA))]
# 
# val_l0<-m3_l0_1[-trainIndex_l0]
# val_l0[,.(table(cvr_vrA))]


# m3[,.(table(cvr_sd_g))]
# 
# m3[,Final_finos_grad:=ifelse(Final_finos_class=="bare_sediment_sandy_010"|Final_finos_class=="uca_sandy_010",paste(Final_finos_class,Sd_cls1,sep="_"),Final_finos_class)]
# m3[,.(table(Final_finos_grad))]
# m3[Final_finos_grad=="uca_sandy_010_Medium Sand",Final_finos_grad:="uca_sandy_010_Fine Sand"] ## aggregate to fine sand because we only have 2 polygons
# #m3[Final_finos_grad=="bare_sediment_sandy_010_Medium Sand",Final_finos_grad:="bare_sediment_sandy_010_Fine Sand"] ## aggregate to fine sand because we only have 2 polygons

m3[,cvr_vrA6:=as.character(cvr_vrA)][cvr_vrA6=="bare_sediment",cvr_vrA6:="sediments"][cvr_vrA6=="uca",cvr_vrA6:="sediments"]
m3[,.(table(cvr_vrA6))]

m3_h<-na.omit(m3[,c(2:26,28,34)])

#m3_2<-m3[!(Final_finos_grad=="bare_sediment_NA"|Final_finos_grad=="uca_NA")]
#m3_2[,.(table(Final_finos_grad))]

#m3_2_1<-m3_2[,c(1:24,27,33)]
#m3_fg<-na.omit(m3_2_1)
#m3_fg[,.(table(Final_finos_grad))]

##Dealing with spatial independence (but will create imbalance in the number of pixels between train and validation db)
set.seed(1)
trainIndex_h<-sample.split(m3_h$cvr_vrA,SplitRatio=.70,group=m3_h$Point)
table(m3_h$cvr_vrA,trainIndex_h)
split(trainIndex_h,m3_h$Point)


# set.seed(1)
# trainIndex_h <- createDataPartition(m3$Final_finos_grad, p = .7, 
#                                      list = FALSE, 
#                                      times = 1)
# head(trainIndex_h)

train_h<-m3_h[trainIndex_h]
train_h[,.(table(cvr_vrA))]

val_h<-m3_h[!trainIndex_h]
val_h[,.(table(cvr_vrA))]


##Select variables
names(train_h)
var_l00<-names(train_h)[c(1:3,4,9,13,18,11,14,15,16,19,23,24)]
var_l000<-names(train_h)[c(3,8,9,11:12,13,14,16,18,21,23)]
var_l01<-names(train_h)[c(1:6,8,9,11:12,13,14,16,18,19,21)]
var_l02<-names(train_h)[c(9,11:12,13,15:16,18,19,20,21,22:24)]
var_l03<-names(train_h)[c(1:3,4,11:12,18,19,21)]

##############################################################################
#################################################################################

## Using For loop to identify the right mtry for model
a=c()
#i=5
for (i in 2:(length(var_l00)-3)) {
  set.seed(2)
  print(paste("mtry=",i,sep=" "))
  rf_l00<-randomForest(train_h[,..var_l00],y=train_h[,as.factor(cvr_vrA)],ntree=1000,mtry=i,replace=T,importance=T,localImp=F,do.trace=100)
  pred_l00<-predict(rf_l00, newdata=val_h[,..var_l00],type="class")
  a[i-1] = mean(pred_l00 == val_h$cvr_vrA)
}
a
plot(2:(length(var_l00)-3),a)
mtry_l00<-which(a==max(a))+1

##############################################################################################
################################################################################################

##Train and fit random forest with selected mtry
set.seed(2)
rf_l00<-randomForest(train_h[,..var_l00],y=train_h[,as.factor(cvr_vrA)],ntree=1000,mtry=mtry_l00,replace=T,importance=T,localImp=F,do.trace=100)

##Validate with validation data set and confusion matrix
val_h$pred_l00<-predict(rf_l00, newdata=val_h[,..var_l00],type="class")
mean(val_h$pred_l00 == val_h$cvr_vrA)                    
#cm_val_l00<-table(val_h$pred_l00,val_h$cvr_vrA)

confusionMatrix(as.factor(val_h$pred_l00),as.factor(val_h$cvr_vrA))

##Predict training data and check validation and confusion matrix
train_h$pred_l00<-predict(rf_l00, newdata=train_h[,..var_l00],type="class")
mean(train_h$pred_l00 == train_h$cvr_vrA)
#cm_train_l00<-table(train_h[,pred_l00],train_h[,cvr_vrA])

## Importance of variables
#### The below functions show the drop in mean accuracy for each of the variables
importance(rf_l00)
varImpPlot(rf_l00, main="Variable importance for classification without grain size measures")




### Predict for sat image and export tif
##PCA variable selection
beginCluster()
pred_img_l00 <- clusterR(sat[[var_l00]], raster::predict, args = list(model = rf_l00))
endCluster()
beep(3)
writeRaster(pred_img_l00,"Data_out/class_tif/pred_img_l0_sel00.tif",overwrite=T)

plot(pred_img_l000,colNA=1)

d<-drawExtent()
d1<-crop(pred_img_l000,d)
plot(d1,colNA=1,col=rainbow(4))

###Isolating macroalgae
macro_mask<-pred_img_l000==1
macro_mask[macro_mask==0]<-NA # turn remaining area (coded zero) into NA
#plot(macro_mask, colNA=1)
writeRaster(macro_mask,"Data_out/Habitat_classes/Level0/macro_mask_rf.tif", overwrite=T)

###Isolating rock
rock_mask<-pred_img_l000==2
rock_mask[rock_mask==0]<-NA # turn remaining area (coded zero) into NA
#plot(rock_mask, colNA=1)
writeRaster(rock_mask,"Data_out/Habitat_classes/Level0/rock_mask_rf.tif", overwrite=T)

###Isolating shell
shell_mask<-pred_img_l000==4
shell_mask[shell_mask==0]<-NA # turn remaining area (coded zero) into NA
#plot(shell_mask, colNA=1)
writeRaster(shell_mask,"Data_out/Habitat_classes/Level0/shell_mask_rf.tif", overwrite=T)

###Isolating exposed sediment areas
seds_mask<-pred_img_l000==3
seds_mask[seds_mask==0]<-NA # turn remaining area (coded zero) into NA
#plot(bseds_mask, colNA=1)
writeRaster(seds_mask,"Data_out/Habitat_classes/Level0/seds_mask_rf.tif", overwrite=T)


### mask sat for exposed sediment areas
beginCluster()
sat_seds<-mask(sat,seds_mask)
writeRaster(sat_seds,"Data_out/Stack/sat_seds_rf.grd",format="raster",overwrite=F)
endCluster()
beep(3)
sat_seds<-stack("Data_out/Stack/sat_seds_rf.grd")
names(sat_seds)
#plot(sat_bs[[1]])


#################################################################################################################
############################### Level 1 class - uca VS Bare sediments #######################################################

train_h[,.(table(uca))]
train_h2<-na.omit(train_h[,c(1:24,30)])
train_h2[,.(table(uca))]

val_h[,.(table(uca))]
val_h2<-na.omit(val_h[,c(1:24,30)])
val_h2[,.(table(uca))]

##Select variables
names(train_h2)
var_l10<-names(train_h2)[c(1:3,4,9,13,18,11,14,15,16,19,23,24)] ##PCA geral 
var_l100<-names(train_h2)[c(1:3,7:8,9,11:12,13,14:15,18,21)] ##PCA uva vs BS
var_l11<-names(train_h2)[c(1:6,8,9,11:12,13,14,16,18,19,21)]
var_l12<-names(train_h2)[c(9,11:12,13,15:16,18,19,20,21,22:24)]
var_l13<-names(train_h2)[c(1:3,4,11:12,18,19,21)]


## Using For loop to identify the right mtry for model
a=c()
#i=5
for (i in 2:(length(var_l100)-3)) {
  set.seed(2)
  print(paste("mtry=",i,sep=" "))
  rf_l100<-randomForest(train_h2[,..var_l100],y=train_h2[,as.factor(uca)],ntree=100,mtry=i,replace=T,importance=T,localImp=F,do.trace=10)
  pred_l100<-predict(rf_l100, newdata=val_h2[,..var_l100],type="class")
  a[i-1] = mean(pred_l100 == val_h2$uca)
}
a
plot(2:(length(var_l100)-3),a)
mtry_l100<-which(a==max(a))+1

##############################################################################################
################################################################################################

##Train and fit random forest with selected mtry
set.seed(2)
rf_l100<-randomForest(train_h2[,..var_l100],y=train_h2[,as.factor(uca)],ntree=100,mtry=mtry_l100,replace=T,importance=T,localImp=F,do.trace=10)

##Validate with validation data set and confusion matrix
val_h2$pred_l100<-predict(rf_l100, newdata=val_h2[,..var_l100],type="class")
mean(val_h2$pred_l100 == val_h2$uca)                    
#cm_val_l100<-table(val_h2$pred_l100,val_h2$uca)

confusionMatrix(as.factor(val_h2$pred_l100),as.factor(val_h2$uca))

##Predict training data and check validation and confusion matrix
train_h2$pred_l100<-predict(rf_l100, newdata=train_h2[,..var_l100],type="class")
mean(train_h2$pred_l100 == train_h2$uca)
#cm_train_l100<-table(train_h2[,pred_l100],train_h2[,uca])

## Importance of variables
#### The below functions show the drop in mean accuracy for each of the variables
importance(rf_l100)
varImpPlot(rf_l100)

## Predict for sat image
###Variable selection PCA seds
beginCluster()
pred_img_l100 <- clusterR(sat_seds[[var_l100]], raster::predict, args = list(model = rf_l100))
endCluster()
beep(3)
writeRaster(pred_img_l100,"Data_out/class_tif/pred_img_l1_sel00.tif",overwrite=T)

plot(pred_img_l100,colNA=1)
d<-drawExtent()
d1<-crop(pred_img_l100,d)
plot(d1,colNA=1,col=rainbow(2))

###Isolating bare sediment areas
bs_mask<-pred_img_l100==1
bs_mask[bs_mask==0]<-NA # turn remaining area (coded zero) into NA
#plot(bs_mask, colNA=1)
writeRaster(bs_mask,"Data_out/Habitat_classes/Level1/bs_mask_rf.tif", overwrite=T)

###Isolating uca areas
uca_mask<-pred_img_l100==2
uca_mask[uca_mask==0]<-NA # turn remaining area (coded zero) into NA
#plot(uca_mask, colNA=1)
writeRaster(uca_mask,"Data_out/Habitat_classes/Level1/uca_mask_rf.tif", overwrite=T)




### mask sat for uca areas
beginCluster()
sat_uca<-mask(sat,uca_mask)
writeRaster(sat_uca,"Data_out/Stack/sat_uca_rf.grd",format="raster",overwrite=F)
endCluster()
beep(3)
sat_uca<-stack("Data_out/Stack/sat_uca_rf.grd")
names(sat_uca)
#plot(sat_uca[[1]])




#########################################################################################
############################## bare sediment ###########################################

train_h[,.(table(Final_finos_grad))]
train_h3_bs<-na.omit(train_h[cvr_vrA=="bare_sediment",c(1:24,33)][!Final_finos_grad=="bare_sediment_NA"])
train_h3_bs[,.(table(Final_finos_grad))]

val_h[,.(table(Final_finos_grad))]
val_h3_bs<-na.omit(val_h[cvr_vrA=="bare_sediment",c(1:24,33)][!Final_finos_grad=="bare_sediment_NA"])
val_h3_bs[,.(table(Final_finos_grad))]

### mask sat for bare sediment areas
beginCluster()
sat_bs<-mask(sat,bs_mask)
writeRaster(sat_bs,"Data_out/Stack/sat_bs_rf.grd",format="raster",overwrite=F)
endCluster()
beep(3)
sat_bs<-stack("Data_out/Stack/sat_bs_rf.grd")
names(sat_bs)
#plot(sat_bs[[1]])


##Select variables
names(train_h3_bs)
var_l20bs<-names(train_h3_bs)[c(1:3,4,9,13,18,11,14,15,16,19,23,24)] ##PCA geral 
var_l200bs<-names(train_h3_bs)[c(1:3,4:5,8,21,18,13,11:12,10,14:15,16)] ##PCA for bare sediemnt
var_l21bs<-names(train_h3_bs)[c(1:6,8,9,11:12,13,14,16,18,19,21)]
var_l22bs<-names(train_h3_bs)[c(9,11:12,13,15:16,18,19,20,21,22:24)]
var_l23bs<-names(train_h3_bs)[c(1:3,4,11:12,18,19,21)]



## Using For loop to identify the right mtry for model
a=c()
#i=5
for (i in 2:(length(var_l21bs)-3)) {
  set.seed(2)
  print(paste("mtry=",i,sep=" "))
  rf_l21bs<-randomForest(train_h3_bs[,..var_l21bs],y=train_h3_bs[,as.factor(Final_finos_grad)],ntree=1000,mtry=i,replace=T,importance=T,localImp=F,do.trace=100)
  pred_l21bs<-predict(rf_l21bs, newdata=val_h3_bs[,..var_l21bs],type="class")
  a[i-1] = mean(pred_l21bs == val_h3_bs$Final_finos_grad)
}
a
plot(2:(length(var_l21bs)-3),a)
mtry_l21bs<-which(a==max(a))+1

##############################################################################################
################################################################################################

##Train and fit random forest with selected mtry
set.seed(2)
rf_l21bs<-randomForest(train_h3_bs[,..var_l21bs],y=train_h3_bs[,as.factor(Final_finos_grad)],ntree=1000,mtry=mtry_l21bs,replace=T,importance=T,localImp=FALSE,do.trace=100)

##Validate with validation data set and confusion matrix
val_h3_bs$pred_l21bs<-predict(rf_l21bs, newdata=val_h3_bs[,..var_l21bs],type="class")
mean(val_h3_bs$pred_l21bs == val_h3_bs$Final_finos_grad)                    
#cm_val_l21bs<-table(val_h3_bs$pred_l21bs,val_h3_bs$Final_finos_grad)

confusionMatrix(as.factor(val_h3_bs$pred_l21bs),as.factor(val_h3_bs$Final_finos_grad))

##Predict training data and check validation and confusion matrix
train_h3_bs$pred_l21bs<-predict(rf_l21bs, newdata=train_h3_bs[,..var_l21bs],type="class")
mean(train_h3_bs$pred_l21bs == train_h3_bs$Final_finos_grad)
#cm_train_l21bs<-table(train_h3_bs[,pred_l21bs],train_h3_bs[,Final_finos_grad])

## Importance of variables
#### The below functions show the drop in mean accuracy for each of the variables
importance(rf_l21bs)
varImpPlot(rf_l21bs)


###Variable selection PCA bs
beginCluster()
pred_img_l21bs <- clusterR(sat_bs[[var_l21bs]], raster::predict, args = list(model = rf_l21bs))
endCluster()
beep(3)
writeRaster(pred_img_l21bs,"Data_out/class_tif/pred_img_l2bs_sel1.tif",overwrite=T)

plot(pred_img_l21bs,colNA=1)
d<-drawExtent()
d1<-crop(pred_img_l21bs,d)
plot(d1,colNA=1,col=rainbow(4))






##################################################################
################### Uca sediments ################################

train_h[,.(table(Final_finos_grad))]
train_h3_uca<-na.omit(train_h[cvr_vrA=="uca",c(1:24,33)][!Final_finos_grad=="uca_NA"])
train_h3_uca[,.(table(Final_finos_grad))]

val_h[,.(table(Final_finos_grad))]
val_h3_uca<-na.omit(val_h[cvr_vrA=="uca",c(1:24,33)][!Final_finos_grad=="uca_NA"])
val_h3_uca[,.(table(Final_finos_grad))]

### mask sat for bare sediment areas
beginCluster()
sat_uca<-mask(sat,uca_mask)
writeRaster(sat_uca,"Data_out/Stack/sat_uca_rf.grd",format="raster",overwrite=F)
endCluster()
beep(3)
sat_uca<-stack("Data_out/Stack/sat_uca_rf.grd")
names(sat_uca)
#plot(sat_uca[[1]])


##Select variables
names(train_h3_uca)
var_l20u<-names(train_h3_uca)[c(1:3,4,9,13,18,11,14,15,16,19,23,24)] ##PCA geral 
var_l200u<-names(train_h3_uca)[c(1:3,9,11:13,18,15:16,23,8,21,14)] ##PCA for uca sediemnts
var_l21u<-names(train_h3_uca)[c(1:6,8,9,11:12,13,14,16,18,19,21)]
var_l22u<-names(train_h3_uca)[c(9,11:12,13,15:16,18,19,20,21,22:24)]
var_l23u<-names(train_h3_uca)[c(1:3,4,11:12,18,19,21)]



## Using For loop to identify the right mtry for model
a=c()
#i=5
for (i in 2:(length(var_l21u)-3)) {
  set.seed(2)
  print(paste("mtry=",i,sep=" "))
  rf_l21u<-randomForest(train_h3_uca[,..var_l21u],y=train_h3_uca[,as.factor(Final_finos_grad)],ntree=1000,mtry=i,replace=T,importance=T,localImp=F,do.trace=100)
  pred_l21u<-predict(rf_l21u, newdata=val_h3_uca[,..var_l21u],type="class")
  a[i-1] = mean(pred_l21u == val_h3_uca$Final_finos_grad)
}
a
plot(2:(length(var_l21u)-3),a)
mtry_l21u<-which(a==max(a))+1

##############################################################################################
################################################################################################

##Train and fit random forest with selected mtry
set.seed(2)
rf_l21u<-randomForest(train_h3_uca[,..var_l21u],y=train_h3_uca[,as.factor(Final_finos_grad)],ntree=1000,mtry=mtry_l21u,replace=T,importance=T,localImp=FALSE,do.trace=100)

##Validate with validation data set and confusion matrix
val_h3_uca$pred_l21u<-predict(rf_l21u, newdata=val_h3_uca[,..var_l21u],type="class")
mean(val_h3_uca$pred_l21u == val_h3_uca$Final_finos_grad)                    
#cm_val_l21u<-table(val_h3_uca$pred_l21u,val_h3_uca$Final_finos_grad)

confusionMatrix(as.factor(val_h3_uca$pred_l21u),as.factor(val_h3_uca$Final_finos_grad))

##Predict training data and check validation and confusion matrix
train_h3_uca$pred_l21u<-predict(rf_l21u, newdata=train_h3_uca[,..var_l21u],type="class")
mean(train_h3_uca$pred_l21u == train_h3_uca$Final_finos_grad)
#cm_train_l21u<-table(train_h3_uca[,pred_l21u],train_h3_uca[,Final_finos_grad])

## Importance of variables
#### The below functions show the drop in mean accuracy for each of the variables
importance(rf_l21u)
varImpPlot(rf_l21u)


###Variable selection PCA uca
beginCluster()
pred_img_l21u <- clusterR(sat_uca[[var_l21u]], raster::predict, args = list(model = rf_l21u))
endCluster()
beep(3)
writeRaster(pred_img_l21u,"Data_out/class_tif/pred_img_l2uca_sel1.tif",overwrite=T)

plot(pred_img_l21u,colNA=1)
d<-drawExtent()
d1<-crop(pred_img_l21u,d)
plot(d1,colNA=1,col=rainbow(3))



############### Final validation ####################################
#####################################################################

macro_mask1<-macro_mask
macro_mask1[is.na(macro_mask1)]<-0

rock_mask1<-rock_mask
rock_mask1[rock_mask1==1]<-3
rock_mask1[is.na(rock_mask1)]<-0

shell_mask1<-shell_mask
shell_mask1[shell_mask1==1]<-5
shell_mask1[is.na(shell_mask1)]<-0

bs_seds1<-pred_img_l21bs
bs_seds1[bs_seds1==1]<-7 #bare_sediment_mixed_1075
bs_seds1[bs_seds1==2]<-9 #bare_sediment_sandy_010_Fine Sand
bs_seds1[bs_seds1==3]<-11 #bare_sediment_sandy_010_Medium Sand
bs_seds1[bs_seds1==4]<-13 #bare_sediment_sandy_010_Very Fine Sand
bs_seds1[is.na(bs_seds1)]<-0

uca_seds1<-pred_img_l21u
uca_seds1[uca_seds1==1]<-15 #uca_mixed_1075 
uca_seds1[uca_seds1==2]<-17 #uca_sandy_010_Fine Sand
uca_seds1[uca_seds1==3]<-19 #uca_sandy_010_Very Fine Sand
uca_seds1[is.na(uca_seds1)]<-0


#### Gathering classes of hierarchical classification
beginCluster()
hierarch<-overlay(macro_mask1,rock_mask1,shell_mask1,bs_seds1,uca_seds1,fun=sum)
hierarch[hierarch==0]<-NA
endCluster()
beep(3)

p<-c("green","red","blue","lightgrey")


#plot(hierarch)
d<-drawExtent()
d1<-crop(hierarch,d)
plot(d1,colNA=1,col=rainbow(10))

writeRaster(hierarch,"Data_out/class_tif/hierarch_rf.tif",overwrite=F)




################################################################################
#################################################################################
################ Finos #############################################

m3[,.(table(Final_finos_class))]
m3_1<-m3[!(Final_finos_class=="bare_sediment_NA"|Final_finos_class=="uca_NA")]
m3_1[,.(table(Final_finos_class))]
data.frame(names(m3_1))

m3_1_1<-m3_1[,c(2:25,26,32)]
m3_f<-na.omit(m3_1_1)
m3_f[,.(table(Final_finos_class))]

sum(table(unique(m3_f$Point)))
sum(table(unique(m3_1$Point)))
sum(table(unique(m3_1_1$Point)))


###Split data 

# ## calculate distance between all points
# d<geodist(x=m3[,34:35],paired=F,sequential=F,measure="geodesic")
# str(d)
# 
# colnames(d)<-m3[,cell]
# d1$cell<-m3[,cell]
# write.csv(d,"Data_out/db/pixel_distance_matrix.csv",header=T)
# 
# r<-m3_fg[Final_finos_grad=="rock"]
# sum(table(unique(r$Point)))
# sp<-r[,.N,by=Point]
# sp1<-sp[order(N)]
# 
# t<-matrix(nrow=(nrow(r)*.7),ncol=ncol(r))
# names(t)<-names(r)
# 
# v<-matrix(nrow=(nrow(r)*.3),ncol=ncol(r))
# names(v)<-names(r)
# 
# t[1,]<-r[sample(1:nrow(r),1,replace=F)]
# r<-r[!cell==t[1,cell],]
# 
# prov<-r[sample(1:nrow(r),1,replace=F)]
# r<-r[!cell==t[2,cell],]
# 
# if(prov[,cell])

##Dealing with spatial independence (but will create imbalance in the number of pixels between train and validation db)
set.seed(1)
trainIndex_f<-sample.split(m3_f$Final_finos_class,SplitRatio=.7,group=m3_f$Point)
table(m3_f$Final_finos_class,trainIndex_f)
split(trainIndex_f,m3_f$Point)

# set.seed(1)
# trainIndex_f1 <- createDataPartition(m3_f$Final_finos_class, p = .7,list = FALSE,times = 1,group=m3_f$Point)

# head(trainIndex_f)

train_f<-m3_f[trainIndex_f]
train_f[,.(table(Final_finos_class))]

val_f<-m3_f[!trainIndex_f]
val_f[,.(table(Final_finos_class))]

##Select variables
names(train_f)
var_f0<-names(train_f)[c(3,8,9,11:12,13,14,16,18,21,23)]
var_f1<-names(train_f)[c(1:6,8,9,11:12,13,14,16,18,19,21)]
var_f2<-names(train_f)[c(9,11:12,13,15:16,18,19,20,21,22:24)]
var_f3<-names(train_f)[c(1:3,4,11:12,18,19,21)]

##############################################################################
#################################################################################

## Using For loop to identify the right mtry for model
a=c()
#i=5
for (i in 2:(length(var_f0)-3)) {
  set.seed(2)
  print(paste("mtry=",i,sep=" "))
  rf_f0<-randomForest(train_f[,..var_f0],y=train_f[,as.factor(Final_finos_class)],ntree=1000,mtry=i,replace=T,importance=T,localImp=FALSE,do.trace=100)
  pred_f0<-predict(rf_f0, newdata=val_f[,..var_f0],type="class")
  a[i-1] = mean(pred_f0 == val_f$Final_finos_class)
}
a
plot(2:(length(var_f0)-3),a)
mtry_f0<-which(a==max(a))+1

##############################################################################################
################################################################################################

##Train and fit random forest with selected mtry
set.seed(2)
rf_f0<-randomForest(train_f[,..var_f0],y=train_f[,as.factor(Final_finos_class)],ntree=1000,mtry=mtry_f0,replace=T,importance=T,localImp=FALSE,do.trace=100)

##Validate with validation data set and confusion matrix
val_f$pred_f0<-predict(rf_f0, newdata=val_f[,..var_f0],type="class")
mean(val_f$pred_f0 == val_f$Final_finos_class)                    
#cm_val_f0<-table(val_f$pred_f0,val_f$Final_finos_class)

confusionMatrix(as.factor(val_f$pred_f0),as.factor(val_f$Final_finos_class))

##Predict training data and check validation and confusion matrix
train_f$pred_f0<-predict(rf_f0, newdata=train_f[,..var_f0],type="class")
mean(train_f$pred_f0 == train_f$Final_finos_class)
#cm_train_f0<-table(train_f[,pred_f0],train_f[,Final_finos_class])

## Importance of variables
#### The below functions show the drop in mean accuracy for each of the variables
importance(rf_f0)
varImpPlot(rf_f0,main = "Variable importance for classification based on mud content")




### Predict for sat image and export tif
##PCA variable selection
beginCluster()
pred_img_f0 <- clusterR(sat[[var_f0]], raster::predict, args = list(model = rf_f0))
endCluster()
beep(3)
writeRaster(pred_img_f0,"Data_out/class_tif/pred_img_f_sel0_pol.tif",overwrite=T)

plot(pred_img_f0,colNA=1)

d<-drawExtent()
d1<-crop(pred_img_f0,d)
plot(d1,colNA=1,col=rainbow(10))


###Variable selection 1

beginCluster()
pred_img_f1 <- clusterR(sat[[var_f1]], raster::predict, args = list(model = rf_f1))
endCluster()
beep(3)
writeRaster(pred_img_f1,"Data_out/class_tif/pred_img_f_sel1.tif",overwrite=T)

plot(pred_img_f1,colNA=1)
d<-drawExtent()
d1<-crop(pred_img_f1,d)
plot(d1,colNA=1,col=rainbow(10))


###Variable selection 2

beginCluster()
pred_img_f2 <- clusterR(sat[[var_f2]], raster::predict, args = list(model = rf_f2))
endCluster()
beep(3)
writeRaster(pred_img_f2,"Data_out/class_tif/pred_img_f_sel2.tif",overwrite=T)

plot(pred_img_f2,colNA=1)
d<-drawExtent()
d1<-crop(pred_img_f2,d)
plot(d1,colNA=1,col=rainbow(10))

###Variable selection 3
beginCluster()
pred_img_f3 <- clusterR(sat[[var_f3]], raster::predict, args = list(model = rf_f3))
endCluster()
beep(3)
writeRaster(pred_img_f3,"Data_out/class_tif/pred_img_f_sel3.tif",overwrite=T)

plot(pred_img_f3,colNA=1)
d<-drawExtent()
d1<-crop(pred_img_f3,d)
plot(d1,colNA=1,col=rainbow(10))

