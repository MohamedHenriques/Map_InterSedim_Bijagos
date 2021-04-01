setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

packs<-c("randomForest","caret","sf","beepr","RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis","data.table","reshape2","sp","rgeos")
lapply(packs,require,character.only=T)

## Load sat img

sat<-stack("Data_out/Stack/Final_stack.tif") ##created in script GraVSSat_Preliminary
names(sat)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204","B08A_20200204",
              "B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","DEM","NDWI","mNDWI","NDMI","NDMI1")

##Load GT polygons
GT_c1<-readOGR("Data_out/Polygons/GT_c1.shp") ##created in script Data_cleanup_SUp_Class
#plot(GT_c1)
#unique(GT_c1@data$cvr_vr1)
DF2<-data.table(GT_c1@data)
str(DF2)



##############################################################################
################################################################################
######################### LEVEL 1 #############################################

####### Bare sediment ##########
bare_sediment_mask<-raster("Data_out/Habitat_classes/bare_sediment_mask.tif")
plot(bare_sediment_mask, col="grey")
plot(GT, col="red",add=T)

GT_c1_bs<-raster::intersect(GT_c1,bare_sediment_mask)

### Wet VS Dry #######
###create threshold with water cover

DF2[,WD:=ifelse(c_water<20,"dry","wet")] # this is the mark that provides the highest accuracy and kappa
DF2[,table(WD)]

##Split data in training + validation using caret balanced splitting

set.seed(10)
trainIndex <- createDataPartition(DF2$WD, p = .7, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

L0_train<-DF2[trainIndex]
L0_val<-DF2[-trainIndex]


###Introduce new columns on training and validation polygons
L0_train1<-L0_train[,.(sedmnt0,Point)]

GT_c_l0_t<-merge(GT_c1,L0_train1,by="Point",all.x=F,all.y=T)
plot(GT_c_l0_t,col="red")
str(GT_c_l0_t@data)

L0_val1<-L0_val[,.(sedmnt0,Point)]

GT_c_l0_v<-merge(GT_c1,L0_val1,by="Point",all.x=F,all.y=T)
plot(GT_c_l0_v)
str(GT_c_l0_v@data)

### Supervised class with rstoolbox and rf: shells
#mask_shells_others<-stack("Data_out/mask/mask_shells_others.tif")
#sat_sed<-mask(sat,mask_shells_others)
#writeRaster(sat_sed,"Data_out/Stack/sat_sed.tif",overwrite=T)
sat_sed<-stack("Data_out/Stack/sat_sed.tif") #made from the remaining area after excluding macroalgae, rock and shell areas


set.seed(11)
beginCluster(7)
SC1_sedmnt0<-superClass(img=sat_sed,model="rf",trainData=GT_c_l0_t,responseCol="sedmnt0.x",valData=GT_c_l0_v,polygonBasedCV=F,predict=T,
                        predType="raw",filename="Data_out/Class_map/SC1_sedmnt0.tif")
endCluster()
beep(3)

plot(SC1_sedmnt0$map, colNA=1,main="Bare Sed VS Uca VS flooded sed")
sedmnt0map<-SC1_sedmnt0$map
SC1_sedmnt0$classMapping

###Isolating bare sediment areas
bare_sediment_mask<-sedmnt0map==1
bare_sediment_mask[bare_sediment_mask==0]<-NA # turn wet area (coded zero) into NA
plot(bare_sediment_mask, colNA=1, main="bare sediment")
writeRaster(bare_sediment_mask,"Data_out/Habitat_classes/bare_sediment_mask.tif",overwrite=T)

area_bare_sediment<-sum(bare_sediment_mask[bare_sediment_mask==1])*res(bare_sediment_mask)[1]^2*10^-6 #calculate bare sediment area size in Km2
