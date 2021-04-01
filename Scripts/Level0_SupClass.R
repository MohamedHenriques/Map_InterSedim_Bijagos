setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

packs<-c("randomForest","caret","sf","beepr","RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis","data.table","reshape2")
lapply(packs,require,character.only=T)

## Load sat img

sat<-stack("Data_out/Stack/Final_stack.tif") ##created in script GraVSSat_Preliminary
names(sat)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204","B08A_20200204",
              "B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","DEM","NDWI","mNDWI","NDMI","NDMI1")

##Load wet and dry masks (produced in script SupClass_WetVSDry)
dry_mask<-raster("Data_out/mask/dry_mask.tif")
plot(dry_mask,colNA=1)

wet_mask<-stack("Data_out/mask/wet_mask.tif")
plot(wet_mask,colNA=1)

##Load GT polygons
GT_c1<-readOGR("Data_out/Polygons/GT_c1.shp") ##created in script Data_cleanup_SUp_Class
#plot(GT_c1)
#unique(GT_c1@data$cvr_vr1)
DF2<-data.table(GT_c1@data)
str(DF2)
########################################

####Level 0 classification

## mask out wet areas first
beginCluster(7)
sat_dry<-mask(sat,dry_mask)
plot(sat_dry, colNA=1,main="sat dry")
endCluster()

##mask out dry areas now
beginCluster(7)
sat_wet<-mask(sat,wet_mask)
plot(sat_wet, colNA=1,main="sat wet")
endCluster()


##Split data in training + validation using caret balanced splitting

set.seed(10)
trainIndex <- createDataPartition(DF2$cvr_vr1, p = .7, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

L0_train<-DF2[trainIndex]
L0_val<-DF2[-trainIndex]


###Introduce new columns on training and validation polygons
L0_train1<-L0_train[,.(covr_vr,cvr_vr1,Point)]

GT_c_l0_t<-merge(GT_c1,L0_train1,by="Point",all.x=F,all.y=T)
plot(GT_c_l0_t,col="red")
str(GT_c_l0_t@data)

L0_val1<-L0_val[,.(covr_vr,cvr_vr1,Point)]

GT_c_l0_v<-merge(GT_c1,L0_val1,by="Point",all.x=F,all.y=T)
plot(GT_c_l0_v)
str(GT_c_l0_v@data)

### Supervised class with rstoolbox and rf: all area
set.seed(11)
beginCluster(7)
SC1<-superClass(img=sat,model="rf",trainData=GT_c_l0_t,responseCol="cvr_vr1.x",valData=GT_c_l0_v,polygonBasedCV=F,predict=T,
                predType="raw",filename="Data_out/Class_map/SC1.tif")
endCluster()
beep(3)

xx<-drawExtent()
adonga_t<-crop(SC1$map,xx)
plot(adonga_t, colNA=1)

### Supervised class with rstoolbox and rf: dry area
set.seed(12)
beginCluster(7)
SC1_dry<-superClass(img=sat_dry,model="rf",trainData=GT_c_l0_t,responseCol="cvr_vr1.x",valData=GT_c_l0_v,polygonBasedCV=F,predict=T,
                predType="raw",filename="Data_out/Class_map/SC1_dry.tif")
endCluster()
beep(3)
plot(SC1_dry$map, colNA=1, main="cover over dry")
SC1_dry$classMapping

xx<-drawExtent()
adonga_dry<-crop(SC1_dry$map,xx)
plot(adonga, colNA=1)

### Supervised class with rstoolbox and rf: dry area
set.seed(13)
beginCluster(7)
SC1_wet<-superClass(img=sat_wet,model="rf",trainData=GT_c_l0_t,responseCol="cvr_vr1.x",valData=GT_c_l0_v,polygonBasedCV=F,predict=T,
                    predType="raw",filename="Data_out/Class_map/SC1_wet.tif")
endCluster()
beep(3)

plot(SC1_wet$map, colNA=1, main="cover over wet")





################################################################ 
###individual classification schemes


#################### macroalgae #################################
##Split data in training + validation using caret balanced splitting

set.seed(10)
trainIndex <- createDataPartition(DF2$macro, p = .7, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

L0_train<-DF2[trainIndex]
L0_val<-DF2[-trainIndex]


###Introduce new columns on training and validation polygons
L0_train1<-L0_train[,.(macro,Point)]

GT_c_l0_t<-merge(GT_c1,L0_train1,by="Point",all.x=F,all.y=T)
plot(GT_c_l0_t,col="red")
str(GT_c_l0_t@data)

L0_val1<-L0_val[,.(macro,Point)]

GT_c_l0_v<-merge(GT_c1,L0_val1,by="Point",all.x=F,all.y=T)
plot(GT_c_l0_v)
str(GT_c_l0_v@data)

### Supervised class with rstoolbox and rf: macroalgae
#sat_macro<-subset(sat,subset=c("B03_20200204","B04_20200204","B08_20200204","B08A_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV"))

set.seed(11)
beginCluster(7)
SC1_macro<-superClass(img=sat,model="rf",trainData=GT_c_l0_t,responseCol="macro.x",valData=GT_c_l0_v,polygonBasedCV=F,predict=T,
                      predType="raw",filename="Data_out/Class_map/SC1_macro.tif")
endCluster()
beep(3)

plot(SC1_macro$map, colNA=1,main="Macroalgae VS rest")
macromap<-SC1_macro$map
SC1_macro$classMapping

###Isolating macroalgae area
macro_mask<-macromap==1
macro_mask[macro_mask==0]<-NA # turn wet area (coded zero) into NA
plot(macro_mask, colNA=1)
writeRaster(macro_mask,"Data_out/Habitat_classes/Level0/macro_mask.tif", overwrite=F)

mask_macro_others<-macromap==2
mask_macro_others[mask_macro_others==0]<-NA # turn wet area (coded zero) into NA
plot(mask_macro_others, colNA=1)
writeRaster(mask_macro_others,"Data_out/mask/mask_macro_others.tif", overwrite=F)

area_macro<-sum(macro_mask[macro_mask==1])*res(macro_mask)[1]^2*10^-6 #calculate dry area size in Km2




####################ROCKS #################################
##Split data in training + validation using caret balanced splitting

set.seed(10)
trainIndex <- createDataPartition(DF2$rocks, p = .7, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

L0_train<-DF2[trainIndex]
L0_val<-DF2[-trainIndex]


###Introduce new columns on training and validation polygons
L0_train1<-L0_train[,.(rocks,Point)]

GT_c_l0_t<-merge(GT_c1,L0_train1,by="Point",all.x=F,all.y=T)
plot(GT_c_l0_t,col="red")
str(GT_c_l0_t@data)

L0_val1<-L0_val[,.(rocks,Point)]

GT_c_l0_v<-merge(GT_c1,L0_val1,by="Point",all.x=F,all.y=T)
plot(GT_c_l0_v)
str(GT_c_l0_v@data)

### Supervised class with rstoolbox and rf: rocks
#mask_macro_others<-raster("Data_out/mask/mask_macro_others.tif")
#sat_rocks<-mask(sat,mask_macro_others)
#writeRaster(sat_rocks,"Data_out/Stack/sat_rocks.tif", overwrite=T)
sat_rocks<-stack("Data_out/Stack/sat_rocks.tif")
beep(3)

set.seed(11)
beginCluster(7)
SC1_rocks<-superClass(img=sat_rocks,model="rf",trainData=GT_c_l0_t,responseCol="rocks.x",valData=GT_c_l0_v,polygonBasedCV=F,predict=T,
                predType="raw",filename="Data_out/Class_map/SC1_rocks.tif")
endCluster()
beep(3)

plot(SC1_rocks$map, colNA=1,main="Rocks VS rest")
rockmap<-SC1_rocks$map
SC1_rocks$classMapping

###Isolating rock area
rocks_mask<-rockmap==2
rocks_mask[rocks_mask==0]<-NA # turn wet area (coded zero) into NA
plot(rocks_mask, colNA=1)
writeRaster(rocks_mask,"Data_out/Habitat_classes/Level0/rocks_mask.tif", overwrite=T)

mask_rock_others<-rockmap==1
mask_rock_others[mask_rock_others==0]<-NA # turn wet area (coded zero) into NA
plot(mask_rock_others, colNA=1)
writeRaster(mask_rock_others,"Data_out/mask/mask_rock_others.tif", overwrite=T)

area_rocks<-sum(rocks_mask[rocks_mask==1])*res(rocks_mask)[1]^2*10^-6 #calculate dry area size in Km2

beep(4)


#################### shells #################################
##Split data in training + validation using caret balanced splitting

set.seed(10)
trainIndex <- createDataPartition(DF2$shells, p = .7, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

L0_train<-DF2[trainIndex]
L0_val<-DF2[-trainIndex]


###Introduce new columns on training and validation polygons
L0_train1<-L0_train[,.(shells,Point)]

GT_c_l0_t<-merge(GT_c1,L0_train1,by="Point",all.x=F,all.y=T)
plot(GT_c_l0_t,col="red")
str(GT_c_l0_t@data)

L0_val1<-L0_val[,.(shells,Point)]

GT_c_l0_v<-merge(GT_c1,L0_val1,by="Point",all.x=F,all.y=T)
plot(GT_c_l0_v)
str(GT_c_l0_v@data)

### Supervised class with rstoolbox and rf: shells
#sat_shells<-subset(sat,subset=c("B03_20200204","B04_20200204","B08_20200204","B08A_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV"))
mask_rock_others<-stack("Data_out/mask/mask_rock_others.tif")
sat_shells<-mask(sat,mask_rock_others)
writeRaster(sat_shells,"Data_out/Stack/sat_shells.tif",overwrite=T)
#sat_shells<-raster("Data_out/Stack/sat_shells.tif") #made from the remaining area after excluding macroalgae and rock areas


set.seed(11)
beginCluster(7)
SC1_shells<-superClass(img=sat_shells,model="rf",trainData=GT_c_l0_t,responseCol="shells.x",valData=GT_c_l0_v,polygonBasedCV=F,predict=T,
                      predType="raw",filename="Data_out/Class_map/SC1_shells.tif")
endCluster()
beep(3)

plot(SC1_shells$map, colNA=1,main="Shells VS rest")
shellsmap<-SC1_shells$map
SC1_shells$classMapping

###Isolating shells area
shells_mask<-shellsmap==2
shells_mask[shells_mask==0]<-NA # turn wet area (coded zero) into NA
plot(shells_mask, colNA=1)
writeRaster(shells_mask,"Data_out/Habitat_classes/shells_mask.tif",overwrite=T)

mask_shells_others<-shellsmap==1
mask_shells_others[mask_shells_others==0]<-NA # turn wet area (coded zero) into NA
plot(mask_shells_others, colNA=1)
writeRaster(mask_shells_others,"Data_out/mask/mask_shells_others.tif",overwrite=T)

area_shells<-sum(shells_mask[shells_mask==1])*res(shells_mask)[1]^2*10^-6 #calculate dry area size in Km2









#################### sediment #################################
##Split data in training + validation using caret balanced splitting

set.seed(10)
trainIndex <- createDataPartition(DF2$sedmnt0, p = .7, 
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
plot(bare_sediment_mask, colNA=1)
writeRaster(bare_sediment_mask,"Data_out/Habitat_classes/bare_sediment_mask.tif",overwrite=T)

area_bare_sediment<-sum(bare_sediment_mask[bare_sediment_mask==1])*res(bare_sediment_mask)[1]^2*10^-6 #calculate bare sediment area size in Km2

##Isolating uca sediment
uca_mask<-sedmnt0map==2
uca_mask[uca_mask==0]<-NA # turn wet area (coded zero) into NA
plot(uca_mask, colNA=1)
writeRaster(uca_mask,"Data_out/Habitat_classes/uca_mask.tif",overwrite=T)

area_uca_mask<-sum(uca_mask[uca_mask==1])*res(uca_mask)[1]^2*10^-6 #calculate bare sediment area size in Km2

##Isolating uca sediment
waterbody_mask<-sedmnt0map==3
waterbody_mask[waterbody_mask==0]<-NA # turn wet area (coded zero) into NA
plot(waterbody_mask, colNA=1)
writeRaster(waterbody_mask,"Data_out/Habitat_classes/waterbody_mask.tif",overwrite=T)

area_waterbody_mask<-sum(waterbody_mask[waterbody_mask==1])*res(waterbody_mask)[1]^2*10^-6 #calculate bare sediment area size in Km2



######### isolate waterbody before removing uca and bare sediment

DF2[covr_vr=="water_body",table(c_water)]
DF2[cvr_vr1=="water_body",table(c_water)]
DF[,table(WB)]
DF2[,table(WB)]
DF2[c_water>=85,table(covr_vr)]



