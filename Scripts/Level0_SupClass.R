<<<<<<< HEAD
setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

packs<-c("randomForest","caret","sf","beepr","RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis","data.table","reshape2")
lapply(packs,require,character.only=T)

## Load sat img

sat<-stack("Data_out/Stack/Final_stack.tif") ##created in script GraVSSat_Preliminary
names(sat)<-names(sat)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
                          "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
                          "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","intensity","iv_multi","rededge_multi","rededge_sum",
                          "visible_multi")

##Load wet and dry masks (produced in script SupClass_WetVSDry)
dry_mask<-raster("Data_out/mask/dry_mask.tif")
#plot(dry_mask,colNA=1)

wet_mask<-stack("Data_out/mask/wet_mask.tif")
#plot(wet_mask,colNA=1)

##Load GT polygons
GT_c1<-readOGR("Data_out/Polygons/GT_c1.shp") ##created in script Data_cleanup_SUp_Class
#plot(GT_c1)
#unique(GT_c1@data$cvr_vr1)
DF2<-data.table(GT_c1@data)
#str(DF2)
#write.csv(DF2,"Data_out/db/DF2.csv",row.names=F)
########################################

####Level 0 classification

## mask out wet areas first
beginCluster(7)
sat_dry<-mask(sat,dry_mask)
#plot(sat_dry, colNA=1,main="sat dry")
endCluster()
beep(3)
writeRaster(sat_dry,"Data_out/Stack/sat_dry.tif",overwrite=T)
sat_dry<-stack("Data_out/Stack/sat_dry.tif")

##mask out dry areas now
beginCluster(7)
sat_wet<-mask(sat,wet_mask)
plot(sat_wet, colNA=1,main="sat wet")
endCluster()
beep(3)
writeRaster(sat_wet,"Data_out/Stack/sat_wet.tif")


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



##########################################################################
##########################################################################
### Supervised class with rstoolbox and rf: all classes all area
set.seed(11)
beginCluster(7)
SC1<-superClass(img=sat,model="rf",trainData=GT_c_l0_t,responseCol="covr_vrA",valData=GT_c_l0_v,polygonBasedCV=F,predict=T,
                predType="raw",filename=NULL)
endCluster()
beep(3)
saveRSTBX(SC1,"Data_out/models/SC1_all",fomat="raster")
SC1<-readRSTBX("Data_out/models/SC1_all")

SC1$classMapping
plot(SC1$map,colNA=1,col=c("lightgrey","green","red","blue","grey30"))
SC1_all_tif<-SC1$map
writeRaster(SC1_all_tif,"Data_out/models/SC1_all.tif")

xx<-drawExtent()
adonga_t<-crop(SC1$map,xx)
plot(adonga_t, colNA=1,col=c("lightgrey","green","red","blue","grey30"))

### Supervised class with rstoolbox and rf: dry area
set.seed(12)
beginCluster(7)
SC1_dry<-superClass(img=sat_dry,model="rf",trainData=GT_c_l0_t,responseCol="covr_vrA",valData=GT_c_l0_v,polygonBasedCV=F,predict=T,
                predType="raw",filename="Data_out/Class_map/SC1_dry.tif")
endCluster()
beep(3)
saveRSTBX(SC1_dry,"Data_out/models/Sc1_dry",format="raster")

plot(SC1_dry$map, colNA=1, main="cover over dry")
SC1_dry$classMapping
writeRaster(SC1_dry$map,"Data_out/models/SC1_dry.tif")



xx<-drawExtent()
adonga_dry<-crop(SC1_dry$map,xx)
plot(adonga, colNA=1)

### Supervised class with rstoolbox and rf: wet area
set.seed(13)
beginCluster(7)
SC1_wet<-superClass(img=sat_wet,model="rf",trainData=GT_c_l0_t,responseCol="covr_vrA",valData=GT_c_l0_v,polygonBasedCV=F,predict=T,
                    predType="raw",filename="Data_out/Class_map/SC1_wet.tif")
endCluster()
beep(3)
saveRSTBX(SC1_wet,"Data_out/models/Sc1_wet",format="raster")
SC1_wet$classMapping

plot(SC1_wet$map, colNA=1, main="cover over wet")
writeRaster(SC1_wet$map,"Data_out/models/SC1_wet.tif")





################################################################ 
###individual classification schemes


#################### macroalgae #################################
##Split data in training + validation using caret balanced splitting

set.seed(10)
trainIndex_macro <- createDataPartition(L0_train$macro, p = .7, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex_macro)

L0_train_macro<-L0_train[trainIndex_macro]
L0_train_macro[,table(macro)]
L0_val_macro<-L0_train[-trainIndex_macro]
L0_val_macro[,table(macro)]

###Introduce new columns on training and validation polygons
#L0_train1_macro<-L0_train_macro[,.(macro,Point)]

GT_c_l0_t_macro<-merge(GT_c1,L0_train_macro,by="Point",all.x=F,all.y=T)
plot(GT_c_l0_t_macro,col="red")
#str(GT_c_l0_t@data)

#L0_val1_macro<-L0_val_macro[,.(macro,Point)]

GT_c_l0_v_macro<-merge(GT_c1,L0_val_macro,by="Point",all.x=F,all.y=T)
plot(GT_c_l0_v_macro)
#str(GT_c_l0_v@data)


### Supervised class with rstoolbox and rf: macroalgae
#sat_macro<-subset(sat,subset=c("B03_20200204","B04_20200204","B08_20200204","B08A_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV"))

set.seed(11)
beginCluster(7)
SC1_macro<-superClass(img=sat,model="rf",trainData=GT_c_l0_t_macro,responseCol="macro.x",valData=GT_c_l0_v_macro,polygonBasedCV=F,predict=T,
                      predType="raw",filename="Data_out/Class_map/SC1_macro_valtot.tif")
endCluster()
beep(3)

plot(SC1_macro$map, colNA=1,main="Macroalgae VS rest")
xx<-drawExtent()
cc<-crop(SC1_macro$map,xx)
plot(cc,colNA=1)

macromap<-SC1_macro$map
SC1_macro$classMapping

###Isolating macroalgae area
macro_mask<-macromap==1
macro_mask[macro_mask==0]<-NA # turn wet area (coded zero) into NA
plot(macro_mask, colNA=1)
writeRaster(macro_mask,"Data_out/Habitat_classes/Level0/macro_mask_valtot.tif", overwrite=T)

mask_macro_others<-macromap==2
mask_macro_others[mask_macro_others==0]<-NA # turn wet area (coded zero) into NA
plot(mask_macro_others, colNA=1)
writeRaster(mask_macro_others,"Data_out/mask/mask_macro_others_valtot.tif", overwrite=T)

area_macro<-sum(macro_mask[macro_mask==1])*res(macro_mask)[1]^2*10^-6 #calculate dry area size in Km2




#################### ROCKS #################################
##Split data in training + validation using caret balanced splitting

set.seed(10)
trainIndex_rock <- createDataPartition(L0_train$rocks, p = .7, 
                                  list = FALSE, 
                                  times = 1)
#head(trainIndex_rock)

L0_train_rock<-L0_train[trainIndex_rock]
L0_val_rock<-L0_train[-trainIndex_rock]


###Introduce new columns on training and validation polygons
#L0_train1_rock<-L0_train_rock[,.(rocks,Point)]

GT_c_l0_t_rock<-merge(GT_c1,L0_train_rock,by="Point",all.x=F,all.y=T)
plot(GT_c_l0_t_rock,col="red")
#str(GT_c_l0_t@data)

#L0_val1_rock<-L0_val_rock[,.(rocks,Point)]

GT_c_l0_v_rock<-merge(GT_c1,L0_val_rock,by="Point",all.x=F,all.y=T)
plot(GT_c_l0_v_rock)
#str(GT_c_l0_v@data)

### Supervised class with rstoolbox and rf: rocks
#mask_macro_others<-raster("Data_out/mask/mask_macro_others.tif")
sat_rocks<-mask(sat,mask_macro_others)
writeRaster(sat_rocks,"Data_out/Stack/sat_rocks_valtot.tif", overwrite=T)
#sat_rocks<-stack("Data_out/Stack/sat_rocks_valtot.tif")
beep(3)

set.seed(11)
beginCluster(7)
SC1_rocks<-superClass(img=sat_rocks,model="rf",trainData=GT_c_l0_t_rock,responseCol="rocks.x",valData=GT_c_l0_v_rock,polygonBasedCV=F,predict=T,
                predType="raw",filename="Data_out/Class_map/SC1_rocks_valtot.tif")
endCluster()
beep(3)

plot(SC1_rocks$map, colNA=1,main="Rocks VS rest")
rockmap<-SC1_rocks$map
SC1_rocks$classMapping

###Isolating rock area
rocks_mask<-rockmap==2
rocks_mask[rocks_mask==0]<-NA # turn wet area (coded zero) into NA
plot(rocks_mask, colNA=1)
writeRaster(rocks_mask,"Data_out/Habitat_classes/Level0/rocks_mask_valtot.tif", overwrite=T)

mask_rock_others<-rockmap==1
mask_rock_others[mask_rock_others==0]<-NA # turn wet area (coded zero) into NA
plot(mask_rock_others, colNA=1)
writeRaster(mask_rock_others,"Data_out/mask/mask_rock_others_valtot.tif", overwrite=T)

area_rocks<-sum(rocks_mask[rocks_mask==1])*res(rocks_mask)[1]^2*10^-6 #calculate dry area size in Km2

beep(4)


#################### shells #################################
##Split data in training + validation using caret balanced splitting

set.seed(10)
trainIndex_shell <- createDataPartition(L0_train$shells, p = .8, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex_shell)

L0_train_shell<-L0_train[trainIndex_shell]
L0_train_shell[,table(shells)]
L0_val_shell<-L0_train[-trainIndex_shell]
L0_val_shell[,table(shells)]

###Introduce new columns on training and validation polygons
#L0_train1_shell<-L0_train_shell[,.(shells,Point)]

GT_c_l0_t_shell<-merge(GT_c1,L0_train_shell,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_shell,col="red")
#str(GT_c_l0_t@data)

#L0_val1_shell<-L0_val_shell[,.(shells,Point)]

GT_c_l0_v_shell<-merge(GT_c1,L0_val_shell,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_shell)
#str(GT_c_l0_v@data)

### Supervised class with rstoolbox and rf: shells
#sat_shells<-subset(sat,subset=c("B03_20200204","B04_20200204","B08_20200204","B08A_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV"))
#mask_rock_others<-stack("Data_out/mask/mask_rock_others_valtot.tif")
#sat_shells<-mask(sat,mask_rock_others)
#writeRaster(sat_shells,"Data_out/Stack/sat_shells_valtot.tif",overwrite=T)
sat_shells<-stack("Data_out/Stack/sat_shells_valtot.tif") #made from the remaining area after excluding macroalgae and rock areas
beep(4)

set.seed(11)
beginCluster(7)
SC1_shells<-superClass(img=sat_shells,model="rf",trainData=GT_c_l0_t_shell,responseCol="shells.x",valData=GT_c_l0_v_shell,polygonBasedCV=F,predict=T,
                      predType="raw",filename="Data_out/Class_map/SC1_shells.tif")
endCluster()
beep(3)
saveRSTBX(SC1_shells,"Data_out/models/SC1_shells",format = "raster")

plot(SC1_shells$map, colNA=1,main="Shells VS rest")
shellsmap<-SC1_shells$map
SC1_shells$classMapping

###Isolating shells area
shells_mask<-shellsmap==2
shells_mask[shells_mask==0]<-NA # turn wet area (coded zero) into NA
plot(shells_mask, colNA=1)
writeRaster(shells_mask,"Data_out/Habitat_classes/Level0/shells_mask_valtot.tif",overwrite=T)

mask_shells_others<-shellsmap==1
mask_shells_others[mask_shells_others==0]<-NA # turn wet area (coded zero) into NA
plot(mask_shells_others, colNA=1)
writeRaster(mask_shells_others,"Data_out/mask/mask_shells_others_valtot.tif",overwrite=T)

area_shells<-sum(shells_mask[shells_mask==1])*res(shells_mask)[1]^2*10^-6 #calculate dry area size in Km2

###############################################################################################################
##############################################################################################################
######################### remove water #####################################################################

set.seed(10)
trainIndex_WB <- createDataPartition(L0_train$WB, p = .7, 
                                      list = FALSE, 
                                      times = 1)
head(trainIndex_WB)

L0_train_WB<-L0_train[trainIndex_WB]
L0_train_WB[,table(WB)]
L0_val_WB<-L0_train[-trainIndex_WB]
L0_val_WB[,table(WB)]

###Introduce new columns on training and validation polygons
#L0_train1_WB<-L0_train_WB[,.(WB,Point)]

GT_c_l0_t_WB<-merge(GT_c1,L0_train_WB,by="Point",all.x=F,all.y=T)
plot(GT_c_l0_t_WB,col="red")
#str(GT_c_l0_t@data)

#L0_val1_WB<-L0_val_WB[,.(WB,Point)]

GT_c_l0_v_WB<-merge(GT_c1,L0_val_WB,by="Point",all.x=F,all.y=T)
plot(GT_c_l0_v_WB)
#str(GT_c_l0_v@data)

### Supervised class with rstoolbox and rf: shells
#mask_shells_others<-stack("Data_out/mask/mask_shells_others.tif")
#sat_WB<-mask(sat,mask_shells_others)
#writeRaster(sat_WB,"Data_out/Stack/sat_WB_valtot.tif",overwrite=T)
sat_WB<-stack("Data_out/Stack/sat_sed.tif") #made from the remaining area after excluding macroalgae, rock and shell areas
beep(2)

set.seed(1111)
beginCluster(7)
SC1_WB<-superClass(img=sat_WB,model="rf",trainData=GT_c_l0_t_WB,responseCol="WB.x",valData=GT_c_l0_v_WB,polygonBasedCV=F,predict=T,
                        predType="raw",filename=NULL)
endCluster()
beep(3)
saveRSTBX(SC1_WB,"Data_out/models/SC1_WB",format = "raster")
SC1_WB<-readRSTBX("Data_out/models/SC1_WB.tif")

plot(SC1_WB$map, colNA=1,main="Bare Sed VS Uca VS flooded sed")
WBmap<-SC1_WB$map
SC1_WB$classMapping

ad<-drawExtent()
adp<-crop(SC1_WB$map,ad)
plot(adp,colNA=1,col=c("white","red"))

###Isolating bare sediment areas
dry_mask<-WBmap==1
dry_mask[dry_mask==0]<-NA # turn wet area (coded zero) into NA
plot(dry_mask, colNA=1)
writeRaster(dry_mask,"Data_out/Habitat_classes/dry_mask_valtot.tif",overwrite=T)

area_dry<-sum(dry_mask[dry_mask==1])*res(dry_mask)[1]^2*10^-6 #calculate bare sediment area size in Km2

##Isolating wet sediment
wet_mask<-WBmap==2
wet_mask[wet_mask==0]<-NA # turn wet area (coded zero) into NA
plot(wet_mask, colNA=1)
writeRaster(wet_mask,"Data_out/Habitat_classes/wet_mask_valtot.tif",overwrite=T)

area_wet_mask<-sum(wet_mask[wet_mask==1])*res(wet_mask)[1]^2*10^-6 #calculate bare sediment area size in Km2








#################### sediment #################################
##Split data in training + validation using caret balanced splitting

set.seed(101)
trainIndex_sed <- createDataPartition(L0_train$sedmnt0, p = .7, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex_sed)

L0_train_sed<-L0_train[trainIndex_sed]
L0_val_sed<-L0_train[-trainIndex_sed]


###Introduce new columns on training and validation polygons
#L0_train1_sed<-L0_train_sed[,.(sedmnt0,Point)]

GT_c_l0_t_sed<-merge(GT_c1,L0_train_sed,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_sed,col="red")
#str(GT_c_l0_t@data)

#L0_val1_sed<-L0_val_sed[,.(sedmnt0,Point)]

GT_c_l0_v_sed<-merge(GT_c1,L0_val_sed,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_sed)
#str(GT_c_l0_v@data)

### Supervised class with rstoolbox and rf: shells
#mask_shells_others<-stack("Data_out/mask/mask_shells_others.tif")
#sat_sed<-mask(sat,mask_shells_others)
#writeRaster(sat_sed,"Data_out/Stack/sat_sed_valtot.tif",overwrite=T)

#sat_sed_dry<-mask(sat,dry_mask)
#writeRaster(sat_sed_dry,"Data_out/Stack/sat_sed_dry_valtot.tif",overwrite=T)

#sat_sed_wet<-mask(sat,wet_mask)
#writeRaster(sat_sed_wet,"Data_out/Stack/sat_sed_wet_valtot.tif",overwrite=T)

sat_sed<-stack("Data_out/Stack/sat_sed_valtot.tif") #made from the remaining area after excluding macroalgae, rock and shell areas
sat_bsed<-stack("Data_out/Stack/sat_bsed_valtot.tif") #Remaining area after isolating macro, rocks and shells
beep(2)


### ALL AREA (This is the best performing separation of bare sediment (including waterbody) vs uca. Used )
set.seed(1111)
beginCluster(7)
SC1_sedmnt0<-superClass(img=sat_bsed,model="rf",trainData=GT_c_l0_t_sed,responseCol="sedmnt0.y",valData=GT_c_l0_v_sed,polygonBasedCV=F,predict=T,
                        predType="raw",filename="Data_out/Class_map/SC1_sedmnt0_valtot.tif")
endCluster()
beep(3)
saveRSTBX(SC1_sedmnt0,"Data_out/models/SC1_sedmnt0",format = "raster",overwrite=T)
SC1_sedmnt0<-readRSTBX("Data_out/models/SC1_sedmnt0.tif")


plot(SC1_sedmnt0$map, colNA=1,main="Bare Sed VS Uca VS flooded sed")
sedmnt0map<-SC1_sedmnt0$map
SC1_sedmnt0$classMapping

ad<-drawExtent()
adp<-crop(SC1_sedmnt0$map,ad)
plot(adp,colNA=1,col=c("white","red"))

###Isolating bare sediment areas
bare_sediment_mask<-sedmnt0map==1
bare_sediment_mask[bare_sediment_mask==0]<-NA # turn wet area (coded zero) into NA
plot(bare_sediment_mask, colNA=1)
writeRaster(bare_sediment_mask,"Data_out/Habitat_classes/bare_sediment_mask_valtot.tif",overwrite=T)

area_bare_sediment<-sum(bare_sediment_mask[bare_sediment_mask==1])*res(bare_sediment_mask)[1]^2*10^-6 #calculate bare sediment area size in Km2

##Isolating uca sediment
uca_mask<-sedmnt0map==2
uca_mask[uca_mask==0]<-NA # turn wet area (coded zero) into NA
plot(uca_mask, colNA=1)
writeRaster(uca_mask,"Data_out/Habitat_classes/uca_mask_valtot.tif",overwrite=T)

area_uca_mask<-sum(uca_mask[uca_mask==1])*res(uca_mask)[1]^2*10^-6 #calculate bare sediment area size in Km2





### DRY

set.seed(1111)
beginCluster(7)
SC1_sedmnt0<-superClass(img=sat_sed_dry,model="rf",trainData=GT_c_l0_t_sed,responseCol="sediment0.y",valData=GT_c_l0_v_sed,polygonBasedCV=F,predict=T,
                       predType="raw",filename="Data_out/Class_map/SC1_sedmnt0_valtot.tif")
endCluster()
beep(3)
saveRSTBX(SC1_sedmnt0,"Data_out/models/SC1_sedmnt0_dry",format = "raster")
SC1_sedmnt0_dry<-readRSTBX("Data_out/models/SC1_sedmnt0_dry.tif")


plot(SC1_sedmnt0_dry$map, colNA=1,main="Bare Sed VS Uca VS flooded sed")
sedmnt0map<-SC1_sedmnt0$map
SC1_sedmnt0$classMapping

ad<-drawExtent()
adp<-crop(SC1_sedmnt0$map,ad)
plot(adp,colNA=1,col=c("white","red"))

###Isolating bare sediment areas
bare_sediment_mask_dry<-sedmnt0map==1
bare_sediment_mask_dry[bare_sediment_mask_dry==0]<-NA # turn wet area (coded zero) into NA
plot(bare_sediment_mask_dry, colNA=1)
writeRaster(bare_sediment_mask_dry,"Data_out/Habitat_classes/bare_sediment_mask_dry_valtot.tif",overwrite=T)

area_bare_sediment_dry<-sum(bare_sediment_mask_dry[bare_sediment_mask_dry==1])*res(bare_sediment_mask_dry)[1]^2*10^-6 #calculate bare sediment area size in Km2

##Isolating uca sediment
uca_mask_dry<-sedmnt0map==2
uca_mask_dry[uca_mask_dry==0]<-NA # turn wet area (coded zero) into NA
plot(uca_mask_dry, colNA=1)
writeRaster(uca_mask_dry,"Data_out/Habitat_classes/uca_mask_dry_valtot.tif",overwrite=T)

area_uca_mask_dry<-sum(uca_mask_dry[uca_mask_dry==1])*res(uca_mask_dry)[1]^2*10^-6 #calculate bare sediment area size in Km2


### WET

sat_sed_wet<-stack("Data_out/Stack/sat_sed_wet_valtot.tif")

set.seed(1111)
beginCluster(7)
SC1_sedmnt0<-superClass(img=sat_sed_wet,model="rf",trainData=GT_c_l0_t_sed,responseCol="sedmnt0.y",valData=GT_c_l0_v_sed,polygonBasedCV=F,predict=T,
                        predType="raw",filename="Data_out/Class_map/SC1_sedmnt0_wet_valtot.tif")
endCluster()
beep(3)
saveRSTBX(SC1_sedmnt0,"Data_out/models/SC1_sedmnt0_wet",format = "raster")
SC1_sedmnt0_wet<-readRSTBX("Data_out/models/SC1_sedmnt0_wet.tif")

plot(SC1_sedmnt0_wet$map, colNA=1,main="Bare Sed VS Uca VS flooded sed")
SC1_sedmnt0_wet<-SC1_sedmnt0_wet$map
SC1_sedmnt0_wet$classMapping

ad<-drawExtent()
adp<-crop(SC1_sedmnt0_wet$map,ad)
plot(adp,colNA=1,col=c("white","red"))

###Isolating bare sediment areas
bare_sediment_mask_wet<-sedmnt0map==1
bare_sediment_mask_wet[bare_sediment_mask_wet==0]<-NA # turn wet area (coded zero) into NA
plot(bare_sediment_mask_wet, colNA=1)
writeRaster(bare_sediment_mask_wet,"Data_out/Habitat_classes/bare_sediment_mask_wet_valtot.tif",overwrite=T)

area_bare_sediment_wet<-sum(bare_sediment_mask_wet[bare_sediment_mask_wet==1])*res(bare_sediment_mask_wet)[1]^2*10^-6 #calculate bare sediment area size in Km2

##Isolating uca sediment
uca_mask_wet<-sedmnt0map==2
uca_mask_wet[uca_mask_wet==0]<-NA # turn wet area (coded zero) into NA
plot(uca_mask_wet, colNA=1)
writeRaster(uca_mask_wet,"Data_out/Habitat_classes/uca_mask_wet_valtot.tif",overwrite=T)

area_uca_mask_wet<-sum(uca_mask_wet[uca_mask_wet==1])*res(uca_mask_wet)[1]^2*10^-6 #calculate bare sediment area size in Km2

beep(4)







##Isolating uca sediment
waterbody_mask<-sedmnt0map==3
waterbody_mask[waterbody_mask==0]<-NA # turn wet area (coded zero) into NA
plot(waterbody_mask, colNA=1)
writeRaster(waterbody_mask,"Data_out/Habitat_classes/waterbody_mask_valtot.tif",overwrite=T)

area_waterbody_mask<-sum(waterbody_mask[waterbody_mask==1])*res(waterbody_mask)[1]^2*10^-6 #calculate bare sediment area size in Km2



######################################## BARE SEDIMENT ################################
#######################################################################################

##Split data in training + validation using caret balanced splitting

set.seed(101)
trainIndex_bsed <- createDataPartition(L0_train$bsed, p = .7, 
                                        list = FALSE, 
                                        times = 1)

#trainIndex_bsed1 <- createDataPartition(L0_train$bsed1, p = .7, 
                                       #list = FALSE, 
                                       #times = 1)
head(trainIndex_bsed)

L0_train_bsed<-L0_train[trainIndex_bsed]
L0_train_bsed[,table(bsed)]
L0_val_bsed<-L0_train[-trainIndex_bsed]
L0_val_bsed[,table(bsed)]

#L0_train_bsed1<-L0_train[trainIndex_bsed1]
#L0_train_bsed1[table(bsed1)]
#L0_val_bsed1<-L0_train[-trainIndex_bsed1]
#L0_val_bsed1[,table(bsed)]

###Introduce new columns on training and validation polygons
#L0_train1_bsed<-L0_train_bsed[,.(bsed,Point)]

GT_c_l0_t_bsed<-merge(GT_c1,L0_train_bsed,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_bsed,col="red")
#str(GT_c_l0_t@data)
#GT_c_l0_t_bsed1<-merge(GT_c1,L0_train_bsed1,by="Point",all.x=F,all.y=T)

#L0_val1_bsed<-L0_val_bsed[,.(bsed,Point)]

GT_c_l0_v_bsed<-merge(GT_c1,L0_val_bsed,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_bsed)
#str(GT_c_l0_v@data)
#GT_c_l0_v_bsed1<-merge(GT_c1,L0_val_bsed1,by="Point",all.x=F,all.y=T)

### Supervised class with rstoolbox and rf: bsed
#mask_shells_others<-stack("Data_out/mask/mask_shells_others_valtot.tif")
#mask_uca_others_valtot<-stack("Data_out/mask/mask_uca_others_valtot.tif")
#sat_bsed<-mask(sat,mask_shells_others)
#writeRaster(sat_bsed,"Data_out/Stack/sat_bsed_valtot.tif",overwrite=T)
#sat_bsed1<-mask(sat,mask_uca_others_valtot) #when isolating uca areas first and then bare sediment including waterbody
#writeRaster(sat_bsed1,"Data_out/Stack/sat_bsed1_valtot.tif",overwrite=T)
#sat_bsed1<-stack("Data_out/Stack/sat_bsed1_valtot.tif") #made from remaining area after removing uca
sat_bsed<-stack("Data_out/Stack/sat_bsed_valtot.tif") #made from the remaining area after excluding macroalgae, rock and shell areas
beep(5)

set.seed(111)
beginCluster(7)
SC1_bsed<-superClass(img=sat_bsed,model="rf",trainData=GT_c_l0_t_bsed,responseCol="bsed.x",valData=GT_c_l0_v_bsed,polygonBasedCV=F,predict=T,
                       predType="raw",filename=NULL)
#SC1_bsed1<-superClass(img=sat_bsed1,model="rf",trainData=GT_c_l0_t_bsed1,responseCol="bsed1.x",valData=GT_c_l0_v_bsed1,polygonBasedCV=F,predict=T,
                     #predType="raw",filename="Data_out/Class_map/SC1_bsed_valtot.tif")
endCluster()
beep(3)
saveRSTBX(SC1_bsed,"Data_out/models/SC1_bsed",format = "raster")
#saveRSTBX(SC1_bsed1,"Data_out/models/SC1_bsed1",format = "raster")

plot(SC1_bsed$map, colNA=1,main="Bare sediment VS rest")
xx<-drawExtent()
cc<-crop(SC1_bsed$map,xx)
plot(cc,colNA=1)

bsedmap<-SC1_bsed$map
SC1_bsed1$classMapping


###Isolating bsed area
bsed_mask<-bsedmap==1
bsed_mask[bsed_mask==0]<-NA # turn wet area (coded zero) into NA
plot(bsed_mask, colNA=1)
writeRaster(bsed_mask,"Data_out/Habitat_classes/bsed_mask_valtot.tif",overwrite=T)

mask_bsed_others<-bsedmap==2
mask_bsed_others[mask_bsed_others==0]<-NA # turn wet area (coded zero) into NA
plot(mask_bsed_others, colNA=1)
writeRaster(mask_bsed_others,"Data_out/mask/mask_bsed_others_valtot.tif",overwrite=T)

area_bsed<-sum(bsed_mask[bsed_mask==1])*res(bsed_mask)[1]^2*10^-6 #calculate dry area size in Km2


####################################################################################################
######################################## UCA  ################################
#######################################################################################

##Split data in training + validation using caret balanced splitting

set.seed(10)
trainIndex_uca <- createDataPartition(L0_train$uca, p = .7, 
                                       list = FALSE, 
                                       times = 1)
head(trainIndex_uca)

L0_train_uca<-L0_train[trainIndex_uca]
L0_train_uca[,table(uca)]
L0_val_uca<-L0_train[-trainIndex_uca]
L0_val_uca[,table(uca)]

###Introduce new columns on training and validation polygons
#L0_train1_uca<-L0_train_uca[,.(uca,Point)]

GT_c_l0_t_uca<-merge(GT_c1,L0_train_uca,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_uca,col="red")
#str(GT_c_l0_t@data)

#L0_val1_uca<-L0_val_uca[,.(uca,Point)]

GT_c_l0_v_uca<-merge(GT_c1,L0_val_uca,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_uca)
#str(GT_c_l0_v@data)

### Supervised class with rstoolbox and rf: uca
#mask_shells_others<-stack("Data_out/mask/mask_shells_others_valtot.tif")
#sat_uca<-mask(sat,mask_bsed_others)
#writeRaster(sat_uca,"Data_out/Stack/sat_uca_valtot.tif",overwrite=T)
#sat_uca<-stack("Data_out/Stack/sat_uca_valtot.tif") #made from the remaining area after excluding bare sediment (exclusing waterbody)
beep(5)

set.seed(11)
beginCluster(7)
SC1_uca<-superClass(img=sat_bsed,model="rf",trainData=GT_c_l0_t_uca,responseCol="uca.x",valData=GT_c_l0_v_uca,polygonBasedCV=F,predict=T,
                     predType="raw",filename="Data_out/Class_map/SC1_uca_valtot.tif")
endCluster()
beep(3)
saveRSTBX(SC1_uca,"Data_out/models/SC1_uca",format="raster")

plot(SC1_uca$map, colNA=1,main="Uca VS rest")
ucamap<-SC1_uca$map
SC1_uca$classMapping

###Isolating uca area
uca_mask<-ucamap==2
uca_mask[uca_mask==0]<-NA # turn wet area (coded zero) into NA
plot(uca_mask, colNA=1)
writeRaster(uca_mask,"Data_out/Habitat_classes/uca_mask_valtot.tif",overwrite=T)

##Isolating bare sediment
mask_uca_others<-ucamap==1
mask_uca_others[mask_uca_others==0]<-NA # turn wet area (coded zero) into NA
plot(mask_uca_others, colNA=1)
writeRaster(mask_uca_others,"Data_out/mask/mask_uca_others_valtot.tif",overwrite=T)

area_uca<-sum(uca_mask[uca_mask==1])*res(uca_mask)[1]^2*10^-6 #calculate dry area size in Km2

