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

sat<-stack("Data_out/Stack/Final_stack.tif") ##created in script GraVSSat_Preliminary
names(sat)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
              "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
              "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","MSAVI2","intensity","iv_multi","rededge_multi","rededge_sum",
              "visible_multi")

##Load wet and dry masks (produced in script SupClass_WetVSDry)
#dry_mask<-raster("Data_out/mask/dry_mask.tif")
#plot(dry_mask,colNA=1)

#wet_mask<-stack("Data_out/mask/wet_mask.tif")
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
#beginCluster(7)
#sat_dry<-mask(sat,dry_mask)
#plot(sat_dry, colNA=1,main="sat dry")
#endCluster()
#beep(3)
#writeRaster(sat_dry,"Data_out/Stack/sat_dry.tif",overwrite=T)
#sat_dry<-stack("Data_out/Stack/sat_dry.tif")

##mask out dry areas now
#beginCluster(7)
#sat_wet<-mask(sat,wet_mask)
#plot(sat_wet, colNA=1,main="sat wet")
#endCluster()
#beep(3)
#writeRaster(sat_wet,"Data_out/Stack/sat_wet.tif")


##Split data in training + validation using caret balanced splitting: Use this for final validation
DF3<-data.table(DF2)
DF3[,covr_vrA:=as.character(covr_vr)][covr_vrA=="water_body",covr_vrA:="bare_sediment"]
DF3[,table(covr_vrA)]
DF3[,covr_vrA1:=covr_vrA][covr_vrA1=="macroalgae"&WD=="wet",covr_vrA1:="macroalgae_wet"][covr_vrA1=="macroalgae"&WD=="dry",covr_vrA1:="macroalgae_dry"]
DF3[,table(covr_vrA1)]
DF3[,covr_vrA2:=covr_vrA][covr_vrA2=="macroalgae"&WD=="wet",covr_vrA2:="macroalgae_wet"][covr_vrA2=="macroalgae"&WD=="dry",covr_vrA2:="macroalgae_dry"][covr_vrA2=="shell"&WD=="wet",covr_vrA2:="shell_wet"][covr_vrA2=="shell"&WD=="dry",covr_vrA2:="shell_dry"]
DF3[,table(covr_vrA2)]
DF3[,covr_vrA3:=covr_vrA][covr_vrA3=="macroalgae"&WD=="wet",covr_vrA3:="macroalgae_wet"][covr_vrA3=="macroalgae"&WD=="dry",covr_vrA3:="macroalgae_dry"][covr_vrA3=="shell"&WD=="wet",covr_vrA3:="shell_wet"][covr_vrA3=="shell"&WD=="dry",covr_vrA3:="shell_dry"][covr_vrA3=="bare_sediment"&WD=="wet",covr_vrA3:="bare_sediment_wet"][covr_vrA3=="bare_sediment"&WD=="dry",covr_vrA3:="bare_sediment_dry"]
DF3[,table(covr_vrA3)]
DF3[,covr_vrA4:=covr_vrA3][covr_vrA4=="uca"&WD=="wet",covr_vrA4:="uca_wet"][covr_vrA4=="uca"&WD=="dry",covr_vrA4:="uca_dry"]
DF3[,table(covr_vrA4)]
DF3[,covr_vrA5:=covr_vrA][covr_vrA5=="bare_sediment"&WD=="wet",covr_vrA5:="bare_sediment_wet"][covr_vrA5=="bare_sediment"&WD=="dry",covr_vrA5:="bare_sediment_dry"][covr_vrA5=="uca"&WD=="wet",covr_vrA5:="uca_wet"][covr_vrA5=="uca"&WD=="dry",covr_vrA5:="uca_dry"]
DF3[,table(covr_vrA5)]
DF3[,covr_vrA6:=covr_vrA][covr_vrA6=="bare_sediment",covr_vrA6:="sediments"][covr_vrA6=="uca",covr_vrA6:="sediments"]
DF3[,table(covr_vrA6)]

set.seed(10)
trainIndex <- createDataPartition(DF3$covr_vrA6, p = .7, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

L0_train<-DF3[trainIndex]
L0_train[,table(covr_vrA6)]

L0_val<-DF3[-trainIndex]
L0_val[,table(covr_vrA6)]


###Introduce new columns on training and validation polygons
#L0_train1<-L0_train[,.(covr_vr,Point)]

GT_c_l0_t<-merge(GT_c1,L0_train,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t,col="red")
#str(GT_c_l0_t@data)

#L0_val1<-L0_val[,.(covr_vr,Point)]

GT_c_l0_v<-merge(GT_c1,L0_val,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v)
#str(GT_c_l0_v@data)

### selection of bands to use
sat1<-subset(sat,c("B02_20200204","B03_20200204","B04_20200204","B08A_20200204","B11_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469","MSAVI2","mNDWI","NDWI","rededge_sum","RVI"))
names(sat1)

sat2<-subset(sat,c("B04_20200204","B05_20200204","B11_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469","MSAVI2","mNDWI","NDWI","intensity","rededge_sum","iv_multi","RVI"))
names(sat2)



##########################################################################
##########################################################################
### Supervised class of cover over with rstoolbox and rf: all classes all area
start<-Sys.time()

set.seed(12)
beginCluster(7)
SC1<-superClass(img=sat2,model="rf",trainData=GT_c_l0_t,responseCol="covr_vrA6",valData=GT_c_l0_v,polygonBasedCV=F,predict=T,
                predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif<-end-start

saveRSTBX(SC1,"Data_out/models/SC1_all",fomat="raster",overwrite=T)
SC1<-readRSTBX("Data_out/models/SC1_all")

SC1$model$finalModel$importance

########
start<-Sys.time()

set.seed(12)
beginCluster(7)
SC1_sel<-superClass(img=sat1,model="rf",trainData=GT_c_l0_t,responseCol="covr_vrA6",valData=GT_c_l0_v,polygonBasedCV=F,predict=T,
                predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif<-end-start


saveRSTBX(SC1_sel,"Data_out/models/SC1_all_sel",fomat="raster",overwrite=F)
SC1_sel<-readRSTBX("Data_out/models/SC1_all_sel")

SC1_sel$model$finalModel$importance

SC1$classMapping
plot(SC1$map,colNA=1,col=c("green","red","lightgrey","blue"))
SC1_tif<-SC1$map
#writeRaster(SC1_tif,"Data_out/models/SC1_selvar.tif")

xx<-drawExtent()
adonga_t<-crop(SC1$map,xx)
plot(adonga_t, colNA=1,col=c("forestgreen","red","lightgrey","blue"))


###Isolating sediments area
seds_mask<-SC1_tif==3
seds_mask[seds_mask==0]<-NA # turn remaining area (coded zero) into NA
plot(seds_mask, colNA=1)
writeRaster(seds_mask,"Data_out/Habitat_classes/Level0/seds_mask_selvar.tif", overwrite=T)

### Saving the macro
mask_macro_t<-SC1_tif==1
mask_macro_t[mask_macro_t==0]<-NA # turn remaining area (coded zero) into NA
plot(mask_macro_t, colNA=1)
writeRaster(mask_macro_t,"Data_out/mask/mask_macro_t_selvar.tif", overwrite=T)

### Saving the rocks
mask_rocks_t<-SC1_tif==2
mask_rocks_t[mask_rocks_t==0]<-NA # turn remaining area (coded zero) into NA
plot(mask_rocks_t, colNA=1)
writeRaster(mask_rocks_t,"Data_out/mask/mask_rocks_t_selvar.tif", overwrite=T)

### Saving the shells
mask_shells_t<-SC1_tif==4
mask_shells_t[mask_shells_t==0]<-NA # turn remaining area (coded zero) into NA
plot(mask_shells_t, colNA=1)
writeRaster(mask_shells_t,"Data_out/mask/mask_shells_t_selvar.tif", overwrite=T)


area_seds<-sum(seds_mask[seds_mask==1])*res(seds_mask)[1]^2*10^-6 #calculate sediments area size in Km2
area_macro<-sum(mask_macro_t[mask_macro_t==1])*res(mask_macro_t)[1]^2*10^-6 #calculate sediments area size in Km2
area_rocks<-sum(mask_rocks_t[mask_rocks_t==1])*res(mask_rocks_t)[1]^2*10^-6 #calculate sediments area size in Km2
area_shells<-sum(mask_shells_t[mask_shells_t==1])*res(mask_shells_t)[1]^2*10^-6 #calculate sediments area size in Km2



###########################################################################
####################################################################################
################## Step 2 classification ###################################

DF3[,sediments_WD:=covr_vrA][sediments_WD=="macroalgae"|sediments_WD=="rock"|sediments_WD=="shell",sediments_WD:="other"][,sediments_WD:=paste(sediments_WD,WD,sep="_")]
DF3[,table(sediments_WD)]
DF4<-DF3[!(sediments_WD=="other_wet"|sediments_WD=="other_dry"|sediments_WD=="NA_NA")]
DF4[,table(sediments_WD)]
DF4[,sediments_WD1:=sediments_WD][sediments_WD=="uca_wet",sediments_WD1:="bare_sediment_wet"]
DF4[,table(sediments_WD1)]
DF4[,finos_class:=ifelse(mud<10,"sandy","muddy")]
DF4[,table(finos_class)]
DF4[,finos_WD:=paste(sediments_WD,finos_class,sep="_")]
DF4[,table(finos_WD)]

DF4[,Grain_EU:=ifelse(mud<10,"sand",ifelse(mud>=10&mud<25,"muddy_sand",ifelse(mud>=25&mud<60,"mixed",ifelse(mud>=60,"muddy",NA))))][cvr_vrA=="bare_sediment"|cvr_vrA=="uca",Final_grain_EU:=paste(cvr_vrA,Grain_EU,sep="_")]
DF4[,table(Final_grain_EU)]
DF4[,table(Grain_EU)]
DF4[,table(cvr_vrA)]
DF4[is.na(mud),.(mud,cvr_vrA,Final_grain_EU)]

DF4[,Final_grain_EU1:=Final_grain_EU][Final_grain_EU=="bare_sediment_muddy_sand",Final_grain_EU1:="bare_sediment_mixed"][Final_grain_EU=="uca_muddy_sand",Final_grain_EU1:="uca_mixed"]
DF4[,table(Final_grain_EU1)]
DF4[,table(uca)]


DF4_1<-DF4[!(is.na(mud)),]
DF4_1[,table(Final_grain_EU)]
DF4_1[,table(uca)]

set.seed(10)
trainIndex <- createDataPartition(DF4$uca, p = .70, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

L1_train_seds<-DF4[trainIndex]
L1_train_seds[,table(uca)]

L1_val_seds<-DF4[-trainIndex]
L1_val_seds[,table(uca)]

###Introduce new columns on training and validation polygons
GT_c_l1_t_seds<-merge(GT_c1,L1_train_seds,by="Point",all.x=F,all.y=T)

GT_c_l1_v_seds<-merge(GT_c1,L1_val_seds,by="Point",all.x=F,all.y=T)


###Mask sat images for sediment areas
seds_mask<-raster("Data_out/Habitat_classes/Level0/seds_mask_selvar.tif")
beginCluster(7)
sat_seds<-raster::mask(sat,seds_mask)
endCluster()
beep(3)
writeRaster(sat_seds,"Data_out/Stack/sat_seds.tif")
sat_seds<-stack("Data_out/Stack/sat_seds.tif")
names(sat_seds)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
              "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
              "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","MSAVI2","intensity","iv_multi","rededge_multi","rededge_sum",
              "visible_multi")

#### Select bands to use ####
sat_seds1<-subset(sat_seds,c("B11_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469","NDMI1","RVI","VH_VV","MSAVI2"))
sat_seds2<-subset(sat_seds,c("B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469","NDMI1","NDWI","VH_VV","MSAVI2"))

######## Random forest class #########
start<-Sys.time()

set.seed(12)
beginCluster(7)
SC1_seds<-superClass(img=sat_seds,model="rf",trainData=GT_c_l1_t_seds,responseCol="uca.y",valData=GT_c_l1_v_seds,polygonBasedCV=F,predict=T,
                    predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif<-end-start

saveRSTBX(SC1_seds,"Data_out/models/SC1_seds",fomat="raster",overwrite=T)
SC1_seds<-readRSTBX("Data_out/models/SC1_seds")

SC1_seds$model$finalModel$importance

SC1_seds$classMapping
plot(SC1_seds$map,colNA=1,col=c("lightgrey","grey35"))
SC1_seds_tif<-SC1_seds$map
writeRaster(SC1_seds_tif,"Data_out/models/SC1_seds_selvar.tif")

xx<-drawExtent()
subs_t<-crop(SC1_seds$map,xx)
plot(subs_t, colNA=1,col=c("khaki1","grey35"))


###Isolating bare sediment area
baresed_mask<-SC1_seds_tif==1
baresed_mask[baresed_mask==0]<-NA # turn remaining area (coded zero) into NA
plot(baresed_mask, colNA=1)
writeRaster(baresed_mask,"Data_out/Habitat_classes/Level0/baresed_mask_selvar.tif", overwrite=T)

### isolating uca
uca_mask<-SC1_seds_tif==2
uca_mask[uca_mask==0]<-NA # turn remaining area (coded zero) into NA
plot(uca_mask, colNA=1)
writeRaster(uca_mask,"Data_out/mask/uca_mask_selvar.tif", overwrite=T)


#######################################################
#####################################################################
######## Classifing uca areas ##################

DF4[,table(finos_class)]
DF5<-DF4[!(is.na(mud))] ## remove data without grain size analysis
DF5[,table(uca)]
DF5[,table(Final_grain_EU)]
DF5[,table(Final_grain_EU1)]

DF6<-DF5[uca=="uca"][,uca:=as.character(uca)] ##Database only for uca
DF6[,table(uca)]
DF6[,table(finos_WD)]
DF6[,table(Final_grain_EU)]
DF6[,finos_WD1:=finos_WD][finos_WD=="uca_wet_muddy"|finos_WD=="uca_wet_sandy",finos_WD1:="uca_wet"][Class_22=="beach_sand",finos_WD1:="beach_sand_uca"]
DF6[,table(finos_WD1)]
DF6[,finos_WD2:=finos_WD][Class_22=="beach_sand",finos_WD2:="beach_sand_uca"]
DF6[,table(finos_WD2)]
DF6[,finos_class1:=finos_class][Class_22=="beach_sand",finos_class1:="beach_sand_uca"]
DF6[,table(finos_class1)]

DF7<-DF6[!(cvr_sd_g=="uca_NA"|cvr_sd_g=="uca_Medium Sand")]
DF7[,cvr_sd_g:=as.character(cvr_sd_g)]
DF7[,table(cvr_sd_g)]


DF7[cvr_sd_g=="uca_NA",.(Class_22,cvr_sd_g,Island)]

ggplot(DF6,aes(x=D50_um_,fill=Sd_clss))+
  geom_histogram(binwidth=1)+
  theme_bw()


set.seed(10)
trainIndex <- createDataPartition(DF6$finos_class, p = .70, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

L1_train_uca<-DF6[trainIndex]
L1_train_uca[,table(finos_class)]

L1_val_uca<-DF6[-trainIndex]
L1_val_uca[,table(finos_class)]

###Introduce new columns on training and validation polygons
GT_c_l1_t_uca<-merge(GT_c1,L1_train_uca,by="Point",all.x=F,all.y=T)

GT_c_l1_v_uca<-merge(GT_c1,L1_val_uca,by="Point",all.x=F,all.y=T)

###Mask sat images for uca areas
uca_mask<-raster("Data_out/Habitat_classes/Level0/uca_mask_selvar.tif")
beginCluster(7)
sat_uca<-raster::mask(sat,uca_mask)
endCluster()
beep(3)
writeRaster(sat_uca,"Data_out/Stack/sat_uca.tif", overwrite=T)
sat_uca<-stack("Data_out/Stack/sat_uca.tif")
names(sat_uca)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
                  "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
                  "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","MSAVI2","intensity","iv_multi","rededge_multi","rededge_sum",
                  "visible_multi")


#### Select bands to use ####
sat_uca1<-subset(sat_uca,c("S1_20200128_VH","S1_20200128_VV","B11_20200204","rededge_sum","intensity","dem_104_469","NDMI1","NDWI","mNDWI","MSAVI2","VH_VV"))

######## Random forest class #########
start<-Sys.time()

set.seed(12)
beginCluster(7)
SC1_uca<-superClass(img=sat_uca,model="rf",trainData=GT_c_l1_t_uca,responseCol="finos_class",valData=GT_c_l1_v_uca,polygonBasedCV=F,predict=T,
                     predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif<-end-start

saveRSTBX(SC1_uca,"Data_out/models/SC1_uca",fomat="raster",overwrite=T)
SC1_uca<-readRSTBX("Data_out/models/SC1_uca")

SC1_uca$model$finalModel$importance

SC1_uca$classMapping
plot(SC1_uca$map,colNA=1,col=c("cadetblue","lightgrey","cadetblue1"))
SC1_uca_tif<-SC1_uca$map
writeRaster(SC1_uca_tif,"Data_out/models/SC1_uca_selvar.tif")

xx<-drawExtent()
subs_t<-crop(SC1_uca$map,xx)
plot(subs_t, colNA=1,col=c("cadetblue","lightgrey","cadetblue1"))





#######################################################
#####################################################################
######## Classifing bare sediment areas ##################

DF4[,table(finos_class)]
DF4[,table(finos_class1)]
DF5<-DF4[!(is.na(mud))] ## remove data without grain size analysis
DF5[,table(uca)]
DF5[,table(Final_grain_EU)]
DF5[,table(Final_grain_EU1)]

DF6_bs<-DF5[uca=="other"][,uca:=as.character(uca)] ##Database only for uca
DF6_bs[,table(uca)]
DF6_bs[,table(finos_WD)]
DF6_bs[,table(finos_class)]
DF6_bs[,table(WD)]
DF6_bs[,table(Final_grain_EU)]
DF6_bs[,table(Final_grain_EU1)]

# juntar as areas wet so numa classe
DF6_bs[,finos_WD1:=finos_WD][finos_WD=="bare_sediment_wet_muddy"|finos_WD=="bare_sediment_wet_sandy",finos_WD1:="bare_sediment_wet"][Class_22=="beach_sand",finos_WD1:="beach_sand_bare_sediment"]
DF6_bs[,table(finos_WD1)]

DF6_bs[,finos_WD2:=finos_WD][Class_22=="beach_sand"&WD=="dry",finos_WD2:="beach_sand_dry_bare_sediment"][Class_22=="beach_sand"&WD=="wet",finos_WD2:="beach_sand_wet_bare_sediment"]
DF6_bs[,table(finos_WD2)]

DF6_bs[,finos_WD3:=finos_WD][finos_WD=="bare_sediment_wet_muddy"|finos_WD=="bare_sediment_wet_sandy",finos_WD1:="bare_sediment_wet"][Class_22=="beach_sand",finos_WD1:="beach_sand_bare_sediment"]
DF6_bs[,table(finos_WD3)]

DF6_bs[,finos_class1:=finos_class][Class_22=="beach_sand",finos_class1:="beach_sand_uca"]
DF6_bs[,table(finos_class1)]

DF7<-DF6[!(cvr_sd_g=="uca_NA"|cvr_sd_g=="uca_Medium Sand")]
DF7[,cvr_sd_g:=as.character(cvr_sd_g)]
DF7[,table(cvr_sd_g)]


DF7[cvr_sd_g=="uca_NA",.(Class_22,cvr_sd_g,Island)]

ggplot(DF6,aes(x=D50_um_,fill=Sd_clss))+
  geom_histogram(binwidth=1)+
  theme_bw()


set.seed(10)
trainIndex <- createDataPartition(DF6_bs$finos_class1, p = .70, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

L1_train_bs<-DF6_bs[trainIndex]
L1_train_bs[,table(finos_class1)]

L1_val_bs<-DF6_bs[-trainIndex]
L1_val_bs[,table(finos_class1)]

###Introduce new columns on training and validation polygons
GT_c_l1_t_bs<-merge(GT_c1,L1_train_bs,by="Point",all.x=F,all.y=T)

GT_c_l1_v_bs<-merge(GT_c1,L1_val_bs,by="Point",all.x=F,all.y=T)


###Mask sat images for bare sediment areas
baresed_mask<-raster("Data_out/Habitat_classes/Level0/baresed_mask_selvar.tif")
beginCluster(7)
sat_baresed<-raster::mask(sat,baresed_mask)
endCluster()
beep(3)
writeRaster(sat_baresed,"Data_out/Stack/sat_baresed.tif",overwrite=T)
sat_baresed<-stack("Data_out/Stack/sat_baresed.tif")
names(sat_baresed)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
                      "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
                      "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","MSAVI2","intensity","iv_multi","rededge_multi","rededge_sum",
                      "visible_multi")


#### Select bands to use ####
sat_baresed1<-subset(sat_baresed,c("S1_20200128_VH","S1_20200128_VV","rededge_multi","B02_20200204","B03_20200204","B04_20200204","iv_multi","mNDWI","NDWI"))

######## Random forest class #########
start<-Sys.time()

set.seed(12)
beginCluster(7)
SC1_bs<-superClass(img=sat_baresed1,model="rf",trainData=GT_c_l1_t_bs,responseCol="finos_class1",valData=GT_c_l1_v_bs,polygonBasedCV=F,predict=T,
                    predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif<-end-start

saveRSTBX(SC1_bs,"Data_out/models/SC1_bs_finos",fomat="raster",overwrite=T)
SC1_bs<-readRSTBX("Data_out/models/SC1_bs_finos")
saveRSTBX(SC1_bs,"Data_out/models/SC1_bs_finos1",fomat="raster",overwrite=T)
SC1_bs<-readRSTBX("Data_out/models/SC1_bs_finos1")

SC1_bs$model$finalModel$importance

SC1_bs$classMapping
plot(SC1_bs$map,colNA=1,col=c("cadetblue","lightgrey","cadetblue1"))
SC1_bs_tif<-SC1_bs$map
writeRaster(SC1_bs_tif,"Data_out/models/SC1_bs1_selvar.tif")

xx<-drawExtent()
subs_t<-crop(SC1_bs$map,xx)
plot(subs_t, colNA=1,col=c("cadetblue","lightgrey","cadetblue1"))


############################################################################################
################# Dividir bare sediment em wet e dry antes da classificação ################
DF6_bs[,table(WD)]

set.seed(10)
trainIndex <- createDataPartition(DF6_bs$WD, p = .7, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

L1_train_bs_WD<-DF6_bs[trainIndex]
L1_train_bs_WD[,table(WD)]

L1_val_bs_WD<-DF6_bs[-trainIndex]
L1_val_bs_WD[,table(WD)]

###Introduce new columns on training and validation polygons
GT_c_l1_t_bs_WD<-merge(GT_c1,L1_train_bs_WD,by="Point",all.x=F,all.y=T)

GT_c_l1_v_bs_WD<-merge(GT_c1,L1_val_bs_WD,by="Point",all.x=F,all.y=T)

## sat image
sat_baresed<-stack("Data_out/Stack/sat_baresed.tif")
names(sat_baresed)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
                      "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
                      "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","MSAVI2","intensity","iv_multi","rededge_multi","rededge_sum",
                      "visible_multi")

#### Select bands to use ####
sat_baresed2<-subset(sat_baresed,c("B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","VH_VV","iv_multi","MSAVI2","rededge_sum","NDMI1","mNDWI","NDWI","dem_104_469"))

######## Random forest class #########
start<-Sys.time()

set.seed(12)
beginCluster(5)
SC1_bs_WD<-superClass(img=sat_baresed,model="rf",trainData=GT_c_l1_t_bs_WD,responseCol="WD.y",valData=GT_c_l1_v_bs_WD,polygonBasedCV=F,predict=T,
                   predType="raw",filename=NULL)
endCluster()
beep(3)
end<-Sys.time()
dif<-end-start

saveRSTBX(SC1_bs_WD,"Data_out/models/SC1_bs_WD",fomat="raster",overwrite=T)

SC1_bs_WD$model$finalModel$importance

SC1_uca$classMapping
plot(SC1_uca$map,colNA=1,col=c("cadetblue","lightgrey","cadetblue1"))
SC1_uca_tif<-SC1_uca$map
writeRaster(SC1_uca_tif,"Data_out/models/SC1_uca_selvar.tif")

xx<-drawExtent()
subs_t<-crop(SC1_uca$map,xx)
plot(subs_t, colNA=1,col=c("cadetblue","lightgrey","cadetblue1"))







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



############################################################################################
###############################################################################################
################################################################ ############################
###################### individual classification schemes #################################


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
SC1_macro<-superClass(img=sat,model="rf",trainData=GT_c_l0_t,responseCol="macro.x",valData=GT_c_l0_v,polygonBasedCV=F,predict=T,
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
beginCluster(7)
sat_rocks<-mask(sat,mask_macro_others)
endCluster()
writeRaster(sat_rocks,"Data_out/Stack/sat_rocks_valtot.tif", overwrite=T)
#sat_rocks<-stack("Data_out/Stack/sat_rocks_valtot.tif")
beep(3)

set.seed(11)
beginCluster(7)
SC1_rocks<-superClass(img=sat_rocks,model="rf",trainData=GT_c_l0_t,responseCol="rocks.x",valData=GT_c_l0_v,polygonBasedCV=F,predict=T,
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
sat_shells<-mask(sat,mask_rock_others)
#writeRaster(sat_shells,"Data_out/Stack/sat_shells_valtot.tif",overwrite=T)
sat_shells<-stack("Data_out/Stack/sat_shells_valtot.tif") #made from the remaining area after excluding macroalgae and rock areas
beep(4)

set.seed(11)
beginCluster(7)
SC1_shells<-superClass(img=sat_shells,model="rf",trainData=GT_c_l0_t,responseCol="shells.x",valData=GT_c_l0_v,polygonBasedCV=F,predict=T,
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
#plot(shells_mask, colNA=1)
writeRaster(shells_mask,"Data_out/Habitat_classes/Level0/shells_mask_valtot.tif",overwrite=T)

mask_shells_others<-shellsmap==1
mask_shells_others[mask_shells_others==0]<-NA # turn wet area (coded zero) into NA
#plot(mask_shells_others, colNA=1)
writeRaster(mask_shells_others,"Data_out/mask/mask_shells_others_valtot.tif",overwrite=T)

area_shells<-sum(shells_mask[shells_mask==1])*res(shells_mask)[1]^2*10^-6 #calculate dry area size in Km2

###############################################################################################################
##############################################################################################################
######################### remove water #####################################################################

set.seed(10)
trainIndex_WD <- createDataPartition(DF3$WD20, p = .7, 
                                      list = FALSE, 
                                      times = 1)
head(trainIndex_WD)

L0_train_WD<-DF3[trainIndex_WD]
L0_train_WD[,table(WD)]
L0_val_WD<-DF3[-trainIndex_WD]
L0_val_WD[,table(WD)]

###Introduce new columns on training and validation polygons
#L0_train1_WB<-L0_train_WB[,.(WB,Point)]

GT_c_l0_t_WD<-merge(GT_c1,L0_train_WD,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_t_WD,col="red")
#str(GT_c_l0_t@data)

#L0_val1_WB<-L0_val_WB[,.(WB,Point)]

GT_c_l0_v_WD<-merge(GT_c1,L0_val_WD,by="Point",all.x=F,all.y=T)
#plot(GT_c_l0_v_WD)
#str(GT_c_l0_v@data)

### Supervised class with rstoolbox and rf: shells
#mask_shells_others<-stack("Data_out/mask/mask_shells_others.tif")
beginCluster(7)
sat_WD_all<-mask(sat,mask_shells_others)
sat_WD_ess<-mask(sat_WD,mask_shells_others)
endCluster()
#writeRaster(sat_WD_all,"Data_out/Stack/sat_WD_all_valtot.tif",overwrite=T)
#writeRaster(sat_WD_ess,"Data_out/Stack/sat_WD_ess_valtot.tif",overwrite=T)
sat_WD_all<-stack("Data_out/Stack/sat_WD_all_valtot.tif") #made from the remaining area after excluding macroalgae, rock and shell areas
#sat_WD_ess<-stack("Data_out/Stack/sat_WD_ess_valtot.tif")
beep(2)

set.seed(1111)
beginCluster(7)
SC1_BS_WD20<-superClass(img=sat_WD_all,model="rf",trainData=GT_c_l0_t_WD,responseCol="WD20",valData=GT_c_l0_v_WD,polygonBasedCV=F,predict=T,
                        predType="raw",filename=NULL)
endCluster()
beep(3)
saveRSTBX(SC1_BS_WD20,"Data_out/models/SC1_BS_WD20",format = "raster",overwrite=T)
SC1_BS_WD20<-readRSTBX("Data_out/models/SC1_BS_WD20.tif")

plot(SC1_BS_WD20$map,col=pWD, colNA=1,main="Wet VS Dry - 20% cut, all bands")
WDmap<-SC1_BS_WD20$map
SC1_BS_WD20$classMapping

ad<-drawExtent()
adp<-crop(SC1_BS_WD20$map,ad)
plot(adp,col=pWD, colNA=1,main="Wet VS Dry - 20% cut, all bands")

###Isolating bare sediment areas
dry_mask<-WDmap==1
dry_mask[dry_mask==0]<-NA # turn wet area (coded zero) into NA
#plot(dry_mask, colNA=1)
writeRaster(dry_mask,"Data_out/Habitat_classes/dry_mask_valtot.tif",overwrite=T)

area_dry<-sum(dry_mask[dry_mask==1])*res(dry_mask)[1]^2*10^-6 #calculate bare sediment area size in Km2

##Isolating wet sediment
wet_mask<-WDmap==2
wet_mask[wet_mask==0]<-NA # turn wet area (coded zero) into NA
#plot(wet_mask, colNA=1)
writeRaster(wet_mask,"Data_out/Habitat_classes/wet_mask_valtot.tif",overwrite=T)

area_wet<-sum(wet_mask[wet_mask==1])*res(wet_mask)[1]^2*10^-6 #calculate bare sediment area size in Km2

################ using PCA ot separate wet and dry

set.seed(1111)
beginCluster(7)
SC1_PCA_WD20<-superClass(img=PCA_map,model="rf",trainData=GT_c_l0_t_WD,responseCol="WD20",valData=GT_c_l0_v_WD,polygonBasedCV=F,predict=T,
                        predType="raw",filename=NULL)
endCluster()
beep(3)
saveRSTBX(SC1_PCA_WD20,"Data_out/models/SC1_PCA_WD20",format = "raster",overwrite=T)
SC1_BS_WD20<-readRSTBX("Data_out/models/SC1_PCA_WD20")





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
#sat_bsed<-stack("Data_out/Stack/sat_bsed_valtot.tif") #Remaining area after isolating macro, rocks and shells
beep(2)
sat_bsed_uca<-sat_WD_all ##same masked area used to separate wet from dry Use allyas this mask
sat_bsed_uca<-stack("Data_out/Stack/sat_WD_all_valtot.tif") ##same masked area used to separate wet from dry Use allyas this mask

### ALL AREA (This is the best performing separation of bare sediment (including waterbody) vs uca. Used )
set.seed(1111)
beginCluster(7)
SC1_sedmnt0<-superClass(img=sat_bsed_uca,model="rf",trainData=GT_c_l0_t,responseCol="sedmnt0.y",valData=GT_c_l0_v,polygonBasedCV=F,predict=T,
                        predType="raw",filename=NULL)
endCluster()
beep(3)
saveRSTBX(SC1_sedmnt0,"Data_out/models/SC1_sedmnt0",format = "raster",overwrite=T)
beep(3)
SC1_sedmnt0<-readRSTBX("Data_out/models/SC1_sedmnt0.tif")


plot(SC1_sedmnt0$map,col=c("khaki1","grey45"), colNA=1,main="Bare Sed VS Uca")
sedmnt0map<-SC1_sedmnt0$map
SC1_sedmnt0$classMapping

ad<-drawExtent()
adp<-crop(SC1_sedmnt0$map,ad)
plot(adp,colNA=1,col=c("khaki1","grey45"),main="Bare Sed VS Uca")

###Isolating bare sediment areas
bare_sediment_mask<-sedmnt0map==1
bare_sediment_mask[bare_sediment_mask==0]<-NA # turn wet area (coded zero) into NA
#plot(bare_sediment_mask, colNA=1)
writeRaster(bare_sediment_mask,"Data_out/Habitat_classes/bare_sediment_mask_valtot.tif",overwrite=T)

area_bare_sediment<-sum(bare_sediment_mask[bare_sediment_mask==1])*res(bare_sediment_mask)[1]^2*10^-6 #calculate bare sediment area size in Km2

##Isolating uca sediment
uca_mask<-sedmnt0map==2
uca_mask[uca_mask==0]<-NA # turn wet area (coded zero) into NA
#plot(uca_mask, colNA=1)
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

