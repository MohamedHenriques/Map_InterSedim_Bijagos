setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

packs<-c("randomForest","caret","sf","beepr","RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis","data.table","reshape2")
lapply(packs,require,character.only=T)


### Macroalgae
macro_mask<- stack("Data_out/Habitat_classes/Level0/macro_mask_valtot.tif")
macro_mask1<-macro_mask
macro_mask1[is.na(macro_mask1)]<-0
#plot(macro_mask1,colNA=1)

#plot(macro_mask1, col="green",colNA=1)


### Rocks
rocks_mask<-stack("Data_out/Habitat_classes/Level0/rocks_mask_valtot.tif")
rocks_mask1<-rocks_mask
rocks_mask1[rocks_mask1==1]<-3
rocks_mask1[is.na(rocks_mask1)]<-0

#plot(rocks_mask,col="yellow",colNA=1,add=T)
### Shells
shells_mask<-stack("Data_out/Habitat_classes/Level0/shells_mask_valtot.tif")
shells_mask1<-shells_mask
shells_mask1[shells_mask1==1]<-5
shells_mask1[is.na(shells_mask1)]<-0

#### without wet and dry
### Uca sediment
uca_mask<-stack("Data_out/Habitat_classes/uca_mask_valtot.tif")
uca_mask1<-uca_mask
uca_mask1[uca_mask1==1]<-7
uca_mask1[is.na(uca_mask1)]<-0

###Bare sediment without waterbody VS others
bare_sediment_mask<-stack("Data_out/Habitat_classes/bare_sediment_mask_valtot.tif")
bare_sediment_mask1<-bare_sediment_mask
bare_sediment_mask1[bare_sediment_mask1==1]<-9
bare_sediment_mask1[is.na(bare_sediment_mask1)]<-0

######################################
### Dry Bare sediment
bare_sediment_mask_dry<-stack("Data_out/Habitat_classes/bare_sediment_mask_dry_valtot.tif")
bare_sediment_mask_dry1<-bare_sediment_mask_dry
bare_sediment_mask_dry1[bare_sediment_mask_dry1==1]<-11
bare_sediment_mask_dry1[is.na(bare_sediment_mask_dry1)]<-0

### Wet Bare sediment
bare_sediment_mask_wet<-stack("Data_out/Habitat_classes/bare_sediment_mask_wet_valtot.tif")
bare_sediment_mask_wet1<-bare_sediment_mask_wet
bare_sediment_mask_wet1[bare_sediment_mask_wet1==1]<-13
bare_sediment_mask_wet1[is.na(bare_sediment_mask_wet1)]<-0

### Dry Uca sediment
uca_mask_dry<-stack("Data_out/Habitat_classes/uca_mask_dry_valtot.tif")
uca_mask_dry1<-uca_mask_dry
uca_mask_dry1[uca_mask_dry1==1]<-15
uca_mask_dry1[is.na(uca_mask_dry1)]<-0

### Wet Uca sediment
uca_mask_wet<-stack("Data_out/Habitat_classes/uca_mask_wet_valtot.tif")
uca_mask_wet1<-uca_mask_wet
uca_mask_wet1[uca_mask_wet1==1]<-17
uca_mask_wet1[is.na(uca_mask_wet1)]<-0

#############################################
#### Gathering classes of level 0

Level0_1<-overlay(macro_mask1, rocks_mask1,shells_mask1,uca_mask1,bare_sediment_mask1, fun=sum)
Level0_1[Level0_1==0]<-NA
beep(3)
plot(Level0_1[Level0_1>0])
Level0_1
plot(Level0_1)
writeRaster(Level0_1,"Data_out/models/Level0_step_by_step_SupClass.tif",overwrite=F)
Level0_1<-raster("Data_out/models/Level0_step_by_step_SupClass.tif")

p<-c("green","red","blue","grey50","lightgrey")

df<-data.frame(class=c("macroalgae", "rock","shell","uca","bare_sediment"),classID=seq(1,9,2))
plot(Level0_1, col=p)

xx<-drawExtent()
pp<-crop(Level0_1,xx)
plot(pp, col=p)
plot(GT_c_l0_v, col="pink", add=T)

GT_c_l0_v$cover_over_f<-ifelse(GT_c_l0_v$covr_vrA=="macroalgae",1,
                               ifelse(GT_c_l0_v$covr_vrA=="rock",3,
                                ifelse(GT_c_l0_v$covr_vrA=="shell",5,
                                ifelse(GT_c_l0_v$covr_vrA=="uca",7,
                                ifelse(GT_c_l0_v$covr_vrA=="bare_sediment",9,NA)))))

Val_map<-validateMap(Level0_1,valData=GT_c_l0_v,responseCol="cover_over_f",mode="classification",nSamples = 500)





Level0_2<-overlay(macro_mask1, rocks_mask1,shells_mask1,bare_sediment_mask_dry1,bare_sediment_mask_wet1,uca_mask_dry1,uca_mask_wet1,fun=sum)


