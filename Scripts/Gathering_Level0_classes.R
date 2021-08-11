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


### Macroalgae
macro_mask<-raster("Data_out/Habitat_classes/Level0/mask_macro_validation_bub.tif")
#macro_mask<-raster("Data_out/Habitat_classes/Level0/mask_macro_validation.tif")
#macro_mask<- stack("Data_out/Habitat_classes/Level0/mask_macro_t_selvar.tif")
macro_mask1<-macro_mask
macro_mask1[is.na(macro_mask1)]<-0
#plot(macro_mask1,colNA=1)

#plot(macro_mask1, col="green",colNA=1)


### Rocks
rocks_mask<-raster("Data_out/Habitat_classes/Level0/mask_rocks_validation_bub.tif")
#rocks_mask<-raster("Data_out/Habitat_classes/Level0/mask_rocks_validation.tif")
#rocks_mask<-stack("Data_out/Habitat_classes/Level0/mask_rocks_t_selvar.tif")
rocks_mask1<-rocks_mask
rocks_mask1[rocks_mask1==1]<-3
rocks_mask1[is.na(rocks_mask1)]<-0

#plot(rocks_mask,col="yellow",colNA=1,add=T)
### Shells
shells_mask<-raster("Data_out/Habitat_classes/Level0/mask_shells_validation_bub.tif")
#shells_mask<-raster("Data_out/Habitat_classes/Level0/mask_shells_validation.tif")
#shells_mask<-stack("Data_out/Habitat_classes/Level0/mask_shells_t_selvar.tif")
shells_mask1<-shells_mask
shells_mask1[shells_mask1==1]<-5
shells_mask1[is.na(shells_mask1)]<-0

#### without wet and dry
### Uca sediment
uca_mask<-stack("Data_out/Habitat_classes/Level0/uca_mask_selvar.tif")
uca_mask1<-uca_mask
uca_mask1[uca_mask1==1]<-7
uca_mask1[is.na(uca_mask1)]<-0

###Bare sediment without waterbody VS others
bare_sediment_mask<-stack("Data_out/Habitat_classes/Level0/baresed_mask_selvar.tif")
bare_sediment_mask1<-bare_sediment_mask
bare_sediment_mask1[bare_sediment_mask1==1]<-9
bare_sediment_mask1[is.na(bare_sediment_mask1)]<-0


### Uca sediments
uca_seds<-raster("Data_out/models/SC1_0_mg1uca_val_bub.tif")
#uca_seds<-raster("Data_out/models/SC1_0_mg1uca_val.tif")
#uca_seds<-raster("Data_out/models/SC1_0_mg1uca.tif")
uca_seds1<-uca_seds
uca_seds1[uca_seds1==1]<-7
uca_seds1[uca_seds1==2]<-9
uca_seds1[uca_seds1==3]<-11
uca_seds1[is.na(uca_seds1)]<-0

## Bare sediment seds
bs_seds<-raster("Data_out/models/SC1_0_mg1bs_val_bub.tif")
#bs_seds<-raster("Data_out/models/SC1_0_mg1bs_val.tif")
#bs_seds<-raster("Data_out/models/SC1_0_mg1bs.tif")
bs_seds1<-bs_seds
bs_seds1[bs_seds1==1]<-13
bs_seds1[bs_seds1==2]<-15
bs_seds1[bs_seds1==3]<-17
bs_seds1[bs_seds1==4]<-19
bs_seds1[is.na(bs_seds1)]<-0




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

beginCluster()
Level0_1<-overlay(macro_mask1, rocks_mask1,shells_mask1,uca_seds1,bs_seds1, fun=sum)
Level0_1[Level0_1==0]<-NA
endCluster()
beep(3)

plot(Level0_1[Level0_1>0])
Level0_1
plot(Level0_1)
#writeRaster(Level0_1,"Data_out/models/Hierarchical_SupClass.tif",overwrite=T)
writeRaster(Level0_1,"Data_out/models/Hierarchical_SupClass_val.tif",overwrite=T)
Level0_1<-raster("Data_out/models/Level0_step_by_step_SupClass.tif")

p<-c("green","red","blue","grey50","lightgrey")

df<-data.frame(class=c("macroalgae", "rock","shell","uca_muddy_10100","uca_sandy_010_Fine Sand","uca_sandy_010_Very Fine Sand","bare_sediment_muddy_10100","bare_sediment_sandy_010_Fine Sand","bare_sediment_sandy_010_Medium Sand","bare_sediment_sandy_010_Very Fine Sand"),classID=seq(1,19,2))
df$class<-factor(df$class, levels=c("macroalgae", "rock","shell","uca_muddy_10100","uca_sandy_010_Fine Sand","uca_sandy_010_Very Fine Sand","bare_sediment_muddy_10100","bare_sediment_sandy_010_Fine Sand","bare_sediment_sandy_010_Medium Sand","bare_sediment_sandy_010_Very Fine Sand"))
#plot(Level0_1, col=p)

xx<-drawExtent()
pp<-crop(Level0_1,xx)
plot(pp, col=p)
plot(GT_c_l0_v, col="pink", add=T)

GT_c_l0_v$cover_over_f<-ifelse(GT_c_l0_v$cvr_vrA.y=="macroalgae",1,
                               ifelse(GT_c_l0_v$cvr_vrA.y=="rock",3,
                                ifelse(GT_c_l0_v$cvr_vrA.y=="shell",5,
                                ifelse(GT_c_l0_v$cvr_vrA.y=="uca",7,
                                ifelse(GT_c_l0_v$cvr_vrA.y=="bare_sediment",9,NA)))))


GT_c_l0_v_mg1$Final_finos_grad1<-factor(GT_c_l0_v_mg1$Final_finos_grad,levels=c("macroalgae", "rock","shell","uca_muddy_10100","uca_sandy_010_Fine Sand","uca_sandy_010_Very Fine Sand","bare_sediment_muddy_10100","bare_sediment_sandy_010_Fine Sand","bare_sediment_sandy_010_Medium Sand","bare_sediment_sandy_010_Very Fine Sand"))

Val_map<-validateMap(Level0_1,valData=GT_c_l0_v_mg1,responseCol="Final_finos_grad1",mode="classification",nSamples = 10000,classMapping = df)


