setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

packs<-c("randomForest","caret","sf","beepr","RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis","data.table","reshape2")
lapply(packs,require,character.only=T)


### Macroalgae
macro_mask<- stack("Data_out/Habitat_classes/Level0/macro_mask_valtot.tif")
plot(macro_mask, col="green",colNA=1)
xx<-drawExtent()
pp<-crop(macro_mask,xx)
plot(pp, col="green",colNA=1)



### Rocks
rocks_mask<-stack("Data_out/Habitat_classes/Level0/rocks_mask_valtot.tif")
plot(rocks_mask,col="yellow",colNA=1,add=T)
### Shells
shells_mask<-stack("Data_out/Habitat_classes/Level0/shells_mask_valtot.tif")

### Dry Bare sediment
bare_sediment_mask_dry<-stack("Data_out/Habitat_classes/bare_sediment_mask_dry_valtot.tif")

### Wet Bare sediment
bare_sediment_mask_wet<-stack("Data_out/Habitat_classes/bare_sediment_mask_wet_valtot.tif")

### Dry Uca sediment
uca_mask_dry<-stack("Data_out/Habitat_classes/uca_mask_dry_valtot.tif")

### Wet Uca sediment
uca_mask_wet<-stack("Data_out/Habitat_classes/uca_mask_wet_valtot.tif")

######################################
####OR without wet and dry
### Uca sediment (isolated from remaining area after separating bsed_mask)
uca_mask<-stack("Data_out/Habitat_classes/uca_mask_valtot.tif")

###Bare sediment without waterbody VS others
bare_sediment_mask<-stack("Data_out/mask/mask_uca_others_valtot.tif")


#############################################
#### Gathering classes of level 0
Level0_1<-overlay(macro_mask, rocks_mask,shells_mask,bare_sediment_mask,uca_mask, fun=sum)
plot(Level0_1)
Level0_2<-overlay(macro_mask, rocks_mask,shells_mask,bare_sediment_mask_dry,bare_sediment_mask_wet,uca_mask_dry,uca_mask_wet,fun=sum)