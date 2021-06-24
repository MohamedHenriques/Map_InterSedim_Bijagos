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


packs<-c("foreach","doParallel","randomForest","caret","sf","beepr","RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis","data.table","reshape2")
npacks <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(npacks)) install.packages(npacks)
#install_github("vqv/ggbiplot")
lapply(packs,require,character.only=T)

p<-c("green","red","blue","grey50","lightgrey")

## Load sat img

sat<-stack("Data_out/Stack/Final_stack.tif") ##created in script GraVSSat_Preliminary
names(sat)<-names(sat)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
                          "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
                          "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","intensity","iv_multi","rededge_multi","rededge_sum",
                          "visible_multi")

sat_sel_tot<-sat[[-9]]
names(sat_sel_tot)

sat_sel_tot1<-sat[[-c(9,10,15:17,21:26)]]
sat_sel_tot2<-sat[[-c(1:9,10,15:17,21,24,26)]]

### PCA for all variables
table(is.na(sat))
beginCluster(7)
system.time(
  PCA_tot<-rasterPCA(sat_sel_tot,spca=T,nsnSamples = NULL,maskCheck=F)
)
endCluster()
beep(3)

saveRSTBX(PCA_tot,"Data_out/PCA/PCA_tot",format="raster",overwrite=T)
PCA_tot<-readRSTBX("Data_out/PCA/PCA_tot.tif")
PCA_tot_map<-PCA_tot$map
plot(PCA_tot_map[[1]])
summary(PCA_tot$model)


### PCA for sel variables 1

beginCluster(7)
system.time(
  PCA_tot_sel1<-rasterPCA(sat_sel_tot1,spca=T,nsnSamples = NULL,maskCheck=F)
)
endCluster()
beep(3)

saveRSTBX(PCA_tot_sel1,"Data_out/PCA/PCA_tot_sel1",format="raster",overwrite=T)
PCA_tot_sel1_map<-PCA_tot_sel1$map
plot(PCA_tot_sel1[[1]])
summary(PCA_tot_sel1$model)

### PCA for sel variables 2

beginCluster(7)
system.time(
  PCA_tot_sel2<-rasterPCA(sat_sel_tot2,spca=T,nsnSamples = NULL,maskCheck=F)
)
endCluster()
beep(3)

saveRSTBX(PCA_tot_sel2,"Data_out/PCA/PCA_tot_sel2",format="raster",overwrite=T)
PCA_tot_sel2_map<-PCA_tot_sel2$map
plot(PCA_tot_sel2[[1]])
summary(PCA_tot_sel2$model)




### subset sat stack to select bands to preform a pca aiming at separating wet and dry areas
sel_WD<-c("B08_20200204","B08A_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
          "NDWI","mNDWI","NDMI","NDMI1","VH_VV")
sat_WD<-raster::subset(sat,sel_WD)
names(sat_WD)

### PCA to seperate wet and dry areas
#table(is.na(sat_WD))
beginCluster(7)
system.time(
  PCA_WD<-rasterPCA(sat_WD,spca=T,nsnSamples = NULL,maskCheck=F)
)
endCluster()
beep(3)

saveRSTBX(PCA_WD,"Data_out/PCA/PCA_WD",format="raster",overwrite=F)
PCA_map<-PCA_WD$map
plot(PCA_map[[1]])
summary(PCA_WD$model)