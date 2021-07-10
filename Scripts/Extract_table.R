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


packs<-c("sf","beepr","RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis","data.table","reshape2")
npacks <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(npacks)) install.packages(npacks)
#install_github("vqv/ggbiplot")
lapply(packs,require,character.only=T)

GT_c1<-readOGR("Data_out/Polygons/GT_c1.shp") ##created in script Data_cleanup_SUp_Class

sat<-stack("Data_out/Stack/Final_stack1.grd")
names(sat)
#names(sat)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
#"B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
#"NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","MSAVI2","intensity","iv_multi","rededge_multi","rededge_sum",
#"visible_multi")

## Extract values (use GT_c1, created in script Data_cleanup_SUp_Class.R)
beginCluster(7)
system.time(DF<-extract(sat,GT_c1,cellnumbers=T,df=F,factors=T,nl=26,na.rm=T))
endCluster()
beep(3)


##Remove points that do not fall into Urok area
#names(DF)<-seq_along(DF)
#DF1<-Filter(Negate(is.null), DF)
Point<-seq_along(DF)

###Unpack list and ppend it to a data frame
n<-c(colnames(DF[[1]]),colnames(GT_c1@data))
m<-matrix(data=NA,nrow=1,ncol=length(n))
colnames(m)<-n

for(i in Point) {
  m<-rbind(m,cbind(DF[i],GT_c1@data[i,]))
  print(paste(i,"done"))
}

m1<-m[-1,] ###remove first row of NAs

##Check for pixels that fall outside intertidal masked area
unique(m1[is.na(m1$B02_20200204),"Point"])

#write.table(m1,"Data_out/db/GraVSSat_db_20210114.csv",row.names=F,sep=";")

#write.table(m1,"Data_out/db/DF_extract_20210429.csv",row.names=F,sep=";")
#write.table(m1,"Data_out/db/DF_extract_20210622.csv",row.names=F,sep=";")
write.table(m1,"Data_out/db/DF_extract_20210710.csv",row.names=F,sep=";")

