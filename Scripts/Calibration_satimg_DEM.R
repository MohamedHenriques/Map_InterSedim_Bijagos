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



##Load/install packages. In case the PC used does not have the packages installed
packs<-c("lmodel2","raster","devtools","ggbiplot","caret","sf","beepr","ggplot2","viridis","data.table","mixtools","RStoolbox","rgdal","nplr")
npacks <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(npacks)) install.packages(npacks)
#install_github("vqv/ggbiplot")
lapply(packs,require,character.only=T)


# grafpdf<-T
# 
# if(grafpdf) pdf("teste.pdf", width=11, height=8)


####cut sat images to their intersection

caminhos_bub<-#(um nome para cada data)
caminhos_bol<-#(um nome para cada data)

##Load S2 images 20200204
for(i in 1:length(caminhos)){
  path_bub<-caminhos_bub[i]
  files_bub<-list.files(path=path_bub,pattern=".tif",full.names = T)
  S2_bub<-raster(files_bub)
  #plot(S2_bub[[1]])
  names(S2_bub)
  
  path_bol<-caminhos_bol[i]
  files_bol<-list.files(path=path_bol,pattern=".tif",full.names = T)
  S2_bol<-raster(files_bol)
  names(S2_bol)
  #plot(S2_bol)
  
  ##extract common area of the 2scenes and use it to cut all the other bands
  crop_area<-raster::intersect(S2_bub[[1]],S2_bol[[1]])
  #plot(crop_area)
  
  S2_bub_c<-crop(S2_bub,crop_area)
  #plot(S2_bub_c[[1:4]])
  names(S2_bub_c)
  
  S2_bol_c<-crop(S2_bol,crop_area)
  #plot(S2_bol_c[[1:4]])
  names(S2_bol_c)
  
  extent(S2_bub_c)==extent(S2_bol_c)
  
  
  #z<-3
  vr_ref<-getValues(S2_bub_c)
  
  print(paste(i),"calibration begining",sep=" ")
  S2_bol_c
  y<-getValues(S2_bol_c)

  lmod<-lmodel2(y~vr_ref)
  slope<-lmod[[3]][3,3]
  inter<-lmod[[3]][3,2]
  print(paste("Method lmodel2 -> Slope =", lmod[[3]][3,3], "; inters =", lmod[[3]][3,2]))
    
    
  writeRaster(S2_bol*(1/slope)-inter/slope,filename=paste("Data_out/sat_bol_calibrated/",names(S2_bol),"_calibrated.tif",sep=""))
  for (i in 1:3) {gc()}
  print(paste(i,"ended",sep=" "))
  
}  

path_bub<-"C:/Doutoramento1/Capitulos/Mapping_intertidal_sediments/Satellite_images/Sentinel2/Resampled/Bubaque/im_20200204/2A"
files_bub<-list.files(path=path_bub,pattern=".tif",full.names = T)
S2_20200204_bub<-stack(files_bub[-9])
#plot(S2_20200204_bub[[1]])
names(S2_20200204_bub)

path_bol<-"C:/Doutoramento1/Capitulos/Mapping_intertidal_sediments/Satellite_images/Sentinel2/Resampled/Bolama/im_20200204/2A"
files_bol<-list.files(path=path_bol,pattern=".tif",full.names = T)
S2_20200204_bol<-stack(files_bol[-9])
names(S2_20200204_bol)
#plot(S2_20200204_bol)

##extract common area of the 2scenes and use it to cut all the other bands
crop_area<-raster::intersect(S2_20200204_bub[[1]],S2_20200204_bol[[1]])
#plot(crop_area)

S2_20200204_bub_c<-crop(S2_20200204_bub,crop_area)
#plot(S2_20200204_bub_c[[1:4]])
names(S2_20200204_bub_c)

S2_20200204_bol_c<-crop(S2_20200204_bol,crop_area)
#plot(S2_20200204_bol_c[[1:4]])
names(S2_20200204_bol_c)

extent(S2_20200204_bub_c)==extent(S2_20200204_bol_c)

  
z<-3
vr_ref<-getValues(S2_20200204_bub_c)

for (z in (1:nlayers(S2_20200204_bub_c))){
  #par(mfrow=c(2,3))
  print(z)
  b<-S2_20200204_bol_c[[z]]
  y<-getValues(b)
  # l1<-lm(y~vr_ref[,1])
  # abline(l1)
  # 
  # l2<-lm(vr_ref[,1]~y)
  # #abline(-coefficients(l2)[1]/coefficients(l2)[2], 1/coefficients(l2)[2], col=1)
  # 
  # #plot(r_ref, main=names(iv)[17], zlim=c(0,.4))
  # #plot(b, zlim=c(0,.4), main=paste("File", names(iv)[z], "-uncorrected"))
  # inter1=(coefficients(l1)[1]-coefficients(l2)[1]/coefficients(l2)[2])/2
  # slope1=(coefficients(l1)[2]+1/coefficients(l2)[2])/2
          
  lmod<-lmodel2(y~vr_ref[,z])
  slope<-lmod[[3]][3,3]
  inter<-lmod[[3]][3,2]
  #print(paste("Method JPG -> Slope =", slope, "; inters =", inter))
  print(paste("Method lmodel2 -> Slope =", lmod[[3]][3,3], "; inters =", lmod[[3]][3,2]))

  
  writeRaster(S2_20200204_bol[[z]]*(1/slope)-inter/slope,filename=paste("Data_out/sat_bol_calibrated/",names(S2_20200204_bol)[z],"_calibrated.tif",sep=""))
  for (i in 1:3) {gc()}
  #abline(inter, slope, col=2)
  #plot(r_ref,b)
  #abline(0,1, lty=2)
  #legend("toplef", legend=c("Line of equal values", "Correction model"), lty=c(2,1), col=c(1,2), bty="n", cex=1)
  #plot(r_ref, b*(1/slope)-inter/slope, main=paste("File", names(iv)[z], "-uncorrected"))
  #abline(0,1, lty=2)
  #legend("toplef", legend=c("Line of equal values", "Correction model"), lty=c(2,1), col=c(1,2), bty="n", cex=1)
  print(paste(z,"ended",sep=" "))
}

# img1<-S2_20200204_bol[[1]]*(1/slope)-inter/slope
# img2<-S2_20200204_bol[[2]]*(1/slope)-inter/slope
# img3<-S2_20200204_bol[[3]]*(1/slope)-inter/slope
# 
# img<-stack(img1,img2,img3)
# writeRaster(img,"Data_out/img_test_RGB.tif")
# ggRGB(img,3,2,1,stretch = "lin")+ggtitle("new")
# ggRGB(S2_20200204_bol,3,2,1,stretch = "lin")+ggtitle("old")
# ggRGB(S2_20200204_bub,3,2,1,stretch = "lin")+ggtitle("ref")
# 
# 
# test<-merge(S2_20200204_bub[[1:3]],img[[1:3]],overlap=T)
# names(test)<-names(S2_20200204_bub)[1:3]
# ggRGB(test,3,2,1,stretch = "lin")+ggtitle("merged")


sel<-sample(1:length(y),5000)

plot(y[sel],vr_ref[sel,1])
abline(0,1)

points(y[sel],vr_ref[sel,1]*(1/slope)-inter/slope,col=2)




