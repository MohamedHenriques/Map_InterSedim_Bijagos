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
packs<-c("raster","devtools","ggbiplot","caret","sf","beepr","ggplot2","viridis","data.table","mixtools","RStoolbox","rgdal","nplr")
npacks <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(npacks)) install.packages(npacks)
#install_github("vqv/ggbiplot")
lapply(packs,require,character.only=T)


grafpdf<-T

if(grafpdf) pdf("teste.pdf", width=11, height=8)

## Load sat imgs
bandas_iv_bub<-list.files("C:/Doutoramento1/Capitulos/Mapping_intertidal_sediments/Images_for_DEM/Bubaque/B8", pattern=".tif", full.names = TRUE)
iv_bub1<-stack(bandas_iv_bub)

bandas_iv_bol<-list.files("C:/Doutoramento1/Capitulos/Mapping_intertidal_sediments/Images_for_DEM/Bolama/B8", pattern=".tif", full.names = TRUE)
iv_bol1<-stack(bandas_iv_bol)

#plot(iv_bub1, colNA=1)
#plot(iv_bol1, colNA=1)

## Convert to true reflectances
iv_bub<-iv_bub1/10000
rm(iv_bub1)
writeRaster(iv_bub,"Data_out/DEM/sat_img_corr/stack_iv_bub.grd",format="raster")
iv_bub<-stack("Data_out/DEM/sat_img_corr/stack_iv_bub.grd")

iv_bol<-iv_bol1/10000
rm(iv_bol1)
writeRaster(iv_bol,"Data_out/DEM/sat_img_corr/stack_iv_bol.grd",format="raster")
iv_bol<-stack("Data_out/DEM/sat_img_corr/stack_iv_bol.grd")

##extract values of reference band for regression (excluding intertidal area)
banda_ref<-14
r_ref<-iv_bub[[banda_ref]]
vr_ref_bub<-getValues(r_ref)

sel_bub<-which(vr_ref_bub<.05|vr_ref_bub>.2) ## This excludes intertidal area from the selection
x_bub<-vr_ref_bub[sel_bub]


## Image inter-calibration
beginCluster()
start<-Sys.time()
#z<-c(1:3)
for (z in (1:3))
  
{
  #par(mfrow=c(2,3))
  print(paste(z,"started",sep=" "))
  b<-iv_bub[[z]]
  y<-getValues(b)[sel_bub]
  
  l1<-lm(y~x_bub)
  
  #abline(l1)
  
  l2<-lm(x_bub~y)
  
  #abline(-coefficients(l2)[1]/coefficients(l2)[2], 1/coefficients(l2)[2], col=1)
  #plot(r_ref, main=names(iv)[17], zlim=c(0,.4))
  #plot(b, zlim=c(0,.4), main=paste("File", names(iv)[z], "-uncorrected"))
  
  inter=(coefficients(l1)[1]-coefficients(l2)[1]/coefficients(l2)[2])/2
  slope=(coefficients(l1)[2]+1/coefficients(l2)[2])/2
  
  #plot(b*(1/slope)-inter/slope, zlim=c(0, 0.4), main=paste("File", names(iv)[z], "-corrected"))
  #obj<-b*(1/slope)-inter/slope
  rm(l1)
  rm(l2)
  for (i in 1:3) {gc()}
  
  writeRaster(b*(1/slope)-inter/slope,filename=paste("Data_out/DEM/bands_intercalibrated/",names(iv_bub)[z],"_corrected.tif",sep=""))
  
  #abline(inter, slope, col=2)
  #plot(r_ref,b)
  #abline(0,1, lty=2)
  #legend("toplef", legend=c("Line of equal values", "Correction model"), lty=c(2,1), col=c(1,2), bty="n", cex=1)
  #plot(r_ref, b*(1/slope)-inter/slope, main=paste("File", names(iv)[z], "-uncorrected"))
  #abline(0,1, lty=2)
  #legend("toplef", legend=c("Line of equal values", "Correction model"), lty=c(2,1), col=c(1,2), bty="n", cex=1)
  print(paste(z,"ended",sep=""))
}

#if(grafpdf)dev.off()
end<-Sys.time()
endCluster()
time<-end-start
time


system.time(b*(1/slope)-inter/slope)

beginCluster()
system.time(
  clusterR(b,fun=function(x,slope,inter){return(x*(1/slope)-inter/slope)},args=list(slope=slope,inter=inter),progress="text")
)
endCluster()


