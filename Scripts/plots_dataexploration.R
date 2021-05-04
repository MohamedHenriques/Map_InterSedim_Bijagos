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
packs<-c("devtools","ggbiplot","caret","sf","beepr","ggplot2","viridis","data.table","reshape2","corrplot","PerformanceAnalytics","Hmisc")
npacks <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(npacks)) install.packages(npacks)
#install_github("vqv/ggbiplot")

lapply(packs,require,character.only=T)

##load extract data
m1<-fread("Data_out/db/DF_extract_20210429.csv") # created in script Sat_image_stack.R
str(m1)
m1[,c_uca:=as.numeric(c_uca)]

## create subset of columns
m2<-m1[,c(2:28,34:39,49:52,57:61,63,66,70:77,81)]
str(m2)

table(is.na(m2))
m2[,table()]



##################################################################################################
#####################################################################################################
###plots for uca

melt1<-melt(m2,id=c("cvr_sd_f","cvr_sd_g","WD","c_uca"),measure=c(10:26,28,33,38:42),variable.factor=F,value.factor=F)
str(melt1)
melt1[,table(variable)]
melt1[,table(cvr_sd_f)]

ggplot(melt1,aes(x=c_uca,y=value))+
  #geom_point(position = position_jitter())+
  stat_summary(na.rm=T, col="red")+
  stat_smooth(na.rm=T, method="gam")+
  facet_wrap(.~variable,scales="free")+
  theme_bw()







## subset database for a PCA focused on separating water from dry areas (column WD, 30% cut)
m3<-m2[,.(NDWI,mNDWI,NDMI,NDMI1,B11_20200204,B12_20200204,S1_20200128_VH,S1_20200128_VV,mud,Sand,WD)]
str(m3)
m4<-na.omit(m3)
str(m4)
m4[,table(WD)]


##run PCA analysis
m4_pca<-prcomp(m4[,-11],center=T,scale=T)
summary(m4_pca)

##plot PCA

ggbiplot(m4_pca,choices=1:2,ellipse=T,groups=m4$WD,varname.size=5,alpha=.4,var.axes = T)+
  theme_bw()+
  labs(colour="Wet VS Dry (30% cut)")

#stat_ellipse(lwd=1,level=.6,aes(group=m3$cover_over,colour=m3$cover_over))


##sample database to reduce size
set.seed(0)
m3<-m2[sample(1:nrow(m2),20000,replace=F),]
m4<-m3[,-c(19:21)]
m5<-m2[,-c(35:40)]





##run PCA analysis
m4_pca<-prcomp(m4,center=T,scale=T)
summary(m4_pca)

m5_pca<-prcomp(m5,center=T,scale=T)
summary(m5_pca)

##plot PCA

ggbiplot(m4_pca,ellipse=T,groups=m3,varname.size=5,alpha=.4,var.axes = T)+
  theme_bw()+
  labs(colour="Wet VS Dry (20% cut)")
  
  #stat_ellipse(lwd=1,level=.6,aes(group=m3$cover_over,colour=m3$cover_over))


##Compute general correlations between numerical variables
num<-names(m5)[sapply(m5, is.numeric)]
dbc<-m5[,c(num),with=F]
dbc1<-sapply(dbc, as.numeric)
res<-rcorr(as.matrix(dbc))
resr<-round(res$r,2)
resp<-round(res$P,2)

# Insignificant correlation are crossed
corrplot(resr,type="upper",order="hclust",p.mat=resp,sig.level=0.05,insig="pch")



