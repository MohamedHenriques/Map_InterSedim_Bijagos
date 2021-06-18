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
packs<-c("pca3d","devtools","ggbiplot","caret","sf","beepr","ggplot2","viridis","data.table","reshape2","corrplot","PerformanceAnalytics","Hmisc")
npacks <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(npacks)) install.packages(npacks)
#install_github("vqv/ggbiplot")
lapply(packs,require,character.only=T)

##load extract data
m1<-fread("Data_out/db/DF_extract_20210429.csv") # created in script Sat_image_stack.R
str(m1)
m1[,c_uca:=as.numeric(c_uca)]

## create subset of columns
m2<-m1[,c(2:28,34:39,46,49:52,57:61,63,66,70:77,81)]
str(m2)

table(is.na(m2))

m2[,MSAVI2:=(2*B08_20200204+1-sqrt((2*B08_20200204+1)^2-8*(B08_20200204-B04_20200204)))/2]

m2[(WD=="dry"&cvr_vrA=="bare_sediment"),table(cvr_sd_g)]
m2[(WD=="wet"&cvr_vrA=="bare_sediment"),table(cvr_sd_g)]
m2[(WD=="dry"&cvr_vrA=="uca"),table(cvr_sd_g)]
m2[(WD=="wet"&cvr_vrA=="uca"),table(cvr_sd_g)]
m2[,table(uca)]

order(m2[uca=="uca",unique(mud)])

## Sediment classes as defined by EU (2019)
m2[,Grain_EU:=ifelse(mud<10,"sand_010",ifelse(mud>=10&mud<25,"muddy_sand_1025",ifelse(mud>=25&mud<60,"mixed_2560",ifelse(mud>=60&mud<=100,"muddy_60100",NA))))][,Final_grain_EU:=ifelse(cvr_vrA=="bare_sediment"|cvr_vrA=="uca",paste(cvr_vrA,Grain_EU,sep="_"),ifelse(cvr_vrA=="macroalgae","macroalgae",ifelse(cvr_vrA=="rock","rock",ifelse(cvr_vrA=="shell","shell",NA))))]
m2[,Final_grain_EU1:=Final_grain_EU][Final_grain_EU=="bare_sediment_muddy_sand_1025",Final_grain_EU1:="bare_sediment_mixed_2560"][Final_grain_EU=="uca_muddy_sand_1025",Final_grain_EU1:="uca_mixed_2560"][Final_grain_EU1=="bare_sediment_mixed_2560",Final_grain_EU1:="bare_sediment_mixed_1060"][Final_grain_EU1=="uca_mixed_2560",Final_grain_EU1:="uca_mixed_1060"]
m2[,.(table(Final_grain_EU))]
m2[,.(table(Final_grain_EU1))]
m2[mud==10,.(Final_grain_EU,Final_grain_EU1,mud)]

m2[,Grain_EU1:=ifelse(mud<10,"sand_010",ifelse(mud>=10&mud<25,"muddy_sand_1025",ifelse(mud>=25&mud<=100,"muddy_25100",NA)))][,Final_grain_EU2:=ifelse(cvr_vrA=="bare_sediment"|cvr_vrA=="uca",paste(cvr_vrA,Grain_EU1,sep="_"),ifelse(cvr_vrA=="macroalgae","macroalgae",ifelse(cvr_vrA=="rock","rock",ifelse(cvr_vrA=="shell","shell",NA))))]
m2[,.(table(Final_grain_EU2))]

## Sediment class as used by Belo in MSc Thesis
m2[,finos_class:=ifelse(mud<10,"sandy_010",ifelse(mud>=10&mud<=100,"muddy_10100",NA))]
m2[!(is.na(mud)),table(finos_class)]

m2[,Final_finos_class:=paste(cvr_vrA,finos_class,sep="_")][cvr_vrA=="macroalgae"|cvr_vrA=="rock"|cvr_vrA=="shell",Final_finos_class:=cvr_vrA]
m2[,table(Final_finos_class)]



## Sediment classes as defined by Folk1970 and redrawn by Beninger2018
m2[,FolkBeninger:=ifelse(mud<10,"sand_010",ifelse(mud>=10&mud<50,"muddy_sand_1050",ifelse(mud>=50&mud<90,"sandy_mud_5090",ifelse(mud>=90&mud<=100,"mud_90100",NA))))]
m2[,table(FolkBeninger)]

m2[,Final_FolkBeninger:=paste(cvr_vrA,FolkBeninger,sep="_")][cvr_vrA=="macroalgae"|cvr_vrA=="rock"|cvr_vrA=="shell",Final_FolkBeninger:=cvr_vrA]
m2[,table(Final_FolkBeninger)]

## Sediment classes as defined by Folk1970 and redrawn by Beninger2018
m2[,FolkBeninger1:=ifelse(mud<10,"sand_010",ifelse(mud>=10&mud<90,"mixed_1090",ifelse(mud>=90&mud<=100,"mud_90100",NA)))]
m2[,table(FolkBeninger1)]

m2[,Final_FolkBeninger1:=paste(cvr_vrA,FolkBeninger1,sep="_")][cvr_vrA=="macroalgae"|cvr_vrA=="rock"|cvr_vrA=="shell",Final_FolkBeninger1:=cvr_vrA]
m2[,table(Final_FolkBeninger1)]

## Sediment classes as defined by Folk1970 and redrawn by Beninger2018
m2[,FolkBeninger2:=ifelse(mud<50,"sandy_0_50",ifelse(mud>=50&mud<=100,"mixed_50_100",NA))]
m2[,table(FolkBeninger2)]

m2[,Final_FolkBeninger2:=paste(cvr_vrA,FolkBeninger2,sep="_")][cvr_vrA=="macroalgae"|cvr_vrA=="rock"|cvr_vrA=="shell",Final_FolkBeninger2:=cvr_vrA]
m2[,table(Final_FolkBeninger2)]

## Sediment classes as defined by Folk1970 and redrawn by Beninger2018
m2[,FolkBeninger3:=ifelse(mud<10,"sand_010",ifelse(mud>=10&mud<50,"muddy_sand_1050",ifelse(mud>=50&mud<=100,"muddy_50100",NA)))]
m2[,table(FolkBeninger3)]

m2[,Final_FolkBeninger3:=paste(cvr_vrA,FolkBeninger3,sep="_")][cvr_vrA=="macroalgae"|cvr_vrA=="rock"|cvr_vrA=="shell",Final_FolkBeninger3:=cvr_vrA]
m2[,table(Final_FolkBeninger3)]

m2[,covr_vrA6:=cvr_vrA][covr_vrA6=="bare_sediment",covr_vrA6:="sediments"][covr_vrA6=="uca",covr_vrA6:="sediments"]
m2[,table(covr_vrA6)]

write.table(m2,"Data_out/db/DF_extract_20210614.csv",row.names=F,sep=";")

##################################################################################################
#####################################################################################################
m2_ad<-m2[Island=="Adonga"]
m2_rest<-m2[!Island=="Adonga"]


ggplot(m2[!(is.na(mud)|is.na(Sd_cls1))],aes(x=mud, fill=Sd_cls1))+
  geom_histogram(col="white",binwidth=.9)+
  #stat_summary()+
  theme_bw()+
  facet_grid(~uca)+
  scale_x_continuous(breaks=seq(0,100,10))+
  labs(title="exposed sediments")

ggplot(m2[!(is.na(mud)|is.na(Sd_cls1))],aes(x=Mn_fw_p,fill=Sd_cls1))+
  geom_histogram(binwidth=.02,col="white")+
  #stat_summary()+
  theme_bw()+
  #geom_vline(xintercept=median(m2[!(is.na(mud)|is.na(Sd_cls1)),Mn_fw_p]),col="blue",lwd=1.2)+
  #geom_vline(xintercept=mean(m2[!(is.na(mud)|is.na(Sd_cls1)),Mn_fw_p]),col="red",lwd=1.2)+
  facet_grid(~uca)+
  #scale_x_continuous(breaks=seq(100,400,25))+
  labs(title="exposed sediments, mean grain size distribution (phi)")


ggplot(m2[cvr_vrA=="bare_sediment"|cvr_vrA=="uca"],aes(x=Mn_fw_m,y=intensity, col=cvr_sd_f))+
  #geom_point()+
  stat_summary(na.rm=T,pch=17,size=1,alpha=.5)+
  theme_bw()+
  #scale_x_continuous(breaks=seq(0,4,0.1))+
  scale_x_continuous(breaks=seq(0,1000,50))+
  #scale_x_continuous(breaks=seq(90,450,10))+
  #scale_y_continuous(breaks=seq(60,210,10),limits = c(50,210))+
  #scale_y_continuous(breaks=seq(600,2300,50),limits = c(600,2300))+
  labs(title="Bare_sediment areas")

ggplot()

###plots for uca

melt1<-melt(m2,id=c("cvr_sd_f","cvr_sd_g","Class_22","WD","c_uca","D50_um_","Mn_fw_p","Mn_fw_m","c_mcrph","c_water","c_rocks","c_shlls","uca"),measure=c(7:8,10:26,28,33,38:42),variable.factor=F,value.factor=F)
str(melt1)
melt1[,table(variable)]
melt1[,table(cvr_sd_f)]
melt1[,table(Class_22)]

### All types of sediment together
ggplot(melt1,aes(x=c_rocks,y=value))+
  stat_summary(fun.y = mean,aes(col=WD,group=WD),
               fun.ymin = function(x) mean(x) - sd(x), 
               fun.ymax = function(x) mean(x) + sd(x), 
               geom = "pointrange",na.rm=T,position = position_jitter()) +
  #stat_summary(fun=median,na.rm=T, col="green")+
  stat_summary(fun=mean,na.rm=T, geom="line",aes(col=WD,group=WD))+
  stat_summary(fun.y = mean,col=1,
               fun.ymin = function(x) mean(x) - sd(x), 
               fun.ymax = function(x) mean(x) + sd(x), 
               geom = "pointrange",na.rm=T,position = position_jitter()) +
  stat_summary(fun=mean,na.rm=T, geom="line",col=1,lty=2)+
  #stat_smooth(na.rm=T, method="gam")+
  facet_wrap(.~variable,scales="free")+
  labs(x="shells cover (%)",title="Green=median; Red=mean+-SE")+
  scale_x_continuous(breaks=seq(0,100,10))+
  theme_bw()

##wet VS dry



melt1_uca<-melt1[!(uca=="other"|cvr_sd_f=="macroalgae"|cvr_sd_f=="rock"|cvr_sd_f=="shell")]

melt1_bs<-melt1[!(uca=="uca"|cvr_sd_f=="macroalgae"|cvr_sd_f=="rock"|cvr_sd_f=="shell")]
melt1_bs_bs<-melt1_bs[cvr_sd_f=="bare_sediment_beach_sand"]
melt1_bs_bs[,table(cvr_sd_f=="bare_sediment_beach_sand")]
melt1_bs_bs[WD=="dry",table(WD)]

ggplot(melt1_bs,aes(x=Mn_fw_p,fill=cvr_sd_g))+
  geom_density()

ggplot(melt1_uca,aes(x=D50_um_,fill=cvr_sd_g))+
  geom_density()


ggplot(melt1_bs[WD=="dry"],aes(x=Mn_fw_p,y=value,col=cvr_sd_f))+
  stat_summary(fun=median,na.rm=T, col="green")+
  stat_summary(na.rm=T)+
  labs(x="D50_um",title="bare_sediment dry")+
  facet_wrap(.~variable,scales="free")+
  stat_smooth(na.rm=T, method="gam")+
  #scale_x_continuous(breaks=seq(0,100,10))+
  theme_bw()
  
ggplot(melt1_bs[WD=="dry"],aes(x=Mn_fw_p,y=cvr_sd_g,col=cvr_sd_g))+
  stat_summary(na.rm=T,size=1)+
  stat_summary(fun=median,na.rm=T, col="green",size=1,shape=17)+
  geom_point(col="lightgrey",size=2)+
  labs(x="D50_um",title="bare_sediment dry")+
  #facet_wrap(.~variable,scales="free")+
  #stat_smooth(na.rm=T, method="gam")+
  #scale_x_continuous(breaks=seq(0,100,10))+
  theme_bw()

###Only for sediment
ggplot(melt1[!(cvr_sd_f=="macroalgae"|cvr_sd_f=="rock"|cvr_sd_f=="shell")],aes(x=c_uca,y=value))+
  #geom_point(position = position_jitter())+
  stat_summary(na.rm=T, col="red")+
  stat_summary(fun=median,na.rm=T, col="green")+
  stat_smooth(na.rm=T, method="gam",formula=y ~ s(x, bs = "cs",k=6))+
  facet_wrap(.~variable,scales="free")+
  labs(x="Uca cover (%)",title="Only  beach sand. Green=median; Red=mean+-SE")+
  scale_x_continuous(breaks=seq(0,100,10))+
  theme_bw()

###Excepting beach sand
ggplot(melt1[!(cvr_sd_f=="bare_sediment_beach_sand"|cvr_sd_f=="uca_beach_sand")],aes(x=c_uca,y=value))+
  #geom_point(position = position_jitter())+
  stat_summary(na.rm=T, col="red")+
  stat_summary(fun=median,na.rm=T, col="green")+
  stat_smooth(na.rm=T, method="gam",formula=y ~ s(x, bs = "cs",k=8))+
  facet_wrap(.~variable,scales="free")+
  labs(x="Uca cover (%)",title="Without  beach sand. Green=median; Red=mean+-SE")+
  scale_x_continuous(breaks=seq(0,100,10))+
  theme_bw()

### Per cover
melt2<-melt1[!(cvr_sd_f=="macroalgae"|cvr_sd_f=="rock"|cvr_sd_f=="shell")][variable=="c_water"|variable=="dem_104_469"|variable=="Mn_fw_m"|variable=="mud"|variable=="mNDWI"|variable=="NDMI1"|variable=="rededge_sum"|variable=="RVI"|variable=="S1_20200128_VH"|variable=="S1_20200128_VV"|variable=="VH_VV"]
str(melt2)
melt2[,table(cvr_sd_f)]
melt2[,table(variable)]
melt2[,table(Class_22)]
melt2[,table(c_uca)]

ggplot(melt2[Class_22=="muddy_sand"|Class_22=="sand"|Class_22=="sandy_mud"],aes(x=c_uca,y=value))+
  #geom_point(position = position_jitter())+
  stat_summary(na.rm=T, col="red")+
  stat_summary(fun=median,na.rm=T, col="green")+
  stat_smooth(na.rm=T, method="gam",formula=y ~ s(x, bs = "cs",k=5))+
  facet_wrap(~variable,scales="free")+
  labs(x="Uca cover (%)",title="Sandy (sand+muddy_sand+sandy_mud). Green=median; Red=mean+-SE")+
  scale_x_continuous(breaks=seq(0,100,10))+
  theme_bw()




#####################################################################################
#######################################################################################
###Create database with balanced number of points for each habitat class
##Database for cover over
set.seed(1)
n<-m2[,.(table(cvr_vrA))]
str(n)
rndid<-with(m2, ave(B02_20200204,cvr_vrA,FUN=function(x) {sample.int(length(x))}))
m3_cvrA<-m2[rndid<=min(n$N),]
m3_cvrA[,table(cvr_vrA)]


##Database for uca vs bare sediment
m2_ucabs<-m2[cvr_vrA=="bare_sediment"|cvr_vrA=="uca"]

set.seed(2)
n<-m2_ucabs[,.(table(uca))]
str(n)
rndid<-with(m2_ucabs, ave(B02_20200204,uca,FUN=function(x) {sample.int(length(x))}))
m3_ucabs<-m2_ucabs[rndid<=min(n$N),]
m3_ucabs[,table(uca)]


##Database for sediment grain types uca
m2[,finos_class:=ifelse(mud<10,"sandy","muddy")]
m2[,table(finos_class)]
m2_uca<-m2[cvr_vrA=="uca"]

set.seed(3)
n<-m2_uca[,.(table(finos_class))]
str(n)
rndid<-with(m2_uca, ave(B02_20200204,finos_class,FUN=function(x) {sample.int(length(x))}))
m3_uca<-m2_uca[rndid<=min(n$N),]
m3_uca[,table(finos_class)]


##Database for sediment grain types bare sediment
#m2[,finos_class:=ifelse(mud<10,"sandy","muddy")]
m2[,table(finos_class)]
m2_bs<-m2[cvr_vrA=="bare_sediment"]

set.seed(4)
n<-m2_bs[,.(table(finos_class))]
str(n)
rndid<-with(m2_bs, ave(B02_20200204,finos_class,FUN=function(x) {sample.int(length(x))}))
m3_bs<-m2_bs[rndid<=min(n$N),]
m3_bs[,table(finos_class)]




##Database for wet vs dry
n<-m2[,.(table(WD))] ## No need, number of pixels similar
str(n)

##Database for grain size 
set.seed(2)
m3<-m2[!(Class_22=="water_body")] ##remove water body, does not matter
n<-m3[,.(table(Class_22))] ## No need, number of pixels similar
str(n)
rndid<-with(m3, ave(B02_20200204,Class_22,FUN=function(x) {sample.int(length(x))}))
m3_C22<-m3[rndid<=min(n$N),]
m3_C22[,table(Class_22)]

##Database for cvr_sd_f 
set.seed(3)
m3_1<-m2[!(cvr_sd_f=="bare_sediment")] ##this is also water body, does not matter
n<-m3_1[,.(table(cvr_sd_f))] ## No need, number of pixels similar
str(n)
rndid<-with(m3_1, ave(B02_20200204,cvr_sd_f,FUN=function(x) {sample.int(length(x))}))
m3_cvrF<-m3_1[rndid<=min(n$N),]
m3_cvrF[,table(cvr_sd_f)]

##Database for cvr_sd_g 
set.seed(4)
#m3_2<-m2[!(cvr_sd_g=="bare_sediment_NA")] ##this is also water body, does not matter
n<-m2[,.(table(cvr_sd_g))] ## No need, number of pixels similar
str(n)
rndid<-with(m2, ave(B02_20200204,cvr_sd_g,FUN=function(x) {sample.int(length(x))}))
m3_cvrG<-m2[rndid<=50] ##Chose 50 because one of the classes has too few observations
m3_cvrG[,table(cvr_sd_g)]


################################################################
################################################################
##Compute general correlations between numerical variables

num<-names(m2)[sapply(m2, is.numeric)]
dbc<-m2[,c(num),with=F]
dbc1<-sapply(dbc, as.numeric)
res<-rcorr(as.matrix(dbc))
resr<-round(res$r,1)
resp<-round(res$P,1)

# Insignificant correlation are crossed
corrplot(resr,order="original",p.mat=resp,sig.level=0.05,insig="pch",method="number",diag=T)


##Compute correlations for STEP 1 (cover over) between numerical variables

num<-names(m3_cvrA)[sapply(m3_cvrA, is.numeric)]
dbc<-m3_cvrA[,c(num),with=F]
dbc1<-sapply(dbc, as.numeric)
res<-rcorr(as.matrix(dbc))
resr<-round(res$r,1)
resp<-round(res$P,1)

# Insignificant correlation are crossed
corrplot(resr,order="original",p.mat=resp,sig.level=0.05,insig="pch",method="number",diag=T)



##Compute correlations for STEP 2 (uca vs bare sed) between numerical variables

num<-names(m3_ucabs)[sapply(m3_ucabs, is.numeric)]
dbc<-m3_ucabs[,c(num),with=F]
dbc1<-sapply(dbc, as.numeric)
res<-rcorr(as.matrix(dbc))
resr<-round(res$r,1)
resp<-round(res$P,1)

# Insignificant correlation are crossed
corrplot(resr,order="original",p.mat=resp,sig.level=0.05,insig="pch",method="number",diag=T)


##Compute correlations for STEP 3 uca between numerical variables

num<-names(m3_uca)[sapply(m3_uca, is.numeric)]
dbc<-m3_uca[,c(num),with=F]
dbc1<-sapply(dbc, as.numeric)
res<-rcorr(as.matrix(dbc))
resr<-round(res$r,1)
resp<-round(res$P,1)

# Insignificant correlation are crossed
corrplot(resr,order="original",p.mat=resp,sig.level=0.05,insig="pch",method="number",diag=T)


##Compute correlations for STEP 3 bare sediment between numerical variables

num<-names(m3_bs)[sapply(m3_bs, is.numeric)]
dbc<-m3_bs[,c(num),with=F]
dbc1<-sapply(dbc, as.numeric)
res<-rcorr(as.matrix(dbc))
resr<-round(res$r,1)
resp<-round(res$P,1)

# Insignificant correlation are crossed
corrplot(resr,order="original",p.mat=resp,sig.level=0.05,insig="pch",method="number",diag=T)









##Correlation plot for dry and wet areas seperately
m2_dry<-m2[WD=="dry"]
m2_wet<-m2[WD=="wet"]

###Dry
num_dry<-names(m2_dry)[sapply(m2_dry, is.numeric)]
dbc_dry<-m2_dry[,c(num_dry),with=F]
dbc1_dry<-sapply(dbc_dry, as.numeric)
res_dry<-rcorr(as.matrix(dbc_dry))
resr_dry<-round(res_dry$r,2)
resp_dry<-round(res_dry$P,2)

# Insignificant correlation are crossed
corrplot(resr_dry,type="upper",order="original",p.mat=resp,sig.level=0.05,insig="pch",method="circle",diag=T,main="Dry areas <30% water")



###wet
num_wet<-names(m2_wet)[sapply(m2_wet, is.numeric)]
dbc_wet<-m2_wet[,c(num_wet),with=F]
dbc1_wet<-sapply(dbc_wet, as.numeric)
res_wet<-rcorr(as.matrix(dbc_wet))
resr_wet<-round(res_wet$r,2)
resp_wet<-round(res_wet$P,2)

# Insignificant correlation are crossed
corrplot(resr_wet,type="upper",order="original",p.mat=resp,sig.level=0.05,insig="pch",method="circle",diag=T,main="wet areas >=30% water")








#########################################################################################
#########################################################################################
##database for general PCA
m2_1<-na.omit(m2[,c(1:8,10:26,55,45:46,49,54,56:60,59,61:63,65:71)])
str(m2_1)

## subset database for a PCA focused on step1 
m4_step1<-na.omit(m3_cvrA[,c(45,49,1:26,54,58)])
str(m4_step1)

m4_step1[,table(cvr_vrA)]
m4_step1[,table(WD)]

m4_step1_tot<-na.omit(m2[,c(45,49,1:26,54,58)])
str(m4_step1_tot)

m4_step1_tot[,table(cvr_vrA)]
m4_step1_tot[,table(WD)]

##run PCA analysis
general_pca<-prcomp(m2_1[,!c(27:46)],center=T,scale=T)
summary(general_pca)


step1_pca<-prcomp(m4_step1[,!c("WD","cvr_vrA")],center=T,scale=T)
summary(step1_pca)

##run PCA analysis
step1_pca_1<-prcomp(m4_step1_tot[,!c("WD","cvr_vrA")],center=T,scale=T)
summary(step1_pca_1)


##plot PCA

ggbiplot(general_pca,choices=1:2,ellipse=T,groups=m2_1$Final_finos_class,varname.size=5,alpha=.4,var.axes = T)+
  theme_bw()+
  labs(colour="General PCA")

pca3d(step1_pca, group=factor(m4_step1$cvr_vrA),components=1:3,show.group.labels = T,legend="right",biplot=F,new=T)
#snapshotPCA3d(file="first_plot.png")
dev.off()

stp1_p<-step1_pca_1$x

sat_stp1<-na.omit(sat)
sat_stp1$pc1<-stp1_p[,1]

plot(stp1_p[,1], col = cm.colors(15), axes = FALSE)




#### STep 2 PCA 
m4_step2<-na.omit(m3_ucabs[,c(53,49,1:26,54,58)])
str(m4_step2)

m4_step2_1<-na.omit(m2[,c(53,49,1:26,54,58)])
str(m4_step2_1)

m4_step2_1[,table(uca)]
m4_step2_1[,table(WD)]

##run PCA analysis
step2_pca<-prcomp(m4_step2[,!c("WD","uca")],center=T,scale=T)
summary(step2_pca)

##run PCA analysis
step2_pca1<-prcomp(m4_step2_1[,!c("WD","uca")],center=T,scale=T)
summary(step2_pca1)

##plot PCA

ggbiplot(step2_pca1,choices=1:2,ellipse=T,groups=m4_step2_1$uca,varname.size=5,alpha=.4,var.axes = T)+
  theme_bw()+
  labs(colour="step 2 uca vs bare sediment")



#### STep 3 UCA PCA 
m4_step3uca<-na.omit(m3_uca[,c(60,49,1:26,54,58)])
str(m4_step3uca)

m4_step3uca_1<-na.omit(m2[uca=="uca",c(60,49,1:26,54,58)])
str(m4_step3uca_1)

m4_step3uca[,table(finos_class)]
m4_step3uca_1[,table(finos_class)]

##run PCA analysis
step3uca_pca<-prcomp(m4_step3uca[,!c("WD","finos_class")],center=T,scale=T)
summary(step3uca_pca)

##run PCA analysis
step3uca_pca1<-prcomp(m4_step3uca_1[,!c("WD","finos_class")],center=T,scale=T)
summary(step3uca_pca1)

##plot PCA

ggbiplot(step3uca_pca1,choices=1:2,ellipse=T,groups=m4_step3uca_1$finos_class,varname.size=5,alpha=.4,var.axes = T)+
  theme_bw()+
  labs(colour="step 3 uca")





#### STep 3 BS PCA 
m4_step3bs<-na.omit(m3_bs[,c(60,49,1:26,54,58)])
str(m4_step3bs)

m4_step3bs_1<-na.omit(m2[uca=="other",c(60,49,1:26,54,58)])
str(m4_step3bs_1)

m4_step3bs[,table(finos_class)]
m4_step3bs_1[,table(finos_class)]

##run PCA analysis
step3bs_pca<-prcomp(m4_step3bs[,!c("WD","finos_class")],center=T,scale=T)
summary(step3bs_pca)

##run PCA analysis
step3bs_pca1<-prcomp(m4_step3bs_1[,!c("WD","finos_class")],center=T,scale=T)
summary(step3bs_pca1)

##plot PCA

ggbiplot(step3bs_pca,choices=1:2,ellipse=T,groups=m4_step3bs$finos_class,varname.size=5,alpha=.4,var.axes = T)+
  theme_bw()+
  labs(colour="step 3 bare sediment")








##sample balanced number of points between dry and wet
set.seed(5)
n<-m4_1[,.(table(WD))] ## No need, number of pixels similar
rndid<-with(m4_1, ave(NDWI,WD,FUN=function(x) {sample.int(length(x))}))
m4_WD<-m4_1[rndid<=min(n$N)] ##Chose 50 because one of the classes has too few observations
m4_WD[,table(WD)]

##run PCA analysis
m4_pca<-prcomp(m4_WD[,!c("WD","cvr_vrA","cvr_sd_f","cvr_sd_g","uca","D50_um_","Mn_fw_m","Mn_fw_p","intensity")],center=T,scale=T)
summary(m4_pca)

##plot PCA

ggbiplot(m4_pca,choices=1:2,ellipse=T,groups=m4_WD$WD,varname.size=5,alpha=.4,var.axes = T)+
  theme_bw()+
  labs(colour="Wet VS Dry (30% cut)")

pca3d(m4_pca, group=factor(m4_WD$uca),components=1:3,show.group.labels = T,legend="right",biplot=F,new=T)
#snapshotPCA3d(file="first_plot.png")
dev.off()











## subset database for a PCA focused on separating water from dry areas (column WD, 30% cut)
m4<-m2[,.(NDWI,mNDWI,NDMI,NDMI1,B11_20200204,B12_20200204,intensity,S1_20200128_VH,S1_20200128_VV,dem_104_469,c_uca,c_water,mud,Sand,Mn_fw_m,Mn_fw_p,D50_um_,WD,cvr_vrA,cvr_sd_f,cvr_sd_g,uca)][c(cvr_vrA=="bare_sediment"|cvr_vrA=="uca")]
str(m4)
m4_1<-na.omit(m4)
str(m4_1)
m4_1[,table(WD)]

##sample balanced number of points between dry and wet
set.seed(5)
n<-m4_1[,.(table(WD))] ## No need, number of pixels similar
rndid<-with(m4_1, ave(NDWI,WD,FUN=function(x) {sample.int(length(x))}))
m4_WD<-m4_1[rndid<=min(n$N)] ##Chose 50 because one of the classes has too few observations
m4_WD[,table(WD)]

##run PCA analysis
m4_pca<-prcomp(m4_WD[,!c("WD","cvr_vrA","cvr_sd_f","cvr_sd_g","uca","D50_um_","Mn_fw_m","Mn_fw_p","intensity")],center=T,scale=T)
summary(m4_pca)

##plot PCA

ggbiplot(m4_pca,choices=1:2,ellipse=T,groups=m4_WD$WD,varname.size=5,alpha=.4,var.axes = T)+
  theme_bw()+
  labs(colour="Wet VS Dry (30% cut)")

pca3d(m4_pca, group=factor(m4_WD$uca),components=1:3,show.group.labels = T,legend="right",biplot=F,new=T)
#snapshotPCA3d(file="first_plot.png")
dev.off()

### Now separating wet and dry areas
m4_dry<-m4_1[WD=="dry"]
m4_dry[,table(uca)]

##sample balanced number of points between uca and other
set.seed(6)
n<-m4_dry[,.(table(uca))]
rndid<-with(m4_dry, ave(NDWI,uca,FUN=function(x) {sample.int(length(x))}))
m4_dryuca<-m4_dry[rndid<=min(n$N)] ##Chose 50 because one of the classes has too few observations
m4_dryuca[,table(uca)]

##run PCA analysis
m4_dryuca_pca<-prcomp(m4_dryuca[,!c("WD","cvr_vrA","cvr_sd_f","cvr_sd_g","uca")],center=T,scale=T)
summary(m4_dryuca_pca)

##plot PCA
ggbiplot(m4_dryuca_pca,choices=1:2,ellipse=T,groups=m4_dryuca$cvr_sd_g,varname.size=5,alpha=.4,var.axes = T)+
  theme_bw()+
  labs(colour="Grain size (gradistat) - Dry areas")



#############
##Wet areas now
m4_wet<-m4_1[WD=="wet"]
m4_wet[,table(uca)]

##sample balanced number of points between uca and other
set.seed(7)
n<-m4_wet[,.(table(uca))]
rndid<-with(m4_wet, ave(NDWI,uca,FUN=function(x) {sample.int(length(x))}))
m4_wetuca<-m4_wet[rndid<=min(n$N)] ##Chose 50 because one of the classes has too few observations
m4_wetuca[,table(uca)]

##run PCA analysis
m4_wetuca_pca<-prcomp(m4_wetuca[,!c("WD","cvr_vrA","cvr_sd_f","cvr_sd_g","uca")],center=T,scale=T)
summary(m4_wetuca_pca)

##plot PCA
ggbiplot(m4_wetuca_pca,choices=1:2,ellipse=T,groups=m4_wetuca$uca,varname.size=5,alpha=.4,var.axes = T)+
  theme_bw()+
  labs(colour="Uca VS Other (30% cut) - wet areas")





###Run PCA again with all data
m2_1<-m2[c(cvr_vrA=="bare_sediment"|cvr_vrA=="uca"),!c("Point","Sd_cls1","WB","rocks","macro","shells")]
m2_2<-na.omit(m2_1)


###################Wet VS Dry ######################################
##sample balanced number of points between dry and wet
set.seed(8)
n<-m2_2[,.(table(WD))] 
rndid<-with(m2_2, ave(NDWI,WD,FUN=function(x) {sample.int(length(x))}))
m2_2WD<-m2_2[rndid<=min(n$N)] ##Chose 50 because one of the classes has too few observations
m2_2WD[,table(WD)]


m2_pca<-prcomp(m2_2WD[,!c("Class_22","WD","cvr_vrA","cvr_sd_f","cvr_sd_g","uca")],center=T,scale=T)
summary(m2_pca)

ggbiplot(m2_pca,choices=1:2,ellipse=T,groups=m2_2WD$cvr_sd_g,varname.size=5,alpha=.4,var.axes = T)+
  theme_bw()+
  labs(colour="Sediment type (gradistat)")

pca3d(m2_pca, group=factor(m2_2WD$WD),components=1:3,show.group.labels = T,legend="right",biplot=T,new=T)



############################## Grain size (field) #################################

##sample balanced number of points between grain size classes
set.seed(9)
n<-m2_2[,.(table(cvr_sd_f))] 
rndid<-with(m2_2, ave(NDWI,cvr_sd_f,FUN=function(x) {sample.int(length(x))}))
m2_2sedF<-m2_2[rndid<=min(n$N)] ##Chose 50 because one of the classes has too few observations
m2_2sedF[,table(cvr_sd_f)]


m2_pca_sedF<-prcomp(m2_2sedF[,!c("Class_22","WD","cvr_vrA","cvr_sd_f","cvr_sd_g","uca","c_shlls","c_rocks")],center=T,scale=T)
summary(m2_pca_sedF)

ggbiplot(m2_pca_sedF,choices=1:2,ellipse=T,groups=m2_2sedF$cvr_sd_f,varname.size=5,alpha=.4,var.axes = T)+
  theme_bw()+
  labs(colour="Sediment type (field)")

######### For dry areas

m2_2_dry<-m2_2[WD=="dry"]

##sample balanced number of points between grain size classes
set.seed(10)
n<-m2_2_dry[,.(table(cvr_sd_f))] 
rndid<-with(m2_2, ave(NDWI,cvr_sd_f,FUN=function(x) {sample.int(length(x))}))
m2_2sedF<-m2_2[rndid<=min(n$N)] ##Chose 50 because one of the classes has too few observations
m2_2sedF[,table(cvr_sd_f)]


m2_pca_dry_sedF<-prcomp(m2_2_dry[,!c("Class_22","WD","cvr_vrA","cvr_sd_f","cvr_sd_g","uca","c_shlls","c_rocks")],center=T,scale=T)
summary(m2_pca_dry_sedF)

ggbiplot(m2_pca_dry_sedF,choices=1:2,ellipse=T,groups=m2_2_dry$cvr_sd_f,varname.size=5,alpha=.4,var.axes = T)+
  theme_bw()+
  labs(colour="Sediment type (field) - Dry areas")

######### For wet areas

m2_2_wet<-m2_2[WD=="wet"]

##sample balanced number of points between grain size classes
set.seed(10)
n<-m2_2_wet[,.(table(cvr_sd_f))] 
rndid<-with(m2_2, ave(NDWI,cvr_sd_f,FUN=function(x) {sample.int(length(x))}))
m2_2sedF<-m2_2[rndid<=min(n$N)] ##Chose 50 because one of the classes has too few observations
m2_2sedF[,table(cvr_sd_f)]


m2_pca_wet_sedF<-prcomp(m2_2_wet[,!c("Class_22","WD","cvr_vrA","cvr_sd_f","cvr_sd_g","uca","c_shlls","c_rocks")],center=T,scale=T)
summary(m2_pca_wet_sedF)

ggbiplot(m2_pca_wet_sedF,choices=1:2,ellipse=T,groups=m2_2_wet$cvr_sd_f,varname.size=5,alpha=.4,var.axes = T)+
  theme_bw()+
  labs(colour="Sediment type (field) - wet areas")






