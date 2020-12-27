setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

packs<-c("PerformanceAnalytics","Hmisc","sf","beepr","RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis","data.table","reshape2","corrplot")
lapply(packs,require,character.only=T)

## Load DB for Urok, created in script GraVSSSat_Preliminary
m1<-fread("Data_out/db/GraVSSat_db_20201221.csv")
str(m1)

### Checking for overlayed polygons and number of points
m1[,.(numbcell=length(unique(cell)))]
m1$cell[duplicated(m1$cell)] # These pixels are duplicated in different polygons. Ignore now but deal with later


m1[,.(numbpoints=length(unique(Point)))]

### Clean database and select only columns that matter
db<-m1[,!c("Ãƒ___Day","Site","Radius","uc_dnst","greenss","mangrov","sed_Id","obs","obs2","finos","Sed_ID_1","sedID","Sd_clss","Clss_22"),with=F]

### Calculate NDVI
db[,NDVI:=(B08-B04)/(B08+B04)][,NDVI:=round(NDVI,2)]

###Calculate NDWI1
db[,NDWI1:=(B08-B12)/(B08+B12)][,NDWI1:=round(NDWI1,2)]

###Calculate NDWI (with green band)
db[,NDWI:=(B03-B08)/(B03+B08)][,NDWI:=round(NDWI,2)]

###Calculate mNDWI
db[,mNDWI:=(B03-B11)/(B03+B11)][,mNDWI:=round(mNDWI,2)]

###Calculate NDMI
db[,NDMI:=(B08-B11)/(B08+B11)][,NDMI:=round(NDMI,2)]

###Create new levels of habitat
db[,level0:=Class_3][Class_2=="water_body",level0:="water_body"][is.na(Class_3),level0:="bare_sediment"]

### Transform sat bands in numeric
convcols<-names(db)[sapply(db, is.integer)] ## Store the names of all columns with type integer
convcols1<-convcols[-13] #remove the column Point, which is to keep as integer
not<-c(names(db)[!sapply(db, is.integer)],convcols[13]) #store the names of the columns that are not for conversion (not integer)
db1<-db[,lapply(.SD,as.numeric),.SDcols=convcols1,by=not]

### Transform column c_uca in numeric (remove the ?)
db1[c_uca=="?",c_uca:=NA]
db1[,c_uca:=as.numeric(c_uca)]

### Moisture index
db1[,MI:=(B08a-B11)/(B08a+B11)]

### melted Database
#sat<-c(colnames(db1)[c(1:4,22:26,30:40)])
#env<-c(colnames(db1)[c(8,41:45)])
#grain<-c(colnames(db1)[c(9:13,16:20)])
#db2<-melt(db1,id.vars=c("cell","Point","Class_1","Class_2","Class_3","level0","Sd_cls1",sat,grain),
          #measure.vars = env, variable.name="env",value.name = "env_value")

#db3<-melt(db2,id.vars=c("cell","Point","Class_1","Class_2","Class_3","level0","Sd_cls1",sat,"env","env_value"),
          #measure.vars = grain, variable.name="grain",value.name = "grain_value")

#db4<-melt(db3,id.vars=c("cell","Point","Class_1","Class_2","Class_3","level0","Sd_cls1","env","env_value","grain","grain_value"),
          #measure.vars = sat, variable.name="sat",value.name = "sat_value")


##Compute general correlations between numerical variables
num<-names(db1)[sapply(db1, is.numeric)]
dbc<-db1[,c(num),with=F]
dbc1<-sapply(dbc, as.numeric)
res<-rcorr(as.matrix(dbc))
resr<-round(res$r,2)
resp<-round(res$P,2)

# Insignificant correlation are crossed
corrplot(resr,type="upper",order="hclust",p.mat=resp,sig.level=0.05,insig="pch")

##################################################

unique(db1$level0)

############ Level 0: radar VH vs Class3
ggplot(data=db1,aes(x=level0,y=S1_VH))+
  #geom_boxplot()+
  #geom_point(position=position_jitter(width=.3, height=0), col="red")+
  stat_summary(size=1)+
  theme_bw()

############ Level 0: radar VV vs Class3
ggplot(data=db1,aes(x=level0,y=S1_VV))+
  #geom_boxplot()+
  #geom_point(position=position_jitter(width=.3, height=0), col="red")+
  stat_summary(size=1)+
  theme_bw()

############ Level 0: NDVI vs Class3
ggplot(data=db1,aes(x=Class_3,y=NDVI))+
  #geom_boxplot()+
  #geom_point(position=position_jitter(width=.3, height=0), col="red")+
  stat_summary(size=1)+
  theme_bw()

############ Level 0: NDWI vs Class3
ggplot(data=db1,aes(x=level0,y=NDWI))+
  #geom_boxplot()+
  #geom_point(position=position_jitter(width=.3, height=0), col="red")+
  stat_summary(size=1)+
  theme_bw()

############ Level 0: NDWI1 vs Class3
ggplot(data=db1,aes(x=level0,y=NDWI1))+
  #geom_boxplot()+
  #geom_point(position=position_jitter(width=.3, height=0), col="red")+
  stat_summary(size=1)+
  theme_bw()

############ Level 0: mNDWI vs Class3
ggplot(data=db1,aes(x=level0,y=mNDWI))+
  #geom_boxplot()+
  #geom_point(position=position_jitter(width=.3, height=0), col="red")+
  stat_summary(size=1)+
  theme_bw()

############ Level 0: NDMI vs Class3
ggplot(data=db1,aes(x=level0,y=NDMI))+
  #geom_boxplot()+
  #geom_point(position=position_jitter(width=.3, height=0), col="red")+
  stat_summary(size=1)+
  theme_bw()

############ Level 0: Tempo exposicao vs Class3
ggplot(data=db1,aes(x=level0,y=tempo_exposicao_Bijagos))+
  #geom_boxplot()+
  #geom_point(position=position_jitter(width=.3, height=0), col="red")+
  stat_summary(size=1)+
  theme_bw()

############ Level 0: visivel vs Class3
ggplot(data=db1,aes(x=level0,y=B04))+
  #geom_boxplot()+
  #geom_point(position=position_jitter(width=.3, height=0), col="red")+
  stat_summary(size=1)+
  theme_bw()

##################### Isolating bare sediment only

db2<-db1[level0=="bare_sediment"]
str(db2)
db2[!is.na(Sand),length(unique(Point))]
db2[,.N,by=Class_2]
db2[,Class_2:=factor(Class_2,levels=c("beach_sand","sand","muddy_sand","sandy_mud","mud","water_body"))]

## água
db2[,water:=as.character(c_water)][c_water<30,water:="<30"][c_water>=30,water:=">30"]


############ Level 1: Class2 vs sat
ggplot(data=db2,aes(x=Class_2,y=MI))+
  #geom_boxplot()+
  #geom_point(position=position_jitter(width=.3, height=0), col="red")+
  stat_summary(size=1)+
  theme_bw()

############ Level 1: Class2 vs env
ggplot(data=db2,aes(x=Class_2,y=c_chnnl))+
  #geom_boxplot()+
  #geom_point(position=position_jitter(width=.4, height=0), col="red")+
  stat_summary(size=1)+
  theme_bw()


############ Level 1: Gra vs sat
ggplot(data=db2,aes(x=Mn_fw_m,y=NDMI))+
  #geom_boxplot()+
  #geom_point(position=position_jitter(width=.4, height=0), col="red")+
  stat_summary(size=1)+
  geom_smooth(stat="smooth")+
  theme_bw()

####################Try to remove water from bare sediment

############ Level 1: Gra vs sat
ggplot(data=db2[!Class_2=="water_body"],aes(x=c_water,y=MI))+
  #geom_boxplot()+
  geom_point(position=position_jitter(width=.4, height=0), col="red")+
  #stat_summary(size=1)+
  geom_smooth(method="lm")+
  theme_bw()

ggplot(data=db2[!Class_2=="water_body"])+
  geom_histogram(aes(x=c_water),bins = 100,binwidth = 2)+
  theme_bw()

### Try remove points with more than 30% water


db3<-db2[c_water<30,]
db3[,length(unique(Point))]


############ Level 1: Class2 vs sat
ggplot(data=db2,aes(x=Class_2,y=S1_VV,col=water))+
  #geom_boxplot()+
  #geom_point(position=position_jitter(width=.3, height=0), col="red")+
  stat_summary(fun=mean,geom="point",size=4,position=position_dodge(width=.3))+
  stat_summary(fun.data=mean_sdl,fun.args=list(mult=1),geom="errorbar",width=0.2,position=position_dodge(width=.3))+
  theme_bw()+
  labs(title="Bare sediment")


############ Level 1: Class2 vs env
ggplot(data=db2,aes(x=Class_2,y=c_chnnl, col=water))+
  #geom_boxplot()+
  #geom_point(position=position_jitter(width=.4, height=0), col="red")+
  stat_summary(size=1)+
  theme_bw()


############ Level 1: Gra vs sat
ggplot(data=db2,aes(x=mud,y=B12, col=water))+
  #geom_boxplot()+
  #geom_point(position=position_jitter(width=.4, height=0), col="red")+
  stat_summary(size=1,position=position_dodge(width=.3))+
  #geom_smooth(stat="smooth")+
  theme_bw()


######################################################### Statistics granulometry vs sat

test1<-aov(Mn_fw_p~S1_VH+S1_VV+MI+NDVI+NDWI+NDWI1+mNDWI+NDMI+c_uca+B08+B02+B03+B04+B05+B06+B07+B08a+B09+B11+B12,data=db2)
summary(test1)


