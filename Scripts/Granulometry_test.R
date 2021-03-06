setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()


## Pacotes
packs<-c("data.table","raster","ggplot2","rgdal","viridis","sp","RColorBrewer","scales","tools","rgeos","xlsx","spatstat","G2Sd","reshape2")
lapply(packs,require,character.only=T)

##Load granulometry data
Gra<-fread("D:/Work/FCUL/Doutoramento/Capitulos/Mapping_intertidal_sediments/Data/Granulometry/Data_gran_20201126_OK.csv")

str(Gra)

Gra1<-Gra[!(Sed_ID==100 | Sed_ID==700 | Sed_ID==3346 | Sed_ID==3549 | Sed_ID==3819),c(1,3:7)] # clean database 
colnames(Gra1)[2:6]<-c(2000,500,250,63,0) # to use in gradistat

##reshape from long to wide
a<-melt(Gra1,id.vars = "Sed_ID")
Gra2<-dcast(a,variable~Sed_ID,fun.aggregate = mean)

rownames(Gra2)<-as.character(Gra2$variable)
Gra3<-Gra2[,-1]
#colnames(Gra3)<-paste("X",colnames(Gra3),sep="_")

###Data with percent for ploting
Gra11<-Gra[!(Gra$Sed_ID==100 | Gra$Sed_ID==700 | Gra$Sed_ID==3346 | Gra$Sed_ID==3549),c(1,8:12)] # clean database 

aa<-melt(Gra11,id.vars = "Sed_ID")
Gra22<-dcast(aa,variable~Sed_ID,fun.aggregate = mean)
Gra22$variable<-c(2000,500,250,63,0)
rownames(Gra22)<-Gra22$variable
#Gra33<-Gra22[,-1]


Gra55<-melt(Gra22,id.vars = "variable")
colnames(Gra55)[1:2]<-c("crivo","Sed_ID")
Gra66<-dcast(Gra55,Sed_ID~crivo,fun.aggregate = mean)


###Gradistat
G<-granstat(Gra3)

sedID<-data.table(Sed_ID=colnames(G))
sedID[,order:=1:length(colnames(G))]

sedID1<-dcast.data.table(sedID,"Sed_ID"~order,value.var="Sed_ID")
sedID2<-sedID1[,-1]
names(sedID2)<-colnames(G)

G1<-rbind(G,sedID2)
G1[,stat:=c(rownames(G),"SedID")]

#rownames(G1)[length(G1[,1])]<-"SedID"

G2<-melt(G1,id.vars = "stat")
colnames(G2)[2]<-c("sedID")
G3<-G2[stat=="Mean.fw.um"|stat=="Mean.fw.phi"|stat=="Sediment"|stat=="D50(um)"|stat=="Texture"|stat=="mud"|stat=="Sand"]
G4<-dcast.data.table(G3,sedID~stat)
str(G4)
G5<-G4[,lapply(.SD,as.numeric),by=.(sedID,Sediment,Texture)]


ggplot(G5,aes(x=mud,y=Mean.fw.phi))+
  geom_point(size=2.5)+
  stat_smooth(method=lm,se=F,lwd=1,fullrange = F)+
  theme_bw()

cor.test(G5$mud,G5$Mean.fw.phi)


###Create a new sed id code without the x in the names
G5[,Sed_ID:=as.numeric(gsub("X","",sedID))]
G6<-setorder(G5,Sed_ID)

Gra111<-setorder(Gra1,Sed_ID)

#which(G6$Sed_ID!=Gra111$Sed_ID)

Gra_F<-merge(Gra111,G6,all=T)

#write.table(Gra_F,"Data_out/db/sediment_stats_20201113.csv",row.names=F,sep=";")

Gra_F1<-merge(Gra66,G5,all=T)


ggplot(Gra_F1,aes(x=`500`,y=Mean.fw.phi))+
  geom_point(size=2.5)+
  stat_smooth(method=lm,se=F,lwd=1,fullrange = F)+
  theme_bw()

ggplot(Gra_F1,aes(x=`250`,y=Mean.fw.phi))+
  geom_point(size=2.5)+
  stat_smooth(method=lm,se=F,lwd=1,fullrange = F)+
  theme_bw()

ggplot(Gra_F1,aes(x=`63`,y=Mean.fw.phi))+
  geom_point(size=2.5)+
  stat_smooth(method=lm,se=F,lwd=1,fullrange = F)+
  theme_bw()

ggplot(Gra_F1,aes(x=`0`,y=Mean.fw.phi))+
  geom_point(size=2.5)+
  stat_smooth(method=lm,se=F,lwd=1,fullrange = F)+
  theme_bw()

################ shapefile part now

## Load GNB shape
#GNB<-readOGR("./Shapefiles/GNB/gnb_poly.shp")
#crs(GNB)
# Change the projection to lat long
#GNB1<-spTransform(GNB,crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#crs(GNB1)


GT<-readOGR("./Data_out/Polygons/poly_GT_20210113.shp")

#plot(GNB1)
plot(GT, col="red")
#str(GT)

#GT_DF<-GT@data
#str(GT_DF)

Gra_F[,Point:=as.factor(Sed_ID)]

###Check for duplicated ID
AA<-data.frame(table(Gra_F$Point))
AA[AA$Freq > 1,]

###quick fix, deal with this sediment duplication later - check Gra_F dor the index row to delete - it changes with every new db
Gra_Final<-unique(Gra_F,by="Point")
#Gra_Final<-Gra_F[-which(duplicated(Gra_F$Point)),] ### delete duplicate entry (point 3556)

GT_Final<-merge(GT,Gra_Final,by="Point",all.y=T)
plot(GT_Final)

GT_Final$Sed_class<-substr(GT_Final$Sediment,1,regexpr(",",GT_Final$Sediment)-1)
GT_Final$Sed_class1<-factor(GT_Final$Sed_class,levels=c("Medium Sand","Fine Sand","Very Fine Sand","Very Coarse Silt","Medium Silt"))
GT_Final$Class_22<-factor(GT_Final$Class_2,levels=c("beach_sand","sand","muddy_sand","sandy_mud","mud"))

length(unique(GT$Point))
length(unique(Gra_Final$Point))
length(unique(GT_Final$Point))

ID_GT<-as.numeric(as.character(unique(GT$Point)))
ID_Gra<-as.numeric(as.character(unique(Gra_Final$Point)))
ID_GT_Final<-as.numeric(as.character(unique(GT_Final$Point)))

###These sed IDs are from Adonga, which is not in the used GT polygon shapefile
setdiff(ID_Gra,ID_GT_Final)
setdiff(ID_Gra,ID_GT)

plot(GT_Final,col="red")
writeOGR(GT_Final,"Data_out/Polygons",layer="Poly_GT_Gra_ended_20210113",driver="ESRI Shapefile",overwrite=F)

View(GT_Final@data)


ggplot(GT_Final@data,aes(x=Class_22,y=Sed_class1))+
  geom_point(size=3.5)+
  #stat_summary(size=1)+
  scale_x_discrete(limits=c("beach_sand","sand","muddy_sand","sandy_mud","mud"))

ggplot(GT_Final@data,aes(x=Class_3,y=mud,col=Class_22))+
  #geom_point(size=3.5)+
  stat_summary(size=0.7,na.rm=F, position = position_dodge(width = 0.2))+
  scale_x_discrete(limits=c(NA,"uca","macro_uca"))

ggplot(GT_Final@data,aes(x=Sed_class1,y=Mean.fw.phi,col=Class_22))+
  #geom_point(size=2,position=position_jitter(width=.1, height=0))+
  stat_summary(size=1,position = position_dodge(width = 0.5))

ggplot(GT_Final@data,aes(x=Class_22,y=mud))+
  geom_point(size=1)+
  stat_summary(size=1)

ggplot(GT_Final@data,aes(x=Class_22,y=Sed_class1,col=GT_Final$`D50(um)`))+
  geom_point(size=2,position=position_dodge(width=.2))
  #stat_summary(size=1)

ggplot(GT_Final@data,aes(x=Sed_class1,y=mud))+
  geom_point(size=1)+
  stat_summary(size=1)


### Um pouco de estatítica

##### Diferenças na percentagem de mud entre as classes definidas no campo
test1<-aov(mud~Class_22,data=GT_Final@data)
summary(test1)

TukeyHSD(test1)

##### Diferenças na percentagem de mud entre as classes do gradistat
test2<-aov(mud~Sed_class1,data=GT_Final@data)
summary(test2)

TukeyHSD(test2)

##### Diferenças no phi entre as classes definidas no campo
test3<-aov(Mean.fw.phi~Class_22,data=GT_Final@data)
summary(test3)

TukeyHSD(test3)


##### Diferenças no phi entre as classes do gradistat (so para ver se o gajo esta a fazer o que diz que está a fazer)
test4<-aov(Mean.fw.phi~Sed_class1,data=GT_Final@data)
summary(test4)

TukeyHSD(test4)

##### Diferenças no median grain size distribution entre as classes definidas no campo
test5<-aov(GT_Final$`D50(um)`~Class_22,data=GT_Final@data)
summary(test5)

TukeyHSD(test5)


##### Diferenças no median grain size distribution entre as classes do gradistat
test6<-aov(GT_Final$`D50(um)`~Sed_class1,data=GT_Final@data)
summary(test6)

TukeyHSD(test6)
