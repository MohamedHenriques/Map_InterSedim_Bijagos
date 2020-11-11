setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

## Pacotes
packs<-c("raster","ggplot2","rgdal","viridis","sp","RColorBrewer","scales","tools","rgeos","xlsx","spatstat","G2Sd","reshape2")
lapply(packs,require,character.only=T)

##Load granulometry data
Gra<-read.xlsx("D:/Work/FCUL/Doutoramento/Capitulos/Mapping_intertidal_sediments/Data/Granulometry/Data_gran_20201109.XLSX",1)
str(Gra)
colnames(Gra)[3:7]<-c(2000,500,250,63,0)

Gra1<-Gra[-which(Gra$Sed_ID==100),c(1,3:12)] # clean database 

##Transpose dataframe
a<-melt(Gra1,id.vars = "Sed_ID")
Gra2<-dcast(a,variable~Sed_ID,fun.aggregate = mean)
Gra2$variable<-c(2000,500,250,63,0)
rownames(Gra2)<-Gra2$variable
Gra3<-Gra2[,-1]

###Data with percent for ploting
Gra4<-Gra2[c(6:10),]
Gra5<-melt(Gra4,id.vars = "variable")
colnames(Gra5)[1:2]<-c("crivo","Sed_ID")
Gra6<-dcast(Gra5,Sed_ID~crivo,fun.aggregate = mean)


###Gradistat
G<-granstat(Gra3)

sedID<-data.frame(colnames(G))
sedID$order<-c(1:length(colnames(G)))
sedID1<-dcast(sedID,"colnames.G."~order,value.var="colnames.G.")
sedID2<-sedID1[,-1]
names(sedID2)<-colnames(G)

G1<-rbind(G,sedID2)
G1$stat<-rownames(G1)
rownames(G1)[length(G1[,1])]<-"SedID"

G2<-melt(G1,id.vars = "stat")
colnames(G2)[2]<-c("sedID")
G3<-G2[which(G2$stat=="Mean.fw.um"|G2$stat=="Mean.fw.phi"|G2$stat=="Sediment"|G2$stat=="D50(um)"|G2$stat=="Texture"|G2$stat=="mud"|G2$stat=="Sand"),]
G4<-dcast(G3,sedID~stat)
G4$mud<-as.numeric(G4$mud)
G4$`D50(um)`<-as.numeric(G4$`D50(um)`)
G4$Mean.fw.phi<-as.numeric(G4$Mean.fw.phi)
G4$Mean.fw.um<-as.numeric(G4$Mean.fw.um)
G4$Sand<-as.numeric(G4$Sand)


ggplot(G4,aes(x=mud,y=Mean.fw.phi))+
  geom_point(size=2.5)+
  stat_smooth(method=lm,se=F,lwd=1,fullrange = F)+
  theme_bw()

cor.test(G4$mud,G4$Mean.fw.phi)


###
G4$Sed_ID<-as.numeric(gsub("X","",G4$sedID))

G5<-G4[order(G4$Sed_ID),]
Gra11<-Gra1[order(Gra1$Sed_ID),]

which(G5$Sed_ID!=Gra11$Sed_ID)

Gra_F<-merge(Gra11,G5,all.x=T,all.y=T)

#write.table(Gra_F,"Data_out/db/sediment_stats_20201109.csv",row.names=F,sep=";")

Gra_F1<-merge(Gra6,G5,all.x=T,all.y=T)



ggplot(Gra_F1,aes(x=Gra_F1$'500',y=Mean.fw.phi))+
  geom_point(size=2.5)+
  stat_smooth(method=lm,se=F,lwd=1,fullrange = F)+
  theme_bw()

ggplot(Gra_F1,aes(x=Gra_F1$'250',y=Mean.fw.phi))+
  geom_point(size=2.5)+
  stat_smooth(method=lm,se=F,lwd=1,fullrange = F)+
  theme_bw()

ggplot(Gra_F1,aes(x=Gra_F1$'63',y=Mean.fw.phi))+
  geom_point(size=2.5)+
  stat_smooth(method=lm,se=F,lwd=1,fullrange = F)+
  theme_bw()

ggplot(Gra_F1,aes(x=Gra_F1$'0',y=Mean.fw.phi))+
  geom_point(size=2.5)+
  stat_smooth(method=lm,se=F,lwd=1,fullrange = F)+
  theme_bw()

################ Satellite part now

## Load GNB shape
GNB<-readOGR("./Shapefiles/GNB/gnb_poly.shp")
crs(GNB)
# Change the projection to lat long
GNB1<-spTransform(GNB,crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
crs(GNB1)


GT<-readOGR("./Data_out/Polygons/poly_GT.shp")

plot(GNB1)
plot(GT, col="red")
str(GT)

GT_DF<-GT@data
str(GT_DF)

Gra_F$Point<-as.factor(Gra_F$Sed_ID)

AA<-data.frame(table(Gra_F$Point))
AA[AA$Freq > 1,]

###quick fix, deal with this repetition later
Gra_Final<-Gra_F[-141,] ### delete duplicate entry (point 3346)

GT_Final<-merge(GT,Gra_Final,by="Point",all.x=F,all.y=T)

GT_Final$Sed_class<-substr(GT_Final$Sediment,1,9)

plot(GT_Final,col="red")

View(GT_Final@data)


ggplot(GT_Final@data,aes(x=Class_2,y=Sed_class))+
  geom_point(size=3.5)+
  #stat_summary(size=1)+
  scale_x_discrete(limits=c("beach_sand","sand","muddy_sand","sandy_mud","mud"))

ggplot(GT_Final@data,aes(x=Sed_class,y=Mean.fw.phi,col=Class_2))+
  #geom_point(size=2,position=position_jitter(width=.1, height=0))+
  stat_summary(size=1,position=position_jitter(width=.1, height=0))+
  scale_x_discrete(limits=c("Medium Sa","Fine Sand","Very Fine"))

ggplot(GT_Final@data,aes(x=Class_2,y=mud))+
  geom_point(size=1)+
  stat_summary(size=1)+
  scale_x_discrete(limits=c("beach_sand","sand","muddy_sand","sandy_mud","mud"))

ggplot(GT_Final@data,aes(x=Class_2,y=Sed_class,col=GT_Final$`D50(um)`))+
  geom_point(size=2,position=position_jitter(width=.2, height=.2))+
  #stat_summary(size=1)+
  scale_x_discrete(limits=c("beach_sand","sand","muddy_sand","sandy_mud","mud"))+
  scale_y_discrete(limits=c("Medium Sa","Fine Sand","Very Fine"))

ggplot(GT_Final@data,aes(x=Sed_class,y=mud))+
  geom_point(size=1)+
  stat_summary(size=1)+
  scale_x_discrete(limits=c("Medium Sa","Fine Sand","Very Fine"))


### Um pouco de estatítica

##### Diferenças na percentagem de mud entre as classes definidas no campo
test1<-aov(mud~Class_2,data=GT_Final@data)
summary(test1)

TukeyHSD(test1)

##### Diferenças na percentagem de mud entre as classes do gradistat
test2<-aov(mud~Sed_class,data=GT_Final@data)
summary(test2)

TukeyHSD(test2)

##### Diferenças no phi entre as classes definidas no campo
test3<-aov(Mean.fw.phi~Class_2,data=GT_Final@data)
summary(test3)

TukeyHSD(test3)


##### Diferenças no phi entre as classes do gradistat
test4<-aov(Mean.fw.phi~Sed_class,data=GT_Final@data)
summary(test4)

TukeyHSD(test4)

##### Diferenças no median grain size distribution entre as classes definidas no campo
test5<-aov(GT_Final$`D50(um)`~Class_2,data=GT_Final@data)
summary(test5)

TukeyHSD(test5)


##### Diferenças no median grain size distribution entre as classes do gradistat
test6<-aov(GT_Final$`D50(um)`~Sed_class,data=GT_Final@data)
summary(test6)

TukeyHSD(test6)
