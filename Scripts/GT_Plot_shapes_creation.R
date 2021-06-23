setwd("c:/Doutoramento1/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

## Pacotes
packs<-c("raster","ggplot2","rgdal","viridis","sp","RColorBrewer","scales","tools","rgeos","xlsx","spatstat","beepr")
lapply(packs,require,character.only=T)


## Load GNB shape
GNB<-readOGR("./Shapefiles/GNB/gnb_poly.shp")
crs(GNB)
# Change the projection to lat long
GNB1<-spTransform(GNB,crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
crs(GNB1)

#my.palette<-rev(brewer.pal(10, "Paired"))

## load gps GT points

locations<-c("C:/Doutoramento1/Capitulos/Mapping_intertidal_sediments/Data/Data_groundtruthing/Formosa/urok_gt_gpx","C:/Doutoramento1/Capitulos/Mapping_intertidal_sediments/Data/Data_groundtruthing/Bubaque/bubaque_gt_gpx")

myfiles<-list.files(locations,pattern=".gpx",full.names=T)
myfiles0<-list.files(locations,pattern=".gpx",full.names=F)
myfiles1<-file_path_sans_ext(myfiles0)
a<-list()
my.palette<-rainbow(34)
plot(GNB1)
coord<-data.frame(coords.x1=NULL,coords.x2=NULL)
coord_id<-character()

for(i in 1:length(myfiles)) 
  {
a[[i]]<-readOGR(dsn=myfiles[i], layer="waypoints")
plot(a[[i]], add=T, col=my.palette[i])
coord<-rbind(coord,coordinates(a[[i]]))
coord_id<-c(coord_id,as.character(a[[i]]$name))
}

## replace Ids that were written like "061"
x<-as.character(c(61:99))
x1<-as.character(paste(0,x,sep=""))

for (i in 1:length(x1)) {
  coord_id[which(coord_id==x1[i])]<-x[i]
}

coord_final<-cbind(coord_id,coord)

which(coord_final$coord_id==61)

## Read database
DF<-read.table("C:/Doutoramento1/Capitulos/Mapping_intertidal_sediments/Data/Data_groundtruthing/poly_GT_final_20210622_sedID.csv",header=T,sep=";")
str(DF)

DF$Point<-as.character(DF$Point) #to allow it to merge well with the spatial points id. Factors suck, they have levels and shit

#DF0<-DF[-which(DF$Site=="Adonga"),] #new dataframe doesnt have Adonga data

DF1<-merge(DF,coord_final,by.x="Point",by.y="coord_id",all.x=T,all.y=T)
str(DF1)
names(DF1)[names(DF1)==c("coords.x1","coords.x2")]<-c("lon","lat")
colnames(DF1)

which(is.na(DF1$lon))
which(is.na(DF1$lat))

## Search for duplicates to correct the database and find what Id's should be renamed
DF1$Point[which(duplicated(DF1$Point))]
which(duplicated(DF1$Point))
DF1$Point[which(duplicated(DF1$Point))]


## Changing the Id of points from the GPS that had repeated ID's because of basecamp

mudar<-unique(DF1$Point[which(duplicated(DF1$Point))])
coord_b<-data.frame(coords.x1=NULL,coords.x2=NULL)
coord_id_b<-character()

for(i in 2:6){
  a[[i]]@data$name<-as.character(a[[i]]@data$name)
  ifelse(mean(a[[i]]@data$name%in%mudar)==0,print("no match"),print(c("match",a[[i]]@data$name[which(a[[i]]@data$name%in%mudar)])))
  a[[i]]@data$name<-replace(a[[i]]@data$name,which(a[[i]]@data$name%in%mudar),paste(a[[i]]@data$name[which(a[[i]]@data$name%in%mudar)],"100",sep=""))
  
}

b<-a
b[[6]]@data$name

for (i in 1:length(b)){
  coord_b<-rbind(coord_b,coordinates(b[[i]]))
coord_id_b<-c(coord_id_b,as.character(b[[i]]$name))
  }

## replace Ids that were written like "061"

for (i in 1:length(x1)) {
  coord_id_b[which(coord_id_b==x1[i])]<-x[i]
}

coord_final_b<-cbind(coord_id_b,coord_b)

which(coord_final_b$coord_id_b==61)

## Remerge 

DF2<-merge(DF,coord_final_b,by.x="Point",by.y="coord_id_b",all.x=T,all.y=T)
str(DF2)
names(DF2)[names(DF2)==c("coords.x1","coords.x2")]<-c("lon","lat")
colnames(DF2)

which(is.na(DF2$lon))
which(is.na(DF2$lat))

## Search for duplicates to correct the database and find what Id's should be renamed
DF2$Point[which(duplicated(DF2$Point))]
which(duplicated(DF2$Point))


## Plot points to check them
GNB2<-fortify(GNB1)

ggplot()+
  geom_polygon(data=GNB2,aes(x=long,y=lat,group=group), fill="lightgrey")+
  geom_point(data=DF2,aes(x=lon,y=lat,colour=factor(Day)))+
  theme_bw()
  
## aggregate database with points


## build shape files
coordinates(DF2)<-c("lon","lat")
crs(DF2)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
c<-spTransform(DF2,CRS("+init=epsg:32628"))
crs(c)
c$Radius<-as.numeric(as.character(c$Radius))

c$Radius[which(is.na(c$Radius))]<-0

poly_GT<-gBuffer(c,width=c$Radius,byid=T)

plot(poly_GT[which(poly_GT$Point=="3333100"),], col="red")

plot(poly_GT[which(poly_GT$Point=="3323100"|poly_GT$Point=="3333100"),], col=c("blue","red"))

#writeOGR(poly_GT,"./Data_out/Polygons",layer="poly_GT_20210113",driver="ESRI Shapefile",overwrite=F)
writeOGR(poly_GT,"./Data_out/Polygons",layer="poly_GT_20210622",driver="ESRI Shapefile",overwrite=F)
