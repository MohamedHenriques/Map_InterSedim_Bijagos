setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

## Pacotes
packs<-c("raster","ggplot2","rgdal","viridis","sp","RColorBrewer","scales","tools","rgeos","xlsx","spatstat")
lapply(packs,require,character.only=T)


## Load GNB shape
GNB<-readOGR("./Shapefiles/GNB/gnb_poly.shp")
crs(GNB)
# Change the projection to lat long
GNB1<-spTransform(GNB,crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
crs(GNB1)


GT<-readOGR("./Data_out/Polygons/poly_GT.shp")

plot(GNB1)
plot(GT, col="red")

GT_DF<-GT@data
str(GT_DF)

Gra<-read.xlsx("D:/Work/FCUL/Doutoramento/Capitulos/Mapping_intertidal_sediments/Data/Granulometry/Data_gran_20201005.XLSX",1)
str(Gra)

Gra$Sed_ID_f<-factor(Gra$Sed_ID)

Gra_t<-merge(Gra,GT_DF, by.x="Sed_ID_f",by.y="Point",all.x=T, all.y=F)
str(Gra_t)
Gra_t1<-Gra_t[-100,c(2,9:13,17:30)]



