setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

##load packages
packs<-c("ggplot2","cluster","reshape","GGally","car")
lapply(packs,require,character.only=T)

##load data base created from script Intertidaç_class1
df<-read.table("./Data_in/DF_ex.csv",header=T,sep=";")
str(df)

ggpairs(df[,c(2:5,15,20:32)],lower = list(continuous="smooth_loess"))

scatterplotMatrix(~SAR_VV+SAR_VH+SAR_VH_VV+uca+perc_finos+cover_water,data=df,smoother=F)

## Compute NDWI and mNDWI

df$NDWI_1<-(df$B08_10-df$B12_10)/(df$B08_10+df$B12_10)
df$NDWI_2<-(df$B03_10-df$B08_10)/(df$B03_10+df$B08_10)
df$mNDWI<-(df$B03_10-df$B11_10)/(df$B03_10+df$B11_10)
df$SAR_VH_VV<-(df$SAR_VH/df$SAR_VV)

ggplot(df,aes(x=df$perc_finos,y=SAR_VV))+
  geom_point(size=2)+
  geom_smooth(method=lm,se=F)+
  facet_grid(uca~ .)

ggplot(df,aes(x=factor(uca),y=perc_finos))+
  geom_boxplot(outlier.color = "red")

ggplot(df,aes(x=SAR_VH,y=cover_water,col=class_1))+
  geom_point(size=2)+
  geom_smooth(method=lm,se=F)

# statistics: 

## Granulometria

### Perc finos tem efeito significativo no sinal de SAR VV, assim como a presença de uca. Não há interaçao entre uca e finos...
x<-lm(SAR_VV~factor(uca)*perc_finos,data=df)
summary(x)
anova(x)

### Também efeito significativo de perc finos tb no SAR VH, mas não de uca. Nao há interacção
x1<-lm(SAR_VH~factor(uca)*perc_finos,data=df)
summary(x1)
anova(x1)

### nao significativo, uca marginalmente significativo
x2<-lm(SAR_VH_VV~factor(uca)*perc_finos,data=df)
summary(x2)
anova(x2)


## Moisture

y<-lm(SAR_VV~cover_water,data=df)
summary(y)
anova(y)


y1<-lm(SAR_VH~cover_water,data=df)
summary(y1)
anova(y1)

y2<-lm(SAR_VH_VV~cover_water,data=df)
summary(y2)
anova(y2)



