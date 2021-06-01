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

packs<-c("randomForest","caret","sf","beepr","RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis","data.table","reshape2")
npacks <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(npacks)) install.packages(npacks)
#install_github("vqv/ggbiplot")
lapply(packs,require,character.only=T)

## Load sat img

sat<-stack("Data_out/Stack/Final_stack.tif") ##created in script GraVSSat_Preliminary
names(sat)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204",
              "B08A_20200204","B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","dem_104_469",
              "NDWI","mNDWI","NDMI","NDMI1","NDVI","RVI","VH_VV","MSAVI2","intensity","iv_multi","rededge_multi","rededge_sum",
              "visible_multi")

## transform sat stack in data frame
sat_dt<-as.data.table(sat)
str(sat_dt)



data<-DB1[apply(DB1[,!(1:3)],1,sum)!=0] ### Bray curtis does not work with empty cores. So we have to remove all of them
table(apply(data[,!c(1:3)], 2, sum)==0)

data1<-data[,!c(1:3,86)] ###NMDS requires a matrix of values only, so we need to remove the aggregating variables


data1[,dummy:=88.49558]

data2<-data[,c(1:3,86)] ###store aggregating variables to use latter

data1log<-log(data1+1)


################OVERALL SPATIAL AND TEMPORAL######### CANONICAL 

### CAP Total
###
data2[,site:=factor(site,levels=c("A","AB","BI","E","BR","AD"))]

set.seed(118)
system.time(
  OM1Tot<-CAPdiscrim(data1log~site*season,data=data2,dist="bray",axes=3,m=0,add=F, permutations = 1000)
)
beep(3)
summary(OM1Tot)
OM1Tot$manova
OM1Tot$m

##PERMANOVA
perTot<-adonis(data1log~site*season,data=data2,permutations = 1000)
perTot

distTot<-vegdist(data1log,method="bray")

set.seed(1000)
dispTot<-betadisper(distTot,group=data2$site)
permutest(dispTot)
boxplot(dispTot)

TukTot<-TukeyHSD(dispTot)
plot(TukTot)



#extract CAP scores (x and y coordinates)
data.scoresCAP_Tot = as.data.table(scores(OM1Tot))

#add columns to data frame 
data.scoresCAP_Tot$site = data2$site
data.scoresCAP_Tot$season = data2$season
head(data.scoresCAP_Tot)


###fit sps data in Tot
set.seed(119)
fitCAP_Tot<-envfit(OM1Tot,data1log,permutations=100000)
arrowCAP_Tot<-data.frame(fitCAP_Tot$vectors$arrows,R=fitCAP_Tot$vectors$r,P=fitCAP_Tot$vectors$pvals)
arrowCAP_Tot$FG <- rownames(arrowCAP_Tot)
arrowarrowCAP_Tot.p<-arrowCAP_Tot[arrowCAP_Tot$P<=0.05&arrowCAP_Tot$R>=0.15,]

##Site
PP_CAP_site<-ggplot(data.scoresCAP_Tot,aes(x=LD1,y=LD2))+
  geom_point(size=2,aes(colour=site))+
  #geom_text(aes(label=month),size=7)+
  labs(x="LD1",y="LD2",colour="Site",fill="Site")+
  stat_ellipse(size=2,type="t",aes(group=site,colour=site,fill=site),level=.6,geom="polygon",alpha=.2)+
  scale_colour_manual(values=p)+
  scale_fill_manual(values=p)+
  ggtitle("Canonical Discriminant per site")+
  theme_bw()
PP_CAP_site+
  geom_segment(data=arrowarrowCAP_Tot.p,aes(x=0,y=0,xend=LD1*R*7,yend=LD2*R*7),arrow=arrow(length=unit(.2,"cm")),col="grey40",lwd=1)+
  ggrepel::geom_text_repel(data=arrowarrowCAP_Tot.p,aes(x=LD1*R*7,y=LD2*R*7,label=FG),cex=5,direction="both",segment.size=0.25)




