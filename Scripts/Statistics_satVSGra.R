setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

packs<-c("Hmisc","sf","beepr","RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis","data.table","reshape2","corrplot")
lapply(packs,require,character.only=T)

## Load DB for Urok, created in script GraVSSSat_Preliminary
m1<-fread("Data_out/db/GraVSSat_db_20201221.csv")
str(m1)

### Checking for overlayed polygons
m1[,.(numbcell=length(unique(cell)))]

m1$cell[duplicated(m1$cell)] # These pixels are duplicated in different polygons. Ignore now but deal with later

### Clean database and select only columns that matter
db<-m1[,!c("Ãƒ___Day","Site","Radius","uc_dnst","greenss","mangrov","sed_Id","obs","obs2","finos","Sed_ID_1","sedID","Sd_clss","Clss_22"),with=F]

##Compute correlations between numerical variables
dbc<-db[,c(2:16,21:31,34:38)]
dbc1<-sapply(dbc, as.numeric)
res<-rcorr(dbc1)
resr<-round(res$r,2)
resp<-round(res$P,2)

# Insignificant correlation are crossed
corrplot(resr,type="full",order="hclust",p.mat=resp,sig.level=0.05,insig="pch")
