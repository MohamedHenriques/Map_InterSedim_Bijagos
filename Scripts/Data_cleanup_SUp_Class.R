setwd("D:/Work/FCUL/Doutoramento/R/Mapping_coastal_Habitats_Guinea_Bissau/Github/Map_InterSedim_Bijagos")
rm(list=ls())
graphics.off()

packs<-c("randomForest","caret","sf","beepr","RStoolbox","raster","ggplot2","rgdal","viridis","randomForest","cluster","rasterVis","data.table","reshape2")
lapply(packs,require,character.only=T)

## Load sat img

sat<-stack("Data_out/Stack/Final_stack.tif") ##created in script GraVSSat_Preliminary
names(sat)<-c("B02_20200204","B03_20200204","B04_20200204","B05_20200204","B06_20200204","B07_20200204","B08_20200204","B08A_20200204",
         "B09_20200204","B11_20200204","B12_20200204","S1_20200128_VH","S1_20200128_VV","DEM","NDWI","mNDWI","NDMI","NDMI1")

##Load GT polygons (without Adonga)
GT<-readOGR("Data_out/Polygons/Poly_GT_Gra_ended_20210113.shp")
#plot(GT)
df<-data.table(GT@data)
str(df)
df[,unique(site)]

###Adonga
GT_adonga<-readOGR("D:/Work/FCUL/Doutoramento/Capitulos/Mapping_intertidal_sediments/Data/Data_groundtruthing/Adonga/Poligonos_GT/Poligonos_GT_total")
plot(GT_adonga, add=T, col="green")
View(GT_adonga@data)
dfa<-data.table(GT_adonga@data)
str(dfa)


### Crop GT to keep only points in the available scene
GT_c<-crop(GT,sat)
plot(GT_c)
rm(GT) #Remove object no longer needed

###Check database
str(GT_c@data)


###Correct class names
DF0<-as.data.table(GT_c@data)
str(DF0)

##remove point 3129 (see observations on that data entry)
DF<-DF0[!Point=="3129"]

##Convert uca cover percentage into numeric
DF[,c_uca:=as.numeric(as.character(c_uca))]
DF[,unique(c_uca)]
DF[is.na(c_uca),c_uca:=20] # see comments on this entry. no uca value attributed, will attribute now according to macroalgae cover
DF[is.na(c_uca)]

DF[,unique(Class_1)]
DF[,unique(Class_2)]
DF[,unique(Class_3)]

DF[,Class_11:=as.character(Class_1)]
DF[,Class_22:=as.character(Class_2)]
DF[,Class_33:=as.character(Class_3)]

DF[Class_33=="shell",unique(Class_22)]
DF[Class_33=="macro_gravel",unique(Class_22)]
DF[is.na(Class_33),unique(Class_22)]

### Clean classes in CLass_3
DF[Class_22=="water_body",unique(Class_33)]
DF[Class_22=="water_body",Class_33:="water_body"]

DF[Class_33=="rock_gravilha",Class_33:="gravel_rock"]

DF[is.na(Class_33),Class_33:="bare_sediment"]

###Clean Class_2
DF[is.na(Class_22),unique(Class_33)]

DF[Class_33=="macro_gravel"]
DF[Class_33=="macro_gravel"&Class_22=="muddy_sand",unique(Point)]
DF[Class_33=="macro_gravel",table(c_mcrph)]## Join to macroalgae for now

DF[Class_33=="macro_shell",table(c_mcrph)]
DF[Class_33=="macro_shell",table(c_shlls)] ##join macro_shell to macroalgae class

DF[Class_33=="macro_uca",table(Class_22)] ##macrouca is included in the "baresediment" for Class_2, as well as uca, which is fine
DF[Class_33=="macro_uca",table(c_mcrph)] ##but this poses problems regarding the macroalgae separation, since the large majority of these pixels have >90% macroalgala cover
DF[Class_33=="macro_uca",table(c_uca)]##join it to macroalgae class for now

DF[Class_33=="macro_uca",unique(Point)]
DF[Class_33=="macro_uca"&c_mcrph>=80]

###Check habitat classes
DF[,unique(Class_11)]
DF[,unique(Class_22)]
DF[is.na(Class_22),unique(Class_33)]

DF[,unique(Class_33)]

## Defining final habitat classes

DF[,cover_over:=Class_33][Class_33=="macro_shell",cover_over:="macroalgae"][Class_33=="macro_uca"|Class_33=="macro_gravel",cover_over:="macroalgae"][Class_33=="gravel"|Class_33=="rock_oyster"|Class_33=="gravel_rock",cover_over:="rock"]
DF[,unique(cover_over)]
#DF[,cover_over:=as.factor(cover_over)]

## rename pixels with over 50 water cover
#DF[c_water>=50,cover_over:="inundated"]

###checking percentage cover for each Class3

DF[cover_over=="bare_sediment",table(c_shlls)]
DF[cover_over=="uca",table(c_shlls)]
DF[cover_over=="rock",table(c_shlls)]
DF[cover_over=="shell",table(c_shlls)]
DF[cover_over=="macroalgae",table(c_shlls)]
DF[cover_over=="water_body",table(c_shlls)]
DF[cover_over=="rock",table(c_shlls)]
DF[cover_over=="shell",table(c_shlls)]

DF[cover_over=="bare_sediment",table(c_rocks)]
DF[cover_over=="uca",table(c_rocks)]
DF[cover_over=="rock",table(c_rocks)]
DF[cover_over=="shell",table(c_rocks)]
DF[cover_over=="macroalgae",table(c_rocks)]
DF[cover_over=="water_body",table(c_rocks)]
DF[cover_over=="rock",table(c_rocks)]

DF[cover_over=="bare_sediment",table(c_mcrph)]
DF[cover_over=="uca",table(c_mcrph)]
DF[cover_over=="rock",table(c_mcrph)]
DF[cover_over=="shell",table(c_mcrph)]
DF[cover_over=="macroalgae",table(c_mcrph)]
DF[cover_over=="water_body",table(c_mcrph)]
DF[cover_over=="uca",table(c_mcrph)]
DF[cover_over=="macroalgae",table(c_mcrph)]

DF[cover_over=="bare_sediment",table(c_uca)]
DF[cover_over=="uca",table(c_uca)]
DF[cover_over=="rock",table(c_uca)]
DF[cover_over=="shell",table(c_uca)]
DF[cover_over=="macroalgae",table(c_uca)]
DF[cover_over=="water_body",table(c_uca)]

DF[cover_over=="bare_sediment",table(c_water)]
DF[cover_over=="uca",table(c_water)]
DF[cover_over=="rock",table(c_water)]
DF[cover_over=="shell",table(c_water)]
DF[cover_over=="macroalgae",table(c_water)]
DF[cover_over=="water_body",table(c_water)]

##Clean up uca pixels
DF[cover_over=="uca"&c_mcrph==70,cover_over:="macroalgae"]
DF[cover_over=="uca",table(c_mcrph)]

DF[cover_over=="uca"&c_uca==0,cover_over:="bare_sediment"] ##polygons labelled as uca with zero uca cover, so swhitched them to bare_sediment
DF[cover_over=="uca"&c_uca==20,cover_over:="bare_sediment"]
DF[cover_over=="uca"&c_uca==30,cover_over:="bare_sediment"]

##Clean up macroalgae pixels
DF[cover_over=="macroalgae"&c_mcrph<30,cover_over:="bare_sediment"]
DF[cover_over=="macroalgae"&c_uca>=60&c_mcrph<80,cover_over:="uca"]

##Clean up shells pixels
DF[cover_over=="shell"&c_shlls<30,cover_over:="bare_sediment"]

##Clean up rock pixels
DF[cover_over=="rock"&c_rocks<=40] ###point 63 has 0 for c_rocks, but see comment. WIll leave the point classified as rock for now

##Clean up bare_sediment pixels
DF[cover_over=="bare_sediment"&c_shlls>=25, cover_over:="shell"]
DF[cover_over=="bare_sediment"&c_uca>=80&uc_dnst!="L",bare_sediment:="uca"] ##reallocate some polygons classified as bare-sediment to uca according to c_uca and uc_dnst

##Clean waterbody
DF[c_water>=85,cover_over:="water_body"]

###################################################
#####Creating new columns for habitat types of level 0

## Create new column to classify water body
DF[,WB:=ifelse(c_water>=85,"water_body","other")]
DF[,unique(WB)]



########################################
###########################################################
#########################################################################
###Remapping variables of cover over according to numeric percentage cover of each variable
DF[,unique(cover_over)]
DF[,cover_over1:=ifelse(c_uca>=50,"uca",
                        ifelse(c_rocks>=30,"rock",
                        ifelse(c_shlls>=30,"shell",
                        ifelse(c_mcrph>=30,"macroalgae",
                        ifelse(c_water>=85,"water_body",
                        ifelse(c_uca<=20&c_rocks<=20&c_shlls<=20&c_mcrph<=20&c_water<85,"bare_sediment","mixed"))))))]
DF[,unique(cover_over1)]


####################################################################################
########################################################################################
##creat unique remapped columns for individual classification of rock, macroalgae, shell etc

DF[,rocks:=ifelse(cover_over=="rock","rock","other")]
DF[,unique(rocks)]

DF[,macro:=ifelse(cover_over=="macroalgae","macroalgae","other")]
DF[,unique(macro)]
DF[macro=="macroalgae",table(c_uca)]

DF[,shells:=ifelse(cover_over=="shell","shell","other")]
DF[,unique(shells)]

DF[,sediment0:=ifelse(cover_over=="bare_sediment","bare_sediment",
                     ifelse(cover_over=="uca","uca",
                      ifelse(cover_over=="water_body","waterbody",NA)))]
DF[,unique(sediment0)]

###Introduce new columns on polygons
DF1<-DF[,.(Class_11,Class_22,Class_33,cover_over,cover_over1,WB,rocks, macro,shells,sediment0,Point)]

GT_c1<-merge(GT_c,DF1,by="Point")
plot(GT_c1)
str(GT_c1@data)

writeOGR(GT_c1,"Data_out/Polygons",layer="GT_c1",driver = "ESRI Shapefile",overwrite_layer = T)


