###### Here is the url on how to use GitHub and RStudio together ####
#### note that you have to open a shell to add in new branches ###
### http://r-bio.github.io/intro-git-rstudio/ ###

##Open shell (Tools>Shell in R Studio) and give it the upstream address:
## git remote add upstream https://github.com/Qamarsky/NESP.git. 
## Make sure that it worked by typing git remote -v, 
## it should display 4 lines, 2 that start with origin and the address of your fork, 
## and 2 that start with upstream and the address of the upstream repository. 

# Note that here we used upstream to name the upstream repository but we could have given it another name

## Create a branch for the changes in R studio through the shell: 
## git checkout -b proposed-fixes master. Proposed-fixes is our branch name within this R studio project.


### Once you have made changes, go to the GIT icon in the menu bar and commit changes. 

## once changes are finished, open shell and type 'git push origin proposed-fixes' (Note that
# proposed-fixes is the name of the branch you chose when you originated this branch)



### to get the newest version of the files into R's folder use the command: git pull <remote> <branch>
## In this case we have set the remote file to be called origin, and the branch we are working on is master
## so command is: 'git pull origin master'


rm(list = ls())

#### required libraries ####
library(partykit)
library(maps)
library(ggmap)
library(mgcv)
library(ggplot2)
library(googleVis)
library(multcomp)
library(rdrop2)
library(httpuv)
library(Hmisc)


##### Add in transect data ####

drop_auth()
drop_dir(path="/NESP")

setwd("/Users/uqqschuy/Documents/R data/NESP/")

#Covars<-drop_read_csv("/NESP/Data/Transect data/Transect_data_all_170217.csv", stringsAsFactors=FALSE)
#Covarsx<-drop_read_csv("/NESP/data/transect data/Global/Global_dataset_290317_UID.csv", stringsAsFactors=FALSE)

Covars<-drop_read_csv("/NESP/data/transect data/Global/Global_dataset_170516_UID_SEIF2011RoadsFix.csv", stringsAsFactors=FALSE)
Landcov<-drop_read_csv("/NESP/Data/landuse.csv")
KABsite<-drop_read_csv("/NESP/Data/Transect data/KAB/KAB_site_types.csv")
KAB<- drop_read_csv ("/NESP/Data/Transect data/KAB/KAB_AllData_GlID_UID_08_05_17.csv", stringsAsFactors=FALSE)
CUA <- drop_read_csv("/NESP/Data/Transect data/CUA/CUA_AllData3_UID_22_2_17.csv", stringsAsFactors=FALSE)
CSIRO<-drop_read_csv ("/NESP/data/transect data/CSIRO/CSIRO_public_collection_only_22_2_17.csv")

Grid<-drop_read_csv("NESP/Data/Grid data/Syd_fishnet_centerpoints_covars_200430_inland.csv", stringsAsFactors=FALSE)

################## SKIP IF INTERNET CONNECTED ######################
############## for non-internet version: #########################

#write.csv(Grid, file="/Users/uqqschuy/Documents/R data/NESP/Syd_fishnet_centerpoints_covars_200430_inland.csv")

Covars<-read.csv("Global_dataset_80517_UID.csv", stringsAsFactors=FALSE) ### update this!!! ###
Landcov<-read.csv("landuse.csv")
KABsite<-read.csv("KAB_site_types.csv")
KAB<-read.csv("KAB_AllData_GlID_UID_08_05_17.csv") 
CUA <- read.csv("CUA_AllData3_UID_22_2_17.csv", stringsAsFactors=FALSE)
CSIRO<-read.csv ("CSIRO_public_collection_only_22_2_17.csv")

Grid<-read.csv("Syd_fishnet_centerpoints_covars_200430_inland.csv", stringsAsFactors=FALSE)


################## START HERE ######################

Covars$Year <-as.POSIXlt(strptime(as.character(Covars$Date), format = "%d/%m/%Y"))$year+1900
Covars$Month <- as.POSIXlt(strptime(as.character(Covars$Date), format = "%d/%m/%Y"))$mon+1
Covars$Day <- as.POSIXlt(strptime(as.character(Covars$Date), format = "%d/%m/%Y"))$mday

Covars$Source<-as.factor(Covars$Source)
Covars$State<-as.factor(Covars$State)



##### KAB ######

## get total debris for KAB and put it into Covars dataset

KAB$Total_Debris<-rowSums(KAB[,grepl("No.of", names(KAB))], na.rm=TRUE)

KABmatch<-match(Covars$UID[Covars$Source=="KAB"], KAB$UID)

Covars$Total_Debris[Covars$Source=="KAB"]<-KAB$Total_Debris[KABmatch]

### Now let's look at CUA data ###

### Many of the categories are for some reason not numeric. Change those to numeric and then you can add the columns to get a total

CUA[,88]<-as.numeric(paste(CUA[,88]))
CUA[,89]<-as.numeric(paste(CUA[,89]))
CUA[,93]<-as.numeric(paste(CUA[,93]))
CUA[,94]<-as.numeric(paste(CUA[,94]))
CUA[,98]<-as.numeric(paste(CUA[,98]))
CUA[,99]<-as.numeric(paste(CUA[,99]))
CUA[,100]<-as.numeric(paste(CUA[,100]))
CUA[,101]<-as.numeric(paste(CUA[,101]))
CUA[,107]<-as.numeric(paste(CUA[,107]))
CUA[,114]<-as.numeric(paste(CUA[,114]))
CUA[,119]<-as.numeric(paste(CUA[,119]))
CUA[,120]<-as.numeric(paste(CUA[,120]))
CUA[,122]<-as.numeric(paste(CUA[,122]))
CUA[,123]<-as.numeric(paste(CUA[,123]))
CUA[,124]<-as.numeric(paste(CUA[,124]))
CUA[,125]<-as.numeric(paste(CUA[,125]))
CUA[,128]<-as.numeric(paste(CUA[,128]))
CUA[,135]<-as.numeric(paste(CUA[,135]))
CUA[,141]<-as.numeric(paste(CUA[,141]))
CUA[,143]<-as.numeric(paste(CUA[,143]))
CUA[,145]<-as.numeric(paste(CUA[,145]))
CUA[,147]<-as.numeric(paste(CUA[,147]))
CUA[,149]<-as.numeric(paste(CUA[,149]))
CUA[,150]<-as.numeric(paste(CUA[,150]))
CUA[,152]<-as.numeric(paste(CUA[,152]))
CUA[,156]<-as.numeric(paste(CUA[,156]))
CUA[,173]<-as.numeric(paste(CUA[,173]))

CUA$Total_Debris<-rowSums(CUA[,c(87:109,112:159,161,163:180)], na.rm=TRUE)  ## for some reason many of these rows are not numeric

CUA$Latitude<--(CUA$Latitude)

### get site type

CUA$SiteType<-"Other"
CUA$SiteType[CUA$River=="Y" | CUA$River=="y"]<-"River"
CUA$SiteType[CUA$Parks=="Y" | CUA$Parks=="y"]<-"Recreational Park"
CUA$SiteType[CUA$Beach=="Y" | CUA$Beach=="y"]<-"Beach"
CUA$SiteType[CUA$Roadway=="Y" | CUA$Roadway=="y"]<-"Highway"
CUA$SiteType[CUA$PubBushland=="Y" | CUA$PubBushland=="y"]<-"Bushland"
CUA$SiteType[CUA$School=="Y" | CUA$School=="y"]<-"School"
CUA$SiteType[CUA$OutdoorTrans=="Y" | CUA$OutdoorTrans=="y"]<-"OutdoorTrans"
CUA$SiteType[CUA$ShopsMalls=="Y" | CUA$ShopsMalls=="y"]<-"Shopping Centre"
CUA$SiteType[CUA$DiveSite=="Y" | CUA$DiveSite=="y"]<-"DiveSite"


#### Total takes into account the length in meters of wire, pvc, etc. While total calc does not.


### first would be great to standardise by something... ###
### KAB data theoretically is standardised to a 1000m square area, according to cleanup protocols ###

### standardise CUA by Aream2 because Area km2 has lots of NAs for some reason. Let's standardise to 1000m2 like KAB

## Aream2 is a factor, so we have to change it to numeric. 
CUA$Aream2<-as.numeric(CUA$Aream2)

CUA$Totalper1000m2<-CUA$Total_Debris/CUA$Aream2*1000

CUA.Sub<-CUA[is.na(CUA$Totalper1000m2)==FALSE,]


CUAmatch<-match(Covars$UID[Covars$Source=="CUA"], CUA$UID)

Covars$Total_Debris[Covars$Source=="CUA"]<-CUA$Total_Debris[CUAmatch]


Covars$SiteType[Covars$Source=="CUA"]<-CUA$SiteType[CUAmatch]


##### CSIRO DATA ######



CSIRO$Total_Debris<-rowSums(CSIRO[,5:236])  

CSmatch<-match(Covars$UID[Covars$Source=="CSIRO"|Covars$Source=="Transect"|Covars$Source=="Emu"], CSIRO$UID)

Covars$Total_Debris[Covars$Source=="CSIRO"|Covars$Source=="Transect"|Covars$Source=="Emu"]<-CSIRO$Total_Debris[CSmatch]


#### massaging various covariates
#Covars$Area_m2<-as.numeric(Covars$Area_m2)
Covars$Area_m2[Covars$Source=="KAB"]<-1000

Covars$Totper1000<-(Covars$Total_Debris/Covars$Area_m2)*1000
Covars$All_roads_50<-rowSums(Covars[,c("DualCarriageRd50km","MinorRd50km","PrincialRd50km","SecondaryRd50km","Track50km")], na.rm=TRUE) 
Covars$All_roads_5<-rowSums(Covars[,c("DualCarriageRd5km","MinorRd5km","PrincialRd5km","SecondaryRd5km","Track5km")], na.rm=TRUE)   
Covars$Prim.land<-Landcov$PRIMARY_V7[match(Covars$Landuse_code, Landcov$Landuse)]
#Covars$State<-as.factor(Covars$State)

Covars$roads_5to50km_resids <- lm(Covars$All_roads_5 ~ Covars$All_roads_50)$residuals

Covars$Pop_5km[is.na(Covars$Pop_5km)==TRUE]<-0

Covars$Pop5to50km_resids<-lm(Covars$Pop_5km ~ Covars$Pop_50km)$residuals
Covars$SiteType[Covars$Source=="KAB"]<-as.character(KABsite$site_type[match(Covars$Global_ID[Covars$Source=="KAB"], KABsite$Global_ID)])  ### fix this
Covars$SiteType[Covars$SiteType=="Car park"]<-"Car Park"
Covars$SiteType[Covars$SiteType=="Retail"]<-"Retail Strip"
Covars$SiteType[Covars$SiteType=="Recreational"]<-"Recreational Park"
Covars$SiteType<-as.factor(Covars$SiteType)

## put in a log in case of doing gams.
Covars$Log_Debris<-log(Covars$Totper1000 +1)

## add number of drink containers ### - FIX THIS??

## Covars$Containers<-KAB$Containers[match(Covars$UID, KAB$UID)]

## add in site code
Covars$Sitecode<-KAB$sitecode.x[match(Covars$UID, KAB$UID)]



### remove NAs in Totper1000 ##


Covars2<-Covars[is.finite(Covars$Totper1000)==TRUE,]

## remove NAs in Prim.land (these are all on Heron Island)

Covars2<-Covars2[is.na(Covars2$Prim.land)==FALSE,]

### remove the two outliers (>10000 items - I think they have been mis-recorded)

Covars2<-Covars2[Covars2$Total_Debris<10000,]

## remove NAs in pop5km (these are in Melbourne for some reason)

#Covars2<-Covars2[is.na(Covars2$Pop_5km)==FALSE,]

## remove ones without a date

#Covars2[is.na(Covars2$Year)==TRUE,] ## I think this will take out all of the new data. Let's see if we can find a year for these


## problems with the SEIF missing some bits

IDs<-Covars2$Global_ID[is.na(Covars2$eco_resour50km)==TRUE & is.na(Covars$eco_resour25km)==FALSE]
write.csv(Covars2[is.na(Covars2$eco_resour50km)==TRUE & is.na(Covars$eco_resour25km)==FALSE,], "missingdata.csv")

IDs2<-Covars2$Global_ID[is.na(Covars2$Edu_occupa5km)==TRUE & is.na(Covars2$Edu_occupa1km)==FALSE]

######  Grid data #######


#GridSEIF<-drop_read_csv("NESP/Data/Grid data/Sydney_fishnet_centrepoints_seif2011_170412.csv", stringsAsFactors=FALSE) ### fix file name

#Grid[,17:36]<-GridSEIF[,6:25]

#write.csv(Grid, file="/Users/uqqschuy/Documents/R data/NESP/Syd_fishnet_centerpoints_covars_200430_inland.csv")

#drop_upload("/Users/uqqschuy/Documents/R data/NESP/Syd_fishnet_centerpoints_covars_200430_inland.csv", dest="/NESP/Data/Grid data")


Grid$State<-rep("NSW", times=dim(Grid)[1])

Grid$All_roads_50<-rowSums(Grid[,c("DualCarriageRd50km","MinorRd50km","PrincialRd50km","SecondaryRd50km","Track50km")], na.rm=TRUE)
Grid$All_roads_5<-rowSums(Grid[,c("DualCarriageRd50km","MinorRd50km","PrincialRd50km","SecondaryRd50km","Track50km")], na.rm=TRUE)



#Gridtest<-read.csv("/Users/uqqschuy/Documents/R data/NESP/NESP/Sydney_fishnet_centrepoints_seif2011_170412.csv", stringsAsFactors=FALSE)


#### There are a few where landuse is -9999 because the cells are close to the water or on the water. 
# Change these to water landcover

## Note that there were other cells where landuse was 0, TJ has changed them to nearest landuse value. 

Grid$Landuse[Grid$Landuse<0]<-"663"

Grid$Prim.land<-Landcov$PRIMARY_V7[match(Grid$Landuse, Landcov$Landuse)]

Grid$roads_5to50km_resids<-lm(Grid$All_roads_5 ~ Grid$All_roads_50)$residuals
Grid$Pop5to50km_resids<-lm(Grid$Pop_5km ~ Grid$Pop_50km)$residuals

#### CHECKING DATA #####
#length(Grid2$UID[Grid2$Landuse==(-9999)])
### For some reason there are missing 5 and 50km roads. 
wrongroads<-Grid$UID[Grid$All_roads_5==0| Grid$All_roads_50==0]

write.csv (wrongroads, file="anomalousroads.csv")
#### Note that these predictions are using incorrect roads data - need to fix. 

# Grid$pred<-predict(G.K.M2, newdata=Grid,type="response",se.fit = TRUE, na.action = na.pass)

Syd_Covars<-Covars2[Covars2$Lat <= (-33.671774)  & Covars2$Lat >= (-34.265774) & Covars2$Long <= (151.372906) & Covars2$Long >= (150.718096),]



Syd_KAB<-Syd_Covars[Syd_Covars$Source=="KAB",]
Syd_CSIRO<-Syd_Covars[Syd_Covars$Source=="CSIRO",]
Syd_CSall<-Syd_Covars[Syd_Covars$Source=="Emu" | Syd_Covars$Source==
                        "Transect" | Syd_Covars$Source=="CSIRO",]



###### TEST GRID COVARS AGAINST TRANSECT COVARS #####

Grid_subset<-Grid[Grid$UID %in% unique(Syd_Covars$UID_1),]
## for test
#Gridtest_subset<-Gridtest[Gridtest$UID %in% unique(Syd_Covars$UID_1),]

Syd_Covars_subset<-Syd_Covars[unique(Syd_Covars$UID_1),]

matchindex<-match(Grid_subset$UID, Syd_Covars$UID_1)



plot(Grid_subset$Eco_advan_50km, Syd_Covars$Eco_advan_50km[matchindex])

plot(Grid_subset$eco_resour50km, Syd_Covars$eco_resour50km[matchindex])

## with new grid variable
plot(Gridtest_subset$eco_resour5km, Syd_Covars$eco_resour5km[matchindex])


plot(Grid_subset$Pop_25km, Syd_Covars$Pop_25km_new[matchindex])


### some issues...trying to work them out ####

Syd_subset<-Syd_Covars[matchindex,]
index<-Syd_subset$Pop_50km<2000000
plot(Grid_subset$Pop_50km[index], Syd_subset$Pop_50km[index])
index2<-Syd_subset$Pop_25<400000
plot(Grid_subset$Pop_25km[index2],Syd_subset$Pop_25km[index2])

index3<-Grid_subset$UID[Grid_subset$eco_resour50km<850]

write.csv(Syd_subset[index2,], file="25km anomalies for TJ.csv")
write.csv(Syd_subset[Syd_subset$UID_1 %in% index3,],"50km eco_resource anomaly for TJ.csv")

### a few of the transects don't end up in the grid, I think because the grid cell was perhaps cut off. 
## How shall we address this?

Grid$UID<-as.character(Grid$UID)
write.csv(unique(Syd_Covars$UID_1[Syd_Covars$UID_1 %nin% Grid$UID]), file="transectinwater.csv")


Syd_UID2<-paste(Syd_Covars$Long, Syd_Covars$Lat, sep="")
Grid_UID2<-paste(Grid$)


## TJ went back and changed the UID for these transects to the nearest UID. 


## provide csv to Chris and Kimberley because they will need for Winddf and Waterdf and Distdf
write.csv(Grid[,c("UID","X","Y")], file="new UIDs for transit matrices.csv")




#drop_upload("~/Documents/R data/NESP/R scripts/LoadData.R", dest="/NESP/R scripts")

