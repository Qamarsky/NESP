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


##### Add in transect data ####

drop_auth()
drop_dir(path="/NESP")

#Covars<-drop_read_csv("/NESP/Data/Transect data/Transect_data_all_170217.csv", stringsAsFactors=FALSE)
Covars<-drop_read_csv("/NESP/data/transect data/Global/Global_dataset_290317_UID.csv", stringsAsFactors=FALSE)



Landcov<-drop_read_csv("/NESP/Data/landuse.csv")
KABsite<-drop_read_csv("/NESP/Data/Transect data/KAB/KAB_site_types.csv")

Covars$Year <-as.POSIXlt(strptime(as.character(Covars$Date), format = "%d/%m/%Y"))$year+1900
Covars$Month <- as.POSIXlt(strptime(as.character(Covars$Date), format = "%d/%m/%Y"))$mon+1
Covars$Day <- as.POSIXlt(strptime(as.character(Covars$Date), format = "%d/%m/%Y"))$mday

Covars$Source<-as.factor(Covars$Source)

##### KAB ######

## get total debris for KAB and put it into Covars dataset

KAB<- drop_read_csv ("/NESP/Data/Transect data/KAB/KAB_AllData_GlID_UID_22_2_17.csv", stringsAsFactors=FALSE)

KAB$Total_Debris<-rowSums(KAB[,grepl("No.of", names(KAB))], na.rm=TRUE)

KABmatch<-match(Covars$UID[Covars$Source=="KAB"], KAB$UID)

Covars$Total_Debris[Covars$Source=="KAB"]<-KAB$Total_Debris[KABmatch]

### Now let's look at CUA data ###


CUA <- drop_read_csv("/NESP/Data/Transect data/CUA/CUA_AllData3_UID_22_2_17.csv", stringsAsFactors=FALSE)

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



##### CSIRO DATA ######


CSIRO<-drop_read_csv ("/NESP/data/transect data/CSIRO/CSIRO_public_collection_only_22_2_17.csv")
CSIRO$Total_Debris<-rowSums(CSIRO[,5:236])  

CSmatch<-match(Covars$UID[Covars$Source=="CSIRO"|Covars$Source=="Transect"|Covars$Source=="Emu"], CSIRO$UID)

Covars$Total_Debris[Covars$Source=="CSIRO"|Covars$Source=="Transect"|Covars$Source=="Emu"]<-CSIRO$Total_Debris[CSmatch]


#### massaging various covariates
Covars$Area_m2<-as.numeric(Covars$Area_m2)
Covars$Area_m2[Covars$Source=="KAB"]<-1000

Covars$Totper1000<-(Covars$Total_Debris/Covars$Area_m2)*1000
Covars$All_roads_50<-rowSums(Covars[,64:68], na.rm=TRUE)
Covars$All_roads_5<-rowSums(Covars[,49:53], na.rm=TRUE)
Covars$Prim.land<-Landcov$PRIMARY_V7[match(Covars$Landuse_code, Landcov$Landuse)]
Covars$State<-as.factor(Covars$State)

Covars$roads_5to50km_resids <- lm(Covars$All_roads_5 ~ Covars$All_roads_50)$residuals
Covars$SiteType<-KABsite$site_type[match(Covars$Global_ID, KABsite$Global_ID)]  ### fix this
Covars$SiteType[Covars$SiteType=="Car park"]<-"Car Park"
Covars$SiteType[Covars$SiteType=="Retail"]<-"Retail Strip"
Covars$SiteType[Covars$SiteType=="Recreational"]<-"Recreational Park"
Covars$SiteType<-droplevels(Covars$SiteType)

## put in a log in case of doing gams.
Covars$Log_Debris<-log(Covars$Totper1000 +1)

## add number of drink containers ###

Covars$Containers<-KAB$Containers[match(Covars$UID, KAB$UID)]

## add in site code
Covars$Sitecode<-KAB$sitecode.x[match(Covars$UID, KAB$UID)]

### remove NAs in Totper1000 ##

Covars2<-Covars[is.finite(Covars$Totper1000)==TRUE,]

## remove NAs in Prim.land (these are all on Heron Island)

Covars2<-Covars2[is.na(Covars2$Prim.land)==FALSE,]

## remove NAs in pop5km (these are in Melbourne for some reason)

Covars2<-Covars2[is.na(Covars2$Pop_5km)==FALSE,]


Covars2$Pop5to50km_resids <- lm(Covars2$Pop_5km ~ Covars2$Pop_50km)$residuals





#drop_upload("~/Documents/R data/NESP/R scripts/LoadData.R", dest="/NESP/R scripts")

