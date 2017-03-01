Covars2<-Covars

######## Ctree #########
### Now let's start to get some tree action happening. First try CUA and KAB separately

CUA.M1<-ctree(Totalper1000m2~State+ RailDist + TotalRds50 + Prim.land + pop50km + N.Vols + IllegalDump, data=CUA.Sub)

plot(CUA.M1, terminal_panel=node_barplot)
plot(CUA.M1)

### try using original data, and after dropping off two top CUA surveys
CUA.Sub2<-CUA.Sub[CUA.Sub$Totalcalc<10000,]
CUA.M2<-ctree(Totalcalc~State + RailDist + TotalRds50 + Prim.land + pop50km + N.Vols + IllegalDump, data=CUA.Sub2)

###### Now with new covars ###

G.K.M1<-ctree(TotalDebris ~ RailDistKM + State + All_roads_50 + Prim.land + Pop_25km + 
                eco_resour50km + Eco_advan_50km + Edu_occupa50km + eco_disadv50km,
              data=Covars2[Covars2$Source=="KAB",])

G.Cu.M1<-ctree(TotalDebris ~ RailDistKM + State + All_roads_50 + Prim.land + Pop_50km + 
                 eco_resour50km + Eco_advan_50km + Edu_occupa50km + eco_disadv50km,
               data=Covars2[Covars2$Source=="CUA",])

G.Cu.M2<-ctree(Totper1000 ~ RailDistKM + State + All_roads_50 + Prim.land + Pop_50km + 
                 eco_resour50km + Eco_advan_50km + Edu_occupa50km + eco_disadv50km,
               data=Covars2[Covars2$Source=="CUA",])

G.Cs.M1<-ctree(TotalDebris ~ RailDistKM + State + All_roads_50 + Prim.land + Pop_50km + 
                 eco_resour50km + Eco_advan_50km + Edu_occupa50km + eco_disadv50km,
               data=Covars2[Covars2$Source=="CSIRO",])

G.Cs.M2<-ctree(Totper1000 ~ RailDistKM + State + All_roads_50 + Prim.land + Pop_50km + 
                 eco_resour50km + Eco_advan_50km + Edu_occupa50km + eco_disadv50km,
               data=Covars2[Covars2$Source=="CSIRO",])


## add in the resids

G.K.M2<-ctree(TotalDebris ~ RailDistKM + State + All_roads_50 + Prim.land + Pop_50km + 
                Eco_resour_50km + Eco_advan_50km + Edu_occupa_50km + Eco_disadv_50km + 
                roads_5to50km_resids + Pop5to50km_resids,
              data=Covars2[Covars2$Source=="KAB",])


## Predictions on CSIRO dat

G.C.M1<-ctree(Totper1000 ~ RailDistKM + State + All_roads_50 + Prim.land + Pop_50km + 
                Eco_resour_50km + Eco_advan_50km + Edu_occupa_50km + Eco_disadv_50km + 
                roads_5to50km_resids + Pop5to50km_resids,
              data=Covars2[Covars2$Source=="CSIRO",])

G.Call.M1<-ctree(Totper1000 ~ RailDistKM + State + All_roads_50 + Prim.land + Pop_50km + 
                   Eco_resour_50km + Eco_advan_50km + Edu_occupa_50km + Eco_disadv_50km + 
                   roads_5to50km_resids + Pop5to50km_resids,
                 data=Covars2[Covars2$Source=="Emu" | Covars2$Source=="Transect" | Covars2$Source=="CSIRO",])

quartz()
plot(G.K.M2, type="simple", cex=0.5)



### create predictions with grid cells ####

Grid<-drop_read_csv("NESP/Grid/grid_covars_100816.csv") ### fix file name
Grid$State<-as.factor(rep("NSW", times=dim(Grid)[1]))
Grid$Prim.land<-Landcov$PRIMARY_V7[match(Grid$Landuse, Landcov$Landuse)]


Grid$pred<-predict(G.K.M2, newdata=Grid,type="response",se.fit = TRUE, na.action = na.pass)

### Can't do Grid predictions, so let's look at transect sites in Sydney only.

Syd_Covars<-Covars2[Covars2$Lat < (-33.671774)  & Covars2$Lat > (-34.265774) & Covars2$Long < (151.372906) & Covars2$Lat > (150.718096)]

Syd_KAB<-Syd_Covars[Syd_Covars$Source=="KAB"]
Syd_CSIRO<-Syd_Covars[Syd_Covars$Source=="CSIRO",]
Syd_CSall<-Syd_Covars[Syd_Covars$Source=="Emu" | Syd_Covars$Source==
                           "Transect" | Syd_Covars$Source=="CSIRO",]

Syd_KAB$pred<-predict(G.K.M2, newdata=Syd_KAB, type="response", se.fit=TRUE, na.action=na.pass)
Syd_KAB$resids<-Syd_KAB$Totper1000-Syd_KAB$pred

Syd_CSIRO$pred<-predict(G.C.M1, newdata=Syd_CSIRO, type="response", se.fit=TRUE, na.action=na.pass)
Syd_CSIRO$resids<-Syd_CSIRO$Totper1000-Syd_CSIRO$pred

Syd_CSall$pred<-predict(G.Call.M1, newdata=Syd_CSall, type="response", se.fit=TRUE,na.action=na.pass)
Syd_CSall$resids<-Syd_CSall$Totper1000-Syd_CSall$pred

Syd_all<-rbind(Syd_CSIRO, Syd_KAB)

###### Make predictions with wind and water transport. ####

# Given a transit matrix (e.g. wind)

## To get distribution from SOURCE (Cols) - First normalise proportions by row

#system.time(Wind<-read.csv("~/Documents/R data/NOAAOC/APC/Wind transport matrix unique", sep=","))

Winddf<-drop_read_csv("NESP/Wind transport matrix unique")


## importing distance matrix one by one - deprecated if Sydney Dist matrix is smaller!!

Distdf<-read.csv("~/Documents/R data/NOAAOC/APC/Distance matrix unique", nrows=50000)
Distdf2<-read.csv("~/Documents/R data/NOAAOC/APC/Distance matrix unique", skip=50000, nrows=50000)
Distdf3<-read.csv("~/Documents/R data/NOAAOC/APC/Distance matrix unique", skip=100000, nrows=50000)
Distdf4<-read.csv("~/Documents/R data/NOAAOC/APC/Distance matrix unique", skip=150000, nrows=50000)
Distdf5<-read.csv("~/Documents/R data/NOAAOC/APC/Distance matrix unique", skip=200000, nrows=50000)
Distdf6<-read.csv("~/Documents/R data/NOAAOC/APC/Distance matrix unique", skip=250000, nrows=50000)
Distdf7<-read.csv("~/Documents/R data/NOAAOC/APC/Distance matrix unique", skip=300000)

names(Distdf2)<-names(Distdf)
names(Distdf3)<-names(Distdf)
names(Distdf4)<-names(Distdf)
names(Distdf5)<-names(Distdf)
names(Distdf6)<-names(Distdf)
names(Distdf7)<-names(Distdf)

Distrest<-rbind(Distdf2, Distdf3, Distdf4, Distdf5, Distdf6, Distdf7)
rownames(Distrest)<-Distrest[,1]
Distrest<-Distrest[,-1]
names(Distrest)<-names(Distdf)

Distall<-rbind(Distdf, Distrest)
save(Distall, file="Distdf")
Distdf<-Distall


WindGridMatch<-colnames(Winddf) ## might need to play with this somewhat...
WindGridMatch<-substring(WindGridMatch,2)
WindGridMatch<-as.numeric(WindGridMatch)

TotalDebris<-read.csv("~/Documents/R data/NOAAOC/APC/predicted load on the grid (1).csv")
rownames(TotalDebris)<-rownames(Winddf)
TotalDebris$Unique_ID<-rownames(Winddf)
#TotalDebris<-as.vector(TotalDebris)

## because we don't actually have total debris predicted per each cell, the best hyptheses we can make are:
# 1) Amont of debris is somehow related to how many upwind cells there are (upwind)
# 2) Amount of debris is somehow related to how much total wind comes from those cells
##  This you would do by adding the total positive values for every column
## 3) And finally, you could relate to distance. The greater the distance, the less would be transported
## so for this, take the sign of each wind direction, and multiply it by the distance. Then do an inverse
## for each cell, so greater distances have less value. Then just add the postive values. This gives you basically the number
## of upwind cells, weighted by distance.
# 4) and of course finally you have straight predictions. 

## 1) how many cells in each column are positive??
upwind<-apply(Winddf, 2, function(x) { sum (x>0) })

# this one only valid if we have debris preds for all grid cells 
WindSinkPos<-Winddf
WindSinkPos[WindSinkPos<0]<-0  ## basically changes all negative values to zeros - only counting the proportion of the debris that comes FROM sites

## Next few lines are for when we have debris 

WindSinkPropPos<-sweep(WindSinkPos,2,colSums(WindSinkPos),`/`)

# multiply proportion of debris in each cell that came frWindSinkTot<-sweep(WindSinkPropPos,1,TotalDebris,'*')
WindSinkTot<-WindSinkPropPos*TotalDebris[,1]
WindSinkTotf<-colSums(WindSinkTot) ###MUST MATCH THIS WITH PROPER GRIDMATCH VALUES because total debris may be in different order from wind. 

## This WindSinkPropPos gives me the proportion of the debris that is in my cell that came FROM every other cell.
## so you can create a prediction grid (or portion thereof) that presumes that all cells are sinks, and all of the
## debris in them came from somewhere else. So if we multiply every colsum proportion by the total amount of
## debris that was predicted for each of our sites (since it's a subset), then we can ....



#2 Just use straight colsums (for pos values)
WindtotNoDeb<-colSums(WindSinkPos)

#3) 

Windsign<-Winddf
Windsign[Winddf>0]<-1
Windsign[Winddf<0]<-(-1)

## We can do upwind cells times debris in each of those upwind cells. 
Windsignpos<-Windsign
Windsignpos[Windsignpos<0]<-0
UpwindDeb<-colSums(Windsignpos*TotalDebris[,1])


## Now we do upwind cells divided by distance
Winddist<-Windsign/Distdf
is.na(Winddist) <- do.call(cbind,lapply(Winddist, is.infinite)) ## change Inf values to NA

WinddisttotNodeb<-colSums(Winddist, na.rm=TRUE) ## this is of course both pos and neg.
Winddistpos<-Windsign
Winddistpos[Winddistpos<0]<-0
Winddistpos<-Winddistpos/Distdf

is.na(Winddistpos) <- do.call(cbind,lapply(Winddistpos, is.infinite)) ## change Inf values to NA

Winddistposonly<-colSums(Winddistpos, na.rm=TRUE)

## Now let's do wind dist proportional, and multiply by debris

Winddistprop<-sweep(Winddistpos,2,Winddistposonly,`/`) ##That way we don't run into NA values
Winddistdeb<-colSums(Winddistprop*TotalDebris[,1], na.rm=T)

wind<-data.frame(upwind, WindtotNoDeb,WinddisttotNodeb, Winddistposonly, WindSinkTotf, UpwindDeb, Winddistdeb, WindGridMatch)

save(wind, file="wind")
write.csv(wind, file="wind.csv")

#WindSourceProp[data cols, not totals]<-Wind/rowSums(Wind[data cols])
#WindSourceTot<-WindSourceProp[data columns]*Total_Debris
#WindSourceTotf<-ColSums(WindSourceTot)

## Water matrix ###

Water<-read.csv("~/Documents/R data/NOAAOC/APC/grid_water_sites_gridmatch_nodup.csv")
Waternames<-Water[,1]

Water<-Water[,-1]

save(Water, file="Water")

Elevation<-read.csv("~/Documents/R data/NOAAOC/APC/grid_covars_100816_wtshd.csv")

WaterGridMatch<-colnames(Water) ## might need to play with this somewhat...
WaterGridMatch<-substring(WaterGridMatch,2)
WaterGridMatch<-as.numeric(WaterGridMatch)
WaterGridMatch<-data.frame(WaterGridMatch, WaterGridMatch)
names(WaterGridMatch)<-c("WGM", "Elev")

WaterGridMatch$Elev<-Elevation$Elevation[match(WaterGridMatch$WGM, Elevation$Unique_ID)]
WaterGridMatch$Watershed<-Elevation$Watershed[match(WaterGridMatch$WGM,Elevation$Unique_ID)]

## This will get the difference in elevation between each from to each to.

ElevRow<-as.data.frame(matrix(rep(Elevation$Elevation,each=416), ncol=416, byrow=TRUE))

colnames(ElevRow)<-WaterGridMatch$Watershed

ElevDiff<-sweep(ElevRow,2,WaterGridMatch$Elev, FUN="-")
rownames(ElevDiff)<-Waternames
ElevDiffNA<-ElevDiff
colnames(ElevDiffNA)<-WaterGridMatch$WGM


##Now we have to remove ones that are not the same watershed

for (i in 1: dim(ElevDiffNA)[2]){
  p<-(Elevation$Watershed==colnames(ElevDiff)[i])  # This returns a T/F vector for that column
  ElevDiffNA[,i][!p]<-NA
}

Uphill<-apply(ElevDiffNA, 2, function(x) { sum (!is.na(x)) })

## To test whether there is a threshold value for uphillness, you could plot the cost path value against
## the elevation change

plot(ElevDiff[,410], Water[,410])  # this is not really all that succcessful.
## this should probably be elevdiffna, not elevdiff. 

plot(ElevDiffNA[,210], Water[,270])

#model<-glm(unlist(Water)~unlist(ElevDiff) + unlist(Distdf))

## it looks like the higher values indicate more water flow between two sites. So...
## Total water flow TO a site would simply be the total of every column.

WaterTotnoDeb<-colSums(Water)

## once we get debris loads we can use this one.
WaterSinkProp<-Water
# WaterSinkProp[WaterSinkProp>Toplimit]<-0   ## is it legit to change these to zero??
# or should I change them to NA and get rid of them?? or ???
#v <- v[!is.na(v)]

WaterSinkProp<-sweep(WaterSinkProp,2,colSums(WaterSinkProp),`/`)
WaterSinkTotf<-WaterSinkProp*TotalDebris[,1]
WaterSinkTot<-colSums(WaterSinkProp*TotalDebris[,1])


watermods<-data.frame(Uphill, WaterTotnoDeb,WaterSinkTot, WaterGridMatch$WGM)
names(watermods)<-c("Uphill", "WaterTotnoDeb", "WaterTotDeb", "WGM")

## We need to find a thresh hold value that determines what values are uphill. 
# Use same grid as Water, but calculate elevation from minus elevation to, then plot that against costpath.
# Maybe do this column by column, and use apply (because you're subtracting same elevation in each column)

Waterthresh<-Water
ColEls<-Elevation$Elevation
UnIDs<-name

# This is a little bit more complicated. For the number of uphill sites we need to count the 
## total number of sites in each row that are in the same watershed and at higher elevation (equiv
# to upwind)
# 2) 




all<-data.frame(wind)
all$Uphill<-watermods$Uphill[match(all$WindGridMatch, watermods$WGM)]
all$WaterTotnoDeb<-watermods$WaterTotnoDeb[match(all$WindGridMatch, watermods$WGM)]
all$WaterTotDeb<-watermods$WaterTotDeb[match(all$WindGridMatch, watermods$WGM)]
save(all, file="transitmodels")
write.csv(all, file="alltransitmodels.csv")

#all$WindSourceIndex<-(all$WindSourceTotf-all$WindSourceSinkf)*all$Total_Debris




## Add wind source, wind sink, and pred to Syd_all data set

Syd_all$Upwind<-all$upwind[match(Syd_all$GridMatch, all$WindGridMatch)]
Syd_all$WindSink<-all$WindtotNoDeb[match(Syd_all$GridMatch, all$WindGridMatch)]
Syd_all$WindSinkdis<-all$WinddisttotNodeb[match(Syd_all$GridMatch, all$WindGridMatch)]
Syd_all$WindSinkdispos<-all$Winddistposonly[match(Syd_all$GridMatch, all$WindGridMatch)]
Syd_all$Upwinddeb<-all$UpwindDeb[match(Syd_all$GridMatch, all$WindGridMatch)]
Syd_all$WindSinkdeb<-all$WindSinkTotf[match(Syd_all$GridMatch, all$WindGridMatch)]
Syd_all$Winddistdeb<-all$Winddistdeb[match(Syd_all$GridMatch, all$WindGridMatch)]
Syd_all$Uphill<-all$Uphill[match(Syd_all$GridMatch, all$WindGridMatch)]
Syd_all$WaterSink<-all$WaterTotnoDeb[match(Syd_all$GridMatch, all$WindGridMatch)]
Syd_all$WaterSinkdeb<-all$WaterTotDeb[match(Syd_all$GridMatch, all$WindGridMatch)]
Syd_all$Gamresids<-Covars2$Gamresid[match(Syd_all$Global_ID, Covars2$Global_ID)]


# Also add grid predictionns to Syd_all dataset
Syd_all$gridpred<-TotalDebris$x[match(Syd_all$GridMatch, TotalDebris$Unique_ID)]
plot(Syd_all$gridpred [Syd_all$Source=="KAB"], Syd_all$pred[Syd_all$Source=="KAB"])

### Maybe look at CSIRO predictions using GAM model to see if they are any better
Gampred<-predict(Gam.CS.M2, newdata=Syd_all[Syd_all$Source=="CSIRO",],type="response", se.fit=TRUE,na.action=na.pass)
Gampredunlog<-(exp(Gampred$fit)-1)
Syd_all$Gampred[Syd_all$Source=="CSIRO"]<-Gampredunlog

### This is just CSIRO data set, so we can do predictions on all CSIRO ones separately (e.g. Emu, transect, etc)
Syd_CSall$Upwind<-all$upwind[match(Syd_CSall$GridMatch, all$WindGridMatch)]
Syd_CSall$WindSink<-all$WindtotNoDeb[match(Syd_CSall$GridMatch, all$WindGridMatch)]
Syd_CSall$WindSinkdis<-all$WinddisttotNodeb[match(Syd_CSall$GridMatch, all$WindGridMatch)]
Syd_CSall$WindSinkdispos<-all$Winddistposonly[match(Syd_CSall$GridMatch, all$WindGridMatch)]
Syd_CSall$WaterSink<-all$WaterTotnoDeb[match(Syd_CSall$GridMatch, all$WindGridMatch)]
Syd_CSall$Uphill<-all$Uphill[match(Syd_CSall$GridMatch, all$WindGridMatch)]



#Syd_Covars$WindSource<-all$WindSourceTotf[match(Covars2$Global_ID, all$Global_ID)]

#Covars2$WindSourceIndex<-all$WindSourceIndex[match(Covars2$Global_ID, all$Global_ID)]
#Covars2$resids<-Covars2$TotalDebris-Covars2$Pred


### Gam analysis on KAB data #####

## Think this should probably be on log-transformed data, but let's try it first
# without transform

KAB.G1<-gam(Total_Debris~ te(Latitude,Longitude) + s(rail) + s(Roads) + 
              Prim.land + s(pop50), data=KAB_sub)

vis.gam(KAB.G2,view = c("Longitude","Latitude"), plot.type = "contour", too.far = .05)
map("state", add = T, bg = "white")

KAB.G2<-gam(Total_Debris~ te(Latitude,Longitude) + rail + Roads + 
              Prim.land + pop50, data=KAB_sub)

## gam.check indicates not particularly well fitted. Let's try log-transforming data ##




KAB$Log_Debris<-log(KAB_sub$Total_Debris +1)

### START HERE AND CLEAN UP EVERYTHING BELOW #### 

## Start with a smooth of all terms, then move them out of smooth.

Gam.K.M1<-gam (Log_Debris ~ te (Lat, Long)  + s(RailDistKM)  + s(All_roads_50) + Prim.land 
               + s(Pop_50km) + State + s(Eco_resour_50km) + s(Eco_advan_50km)  + s(Sitecode, bs="re") +
                 s(Edu_occupa_50km) + s(Eco_disadv_50km) + SiteType + s(Pop5to50km_resids) + s(roads_5to50km_resids),
               data=Covars2[Covars2$Source=="KAB",])

save(Gam.K.M1, file="Gam.K.M1")

## all terms out of smooth, except Sitecode and lat/long

Gam.K.M2<-gam (Log_Debris ~ te (Lat, Long) + s(Sitecode, bs="re") + RailDistKM  + All_roads_50 + Prim.land 
               + Pop_25km + State + Eco_resour_50km + Eco_advan_50km + SiteType +
                 Edu_occupa_50km + Eco_disadv_50km + Pop5to50km_resids + roads_5to50km_resids,
               data=Covars2[Covars2$Source=="KAB",])

save(Gam.K.M2, file="Gam.K.M2")

# take out site as random factor, just to see what happens. (as it might be skewing results)

Gam.K.M3<-gam (Log_Debris ~ te (Lat, Long)  + RailDistKM  + All_roads_50 + Prim.land 
               + Pop_25km + State + Eco_resour_50km + Eco_advan_50km + SiteType +
                 Edu_occupa_50km + Eco_disadv_50km + Pop5to50km_resids + roads_5to50km_resids,
               data=Covars2[Covars2$Source=="KAB",])

save(Gam.K.M3, file="Gam.K.M3")

# Back to M2, because it seems to work well, but use Pop_50 instead of Pop_25

Gam.K.M4<-gam (Log_Debris ~ te (Lat, Long) + s(Sitecode, bs="re") + RailDistKM  + All_roads_50 + Prim.land 
               + Pop_50km + State + Eco_resour_50km + Eco_advan_50km + SiteType +
                 Edu_occupa_50km + Eco_disadv_50km + Pop5to50km_resids + roads_5to50km_resids,
               data=Covars2[Covars2$Source=="KAB",])




## higher AIC, so might as well keep to M2. 


vis.gam(Gam.K.M2,view = c("Long","Lat"), plot.type = "contour", too.far = .05)
map(database="world", region= "Australia", add = T, bg = "white")


save(Gam.K.M4, file="Gam.K.M4")

median(Covars2$Eco_resour_50km[Covars2$Source=="KAB"])

Covars2$Town<-as.factor(Covars2$Town)

## Denise would also like a gam (and particularly tensor smooth) for hte other data types.
## Buuuutttt...we need to wait for updated SEIF data for CSIRO. 


Gam.CS.M1<-gam (Log_Debris ~ te (Lat, Long)  + s(RailDistKM)  + s(All_roads_50) + Prim.land 
                + s(Pop_50km) + State + s(Eco_resour_50km) + s(Eco_advan_50km)  + s(Town, bs="re") +
                  s(Edu_occupa_50km) + s(Eco_disadv_50km)  + s(Pop5to50km_resids) + s(roads_5to50km_resids),
                data=Covars2[Covars2$Source=="CSIRO",])


Gam.CS.M2<-gam (Log_Debris ~ te (Lat, Long)  + s(RailDistKM) 
                + s(Pop_50km)   + s(Town, bs="re"), data=Covars2[Covars2$Source=="CSIRO",])

## maybe try removing the site random factor in the first instance

Gam.CS.M3<-gam (Log_Debris ~ te (Lat, Long)  + s(RailDistKM)  + s(All_roads_50) + Prim.land 
                + s(Pop_50km) + State + s(Eco_resour_50km) + s(Eco_advan_50km) +
                  s(Edu_occupa_50km) + s(Eco_disadv_50km)  + s(Pop5to50km_resids) + s(roads_5to50km_resids),
                data=Covars2[Covars2$Source=="CSIRO",])

## Actually should have no spatial values in the beginning. 
## Did not converge after 400 iterations. Can't figure out the syntax to increase
## to 600. Try with site code out as well.

Gam.CS.M4<-gam (Log_Debris ~s(RailDistKM)  + s(All_roads_50) + Prim.land 
                + s(Pop_50km) + State + s(Eco_resour_50km) + s(Eco_advan_50km) +
                  s(Edu_occupa_50km) + s(Eco_disadv_50km)  + s(Pop5to50km_resids) + s(roads_5to50km_resids),
                data=Covars2[Covars2$Source=="CSIRO",], control=list(maxit=600))

## that works a bit better. and decent r2 value (.474)

Gam.CS.M5<-gam (Log_Debris ~s(RailDistKM)  
                + s(Pop_50km) + State  + s(Eco_advan_50km) +
                  s(Edu_occupa_50km) + s(Eco_disadv_50km) + s(roads_5to50km_resids),
                data=Covars2[Covars2$Source=="CSIRO",], control=list(maxit=600))

## gradualy removing nonsig smooths
Gam.CS.M6<-gam (Log_Debris ~s(RailDistKM)  
                + s(Pop_50km) + State  + s(Eco_advan_50km) +
                  s(Edu_occupa_50km) + s(Eco_disadv_50km),
                data=Covars2[Covars2$Source=="CSIRO",], control=list(maxit=600))

# now move into parametric
Gam.CS.M7<-gam (Log_Debris ~RailDistKM  
                + Pop_50km + State  + Eco_advan_50km +
                  Edu_occupa_50km + Eco_disadv_50km,
                data=Covars2[Covars2$Source=="CSIRO",], control=list(maxit=600))

## move non-sig smooths back into smooths

Gam.CS.M7<-gam (Log_Debris ~RailDistKM  
                + Pop_50km + State  + s(Eco_advan_50km) +
                  s(Edu_occupa_50km) + s(Eco_disadv_50km),
                data=Covars2[Covars2$Source=="CSIRO",], control=list(maxit=600))

write.csv(summary.gam(Gam.CS.M7)$p.table, file="Gam.CS.M7p.csv")
write.csv(summary.gam(Gam.CS.M7)$s.table, file="Gam.CS.M7s.csv")

## try with pop 25 - no better. Keep with M7.
Gam.CS.M8<-gam (Log_Debris ~RailDistKM  
                + Pop_25km + State  + s(Eco_advan_50km) +
                  s(Edu_occupa_50km) + s(Eco_disadv_50km),
                data=Covars2[Covars2$Source=="CSIRO",])


### CSIRO all types ####  ## not including Lat/Long

Gam.CSall.M1<-gam (Log_Debris ~ s(RailDistKM)  + s(All_roads_50) + Prim.land 
                   + s(Pop_50km) + State + s(Eco_resour_50km) + s(Eco_advan_50km)  + s(Town, bs="re") +
                     s(Edu_occupa_50km) + s(Eco_disadv_50km)  + s(Pop5to50km_resids) + s(roads_5to50km_resids),
                   data=Covars2[Covars2$Source=="CSIRO" | Covars2$Source=="Emu" | Covars2$Source == "Transect",])


## drop off non-sig smooths

Gam.CSall.M2<-gam (Log_Debris ~ s(RailDistKM)  + s(All_roads_50)
                   + s(Pop_50km) + State +  s(Eco_advan_50km)  + s(Town, bs="re") +
                     s(Eco_disadv_50km),
                   data=Covars2[Covars2$Source=="CSIRO" | Covars2$Source=="Emu" | Covars2$Source == "Transect",])

## move into parametric

Gam.CSall.M3<-gam (Log_Debris ~ RailDistKM + All_roads_50
                   + Pop_50km + State +  Eco_advan_50km  + s(Town, bs="re") +
                     Eco_disadv_50km,
                   data=Covars2[Covars2$Source=="CSIRO" | Covars2$Source=="Emu" | Covars2$Source == "Transect",])


## not so good. Move back into smooth
Gam.CSall.M4<-gam (Log_Debris ~ RailDistKM + s(All_roads_50)
                   + Pop_50km + State +  s(Eco_advan_50km)  + s(Town, bs="re") +
                     s(Eco_disadv_50km),
                   data=Covars2[Covars2$Source=="CSIRO" | Covars2$Source=="Emu" | Covars2$Source == "Transect",])

## need to drop a few out still. But good r2!  .568

Gam.CSall.M5<-gam (Log_Debris ~ RailDistKM +
                     + Pop_50km + State + s(Town, bs="re") +
                     s(Eco_disadv_50km),
                   data=Covars2[Covars2$Source=="CSIRO" | Covars2$Source=="Emu" | Covars2$Source == "Transect",])

## Let's try from the first model, but without town, like we did with just CSIRO data

Gam.CSall.M6<-gam (Log_Debris ~ s(RailDistKM)  + s(All_roads_50) + Prim.land 
                   + s(Pop_50km) + State + s(Eco_resour_50km) + s(Eco_advan_50km)  +
                     s(Edu_occupa_50km) + s(Eco_disadv_50km)  + s(Pop5to50km_resids) + s(roads_5to50km_resids),
                   data=Covars2[Covars2$Source=="CSIRO" | Covars2$Source=="Emu" | Covars2$Source == "Transect",])


Gam.CSall.M7<-gam (Log_Debris ~ s(RailDistKM)  + s(All_roads_50) + Prim.land 
                   + s(Pop_50km) + State  + s(Eco_advan_50km)  +
                     s(Edu_occupa_50km) + s(Eco_disadv_50km)  + s(roads_5to50km_resids),
                   data=Covars2[Covars2$Source=="CSIRO" | Covars2$Source=="Emu" | Covars2$Source == "Transect",])

## out of smooths

Gam.CSall.M8<-gam (Log_Debris ~ RailDistKM  + All_roads_50 + Prim.land 
                   + Pop_50km + State  + Eco_advan_50km  +
                     s(Edu_occupa_50km) + Eco_disadv_50km  + roads_5to50km_resids,
                   data=Covars2[Covars2$Source=="CSIRO" | Covars2$Source=="Emu" | Covars2$Source == "Transect",])


# back into smooths

Gam.CSall.M9<-gam (Log_Debris ~ RailDistKM  + s(All_roads_50) + Prim.land 
                   + Pop_50km + State  + s(Eco_advan_50km)  +
                     s(Edu_occupa_50km) + s(Eco_disadv_50km)  + roads_5to50km_resids,
                   data=Covars2[Covars2$Source=="CSIRO" | Covars2$Source=="Emu" | Covars2$Source == "Transect",])

## that'll do.  

write.csv(summary.gam(Gam.CSall.M9)$p.table, file="Gam.CSall.M9p.csv")
write.csv(summary.gam(Gam.CSall.M9)$s.table, file="Gam.CSall.M9s.csv")

## are there diffs between survey sources?
summary(glm(Log_Debris~Source, data=Covars2[Covars2$Source=="CSIRO" | Covars2$Source=="Emu" | Covars2$Source == "Transect",]))

Gam.CSall.M10<-gam (Log_Debris ~ RailDistKM  + s(All_roads_50) + Prim.land 
                    + Pop_50km + State  + s(Eco_advan_50km)  + Source +
                      s(Edu_occupa_50km) + s(Eco_disadv_50km)  + roads_5to50km_resids,
                    data=Covars2[Covars2$Source=="CSIRO" | Covars2$Source=="Emu" | Covars2$Source == "Transect",])



summary(Covars2[Covars2$Source=="CSIRO" | Covars2$Source=="Emu" | Covars2$Source == "Transect",])

# Use these resids to check against transport models, as well as the ctree ones

Covars2$Gamresid[Syd_all$Source=="CSIRO"]<-Gam.CS.M7$residuals
Covars2$Gamresid[Covars2$Source=="KAB"]<-Gam.K.M2$residuals

## graphing lat/long smooth

quartz()
pdf(file="KAB spatial smooth.pdf")
vis.gam(Gam.K.M2,view = c("Long","Lat"), plot.type = "contour", too.far = .05, xlab="Longitude", ylab="Latitude")
map(database="world", region= "Australia", add = T, bg = "white")
dev.off()

write.csv(summary.gam(Gam.K.M2)$p.table, file="Gam.K.M2p.csv", sep=",")
write.csv(summary.gam(Gam.K.M2)$s.table, file="Gam.K.M2s.csv", sep=",")

write.csv(summary.gam(Gam.K.M8)$p.table, file="Gam.K.M8p.csv", sep=",")
write.csv(summary.gam(Gam.K.M8)$s.table, file="Gam.K.M8s.csv", sep=",")

## summary(glht(Gam.K.M13, linfct = mcp(SiteType="Tukey")))

## tHere does not appear to be any way to do a post-hoc comparison on GAM models. So I will graph mean and se for predicted values for each level
#### prediction of differences between site type #####

predict<-predict(Gam.K.M2, newdata=Covars2, type="response", se.fit=TRUE, na.action=na.pass)
predicttable<- data.frame(unlist(predict$fit),unlist(predict$se.fit))
names(predicttable)<- c("Predict","PredictionSE")
Covars3<-data.frame(Covars2, predicttable)

Covars3<-Covars3[Covars3$Source=="KAB",]

pdf(file="LogDebris by Site Type2")
par(mar = c(9,4,4,2) + 0.1)
barplot(tapply(Covars3$Predict, Covars3$SiteType, mean), ylab="Log Debris", ylim=c(0,5.1), las=2)
x0<- seq(.7,.7+7*1.2,1.2)
y0 <- tapply(Covars3$Predict,Covars3$SiteType,mean)
y1 <- tapply(Covars3$Predict,Covars3$SiteType,mean) + 1.96*tapply(Covars3$PredictionSE,Covars3$SiteType,mean)  
## not for graphing but to determine overlap of SE
y2 <- tapply(Covars3$Predict,Covars3$SiteType,mean) - 1.96*tapply(Covars3$PredictionSE,Covars3$SiteType,mean)  
text(x = x0, y= (y1+0.4),labels = c("a","b","b","bc","ac","a","b","b"))
arrows(x0,y0,x1=x0, y1, angle = 90)
dev.off()

drop_upload("~/Documents/R data/NESP/R scripts/Transect and grid modeling.R", dest="/NESP/R scripts")


