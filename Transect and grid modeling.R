
### set up dropbox ###

drop_auth()
drop_dir(path="/NESP")

##
setwd("/Users/uqqschuy/Documents/R data/NESP")


##### libraries #####
library(party)
library(maps)
library(ggmap)
library(mgcv)
library(ggplot2)
library(googleVis)
library(multcomp)
library(rdrop2)
library(httpuv)
library(Hmisc)

######## Ctree #########
### Now let's start to get some tree action happening. First try CUA and KAB separately

### New models with KAB data

set.seed(4183)

## need to fix up missing variables in Covars2 data. 

KAB.M1New<- cforest(Total_Debris ~ State + Volunteers + RailDistKM + RoadDistKM + Area_m2 +
                      Pop_1km + Pop_5km + Pop_10km + Pop_25km + Pop_50km + Eco_advan_1km +
                      Eco_advan_5km + Eco_advan_25km + Eco_advan_50km + eco_disadv1km +
                      eco_disadv5km + eco_disadv10km + eco_disadv25km + eco_disadv50km +
                      eco_resour1km + eco_resour5km + eco_resour10km + eco_resour25km +
                      eco_resour50km + Edu_occupa1km + Edu_occupa5km + Edu_occupa10km +
                      Edu_occupa25km + Edu_occupa50km + All_roads_50 + All_roads_5 + 
                      roads_5to50km_resids + Pop5to50km_resids,
                 data =Covars2[Covars2$Source=="KAB",], 
                 controls=cforest_unbiased(ntree=3000, mtry=10))


varimp (KAB.M1New) ## this gives the importance of each of the variables

VarKAB<-varimp (KAB.M1New, conditional = TRUE)

## looks like it could be good to use conditional varimp (conditional= TRUE), bedcause that
# "adjusts for correlations between predictor variables" see Strobl et al. (2008) for details. 

#State           Volunteers           RailDistKM           RoadDistKM 
#4075.5837               0.0000            1387.2227            2858.4099 
#Area_m2              Pop_1km              Pop_5km             Pop_10km 
#0.0000             770.4726             628.3146             973.3662 
#Pop_25km             Pop_50km        Eco_advan_1km        Eco_advan_5km 
#667.6906             621.4801             773.3326             811.1754 
#Eco_advan_25km       Eco_advan_50km        eco_disadv1km        eco_disadv5km 
#394.9986             512.3338            1247.5169            2078.1771 
#eco_disadv10km       eco_disadv25km       eco_disadv50km        eco_resour1km 
#1073.8336             473.2998             459.1648            1199.3072 
#eco_resour5km       eco_resour10km       eco_resour25km       eco_resour50km 
#1089.8802             639.5513             573.5218             708.0982 
#Edu_occupa1km        Edu_occupa5km       Edu_occupa10km       Edu_occupa25km 
#1388.8498             856.3418             614.1993             468.7135 
#Edu_occupa50km         All_roads_50          All_roads_5 roads_5to50km_resids 
#656.3565             846.9883             678.3464             683.0208 
#Pop5to50km_resids 
#1516.4880 



### cforest trees don't have weights stored in the ensemble object
### this should get the weights  http://stackoverflow.com/questions/19924402/cforest-prints-empty-tree

y <- cforest(Species ~ ., data=iris, control=cforest_control(mtry=2))
tr <- party:::prettytree(y@ensemble[[1]], names(y@data@get("input")))
plot(new("BinaryTree", tree=tr, data=y@data, responses=y@responses))


tr<-party:::prettytree(KAB.M1New@ensemble[[1]], names(KAB.M1New@data@get("input")))
plot(new("BinaryTree", tree=tr, data=KAB.M1New@data, responses=KAB.M1New@responses))


update_tree <- function(x) {
  if(!x$terminal) {
    x$left <- update_tree(x$left)
    x$right <- update_tree(x$right)
  } else {
    x$weights <- x[[9]]
    x$weights_ <- x[[9]]
  }
  x
}


tr_weights <- update_tree(tr)
plot(new("BinaryTree", tree=tr_weights, data=KAB.M1New@data, responses=KAB.M1New@responses))


### This should be a way to graph the output of the forest model
## https://stats.stackexchange.com/questions/205664/is-there-a-method-to-plot-the-output-of-a-random-forest-in-r






pt <- prettytree(KAB.M1New@ensemble[[1]], names(KAB.M1New@data@get("input"))) 
nt <- new("BinaryTree") 
nt@tree <- pt 
nt@data <- KAB.M1New@data 
nt@responses <- KAB.M1New@responses 

plot(nt, type="simple")



### We have already done the modelling for full data set, so we should just be able to use this model...

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

G.K.M2<-ctree(Total_Debris ~ RailDistKM + State + All_roads_50 + Prim.land + Pop_50km + 
                eco_resour50km + Eco_advan_50km + Edu_occupa50km + eco_disadv50km + 
                roads_5to50km_resids + Pop5to50km_resids,
              data=Covars2[Covars2$Source=="KAB",])


## Predictions on CSIRO dat

G.C.M1<-ctree(Totper1000 ~ RailDistKM + State + All_roads_50 + Prim.land + Pop_50km + 
                eco_resour50km + Eco_advan_50km + Edu_occupa50km + eco_disadv50km + 
                roads_5to50km_resids + Pop5to50km_resids,
              data=Covars2[Covars2$Source=="CSIRO",])

G.Call.M1<-ctree(Totper1000 ~ RailDistKM + State + All_roads_50 + Prim.land + Pop_50km + 
                   eco_resour50km + Eco_advan_50km + Edu_occupa50km + eco_disadv50km + 
                   roads_5to50km_resids + Pop5to50km_resids,
                 data=Covars2[Covars2$Source=="Emu" | Covars2$Source=="Transect" | Covars2$Source=="CSIRO",])

quartz()
plot(G.K.M2, type="simple", cex=0.5)





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


