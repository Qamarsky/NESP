######## FUNCTIONS ##########
panel.corC <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  #     text(0.5, 0.5, txt, cex = cex.cor * r)
  
  text(0.5, 0.5, txt, cex = cex.cor)
}


panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}


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

## for now...

Covars3<-Covars2[complete.cases(Covars2[,c("Total_Debris","State","Volunteers","RailDistKM","RoadDistKM","Area_m2",
                                           "Pop_1km","Pop_5km","Pop_10km","Pop_25km","Pop_50km","Eco_advan_1km","Eco_advan_5km",
                                           "Eco_advan_25km","Eco_advan_50km","eco_disadv1km","eco_disadv5km","eco_disadv10km",
                                           "eco_disadv25km","eco_disadv50km","eco_resour1km","eco_resour5km","eco_resour10km",
                                           "eco_resour25km","eco_resour50km","Edu_occupa1km","Edu_occupa5km","Edu_occupa10km","Edu_occupa25km","Edu_occupa50km","All_roads_50","All_roads_5",
                                           "roads_5to50km_resids","Pop5to50km_resids")]),]




KAB.M1New<- cforest(Total_Debris ~ State + Volunteers + RailDistKM + RoadDistKM + Area_m2 +
                      Pop_1km + Pop_5km + Pop_10km + Pop_25km + Pop_50km + Eco_advan_1km +
                      Eco_advan_5km + Eco_advan_25km + Eco_advan_50km + eco_disadv1km +
                      eco_disadv5km + eco_disadv10km + eco_disadv25km + eco_disadv50km +
                      eco_resour1km + eco_resour5km + eco_resour10km + eco_resour25km +
                      eco_resour50km + Edu_occupa1km + Edu_occupa5km + Edu_occupa10km +
                      Edu_occupa25km + Edu_occupa50km + All_roads_50 + All_roads_5 + 
                      roads_5to50km_resids + Pop5to50km_resids,
                    data =Covars3[Covars3$Source=="KAB",], 
                    controls=cforest_unbiased(ntree=3000, mtry=10))

save(KAB.M1New, file="KAB.M1New.Rdata")


### this is KAB model with new SEIF covars as well as not changing default mtry

KAB.M2New<- cforest(Total_Debris ~ State + Volunteers + RailDistKM + RoadDistKM + Area_m2 +
                      Pop_1km + Pop_5km + Pop_10km + Pop_25km + Pop_50km + Eco_advan_1km +
                      Eco_advan_5km + Eco_advan_25km + Eco_advan_50km + eco_disadv1km +
                      eco_disadv5km + eco_disadv10km + eco_disadv25km + eco_disadv50km +
                      eco_resour1km + eco_resour5km + eco_resour10km + eco_resour25km +
                      eco_resour50km + Edu_occupa1km + Edu_occupa5km + Edu_occupa10km +
                      Edu_occupa25km + Edu_occupa50km + All_roads_50 + All_roads_5 + 
                      roads_5to50km_resids + Pop5to50km_resids,
                    data =Covars3[Covars3$Source=="KAB",], 
                    controls=cforest_unbiased(ntree=2001))


## now try with adding all of the various road buffers in.
Covars3

KAB.M3New<- cforest(Total_Debris ~ State + Volunteers + RailDistKM + RoadDistKM + Area_m2 +
                      Pop_1km + Pop_5km + Pop_10km + Pop_25km + Pop_50km + Eco_advan_1km +
                      Eco_advan_5km + Eco_advan_25km + Eco_advan_50km + eco_disadv1km +
                      eco_disadv5km + eco_disadv10km + eco_disadv25km + eco_disadv50km +
                      eco_resour1km + eco_resour5km + eco_resour10km + eco_resour25km +
                      eco_resour50km + Edu_occupa1km + Edu_occupa5km + Edu_occupa10km +
                      Edu_occupa25km + Edu_occupa50km + All_roads_50 + All_roads_5 + All_roads_10 +
                      All_roads_25 + All_roads_1 +  roads_5to50km_resids + Pop5to50km_resids,
                    data =Covars3[Covars3$Source=="KAB",], 
                    controls=cforest_unbiased(ntree=2001))


save(KAB.M3New, file="KAB.M3New.Rdata")

load("KAB.M2New.Rdata") 

VarKAB3<-varimp (KAB.M3New) ## this gives the importance of each of the variables

#VarKAB<-varimp (KAB.M1New, conditional = TRUE)
save(VarKAB3, file="VarKAB2.Rdata")

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


###### CSIRO CFOREST MODEL #####

CSIRO.M1New<- cforest(Total_Debris ~ State + Volunteers + RailDistKM + RoadDistKM + Area_m2 +
                      Pop_1km + Pop_5km + Pop_10km + Pop_25km + Pop_50km + Eco_advan_1km +
                      Eco_advan_5km + Eco_advan_25km + Eco_advan_50km + eco_disadv1km +
                      eco_disadv5km + eco_disadv10km + eco_disadv25km + eco_disadv50km +
                      eco_resour1km + eco_resour5km + eco_resour10km + eco_resour25km +
                      eco_resour50km + Edu_occupa1km + Edu_occupa5km + Edu_occupa10km +
                      Edu_occupa25km + Edu_occupa50km + All_roads_50 + All_roads_5 + 
                      roads_5to50km_resids + Pop5to50km_resids,
                    data =Covars3[Covars3$Source=="CSIRO",], 
                    controls=cforest_unbiased(ntree=2001))



###### PREDICTIONS ON GRID #####

### ctree model unfortunately used a bit of local site level data, so... #####

Grid$Volunteers<-median(Covars$Volunteers[Covars$Source=="KAB"], na.rm=TRUE)
Grid$Area_m2<-1000
Grid$Volunteers<-as.integer(Grid$Volunteers)
Grid$State<-as.factor(Grid$State)
levels(Grid$State)<-levels(Covars$State)
Grid$State[Grid$State=='ACT']<-'NSW'

pred<-predict(KAB.M3New, newdata=Grid, type="response")

save(pred, file="pred4.RData")
write.csv(Grid, file="Grid.csv")
pred_old<-load(file="pred3.RData")


Grid$pred<-pred

### for some reason throws a class not matching error ###

#Covtest<-Covars[,c("State","Volunteers","RailDistKM","RoadDistKM","Area_m2",
"Pop_1km","Pop_5km","Pop_10km","Pop_25km","Pop_50km","Eco_advan_1km",
"Eco_advan_5km","Eco_advan_25km","Eco_advan_50km","eco_disadv1km",
"eco_disadv5km","eco_disadv10km","eco_disadv25km","eco_disadv50km",
"eco_resour1km","eco_resour5km","eco_resour10km","eco_resour25km",
"eco_resour50km","Edu_occupa1km","Edu_occupa5km","Edu_occupa10km",
"Edu_occupa25km","Edu_occupa50km","All_roads_50","All_roads_5",
"roads_5to50km_resids","Pop5to50km_resids")]


#Gridtest<-Grid[,c("State","Volunteers","RailDistKM","RoadDistKM","Area_m2",
"Pop_1km","Pop_5km","Pop_10km","Pop_25km","Pop_50km","Eco_advan_1km",
"Eco_advan_5km","Eco_advan_25km","Eco_advan_50km","eco_disadv1km",
"eco_disadv5km","eco_disadv10km","eco_disadv25km","eco_disadv50km",
"eco_resour1km","eco_resour5km","eco_resour10km","eco_resour25km",
"eco_resour50km","Edu_occupa1km","Edu_occupa5km","Edu_occupa10km",
"Edu_occupa25km","Edu_occupa50km","All_roads_50","All_roads_5",
"roads_5to50km_resids","Pop5to50km_resids")]

#which(sapply(Covtest, class) != sapply(Gridtest, class)) 

## This finds out that Volunteers doesn't match, so make sure it's an integer. 


##### GAM MODELLING #####

## let's start by looking at some correlations ##
### from VarKAB2 we know that the following buffers are the best sizes:
### So all of the SEIF variables are best with the 1km buffer.
## we did not try all of the roads buffers...might be interesting to go back and use all the different roads buffers?

eco_disadv1km
Edu_occupa1km
Eco_advan_1km
eco_resour1km
All_roads_50
Pop_10km

## so let's look at correlations
#check correlations
#just have a quick look to see which variables are strongly correlated
# use the buffers above. 
ToConsider1 <- c("RailDistKM", "RoadDistKM", "Pop_10km","eco_disadv1km","Edu_occupa1km",
                               "Eco_advan_1km","eco_resour1km","All_roads_50")
quartz
pairs(Covars3[,ToConsider1])
pairs(Covars3[,ToConsider1], lower.panel = panel.smooth, upper.panel = panel.corC)

## there is strong correlation between socio-economic variables. 
### don't know if the zeros in the data will change the correlation significantly.

quartz()
pairs(Covars3[,ToConsider1], lower.panel=panel.smooth, upper.panel=panel.cor)

## looks like there is nearly perfect correlation between eco_disadv1km and eco_advan1km. Let's drop
## off eco_disadv1km. 

pairs(Covars3[,c("RailDistKM", "RoadDistKM", "Pop_10km","Edu_occupa1km",
              "Eco_advan_1km","eco_resour1km","All_roads_50")], lower.panel=panel.smooth, upper.panel=panel.cor)

## now the only thigns correlated are eco_advan and Edu_occupa and eco_advan and eco_resour

Covars3$Edu.Adv.resid<-lm(Edu_occupa1km~Eco_advan_1km, data=Covars3)$residuals

pairs(Covars3[,c("RailDistKM", "RoadDistKM", "Pop_10km","Edu_occupa1km",
                 "eco_resour1km","All_roads_50","Edu.Adv.resid")], lower.panel=panel.smooth, upper.panel=panel.cor)


## no more correlation. So, let's do a gam model!
######  GAM Modeling #####

KAB.Gam1<-gam(Log_Debris~te(Lat, Long) + State + RailDistKM + RoadDistKM + Pop_10km + Edu_occupa1km +
      eco_resour1km + All_roads_50 + Edu.Adv.resid + roads_5to50km_resids + Pop5to50km_resids, data=Covars3[Covars3$Source=="KAB",])

# try with everything in a smooth, first

KAB.Gam2<-gam(Log_Debris~te(Lat, Long) + State + s(RailDistKM)  + s(RoadDistKM) + s(Pop_10km) + s(Edu_occupa1km) +
                s(eco_resour1km) + s(All_roads_50) + s(Edu.Adv.resid) + s(roads_5to50km_resids) + s(Pop5to50km_resids), data=Covars3[Covars3$Source=="KAB",])

KAB.Gam3<-gam(Log_Debris~te(Lat, Long) + State + RailDistKM  + RoadDistKM + Pop_10km + Edu_occupa1km +
                eco_resour1km + All_roads_50 + s(Edu.Adv.resid) + roads_5to50km_resids + s(Pop5to50km_resids), data=Covars3[Covars3$Source=="KAB",])


# AIC is 50549.49. Let's see if taking out some of the terms helps, or if changing the buffer size
# doesn't help to take out edu.adv.resid, pop resids, all_roads, Edu_occupa1km, roaddistkm, raildistkm
# removing eco_resour1km  decreases AIC to 50463.66, but removing roads instead takes it to 50461.94
# changing roads buffer to 25, 10 doesn't help, but changing to 5km reduces AIC to 50458.19
# changing eco_resour to 5, 25, doesn't help, but changing to 10km reduces AIC to 50442.13 (roads no longer sig)
# changing edu-occupa to any other buffer doesn't help
# removing pop_10 decreases to 50353.48, but changing to 1 km decreases to 50353.26. no other buffers better.


KAB.Gam4<-gam(Log_Debris~te(Lat, Long) + State + RoadDistKM + RailDistKM +  Edu_occupa1km +Pop_1km +
                All_roads_5 + s(Edu.Adv.resid) + eco_resour10km + s(Pop5to50km_resids), data=Covars3[Covars3$Source=="KAB",])


### interestingly, came up with a different version by accident, was trying for CSIRO data but put in KAB data instead
## similar AIC, but different model:

KAB.Gam4_alt<-gam(Log_Debris~te(Lat, Long) + State + RailDistKM  + RoadDistKM + Pop_10km + Edu_occupa1km +
               eco_resour1km + All_roads_50 + s(Edu.Adv.resid) + roads_5to50km_resids + s(Pop5to50km_resids), data=Covars3[Covars3$Source=="KAB",])


#### CSIRO GAM #####
CS.Gam1<-gam(Log_Debris~te(Lat, Long) + State + RailDistKM + RoadDistKM + Pop_10km + Edu_occupa1km +
                eco_resour1km + All_roads_50 + Edu.Adv.resid + roads_5to50km_resids + Pop5to50km_resids, data=Covars3[Covars3$Source=="CSIRO",])
# significantly more deviance explained than for KAB! 

CS.Gam2<-gam(Log_Debris~te(Lat, Long) + State + s(RailDistKM)  + s(RoadDistKM) + s(Pop_10km) + s(Edu_occupa1km) +
                s(eco_resour1km) + s(All_roads_50) + s(Edu.Adv.resid) + s(roads_5to50km_resids) + s(Pop5to50km_resids), data=Covars3[Covars3$Source=="CSIRO",])


## look at plots and see which ones could probably come out of smooth

CS.Gam3<-gam(Log_Debris~te(Lat, Long) + State + s(RailDistKM)  + s(RoadDistKM) + s(Pop_10km) + s(Edu_occupa1km) +
               eco_resour1km + s(All_roads_50) + Edu.Adv.resid + roads_5to50km_resids + s(Pop5to50km_resids), data=Covars3[Covars3$Source=="CSIRO",])


### now look at aic and check if you can drop some out, or change buffer size. starts at 2545.859
# changing eco resour buffer size not better.

# removing state, rail dist,pop, all roads, edu.adv.resid not better
# removing  s(RoadDistKM) +  improves to 2545.606
# remvoing eco_resour1km + improves to 2541.864
# changing pop buffer to 25 km improves to 2531.091
# changin roads to 25km improves to 2516.352

CS.Gam3<-gam(Log_Debris~te(Lat, Long) + State + s(RailDistKM) +  s(Pop_25km) + s(Edu_occupa1km) +
               s(All_roads_25) +Edu.Adv.resid + roads_5to50km_resids +s(Pop5to50km_resids), data=Covars3[Covars3$Source=="CSIRO",])

## we could now take some back out of smooth?

CS.Gam4<-gam(Log_Debris~te(Lat, Long) + State + s(RailDistKM) +  Pop_25km + Edu_occupa1km +
               s(All_roads_25) +Edu.Adv.resid + roads_5to50km_resids +s(Pop5to50km_resids), data=Covars3[Covars3$Source=="CSIRO",])





# Use these resids to check against transport models, as well as the ctree ones

Covars3$Gamresid[Covars3$Source=="CSIRO"]<-CS.Gam4$residuals
Covars3$Gamresid[Covars3$Source=="KAB"]<-KAB.Gam4$residuals


##### PREDICTIONS ON TRANSECT DATA #######

####### making predictions on the data based on cforest models for each of the datasets
Syd_Covars<-Covars3[Covars3$Lat <= (-33.671774)  & Covars3$Lat >= (-34.265774) & Covars3$Long <= (151.372906) & Covars3$Long >= (150.718096),]


Syd_KAB<-Syd_Covars[Syd_Covars$Source=="KAB",]
Syd_CSIRO<-Syd_Covars[Syd_Covars$Source=="CSIRO",]
Syd_CSall<-Syd_Covars[Syd_Covars$Source=="Emu" | Syd_Covars$Source==
                        "Transect" | Syd_Covars$Source=="CSIRO",]


Syd_KAB$Sydtreepred<-predict(KAB.M3New, newdata=Syd_KAB, type="response")
colnames(Syd_KAB)<-c(colnames(Syd_KAB)[-length(names(Syd_KAB))], "treepred")

Syd_KAB$treeresids<-Syd_KAB$Totper1000-Syd_KAB$treepred

#Syd_CSIRO$treepred<-predict(CSIRO.M1New, newdata=Syd_CSIRO, type="response")
#Syd_CSIRO$treeresids<-Syd_CSIRO$Totper1000-Syd_CSIRO$treepred


Syd_KAB$Gampredlog<-predict(KAB.Gam4, newdata=Syd_KAB, type="response")
#names(Syd_KAB)<-c(names(Syd_KAB)[-length(names(Syd_KAB))], "gampredlog")

#Syd_KAB$Gampred<-Gampred
#Syd_CSIRO$Gampred<-predict(CS.Gam4, newdata=Syd_CSIRO, type="response")

## really, there are only 3 CSIRO ones in Sydney, so let's not worry about them
Syd_all<-Syd_KAB
Syd_all$Gampredunlog<-exp(Syd_all$Gampred)-1






###### FROM APC ANALYSIS #####


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


