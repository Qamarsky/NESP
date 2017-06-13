###### Make predictions with wind and water transport. ####

#### Note that there are some grids in Sydney that have been cut out due to lack of data,
### probably NAs in the debris/km or something of the sort
### so there are more wind and water transit cells than in the Covars2 dataset. 
## this should not be a problem, just to know...

# Given a transit matrix (e.g. wind)

## To get distribution from SOURCE (Cols) - First normalise proportions by row

#system.time(Wind<-read.csv("~/Documents/R data/NOAAOC/APC/Wind transport matrix unique", sep=","))

###### LOAD WIND #####

Winddf<-drop_read_csv("NESP/analysis/Wind transport/Wind transport matrix.GridToSurveys.csv")
#Winddf<-Winddf[rownames(Winddf) %nin% watercells,] ## remove those cells that are over water and have no covars

#Windnames<-colnames(Winddf)
colnames(Winddf)<-substring(colnames(Winddf),2)
colnames(Winddf)<-sub("^([^.]*.[^.]*).", "\\1-",colnames(Winddf))  ### for some reason Chris' file has changed the "-" to a ".". this changes it back

#namematch<-match(Windnames, Covars2$UID_1)

###### LOAD DIST #####

Distdf<-drop_read_csv("NESP/analysis/Wind transport/Distance matrix.GridToSurveys.csv")
#Distdf<-Distdf[rownames(Distdf) %nin% watercells,] ## remove cells that are over water and have no covars
colnames(Distdf)<-substring(colnames(Distdf),2)
colnames(Distdf)<-sub("^([^.]*.[^.]*).", "\\1-",colnames(Distdf))  ### for some reason Chris' file has changed the "-" to a ".". this changes it back


##### LOAD WATER #####


Water<-drop_read_csv("NESP/Analysis/Water transport/sydney_fishnet_globaldata_costdist_170411.csv")
rownames(Water)<-Water[,1]

Water<-Water[,-1]
colnames(Water)<-substring(colnames(Water),2)
colnames(Water)<-sub("^([^.]*.[^.]*).", "\\1-",colnames(Water))  ### for some reason Chris' file has changed the "-" to a ".". this changes it back


#Water<-Water[rownames(Water) %nin% watercells,] ## remove those cells that are over water and have no covars
#Water<-Water[rownames(Water) %in% rownames(Winddf),]

#save(Water, file="Water")

### there is a chunk of the Water matrix that didn't get values because it's off of the DEM. So let's remove
# these grid cells from Grid, Wind, and Dist matrices. 

Grid<-Grid[Grid$UID %in% rownames(Water),]
Winddf<-Winddf[rownames(Winddf) %in% rownames(Water),]
Distdf<-Distdf[rownames(Distdf) %in% rownames(Water),]

## for some reason there are Water rows that are not in the Grid/Wind files, so let's get rid of them

Water<-Water[rownames(Water) %in% Grid$UID,]

WindGridMatch<-colnames(Winddf) ## might need to play with this somewhat...
#WindGridMatch<-substring(WindGridMatch,2)
#WindGridMatch<-as.numeric(WindGridMatch)

# for some reason there is an extra column in the Water matrix.

Water<-Water[,names(Water) %in% names(Distdf)]

## put water columns in the same order as the other two

Water<-Water[,match(colnames(Distdf), colnames(Water))]

TotalDebris<-Grid[,c("pred","UID")]
#rownames(TotalDebris)<-Grid2$UID

#TotalDebris<-as.vector(TotalDebris)

## because we don't actually have total debris predicted per each cell, the best hyptheses we can make are:
# 1) Amont of debris is somehow related to total number of upwind cells (upwind)
# 2) Amount of debris is somehow related to how much total wind comes from those cells
## a) This you would do by adding the total positive values for every column
## b) And you could add the total amount of debris coming from upwind cells
## c) Amount of debris is the proportion of wind that comes from a particular cell * debris in that cell
# 3) And finally, you could relate to distance. The greater the distance, the less would be transported
## so for this, take the sign of each wind direction, and multiply it by the distance. Then do an inverse
## for each cell, so greater distances have less value. Then just add the postive values. This gives you basically the number
## of upwind cells, weighted by distance.
# 4) and of course finally you have straight predictions. 

## 1) how many cells in each column are positive??
upwind<-apply(Winddf, 2, function(x) { sum (x>0) })

### 2 Total wind coming from upwind cells: 
# 2a) Just use straight colsums (for pos values)
WindSinkPos<-Winddf
WindSinkPos[WindSinkPos<0]<-0  ## basically changes all negative values to zeros - only counting the proportion of the debris that comes FROM sites
WindtotNoDeb<-colSums(WindSinkPos)


# 2b) upwind cells times the amount of debris coming from them (not amount of wind, just whether upwind or not)
Windsign<-Winddf
Windsign[Winddf>0]<-1
Windsign[Winddf<0]<-(-1)
Windsignpos<-Windsign
Windsignpos[Windsignpos<0]<-0
UpwindDeb<-colSums(Windsignpos*TotalDebris[match(rownames(Winddf), TotalDebris$UID),1])


# 2c) ## We can do the proportion of debris coming from upwind cells times debris in each of those upwind cells.
WindSinkPropPos<-sweep(WindSinkPos,2,colSums(WindSinkPos),`/`) ### this gives you the proportion of wind transport from each grid cell (row) to each transect cell (column)

## This WindSinkPropPos gives me the proportion of the debris that is in my cell that came FROM every other cell.
## so you can create a prediction grid (or portion thereof) that presumes that all cells are sinks, and all of the
## debris in them came from somewhere else. So if we multiply every colsum proportion by the total amount of
## debris that was predicted for each of our sites (since it's a subset), then we can ....

# multiply proportion of wind in each cell by the amount of debris in that cell      WindSinkTot<-sweep(WindSinkPropPos,1,TotalDebris,'*')
###MUST MATCH THIS WITH PROPER GRIDMATCH VALUES because total debris may be in different order from wind. 

windmatch<-match(rownames(WindSinkPropPos), TotalDebris$UID)
WindSinkTot<-WindSinkPropPos*TotalDebris[windmatch,1]  
WindSinkTotf<-colSums(WindSinkTot) 

## 3  Now we do upwind cells divided by distance
Winddist<-Windsign/Distdf[match(rownames(Winddf), rownames(Distdf)),]  ## both pos and neg (-1 or +1)
#is.na(Winddist) <- do.call(cbind,lapply(Winddist, is.infinite)) ## change Inf values to NA


##a) total amount of wind, both positive and negative, inversely correlated by dist
## using only +1 or -1 for wind, not magnitude of wind
WinddisttotNodeb<-colSums(Winddist, na.rm=TRUE) ## this is of course both pos and neg.

##b) Total amount of positive wind cells inversely related to distance

Winddistpos<-Winddist 
Winddistpos[Winddist<0]<-0 ## number of upwind 
Winddistposonly<-colSums(Winddistpos, na.rm=TRUE)

##  c) Now let's do total wind divided by the distance, and multiply by debris
Winddistprop<-Winddf/Distdf
Winddistprop[Winddistprop<0]<-0 ## just looking at upwind cells
Winddistdeb<-colSums(Winddistprop*TotalDebris[match(rownames(Winddf), TotalDebris$UID),1], na.rm=T)

wind<-data.frame(upwind, WindtotNoDeb,WinddisttotNodeb, Winddistposonly, WindSinkTotf, UpwindDeb, Winddistdeb, WindGridMatch)

save(wind, file="wind")
write.csv(wind, file="wind.csv")

#WindSourceProp[data cols, not totals]<-Wind/rowSums(Wind[data cols])
#WindSourceTot<-WindSourceProp[data columns]*Total_Debris
#WindSourceTotf<-ColSums(WindSourceTot)

####### Water matrix ######

## 1) Find the number of cells that are uphill from the transects 

Elevation<-read.csv("~/Documents/R data/NOAAOC/APC/grid_covars_100816_wtshd.csv")

WaterGridMatch<-colnames(Water) ## might need to play with this somewhat...
#WaterGridMatch<-substring(WaterGridMatch,2)
#WaterGridMatch<-sub("^([^.]*.[^.]*).", "\\1-",WaterGridMatch)  ### for some reason Chris' file has changed the "-" to a ".". this changes it back
WaterGridMatch<-data.frame(WaterGridMatch, WaterGridMatch)
names(WaterGridMatch)<-c("WGM", "Elev")

WaterGridMatch$Elev<-Grid$elevation[match(WaterGridMatch$WGM, Grid$UID)]
WaterGridMatch$Watershed<-Grid$Watershed[match(WaterGridMatch$WGM,Grid$UID)]

## This will get the difference in elevation between each "from" to each "to".

#establish a data frame with each of the "from" elevations repeated for every column, labeled with watershed of the "to" cells
ElevRow<-as.data.frame(matrix(rep(Grid$elevation,each=306), ncol=306, byrow=TRUE))
colnames(ElevRow)<-WaterGridMatch$Watershed

# subtract the "to" elevation from the "from" elevation to get the difference in elevation
ElevDiff<-sweep(ElevRow,2,WaterGridMatch$Elev, FUN="-") ## difference in elevation between "from" (rownames) and 
rownames(ElevDiff)<-rownames(Water)
ElevDiffNA<-ElevDiff
colnames(ElevDiffNA)<-WaterGridMatch$Watershed


##Now we have to remove ones that are not the same watershed

for (i in 1: dim(ElevDiffNA)[2]){
  p<-(Grid$Watershed==colnames(ElevDiffNA)[i])  # This returns a T/F vector for that column
  ElevDiffNA[,i][!p]<-NA
}

# now count the number of cells that are uphill
Uphill<-ElevDiffNA
Uphill[Uphill>0]<-1
Uphill[Uphill<0]<-NA
Uphillsum<-colSums(Uphill, na.rm=T)


## To test whether there is a threshold value for uphillness, you could plot the cost path value against
## the elevation change

plot(ElevDiff[,410], Water[,410])  # this is not really all that succcessful.
## this should probably be elevdiffna, not elevdiff. 
plot(ElevDiffNA[,210], Water[,270])
#model<-glm(unlist(Water)~unlist(ElevDiff) + unlist(Distdf))

 ### 2b)  Now get the total amount of debris from uphill cells

Uphilldeb<-colSums(Uphill*TotalDebris[match(rownames(Uphill), TotalDebris$UID),1], na.rm=T)


## it looks like the higher values indicate more water flow between two sites. So...
## Total water flow TO a site would simply be the total of every column.

### 2a) This is a bit different from wind, because we're not really just using uphill ones here. Might need to check on that...
WaterTotnoDeb<-colSums(Water)

# 2extra Water times debris (non-proportional)

WaterTotdeb<-colSums(Water*TotalDebris[match(rownames(Water),TotalDebris$UID),1])

## Now apply Debris loads to here
WaterSinkProp<-Water
# WaterSinkProp[WaterSinkProp>Toplimit]<-0   ## is it legit to change these to zero??
# or should I change them to NA and get rid of them?? or ???
#v <- v[!is.na(v)]

WaterSinkProp<-sweep(WaterSinkProp,2,colSums(WaterSinkProp),`/`)
WaterSinkTotf<-WaterSinkProp*TotalDebris[match(rownames(WaterSinkProp), TotalDebris$UID),1]
WaterSinkTot<-colSums(WaterSinkProp*TotalDebris[match(rownames(WaterSinkProp), TotalDebris$UID),1])

#3a) Water inversely proportional to distance, whether positive or negative  - this distance measurement I believe is already incorporated
# Into least cost path analysis, so this is not relevant for water

#3b) Uphill cells inversely prop to distance

Uphilldist<-Uphill/Distdf[match(rownames(Uphill), rownames(Distdf)),1]
Waterdistposonly<-colSums(Uphilldist, na.rm=T)

#3c) Amount of water (full matrix) inversely prop to distance plus debris
watermatch<-match(rownames(Water), rownames(Distdf))
Waterdistdeb<-Water/Distdf[match(rownames(Water), rownames(Distdf)),]*TotalDebris[match(rownames(Water), TotalDebris$UID),1]
Waterdistdebf<-colSums(Waterdistdeb)



watermods<-data.frame(Uphillsum, WaterTotnoDeb,WaterTotdeb, Uphilldeb, WaterSinkTot, Waterdistposonly, Waterdistdebf, WaterGridMatch$WGM)
#names(watermods)<-c("Uphill", "WaterTotnoDeb", "WaterSinkTot", "WGM")

## We need to find a thresh hold value that determines what values are uphill. 
# Use same grid as Water, but calculate elevation from minus elevation to, then plot that against costpath.
# Maybe do this column by column, and use apply (because you're subtracting same elevation in each column)

Waterthresh<-Water
ColEls<-Elevation$Elevation
UnIDs<-name




allmods<-data.frame(wind, watermods)
#all$Uphill<-watermods$Uphill[match(all$WindGridMatch, watermods$WGM)]
#all$WaterTotnoDeb<-watermods$WaterTotnoDeb[match(all$WindGridMatch, watermods$WGM)]
#all$WaterTotDeb<-watermods$WaterTotDeb[match(all$WindGridMatch, watermods$WGM)]
save(allmods, file="transitmodels")
write.csv(allmods, file="alltransitmodels.csv")

#all$WindSourceIndex<-(all$WindSourceTotf-all$WindSourceSinkf)*all$Total_Debris


## Add wind source, wind sink, and pred to Syd_all data set (transects)

names(allmods)[16]<-"UID_1"
Syd_all2<-Syd_all
Syd_all2<-merge(Syd_all, allmods, by="UID_1")



# Also add grid predictionns to Syd_all dataset
Syd_all2$gridpred<-TotalDebris$pred[match(Syd_all2$UID_1, TotalDebris$UID)]
plot(Syd_all2$gridpred [Syd_all2$Source=="KAB"], Syd_all2$treepred[Syd_all$Source=="KAB"])

