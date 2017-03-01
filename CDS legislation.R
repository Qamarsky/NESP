##### if KABr does not yet exist, run LoadData script


KAB<- drop_read_csv ("/NESP/Data/Transect data/KAB/KAB_AllData_GlID.csv")

KAB$Total_Debris<-rowSums(KAB[,grepl("No.of", names(KAB))], na.rm=TRUE)

KAB$Containers<-rowSums(KAB[,c("No.of.Gb","No.of.Gc","No.of.Gd","No.of.Ge", "No.of.Gf", "No.of.Gh","No.of.Gk", "No.of.Gm",
                               "No.of.Gn","No.of.Ma","No.of.Mb","No.of.Mc", "No.of.Md","No.of.Me","No.of.Pc","No.of.Pe",
                               "No.of.Pg","No.of.Ph","No.of.Pi","No.of.Pj", "No.of.Pk","No.of.Ke","No.of.Kf","No.of.Kg",
                               "No.of.Ki")], na.rm=TRUE)

KAB$Lids<-rowSums(KAB[,c("No.of.Mi", "No.of.Px")], na.rm=TRUE)

KAB$Logcont<-log(KAB$Containers+1)

KAB$Propcont<-KAB$Containers/KAB$Total_Debris


### CUA


CUA <- drop_read("/NESP/Data/Transect data/CUA/CUA_AllData3.csv",header = FALSE,sep=",",strip.white = TRUE, skip=1)
CUAnames<- drop_read("/NESP/Data/Transect data/CUA/CUA_AllData3.csv",header = FALSE,sep=",",strip.white = TRUE, nrows=1)

colnames(CUA) <- as.character(unlist(CUAnames[1,]))

### Many of the categories are for some reason not numeric. Change those to numeric and then you can add the columns to get a total

CUA2<-CUA

CUA2[,86]<-as.numeric(paste(CUA2[,86]))
CUA2[,87]<-as.numeric(paste(CUA2[,87]))
CUA2[,91]<-as.numeric(paste(CUA2[,91]))
CUA2[,92]<-as.numeric(paste(CUA2[,92]))
CUA2[,96]<-as.numeric(paste(CUA2[,96]))
CUA2[,97]<-as.numeric(paste(CUA2[,97]))
CUA2[,98]<-as.numeric(paste(CUA2[,98]))
CUA2[,99]<-as.numeric(paste(CUA2[,99]))
CUA2[,105]<-as.numeric(paste(CUA2[,105]))
CUA2[,112]<-as.numeric(paste(CUA2[,112]))
CUA2[,117]<-as.numeric(paste(CUA2[,117]))
CUA2[,118]<-as.numeric(paste(CUA2[,118]))
CUA2[,120]<-as.numeric(paste(CUA2[,120]))
CUA2[,121]<-as.numeric(paste(CUA2[,121]))
CUA2[,122]<-as.numeric(paste(CUA2[,122]))
CUA2[,123]<-as.numeric(paste(CUA2[,123]))
CUA2[,126]<-as.numeric(paste(CUA2[,126]))
CUA2[,133]<-as.numeric(paste(CUA2[,133]))
CUA2[,139]<-as.numeric(paste(CUA2[,139]))
CUA2[,141]<-as.numeric(paste(CUA2[,141]))
CUA2[,143]<-as.numeric(paste(CUA2[,143]))
CUA2[,145]<-as.numeric(paste(CUA2[,145]))
CUA2[,147]<-as.numeric(paste(CUA2[,147]))
CUA2[,148]<-as.numeric(paste(CUA2[,148]))
CUA2[,150]<-as.numeric(paste(CUA2[,150]))
CUA2[,154]<-as.numeric(paste(CUA2[,154]))
CUA2[,171]<-as.numeric(paste(CUA2[,171]))

CUA2$Totalcalc<-rowSums(CUA2[,c(85:107,110:157,159,161:178)], na.rm=TRUE)  ## for some reason many of these rows are not numeric

CUA2$Containers<-rowSums(CUA2[,c(94:96,120,123,124,147:148)])
CUA2$Lids<-rowSums(CUA2[,c(98,152)])
CUA2$State[CUA2$State=="Vic"]<-"VIC"


##### Container analysis #####

lm.K.1<-glm(Logcont~State*Year, data=KAB)

lm.K.2<-lm(Logcont~Year, data=KAB[KAB$State=="NT",])

lm.K.3<-glm(Propcont~State*Year, data=KAB)
Proppred<-predict(lm.K.3, newdata=KAB, type="response", se.fit=T, na.action=na.pass)


Gam.K.1<-gam(Logcont~s(Year, k=8) + State, data=KAB)


## chris' code for beverage containers 
M.0 <- glm(cbind(Containers,round(Total_Debris)-Containers) ~ 1, family = "binomial", data = KAB)
M.S <- glm(cbind(Containers,round(Total_Debris)-Containers) ~ State, family = "binomial", data = KAB)

#so are there more beverage lids, either per beverage item or per cap?
M.tSt <- glm(cbind(Lids,round(Total_Debris)-Lids) ~ State, family = "binomial", data = KAB)
M.bS <- glm(cbind(Lids,Containers) ~ State, family = "binomial", data = KAB)


#get means just to check
by(KAB$Lids,KAB$State,mean)/by(KAB$Containers,KAB$State,mean)

#predict probabilities by site given model
PredictedRatio <- predict(M.S, newdata = KAB,type = "response",se.fit = T, na.action = na.pass)
PredictedLidsPerBottle <- predict(M.bS, newdata = KAB,type = "response",se.fit = T, na.action = na.pass)

#add predictions and se's to final data
KAB2 <- data.frame(KAB,unlist(PredictedRatio$fit),unlist(PredictedRatio$se.fit),
                   unlist(PredictedLidsPerBottle$fit),unlist(PredictedLidsPerBottle$se.fit))
names(KAB2)<- c(names(KAB2)[1:200],"PredictedRatio","PredictionSE","PredictedLidsPerBottle","PredictedLidsSE")

#test the factor levels to see if they are significantly different
library(multcomp)
summary(glht(M.S, mcp(State="Tukey")))

#make a barplot of the regression coefficients and their 95% CI's
pdf(file = "Barplot of regression coefficients for containers")
barplot(by(KAB2$PredictedRatio,KAB2$State,mean), ylim = c(0,.15), ylab = "Proportion of beverage containers", xlab = "State")
x0<- seq(.7,.7+7*1.2,1.2)
y0 <- by(KAB2$PredictedRatio,KAB2$State,mean)
y1 <- by(KAB2$PredictedRatio,KAB2$State,mean) + 1.96*by(KAB2$PredictionSE,KAB2$State,mean)
arrows(x0,y0,x1=x0, y1, angle = 90)
text(x = x0, y= (y1+0.005),labels = c("a","b","c","d","e","a","b","b"))
dev.off()

#what is the ratio between SA and the average across other states?
mean(by(KAB2$PredictedRatio[KAB2$State != "SA"],KAB2$State[KAB2$State != "SA"],mean), 
     na.rm = T)/mean(by(KAB2$PredictedRatio[KAB2$State == "SA"],KAB2$State[KAB2$State == "SA"],mean), na.rm = T)


## lids per bottle ##

#test the factor levels to see if they are significantly different
library(multcomp)
summary(glht(M.bS, mcp(State="Tukey")))


#make a barplot of the regression coefficients and their 95% CI's
pdf(file = "Barplot of regression coefficients for lids per bottleQ")
barplot(by(KAB2$PredictedLidsPerBottle,KAB2$State,mean), ylim = c(0,1), ylab = "Ratio of lids to bottles", xlab = "State")
x0<- seq(.7,.7+7*1.2,1.2)
y0 <- by(KAB2$PredictedLidsPerBottle,KAB2$State,mean)
y1 <- by(KAB2$PredictedLidsPerBottle,KAB2$State,mean) + 1.96*by(KAB2$PredictedLidsSE,KAB2$State,mean)
arrows(x0,y0,x1=x0, y1, angle = 90)
text(x = x0, y= (y1+0.07),labels = c("a","b","c","d","e","b","f","g"))
dev.off()

##### CUA DATA ######

CUA2$Containers[is.na(CUA2$Containers)==TRUE]<-0
CUA2$Lids[is.na(CUA2$Lids)==TRUE]<-0
CUA2$Totalcalc<-as.integer(CUA2$Totalcalc)
CUA2$Lids<-as.integer(CUA2$Lids)

## chris' code for beverage containers 
MC.0 <- glm(cbind(Containers,round(Totalcalc)-Containers) ~ 1, family = "binomial", data = CUA2)
MC.S <- glm(cbind(Containers,round(Totalcalc)-Containers) ~ State, family = "binomial", data = CUA2)

#so are there more beverage lids, either per beverage item or per cap?
MC.tSt <- glm(cbind(Lids,round(Totalcalc)-Lids) ~ State, family = "binomial", data = CUA2)
MC.bS <- glm(cbind(Lids,Containers) ~ State, family = "binomial", data = CUA2)


#get means just to check
by(CUA2$Lids,CUA2$State,mean)/by(CUA2$Containers,CUA2$State,mean)

#predict probabilities by site given model
PredictedRatio <- predict(M.S, newdata = KAB,type = "response",se.fit = T, na.action = na.pass)
PredictedLidsPerBottle <- predict(M.bS, newdata = KAB,type = "response",se.fit = T, na.action = na.pass)

#add predictions and se's to final data
KAB2 <- data.frame(KAB,unlist(PredictedRatio$fit),unlist(PredictedRatio$se.fit),
                   unlist(PredictedLidsPerBottle$fit),unlist(PredictedLidsPerBottle$se.fit))
names(KAB2)<- c(names(KAB2)[1:200],"PredictedRatio","PredictionSE","PredictedLidsPerBottle","PredictedLidsSE")

#test the factor levels to see if they are significantly different
library(multcomp)
summary(glht(M.S, mcp(State="Tukey")))

#make a barplot of the regression coefficients and their 95% CI's
pdf(file = "Barplot of regression coefficients for containers")
barplot(by(KAB2$PredictedRatio,KAB2$State,mean), ylim = c(0,.15), ylab = "Proportion of beverage containers", xlab = "State")
x0<- seq(.7,.7+7*1.2,1.2)
y0 <- by(KAB2$PredictedRatio,KAB2$State,mean)
y1 <- by(KAB2$PredictedRatio,KAB2$State,mean) + 1.96*by(KAB2$PredictionSE,KAB2$State,mean)
arrows(x0,y0,x1=x0, y1, angle = 90)
text(x = x0, y= (y1+0.005),labels = c("a","b","c","d","e","a","b","b"))
dev.off()

#what is the ratio between SA and the average across other states?
mean(by(KAB2$PredictedRatio[KAB2$State != "SA"],KAB2$State[KAB2$State != "SA"],mean), 
     na.rm = T)/mean(by(KAB2$PredictedRatio[KAB2$State == "SA"],KAB2$State[KAB2$State == "SA"],mean), na.rm = T)


## lids per bottle ##

#test the factor levels to see if they are significantly different
library(multcomp)
summary(glht(M.bS, mcp(State="Tukey")))


#make a barplot of the regression coefficients and their 95% CI's
pdf(file = "Barplot of regression coefficients for lids per bottleQ")
barplot(by(KAB2$PredictedLidsPerBottle,KAB2$State,mean), ylim = c(0,1), ylab = "Ratio of lids to bottles", xlab = "State")
x0<- seq(.7,.7+7*1.2,1.2)
y0 <- by(KAB2$PredictedLidsPerBottle,KAB2$State,mean)
y1 <- by(KAB2$PredictedLidsPerBottle,KAB2$State,mean) + 1.96*by(KAB2$PredictedLidsSE,KAB2$State,mean)
arrows(x0,y0,x1=x0, y1, angle = 90)
text(x = x0, y= (y1+0.07),labels = c("a","b","c","d","e","b","f","g"))
dev.off()


drop_upload("~/Documents/R data/NESP/R scripts/CDS legislation.R", dest="/NESP/R scripts")


