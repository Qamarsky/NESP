## Spearman's rank correlation ##
### have to use Spearman's rank instead of Pearson's because totals are not normally distributed ##


# between actual debris and each model

#### CSIRO stuff is actually pretty much only 3 sites, so not really all that useful. 
### KAB ####
Ktpr<-cor.test(Syd_all2$treeresids[Syd_all2$Source=="KAB"], Syd_all2$treepred[Syd_all2$Source=="KAB"], method="spearman")
Kgpr<-cor.test(Syd_all2$treeresids[Syd_all2$Source=="KAB"], Syd_all2$Gampred[Syd_all2$Source=="KAB"], method="spearman")
K1r<-cor.test(Syd_all2$treeresids[Syd_all2$Source=="KAB"], Syd_all2$upwind[Syd_all2$Source=="KAB"], method="spearman")
K2ar<-cor.test(Syd_all2$treeresids[Syd_all2$Source=="KAB"], Syd_all2$WindtotNoDeb[Syd_all2$Source=="KAB"], method="spearman")
K2br<-cor.test(Syd_all2$treeresids[Syd_all2$Source=="KAB"], Syd_all2$UpwindDeb[Syd_all2$Source=="KAB"], method="spearman")
K2cr<-cor.test(Syd_all2$treeresids[Syd_all2$Source=="KAB"], Syd_all2$WindSinkTotf[Syd_all2$Source=="KAB"], method="spearman")
K3ar<-cor.test(Syd_all2$treeresids[Syd_all2$Source=="KAB"], Syd_all2$WinddisttotNodeb[Syd_all2$Source=="KAB"], method="spearman")
K3br<-cor.test(Syd_all2$treeresids[Syd_all2$Source=="KAB"], Syd_all2$Winddistposonly[Syd_all2$Source=="KAB"], method="spearman")
K3cr<-cor.test(Syd_all2$treeresids[Syd_all2$Source=="KAB"], Syd_all2$Winddistdeb[Syd_all2$Source=="KAB"], method="spearman")
K1wr<-cor.test(Syd_all2$treeresids[Syd_all2$Source=="KAB"], Syd_all2$Uphillsum[Syd_all2$Source=="KAB"], method="spearman")
K2awr<-cor.test(Syd_all2$treeresids[Syd_all2$Source=="KAB"], Syd_all2$WaterTotnoDeb[Syd_all2$Source=="KAB"], method="spearman")
K2bwr<-cor.test(Syd_all2$treeresids[Syd_all2$Source=="KAB"], Syd_all2$Uphilldeb[Syd_all2$Source=="KAB"], method="spearman")
K2cwr<-cor.test(Syd_all2$treeresids[Syd_all2$Source=="KAB"], Syd_all2$WaterSinkTot[Syd_all2$Source=="KAB"], method="spearman")
K3bwr<-cor.test(Syd_all2$treeresids[Syd_all2$Source=="KAB"], Syd_all2$Waterdistposonly[Syd_all2$Source=="KAB"], method="spearman")
K3cwr<-cor.test(Syd_all2$treeresids[Syd_all2$Source=="KAB"], Syd_all2$Waterdistdebf[Syd_all2$Source=="KAB"], method="spearman")

K2er<-cor.test(Syd_all2$treeresids[Syd_all2$Source=="KAB"], Syd_all2$WaterTotdeb[Syd_all2$Source=="KAB"], method="spearman")
Kfar<-cor.test(Syd_all2$treeresids[Syd_all2$Source=="KAB"], Syd_all2$flowacc[Syd_all2$Source=="KAB"], method="spearman")


Ktpt<-cor.test(Syd_all2$Totper1000[Syd_all2$Source=="KAB"], Syd_all2$treepred[Syd_all2$Source=="KAB"], method="spearman")
Kgpt<-cor.test(Syd_all2$Totper1000[Syd_all2$Source=="KAB"], Syd_all2$Gampred[Syd_all2$Source=="KAB"], method="spearman")
K1t<-cor.test(Syd_all2$Totper1000[Syd_all2$Source=="KAB"], Syd_all2$upwind[Syd_all2$Source=="KAB"], method="spearman")
K2at<-cor.test(Syd_all2$Totper1000[Syd_all2$Source=="KAB"], Syd_all2$WindtotNoDeb[Syd_all2$Source=="KAB"], method="spearman")
K2bt<-cor.test(Syd_all2$Totper1000[Syd_all2$Source=="KAB"], Syd_all2$UpwindDeb[Syd_all2$Source=="KAB"], method="spearman")
K2ct<-cor.test(Syd_all2$Totper1000[Syd_all2$Source=="KAB"], Syd_all2$WindSinkTotf[Syd_all2$Source=="KAB"], method="spearman")
K3at<-cor.test(Syd_all2$Totper1000[Syd_all2$Source=="KAB"], Syd_all2$WinddisttotNodeb[Syd_all2$Source=="KAB"], method="spearman")
K3bt<-cor.test(Syd_all2$Totper1000[Syd_all2$Source=="KAB"], Syd_all2$Winddistposonly[Syd_all2$Source=="KAB"], method="spearman")
K3ct<-cor.test(Syd_all2$Totper1000[Syd_all2$Source=="KAB"], Syd_all2$Winddistdeb[Syd_all2$Source=="KAB"], method="spearman")
K1wt<-cor.test(Syd_all2$Totper1000[Syd_all2$Source=="KAB"], Syd_all2$Uphillsum[Syd_all2$Source=="KAB"], method="spearman")
K2awt<-cor.test(Syd_all2$Totper1000[Syd_all2$Source=="KAB"], Syd_all2$WaterTotnoDeb[Syd_all2$Source=="KAB"], method="spearman")
K2bwt<-cor.test(Syd_all2$Totper1000[Syd_all2$Source=="KAB"], Syd_all2$Uphilldeb[Syd_all2$Source=="KAB"], method="spearman")
K2cwt<-cor.test(Syd_all2$Totper1000[Syd_all2$Source=="KAB"], Syd_all2$WaterSinkTot[Syd_all2$Source=="KAB"], method="spearman")
K3bwt<-cor.test(Syd_all2$Totper1000[Syd_all2$Source=="KAB"], Syd_all2$Waterdistposonly[Syd_all2$Source=="KAB"], method="spearman")
K3cwt<-cor.test(Syd_all2$Totper1000[Syd_all2$Source=="KAB"], Syd_all2$Waterdistdebf[Syd_all2$Source=="KAB"], method="spearman")

K2et<-cor.test(Syd_all2$Totper1000[Syd_all2$Source=="KAB"], Syd_all2$WaterTotdeb[Syd_all2$Source=="KAB"], method="spearman")
Kfat<-cor.test(Syd_all2$Totper1000[Syd_all2$Source=="KAB"], Syd_all2$flowacc[Syd_all2$Source=="KAB"], method="spearman")


Ktpgr<-cor.test(Syd_all2$Gamresid[Syd_all2$Source=="KAB"], Syd_all2$treepred[Syd_all2$Source=="KAB"], method="spearman")
Kgpgr<-cor.test(Syd_all2$Totper1000[Syd_all2$Source=="KAB"], Syd_all2$Gampred[Syd_all2$Source=="KAB"], method="spearman")
K1gr<-cor.test(Syd_all2$Gamresid[Syd_all2$Source=="KAB"], Syd_all2$upwind[Syd_all2$Source=="KAB"], method="spearman")
K2agr<-cor.test(Syd_all2$Gamresid[Syd_all2$Source=="KAB"], Syd_all2$WindtotNoDeb[Syd_all2$Source=="KAB"], method="spearman")
K2bgr<-cor.test(Syd_all2$Gamresid[Syd_all2$Source=="KAB"], Syd_all2$UpwindDeb[Syd_all2$Source=="KAB"], method="spearman")
K2cgr<-cor.test(Syd_all2$Gamresid[Syd_all2$Source=="KAB"], Syd_all2$WindSinkTotf[Syd_all2$Source=="KAB"], method="spearman")
K3agr<-cor.test(Syd_all2$Gamresid[Syd_all2$Source=="KAB"], Syd_all2$WinddisttotNodeb[Syd_all2$Source=="KAB"], method="spearman")
K3bgr<-cor.test(Syd_all2$Gamresid[Syd_all2$Source=="KAB"], Syd_all2$Winddistposonly[Syd_all2$Source=="KAB"], method="spearman")
K3cgr<-cor.test(Syd_all2$Gamresid[Syd_all2$Source=="KAB"], Syd_all2$Winddistdeb[Syd_all2$Source=="KAB"], method="spearman")
K1wgr<-cor.test(Syd_all2$Gamresid[Syd_all2$Source=="KAB"], Syd_all2$Uphillsum[Syd_all2$Source=="KAB"], method="spearman")
K2awgr<-cor.test(Syd_all2$Gamresid[Syd_all2$Source=="KAB"], Syd_all2$WaterTotnoDeb[Syd_all2$Source=="KAB"], method="spearman")
K2bwgr<-cor.test(Syd_all2$Gamresid[Syd_all2$Source=="KAB"], Syd_all2$Uphilldeb[Syd_all2$Source=="KAB"], method="spearman")
K2cwgr<-cor.test(Syd_all2$Gamresid[Syd_all2$Source=="KAB"], Syd_all2$WaterSinkTot[Syd_all2$Source=="KAB"], method="spearman")
K3bwgr<-cor.test(Syd_all2$Gamresid[Syd_all2$Source=="KAB"], Syd_all2$Waterdistposonly[Syd_all2$Source=="KAB"], method="spearman")
K3cwgr<-cor.test(Syd_all2$Gamresid[Syd_all2$Source=="KAB"], Syd_all2$Waterdistdebf[Syd_all2$Source=="KAB"], method="spearman")

K2egr<-cor.test(Syd_all2$Gamresid[Syd_all2$Source=="KAB"], Syd_all2$WaterTotdeb[Syd_all2$Source=="KAB"], method="spearman")
Kfagr<-cor.test(Syd_all2$Gamresid[Syd_all2$Source=="KAB"], Syd_all2$flowacc[Syd_all2$Source=="KAB"], method="spearman")




test<-c("Tree predictions","Gam predictions", "Upwind sites","Total wind, no deb","Total wind, with debris", 
        "Proportional wind with debris", "Wind by distance, no deb","Wind dist pos only, no deb",
        "Upwind prop to distance with deb", "Uphill sites", "Total water, no debris","Total water, debris", 
        "Uphill sites times debris", "Proportional water times debris", "Nuber of uphill cells proportional to distance",
        "Magnitude of water, proportional to distance, times debris", "flow accumulation measure")
comparison<-rep(c("resids","total", "gamresids"), each=17)

#Ktests<-list(K6r, K2r, K1r, K3r, K4r, K8r, K7r, K9r, K10r, K5r, K11r, K6t, K2t, K1t, K3t, K4t, K8t, K7t, K9t, K10t, K5t, K11t) #, K6gr, K2gr, K1gr, K3gr, K4gr, K8gr, K7gr, K9gr, K10gr, K5gr, K11gr)
Ktests<-list(Ktpr, Kgpr, K1r, K2ar, K2br, K2cr, K3ar, K3br, K3cr, K1wr, K2awr, K2er, K2bwr, K2cwr, K3bwr, K3cwr,Kfar, 
             Ktpt, Kgpt, K1t, K2at, K2bt, K2ct, K3at, K3bt, K3ct, K1wt, K2awt, K2et, K2bwt, K2cwt, K3bwt, K3cwt, Kfat,
             Ktpgr, Kgpgr, K1gr, K2agr, K2bgr, K2cgr, K3agr, K3bgr, K3cgr, K1wgr, K2awgr, K2egr, K2bwgr, K2cwgr, K3bwgr, K3cwgr, Kfagr)


Kp<-lapply(Ktests, FUN = function(x){paste(x$p.value)})
Kr<-lapply(Ktests, FUN = function(x){paste(x$estimate)})


Kstats<-data.frame(comparison, test, as.numeric(unlist(Kr)), as.numeric(unlist(Kp)))
names(Kstats)<-c("comparison","test", "rho", "p")
write.csv(Kstats, "Kstats4.csv")




## Plot some of the cor tests ##

plot(Syd_all2$resids[Syd_all2$Source=="KAB"], Syd_all2$pred[Syd_all2$Source=="KAB"], ylab="Predictions (KAB)", xlab="Ctree Residuals")
plot(Syd_all2$resids[Syd_all2$Source=="KAB"], Syd_all2$WaterSink[Syd_all2$Source=="KAB"],ylab="Total water (KAB)", xlab="Ctree Residuals")
plot(Syd_all2$resids[Syd_all2$Source=="KAB"], Syd_all2$Winddistdeb[Syd_all2$Source=="KAB"], ylab="Wind dist (prop) with debris (KAB)", xlab="Ctree Residuals")
plot(Syd_all2$Totper1000[Syd_all2$Source=="KAB"], Syd_all2$WaterSinkdeb[Syd_all2$Source=="KAB"], ylab="Water Sink deb (KAB)", xlab="Observed debris")
plot(Syd_all2$Totper1000[Syd_all2$Source=="CSIRO"], Syd_all2$pred[Syd_all2$Source=="CSIRO"], ylab="Predictions (CSIRO)", xlab="Observed debris")
plot(Syd_all2$Gamresids[Syd_all2$Source=="CSIRO"], Syd_all2$Gampred[Syd_all2$Source=="CSIRO"], ylab="Gam predictions (CSIRO)", xlab="Gam residuals")
plot(Syd_all2$Totper1000[Syd_all2$Source=="CSIRO"], Syd_all2$Gampred[Syd_all2$Source=="CSIRO"], ylab="Gam predictions (CSIRO)", xlab="Observed debris")
plot(Syd_all2$Totper1000[Syd_all2$Source=="KAB"], Syd_all2$pred[Syd_all2$Source=="KAB"], ylab="Ctree predictions (KAB)", xlab="Observed debris")





# between residuals and source index
cor.test(Covars2$resids, Covars2$WindSourceIndex, method="spearman")

plot(x.var, y.var, xlab="x-label", ylab="y-label", pch=21))
#Add a line of best fit (if appropriate)     abline(lm(y.var ~ x.var)

## or pairwise

cor.mat<-cor.test(all, method="spearman")

drop_upload("~/Documents/R data/NESP/R scripts/Correlation testing.R", dest="/NESP/R scripts")




