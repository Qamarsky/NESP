## Spearman's rank correlation ##

# between actual debris and each model
K1r<-cor.test(Bris_all$resids[Bris_all$Source=="KAB"], Bris_all$WindSink[Bris_all$Source=="KAB"], method="spearman")
K2r<-cor.test(Bris_all$resids[Bris_all$Source=="KAB"], Bris_all$Upwind[Bris_all$Source=="KAB"], method="spearman")
K3r<-cor.test(Bris_all$resids[Bris_all$Source=="KAB"], Bris_all$WindSinkdis[Bris_all$Source=="KAB"], method="spearman")
K4r<-cor.test(Bris_all$resids[Bris_all$Source=="KAB"], Bris_all$WindSinkdispos[Bris_all$Source=="KAB"], method="spearman")
K5r<-cor.test(Bris_all$resids[Bris_all$Source=="KAB"], Bris_all$Uphill[Bris_all$Source=="KAB"], method="spearman")
K6r<-cor.test(Bris_all$resids[Bris_all$Source=="KAB"], Bris_all$pred[Bris_all$Source=="KAB"], method="spearman")
K7r<-cor.test(Bris_all$resids[Bris_all$Source=="KAB"], Bris_all$WaterSink[Bris_all$Source=="KAB"], method="spearman")
K8r<-cor.test(Bris_all$resids[Bris_all$Source=="KAB"], Bris_all$Upwinddeb[Bris_all$Source=="KAB"], method="spearman")
K9r<-cor.test(Bris_all$resids[Bris_all$Source=="KAB"], Bris_all$WindSinkdeb[Bris_all$Source=="KAB"], method="spearman")
K10r<-cor.test(Bris_all$resids[Bris_all$Source=="KAB"], Bris_all$Winddistdeb[Bris_all$Source=="KAB"], method="spearman")
K11r<-cor.test(Bris_all$resids[Bris_all$Source=="KAB"], Bris_all$WaterSinkdeb[Bris_all$Source=="KAB"], method="spearman")

K1t<-cor.test(Bris_all$Totper1000[Bris_all$Source=="KAB"], Bris_all$WindSink[Bris_all$Source=="KAB"], method="spearman")
K2t<-cor.test(Bris_all$Totper1000[Bris_all$Source=="KAB"], Bris_all$Upwind[Bris_all$Source=="KAB"], method="spearman")
K3t<-cor.test(Bris_all$Totper1000[Bris_all$Source=="KAB"], Bris_all$WindSinkdis[Bris_all$Source=="KAB"], method="spearman")
K4t<-cor.test(Bris_all$Totper1000[Bris_all$Source=="KAB"], Bris_all$WindSinkdispos[Bris_all$Source=="KAB"], method="spearman")
K5t<-cor.test(Bris_all$Totper1000[Bris_all$Source=="KAB"], Bris_all$Uphill[Bris_all$Source=="KAB"], method="spearman")
K6t<-cor.test(Bris_all$Totper1000[Bris_all$Source=="KAB"], Bris_all$pred[Bris_all$Source=="KAB"], method="spearman")
K7t<-cor.test(Bris_all$Totper1000[Bris_all$Source=="KAB"], Bris_all$WaterSink[Bris_all$Source=="KAB"], method="spearman")
K8t<-cor.test(Bris_all$Totper1000[Bris_all$Source=="KAB"], Bris_all$Upwinddeb[Bris_all$Source=="KAB"], method="spearman")
K9t<-cor.test(Bris_all$Totper1000[Bris_all$Source=="KAB"], Bris_all$WindSinkdeb[Bris_all$Source=="KAB"], method="spearman")
K10t<-cor.test(Bris_all$Totper1000[Bris_all$Source=="KAB"], Bris_all$Winddistdeb[Bris_all$Source=="KAB"], method="spearman")
K11t<-cor.test(Bris_all$Totper1000[Bris_all$Source=="KAB"], Bris_all$WaterSinkdeb[Bris_all$Source=="KAB"], method="spearman")


K1gr<-cor.test(Bris_all$Gamresids[Bris_all$Source=="KAB"], Bris_all$WindSink[Bris_all$Source=="KAB"], method="spearman")
K2gr<-cor.test(Bris_all$Gamresids[Bris_all$Source=="KAB"], Bris_all$Upwind[Bris_all$Source=="KAB"], method="spearman")
K3gr<-cor.test(Bris_all$Gamresids[Bris_all$Source=="KAB"], Bris_all$WindSinkdis[Bris_all$Source=="KAB"], method="spearman")
K4gr<-cor.test(Bris_all$Gamresids[Bris_all$Source=="KAB"], Bris_all$WindSinkdispos[Bris_all$Source=="KAB"], method="spearman")
K5gr<-cor.test(Bris_all$Gamresids[Bris_all$Source=="KAB"], Bris_all$Uphill[Bris_all$Source=="KAB"], method="spearman")
K6gr<-cor.test(Bris_all$Gamresids[Bris_all$Source=="KAB"], Bris_all$pred[Bris_all$Source=="KAB"], method="spearman")
K7gr<-cor.test(Bris_all$Gamresids[Bris_all$Source=="KAB"], Bris_all$WaterSink[Bris_all$Source=="KAB"], method="spearman")
K8gr<-cor.test(Bris_all$Gamresids[Bris_all$Source=="KAB"], Bris_all$Upwinddeb[Bris_all$Source=="KAB"], method="spearman")
K9gr<-cor.test(Bris_all$Gamresids[Bris_all$Source=="KAB"], Bris_all$WindSinkdeb[Bris_all$Source=="KAB"], method="spearman")
K10gr<-cor.test(Bris_all$Gamresids[Bris_all$Source=="KAB"], Bris_all$Winddistdeb[Bris_all$Source=="KAB"], method="spearman")
K11gr<-cor.test(Bris_all$Gamresids[Bris_all$Source=="KAB"], Bris_all$WaterSinkdeb[Bris_all$Source=="KAB"], method="spearman")


C1r<-cor.test(Bris_all$resids[Bris_all$Source=="CSIRO"], Bris_all$WindSink[Bris_all$Source=="CSIRO"], method="spearman")
C2r<-cor.test(Bris_all$resids[Bris_all$Source=="CSIRO"], Bris_all$Upwind[Bris_all$Source=="CSIRO"], method="spearman")
C3r<-cor.test(Bris_all$resids[Bris_all$Source=="CSIRO"], Bris_all$WindSinkdis[Bris_all$Source=="CSIRO"], method="spearman")
C4r<-cor.test(Bris_all$resids[Bris_all$Source=="CSIRO"], Bris_all$WindSinkdispos[Bris_all$Source=="CSIRO"], method="spearman")
C5r<-cor.test(Bris_all$resids[Bris_all$Source=="CSIRO"], Bris_all$Uphill[Bris_all$Source=="CSIRO"], method="spearman")
C6r<-cor.test(Bris_all$resids[Bris_all$Source=="CSIRO"], Bris_all$pred[Bris_all$Source=="CSIRO"], method="spearman")
C7r<-cor.test(Bris_all$resids[Bris_all$Source=="CSIRO"], Bris_all$WaterSink[Bris_all$Source=="CSIRO"], method="spearman")
C8r<-cor.test(Bris_all$resids[Bris_all$Source=="CSIRO"], Bris_all$Upwinddeb[Bris_all$Source=="CSIRO"], method="spearman")
C9r<-cor.test(Bris_all$resids[Bris_all$Source=="CSIRO"], Bris_all$WindSinkdeb[Bris_all$Source=="CSIRO"], method="spearman")
C10r<-cor.test(Bris_all$resids[Bris_all$Source=="CSIRO"], Bris_all$Winddistdeb[Bris_all$Source=="CSIRO"], method="spearman")
C11r<-cor.test(Bris_all$resids[Bris_all$Source=="CSIRO"], Bris_all$WaterSinkdeb[Bris_all$Source=="CSIRO"], method="spearman")



C1t<-cor.test(Bris_all$Totper1000[Bris_all$Source=="CSIRO"], Bris_all$WindSink[Bris_all$Source=="CSIRO"], method="spearman")
C2t<-cor.test(Bris_all$Totper1000[Bris_all$Source=="CSIRO"], Bris_all$Upwind[Bris_all$Source=="CSIRO"], method="spearman")
C3t<-cor.test(Bris_all$Totper1000[Bris_all$Source=="CSIRO"], Bris_all$WindSinkdis[Bris_all$Source=="CSIRO"], method="spearman")
C4t<-cor.test(Bris_all$Totper1000[Bris_all$Source=="CSIRO"], Bris_all$WindSinkdispos[Bris_all$Source=="CSIRO"], method="spearman")
C5t<-cor.test(Bris_all$Totper1000[Bris_all$Source=="CSIRO"], Bris_all$Uphill[Bris_all$Source=="CSIRO"], method="spearman")
C6t<-cor.test(Bris_all$Totper1000[Bris_all$Source=="CSIRO"], Bris_all$pred[Bris_all$Source=="CSIRO"], method="spearman")
C7t<-cor.test(Bris_all$Totper1000[Bris_all$Source=="CSIRO"], Bris_all$WaterSink[Bris_all$Source=="CSIRO"], method="spearman")
C8t<-cor.test(Bris_all$Totper1000[Bris_all$Source=="CSIRO"], Bris_all$Upwinddeb[Bris_all$Source=="CSIRO"], method="spearman")
C9t<-cor.test(Bris_all$Totper1000[Bris_all$Source=="CSIRO"], Bris_all$WindSinkdeb[Bris_all$Source=="CSIRO"], method="spearman")
C10t<-cor.test(Bris_all$Totper1000[Bris_all$Source=="CSIRO"], Bris_all$Winddistdeb[Bris_all$Source=="CSIRO"], method="spearman")
C11t<-cor.test(Bris_all$Totper1000[Bris_all$Source=="CSIRO"], Bris_all$WaterSinkdeb[Bris_all$Source=="CSIRO"], method="spearman")



C1gr<-cor.test(Bris_all$Gamresids[Bris_all$Source=="CSIRO"], Bris_all$WindSink[Bris_all$Source=="CSIRO"], method="spearman")
C2gr<-cor.test(Bris_all$Gamresids[Bris_all$Source=="CSIRO"], Bris_all$Upwind[Bris_all$Source=="CSIRO"], method="spearman")
C3gr<-cor.test(Bris_all$Gamresids[Bris_all$Source=="CSIRO"], Bris_all$WindSinkdis[Bris_all$Source=="CSIRO"], method="spearman")
C4gr<-cor.test(Bris_all$Gamresids[Bris_all$Source=="CSIRO"], Bris_all$WindSinkdispos[Bris_all$Source=="CSIRO"], method="spearman")
C5gr<-cor.test(Bris_all$Gamresids[Bris_all$Source=="CSIRO"], Bris_all$Uphill[Bris_all$Source=="CSIRO"], method="spearman")
C6gr<-cor.test(Bris_all$Gamresids[Bris_all$Source=="CSIRO"], Bris_all$pred[Bris_all$Source=="CSIRO"], method="spearman")
C7gr<-cor.test(Bris_all$Gamresids[Bris_all$Source=="CSIRO"], Bris_all$WaterSink[Bris_all$Source=="CSIRO"], method="spearman")
C8gr<-cor.test(Bris_all$Gamresids[Bris_all$Source=="CSIRO"], Bris_all$Upwinddeb[Bris_all$Source=="CSIRO"], method="spearman")
C9gr<-cor.test(Bris_all$Gamresids[Bris_all$Source=="CSIRO"], Bris_all$WindSinkdeb[Bris_all$Source=="CSIRO"], method="spearman")
C10gr<-cor.test(Bris_all$Gamresids[Bris_all$Source=="CSIRO"], Bris_all$Winddistdeb[Bris_all$Source=="CSIRO"], method="spearman")
C11gr<-cor.test(Bris_all$Gamresids[Bris_all$Source=="CSIRO"], Bris_all$WaterSinkdeb[Bris_all$Source=="CSIRO"], method="spearman")



C1gp<-cor.test(Bris_all$Gamresids[Bris_all$Source=="CSIRO"], Bris_all$WaterSinkdeb[Bris_all$Source=="CSIRO"], method="spearman")



test<-c("Predictions only", "Upwind sites","Total wind, no deb","Wind dist, no deb","Wind dist pos only, no deb","Upwind with deb", "Water total", "Total wind (prop) with deb","Wind dist (prop) with deb", "Uphill", "Water Sink deb")
comparison<-rep(c("resids","total","gamresids"), each=11)
CStests<-list(C6r, C2r, C1r, C3r, C4r, C8r, C7r, C9r, C10r, C5r, C11r, C6t, C2t, C1t, C3t, C4t, C8t, C7t, C9t, C10t, C5t, C11t, C6gr, C2gr, C1gr, C3gr, C4gr, C8gr, C7gr, C9gr, C10gr, C5gr, C11gr)
CSp<-lapply(CStests, FUN = function(x){paste(x$p.value)})
CSr<-lapply(CStests, FUN = function(x){paste(x$estimate)})

Ktests<-list(K6r, K2r, K1r, K3r, K4r, K8r, K7r, K9r, K10r, K5r, K11r, K6t, K2t, K1t, K3t, K4t, K8t, K7t, K9t, K10t, K5t, K11t, K6gr, K2gr, K1gr, K3gr, K4gr, K8gr, K7gr, K9gr, K10gr, K5gr, K11gr)
Kp<-lapply(Ktests, FUN = function(x){paste(x$p.value)})
Kr<-lapply(Ktests, FUN = function(x){paste(x$estimate)})

CSstats<-data.frame(comparison, test, as.numeric(unlist(CSr)), as.numeric(unlist(CSp)))
names(CSstats)<-c("comparison","test", "rho", "p")

write.csv(CSstats, "CSstats.csv")

Kstats<-data.frame(comparison, test, as.numeric(unlist(Kr)), as.numeric(unlist(Kp)))
names(Kstats)<-c("comparison","test", "rho", "p")
write.csv(Kstats, "Kstats.csv")

CSr1<-cor.test(Bris_CSall$resids, Bris_CSall$WindSink, method="spearman")
CSr2<-cor.test(Bris_CSall$resids, Bris_CSall$Upwind, method="spearman")
CSr3<-cor.test(Bris_CSall$resids, Bris_CSall$WindSinkdis, method="spearman")
CSr4<-cor.test(Bris_CSall$resids, Bris_CSall$WindSinkdispos, method="spearman")
CSr5<-cor.test(Bris_CSall$resids, Bris_CSall$Uphill, method="spearman")
CSr6<-cor.test(Bris_CSall$resids, Bris_CSall$pred, method="spearman")

CSt1<-cor.test(Bris_CSall$Totper1000, Bris_CSall$WindSink, method="spearman")
CSt2<-cor.test(Bris_CSall$Totper1000, Bris_CSall$Upwind, method="spearman")
CSt3<-cor.test(Bris_CSall$Totper1000, Bris_CSall$WindSinkdis, method="spearman")
CSt4<-cor.test(Bris_CSall$Totper1000, Bris_CSall$WindSinkdispos, method="spearman")
CSt5<-cor.test(Bris_CSall$Totper1000, Bris_CSall$Uphill, method="spearman")
CSt6<-cor.test(Bris_CSall$Totper1000, Bris_CSall$pred, method="spearman")

CSalltests<-list(CSr1, CSr2, CSr3, CSr4, CSr5, CSr6, CSt1, CSt2, CSt3, CSt4, CSt5, CSt6)
CSap<-lapply(CSalltests, FUN = function(x){paste(x$p.value)})
CSar<-lapply(CSalltests, FUN = function(x){paste(x$estimate)})


CSallstats<-data.frame(as.numeric(unlist(CSar)), as.numeric(unlist(CSap)))
names(CSallstats)<-c("rho", "p")
write.csv(CSallstats, "CSallstats.csv")



## Plot some of the cor tests ##

plot(Bris_all$resids[Bris_all$Source=="KAB"], Bris_all$pred[Bris_all$Source=="KAB"], ylab="Predictions (KAB)", xlab="Ctree Residuals")
plot(Bris_all$resids[Bris_all$Source=="KAB"], Bris_all$WaterSink[Bris_all$Source=="KAB"],ylab="Total water (KAB)", xlab="Ctree Residuals")
plot(Bris_all$resids[Bris_all$Source=="KAB"], Bris_all$Winddistdeb[Bris_all$Source=="KAB"], ylab="Wind dist (prop) with debris (KAB)", xlab="Ctree Residuals")
plot(Bris_all$Totper1000[Bris_all$Source=="KAB"], Bris_all$WaterSinkdeb[Bris_all$Source=="KAB"], ylab="Water Sink deb (KAB)", xlab="Observed debris")
plot(Bris_all$Totper1000[Bris_all$Source=="CSIRO"], Bris_all$pred[Bris_all$Source=="CSIRO"], ylab="Predictions (CSIRO)", xlab="Observed debris")
plot(Bris_all$Gamresids[Bris_all$Source=="CSIRO"], Bris_all$Gampred[Bris_all$Source=="CSIRO"], ylab="Gam predictions (CSIRO)", xlab="Gam residuals")
plot(Bris_all$Totper1000[Bris_all$Source=="CSIRO"], Bris_all$Gampred[Bris_all$Source=="CSIRO"], ylab="Gam predictions (CSIRO)", xlab="Observed debris")
plot(Bris_all$Totper1000[Bris_all$Source=="KAB"], Bris_all$pred[Bris_all$Source=="KAB"], ylab="Ctree predictions (KAB)", xlab="Observed debris")





# between residuals and source index
cor.test(Covars2$resids, Covars2$WindSourceIndex, method="spearman")

plot(x.var, y.var, xlab="x-label", ylab="y-label", pch=21))
#Add a line of best fit (if appropriate)     abline(lm(y.var ~ x.var)

## or pairwise

cor.mat<-cor.test(all, method="spearman")

drop_upload("~/Documents/R data/NESP/R scripts/Correlation testing.R", dest="/NESP/R scripts")


