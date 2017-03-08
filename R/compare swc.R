data.both.sap<- readRDS("mastra and sap.rds")

vwc.df <- read.csv("FACE_RA_P0037_DAILYMET_20110619-20151026_L2.csv")
vwc.df <- na.omit(vwc.df)
vwc.df$Date <- as.Date(vwc.df$Date)

sap.hr <- readRDS("sap_hrly.rds")  
sap.hr <- subset(sap.hr,select = c("DateHour","Ring","volRing"))

fn <- sprintf("Rings/Ring%s/runfolder/watbal.dat",1)
watbal <- read.table(fn,head=FALSE, skip=37)

# note the et here is evapotranspiration not just tranpiration
names(watbal) <- c("day", "hour", "wsoil", "wsoilroot", "ppt", "canopystore",
                   "evapstore", "drainstore", "tfall", "et", "etmeas",
                   "discharge", "overflow",  "weightedswp", "ktot", "drythick", "soilevap",
                   "soilmoist", "fsoil", "qh", "qe", "qn", "qc", "rglobund",
                   "rglobabv", "radinterc", "rnet", "totlai", "tair", "soilt1", "soilt2",
                   "fracw1", "fracw2", "fracaPAR")

# watbal.sub <- subset(watbal,select = c("day", "hour", "et"))
# names(watbal.sub) <- c("day", "half.hour", "et")
# watbal.sub$Hour <- rep(rep(seq(1,24),each=2),
#                        length(watbal.sub$half.hour)/48)
# 
# watbal.sub <- data.table(watbal.sub)[,list(et=mean(et,na.rm=TRUE)),
#                                  by = c("day","Hour")]
# 
# watbal.sub$DateHour <- rep(seq.POSIXt(as.POSIXlt("2012-10-26 00:00"),
#                                   as.POSIXlt("2013-03-26 24:00"),
#                                   by = "hour"),each=2)

watbal.sum <- data.table(watbal)[,list(swc.v=mean((fracw1 + fracw1)/2, na.rm=TRUE),
                                       tfall = sum(tfall, na.rm=TRUE),
                                       et = sum(et, na.rm=TRUE),
                                       ppt = sum(ppt, na.rm=TRUE)),
                                 by = day]

watbal.sum$Date <- unique(data.both.sap$Date)

data.both.sap.war <- merge(data.both.sap,watbal.sum,by = intersect(names(data.both.sap), names(watbal.sum)))

data.both.sap.war <- merge(data.both.sap.war,vwc.df,by = intersect(names(data.both.sap.war), names(vwc.df)))

data.both.sap.war <- data.both.sap.war[data.both.sap.war$volRing < 100,]
data.both.sap.war <- data.both.sap.war[data.both.sap.war$Ring == "R1",]
# plot(data.both.sap.war$trans~data.both.sap.war$volRing,
#      xlim=c(0,4),
#      ylim=c(0,4))
# abline(a=0,b=1)

data.both.sap.war$t.level <- cut(data.both.sap.war$TAIR,breaks = c(15,20,25,30,35))

pdf("maespa trans vs hp.pdf")
# plot trans
# plot with date
par(mar=c(5,5,2,5))
plot(data.both.sap.war$Trans~data.both.sap.war$Date,
     ylim=c(0,4),col="red",pch=16,
     xlab=" ",
     ylab=" ",
     cex=0.5)
par(new=1)
plot(data.both.sap.war$volRing~data.both.sap.war$Date,
     xlab="Date",
     ylab="Sap flow (L d-1)",
     ylim=c(0,4),
     col="blue",
     pch=16,
     cex=0.5)

legend("topright",legend = c("MAESPA","Heat Pulse"),pch=16,
       col=c("red","blue"),bty='n')
title("sap flow")
# scatter plot with r2
palette(c("cadetblue1","cyan2","darkgoldenrod1","brown3"))
plot(data.both.sap.war$Trans~data.both.sap.war$volRing,
     xlim=c(0,4),
     ylim=c(0,4),
     pch=16,
     cex=0.8,
     xlab=expression(Sap~flow~from~heat~pulse~(L~d^-1)),
     ylab=expression(Sap~flow~MAESPA~(L~d^-1)),
     col=data.both.sap.war$t.level)
abline(a=0,b=1)
fit.sap <- lm(data.both.sap.war$Trans~data.both.sap.war$volRing)
abline(fit.sap,col="navy",lty="dashed")
adj.r.sqrt <- summary(fit.sap)$adj.r.squared
mylabel <- bquote(italic(R)^2 == .(format(adj.r.sqrt, digits = 2)))
text(x = 3.5, y = 3, labels = mylabel)

legend("bottomright",legend = levels(data.both.sap.war$t.level),
       col=palette(),
       pch=16,
       bty='n',
       title = "Tair")

# tran vs vpd
plot(data.both.sap.war$Trans~data.both.sap.war$VPD,
     xlim=c(0,3),
     ylim=c(0,4),col="red",pch=16,cex=0.5,
     xlab="VPD (kPa)",
     ylab="Transpiration (mm/d)")

par(new=1)
plot(data.both.sap.war$volRing~data.both.sap.war$VPD,
     xlim=c(0,3),
     ylim=c(0,4),col="blue",pch=16,cex=0.5,
     xlab=" ",
     ylab=" ")

legend("topright",legend = c("MAESPA","Heat Pulse"),pch=16,
       col=c("red","blue"),bty='n')
title("Trans vs VPD (Daily)")
# plot swc
plot(data.both.sap.war$swc.v~data.both.sap.war$Date,
     ylim=c(0,0.5),
     col="red",
     pch=16,
     xlab="Date",
     ylab="Volumetric SWC",
     cex=0.6
)
par(new=1)
plot(data.both.sap.war$VWC~data.both.sap.war$Date,
     xlab="Date",
     ylab="Volumetric SWC",
     ylim=c(0,0.5),
     col="blue",
     pch=16,
     cex=0.6)

par(new=1)
plot(data.both.sap.war$ppt~data.both.sap.war$Date,
     xlab="Date",
     ylab="PPT",
     type="s",
     ylim=c(0,120),
     col="deepskyblue",
     pch=16,
     ann=0,
     axes=0)
axis(4,at=seq(0,120,30),labels = paste0(seq(0,120,30)),col="deepskyblue",col.axis="deepskyblue")
mtext("PPT (mm)",side=4,line = 3,col="deepskyblue")
legend("topright",legend = c("MAESPA","Measured"),pch=16,
       col=c("red","blue"),bty='n')
title("SWC of the top 80cm")

# plot(data.both.sap.war$swc.v~data.both.sap.war$VWC,
#      xlim=c(0,0.3),
#      ylim=c(0,0.3),
#      xlab="Measured",
#      ylab="MAESPA")
# title("SWC of the top 30cm")
dev.off()