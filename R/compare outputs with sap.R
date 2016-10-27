source("R/load.R")
# get swc from hiev
downloadHIEv(searchHIEv("FACE_RA_P0037_DAILYMET_20110619-20151026_L2.csv"))

swc.df <- read.csv("download/FACE_RA_P0037_DAILYMET_20110619-20151026_L2.csv")
swc.df$Date <- as.Date(swc.df$Date)

soilwater <- swc.df[,c("Date","VWC")]
soilwater <- na.omit(soilwater)

maestra.sap.df.temp <- readRDS("mastra and sap.rds")

maestra.sap.df <- merge(maestra.sap.df.temp,soilwater,by = intersect(names(maestra.sap.df.temp),names(soilwater)))

maestra.sap.df$lai.measured <- maestra.sap.df$LAI
maestra.sap.df$c_treat <- "e"
maestra.sap.df$c_treat[maestra.sap.df$Ring == "R2" | 
                         maestra.sap.df$Ring == "R3" | 
                         maestra.sap.df$Ring == "R6"] <- "a"

maestra.sap.df$c_treat <- as.factor(maestra.sap.df$c_treat)
maestra.sap.sumByC <- summaryBy(GPP + Trans + volRing + VPD + PAR + LAI + VWC + TAIR~
                                  Date + c_treat,
                                data = maestra.sap.df, FUN = mean,na.rm=TRUE)

maestra.sap.sumByC.R <- summaryBy(GPP + Trans + volRing + VPD +PAR + LAI ~ Date + c_treat + Ring,
                                  data = maestra.sap.df, FUN = mean,na.rm=TRUE)

maestra.sap.sumByC.R$mon <- str_sub(as.character(maestra.sap.sumByC.R$Date),1,7)

maestra.sap.sumByC.mon <- summaryBy(GPP.mean + Trans.mean  + volRing.mean + VPD.mean + PAR.mean +LAI.mean~ 
                                      mon + c_treat + Ring,
                                data = maestra.sap.sumByC.R, FUN = mean,na.rm=TRUE)

maestra.sap.sumByC$mon <- str_sub(as.character(maestra.sap.sumByC$Date),1,7)

monthly.met <- summaryBy(VPD.mean + PAR.mean + LAI.mean + VWC.mean + TAIR.mean~
                         mon,
                         data = maestra.sap.sumByC, FUN = mean,na.rm=TRUE)

# get fitted data for each month
months <- unique(maestra.sap.sumByC$mon)

df.out <- data.frame(mon = months,sap.a=NA,sap.e=NA,trans.a=NA,trans.e=NA)

for (month.value in months){
  
  sap.df.sub <- maestra.sap.sumByC[which(maestra.sap.sumByC$mon == month.value),]

# fit and assign data
fit.sap.func <- function(df){
  lm(volRing.mean ~ LAI.mean * c_treat, data = df)
}

fit.trans.func <- function(df){
  lm(Trans.mean ~ LAI.mean * c_treat, data = df)
}

fit.sap <- fit.sap.func(sap.df.sub)

sap.pred <- predict(fit.sap, data.frame(LAI.mean=mean(sap.df.sub$LAI), c_treat = c("a","e")))

df.out$sap.a[which(df.out$mon == month.value)] <- sap.pred[1]
df.out$sap.e[which(df.out$mon == month.value)] <- sap.pred[2]

fit.trans <- fit.trans.func(sap.df.sub)
trans.pred <- predict(fit.trans, data.frame(LAI.mean=mean(sap.df.sub$LAI), c_treat = c("a","e")))

df.out$trans.a[which(df.out$mon == month.value)] <- trans.pred[1]
df.out$trans.e[which(df.out$mon == month.value)] <- trans.pred[2]
  
}

sap.with.met.df <- merge(df.out,monthly.met)

plot(sap.with.met.df$sap.a,ylim=c(0,1.4))
par(new=TRUE)
plot(sap.with.met.df$sap.e,ylim=c(0,1.4),col="red")

pdf("Trans vs lai of each month.pdf",width = 10,height = 7)
plot(sap.with.met.df$sap.a ~ sap.with.met.df$VPD.mean.mean,ylim=c(0,1.4),xlim=c(0,1.5),pch=16,
     xlab = "VPD",ylab="Transpiration from heat pulse (mm/d)")
abline(lm(sap.with.met.df$sap.a ~ sap.with.met.df$VPD.mean.mean))
par(new=TRUE)
plot(sap.with.met.df$sap.e~ sap.with.met.df$VPD.mean.mean,xlim=c(0,1.5),ylim=c(0,1.4),pch=16,
     col="red",
     xlab = " ",ylab=" ")
abline(lm(sap.with.met.df$sap.e ~ sap.with.met.df$VPD.mean.mean),col="red")
legend("topleft",legend = c("Elevated","Ambient"),pch=16,
       col=c("red","black"))
title("Heat pulse vs vpd")

plot(sap.with.met.df$trans.a ~ sap.with.met.df$VPD.mean.mean,ylim=c(0,1.4),xlim=c(0,1.5),pch=16,
     xlab = "VPD",ylab="Transpiration from MAESTRA (mm/d)")
abline(lm(sap.with.met.df$trans.a ~ sap.with.met.df$VPD.mean.mean))
par(new=TRUE)
plot(sap.with.met.df$trans.e~ sap.with.met.df$VPD.mean.mean,xlim=c(0,1.5),ylim=c(0,1.4),pch=16,col="red",
     xlab = " ",ylab=" ")
abline(lm(sap.with.met.df$trans.e ~ sap.with.met.df$VPD.mean.mean),col="red")
legend("topleft",legend = c("Elevated","Ambient"),pch=16,
       col=c("red","black"))
title("Model vs vpd")

plot(sap.with.met.df$sap.a ~ sap.with.met.df$PAR.mean.mean,ylim=c(0,1.4),xlim=c(0,10),
     xlab = "PAR",ylab="Transpiration from heat pulse (mm/d)")
abline(lm(sap.with.met.df$trans.a ~ sap.with.met.df$PAR.mean.mean))
par(new=TRUE)
plot(sap.with.met.df$sap.e~ sap.with.met.df$PAR.mean.mean,xlim=c(0,10),ylim=c(0,1.4),pch=16,col="red",
     xlab = " ",ylab=" ")
abline(lm(sap.with.met.df$trans.e ~ sap.with.met.df$PAR.mean.mean),col="red")
legend("topleft",legend = c("Elevated","Ambient"),pch=16,
       col=c("red","black"))
title("Heat pulse vs PAR")

plot(sap.with.met.df$trans.a ~ sap.with.met.df$PAR.mean.mean,ylim=c(0,1.4),xlim=c(0,10),pch=16,
     xlab = "PAR",ylab="Transpiration from MAESTRA (mm/d)")
abline(lm(sap.with.met.df$trans.a ~ sap.with.met.df$PAR.mean.mean))
par(new=TRUE)
plot(sap.with.met.df$trans.e~ sap.with.met.df$PAR.mean.mean,xlim=c(0,10),ylim=c(0,1.4),pch=16,col="red",
     xlab = " ",ylab=" ")
abline(lm(sap.with.met.df$trans.e ~ sap.with.met.df$PAR.mean.mean),col="red")
legend("topleft",legend = c("Elevated","Ambient"),pch=16,
       col=c("red","black"))
title("Model vs PAR")
dev.off()
# summary(fit.trans)
# mean(sap.df.sub$volRing.mean[which(sap.df.sub$c_treat == "a")])
# mean(sap.df.sub$volRing.mean[which(sap.df.sub$c_treat == "e")])


# make smooth sap data 
# hp data
maestra.sap.df$LAI <- maestra.sap.df$volRing
sm.df.e <- makesmoothLAI(maestra.sap.df[maestra.sap.df$c_treat == "e",],timestep= "1 days", kgam=15, how=c("mean"))
sm.df.a <- makesmoothLAI(maestra.sap.df[maestra.sap.df$c_treat == "a",],timestep= "1 days", kgam=15, how=c("mean"))

maestra.sap.sumByC$hp.sm[maestra.sap.sumByC$c_treat == "a"] <- sm.df.a$LAIsmooth
maestra.sap.sumByC$hp.sm[maestra.sap.sumByC$c_treat == "e"] <- sm.df.e$LAIsmooth

# model
maestra.sap.df$LAI <- maestra.sap.df$Trans
sm.df.e <- makesmoothLAI(maestra.sap.df[maestra.sap.df$c_treat == "e",],timestep="1 days", kgam=15, how=c("mean"))
sm.df.a <- makesmoothLAI(maestra.sap.df[maestra.sap.df$c_treat == "a",],timestep="1 days", kgam=15, how=c("mean"))
maestra.sap.sumByC$trans.sm[maestra.sap.sumByC$c_treat == "a"] <- sm.df.a$LAIsmooth
maestra.sap.sumByC$trans.sm[maestra.sap.sumByC$c_treat == "e"] <- sm.df.e$LAIsmooth

maestra.sap.df$LAI <- maestra.sap.df$lai.measured 
maestra.sap.sumByC.e <- maestra.sap.sumByC[maestra.sap.sumByC$c_treat == "e",]

maestra.sap.sumByC.a <- maestra.sap.sumByC[maestra.sap.sumByC$c_treat == "a",]

fit.1 <- gam(volRing.mean ~ s(PAR.mean) + s(VPD.mean),data = maestra.sap.sumByC.a )
summary(fit.1)

fit.2 <- gam(volRing.mean ~ s(PAR.mean) + s(VPD.mean),data = maestra.sap.sumByC.e )
summary(fit.2)

maestra.sap.sumByC$sap.gam <- NA
maestra.sap.sumByC$sap.gam[maestra.sap.sumByC$c_treat == "a"] <- 
  predict(fit.1,data = maestra.sap.sumByC.a )
maestra.sap.sumByC$sap.gam[maestra.sap.sumByC$c_treat == "e"] <- 
  predict(fit.2,data = maestra.sap.sumByC.e )

# maestra.sap.sumByC <- maestra.sap.sumByC.a[order(maestra.sap.sumByC.a$VPD.mean),]

# out.1 <- capture.output(summary(fit.1))
# out.2 <- capture.output(summary(fit.2))
# 
# 
# cat("****Summary of GAM of measured lai vs climate****", out.1, file="summary_of_GAM.txt", sep="\n", append=FALSE)
# cat("\n\n\n\n****Summary of GAM of measured lai vs climate and nutirents****", out.1.1, file="summary_of_GAM.txt", sep="\n", append=TRUE)


# plot sap and trans togeher
figure.1 <- function(){
  pdf("maestra trans vs sap.pdf",width = 10, height = 7)
  on.exit(dev.off())
  # hp vs model
  par(mar=c(6,6,4,3))
  plot(Trans~volRing,data=maestra.sap.df,
       xlim=c(0,4),ylim=c(0,4),col="coral",cex=0.6,pch=16,
       xlab=expression("Sap flow from heat pulse "~(mm~ ~day^-1)),
       ylab=expression("Sap flow from MAESTRA "~(mm~ ~day^-1)))
  abline(a=0,b=1,lty="dashed")
  title("Daily transpiration")
  
  # hp model vs date
  par(mar=c(6,6,4,3))
  plot(Trans~Date,col="coral",
       data=maestra.sap.df,
       ylim=c(0,4),pch=c(15,3,4,16,17,8)[Ring],cex=0.3,
       xlab=expression("Date"),
       ylab=expression("Sap flow"~(mm~ ~day^-1)),
       xaxt="n")
  
  axis(1,at = seq(as.Date("2012-10-01"),as.Date("2014-12-31"),"months"),
       labels = c("O","N","D",rep(c("J","F","M","A","M","J","J","A","S","O","N","D"),2)),
       cex.axis = 0.8)
  mtext("2013",side = 1,line = 2,adj = 0.13,cex = 0.8)
  mtext("2014",side = 1,line = 2,cex = 0.8)
  
  par(new=TRUE)
  plot(volRing~Date,col="darkturquoise",
       data=maestra.sap.df,
       ylim=c(0,4),pch=c(18,3,4,16,17,8)[Ring],cex=0.3,
       xlab="",ylab="",
       ann=FALSE,axes=FALSE)
  legend("top",horiz = TRUE,legend = levels(maestra.sap.df$Ring),pch=c(18,3,4,16,17,8),col = "black",cex=0.8,bty='n')
  legend("topright",bty='n',legend=c("Measured","Modelled"),col=c("darkturquoise","coral"),pch=15)
  mtext("Filled symbos mark elevated rings",side = 3,line = 0,cex = 0.7)
  
  title("Daily transpiration over time")
  
#   # trans vs vpd
#   par(mar=c(6,6,4,3))
#   palette(c("red","blue"))
#   plot(volRing~VPD,data=maestra.sap.df,
#        xlim=c(0,3),ylim=c(0,3),col=c_treat,
#        cex=0.6,pch=16,
#        xlab=expression("VPD "~ ~(kPa)),
#        ylab=expression("Transpiration "~(mm~ ~day^-1)))
#   par(new=TRUE)
#   plot(sap.gam~VPD,data=maestra.sap.df[maestra.sap.df$c_treat == "e",],lty="solid",col="red",
#        xlim=c(0,3),ylim=c(0,3),xlab="",ylab="",ann=FALSE,axes=FALSE)
#   par(new=TRUE)
#   plot(sap.gam~VPD,data=maestra.sap.df[maestra.sap.df$c_treat == "a",],lty="solid",col="blue",
#        xlim=c(0,3),ylim=c(0,3),xlab="",ylab="",ann=FALSE,axes=FALSE)
#   
#   # abline(lm(volRing~VPD,data=maestra.sap.df[maestra.sap.df$c_treat == "e",]),lty="solid",col="red")
#   # abline(lm(volRing~VPD,data=maestra.sap.df[maestra.sap.df$c_treat == "a",]),lty="solid",col="blue")
#   title("Transpiration vs VPD")
#   
#   par(new=TRUE)
#   plot(Trans~VPD,data=maestra.sap.df,
#        xlim=c(0,3),ylim=c(0,3),col=c_treat,
#        cex=0.6,pch=3,
#        xlab="",
#        ylab="",ann=FALSE,axes=FALSE)
#   abline(lm(Trans~VPD,data=maestra.sap.df[maestra.sap.df$c_treat == "e",]),lty="dashed",col="red")
#   abline(lm(Trans~VPD,data=maestra.sap.df[maestra.sap.df$c_treat == "a",]),lty="dashed",col="blue")
#   legend("topleft",legend = c("Modelled","Heat Pulse"),pch=c(3,16),col="black",
#          bty='n',cex=0.8)
#   legend(-0.15,2.75,legend = c("Elevated","Ambient"),pch=15,col=c("red","blue"),
#          bty='n',cex=0.8)
#   legend(-0.15,2.35,legend = c("Modelled","Heat Pulse"),lty = c("dashed","solid"),col="black",
#          bty='n',cex=0.8)
#   
#   # trans vs PAR
#   par(mar=c(6,6,4,3))
#   palette(c("red","blue"))
#   plot(volRing~PAR,data=maestra.sap.df,
#        xlim=c(0,14),ylim=c(0,3),col=c_treat,
#        cex=0.6,pch=16,
#        xlab=expression("PAR "~ ~(kPa)),
#        ylab=expression("Transpiration "~(mm~ ~day^-1)))
#   abline(lm(volRing~PAR,data=maestra.sap.df[maestra.sap.df$c_treat == "e",]),lty="solid",col="red")
#   abline(lm(volRing~PAR,data=maestra.sap.df[maestra.sap.df$c_treat == "a",]),lty="solid",col="blue")
#   title("Transpiration vs PAR")
#   
#   par(new=TRUE)
#   plot(Trans~PAR,data=maestra.sap.df,
#        xlim=c(0,14),ylim=c(0,3),col=c_treat,
#        cex=0.6,pch=3,
#        xlab="",
#        ylab="",ann=FALSE,axes=FALSE)
#   abline(lm(Trans~PAR,data=maestra.sap.df[maestra.sap.df$c_treat == "e",]),lty="dashed",col="red")
#   abline(lm(Trans~PAR,data=maestra.sap.df[maestra.sap.df$c_treat == "a",]),lty="dashed",col="blue")
#   legend("topleft",legend = c("Modelled","Heat Pulse"),pch=c(3,16),col="black",
#          bty='n',cex=0.8)
#   legend(-0.62,2.75,legend = c("Elevated","Ambient"),pch=15,col=c("red","blue"),
#          bty='n',cex=0.8)
#   legend(-0.62,2.35,legend = c("Modelled","Heat Pulse"),lty = c("dashed","solid"),col="black",
#          bty='n',cex=0.8)
  
  # summerised trans vs vpd
  # trans vs vpd
  par(mar=c(6,6,4,3))
  palette(c("red","blue"))
  plot(volRing.mean~VPD.mean,data=maestra.sap.sumByC,
       xlim=c(0,3),ylim=c(0,2.5),col=c_treat,
       cex=0.6,pch=16,
       xlab=expression("VPD "~ ~(kPa)),
       ylab=expression("Transpiration "~(mm~ ~day^-1)))
  # abline(gam(volRing.mean~s(VPD.mean),data=maestra.sap.sumByC.e))
  par(new=TRUE)
  vpds <- maestra.sap.sumByC.e[order(maestra.sap.sumByC.e$VPD.mean),]
  
  plot(predict(gam(volRing.mean~s(VPD.mean),data=vpds),data=vpds)~vpds$VPD.mean,
       type = "l",lwd = 2,col="red",
       xlim=c(0,3),ylim=c(0,3),xlab="",ylab="",ann=FALSE,axes=FALSE)
  par(new=TRUE)
  vpds <- maestra.sap.sumByC.a[order(maestra.sap.sumByC.a$VPD.mean),]
  
  plot(predict(gam(volRing.mean~s(VPD.mean),data=vpds),data=vpds)~vpds$VPD.mean,
       type = "l",lwd = 2,col="blue",
       xlim=c(0,3),ylim=c(0,3),xlab="",ylab="",ann=FALSE,axes=FALSE)
  
  par(new=TRUE)
  plot(Trans.mean~VPD.mean,data=maestra.sap.sumByC,
       xlim=c(0,3),ylim=c(0,3),col=c_treat,
       cex=0.6,pch=3,
       xlab="",
       ylab="",ann=FALSE,axes=FALSE)
  
  par(new=TRUE)
  vpds <- maestra.sap.sumByC.e[order(maestra.sap.sumByC.e$VPD.mean),]
  
  plot(predict(gam(Trans.mean~s(VPD.mean),data=vpds),data=vpds)~vpds$VPD.mean,
       type = "l",lwd = 2,col="red",lty="dashed",
       xlim=c(0,3),ylim=c(0,3),xlab="",ylab="",ann=FALSE,axes=FALSE)
  par(new=TRUE)
  vpds <- maestra.sap.sumByC.a[order(maestra.sap.sumByC.a$VPD.mean),]
  
  plot(predict(gam(Trans.mean~s(VPD.mean),data=vpds),data=vpds)~vpds$VPD.mean,
       type = "l",lwd = 2,col="blue",lty="dashed",
       xlim=c(0,3),ylim=c(0,3),xlab="",ylab="",ann=FALSE,axes=FALSE)
  
  legend("topleft",legend = c("Modelled","Heat Pulse"),pch=c(3,16),col="black",
         bty='n',cex=0.8)
  legend(-0.13,2.75,legend = c("Elevated","Ambient"),pch=15,col=c("red","blue"),
         bty='n',cex=0.8)
  legend("topright",legend = c("Modelled","Heat Pulse"),lty = c("dashed","solid"),col="black",
         bty='n',cex=0.8,title="GAM")
  title("Transpiration vs VPD")
  
  # trans vs swc
  palette(c("red","blue"))
  plot(volRing.mean~VWC.mean,data=maestra.sap.sumByC,
       xlim=c(0,0.4),ylim=c(0,2.5),col=c_treat,
       cex=0.6,pch=16,
       xlab="SWC %",
       ylab=expression("Transpiration "~(mm~ ~day^-1)))
  par(new=TRUE)
  plot(Trans.mean~VWC.mean,data=maestra.sap.sumByC,
       xlim=c(0,0.4),ylim=c(0,3),col=c_treat,
       cex=0.6,pch=3,
       xlab="",
       ylab="",ann=FALSE,axes=FALSE)
  legend("topright",legend = c("Modelled","Heat Pulse"),pch=c(3,16),col="black",
         bty='n',cex=0.8)
  legend("top",legend = c("Elevated","Ambient"),pch=15,col=c("red","blue"),
         bty='n',cex=0.8,horiz = TRUE)
  title("Transpiration vs Soil water content")
  
  # trans vs swc
  palette(c("red","blue"))
  plot(c(Trans.mean-volRing.mean)~VWC.mean,data=maestra.sap.sumByC,
       xlim=c(0,0.4),yim=c(-1,2),col=c_treat,
       cex=0.6,pch=16,
       xlab="SWC %",
       ylab=expression("Difference "~(mm~ ~day^-1)))

  abline(h=0,lty="dashed")

  legend("top",legend = c("Elevated","Ambient"),pch=15,col=c("red","blue"),
         bty='n',cex=0.8,horiz = TRUE)
  title("Modelled - heat pulse transpiration vs Soil water content")
  # trans vs PAR
  par(mar=c(6,6,4,3))
  palette(c("red","blue"))
  plot(volRing.mean~PAR.mean,data=maestra.sap.sumByC,
       xlim=c(0,15),ylim=c(0,2.5),col=c_treat,
       cex=0.6,pch=16,
       xlab=expression("PAR "~ ~(kPa)),
       ylab=expression("Transpiration "~(mm~ ~day^-1)))
  # abline(gam(volRing.mean~s(PAR.mean),data=maestra.sap.sumByC.e))
  par(new=TRUE)
  PARs <- maestra.sap.sumByC.e[order(maestra.sap.sumByC.e$PAR.mean),]
  
  plot(predict(gam(volRing.mean~s(PAR.mean),data=PARs),data=PARs)~PARs$PAR.mean,
       type = "l",lwd = 2,col="red",
       xlim=c(0,15),ylim=c(0,3),xlab="",ylab="",ann=FALSE,axes=FALSE)
  par(new=TRUE)
  PARs <- maestra.sap.sumByC.a[order(maestra.sap.sumByC.a$PAR.mean),]
  
  plot(predict(gam(volRing.mean~s(PAR.mean),data=PARs),data=PARs)~PARs$PAR.mean,
       type = "l",lwd = 2,col="blue",
       xlim=c(0,15),ylim=c(0,3),xlab="",ylab="",ann=FALSE,axes=FALSE)
  
  par(new=TRUE)
  plot(Trans.mean~PAR.mean,data=maestra.sap.sumByC,
       xlim=c(0,15),ylim=c(0,3),col=c_treat,
       cex=0.6,pch=3,
       xlab="",
       ylab="",ann=FALSE,axes=FALSE)
  
  par(new=TRUE)
  PARs <- maestra.sap.sumByC.e[order(maestra.sap.sumByC.e$PAR.mean),]
  
  plot(predict(gam(Trans.mean~s(PAR.mean),data=PARs),data=PARs)~PARs$PAR.mean,
       type = "l",lwd = 2,col="red",lty="dashed",
       xlim=c(0,15),ylim=c(0,3),xlab="",ylab="",ann=FALSE,axes=FALSE)
  par(new=TRUE)
  PARs <- maestra.sap.sumByC.a[order(maestra.sap.sumByC.a$PAR.mean),]
  
  plot(predict(gam(Trans.mean~s(PAR.mean),data=PARs),data=PARs)~PARs$PAR.mean,
       type = "l",lwd = 2,col="blue",lty="dashed",
       xlim=c(0,15),ylim=c(0,3),xlab="",ylab="",ann=FALSE,axes=FALSE)
  
  legend("topleft",legend = c("Modelled","Heat Pulse"),pch=c(3,16),col="black",
         bty='n',cex=0.8)
  legend(-0.6,2.75,legend = c("Elevated","Ambient"),pch=15,col=c("red","blue"),
         bty='n',cex=0.8)
  legend("top",legend = c("Modelled","Heat Pulse"),lty = c("dashed","solid"),col="black",
         bty='n',cex=0.8,title="GAM")
  title("Transpiration vs PAR")
  
  
  # trans vs date by treatment
  par(mar=c(6,6,4,3))

  plot(trans.sm~Date,col="red",
       data=maestra.sap.sumByC.e ,
       ylim=c(0,2),type = "l",lty="dashed",
       xlab=expression("Date"),
       ylab=expression("Transpiration"~(mm~ ~day^-1)),
       xaxt="n")
  par(new=TRUE)
  plot(trans.sm~Date,col="blue",
       data=maestra.sap.sumByC.a ,
       ylim=c(0,2),type = "l",lty="dashed",
       xlab="",ylab="",
       ann=FALSE,axes=FALSE)
  
  axis(1,at = seq(as.Date("2012-10-01"),as.Date("2014-12-31"),"months"),
       labels = c("O","N","D",rep(c("J","F","M","A","M","J","J","A","S","O","N","D"),2)),
       cex.axis = 0.8)
  mtext("2013",side = 1,line = 2,adj = 0.13,cex = 0.8)
  mtext("2014",side = 1,line = 2,cex = 0.8)
  
  par(new=TRUE)
  
  plot(hp.sm~Date,col="blue",
       data=maestra.sap.sumByC.a ,
       ylim=c(0,2),type = "l",lty="solid",
       xlab="",ylab="",
       ann=FALSE,axes=FALSE)
  
  par(new=TRUE)
  
  plot(hp.sm~Date,col="red",
       data=maestra.sap.sumByC.e ,
       ylim=c(0,2),type = "l",lty="solid",
       xlab="",ylab="",
       ann=FALSE,axes=FALSE)
  
  par(new=TRUE)
  
  plot(LAI~Date,col="red",
       data=maestra.sap.df[which(maestra.sap.df$c_treat == "e"),] ,
       ylim=c(1,6),type = "l",lty="dotted",
       xlab="",ylab="",
       ann=FALSE,axes=FALSE)
  par(new=TRUE)
  plot(LAI~Date,col="blue",
       data=maestra.sap.df[which(maestra.sap.df$c_treat == "a"),] ,
       ylim=c(1,6),type = "l",lty="dotted",
       xlab="",ylab="",
       ann=FALSE,axes=FALSE)
  legend("top",horiz = TRUE,legend = c("Elevated","Ambient") ,pch=15,col = c("red","blue"),cex=0.8,bty='n')
  legend("topright",bty='n',legend=c("Heat pulse","Modelled","LAI"),col="black",lty = c("solid","dashed","dotted"),cex=0.8)
  axis(4,at=seq(1,2,0.5),labels = paste0(seq(1,2,0.5)))
  mtext("LAI",side=4,line=2,adj = 0.2)
  title("Smoothed daily transpiration over time of each treatment")
  
}

figure.1()

figure.2 <- function(){
  pdf("monthly maestra trans vs lai.pdf",width = 10, height = 7)
  on.exit(dev.off())
  par(mar=c(5,5,5,5),mfrow=c(1,1))
  palette(c("blue","red"))
  plot(maestra.sap.sumByC.mon$Trans.mean.mean ~ maestra.sap.sumByC.mon$LAI.mean.mean,
       xlim=c(1,2.5),ylim=c(0,3),
       xlab="LAI",ylab=expression("Transpiration"~ ~"("~mm~H[2]~O~m^-2~hr^-1~")"),
       col=maestra.sap.sumByC.mon$c_treat,pch=3)
  par(new =TRUE)
  plot(maestra.sap.sumByC.mon$volRing.mean.mean ~ maestra.sap.sumByC.mon$LAI.mean.mean,
       xlim=c(1,2.5),ylim=c(0,3),
       xlab=" ",ylab=" ",ann=FALSE,axes=FALSE,
       col=maestra.sap.sumByC.mon$c_treat,pch=16)
  legend("topleft",legend=c("Modelled","Heat pulse"),pch=c(3,16),col="black",
         bty='n',cex=0.6)
  legend("top",legend=c("Elevated","Ambient"),col=c("red","blue"),
         pch=15,bty='n',cex=0.6,horiz = TRUE)
  par(new =TRUE)
  temp.order <- maestra.sap.sumByC.mon[order(maestra.sap.sumByC.mon$LAI.mean.mean),]
  plot(temp.order$VPD.mean.mean ~ temp.order$LAI.mean.mean,
       xlim=c(1,2.5),ylim=c(0,8),
       xlab=" ",ylab=" ",ann=FALSE,axes=FALSE,
       col="coral",type = "l",lwd=1)
  axis(4,at = seq(0,1.5,0.5),labels = paste0(seq(0,1.5,0.5)),
       col = "coral",col.axis= "coral")
  mtext("VPD (kPa)",side = 4,line = 2, col = "coral",adj = 0.05)
  
  par(new =TRUE)

  plot(temp.order$PAR.mean.mean ~ temp.order$LAI.mean.mean,
       xlim=c(1,2.5),ylim=c(-12,12),
       xlab=" ",ylab=" ",ann=FALSE,axes=FALSE,
       col="gold",type = "l",lwd=1)
  axis(4,at = seq(4,12,4),labels = paste0(seq(4,12,4)),
       col = "gold",col.axis= "gold")
  mtext(expression(PAR~ ~(mu~mol~m^-2~s^-1)),side = 4,line = 2, col = "gold",adj = 0.95)
  title("Monthly ET vs LAI of each ring coloured by treatment")
}

figure.2()


# plotCIdate <- function(g, df, add=FALSE, linecols=c("black","red","forestgreen"), ...){  
#   
#   dates <- seq(min(df$Date), max(df$Date), by="1 day")
#   
#   fd <- derivSimulCI(g, samples = 10000, n=length(dates))
#   
#   dydt <- fd[[1]]$deriv[,1]
#   CI <- apply(fd[[1]]$simulations, 1, quantile,probs = c(0.025, 0.975))
#   
#   if(!add){
#     plot(dates, dydt, type='l', col=linecols[1], ... ,
#          panel.first=addpoly(x=dates, y1=CI[2,], y2=CI[1,]))
#   } else {
#     addpoly(x=dates, y1=CI[2,], y2=CI[1,])
#     lines(dates, dydt, col=linecols[1])
#   }
#   abline(h=0)
#   
#   x <- signifD(dydt,dydt,CI[2,],CI[1,],0)
#   lines(dates, x[["incr"]], col=linecols[3], lwd=2)
#   lines(dates, x[["decr"]], col=linecols[2], lwd=2)
# }
# 
# maestra.sap.sumByC$Time <- as.numeric(maestra.sap.sumByC$Date - min(maestra.sap.sumByC$Date))
# g <- gamm(hp.sm ~ s(Time, k=15), random = list(c_treat=~1), data=maestra.sap.sumByC)
# 
# plotCIdate(g, maestra.sap.sumByC, axes=FALSE,xlab="",xlim=xl,
#            ylim=c(-0.016, 0.016),
#            ylab=expression(d*italic(L)/dt~(m^2~m^-2~d^-1)))
# abline(h=0)
# 
# `derivSimulCI` <- function(mod, n = 200, eps = 1e-7, newdata, term,
#                            samples = 10000) {
#   stopifnot(require("MASS"))
#   if(inherits(mod, "gamm"))
#     mod <- mod$gam
#   m.terms <- attr(terms(mod), "term.labels")
#   if(missing(newdata)) {
#     newD <- sapply(model.frame(mod)[, m.terms, drop = FALSE],
#                    function(x) seq(min(x), max(x) - (2*eps), length = n))
#     names(newD) <- m.terms
#   } else {
#     newD <- newdata
#   }
#   newDF <- data.frame(newD) ## needs to be a data frame for predict
#   X0 <- predict(mod, newDF, type = "lpmatrix")
#   newDF <- newDF + eps
#   X1 <- predict(mod, newDF, type = "lpmatrix")
#   Xp <- (X1 - X0) / eps
#   Xp.r <- NROW(Xp)
#   Xp.c <- NCOL(Xp)
#   ## dims of bs
#   bs.dims <- sapply(mod$smooth, "[[", "bs.dim") - 1
#   ## number of smooth terms
#   t.labs <- attr(mod$terms, "term.labels")
#   ## match the term with the the terms in the model
#   if(!missing(term)) {
#     want <- grep(term, t.labs)
#     if(!identical(length(want), length(term)))
#       stop("One or more 'term's not found in model!")
#     t.labs <- t.labs[want]
#   }
#   nt <- length(t.labs)
#   ## list to hold the derivatives
#   lD <- vector(mode = "list", length = nt)
#   names(lD) <- t.labs
#   ## sample draws from the posterior distribution of model coefficients
#   Rbeta <- t(mvrnorm(n = samples, coef(mod), vcov(mod)))
#   ## loop over the terms
#   for(i in seq_len(nt)) {
#     want <- grep(t.labs[i], colnames(X1))
#     lD[[i]] <- list(deriv = Xp[, want] %*% coef(mod)[want],
#                     simulations = Xp[, want] %*% Rbeta[want, ])
#   }
#   class(lD) <- "derivSimulCI"
#   lD$gamModel <- mod
#   lD$eps <- eps
#   lD$eval <- newD - eps
#   lD ##return
# }
# 


#   # summerised trans vs vpd
#   # trans vs vpd
#   par(mar=c(6,6,4,3))
#   palette(c("red","blue"))
#   plot(volRing.mean~VPD.mean,data=maestra.sap.sumByC,
#        xlim=c(0,3),ylim=c(0,2.5),col=c_treat,
#        cex=0.6,pch=16,
#        xlab=expression("VPD "~ ~(kPa)),
#        ylab=expression("Transpiration "~(mm~ ~day^-1)))
#   # abline(gam(volRing.mean~s(VPD.mean),data=maestra.sap.sumByC.e))
#   par(new=TRUE)
#   vpds <- maestra.sap.sumByC.e[order(maestra.sap.sumByC.e$VPD.mean),]
#   
#   plot(predict(gam(volRing.mean~s(VPD.mean),data=vpds),data=vpds)~vpds$VPD.mean,
#        type = "l",lwd = 2,col="red",
#        xlim=c(0,3),ylim=c(0,3),xlab="",ylab="",ann=FALSE,axes=FALSE)
#   par(new=TRUE)
#   vpds <- maestra.sap.sumByC.a[order(maestra.sap.sumByC.a$VPD.mean),]
#   
#   plot(predict(gam(volRing.mean~s(VPD.mean),data=vpds),data=vpds)~vpds$VPD.mean,
#        type = "l",lwd = 2,col="blue",
#        xlim=c(0,3),ylim=c(0,3),xlab="",ylab="",ann=FALSE,axes=FALSE)
#   
#     par(new=TRUE)
#     plot(Trans.mean~VPD.mean,data=maestra.sap.sumByC,
#          xlim=c(0,3),ylim=c(0,3),col=c_treat,
#          cex=0.6,pch=3,
#          xlab="",
#          ylab="",ann=FALSE,axes=FALSE)
# 
#     par(new=TRUE)
#     vpds <- maestra.sap.sumByC.e[order(maestra.sap.sumByC.e$VPD.mean),]
#     
#     plot(predict(gam(Trans.mean~s(VPD.mean),data=vpds),data=vpds)~vpds$VPD.mean,
#          type = "l",lwd = 2,col="red",lty="dashed",
#          xlim=c(0,3),ylim=c(0,3),xlab="",ylab="",ann=FALSE,axes=FALSE)
#     par(new=TRUE)
#     vpds <- maestra.sap.sumByC.a[order(maestra.sap.sumByC.a$VPD.mean),]
#     
#     plot(predict(gam(Trans.mean~s(VPD.mean),data=vpds),data=vpds)~vpds$VPD.mean,
#          type = "l",lwd = 2,col="blue",lty="dashed",
#          xlim=c(0,3),ylim=c(0,3),xlab="",ylab="",ann=FALSE,axes=FALSE)
#     
#     legend("topleft",legend = c("Modelled","Heat Pulse"),pch=c(3,16),col="black",
#            bty='n',cex=0.8)
#     legend(-0.13,2.75,legend = c("Elevated","Ambient"),pch=15,col=c("red","blue"),
#            bty='n',cex=0.8)
#     legend("topright",legend = c("Modelled","Heat Pulse"),lty = c("dashed","solid"),col="black",
#            bty='n',cex=0.8,title="GAM")
#     title("Transpiration vs VPD")
#     
#     