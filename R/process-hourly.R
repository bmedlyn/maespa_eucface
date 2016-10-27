# the trunck hieght problems still there is ring 6
source("R/load.R")
#test - lai sensitivity test
test <- 1
# base-base lai
base <- 0.8

# make sure you want to do this first
hourly.data <- TRUE
# eucGPP(lai.test = test,lai.base = base)

#analysis###################
# read inputs and outputs
met <- list()
input <- list()
flux <- list()
hr.flux <- list()
for (i in 1:6){
  
  met[[i]] <- Readmet(i) 
  input[[i]] <- ReadInput(i)
  flux[[i]] <- ReadDayFlux(i)
  hr.flux[[i]] <- ReadHourFlux(i)
  
}

col.nm <- c("DOY","Tree","Spec","HOUR","hrPAR",
            "hrNIR","hrTHM","hrPs","hrRf","hrRmW",
            "hrLE","LECAN","Gscan","Gbhcan","hrH",
            "TCAN","ALMAX","PSIL","PSILMIN","CI",
            "TAIR","VPD","PAR","ZEN","AZ")

for (i in 1:6){
  names(hr.flux[[i]]) <- col.nm
}

ring.hr.avg.df <- get.hr.Rings(hr.flux)
# names(ring.hr.avg.df)[is.na(names(ring.hr.avg.df) == TRUE)] <- "TAIR"
ring.hr.avg.df$GPP.r <- 12 * ring.hr.avg.df$GPP / (pi*12.5^2) * 10^-6 #g C m-2 ground s-1
ring.hr.avg.df$PAR.r <- ring.hr.avg.df$PAR / (pi * 12.5^2)
ring.hr.avg.df$ET.r <- ring.hr.avg.df$ET / (pi*12.5^2) * 1.8 * 0.01 * 10^-3 #mm h2o s-1

ring.hr.avg.df$time <- floor((ring.hr.avg.df$HOUR-1) / 2)

ring.hr.avg.df.sub <- summaryBy(GPP.r + PAR.r + ET.r + VPD + LAI + TAIR~ 
                                  DOY + time + Date + Ring,
                                data = ring.hr.avg.df,FUN = mean,na.rm = TRUE)
names(ring.hr.avg.df.sub) <- c("DOY","time","Date","Ring","GPP","PAR", "ET", "VPD","LAI","TAIR")

ring.hr.avg.df.sub <- ring.hr.avg.df.sub[,c("DOY","Date","time","Ring","GPP","PAR", "ET", "VPD","LAI","TAIR")]

sap.hr <- readRDS("sap_hrly.rds")
# get hour and date
sap.hr.sub <- subset(sap.hr,select = c("DateHour","Ring","volRing"))
sap.hr.sub$Date <- as.Date(sap.hr.sub$DateHour)
sap.hr.sub$time <- as.numeric(str_sub(as.character(sap.hr.sub$DateHour),12,13))
# get ring number
sap.hr.sub$Ring <- as.character(sap.hr.sub$Ring)
str_sub(sap.hr.sub$Ring,0,1) <- ""
sap.hr.sub$Ring <- as.numeric(sap.hr.sub$Ring)

data.both.sap.hr <- merge(ring.hr.avg.df.sub,sap.hr.sub,
                          by = intersect(names(ring.hr.avg.df.sub), names(sap.hr.sub)))

data.both.sap.hr$GPP.h <- data.both.sap.hr$GPP * 3600 ##g C m-2 ground hr-1
data.both.sap.hr$ET.h <- data.both.sap.hr$ET * 3600 ##mm h2o m-2 ground hr-1
data.both.sap.hr$c_treat <- "A"
data.both.sap.hr$c_treat[data.both.sap.hr$Ring == 1 | 
                           data.both.sap.hr$Ring == 4 | 
                           data.both.sap.hr$Ring == 5] <- "E"

data.both.sap.hr$c_treat <- as.factor(data.both.sap.hr$c_treat)
data.both.sap.hr$day.time <- "night"
data.both.sap.hr$day.time[data.both.sap.hr$time > 8 & data.both.sap.hr$time < 13] <- "mor"
data.both.sap.hr$day.time[data.both.sap.hr$time >= 13 & data.both.sap.hr$time <= 16] <- "aft"
saveRDS(data.both.sap.hr,"hr flux with met.rds")
# # get swc
# swc.df <- read.csv("download/FACE_RA_P0037_DAILYMET_20110619-20151026_L2.csv")
# swc.df$Date <- as.Date(swc.df$Date)
# 
# soilwater <- swc.df[,c("Date","VWC")]
# soilwater <- na.omit(soilwater)
# 
# see <- merge(data.both.sap.hr,soilwater,by="Date")
# plot sap vs vpd

figure.sap.1 <- function(){

  pdf("Hourly data plot.pdf",width = 10, height = 7)
  on.exit(dev.off())
  
  par(mar=c(5,5,5,5))
  plot(data.both.sap.hr$ET.h ~ data.both.sap.hr$VPD,
       xlim=c(0,9),ylim=c(0,0.5),
       xlab="VPD (kPa)",ylab=expression("Sap flow"~ ~"("~mm~H[2]~O~m^-2~hr^-1~")"),
       cex=0.5,col="coral",pch=16)
  
  # vpd.order <- data.both.sap.hr[order(data.both.sap.hr$VPD),]
  # fit.1 <- gam(ET.h~s(VPD),data = vpd.order)
  # et.pred.1 <- predict(fit.1,data = vpd.order)
  # par(new=TRUE)
  # plot(et.pred.1 ~ vpd.order$VPD,
  #      xlim=c(0,9),ylim=c(0,0.5),
  #      xlab=" ",ylab="",
  #      type = "l",col="coral",lwd = 1.2)
  
  par(new=TRUE)
  plot(data.both.sap.hr$volRing ~ data.both.sap.hr$VPD,
       xlim=c(0,9),ylim=c(0,0.5),
       xlab=" ",ylab="",
       cex=0.5,col="navy",pch=16)
  legend("topleft",bty='n',legend = c("MAESPA","Heat pulse"),pch=16,
         col=c("coral","navy"),cex=0.5)
  title("Modelled and heat pulse transpiration vs VPD")
  
  # plot model vs hp
  plot(data.both.sap.hr$ET.h ~ data.both.sap.hr$volRing,
       xlim=c(0,0.5),ylim=c(0,0.5),
       xlab="Heat pulse",ylab=expression("Modelled"~ ~"("~mm~H[2]~O~m^-2~hr^-1~")"),
       cex=0.5,col="coral",pch=16)
  abline(a=0,b=1,col="black",lty="dashed")
  legend("top",legend=expression("Sap flow unit:"~mm~H[2]~O~m^-2~hr^-1),
         bty='n',horiz = TRUE,cex=0.5)
  title("Modelled vs heat pulse")
  
  # plot diff (residual)
  plot(data.both.sap.hr$ET.h - data.both.sap.hr$volRing ~ 
         data.both.sap.hr$VPD,
       xlim=c(0,9),
       xlab="VPD (kPa)",ylab=expression("Difference in sap flow"~ ~"("~mm~H[2]~O~m^-2~hr^-1~")"),
       cex=0.5,col="deepskyblue",pch=16)
  abline(h=0,col="black",lty="dashed")
  title("Modelled sap flow minus heat pulse data")
  
  plot(data.both.sap.hr$ET.h - data.both.sap.hr$volRing ~ 
         data.both.sap.hr$TAIR,
       xlab="TAIR",ylab=expression("Difference in sap flow"~ ~"("~mm~H[2]~O~m^-2~hr^-1~")"),
       cex=0.5,col="deepskyblue",pch=16)
  abline(h=0,col="black",lty="dashed")
  title("Modelled sap flow minus heat pulse data")
  
  plot(data.both.sap.hr$ET.h - data.both.sap.hr$volRing ~ 
         data.both.sap.hr$PAR,
       xlab="Date",ylab=expression("Difference in sap flow"~ ~"("~mm~H[2]~O~m^-2~hr^-1~")"),
       cex=0.5,col="deepskyblue",pch=16)
  abline(h=0,col="black",lty="dashed")
  title("Modelled sap flow minus heat pulse data")

  boxplot(data.both.sap.hr$ET.h - data.both.sap.hr$volRing ~ 
         data.both.sap.hr$day.time,ylim=c(-0.1,0.15),
         ylab=expression("Difference in sap flow"~ ~"("~mm~H[2]~O~m^-2~hr^-1~")"))
  abline(h=0,col="black",lty="dashed")
  title("Modelled sap flow minus heat pulse data")
  
  
  # barplot(data.both.sap.hr$ET.h - data.both.sap.hr$volRing)
  # palette(c("red","blue"))
  # plot(data.both.sap.hr$ET.h[data.both.sap.hr$time == 12] ~ 
  #        data.both.sap.hr$LAI[data.both.sap.hr$time == 12],
  #      xlim=c(1,2),ylim=c(0,0.5),
  #      xlab="LAI",ylab=expression("Sap flow"~ ~"("~mm~H[2]~O~m^-2~hr^-1~")"),
  #      cex=0.5,col=data.both.sap.hr$c_treat[data.both.sap.hr$time == 12],pch=16)
  
  # see co2 effect
  palette(c("blue","red"))
  plot(data.both.sap.hr$ET.h ~ data.both.sap.hr$VPD,
       xlim=c(0,9),ylim=c(0,0.5),
       xlab="VPD (kPa)",ylab=expression("Sap flow"~ ~"("~mm~H[2]~O~m^-2~hr^-1~")"),
       cex=0.5,col=data.both.sap.hr$c_treat,pch=16)
  legend("top",legend=c("Elevated","Ambient"),col=c("red","blue"),
         pch=15,bty='n',cex=0.6,horiz = TRUE)
  title("Modelled ET VS VPD coloured by treatment")
  
  plot(data.both.sap.hr$volRing ~ data.both.sap.hr$VPD,
       xlim=c(0,9),ylim=c(0,0.5),
       xlab="VPD (kPa)",
       ylab=expression("Sap flow"~ ~"("~mm~H[2]~O~m^-2~hr^-1~")"),
       cex=0.5,col=data.both.sap.hr$c_treat,pch=16)
  legend("top",legend=c("Elevated","Ambient"),col=c("red","blue"),
         pch=15,bty='n',cex=0.6,horiz = TRUE)
  title("Heat pulse ET VS VPD coloured by treatment")
  
  vpd.order <- data.both.sap.hr[order(data.both.sap.hr$VPD),]
  
  fit <- list()
  et.pred <- list()
  
  for (i in 1:6){
    fit[[i]] <- gam(ET.h~s(VPD),data = vpd.order[vpd.order$Ring == i,])
    et.pred[[i]] <- predict(fit[[i]],data = vpd.order[vpd.order$Ring == i,])
  }
  
  fit.hp <- list()
  et.pred.hp <- list()
  
  for (i in 1:6){
    fit.hp[[i]] <- gam(volRing~s(VPD),data = vpd.order[vpd.order$Ring == i,])
    et.pred.hp[[i]] <- predict(fit.hp[[i]],data = vpd.order[vpd.order$Ring == i,])
  }
  
  par(mar=c(5,5,5,5),mfrow=c(1,1))
  palette(c("blue","red"))
  
  plot(0,bty='n',pch='',
       xlim=c(0,9),ylim=c(0,0.4),
       xlab="VPD (kPa)",ylab=expression("Sap flow"~ ~"("~mm~H[2]~O~m^-2~hr^-1~")"))
  
  for (i in 1:6){
    par(new=TRUE)
    plot(et.pred[[i]] ~ vpd.order$VPD[vpd.order$Ring == i],
         xlim=c(0,9),ylim=c(0,0.4),
         xlab=" ",ylab="",ann=FALSE,axes=FALSE,
         type = "l",col=vpd.order$c_treat[vpd.order$Ring == i],lwd = 1.2)
  }
  
  for (i in 1:6){
    par(new=TRUE)
    plot(et.pred.hp[[i]] ~ vpd.order$VPD[vpd.order$Ring == i],
         xlim=c(0,9),ylim=c(0,0.4),
         xlab="",ylab="",ann=FALSE,axes=FALSE,
         type = "l",col=vpd.order$c_treat[vpd.order$Ring == i],
         lwd = 1.2,lty = "dashed")
  }
  
  legend("topleft",legend=c("Modelled","Heat pulse"),lty=c("solid","dashed"),
         bty='n',cex=0.6)
  legend("top",legend=c("Elevated","Ambient"),col=c("red","blue"),
         pch=15,bty='n',cex=0.6,horiz = TRUE)
  title("Transpiration of each ring vs VPD")
  
  
  
  # et over lai
  vpd.order$et.lai.model <- vpd.order$ET.h / vpd.order$LAI
  vpd.order$et.lai.hp <- vpd.order$volRing / vpd.order$LAI
  fit.lai <- list()
  et.lai.pred <- list()
  
  for (i in 1:6){
    fit.lai[[i]] <- gam(et.lai.model~s(VPD),data = vpd.order[vpd.order$Ring == i,])
    et.lai.pred[[i]] <- predict(fit.lai[[i]],data = vpd.order[vpd.order$Ring == i,])
  }
  
  fit.lai.hp <- list()
  et.lai.pred.hp <- list()
  
  for (i in 1:6){
    fit.lai.hp[[i]] <- gam(et.lai.hp~s(VPD),data = vpd.order[vpd.order$Ring == i,])
    et.lai.pred.hp[[i]] <- predict(fit.lai.hp[[i]],data = vpd.order[vpd.order$Ring == i,])
  }
  
  par(mar=c(5,5,5,5),mfrow=c(1,1))
  palette(c("blue","red"))
  
  plot(0,bty='n',pch='',
       xlim=c(0,9),ylim=c(0,0.2),
       xlab="VPD (kPa)",ylab=expression("Sap flow over LAI"~ ~"("~mm~H[2]~O~m^-2~leaf~hr^-1~")"))
  
  for (i in 1:6){
    par(new=TRUE)
    plot(et.lai.pred[[i]] ~ vpd.order$VPD[vpd.order$Ring == i],
         xlim=c(0,9),ylim=c(0,0.2),
         xlab=" ",ylab="",ann=FALSE,axes=FALSE,
         type = "l",col=vpd.order$c_treat[vpd.order$Ring == i],lwd = 1.2)
  }
  
  for (i in 1:6){
    par(new=TRUE)
    plot(et.lai.pred.hp[[i]] ~ vpd.order$VPD[vpd.order$Ring == i],
         xlim=c(0,9),ylim=c(0,0.2),
         xlab="",ylab="",ann=FALSE,axes=FALSE,
         type = "l",col=vpd.order$c_treat[vpd.order$Ring == i],
         lwd = 1.2,lty = "dashed")
  }
  
  legend("topleft",legend=c("Modelled","Heat pulse"),lty=c("solid","dashed"),
         bty='n',cex=0.6)
  legend("top",legend=c("Elevated","Ambient"),col=c("red","blue"),
         pch=15,bty='n',cex=0.6,horiz = TRUE)
  title("Transpiration per unit leaf area of each ring vs VPD")
  
}

figure.sap.1()
# cord.x <- c(vpd.order$VPD[vpd.order$Ring == 1],rev(vpd.order$VPD[vpd.order$Ring == 1]))
# cord.y <- c(et.pred[[1]]  + fit[[1]]$residuals,rev(et.pred[[1]]  - fit[[1]]$residuals))
# polygon(cord.x,cord.y,col='skyblue',lty = 0)


