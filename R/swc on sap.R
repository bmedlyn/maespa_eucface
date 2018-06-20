source("r/process neutron.r")
source("r/load.r")
hr.df.all <- readRDS("output/maestraVPD/mastra and sap hr.rds")
hr.df.daily <- readRDS("output/maestraVPD/mastra and sap.rds")
# plot(volRing~Date,data = hr.df.daily[hr.df.daily$Ring=='R1',],ylim=c(0,1))
# plot(volRing~Trans,data = hr.df.daily[hr.df.daily$Ring=='R6',],ylim=c(0,2))
# abline(a=0,b=1)
# plot(sap~trans,data = hr.df.all,xlim=c(0,0.5),ylim=c(0,0.5))
# abline(a=0,b=1)
# plot(sap~trans,data = hr.df.all,xlim=c(0,0.4),ylim=c(0,0.4),pch=16)
rain.date <- unique(hr.df.daily$Date[hr.df.daily$PPT > 0])
hr.df.day <- hr.df.all[hr.df.all$PAR > 0.01,]
hr.df.day <- hr.df.day[!as.Date(hr.df.day$DateTime) %in% rain.date,]
hr.df.day$Ring <- as.character(hr.df.day$Ring)
hr.df.day$Date <- as.Date(hr.df.day$DateTime)

swc.df <- tidyr::spread(swc.depth.df.all,Depth,VWC)

hr.swc.df <- merge(hr.df.day,swc.df,by=c("Date","Ring"),all.x=TRUE)
hr.swc.df$swc <- (hr.swc.df$`-25` + hr.swc.df$`-50`+ hr.swc.df$`-75`+ hr.swc.df$`-100`)/ 4 /100

hr.swc.df.1314 <- hr.swc.df[lubridate::year(hr.swc.df$Date) %in% 2013:2014,]
hr.swc.df.1314$sap[hr.swc.df.1314$sap > 1] <- NA
hr.swc.df.1314 <- hr.swc.df.1314[complete.cases(hr.swc.df.1314$sap),]
hr.swc.df.1314$Rs.w <- hr.swc.df.1314$PAR * 1000 / 3.6 * 2

for (i in 1:6){
  sm[[i]]$Ring <- paste0('R',i)
}

lai.df <- do.call(rbind,sm)
lai.df$Ring <- as.factor(lai.df$Ring)

# lai.df <- hr.df.daily[,c('Date','Ring','LAI')]

hr.swc.lai.df.1314 <- merge(hr.swc.df.1314,lai.df,by=c('Date','Ring'),all.x=TRUE)
hr.swc.lai.df.1314$LAI <- hr.swc.lai.df.1314$LAIsmooth - 0.8

# get tdr swc####
swc.day.ls <- list()

for ( i in 1:6){
  # from remko
  meanVWC <- function(dfr){
    vwccols <- grep("VWC_",names(dfr))
    dfr <- dfr[,vwccols]
    dfr[dfr > 1] <- NA
    rowMeans(dfr, na.rm=TRUE)
  }
  
  swc.df <- downloadTOA5(sprintf("FACE_R%s_B1_SoilVars",i),
                         startDate = min(hr.swc.lai.df.1314$Date),
                         endDate = max(hr.swc.lai.df.1314$Date))

  swc.df.sub <- subset(swc.df,select = c("Date",
                                     "DateTime",
                                     "Theta5_1_Avg","Theta5_2_Avg",
                                     "Theta30_1_Avg","Theta30_2_Avg",
                                     "Theta75_1_Avg","Theta75_2_Avg"))
  
  swc.df.sub$swc.0.5 <- (swc.df.sub$Theta5_1_Avg + swc.df.sub$Theta5_2_Avg)/2
  
  swc.df.sub$swc.5.30 <- (swc.df.sub$Theta30_1_Avg +swc.df.sub$Theta30_2_Avg)/2
  
  swc.df.sub$swc.30.75 <- (swc.df.sub$Theta75_1_Avg + swc.df.sub$Theta75_2_Avg)/2
  
  swc.df.sub$swc.tdr <- meanVWC(swc.df) * 100
  
  swc.day.ls[[i]] <- data.table(swc.df.sub)[,list(Ring = paste0("R",i),
                                                  swc.theta.5 = mean(swc.0.5, na.rm=TRUE),
                                                  swc.theta.30 = mean(swc.5.30, na.rm=TRUE),
                                                  swc.theta.75 = mean(swc.30.75, na.rm=TRUE),
                                                  swc.tdr.mean = mean(swc.tdr, na.rm=TRUE)),
                                            
                                            by = Date]
}

swc.day.df <- do.call(rbind,swc.day.ls)
hr.swc.lai.df.1314 <- merge(hr.swc.lai.df.1314,swc.day.df)
hr.swc.lai.df.1314$swc.theta <- (hr.swc.lai.df.1314$swc.theta.5 + hr.swc.lai.df.1314$swc.theta.30) / 2 /100
hr.swc.lai.df.1314$swc.tdr <- hr.swc.lai.df.1314$swc.tdr.mean /100
#filter data##### 
hr.swc.lai.df.1314$et <- hr.swc.lai.df.1314$sap / hr.swc.lai.df.1314$LAI
hr.swc.lai.df.1314$et[hr.swc.lai.df.1314$et >0.5] <- NA
hr.swc.lai.df.1314 <- hr.swc.lai.df.1314[hr.swc.lai.df.1314$et > 0.001,]
hr.swc.lai.df.1314 <- hr.swc.lai.df.1314[hr.swc.lai.df.1314$Rs.w > 50,]
hr.swc.lai.df.1314 <- subset(hr.swc.lai.df.1314,select = -CO2)
hr.swc.lai.df.1314 <- hr.swc.lai.df.1314[complete.cases(hr.swc.lai.df.1314),]
hr.swc.lai.df.1314$c.treat <- NA
hr.swc.lai.df.1314$c.treat[hr.swc.lai.df.1314$Ring %in% paste0('R',c(1,4,5))] <- 'E'
hr.swc.lai.df.1314$c.treat[hr.swc.lai.df.1314$Ring %in% paste0('R',c(2,3,6))] <- 'A'
hr.swc.lai.df.1314$c.treat <- as.factor(hr.swc.lai.df.1314$c.treat)
hr.swc.lai.df.1314 <- hr.swc.lai.df.1314[hr.swc.lai.df.1314$RH < 1,]
hr.swc.lai.df.1314 <- hr.swc.lai.df.1314[hour(hr.swc.lai.df.1314$DateTime) %in% seq(8,18),]
hr.swc.lai.df.1314 <- hr.swc.lai.df.1314[trans > 0]
# functions floowing Whitley et al., 2013####
par.func <- function(par,k){
  # if(k >1000){
  #   return(k*-999)
  # }
  # if(k<0){
  #   return(k*999)
  # }
  return((par/1000) * (1000 + k) / (par + k))
  
}

# Drake et al. 2017
swc.func <- function(swc,swc.max,swc.min,q = 0.32){
  out.vec <- ((swc - swc.min) / (swc.max - swc.min))^q
  out.vec[which(swc > swc.max)] <- 1
  out.vec[which(swc < swc.min)] <- 0
  return(out.vec)
}
# swc.func(0.1,0.5,0.1)
# Whitley et al., 2013#
vpd.func <- function(vpd,k1,k2,d.max){
  if(k1<0 | k1>1 |k2<0 | k2>1){
    return(999*k1*k2)
  }
  
  return(exp(-k1*(vpd-d.max)^2 / (vpd + k2)))
}


vpd.func.2 <- function(vpd,k1){
  vpd * exp(-k1*(vpd))
}

# whitlry E######
whitley.e.func <- function(dat,pars,et.per.l='et',use.lai = FALSE,use.tdr=FALSE){
  
  # e.max <- pars[1]
  # par <- dat$Rs.w
  # k <- pars[2]
  # swc <- dat$swc
  # swc.max <- pars[3]
  # swc.min <- pars[4]
  # q <- pars[5]
  # vpd <- dat$VPD
  # k1 <- pars[6]
  # k2 <- pars[7]
  # d.max <- pars[8]
  
  # e.max <- pars[1]
  # par <- dat$Rs.w
  # k <- pars[2]
  # swc <- dat$swc
  # swc.max <- pars[3]
  # swc.min <- pars[4]
  # q <- 1
  # vpd <- dat$VPD
  # k1 <- pars[5]
  if (use.tdr ==TRUE){
    swc <- dat$swc.tdr
  }else{
    swc <- dat$swc
  }

  e.max <- pars[1]
  par <- dat$Rs.w
  k <- pars[2]
  swc <- dat$swc
  swc.max <- pars[3]
  swc.min <- pars[4]
  q <- 0.32
  vpd <- dat$VPD
  k1 <- pars[5]
  k2 <- pars[6]
  d.max <- pars[7]
  LAI.vec <- dat$LAI
  
  if (et.per.l == 'et'){
    et.vec <- dat$et
  }
  if (et.per.l == 'sap'){
    et.vec <- dat$sap
  }
  if (et.per.l == 'trans'){
    et.vec <- dat$trans
  }

  if (use.lai == TRUE){
    lai.factor <- (1-exp(-0.5*LAI.vec))
  }else{
    lai.factor <- 1
  }

  e.pred <- e.max * par.func(par,k) * swc.func(swc,swc.max,swc.min,q) * vpd.func(vpd,k1,k2,d.max) * lai.factor
  
  e.sd <- sd(dat$et,na.rm=TRUE)
  
  resid.e <- ((e.pred - dat$et)/e.sd)^2
  
  # resid.e[is.na(resid.e)] <- 10
 
  resid.sum <- sum(resid.e,na.rm = TRUE)
  
  return(resid.sum)
}

# e.max <- pars[1]
# k <- pars[2]
# swc.max <- pars[3]
# swc.min <- pars[4]
# q <- pars[5]
# k1 <- pars[6]
# k2 <- pars[7]
# d.max <- pars[8]

# 0.6 * par.func(2,0.2) * swc.func(0.2,0.6,0.1,0.2) * vpd.func(1,0.2,0.4,3)

# lower <- c(0.4,100, 0.3,0,  0.1,0.01, 0.01, 2) 
# upper <- c(1,  800,   0.6,0.2,0.8,0.3,  0.6,  4)
# 0.6 * par.func(800,500) * swc.func(0.5,0.6,0.1,0.2) * vpd.func.2(4,0.2)
# lower <- c(0.4,100, 0.3,0,  0.1) 
# upper <- c(1,  800, 0.6,0.2,0.8)
lower <- c(0.2,100, 0.1,0,  0.1,0,1)
upper <- c(1,  1000,0.8,0.2,0.4,1,4)
NPmax <- 200
maxiter <- 100

set.seed(1234)

#fit de#####
library(DEoptim)
e.fit <- DEoptim(fn=whitley.e.func,lower=lower,upper=upper,
                     dat=hr.swc.lai.df.1314,#[hr.swc.lai.df.1314$Rs.w > 100,],
                 DEoptim.control(NP = NPmax,itermax=maxiter,trace=T,parallelType = 1,
                                           parVar = list("par.func","swc.func","vpd.func")))

e.fit.best <- unname(e.fit$optim$bestmem)

hr.swc.lai.df.1314$e.predict <- e.fit.best[1] * par.func(hr.swc.lai.df.1314$Rs.w,e.fit.best[2]) *
  swc.func(hr.swc.lai.df.1314$swc,e.fit.best[3],e.fit.best[4],1) * 
  vpd.func(hr.swc.lai.df.1314$VPD,e.fit.best[5],e.fit.best[6],e.fit.best[7])
# fit de for tdr#####
tdr.df <- hr.swc.lai.df.1314[,c("et",'VPD','Rs.w','swc.tdr')]
names(tdr.df) <- c("et",'VPD','Rs.w','swc')
e.fit.tdr <- DEoptim(fn=whitley.e.func,lower=lower,upper=upper,
                     dat=tdr.df,#[hr.swc.lai.df.1314$Rs.w > 100,],
                     DEoptim.control(NP = NPmax,itermax=maxiter,trace=T,parallelType = 1,
                                     parVar = list("par.func","swc.func","vpd.func")))

e.fit.best.tdr <- unname(e.fit.tdr$optim$bestmem)

hr.swc.lai.df.1314$e.predict.tdr <- e.fit.best.tdr[1] * par.func(hr.swc.lai.df.1314$Rs.w,e.fit.best.tdr[2]) *
  swc.func(hr.swc.lai.df.1314$swc,e.fit.best.tdr[3],e.fit.best.tdr[4],1) * 
  vpd.func(hr.swc.lai.df.1314$VPD,e.fit.best.tdr[5],e.fit.best.tdr[6],e.fit.best.tdr[7])



# hr.swc.lai.df.1314$e.predict <- e.fit.best[1] * par.func(hr.swc.lai.df.1314$Rs.w,e.fit.best[2]) * 
#   swc.func(hr.swc.lai.df.1314$swc,e.fit.best[3],e.fit.best[4],1) * 
#   vpd.func(hr.swc.lai.df.1314$VPD,e.fit.best[5],e.fit.best[6],e.fit.best[7])

plot(et~e.predict.tdr,data = hr.swc.lai.df.1314,xlim=c(0,0.3),ylim=c(0,0.3))
abline(a=0,b=1,col="grey")
summary(lm(et~e.predict.tdr,data = hr.swc.lai.df.1314))

plot(et~e.predict,data = hr.swc.lai.df.1314,xlim=c(0,0.3),ylim=c(0,0.3))
abline(a=0,b=1,col="grey")
summary(lm(et~e.predict,data = hr.swc.lai.df.1314))
# 
# library(mgcv)
# fit.gam <- gam(et~s(Rs.w) + s(VPD) ,data = hr.swc.lai.df.1314[hr.swc.lai.df.1314$Rs.w > 100,])
# fit.gam.swc <- gam(et~s(Rs.w) + s(VPD) + s(swc),data = hr.swc.lai.df.1314[hr.swc.lai.df.1314$Rs.w > 100,])
# see <- predict(fit.gam)
# see.swc <- predict(fit.gam.swc)
# plot(et~see,data = hr.swc.lai.df.1314[hr.swc.lai.df.1314$Rs.w > 100,])
# summary(lm(et~see.swc,data = hr.swc.lai.df.1314[hr.swc.lai.df.1314$Rs.w > 100,]))
# summary(lm(et~see,data = hr.swc.lai.df.1314[hr.swc.lai.df.1314$Rs.w > 100,]))
# plot(et~swc,data = hr.swc.lai.df.1314[hr.swc.lai.df.1314$Rs.w > 100,])
# plot(et~swc,data = hr.swc.lai.df.1314)
# summary(lm(et~swc,data = hr.swc.lai.df.1314[hr.swc.lai.df.1314$Rs.w > 100,]))
# par(mfrow=c(1,1))
# plot(et~swc,data = hr.swc.lai.df.1314[hr.swc.lai.df.1314$Rs.w > 100,])
# 
# par(mfrow=c(1,3))
# et.max <- max(hr.swc.lai.df.1314$trans,na.rm = TRUE)
# plot(x = 0:1000,y=e.fit.best[1]*par.func(0:1000,e.fit.best[2]),type='l',
#      xlab='PAR (Wm-2)',ylab='f(PAR)')
# points((trans)~Rs.w,data = hr.swc.lai.df.1314,pch=16,col='grey')
# plot(x = seq(0,8,0.1),y=e.fit.best[1]*vpd.func(seq(0,8,0.1),e.fit.best[5],e.fit.best[6],e.fit.best[7]),type="l",
#      xlab='D (kPa)',ylab='f(D)')
# points((trans)~VPD,data = hr.swc.lai.df.1314,pch=16,col='grey')
# plot(x = seq(0,0.3,0.01),y=e.fit.best[1]*swc.func(seq(0,0.3,0.01),e.fit.best[3],e.fit.best[4],1),type="l",
#      xlab='SWC (m2 m-2)',ylab='f(swc)')
# points((trans)~swc,data = hr.swc.lai.df.1314,pch=16,col='grey')

# swc.func(0.6,e.fit.best[3],e.fit.best[4],1)

# out.df <- data.frame(y=hr.swc.lai.df.1314$et[hr.swc.lai.df.1314$Rs.w > 00],
#                      x1 = hr.swc.lai.df.1314$Rs.w[hr.swc.lai.df.1314$Rs.w > 00],
#                      x2 = hr.swc.lai.df.1314$VPD[hr.swc.lai.df.1314$Rs.w > 00],
#                      x3 = hr.swc.lai.df.1314$swc[hr.swc.lai.df.1314$Rs.w > 00])
# write.csv(out.df,"test.csv",row.names = FALSE)


# quantile reg####
library(quantreg)
# fit swc
# fit.q.90 <- nlrq(et~0.3*swc.func(swc,swc.max,swc.min = 0.05,q = 0.3),
#                  data = hr.swc.lai.df.1314,
#                  tau=0.9,start=list(swc.max=0.8))
plot(sap~swc.tdr,data = hr.swc.lai.df.1314[c.treat == "A",])
points(sap~swc.tdr,data = hr.swc.lai.df.1314[c.treat == "E",],pch=16,col="grey")
fit.q.90.tdr <- nlrq(et~0.3*swc.func(swc.tdr,swc.max,swc.min = 0.001,q = 0.3),
                 data = hr.swc.lai.df.1314,
                 tau=0.9,start=list(swc.max=0.2))

fit.q.90.a <- nlrq(et~0.3*swc.func(swc.tdr,swc.max,swc.min = 0.001,q = 0.3),
                   data = hr.swc.lai.df.1314[hr.swc.lai.df.1314$c.treat == 'A',],
                   tau=0.9,start=list(swc.max=0.2))

fit.q.90.e <- nlrq(et~0.3*swc.func(swc.tdr,swc.max,swc.min = 0.001,q = 0.3),
                   data = hr.swc.lai.df.1314[hr.swc.lai.df.1314$c.treat == 'E',],
                   tau=0.9,start=list(swc.max=0.2))

# fit.q.90.tdr <- nlrq(et~0.3*swc.func(swc.tdr,swc.max,swc.min,q),
#                      data = hr.swc.lai.df.1314,
#                      tau=0.9,start=list(swc.max=0.8,swc.min=0.05,q=0.3))

summary(fit.q.90.tdr)
# fit par
fit.q.90.par <- nlrq(et~e.max*par.func(Rs.w,k),
                    data = hr.swc.lai.df.1314,
                    tau=0.9,start=list(e.max=0.2,k = 400),
                    control = list(maxiter=200))

fit.q.90.par.a <- nlrq(et~e.max*par.func(Rs.w,k),
                   data = hr.swc.lai.df.1314[hr.swc.lai.df.1314$c.treat == 'A',],
                   tau=0.9,start=list(e.max=0.2,k = 400))

fit.q.90.par.e <- nlrq(et~e.max*par.func(Rs.w,k),
                   data = hr.swc.lai.df.1314[hr.swc.lai.df.1314$c.treat == 'E',],
                   tau=0.9,start=list(e.max=0.2,k = 400))
summary(fit.q.90.par)
par(mfrow=c(2,1))
plot(et~Rs.w,data = hr.swc.lai.df.1314)
lines(x=0:1000,y=0.2618514 * par.func(0:1000,512.7826892),col="navy")
lines(x=0:1000,y=0.2818115 * par.func(0:1000,551.9524100),col="red")


# fit d
fit.q.90.d <- nlrq(et~e.max*vpd.func(VPD,k1,k2,d.max),
                    data = hr.swc.lai.df.1314,
                    tau=0.90,start=list(k1 = 0.2,k2=0.2,d.max=2,e.max=0.4),
                    control = list(maxiter=200))

fit.q.90.d.a <- nlrq(et~e.max*vpd.func(VPD,k1,k2,d.max),
                       data = hr.swc.lai.df.1314[hr.swc.lai.df.1314$c.treat == 'A',],
                       tau=0.9,start=list(k1 = 0.2,k2=0.2,d.max=2,e.max=0.4))

fit.q.90.d.e <- nlrq(et~e.max*vpd.func(VPD,k1,k2,d.max),
                       data = hr.swc.lai.df.1314[hr.swc.lai.df.1314$c.treat == 'E',],
                       tau=0.9,start=list(k1 = 0.2,k2=0.2,d.max=2,e.max=0.4))
plot(et~VPD,data = hr.swc.lai.df.1314)
lines(x=0:8,y=0.2327424 * vpd.func(0:8,0.3702648,0.9770205,2.2633071),col="navy")
lines(x=0:8,y=0.2420323 * vpd.func(0:8,0.2827370,0.7758266,2.3234391),col="red")
summary(fit.q.90.d)
# et.q.90.par <- predict(fit.q.90.par)

# plot par####
par(mfrow=c(1,3),mar=c(5,5,1,1))
rs.vec <- 0:1000
et.q.90.par <- coef(fit.q.90.par)[[1]] * par.func(rs.vec,coef(fit.q.90.par)[[2]]) 

# plot(hr.swc.lai.df.1314$et~hr.swc.lai.df.1314$Rs.w,col="grey",pch=16,cex=0.5,ylim=c(0,0.4),
#      xlab=expression(Solar~radiation~(W~m^-2)),ylab=expression(ET~(mm~hr^-1)))
smoothScatter(hr.swc.lai.df.1314$et~hr.swc.lai.df.1314$Rs.w,col="grey",pch='',cex=0.5,ylim=c(0,0.4),
              xlab=expression(Solar~radiation~(W~m^-2)),ylab=expression(ET~(mm~hr^-1)))
points(et.q.90.par~rs.vec,type="l",col="grey")
points(x = 0:1000,y=e.fit.best[1]*par.func(0:1000,e.fit.best[2]),type='l',col="red")
points(x = 0:1000,y=e.fit.best.tdr[1]*par.func(0:1000,e.fit.best.tdr[2]),type='l',col="coral")
# pars from Rhys's thesis of castlereagh
points(x = 0:1000,y=0.14*par.func(0:1000,50),type='l',col="blue")

legend("topleft",legend = c("90% quantile","DE fit",'Castlereagh',"TDR"),
       lty = "solid",col=c('grey','red',"blue",'coral'),bty='n')
# plot d
d.vec <- seq(0,8,0.01)
et.q.90.d <- vpd.func(d.vec,coef(fit.q.90.d)[[1]],coef(fit.q.90.d)[[2]],coef(fit.q.90.d)[[3]]) *
  coef(fit.q.90.d)[[4]]

# plot(hr.swc.lai.df.1314$et~hr.swc.lai.df.1314$VPD,col="grey",pch=16,cex=0.5,ylim=c(0,0.4),
#      xlab=expression(D~(kPa)),ylab=expression(ET~(mm~hr^-1)))
smoothScatter(hr.swc.lai.df.1314$et~hr.swc.lai.df.1314$VPD,col="grey",pch='',cex=0.5,ylim=c(0,0.4),
     xlab=expression(D~(kPa)),ylab=expression(ET~(mm~hr^-1)))
points(et.q.90.d~d.vec,type="l",col="grey")
points(x = seq(0,8,0.1),y=e.fit.best[1]*vpd.func(seq(0,8,0.1),e.fit.best[5],e.fit.best[6],e.fit.best[7]),
       type="l",col="red")
points(x = seq(0,8,0.1),y=e.fit.best.tdr[1]*vpd.func(seq(0,8,0.1),e.fit.best[5],e.fit.best[6],e.fit.best[7]),
       type='l',col="coral")
points(x = seq(0,8,0.1),y=0.14*vpd.func(seq(0,8,0.1),0.23,0.27,1.83),
       type="l",col="blue")
# plot swc
swc.vec <- seq(0,0.3,0.001)
et.q.90 <- swc.func(swc.vec,coef(fit.q.90)[[1]],coef(fit.q.90)[[2]],coef(fit.q.90)[[3]]) * 0.3
et.q.90.tdr <- swc.func(swc.vec,coef(fit.q.90.tdr)[[1]],0.05,0.28) * 0.3

# plot(et.q.90~swc.vec,pch=16,ylim=c(0,0.4),type="l",col="grey",
#      xlab=expression(SWC),ylab=expression(ET~(mm~hr^-1)))
smoothScatter(hr.swc.lai.df.1314$et~hr.swc.lai.df.1314$swc,pch='',cex=0.5,ylim=c(0,0.4),
     xlab=expression(SWC),ylab=expression(ET~(mm~hr^-1)))
points(et.q.90~swc.vec,pch=16,ylim=c(0,0.4),type="l",col="grey",
     xlab=expression(SWC),ylab=expression(ET~(mm~hr^-1)))
points(et.q.90.tdr~swc.vec,type="l",lty='dashed',col="grey")
# points(hr.swc.lai.df.1314$et~hr.swc.lai.df.1314$swc,col="grey",pch=16,cex=0.5)
points(x = seq(0,0.3,0.01),y=e.fit.best[1]*swc.func(seq(0,0.3,0.01),e.fit.best[3],e.fit.best[4],0.32),
       type="l",col="red")
points(x = seq(0,0.3,0.01),y=e.fit.best.tdr[1]*swc.func(seq(0,0.3,0.01),e.fit.best[3],e.fit.best[4],0.32),
       type='l',col="coral")
legend("topleft",legend = c("90% quantile TDR"),
       lty = "dashed",col=c('grey'),bty='n')
# plot scater
par(mfrow=c(3,2),mar=c(5,5,1,1))
plot(et~e.predict,data = hr.swc.lai.df.1314,xlim=c(0,0.4),ylim=c(0,0.4),pch=16,col='grey')
abline(a=0,b=1,col="black")
legend("topleft",legend = "DE fit",bty='n')

plot(et~e.predict.tdr,data = hr.swc.lai.df.1314,xlim=c(0,0.4),ylim=c(0,0.4),pch=16,col='grey')
abline(a=0,b=1,col="black")
legend("topleft",legend = "DE fit TDR",bty='n')

et.q.90 <- coef(fit.q.90.par)[[1]] * par.func(hr.swc.lai.df.1314$Rs.w,coef(fit.q.90.par)[[2]]) * 
  vpd.func(hr.swc.lai.df.1314$VPD,coef(fit.q.90.d)[[1]],coef(fit.q.90.d)[[2]],coef(fit.q.90.d)[[3]]) * 
  swc.func(hr.swc.lai.df.1314$swc,coef(fit.q.90)[[1]],coef(fit.q.90)[[2]],coef(fit.q.90)[[3]])
  

plot(hr.swc.lai.df.1314$et~et.q.90,xlim=c(0,0.4),ylim=c(0,0.4),pch=16,col='grey',ylab='et')
abline(a=0,b=1,col="black")
legend("topleft",legend = "90% NLQR",bty='n')

et.q.90.tdr <- coef(fit.q.90.par)[[1]] * par.func(hr.swc.lai.df.1314$Rs.w,coef(fit.q.90.par)[[2]]) * 
  vpd.func(hr.swc.lai.df.1314$VPD,coef(fit.q.90.d)[[1]],coef(fit.q.90.d)[[2]],coef(fit.q.90.d)[[3]]) * 
  swc.func(hr.swc.lai.df.1314$swc,coef(fit.q.90.tdr)[[1]],0.05,0.3)

plot(hr.swc.lai.df.1314$et~et.q.90.tdr,xlim=c(0,0.4),ylim=c(0,0.4),pch=16,col='grey',ylab='et')
abline(a=0,b=1,col="black")
legend("topleft",legend = "90% NLQR TDR",bty='n')

plot(swc~swc.tdr,data = hr.swc.lai.df.1314,pch=16,col='grey',xlab="TDR",ylab='Neutron')
abline(a=0,b=1,col="black")
summary(lm(swc~swc.tdr,data = hr.swc.lai.df.1314))

# plot et lai####
hr.swc.lai.df.1314$v.level <- cut(hr.swc.lai.df.1314$VPD,c(0,1,2,3,10))
palette(c('blue','darkseagreen','coral','red'))
plot(sap~LAI,data = hr.swc.lai.df.1314,pch=16,col=palette()[v.level])
abline(rq(sap~LAI,data = hr.swc.lai.df.1314,tau=.90))
fit.nlqr.lai <- nlrq(sap~e.max*(1-exp(-0.5*LAI)),
                     data = hr.swc.lai.df.1314,
                     tau=0.90,start=list(e.max=0.2),
                     control = list(maxiter=200))
summary(fit.nlqr.lai)
hr.swc.lai.df.1314$sap.vec <- predict(fit.nlqr.lai)
points(sap.vec~LAI,data = hr.swc.lai.df.1314[order(hr.swc.lai.df.1314$LAI)],type='l',col='grey',lwd=2)
legend("topleft",legend = paste0("<",c(0,1,2,3,10)),pch=16,col=palette(),title = "VPD kPa")




#fit de lai#####
library(DEoptim)
whitley.e.lai.func <- function(dat,pars){
  
  e.max <- pars[1]
  par <- dat$Rs.w
  k <- pars[2]
  swc <- dat$swc
  swc.max <- pars[3]
  swc.min <- pars[4]
  q <- 0.32
  vpd <- dat$VPD
  k1 <- pars[5]
  k2 <- pars[6]
  d.max <- pars[7]
  LAI.vec <- dat$LAI
  et.vec <- dat$sap
  
  e.pred <- e.max * par.func(par,k) * swc.func(swc,swc.max,swc.min,q) * 
    vpd.func(vpd,k1,k2,d.max) * (1-exp(-0.5*LAI.vec))
  
  e.sd <- sd(et.vec,na.rm=TRUE)
  
  resid.e <- ((e.pred - et.vec)/e.sd)^2
  
  # resid.e[is.na(resid.e)] <- 10
  
  resid.sum <- sum(resid.e,na.rm = TRUE)
  
  return(resid.sum)
}
lower <- c(0.2,100, 0.1,0,  0.1,0,1)
upper <- c(1,  1000,0.8,0.2,0.4,1,4)
NPmax <- 200
maxiter <- 100

set.seed(1234)

e.lai.fit <- DEoptim(fn=whitley.e.func,lower=lower,upper=upper,
                 dat=hr.swc.lai.df.1314,
                 t.per.l='et',use.lai=TRUE,use.tdr=TRUE,#[hr.swc.lai.df.1314$Rs.w > 100,],
                 DEoptim.control(NP = NPmax,itermax=maxiter,trace=T,parallelType = 1,
                                 parVar = list("par.func","swc.func","vpd.func")))

e.lai.fit.best <- unname(e.lai.fit$optim$bestmem)

hr.swc.lai.df.1314$e.predict.lai <- e.lai.fit.best[1] * par.func(hr.swc.lai.df.1314$Rs.w,e.lai.fit.best[2]) *
  swc.func(hr.swc.lai.df.1314$swc,e.lai.fit.best[3],e.lai.fit.best[4],1) * 
  vpd.func(hr.swc.lai.df.1314$VPD,e.lai.fit.best[5],e.lai.fit.best[6],e.lai.fit.best[7]) * 
  (1-exp(-0.5*hr.swc.lai.df.1314$LAI))
# fit DE lai for a
e.lai.fit.a <- DEoptim(fn=whitley.e.func,lower=lower,upper=upper,
                     dat=hr.swc.lai.df.1314[c.treat =='A',],
                     t.per.l='et',use.lai=TRUE,use.tdr=TRUE,#[hr.swc.lai.df.1314$Rs.w > 100,],
                     DEoptim.control(NP = NPmax,itermax=maxiter,trace=T,parallelType = 1,
                                     parVar = list("par.func","swc.func","vpd.func")))

e.lai.fit.best.a <- unname(e.lai.fit.a$optim$bestmem)

# hr.swc.lai.df.1314$e.predict.lai.a <- e.lai.fit.best[1] * par.func(hr.swc.lai.df.1314$Rs.w,e.lai.fit.best[2]) *
#   swc.func(hr.swc.lai.df.1314$swc,e.lai.fit.best[3],e.lai.fit.best[4],1) * 
#   vpd.func(hr.swc.lai.df.1314$VPD,e.lai.fit.best[5],e.lai.fit.best[6],e.lai.fit.best[7]) * 
#   (1-exp(-0.5*hr.swc.lai.df.1314$LAI))
# fit DE lai for e
e.lai.fit.e <- DEoptim(fn=whitley.e.func,lower=lower,upper=upper,
                     dat=hr.swc.lai.df.1314[c.treat =='E',],
                     t.per.l='et',use.lai=TRUE,use.tdr=TRUE,#[hr.swc.lai.df.1314$Rs.w > 100,],
                     DEoptim.control(NP = NPmax,itermax=maxiter,trace=T,parallelType = 1,
                                     parVar = list("par.func","swc.func","vpd.func")))

e.lai.fit.best.e <- unname(e.lai.fit.a$optim$bestmem)

# hr.swc.lai.df.1314$e.predict.lai <- e.lai.fit.best[1] * par.func(hr.swc.lai.df.1314$Rs.w,e.lai.fit.best[2]) *
#   swc.func(hr.swc.lai.df.1314$swc,e.lai.fit.best[3],e.lai.fit.best[4],1) * 
#   vpd.func(hr.swc.lai.df.1314$VPD,e.lai.fit.best[5],e.lai.fit.best[6],e.lai.fit.best[7]) * 
#   (1-exp(-0.5*hr.swc.lai.df.1314$LAI))

# fit de for lai trans#####
e.fit.trans <- DEoptim(fn=whitley.e.func,lower=lower,upper=upper,
                       dat=hr.swc.lai.df.1314,et.per.l='trans',use.lai=TRUE,use.tdr=TRUE,#[hr.swc.lai.df.1314$Rs.w > 100,],
                       DEoptim.control(NP = NPmax,itermax=maxiter,trace=T,parallelType = 1,
                                       parVar = list("par.func","swc.func","vpd.func")))

e.fit.best.trans <- unname(e.fit.trans$optim$bestmem)

hr.swc.lai.df.1314$e.predict.tdr <- e.fit.best.trans[1] * par.func(hr.swc.lai.df.1314$Rs.w,e.fit.best.trans[2]) *
  swc.func(hr.swc.lai.df.1314$swc,e.fit.best.tdr[3],e.fit.best.tdr[4],1) * 
  vpd.func(hr.swc.lai.df.1314$VPD,e.fit.best.tdr[5],e.fit.best.tdr[6],e.fit.best.tdr[7])


# fit de sap
e.fit.sap <- DEoptim(fn=whitley.e.func,lower=lower,upper=upper,
                 dat=hr.swc.lai.df.1314,et.per.l='sap',use.lai=TRUE,use.tdr=TRUE,#[hr.swc.lai.df.1314$Rs.w > 100,],
                 DEoptim.control(NP = NPmax,itermax=maxiter,trace=T,parallelType = 1,
                                 parVar = list("par.func","swc.func","vpd.func")))

e.fit.sap.best <- unname(e.fit.sap$optim$bestmem)

hr.swc.lai.df.1314$e.predict.sap <- e.fit.sap.best[1] * par.func(hr.swc.lai.df.1314$Rs.w,e.fit.sap.best[2]) *
  swc.func(hr.swc.lai.df.1314$swc,e.fit.sap.best[3],e.fit.sap.best[4],1) * 
  vpd.func(hr.swc.lai.df.1314$VPD,e.fit.sap.best[5],e.fit.sap.best[6],e.fit.sap.best[7])

# 
plot(sap~e.predict.sap,data = hr.swc.lai.df.1314,pch=16,xlab='modelled',ylab='sap flow',cex=0.5)
points(sap~e.predict.lai,data = hr.swc.lai.df.1314,pch=16,col="grey",cex=0.5)
legend('topleft',legend = c('1-exp(-0.5*L)','1'),title = "LAI impact",pch=16,col=c('grey','black'))
abline(a=0,b=1)
plot(e.predict.lai~e.predict.sap,data = hr.swc.lai.df.1314,
     xlab='LAI impact = 1',ylab='1-exp(-0.5*L)',pch=16,cex=0.8)
abline(a=0,b=1)

summary(step(lm(sap~swc + LAI + PAR + VPD ,data=hr.swc.lai.df.1314),direction="both"))


par(mfrow=c(1,3))
# et.max <- max(hr.swc.lai.df.1314$e,na.rm = TRUE)
plot((et)~Rs.w,data = hr.swc.lai.df.1314,pch=16,col='grey',xlab='Rs (Wm-2)',ylab='ET',ylim=c(0,0.5))
points(x = 0:1000,y=e.fit.best[1]*par.func(0:1000,e.fit.best[2]),type='l')


plot((et)~VPD,data = hr.swc.lai.df.1314,pch=16,col='grey',xlab='D (kPa)',ylab='ET',ylim=c(0,0.5))
points(x = seq(0,8,0.1),y=e.fit.best[1]*vpd.func(seq(0,8,0.1),e.fit.best[5],e.fit.best[6],e.fit.best[7]),type="l")

plot((et)~swc,data = hr.swc.lai.df.1314,pch=16,col='grey',xlab='SWC',ylab='ET',ylim=c(0,0.5))
points(x = seq(0,0.3,0.01),y=e.fit.best[1]*swc.func(seq(0,0.3,0.01),e.fit.best[3],e.fit.best[4],1),type="l")


plot(trans~e.predict,data = hr.swc.lai.df.1314,xlim=c(0,0.3),ylim=c(0,0.3))
abline(a=0,b=1,col="grey")


#fit de lai Ca treat#####
library(DEoptim)
lower <- c(0.2,100, 0.1,0,  0.1,0,1)
upper <- c(1,  1000,0.8,0.2,0.4,1,4)
NPmax <- 200
maxiter <- 100
set.seed(1234)
# amb
amb.df <- hr.swc.lai.df.1314[c.treat == "A",]
e.lai.fit.a <- DEoptim(fn=whitley.e.func,lower=lower,upper=upper,
                     dat=amb.df,et.per.l='sap',use.lai=TRUE,use.tdr=FALSE,
                     DEoptim.control(NP = NPmax,itermax=maxiter,trace=T,parallelType = 1,
                                     parVar = list("par.func","swc.func","vpd.func")))

e.lai.fit.best.a <- unname(e.lai.fit.a$optim$bestmem)

amb.df$e.predict.lai <- e.lai.fit.best.a[1] * par.func(amb.df$Rs.w,e.lai.fit.best.a[2]) *
  swc.func(amb.df$swc,e.lai.fit.best.a[3],e.lai.fit.best.a[4],1) *
  vpd.func(amb.df$VPD,e.lai.fit.best.a[5],e.lai.fit.best.a[6],e.lai.fit.best.a[7]) *
  (1-exp(-0.5*amb.df$LAI))

e.fit.trans.amb <- DEoptim(fn=whitley.e.func,lower=lower,upper=upper,
                       dat=amb.df,et.per.l='trans',use.lai=TRUE,use.tdr=FALSE,#[hr.swc.lai.df.1314$Rs.w > 100,],
                       DEoptim.control(NP = NPmax,itermax=maxiter,trace=T,parallelType = 1,
                                       parVar = list("par.func","swc.func","vpd.func")))

e.fit.best.trans.amb <- unname(e.fit.trans.amb$optim$bestmem)
# ele
ele.df <- hr.swc.lai.df.1314[c.treat == "E",]
e.lai.fit.e <- DEoptim(fn=whitley.e.func,lower=lower,upper=upper,
                       dat=ele.df,et.per.l='sap',use.lai=TRUE,use.tdr=FALSE,
                       DEoptim.control(NP = NPmax,itermax=maxiter,trace=T,parallelType = 1,
                                       parVar = list("par.func","swc.func","vpd.func")))

e.lai.fit.best.e <- unname(e.lai.fit.e$optim$bestmem)

# ele.df$e.predict.lai <- e.lai.fit.best.e[1] * par.func(ele.df$Rs.w,e.lai.fit.best.e[2]) *
#   swc.func(ele.df$swc,e.lai.fit.best.e[3],e.lai.fit.best.e[4],1) *
#   vpd.func(ele.df$VPD,e.lai.fit.best.e[5],e.lai.fit.best.e[6],e.lai.fit.best.e[7]) *
#   (1-exp(-0.5*ele.df$LAI))

e.fit.trans.ele <- DEoptim(fn=whitley.e.func,lower=lower,upper=upper,
                           dat=ele.df,et.per.l='trans',use.lai=TRUE,use.tdr=FALSE,#[hr.swc.lai.df.1314$Rs.w > 100,],
                           DEoptim.control(NP = NPmax,itermax=maxiter,trace=T,parallelType = 1,
                                           parVar = list("par.func","swc.func","vpd.func")))

e.fit.best.trans.ele <- unname(e.fit.trans.ele$optim$bestmem)
plot(hr.swc.lai.df.1314$sap~hr.swc.lai.df.1314$swc,pch=16,cex=0.2,ylim=c(0,0.4),
     xlab=expression(theta),ylab=expression(ET~(mm~hr^-1)),
     col = (c("lightskyblue", 'coral')[hr.swc.lai.df.1314$c.treat]))

plot(sap~Rs.w,pch=16,cex=0.2,ylim=c(0,0.4),data = hr.swc.lai.df.1314[c.treat =='A'],
     xlab=expression(Rs~(W~m^-2)),ylab=expression(ET~(mm~hr^-1)),
     col = c("lightskyblue"))
plot(sap~Rs.w,pch=16,cex=0.2,ylim=c(0,0.4),data = hr.swc.lai.df.1314[c.treat =='E'],
     xlab=expression(Rs~(W~m^-2)),ylab=expression(ET~(mm~hr^-1)),
     col = c("coral"))
plot(sap~trans,data=hr.swc.lai.df.1314)
check.df <- hr.swc.lai.df.1314[sap < 0.02]

# fit ring#####
library(DEoptim)
lower <- c(0.2,100, 0.1,0,  0.1,0,1)
upper <- c(1,  1000,0.8,0.2,0.4,1,4)
NPmax <- 200
maxiter <- 100
set.seed(1234)
de.sap.tdr.ls <- list()
de.trans.tdr.ls <- list()
de.sap.neu.ls <- list()
de.trans.neu.ls <- list()
for (i in 1:6){
  e.fit.temp <- DEoptim(fn=whitley.e.func,lower=lower,upper=upper,
                            dat=hr.swc.lai.df.1314[Ring == paste0('R',i),],
                            et.per.l='trans',use.lai=TRUE,use.tdr=TRUE,
                            DEoptim.control(NP = NPmax,itermax=maxiter,trace=T,parallelType = 1,
                                            parVar = list("par.func","swc.func","vpd.func")))
  de.sap.tdr.ls[[i]] <- unname(e.fit.temp$optim$bestmem)
  
  e.fit.temp <- DEoptim(fn=whitley.e.func,lower=lower,upper=upper,
                        dat=hr.swc.lai.df.1314[Ring == paste0('R',i),],
                        et.per.l='trans',use.lai=TRUE,use.tdr=FALSE,
                        DEoptim.control(NP = NPmax,itermax=maxiter,trace=T,parallelType = 1,
                                        parVar = list("par.func","swc.func","vpd.func")))
  de.sap.neu.ls[[i]] <- unname(e.fit.temp$optim$bestmem)
  # 
  e.fit.temp <- DEoptim(fn=whitley.e.func,lower=lower,upper=upper,
                        dat=hr.swc.lai.df.1314[Ring == paste0('R',i),],
                        et.per.l='sap',use.lai=TRUE,use.tdr=TRUE,
                        DEoptim.control(NP = NPmax,itermax=maxiter,trace=T,parallelType = 1,
                                        parVar = list("par.func","swc.func","vpd.func")))
  de.trans.tdr.ls[[i]] <- unname(e.fit.temp$optim$bestmem)
  
  e.fit.temp <- DEoptim(fn=whitley.e.func,lower=lower,upper=upper,
                        dat=hr.swc.lai.df.1314[Ring == paste0('R',i),],
                        et.per.l='sap',use.lai=TRUE,use.tdr=FALSE,
                        DEoptim.control(NP = NPmax,itermax=maxiter,trace=T,parallelType = 1,
                                        parVar = list("par.func","swc.func","vpd.func")))
  de.trans.neu.ls[[i]] <- unname(e.fit.temp$optim$bestmem)
  rm(e.fit.temp)
}
# plot by ring####
plot.de.fit.func <- function(de.trans.tdr.ls,hr.swc.lai.df.1314,plot.trans = FALSE,
                             x.in ,x.lim=c(0,1000)){
  par(mfrow=c(3,2),oma=c(5,5,3,2))
  palette(c('red','navy','navy',
            'red','red','navy'))

  for (i in 1:6){
    dat <- hr.swc.lai.df.1314[Ring == paste0('R',i),]

    x.vec.nm <- match.arg(x.in,c("Rs.w",'VPD','swc','swc.tdr','swc.theta'))
    
    x.df <- c(dat[,x.vec.nm,with=FALSE])
    names(x.df) <- 'x'  
    par(mar=c(0,0,0,0))
    if(plot.trans==TRUE){
      plot(dat$trans~x.df$x,
           pch=16,cex=0.2,ylim=c(0,0.4),xlim=x.lim,
           xlab='',ylab='',ann=FALSE,axes=FALSE,
           col = palette()[i])
    }else{
      plot(dat$sap~x.df$x,
           pch=16,cex=0.2,ylim=c(0,0.4),xlim=x.lim,
           xlab='',ylab='',ann=FALSE,axes=FALSE,
           col = palette()[i])
    }
    
    if(x.vec.nm == 'Rs.w'){
      x.vec <- 0:1000
      y.vec <- de.trans.tdr.ls[[i]][1]*
        par.func(0:1000,de.trans.tdr.ls[[i]][2])*
        (1-exp(-0.5*mean(dat$LAI)))
    }
    
    if(x.vec.nm == 'VPD'){
      x.vec <-seq(0,8,0.1)
      y.vec <-de.trans.tdr.ls[[i]][1]*
        vpd.func(x.vec,de.trans.tdr.ls[[i]][5],de.trans.tdr.ls[[i]][6],de.trans.tdr.ls[[i]][7]) *
        (1-exp(-0.5*mean(dat$LAI)))
    }
    
    if(x.vec.nm %in% c('swc','swc.tdr','swc.theta')){
      x.vec <-seq(0,0.3,0.01)
      y.vec <- de.trans.tdr.ls[[i]][1]*
        swc.func(seq(0,0.3,0.01),de.trans.tdr.ls[[i]][3],de.trans.tdr.ls[[i]][4],1) * 
      (1-exp(-0.5*mean(dat$LAI)))
    }
    
    points(x = x.vec,
           y = y.vec,
           type='l',col=palette()[i],lty="solid",lwd=2)
    x.interval <- pretty(x.vec,n = 5)
    axis(1,at=x.interval,labels = rep('',length(x.interval)))
    
    if (i %in% c(1,3,5)){
      axis(2,at=seq(0,0.3,0.1),labels = seq(0,0.3,0.1))
    }else{
      axis(4,at=seq(0,0.3,0.1),labels = seq(0,0.3,0.1))
    }
    
    if (i %in% c(5,6)){
      axis(1,
           at =     x.interval,
           labels = x.interval)
    }
    
    legend("topleft",legend = paste0('R',i),bty='n')
    
    if(plot.trans == TRUE){
      a.nm <- 'ET'
    }else{
      a.nm <- 'Sap'
    }

    mtext(paste0(x.vec.nm,a.nm),side=3,line=0,outer=TRUE)
  }
}
pdf("swc_et_sap.pdf",width = 6,height = 8)
# plot tdr trans####
# rs
plot.de.fit.func(de.trans.tdr.ls,hr.swc.lai.df.1314,plot.trans = TRUE,
                 x.in = 'Rs.w',x.lim=c(0,1000))
mtext(expression(R[s]~(W~m^-2)),side=1,line=3,outer=TRUE)
mtext(expression("Modelled ET"~(kg~m^-2~hr-1)),side=2,line=3,outer=TRUE)
mtext("tdr et",side=3,line=0,outer=TRUE,adj=0)
# vpd
plot.de.fit.func(de.trans.tdr.ls,hr.swc.lai.df.1314,plot.trans = TRUE,
                 x.in = 'VPD',x.lim=c(0,7))
mtext(expression(D[s]~(kPa)),side=1,line=3,outer=TRUE)
mtext(expression("Modelled ET"~(kg~m^-2~hr-1)),side=2,line=3,outer=TRUE)
# swc.tdr
plot.de.fit.func(de.trans.tdr.ls,hr.swc.lai.df.1314,plot.trans = TRUE,
                 x.in = 'swc.tdr',x.lim=c(0,0.35))
mtext(expression(theta),side=1,line=3,outer=TRUE)
mtext(expression("Modelled ET"~(kg~m^-2~hr-1)),side=2,line=3,outer=TRUE)

# plot tdr sap####
# rs
plot.de.fit.func(de.sap.tdr.ls,hr.swc.lai.df.1314,plot.trans = FALSE,
                 x.in = 'Rs.w',x.lim=c(0,1000))
mtext(expression(R[s]~(W~m^-2)),side=1,line=3,outer=TRUE)
mtext(expression("Sapflow"~(kg~m^-2~hr-1)),side=2,line=3,outer=TRUE)
mtext("tdr sap",side=3,line=0,outer=TRUE,adj=0)
# vpd
plot.de.fit.func(de.sap.tdr.ls,hr.swc.lai.df.1314,plot.trans = FALSE,
                 x.in = 'VPD',x.lim=c(0,7))
mtext(expression(D[s]~(kPa)),side=1,line=3,outer=TRUE)
mtext(expression("Sapflow"~(kg~m^-2~hr-1)),side=2,line=3,outer=TRUE)
# swc.tdr
plot.de.fit.func(de.sap.tdr.ls,hr.swc.lai.df.1314,plot.trans = FALSE,
                 x.in = 'swc.tdr',x.lim=c(0,0.35))
mtext(expression(theta),side=1,line=3,outer=TRUE)
mtext(expression("Sapflow"~(kg~m^-2~hr-1)),side=2,line=3,outer=TRUE)

# plot neo trans####
# rs
plot.de.fit.func(de.trans.neu.ls,hr.swc.lai.df.1314,plot.trans = TRUE,
                 x.in = 'Rs.w',x.lim=c(0,1000))
mtext(expression(R[s]~(W~m^-2)),side=1,line=3,outer=TRUE)
mtext(expression("Modelled ET"~(kg~m^-2~hr-1)),side=2,line=3,outer=TRUE)
mtext("neo et",side=3,line=0,outer=TRUE,adj=0)
# vpd
plot.de.fit.func(de.trans.neu.ls,hr.swc.lai.df.1314,plot.trans = TRUE,
                 x.in = 'VPD',x.lim=c(0,7))
mtext(expression(D[s]~(kPa)),side=1,line=3,outer=TRUE)
mtext(expression("Modelled ET"~(kg~m^-2~hr-1)),side=2,line=3,outer=TRUE)
# swc.tdr
plot.de.fit.func(de.trans.neu.ls,hr.swc.lai.df.1314,plot.trans = TRUE,
                 x.in = 'swc',x.lim=c(0,0.35))
mtext(expression(theta),side=1,line=3,outer=TRUE)
mtext(expression("Modelled ET"~(kg~m^-2~hr-1)),side=2,line=3,outer=TRUE)


# plot neu sap####
# rs
plot.de.fit.func(de.sap.neu.ls,hr.swc.lai.df.1314,plot.trans = FALSE,
                 x.in = 'Rs.w',x.lim=c(0,1000))
mtext(expression(R[s]~(W~m^-2)),side=1,line=3,outer=TRUE)
mtext(expression("Sapflow"~(kg~m^-2~hr-1)),side=2,line=3,outer=TRUE)
mtext("neo sap",side=3,line=0,outer=TRUE,adj=0)

# vpd
plot.de.fit.func(de.sap.neu.ls,hr.swc.lai.df.1314,plot.trans = FALSE,
                 x.in = 'VPD',x.lim=c(0,7))
mtext(expression(D[s]~(kPa)),side=1,line=3,outer=TRUE)
mtext(expression("Sapflow"~(kg~m^-2~hr-1)),side=2,line=3,outer=TRUE)
# swc
plot.de.fit.func(de.sap.neu.ls,hr.swc.lai.df.1314,plot.trans = FALSE,
                 x.in = 'swc',x.lim=c(0,0.35))
mtext(expression(theta),side=1,line=3,outer=TRUE)
mtext(expression("Sapflow"~(kg~m^-2~hr-1)),side=2,line=3,outer=TRUE)

par(mfrow=c(1,1),mar=c(5,5,1,1))

plot(swc.theta~swc.tdr,data = hr.swc.lai.df.1314,pch=16,col="darkseagreen",cex=0.5,
     xlab="SWC TDR",ylab='SWC Neotron/Theta')
abline(a=0,b=1)
points(swc~swc.tdr,data = hr.swc.lai.df.1314,pch=16,col="lightskyblue",cex=0.5)
legend("topleft",legend = c('Theta','Neutron'),pch=16,col=c('darkseagreen','lightskyblue'))

dev.off()

# make a plot with DE, nlqr, and data#####
par(mfrow=c(1,3),mar=c(5,5,1,1))
# et.max <- max(hr.swc.lai.df.1314$e,na.rm = TRUE)
# plot par
# smoothScatter(hr.swc.lai.df.1314$trans~hr.swc.lai.df.1314$Rs.w,pch='',cex=0.5,ylim=c(0,0.4),
#               xlab=expression(Solar~radiation~(W~m^-2)),ylab=expression(ET~(kg~m^-2~hr^-1)),
#               colramp = colorRampPalette(c("white", 'grey10')))
plot(hr.swc.lai.df.1314$trans~hr.swc.lai.df.1314$Rs.w,pch=16,cex=0.2,ylim=c(0,0.4),
              xlab=expression(Solar~radiation~(W~m^-2)),ylab=expression(ET~(mm~hr^-1)),
              col = (c("lightskyblue", 'coral')[hr.swc.lai.df.1314$c.treat]))
points(x = 0:1000,y=e.fit.best.trans.ele[1]*par.func(0:1000,e.fit.best.trans.ele[2])*(1-exp(-0.5*1.)),
       type='l',col='red',lty="solid",lwd=2)
points(x = 0:1000,y=e.lai.fit.best.e[1]*par.func(0:1000,e.lai.fit.best.e[2])*(1-exp(-0.5*1.)),
       type='l',col='red',lty="dashed",lwd=2)
points(x = 0:1000,y=e.fit.best.trans.amb[1]*par.func(0:1000,e.fit.best.trans.amb[2])*(1-exp(-0.5*1.)),
       type='l',col='blue',lty="solid",lwd=2)
points(x = 0:1000,y=e.lai.fit.best.a[1]*par.func(0:1000,e.lai.fit.best.a[2])*(1-exp(-0.5*1.)),
       type='l',col='blue',lty="dashed",lwd=2)
lines(x=0:1000,y=coef(fit.q.90.par.a)[[1]] * par.func(0:1000,coef(fit.q.90.par.a)[[2]]),
      col="blue",lty="dotted",lwd=2)
lines(x=0:1000,y=coef(fit.q.90.par.e)[[1]] * par.func(0:1000,coef(fit.q.90.par.e)[[2]]),
      col="red",lty="dotted",lwd=2)
legend("topleft",legend = c('90% NLQR','DE Model','DE Sapflow','E',"A"),
       lty = c('dotted','solid','dashed','solid','solid'),col=c('black','black','black','red','blue'),
       bty='n',ncol = 2)
# plot d
# smoothScatter(hr.swc.lai.df.1314$trans~hr.swc.lai.df.1314$VPD,pch='',cex=0.5,ylim=c(0,0.4),
#               xlab=expression(D~(kPa)),ylab=expression(ET~(kg~m^-2~hr^-1)),
#               colramp = colorRampPalette(c("white", 'grey10')))
plot(hr.swc.lai.df.1314$trans~hr.swc.lai.df.1314$VPD,pch=16,cex=0.2,ylim=c(0,0.4),
     xlab=expression(D~(kPa)),ylab=expression(ET~(mm~hr^-1)),
     col = (c("lightskyblue", 'coral')[hr.swc.lai.df.1314$c.treat]))
points(x = seq(0,8,0.1),y=e.fit.best.trans.ele[1]*(1-exp(-0.5*1.))*
         vpd.func(seq(0,8,0.1),e.fit.best.trans.ele[5],e.fit.best.trans.ele[6],e.fit.best.trans.ele[7]),
       type="l",col='red',lty="solid",lwd=2)
points(x = seq(0,8,0.1),y=e.fit.best.trans.amb[1]*(1-exp(-0.5*1.))*
         vpd.func(seq(0,8,0.1),e.fit.best.trans.amb[5],e.fit.best.trans.amb[6],e.fit.best.trans.amb[7]),
       type="l",col='blue',lty="solid",lwd=2)
points(x = seq(0,8,0.1),y=e.lai.fit.best.e[1]*(1-exp(-0.5*1.))*
         vpd.func(seq(0,8,0.1),e.lai.fit.best.e[5],e.lai.fit.best.e[6],e.lai.fit.best.e[7]),
       type="l",col='red',lty="dashed",lwd=2)
points(x = seq(0,8,0.1),y=e.lai.fit.best.a[1]*(1-exp(-0.5*1.))*
         vpd.func(seq(0,8,0.1),e.lai.fit.best.a[5],e.lai.fit.best.a[6],e.lai.fit.best.a[7]),
       type="l",col='blue',lty="dashed",lwd=2)
lines(x=seq(0,8,0.01),y=coef(fit.q.90.d.a)[[4]] * vpd.func(seq(0,8,0.01),coef(fit.q.90.d.a)[[1]],
                                                   coef(fit.q.90.d.a)[[2]],coef(fit.q.90.d.a)[[3]]),
      col="navy",lty="dotted",lwd=2)
lines(x=seq(0,8,0.01),y=coef(fit.q.90.d.e)[[4]] * vpd.func(seq(0,8,0.01),coef(fit.q.90.d.e)[[1]],
                                                 coef(fit.q.90.d.e)[[2]],coef(fit.q.90.d.e)[[3]]),
      col="red",lty="dotted",lwd=2)
# plot swc
# smoothScatter(hr.swc.lai.df.1314$trans~hr.swc.lai.df.1314$swc.tdr,pch='',cex=0.5,ylim=c(0,0.4),
#               xlab=expression(theta),ylab=expression(ET~(kg~m^-2~hr^-1)),
#               colramp = colorRampPalette(c("white", 'grey10')))
plot(hr.swc.lai.df.1314$trans~hr.swc.lai.df.1314$swc.tdr,pch=16,cex=0.2,ylim=c(0,0.4),
     xlab=expression(theta),ylab=expression(ET~(mm~hr^-1)),
     col = (c("lightskyblue", 'coral')[hr.swc.lai.df.1314$c.treat]))
points(x = seq(0,0.3,0.01),y=e.fit.best.trans.ele[1]*(1-exp(-0.5*1.))*
         swc.func(seq(0,0.3,0.01),e.fit.best.trans.ele[3],e.fit.best.trans.ele[4],1),
       type="l",col='red',lty="solid",lwd=2)
points(x = seq(0,0.3,0.01),y=e.fit.best.trans.amb[1]*(1-exp(-0.5*1.))*
         swc.func(seq(0,0.3,0.01),e.fit.best.trans.amb[3],e.fit.best.trans.amb[4],1),
       type="l",col='blue',lty="solid",lwd=2)
points(x = seq(0,0.3,0.01),y=e.lai.fit.best.e[1]*(1-exp(-0.5*1.))*
         swc.func(seq(0,0.3,0.01),e.lai.fit.best.e[3],e.lai.fit.best.e[4],0.32),
       type="l",col='red',lty="dashed",lwd=2)
points(x = seq(0,0.3,0.01),y=e.lai.fit.best.a[1]*(1-exp(-0.5*1.))*
         swc.func(seq(0,0.3,0.01),e.lai.fit.best.a[3],e.lai.fit.best.a[4],0.32),
       type="l",col='blue',lty="dashed",lwd=2)
lines(x=seq(0,0.5,0.01),y= 0.3*swc.func(seq(0,0.5,0.01),coef(fit.q.90.a)[[1]],swc.min = 0.001,q = 0.32),
      col="navy",lty="dotted",lwd=2)
lines(x=seq(0,0.5,0.01),y= 0.3*swc.func(seq(0,0.5,0.01),coef(fit.q.90.e)[[1]],swc.min = 0.001,q = 0.32),
      col="red",lty="dotted",lwd=2)

plot(trans~VPD,data = hr.swc.lai.df.1314,#[Ring %in% paste0("R",1:5)],
     pch=16,col=c('blue','red')[c.treat],
     # xlab="modelled et (kg m-2 hr-1)",ylab='sapflow (kg m-2 hr-1)',
     ann=FALSE,cex=0.1)
par(mfrow=c(3,2))
par(mar=c(2,2,2,2),oma = c(3,3,1,1))
plot(sap~c(trans),data = hr.swc.lai.df.1314[Ring=='R1'],#[Ring %in% paste0("R",1:5)],
     xlim=c(0,0.4),ylim=c(0,0.4),pch=16,col=c('blue','red')[c.treat],
     # xlab="modelled et (kg m-2 hr-1)",ylab='sapflow (kg m-2 hr-1)',
     ann=FALSE,cex=0.1)
axis(2,at = seq(0,0.4,0.1),labels = seq(0,0.4,0.1))
abline(a=0,b=1,col="grey")
legend('topleft',legend = paste0('R1'))
for(i in 2:6){
  plot(sap~c(trans),data = hr.swc.lai.df.1314[Ring==paste0('R',i)],#[Ring %in% paste0("R",1:5)],
       xlim=c(0,0.4),ylim=c(0,0.4),pch=16,col=c('blue','red')[c.treat],
       xlab="modelled et (kg m-2 hr-1)",ylab='sapflow (kg m-2 hr-1)',
       ann=FALSE,cex=0.1)
  abline(a=0,b=1,col="grey")
  legend('topleft',legend = paste0('R',i))
}
mtext("modelled et (kg m-2 hr-1)",side = 1,outer = TRUE,line = 1.2)
mtext("sapflow (kg m-2 hr-1)",side = 2,outer = TRUE,line = 1.2)
# plot(sap~trans,data = hr.swc.lai.df.1314[Ring == 'R4',],xlim=c(0,0.4),ylim=c(0,0.4))
# abline(a=0,b=1,col="grey")
# plot(sap~LAI, data = hr.swc.lai.df.1314,pch=16,col="grey")
# points(x=seq(0.4,1.6,0.001),y=e.lai.fit.best.a[1]*(1-exp(-0.5*seq(0.4,1.6,0.001))),type="l",col='black',lty="solid")


# 

out.df <- rbind(ele.df,amb.df)
out.df$c.treat <- as.factor(out.df$c.treat)
plot(et~e.predict.lai,data = out.df,pch=16,xlab='modelled',ylab='sap flow',
     cex=0.5,col=c('blue','red')[c.treat])
points(sap~e.predict.lai,data = hr.swc.lai.df.1314,pch=16,col="grey",cex=0.5)
legend('topleft',legend = c('1-exp(-0.5*L)','1'),title = "LAI impact",pch=16,col=c('grey','black'))


plot(sap~e.predict.sap,data = hr.swc.lai.df.1314,pch=16,xlab='modelled',ylab='sap flow',cex=0.5)
points(sap~e.predict.lai,data = hr.swc.lai.df.1314,pch=16,col="grey",cex=0.5)
legend('topleft',legend = c('1-exp(-0.5*L)','1'),title = "LAI impact",pch=16,col=c('grey','black'))
abline(a=0,b=1)
plot(e.predict.lai~e.predict.sap,data = hr.swc.lai.df.1314,
     xlab='LAI impact = 1',ylab='1-exp(-0.5*L)',pch=16,cex=0.8)
abline(a=0,b=1)

summary(step(lm(sap~swc + LAI + PAR + VPD ,data=hr.swc.lai.df.1314),direction="both"))


par(mfrow=c(1,3))
# et.max <- max(hr.swc.lai.df.1314$e,na.rm = TRUE)
plot((et)~Rs.w,data = hr.swc.lai.df.1314,pch=16,col='grey',xlab='Rs (Wm-2)',ylab='ET',ylim=c(0,0.5))
points(x = 0:1000,y=e.fit.best[1]*par.func(0:1000,e.fit.best[2]),type='l')


plot((et)~VPD,data = hr.swc.lai.df.1314,pch=16,col='grey',xlab='D (kPa)',ylab='ET',ylim=c(0,0.5))
points(x = seq(0,8,0.1),y=e.fit.best[1]*vpd.func(seq(0,8,0.1),e.fit.best[5],e.fit.best[6],e.fit.best[7]),type="l")

plot((et)~swc,data = hr.swc.lai.df.1314,pch=16,col='grey',xlab='SWC',ylab='ET',ylim=c(0,0.5))
points(x = seq(0,0.3,0.01),y=e.fit.best[1]*swc.func(seq(0,0.3,0.01),e.fit.best[3],e.fit.best[4],1),type="l")


plot(trans~e.predict,data = hr.swc.lai.df.1314,xlim=c(0,0.3),ylim=c(0,0.3))
abline(a=0,b=1,col="grey")











sub.df <- hr.swc.lai.df.1314[hr.swc.lai.df.1314$PAR > 1 & 
                               hr.swc.lai.df.1314$VPD> 1 & 
                               hr.swc.lai.df.1314$VPD < 2,]

plot(et~swc,data = sub.df,
     col="grey",pch=16,cex=0.5,ylim=c(0,0.4))
summary(lm(et~swc,data = sub.df))


#try log linear ###############
test <- hr.swc.lai.df.1314[hr.swc.lai.df.1314$Rs.w > 100 & 
                             hr.swc.lai.df.1314$VPD >0 & 
                             hr.swc.lai.df.1314$swc >0,]

fit.nls <- nls(log(et) ~ a * log(PAR * 3600) + b * log(VPD) + c * log(swc) + d,
               data = test, start = list(a = 0.9,b = 0.2,c = 0.3,d = 1))

et.nl <- exp(predict(fit.nls))

summary(lm(et.nl~test$et))

fit.nls.no.swc <- nls(log(et) ~ a * log(PAR) + b * log(VPD) + d,
                      data = test, start = list(a = 0.9,b = 0.2,d = 1))

et.nl.no.swc <- exp(predict(fit.nls.no.swc))

summary(lm(et.nl.no.swc~test$et))


# no swc fit#################
whitley.e.no.swc.func <- function(dat,pars){
  
  e.max <- pars[1]
  par <- dat$Rs.w
  k <- pars[2]
  # swc <- dat$swc
  # swc.max <- pars[3]
  # swc.min <- pars[4]
  q <- 1
  vpd <- dat$VPD
  k1 <- pars[3]
  k2 <- pars[4]
  d.max <- pars[5]
  
  e.pred <- e.max * par.func(par,k) * vpd.func(vpd,k1,k2,d.max)
  
  e.sd <- sd(dat$et)
  
  resid.e <- ((e.pred - dat$et)/e.sd)^2
  
  
  resid.e[is.na(resid.e)] <- 0
  
  resid.sum <- sum(resid.e)
  
  return(resid.sum)
}
lower <- c(0.3,100,  0.1,0,1)
upper <- c(1,  1000, 0.8,2,4)
NPmax <- 200
maxiter <- 200

set.seed(1234)

#
library(DEoptim)
e.no.swc.fit <- DEoptim(fn=whitley.e.no.swc.func,lower=lower,upper=upper,
                        dat=hr.swc.lai.df.1314[hr.swc.lai.df.1314$Rs.w > 400,],
                        DEoptim.control(NP = NPmax,itermax=maxiter,trace=T,parallelType = 1,
                                        parVar = list("par.func","vpd.func")))

e.no.swc.fit.best <- unname(e.no.swc.fit$optim$bestmem)

hr.swc.lai.df.1314$e.predict.no.swc <- e.fit.best[1] * par.func(hr.swc.lai.df.1314$Rs.w,e.fit.best[2]) * 
  vpd.func(hr.swc.lai.df.1314$VPD,e.fit.best[3],e.fit.best[4],e.fit.best[5])

plot(et~e.predict.no.swc,data = hr.swc.lai.df.1314)
summary(lm(et~e.predict.no.swc,data = hr.swc.lai.df.1314))

