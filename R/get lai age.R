# FACE_P0017_RA_Litter
litter.files.1 <- downloadHIEv(searchHIEv("FACE_P0017_RA_Litter",startDate = ("2012-01-01"),
                                        uploader_email = "k.crous@uws.edu.au"),stati="CLEANSED")

litter.files.more <- downloadHIEv(searchHIEv(c("FACE_P0017_RA_Litter_2016"),exclude = c(paste0(2016,0,1:8),2017),
                                        uploader_email = "vinod.kumar@uws.edu.au"),stati="RAW")

litter.files <- c(litter.files.1,litter.files.more)
litter.ls <- list()

for(i in 1:length(litter.files)){
  fn <- paste0("download/",litter.files[i])
  firstLine <- str_extract(readLines(fn,n=1), "WEIGHT")
  if (is.na(firstLine) == TRUE){
    litter.ls[[i]] <- read.csv(fn)
  }else{
    litter.ls[[i]] <- read.csv(fn,skip = 1)
  }
  
  names(litter.ls[[i]]) <- tolower(names(litter.ls[[i]]))
  names(litter.ls[[i]]) <- gsub("..g.", "", names(litter.ls[[i]]))
    
  litter.ls[[i]] <- litter.ls[[i]][,c("ring","date","trap","leaf")]
  litter.ls[[i]] <- litter.ls[[i]][,1:4]
}

litter.df <- do.call(rbind,litter.ls)
litter.df$date <- as.Date(as.character(litter.df$date),"%d/%m/%Y")
litter.df$leaf <- as.numeric(litter.df$leaf)
litter.df <- litter.df[complete.cases(litter.df),]

litter.ring.df <- doBy::summaryBy(leaf~date+ring,
                                  data = litter.df,
                                  FUN=mean,na.rm=TRUE)
SLA <- 52.6 * 10^-4 #m2 g-1; from Duursma 2016
Ring.area <- pi*12.5^2
basket.area <- 0.1979 #m2 from hiev
litter.ring.df$litter <- litter.ring.df$leaf.mean * SLA / basket.area 
litter.ring.df$Date <- litter.ring.df$date
# get lai
for (i in 1:6){
  sm[[i]]$ring <- i
}

lai.df <- do.call(rbind,sm)

lai.litter.df <- merge(lai.df,litter.ring.df,by=c("Date","ring"))
lai.litter.df$lai.08 <- lai.litter.df$LAIsmooth - 0.8

lai.litter.df$dLAI <- c(0,diff(lai.litter.df$lai.08))
lai.litter.df$production <- lai.litter.df$dLAI + lai.litter.df$litter
lai.litter.df$production[lai.litter.df$prodction < 0] <- 0
# leaf.litter <- downloadCSV("FACE_RA_P0037_LEAFLITTER_20121009-20140814_L2.csv")
# read litter, lai, and leaf production from Remko's paper
# Issue: the data covered only 2013-2014. Not litter data can be found even on hiev.

# column needed Date, LAI, litter
# leaf.litter <- read.csv("data/leaf production per month.csv")

# leaf.litter <- leaf.litter[,c("Date","treatment","LAI","lit","laprod")]
# leaf.litter$Date <- as.Date(as.character(leaf.litter$Date))
# leaf.litter.amb <- leaf.litter[leaf.litter$treatment == "ambient",]
# leaf.litter.amb$diff.days <- c(NULL,diff(as.Date(as.character(leaf.litter.amb$Date))))
# the interval is ~ 30 days. igorened the difference for simplicity 

lai.litter.df$treat <- NA
lai.litter.df$treat[lai.litter.df$ring %in% c(1,4,5)] <- "E"
lai.litter.df$treat[lai.litter.df$ring %in% c(2,3,6)] <- "A"

# lai.litter.df.treat <- doBy::summaryBy(lai.08 + litter + production ~ Date + treat,
#                                        data = lai.litter.df,FUN=mean,na.rm=TRUE,
#                                        keep.names = TRUE)
lai.litter.df.treat <- doBy::summaryBy(lai.08 + litter + production ~ Date + treat,
                                       data = lai.litter.df,FUN=mean,na.rm=TRUE,
                                       keep.names = TRUE)
leaf.litter.amb <- lai.litter.df.treat[lai.litter.df.treat$treat == "E",]
names(leaf.litter.amb) <- c("Date","treat","LAI","lit","laprod")
# give leaf age class provided initial condition, residential time of leaf, and leaf production
get.leaf.age.func <- function(leaf.litter.amb,x1,x2,t1,t2){
  # # get the total num of obs
  ndays <- length(unique(leaf.litter.amb$Date))
  # # make a empty df to store the result
  # lai.pred.df <- data.frame(LAI = NA,
  #                           ring = rep(1:6,ndays),
  #                           new=NA,
  #                           mature=NA,
  #                           old=NA,
  #                           litter=NA,
  #                           Date=NA)
  # # get obs values for evaluation later
  # lai.pred.df$Date <- as.Date(as.character(leaf.litter.amb$Date))
  # lai.pred.df$obs.lai <- leaf.litter.amb$LAI
  # lai.pred.df$obs.litter <- leaf.litter.amb$lit
  lai.pred.df <- leaf.litter.amb
  lai.pred.df$obs.lai <- leaf.litter.amb$LAI
  lai.pred.df$obs.litter <- leaf.litter.amb$lit
  lai.pred.df <- subset(lai.pred.df, select=-c(LAI,lit))
    
  # needed to input leaf production
  lai.pred.df$new <- leaf.litter.amb$laprod
  lai.pred.df$new[lai.pred.df$new <0] <-  0
  
  # set the initial condition
  lai.pred.df$mature[1] <- x1
  lai.pred.df$old[1] <- x2

  lai.pred.df$mature[1] <- lai.pred.df$new[1] + lai.pred.df$mature[1]
  lai.pred.df$old[1] <- lai.pred.df$old[1]
  lai.pred.df$litter[1] <- lai.pred.df$old[1]/t2
  lai.pred.df$LAI[1] <- lai.pred.df$mature[1] + lai.pred.df$old[1] - lai.pred.df$litter[1]

  # give value to the rest 
  for (i in 1:(ndays-1)){
    lai.pred.df$mature[i+1] <- ((lai.pred.df$new[i+1] + lai.pred.df$mature[i] - lai.pred.df$mature[i]/t1))
    lai.pred.df$old[i+1] <- c(lai.pred.df$mature[i]/t1 + lai.pred.df$old[i]  - lai.pred.df$old[i]/t2)
    lai.pred.df$litter[i+1] <- lai.pred.df$old[i+1]/t2
    
    # if(lai.pred.df$new[i+1] > 0.2){
    #   lai.pred.df$litter[i+1] <- lai.pred.df$old[i+1]
    #   lai.pred.df$old[i+1] <- 0
    # }
    
    lai.pred.df$LAI[i+1] <- lai.pred.df$mature[i+1] + lai.pred.df$old[i+1] - lai.pred.df$litter[i+1]
  }
  
  return(lai.pred.df)
  
}

# function used to fit modelled lai and litter to obs
fit.leaf.age.func <- function(pars,dat){
  #- pull out the parameters from the pars vector
  x1 <- pars[1]
  x2 <- pars[2]
  t1 <- pars[3]
  t2 <- pars[4]
  
  out <-  get.leaf.age.func(dat,x1,x2,t1,t2)

  max.lai <- sd(out$obs.lai)
  max.litter <- sd(out$obs.litter)

  resid.lai <- ((out$LAI - out$obs.lai) / max.lai)^2
  resid.litter <- ((out$litter - out$obs.litter) / max.litter)^2
  
  resid.lai[is.na(resid.lai)] <- 10
  resid.litter[is.na(resid.litter)] <- 10
 
  resid.sum <- sum(resid.lai#,
                   # resid.litter
                   ) 
  
  return(resid.sum)
}

# pars for fitting
lower <- c(0.1,0.1,1, 1) 
upper <- c(2,  2,  12,12)
NPmax <- 200
maxiter <- 100

#- set seed for repeatability
set.seed(1234)

#------------------------------------------------------------------------------------------------------------------
dat = leaf.litter.amb
# DE fit
library(DEoptim)
leaf.age.fit <- DEoptim(fn=fit.leaf.age.func,
                                 lower=lower,upper=upper,
                                 dat=dat,
                                 DEoptim.control(NP = NPmax,itermax=maxiter,trace=T,parallelType = 1,
                                                 parVar = list("get.leaf.age.func","fit.leaf.age.func")))

leaf.age.fit.best <- unname(leaf.age.fit$optim$bestmem)

# make some plots
see <- get.leaf.age.func(leaf.litter.amb,
                         leaf.age.fit.best[1],leaf.age.fit.best[2],
                         leaf.age.fit.best[3],leaf.age.fit.best[4])

see$fraction.old <- see$old / (see$mature + see$old)
see$fraction.mature <- see$mature / (see$mature + see$old)

par(mfrow=c(1,1))
plot(LAI~obs.lai,data = see)
abline(a=0,b=1)
summary(lm(LAI~obs.lai,data = see))

plot(litter~obs.litter,data = see)
abline(a=0,b=1)
summary(lm(litter~obs.litter,data = see))

par(mfrow=c(2,1))
par(mar=c(1,5,5,1))
plot(fraction.old~Date,data = see,type="l",col="brown",lwd=2,ylim=c(0,1),ylab="Fraction",xlab='',
     xaxt='n')
lines(fraction.mature~Date,data = see,col="darkseagreen",lwd=2)
# par(new=TRUE)
# plot(obs.lai~Date,data = see,ylim=c(0.5,2),type="l",lwd=2,lty="dotted",
#      ann=FALSE,
#      axes=FALSE,xlab='',ylab='')
# axis(4,seq(0.5,2,0.5),seq(0.5,2,0.5))
# mtext(expression(LAI~(m^2~m^-2)),side = 4,line = 3)
# legend("topleft",legend = c("Matrure","Old","LAI"),
#        lty=c(1,1,3),
#        col=c("darkseagreen","brown","black"),bty='n')

par(mar=c(5,5,0,1))
plot(LAI~Date,data = see,ylim=c(0,1.5),type="l")
lines(mature~Date,data = see,col="darkseagreen")
lines(old~Date,data = see,col="brown")
points(obs.lai~Date,data = see,col="grey",pch=16)
legend("topleft",legend = c("Matrure","Old","Total","Obs"),
       pch=c(NA,NA,NA,16),lty=c(1,1,1,NA),
       col=c("darkseagreen","brown","black","grey"),bty='n')

plot(obs.litter~Date,data = see,col="green",type="l")
points(litter~Date,data = see)


plot(laprod~Date,data = see,col="green",type="l")
# 
litter.ts <- ts(see$obs.litter,frequency = 12,start = c(2012,11), end = c(2016,12))
litter.deco <- decompose(litter.ts, "multiplicative")

plot(as.ts(litter.deco$trend))
plot(as.ts(litter.deco$random))
plot(as.ts(litter.deco$seasonal))

fit <- arima(AirPassengers, order=c(1,0,0), list(order=c(2,1,0), period=12))
fit.litter.ts <- arima(litter.ts, order=c(1,0,0), list(order=c(2,1,0), period=12))

fore.litter.ts <- predict(fit.litter.ts, n.ahead=24)
ts.plot(litter.ts, fore.litter.ts$pred,col=c(1,3), lty = c(1,1))



library(wavelets)

w <- dwt(see$obs.lai)
