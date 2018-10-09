source('r/load.r')
# get greenness data####
green.df <- downloadCSV("FACE_P0037_RA_CANOPYGREENNESS-FULL_OPEN_L2.dat")
names(green.df) <- c("DateTime","Date","Ring","angle","Green")
# average by ring and date
green.sum.df <- doBy::summaryBy(Green~Date + Ring,
                                data = green.df,FUN = mean,na.rm=TRUE,keep.names = TRUE)

lai.gcc.df <- green.sum.df[complete.cases(green.sum.df),]
lai.gcc.df$gcc.norm <-  (lai.gcc.df$Green - quantile(lai.gcc.df$Green,probs = 0.2,na.rm=T)[[1]]) / 
  (quantile(lai.gcc.df$Green,probs = 0.8)[[1]] - quantile(lai.gcc.df$Green,probs = 0.2,na.rm=T)[[1]])

# function to get the no days from Oct 1####
get.days.func <- function(lai.gcc.df){
  
  lai.gcc.df$ndays <- NA
  df.years <- unique(year(lai.gcc.df$Date))
  temp.ls <- list()
  for (i in seq_along(df.years)){
    s.date <- as.Date(paste0(df.years[i],"-10-01"))
    e.date <- as.Date(paste0(df.years[i] + 1,"-10-01"))
    temp.ls[[i]] <- lai.gcc.df[lai.gcc.df$Date >= as.Date(s.date) & lai.gcc.df$Date < as.Date(e.date),] 
    temp.ls[[i]]$ndays <- as.numeric(temp.ls[[i]]$Date - as.Date(s.date))
  }
  
  out.df <- do.call(rbind,temp.ls)
  return(out.df)
}

lai.gcc.df$Date <- as.Date(lai.gcc.df$Date)
lai.gcc.df <- get.days.func(lai.gcc.df)

euc.sum.df <- readRDS("cache/ecu_aci_sum.rds")
euc.sum.df$Date <- as.Date(euc.sum.df$Date,'%d/%m/%y')
euc.sum.df <- get.days.func(euc.sum.df)
# make the number of days smaller so the fitting doesn't go wild#####
pheno.gcc.func <- function(day.num,a=66,b=7,c=0.01){
  x <- day.num/100
  return(a * x^b * c^x )
}

# pheno.gcc.func <- function(day.num,m1,m2,m3,m4,b1=80,b2=200,amp,base.in){
#   amp * (1/(1+exp(m1+m2*(day.num - b1))) -
#            1/(1+exp(m3+m4*(day.num - b2)))) + base.in
# }

# plot(c(pheno.gcc.func(1:365,
#                     15,0.5,4,0.01,b1=80,b2=200,
#                     0.5))~c(1:365))

# 37; 0.27; 24; 0.082
# Fit for GCC pheno####
fit.ggc <- nls(gcc.norm~pheno.gcc.func(ndays,a,b,0.0005),data = lai.gcc.df,
               start = list(a = 2000,b=9))

# fit.ggc <- nls(gcc.norm~pheno.gcc.func(ndays,m1,m2,m3,m4,b1,b2,amp),data = lai.gcc.df,
#                start = list(m1 = 0.1,m2=0.2,m3=0.01,m4=0.1,
#                             b1=80,b2=200,amp=3))

plot(gcc.norm~(ndays),data = lai.gcc.df)

library(DEoptim)

tar.func <- function(dat,par){
  m1 = par[1]
  m2=par[2]
  m3=par[3]
  m4=par[4]
  b1=par[5]
  b2=par[6]
  amp=par[7]
  base.in = par[8]
  
  
  ndays=dat$ndays
  pheno.pred <- pheno.gcc.func(ndays,m1,m2,m3,m4,b1,b2,amp,base.in)

  res.vec <- sum(((pheno.pred - dat$gcc.norm) / sd(dat$gcc.norm)) ^ 2)
  return(res.vec)
}

# pars are psiv, SF, K, b, and c
lower <- c(1,0.1,1,0.001,50,150,
           0.1,0.1) 
# changed minimum Kmax from 2 to 5
upper <- c(40,0.5,40,0.5,150,250,
           5,5)
NPmax <- 100
maxiter <- 150

#- set seed for repeatability
set.seed(1234)

pheno.fit <- DEoptim(fn=tar.func,lower=lower,upper=upper,
                     dat=lai.gcc.df,
                     DEoptim.control(NP = NPmax,itermax=maxiter,trace=T,parallelType = 1,
                                     parVar = list("pheno.gcc.func")))


pheno.fit.best <- unname(pheno.fit$optim$bestmem)
plot(c(pheno.gcc.func(1:365,
                      1.30,0.122,16.68,0.10,149.6,224.06,1.057,0.2921037))~c(1:365),type='l')

plot(gcc.norm~(ndays),data = lai.gcc.df)

points(c(pheno.gcc.func(1:365,
                      1.30,0.122,16.68,0.10,149.6,224.06,1.057,0.2921037))~c(1:365),type='l',
       lwd=3,col='red')
# plot fitted vc pheno
euc.sum.df$treat <- NA
euc.sum.df$treat[euc.sum.df$Ring %in% c(1,4,5)] <- 'E'
euc.sum.df$treat[euc.sum.df$Ring %in% c(2,3,6)] <- 'A'
euc.sum.df$treat <- as.factor(euc.sum.df$treat)
euc.vc.ls <- split(euc.sum.df,euc.sum.df$treat)

pred.vc.func <- function(in.vec,days.vec=1:365){
  up.q <- quantile(in.vec,probs = 0.8)
  low.q <- quantile(in.vec,probs = 0.2)
  vc.max.min.df <- data.frame(x = days.vec,
                              y = (up.q - low.q) *
                                pheno.gcc.func(days.vec,coef(fit.ggc)[[1]],coef(fit.ggc)[[2]],0.0005) +
                                low.q)
  vc.max.min.df <- vc.max.min.df[order(vc.max.min.df$x),]
  return(vc.max.min.df)
}
plot.vc.fit <- function(euc.sum.df,col.in){
  vc.max.min.df <- pred.vc.func(euc.sum.df)
  lines(vc.max.min.df$x,vc.max.min.df$y, lwd=2,col=col.in)
}

par(mar=c(5,5,1,1))
palette(c('blue','red'))
plot(Vcmax~ndays,data = euc.sum.df,xlim=c(0,365),
     col=treat,pch=16,cex=1,
     xlab='Days since Oct 1',
     ylab = expression(V[cmax]~(mu*mol~m^-2~s^-1)))
plot.vc.fit(euc.vc.ls[[1]]$Vcmax,col.in = 'blue')
plot.vc.fit(euc.vc.ls[[2]]$Vcmax,col.in = 'red')


plot(Jmax~ndays,data = euc.sum.df,xlim=c(0,365),
     col=treat,pch=16,cex=1,
     xlab='Days since Oct 1',
     ylab = expression(J[max]~(mu*mol~m^-2~s^-1)))
plot.vc.fit(euc.vc.ls[[1]]$Jmax,col.in = 'blue')
plot.vc.fit(euc.vc.ls[[2]]$Jmax,col.in = 'red')


euc.vj.df <- data.frame(Vc.a = (pred.vc.func(euc.vc.ls[[1]]$Vcmax)),
                        Vc.e = pred.vc.func(euc.vc.ls[[2]]$Vcmax),
                        J.a = pred.vc.func(euc.vc.ls[[1]]$Jmax),
                        J.e = pred.vc.func(euc.vc.ls[[2]]$Jmax))

euc.4y.df <- data.frame(date = seq.Date(as.Date('2013-01-01'),
                                        as.Date('2016-12-31'),
                                        by='day'))

euc.4y.df$ndays <- yday(euc.4y.df$date) - 91

euc.4y.df$ndays[euc.4y.df$ndays < 0] <- euc.4y.df$ndays[euc.4y.df$ndays < 0] + 365
euc.4y.ls <- split(euc.4y.df,year(euc.4y.df$date))

euc.pred.ls <- list()
for (i in 1:length(euc.4y.ls)){
  euc.pred.ls[[i]] <- data.frame(v.a = pred.vc.func(euc.vc.ls[[1]]$Vcmax,euc.4y.ls[[i]]$ndays)$y,
                                 v.e = pred.vc.func(euc.vc.ls[[2]]$Vcmax,euc.4y.ls[[i]]$ndays)$y,
                                 j.a = pred.vc.func(euc.vc.ls[[1]]$Jmax,euc.4y.ls[[i]]$ndays)$y,
                                 j.e = pred.vc.func(euc.vc.ls[[2]]$Jmax,euc.4y.ls[[i]]$ndays)$y,
                                 date = euc.4y.ls[[i]]$date)
  
}

euc.pred.df <- do.call(rbind,euc.pred.ls)
temp.df <- data.frame(date = unique(floor_date(euc.pred.df$date, "week")))

out.df <- merge(temp.df,euc.pred.df,by='date')
saveRDS(out.df,'cache/euc.vj.rds')
