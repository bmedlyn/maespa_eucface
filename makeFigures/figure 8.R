library(lubridate)
maespa.df.hr <- readRDS("output/maestraVPD/mastra and sap hr.rds")
maespa.df.hr <- maespa.df.hr[year(maespa.df.hr$DateTime) == 2013 & maespa.df.hr$PAR > 0.5,]
maespa.ls <- split(maespa.df.hr,f=maespa.df.hr$Ring)
# 
cr.met.df <- read.csv('data/CR/Jan-Aug 2007 met with timestamp & RH.csv')
cr.sf.df <- read.csv('data/CR/sf jan-aug 2007 -jim.csv')

names(cr.sf.df) <- c('date.time','sf.cm3.hr.1.m.2')

cr.sf.met.df <- merge(cr.sf.df,cr.met.df[c('date.time','PAR','VPD')])
cr.sf.met.df <- cr.sf.met.df[cr.sf.met.df$PAR > 1000,]
cr.sf.met.df <- cr.sf.met.df[cr.sf.met.df$VPD > 0.05,]
cr.sf.met.df <- cr.sf.met.df[cr.sf.met.df$sf.cm3.hr.1.m.2 > 0,]
cr.sf.met.df$sf.norm <- cr.sf.met.df$sf.cm3.hr.1.m.2 / max(cr.sf.met.df$sf.cm3.hr.1.m.2,na.rm = TRUE)

for (i in 1:6){
  maespa.ls[[i]]$e.norm <- maespa.ls[[i]]$trans / max(maespa.ls[[i]]$trans,na.rm=TRUE)
  maespa.ls[[i]]$sap.norm <- maespa.ls[[i]]$sap / max(maespa.ls[[i]]$sap,na.rm=TRUE)
}

maespa.df <- do.call(rbind,maespa.ls)
maespa.df <- maespa.df[maespa.df$trans > 0,]
maespa.df <- maespa.df[maespa.df$PAR > 0.8,]
maespa.df <- maespa.df[maespa.df$Ring == "R3",]

tran.func <- function(cols,alpha=0.1){
  targe.vec <- col2rgb(cols)
  out.col <- apply(targe.vec,2,FUN = function(x){
    rgb(red = x["red"]/255, green=x["green"]/255, blue=x["blue"]/255,alpha=alpha)})
  return(out.col)
}
library(mgcv)
fit.90.trans <- gam(e.norm~s(VPD,k=4),
                     data = maespa.df)
fit.90.sap <- gam(sap.norm~s(VPD,k=4),
                  data = maespa.df)

fit.90.cr <- gam(sf.norm~s(VPD,k=4),
                  data = cr.sf.met.df)

par(mfrow=c(2,2))
par(mar=c(5,5,1,1))
plot(sf.norm~VPD,data = cr.sf.met.df,type='p',col=tran.func("red",0.3),pch=16,cex=0.5,
     xlab = expression(D~(kPa)),
     ylab = expression(E~(normalized)),
     xlim=c(0,8))
cr.sf.met.df$sf.pred <- predict(fit.90.cr)
lines(sf.pred~VPD,data = cr.sf.met.df[order(data = cr.sf.met.df$VPD),],lwd=3,col='red')
legend('topleft',legend = '(a)',bty='n')
plot(sap.norm~(VPD),data = maespa.df,type='p',col=tran.func("coral",0.3),pch=16,cex=0.5,
     xlab = expression(D~(kPa)),
     ylab = expression(E~(normalized)),
     xlim=c(0,8))
maespa.df$sf.pred <- predict(fit.90.sap)
lines(sf.pred~VPD,data = maespa.df[order(maespa.df$VPD),],lwd=3,col='coral')
legend('topleft',legend = '(b)',bty='n')
plot(e.norm~(VPD),data = maespa.df,type='p',col=tran.func("navy",0.3),pch=16,cex=0.5,
     xlab = expression(D~(kPa)),
     ylab = expression(E~(normalized)),
     xlim=c(0,8))
maespa.df$e.pred <- predict(fit.90.trans)
lines(e.pred~VPD,data = maespa.df[order(maespa.df$VPD),],lwd=3,col='blue')
legend('topleft',legend = '(c)',bty='n')

plot(1000,data = maespa.df,type='p',col=tran.func("blue",0.3),pch=16,cex=0.5,
     xlab = '',
     ylab = '',
     ann=F,axes=F,
     xlim=c(0,8))
legend('topleft',legend = c('Castlereagh','Sapflow','MAESPA'),
       pch=16,col=c('red','coral','blue'),bty='n')

