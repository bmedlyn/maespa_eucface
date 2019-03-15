d.vec <- seq(0,8,0.01)
d.function <- function(d,k1,k2,dmax){
  exp(-k1*(d-dmax)^2/(d+k2))
}

# e.d <- d.function(d.vec,0.23,0.27,1.83)

library(lubridate)
# maespa.df <- readRDS("output/maestraVPD/mastra and sap.rds")
maespa.df.hr <- readRDS("output/maestraVPD/mastra and sap hr.rds")
maespa.df.hr <- maespa.df.hr[year(maespa.df.hr$DateTime) == 2013 & maespa.df.hr$PAR > 0.5,]
maespa.ls <- split(maespa.df.hr,f=maespa.df.hr$Ring)

cr.met.df <- read.csv('data/CR/Jan-Aug 2007 met with timestamp & RH.csv')
cr.sf.df <- read.csv('data/CR/sf jan-aug 2007 -jim.csv')

names(cr.sf.df) <- c('date.time','sf.cm3.hr.1.m.2')

cr.sf.met.df <- merge(cr.sf.df,cr.met.df[c('date.time','PAR','VPD')])
cr.sf.met.df <- cr.sf.met.df[cr.sf.met.df$PAR > 1000,]
cr.sf.met.df <- cr.sf.met.df[cr.sf.met.df$VPD > 0.05,]
cr.sf.met.df <- cr.sf.met.df[cr.sf.met.df$sf.cm3.hr.1.m.2 > 0,]
cr.sf.met.df$sf.norm <- cr.sf.met.df$sf.cm3.hr.1.m.2 / max(cr.sf.met.df$sf.cm3.hr.1.m.2,na.rm = TRUE)
# see <- as.data.frame(see)
# quantile(maespa.df.hr$PAR,tau=.9)
for (i in 1:6){
  # i=3
  maespa.ls[[i]]$e.norm <- maespa.ls[[i]]$trans / max(maespa.ls[[i]]$trans,na.rm=TRUE)
  maespa.ls[[i]]$sap.norm <- maespa.ls[[i]]$sap / max(maespa.ls[[i]]$sap,na.rm=TRUE)
}

maespa.df <- do.call(rbind,maespa.ls)
maespa.df <- maespa.df[maespa.df$trans > 0,]
maespa.df <- maespa.df[maespa.df$PAR > 0.8,]
maespa.df <- maespa.df[maespa.df$Ring == "R3",]
# plot(trans~VPD,data = maespa.df,type='p',col=tran.func("red",0.3),pch=16,cex=0.5)

tran.func <- function(cols,alpha=0.1){
  targe.vec <- col2rgb(cols)
  out.col <- apply(targe.vec,2,FUN = function(x){
    rgb(red = x["red"]/255, green=x["green"]/255, blue=x["blue"]/255,alpha=alpha)})
  return(out.col)
}
library(quantreg)
fit.90.trans <- nlrq(e.norm~d.function(VPD,k1,k2,dmax),
                     data = maespa.df,
                     tau=0.8,start=list(k1=0.2,k2=0.2,dmax=1.5))
fit.90.sap <- nlrq(sap.norm~d.function(VPD,k1,k2,dmax),
                   data = maespa.df,
                   tau=0.8,start=list(k1=0.2,k2=0.2,dmax=1.5))

fit.90.cr <- nlrq(sf.norm~d.function(VPD,k1,k2,dmax),
                   data = cr.sf.met.df,
                   tau=0.8,start=list(k1=0.2,k2=0.2,dmax=1.5))

plot(sf.norm~VPD,data = cr.sf.met.df,type='p',col=tran.func("red",0.3),pch=16,cex=0.5,
     xlab = expression(D~(kPa)),
     ylab = expression(E~(normalized)),
     xlim=c(0,8))
# points(e.d~d.vec,type='l',col="grey",lwd=2)
# 
plot.trans.df <- data.frame(et=predict(fit.90.trans),
                            VPD = maespa.df$VPD)
plot.trans.df <- plot.trans.df[order(plot.trans.df$VPD),]
points(et~VPD,data = plot.trans.df,col="black",type='l',lwd=3)
# 
plot.sap.df <- data.frame(et=predict(fit.90.sap),
                            VPD = maespa.df$VPD)
plot.sap.df <- plot.sap.df[order(plot.sap.df$VPD),]
points(et~VPD,data = plot.sap.df,col="blue",type='l',lwd=3)
# 
plot.cr.df <- data.frame(et=predict(fit.90.cr),
                          VPD = cr.sf.met.df$VPD)
plot.cr.df <- plot.cr.df[order(plot.cr.df$VPD),]
points(et~VPD,data = plot.cr.df,col="red",type='l',lwd=3)
legend('topright',
       legend = c('Modeled','Sapflow','Castlereagh'),
       lty='solid',col=c('black','blue','red'),
       bty='n')
# plot(e.norm~VPD,data = maespa.df)
# points(e.d~d.vec,type='l',col="red")
# points(sap.norm~VPD,data = maespa.df[year(maespa.df$DateTime) == 2013,],col="navy")



# plot.bin.func <- function(x.vec,y.vec,col.plot="red"){
#   # x.vec = maespa.vpd.df$VPD
#   # y.vec = maespa.vpd.df$sap
#   temp.df <- data.frame(x=x.vec,
#                         y=y.vec)
#   
#   temp.df$x.level <- cut(temp.df$x,seq(0,8,0.5),labels = paste0(seq(0.5,8,0.5)))
#   temp.df <- temp.df[is.na(temp.df$x.level) != TRUE,]
#   library(doBy)
#   compare.bin <- summaryBy(y ~ x.level,data =temp.df,
#                            FUN=c(mean,median,sd),na.rm=TRUE)
#   
#   compare.bin$vpd.vec <- as.numeric(as.character(compare.bin$x.level))
#   # compare.bin <- compare.bin[complete.cases(compare.bin),]
#   
#   plot(y.mean~vpd.vec,data =compare.bin,type="b",pch=16,ylim=c(0,1),
#        xlab=" ",#expression("D (kPa)"),
#        ylab = " ",
#        yaxt='n',
#        xaxt='n',
#        col=col.plot)
#   polygon(x = c(compare.bin$vpd.vec,
#                 rev(compare.bin$vpd.vec)),
#           y = c(c(compare.bin$y.mean-compare.bin$y.sd),
#                 rev(compare.bin$y.mean+compare.bin$y.sd)),
#           col = tran.func(col.plot),
#           border = NA)
# }
# 
# par(mar=c(5,5,1,1))
# plot.bin.func(maespa.df$VPD,maespa.df$sap.norm,col.plot = "blue")
# par(new=TRUE)
# plot.bin.func(maespa.df$VPD,maespa.df$e.norm,col.plot = "grey")
# par(new=TRUE)
# plot.bin.func(maespa.vpd.df$VPD,maespa.vpd.df$trans,col.plot = "red")
# axis(1,at=seq(0,8,2),seq(0,8,2))
# mtext(expression(D~(kPa)),side = 1,line = 3)
# 
# axis(2,at=seq(0,1,0.2),seq(0,1,0.2))
# mtext(expression(E~(kg~m^-2~hr^-1)),side = 2,line = 3)
# 
# legend("topleft",legend = c("Obs","MAESPA","MAESPA V-D"),
#        pch=16,col=c("blue","grey","red"),bty='n')

