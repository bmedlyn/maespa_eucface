

maespa.df.hr <- readRDS("output/maestraVPD/mastra and sap hr.rds")
maespa.df.hr$date <- as.Date(maespa.df.hr$DateTime)
maespa.df.hr <- summaryBy(.~date + hour + Ring,
                          data = maespa.df.hr,
                          FUN=mean,keep.names = TRUE,na.rm=T)

maespa.df.hr <- readRDS("output/maestraVPD/mastra and sap hr.rds")
plot(trans~sap,data = maespa.df.hr,pch=16,xlim=c(0,.15),ylim=c(0,.15))
abline(a=0,b=1,col="grey")
library(lubridate)
# maespa.df.hr$Date <- maespa.df.hr$date
maespa.df.hr$year <- year(maespa.df.hr$Date)
maespa.df.hr$month <- month(maespa.df.hr$Date)
maespa.df.hr$hour <- hour(maespa.df.hr$Date)
maespa.df.hr <- maespa.df.hr[maespa.df.hr$year %in% c(2013),]
maespa.df.hr$sap[maespa.df.hr$sap > 0.5] <- NA
maespa.df.hr$c.treat <- NA
maespa.df.hr$c.treat[maespa.df.hr$Ring %in% c(paste0("R",c(2,3,6)))] <- "A"
maespa.df.hr$c.treat[maespa.df.hr$Ring %in% c(paste0("R",c(1,4,5)))] <- "E"

# see.flx <- summaryBy(GPP + Ra + Trans +soil.e ~ year + Ring,
#                      data = maespa.df.hr,FUN=sum)
rm(sd)
library(doBy)
see.dec <- summaryBy(Photo + trans + sap ~ hour,
                     data = maespa.df.hr[maespa.df.hr$month %in% c(1,2),],
                     FUN=c(mean,sd),na.rm=TRUE)

see.jun <- summaryBy(Photo + trans + sap ~ hour,
                     data = maespa.df.hr[maespa.df.hr$month %in% c(7,8),],
                     FUN=c(mean,sd),na.rm=TRUE)

plot(trans.mean~hour,data = see.dec,type='b',pch=16,col="coral")
points(sap.mean~hour,data = see.dec,type='b',pch=16,col="blue")

plot(trans.mean~hour,data = see.jun,type='b',pch=16,col="coral")
points(sap.mean~hour,data = see.jun,type='b',pch=16,col="blue")

see.dec.ring <- summaryBy(Photo + trans + sap ~ hour+Ring,
                     data = maespa.df.hr[maespa.df.hr$month == 12,],
                     FUN=c(mean,sd),na.rm=TRUE)

see.jun.ring <- summaryBy(Photo + trans + sap ~ hour+Ring,
                     data = maespa.df.hr[maespa.df.hr$month == 6,],
                     FUN=c(mean,sd),na.rm=TRUE)


sap.13 <- summaryBy(Photo + trans + sap ~ year + Ring,
                      data = maespa.df.hr,id=~c.treat,
                      FUN=sum,na.rm=TRUE)



sap.treat <- summaryBy(Photo.sum + trans.sum + sap.sum ~ year + c.treat,
                    data = sap.13,
                    FUN=c(mean,sd),na.rm=TRUE)

plot.hrly.func <- function(see.dec.ring,sap.13,title.in){
  
  par(mfrow=c(3,2),mar=c(5,5,1,1),omi = c(0,0,1,0))
  plot(trans.mean~hour,data = see.dec.ring[see.dec.ring$Ring =='R1',],type='b',pch=16,col="coral",
       ylim=c(0,0.2),ylab=expression(E~(mm~hr^-1)))
  points(sap.mean~hour,data = see.dec.ring[see.dec.ring$Ring =='R1',],type='b',pch=16,col="blue")
  legend("topleft",legend = paste0('R',1),bty='n')
  legend("right",legend = c("Sap Flow","MAESPA"),pch=16,col=c('blue','coral'),bty='n')
  legend("top",legend = c(paste0("MAESPA E = ",format(sap.13$trans.sum[sap.13$Ring == "R1"],
                                                         digits = 3),'mm yr-1'),
                             paste0("SapFlow E = ",format(sap.13$sap.sum[sap.13$Ring == "R1"],
                                                          digits = 3),'mm yr-1')),
         bty='n')
  for(i in 2:6){
      plot(trans.mean~hour,data = see.dec.ring[see.dec.ring$Ring ==paste0("R",i),],type='b',pch=16,col="coral",
           ylim=c(0,0.2),ylab=expression(E~(mm~hr^-1)))
      points(sap.mean~hour,data = see.dec.ring[see.dec.ring$Ring ==paste0("R",i),],type='b',pch=16,col="blue")
      legend("topleft",legend = paste0('R',i),bty='n')
      legend("top",legend = c(paste0("MAESPA E = ",format(sap.13$trans.sum[sap.13$Ring == paste0('R',i)],
                                                             digits = 3),'mm yr-1'),
                                 paste0("SapFlow E = ",format(sap.13$sap.sum[sap.13$Ring == paste0('R',i)],
                                                              digits = 3),'mm yr-1')),bty='n')

  }
mtext(title.in,side=3,line=0,outer = TRUE,adj=0.5)
}
pdf("E hrly pattern.pdf",width = 6,height = 7)
plot.hrly.func(see.dec.ring,sap.13,"summer")
plot.hrly.func(see.jun.ring,sap.13,"winter")
dev.off()

plot(trans.mean~hour,data = see.dec.ring[see.dec.ring$Ring =='R1',],type='b',pch=16,col="coral",ylim=c(0,0.15))
points(sap.mean~hour,data = see.dec.ring[see.dec.ring$Ring =='R1',],type='b',pch=16,col="blue")

plot(trans.mean~hour,data = see.dec.ring[see.dec.ring$Ring =='R2',],type='b',pch=16,col="coral")
points(sap.mean~hour,data = see.dec.ring[see.dec.ring$Ring =='R2',],type='b',pch=16,col="blue")

plot(trans.mean~hour,data = see.dec.ring[see.dec.ring$Ring =='R3',],type='b',pch=16,col="coral")
points(sap.mean~hour,data = see.dec.ring[see.dec.ring$Ring =='R3',],type='b',pch=16,col="blue")

plot(trans.mean~hour,data = see.dec.ring[see.dec.ring$Ring =='R4',],type='b',pch=16,col="coral")
points(sap.mean~hour,data = see.dec.ring[see.dec.ring$Ring =='R4',],type='b',pch=16,col="blue")

plot(trans.mean~hour,data = see.dec.ring[see.dec.ring$Ring =='R5',],type='b',pch=16,col="coral",ylim=c(0,0.2))
points(sap.mean~hour,data = see.dec.ring[see.dec.ring$Ring =='R5',],type='b',pch=16,col="blue")

plot(trans.mean~hour,data = see.dec.ring[see.dec.ring$Ring =='R6',],type='b',pch=16,col="coral")
points(sap.mean~hour,data = see.dec.ring[see.dec.ring$Ring =='R6',],type='b',pch=16,col="blue")


plot(trans.mean~hour,data = see.jun.ring[see.jun.ring$Ring =='R6',],type='b',pch=16,col="coral")
points(sap.mean~hour,data = see.jun.ring[see.jun.ring$Ring =='R6',],type='b',pch=16,col="blue")
# get night time sapflow
sap.night <- maespa.df.hr[maespa.df.hr$PAR == 0,]
sap.night.13 <- summaryBy(Photo + trans + sap ~ year,
                     data = sap.night[sap.night$Ring == 'R1',],
                     FUN=sum,na.rm=TRUE)


# # 
# maespa.df.hr <- readRDS("output/maestra/mastra and sap hr.rds")
plot(trans~VPD,data = maespa.df.hr[maespa.df.hr$Ring == "R1",])
plot(sap~VPD,data = maespa.df.hr[maespa.df.hr$Ring == "R1",],ylim=c(0,0.2))
