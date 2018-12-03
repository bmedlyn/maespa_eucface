# input <- list()
# for (i in 1:6){
#   input[[i]] <- ReadInput(i)
# }
# 
# met.df <- input[[1]]
# 
# 
# s.date <- as.POSIXlt(sprintf("%s 00:00",as.Date('2013-01-01')),tz="UTC")
# e.date <- as.POSIXlt(sprintf("%s 23:30",as.Date('2016-12-31')),tz="UTC")
# met.df$DateTime <- rep(seq(s.date,e.date,by="hour"),each=2)
# 
# met.df <- summaryBy(.~DateTime,data = met.df,FUN=mean,keep.names = T)
library(doBy)
library(lubridate)
library(zoo)

maespa.df.hr <- readRDS("output/maestraVPD/mastra and sap hr.rds")
maespa.df.hr$ppfd <- maespa.df.hr$PAR / 3600 * 10^6 * 4.57
maespa.df.hr$Date <- as.Date(maespa.df.hr$DateTime)

swc.50.df <- readRDS('cache/g1 swc.rds')
euc.sum.df <- readRDS("cache/ecu_aci_sum.rds")
euc.sum.df$Ring <- paste0("R",euc.sum.df$Ring)
euc.sum.df$Date <- as.Date(euc.sum.df$Date,'%d/%m/%y')
euc.sum.df$treat <- NA
euc.sum.df$treat[euc.sum.df$Ring %in% paste0('R',c(1,4,5))] <- 'e'
euc.sum.df$treat[euc.sum.df$Ring %in% paste0('R',c(2,3,6))] <- 'a'
vj.sum <- summaryBy(.~treat,data = euc.sum.df,
                    FUN=c(mean,sd))
vj.sum$treat <- as.factor(vj.sum$treat)
plot(Vcmax.mean~treat,data = vj.sum)

temp.df <- merge(maespa.df.hr,swc.50.df,by=c('Date',"Ring"),all=TRUE)
temp.1.df <- merge(temp.df,euc.sum.df,by=c('Date',"Ring"),all=TRUE)
temp.1.df$g1 <- na.locf(temp.1.df$g1)

temp.1.df$Vcmax <- na.locf(temp.1.df$Vcmax,fromLast = FALSE)
temp.1.df$Jmax <- na.locf(temp.1.df$Jmax,fromLast = FALSE)
temp.1.df <- temp.1.df[temp.1.df$Date >= as.Date('2013-01-01') & 
                         temp.1.df$Date <= as.Date('2013-12-31'),]
maespa.df.hr.r1 <- temp.1.df[temp.1.df$Ring == 'R1',]

library(plantecophys)
photo.df <- Photosyn(g1 = 4.3,#maespa.df.hr.r1$g1,#Vcmax=maespa.df.hr.r1$Vcmax,Jmax = maespa.df.hr.r1$Jmax,
                     Vcmax = 90,Jmax = 1.6 * 90,
                     VPD = maespa.df.hr.r1$VPD, Ca = 550,#maespa.df.hr.r1$CA, 
                     PPFD = maespa.df.hr.r1$ppfd, Tleaf = maespa.df.hr.r1$TAIR,
                     gsmodel = c("BBOpti"),
                     
                     alpha = 0.3, theta = 0.4756,
                     EaV = 74189.7435218429, 
                     EdVC = 2e+05, 
                     delsC = 641.989, 
                     
                     EaJ = 39513,
                     EdVJ = 2e+05, 
                     delsJ = 640.2658)

photo.df.a <- Photosyn(g1 = 4.3,#maespa.df.hr.r1$g1,#Vcmax=maespa.df.hr.r1$Vcmax,Jmax = maespa.df.hr.r1$Jmax,
                       Vcmax = 90,Jmax = 1.6 * 90,
                     VPD = maespa.df.hr.r1$VPD, Ca = 400,#maespa.df.hr.r1$CA-150, 
                     PPFD = maespa.df.hr.r1$ppfd, Tleaf = maespa.df.hr.r1$TAIR,
                     gsmodel = c("BBOpti"),
                     
                     alpha = 0.3, theta = 0.4756,
                     EaV = 74189.7435218429, 
                     EdVC = 2e+05, 
                     delsC = 641.989, 
                     
                     EaJ = 39513,
                     EdVJ = 2e+05, 
                     delsJ = 640.2658)

temp.a.df <- photo.df.a[,c('ALEAF','Ac','Aj')]
names(temp.a.df) <- paste0(c('ALEAF','Ac','Aj'),'.amb')

photo.df <- cbind(photo.df,temp.a.df)
photo.df <- photo.df[photo.df$PPFD > 400,]
photo.df$A.inc <- (photo.df$ALEAF / photo.df$ALEAF.amb )
photo.df$Ac.inc <- (photo.df$Ac / photo.df$Ac.amb )
photo.df$Aj.inc <- (photo.df$Aj / photo.df$Aj.amb )

par(mfrow=c(2,1))
par(mar=c(1,5,4,1))
plot(Ac~Tleaf,data = photo.df,pch=16,col='red',
     xlab='',xaxt='n',ylab=expression(A[c]~(mu*mol~m^-2~s^-1)))
points(Ac.amb~Tleaf,data = photo.df,pch=16,col='blue')
legend('topleft',legend = '(a)',bty='n')
par(mar=c(5,5,0,1))
plot(Ac.inc~Tleaf,data = photo.df,pch=16,yaxt='n',
     xlab=expression('T'[air]),ylab = expression(C[a]~response~('%')))
axis(2,at = seq(1,1.6,0.2),labels = c(0,20,40,60))
legend(35,2,legend = format(mean(photo.df$Ac.inc),digits = 3),bty='n')
legend('topleft',legend = '(b)',bty='n')


par(mfrow=c(2,1))
par(mar=c(1,5,4,1))
plot(Aj~Tleaf,data = photo.df,pch=16,col='red',
     xlab='',xaxt='n',ylab=expression(A[j]~(mu*mol~m^-2~s^-1)))
points(Aj.amb~Tleaf,data = photo.df,pch=16,col='blue')
legend('topleft',legend = '(a)',bty='n')
par(mar=c(5,5,0,1))
plot(Aj.inc~Tleaf,data = photo.df,pch=16,yaxt='n',
     ylim=c(1,1.6),
     xlab=expression('T'[air]),ylab = expression(C[a]~response~('%')))
axis(2,at = seq(1,1.6,0.2),labels = c(0,20,40,60))
legend(35,2,legend = format(mean(photo.df$Ac.inc),digits = 3),bty='n')
legend('topleft',legend = '(b)',bty='n')







# 
# plot(Aj~Tleaf,data = photo.df,pch=16,col='red')
# points(Aj.amb~Tleaf,data = photo.df,pch=16,col='blue')
# 
# format(mean(photo.df$Ac.inc),digits = 3)
# plot(Aj.inc~Tleaf,data = photo.df)
# legend(35,1.4,legend = format(mean(photo.df$Aj.inc),digits = 3),bty='n')

# # half year gpp
# data.both.sap.150 <- readRDS("output/150/mastra and sap.rds")
# data.both.sap <- readRDS("output/maestra/mastra and sap.rds")
# all.e.df <- rbind(data.both.sap.150[data.both.sap.150$Ring %in% paste0('R',c(2,3,6)),],
#                   data.both.sap[data.both.sap$Ring %in% paste0('R',c(1,4,5)),])
# all.e.df$year <- year(all.e.df$Date)
# all.a.df <- rbind(data.both.sap.150[data.both.sap.150$Ring %in% paste0('R',c(1,4,5)),],
#                   data.both.sap[data.both.sap$Ring %in% paste0('R',c(2,3,6)),])
# all.a.df$year <- year(all.a.df$Date)
# rm(sd)
# names(all.e.df) <- paste0(names(all.e.df),'.e')
# all.ae.df <- merge(all.e.df,all.a.df[,c('Date','Ring','GPP','Trans')],
#                    by.x=c('Date.e','Ring.e'),by.y=c('Date','Ring'))
# 
# all.ae.df$season <- NA
# all.ae.df$season[month(all.ae.df$Date) %in% c(4,5,6,7,8,9)] <- 'cool'
# all.ae.df$season[month(all.ae.df$Date) %in% c(1,2,3,10,11,12)] <- 'warm'
# 
# 
# ae.season.sum.df <- summaryBy(GPP.e + GPP ~ Ring + season,data = all.ae.df,
#                               FUN=mean,keep.names = T)
# 
# ae.season.sum.df$GPP.e / ae.season.sum.df$GPP
# 
# ae.season.sum.df$GPP.e /sum(ae.season.sum.df$GPP.e)
# ae.season.sum.df$GPP /sum(ae.season.sum.df$GPP)
# sum(ae.season.sum.df$GPP.e) / sum(ae.season.sum.df$GPP)
#  
# see <- (photo.df$Ac / photo.df.a$Ac)
# plot(see,ylim=c(1,1.3))
# mean(see[see >1])
# 
# see <- (photo.df$Aj / photo.df.a$Aj)
# plot(see,ylim=c(1,1.3))
# mean(see[see >1],na.rm=TRUE)

# 
# 
# # 
# # 
# coord_H_D <- read.csv("dendrometers-n-coord2013_for_TEG.csv")
# coord_H_D <- na.omit(coord_H_D)
# 
# #DBH################################################################
# coord_H_D$DBH <- coord_H_D$DIAM01.2013
# coord_H_D$BA <- (coord_H_D$DIAM01.2013/100 / 2)^2 * pi
# 
# library(doBy)
# 
# ba.ring.df <- summaryBy(BA~Ring,data = coord_H_D,
#                         FUN=sum,na.rm=T,keep.names = T)
# 
# ba.ring.df$ba.m2.ha <- ba.ring.df$BA / (12.5^2*pi) * 1e4
# ba.ring.df$Ring <- paste0('R',ba.ring.df$Ring)
# 
# ba.maespa.df <- merge(maespa.df,ba.ring.df)
# 
# 
# see.ring.sum <- read.csv('annual fluxes.csv')
# ring.mean.df <- summaryBy(.~Ring,data = see.ring.sum,
#                           FUN=mean,keep.names = T) 
# ba.maespa.df <- merge(ring.mean.df,ba.ring.df)
# 
# palette(c("brown1","cadetblue1","cadetblue3",
#           "brown2","brown4","deepskyblue"
#           
# ))
# plot(APAR.sum.mean~ba.m2.ha,data = ba.maespa.df,
#      pch=16,col=Ring)
# legend("bottomright",legend = levels(ba.maespa.df$Ring),pch=16,col=palette())
