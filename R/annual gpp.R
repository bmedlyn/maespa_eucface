maespa.df <- readRDS("output/maestraVPD/mastra and sap.rds")
maespa.df.hr <- readRDS("output/maestraVPD/mastra and sap hr.rds")
# plot(trans~sap,data = maespa.df.hr[PAR > 0 & Ring=='R1',],xlim=c(0,0.5))
# abline(a=0,b=1)
sum(maespa.df.hr$trans[maespa.df.hr$PAR >0 & maespa.df.hr$Ring %in% paste0('R',c(2,3,6))])/4/3
sum(maespa.df.hr$sap[maespa.df.hr$PAR >0 & maespa.df.hr$Ring=='R3'])/4

maespa.df$year <- year(maespa.df$Date)
maespa.df$volRing[maespa.df$volRing > 2.5] <- NA
see.flx <- summaryBy(GPP + Ra + Trans + soil.e + volRing + APAR~ year + Ring,data = maespa.df,FUN=sum,na.rm=T)
see.lai <- summaryBy(LAI ~ year + Ring,data = maespa.df,FUN=mean)
see <- merge(see.flx,see.lai)

# see <- see[see$year %in% c(2014,2015,2016),]
see.amb <- see[see$Ring %in% c(paste0("R",c(2,3,6))),]

see$c.treat <- NA
see$c.treat[see$Ring %in% c(paste0("R",c(2,3,6)))] <- "A"
see$c.treat[see$Ring %in% c(paste0("R",c(1,4,5)))] <- "E"


rm(sd)
see.ring.sum <- summaryBy(GPP.sum + Ra.sum + Trans.sum + soil.e.sum + volRing.sum + APAR.sum~ year+Ring,data = see,FUN=mean)
write.csv(see.ring.sum,'annual fluxes.csv',row.names = F)
see.amb.sum <- summaryBy(GPP.sum + Ra.sum + Trans.sum + soil.e.sum + volRing.sum ~ year+c.treat,
                         data = see,FUN=c(mean,sd),na.rm=TRUE)
write.csv(see.amb.sum,'maespa outputs 1316.csv',row.names = F)

see.ring.all.sum <- summaryBy(GPP.sum + Ra.sum + Trans.sum + soil.e.sum + volRing.sum ~ Ring+c.treat,
                         data = see,FUN=c(mean,sd),na.rm=TRUE)

see.amb.sum$c.treat <- as.factor(see.amb.sum$c.treat)
palette(c('blue','red'))
plot(GPP.sum.mean~as.character(year) ,data = see.amb.sum,pch=16,col=c.treat)
legend("topleft",legend = levels(see.amb.sum$c.treat),pch=16,col=palette())




see.amb.sum$GPP.sum.mean*1.4
see.amb.sum[see.amb.sum$c.treat =='A',]
mean(see.amb.sum$GPP.sum.mean)
mean(see[see$Ring %in% c(paste0("R",c(1,4,5))),]$GPP.sum)
mean(see.amb.sum$Trans.sum.mean)
mean(see[see$Ring %in% c(paste0("R",c(1,4,5))),]$Trans.sum)

plot(PAR~Date,dataa= maespa.df[maespa.df$Ring == 'R2',])

mean(see$LAI.mean[see$c.treat == "A"])
mean(see$LAI.mean[see$c.treat == "E"])

palette(rainbow(6))
see$Ring <- as.factor(see$Ring)
plot(GPP.sum~year,data = see,pch=16,col=Ring)
plot(GPP.sum~year,data = see,pch=16,col=Ring)
plot(LAI.mean~year,data = see,pch=16,col=Ring)


palette(c("brown1","coral","brown4",
          # "brown1","brown2","brown4"
          "cadetblue1","cadetblue3","deepskyblue"
))
plot(GPP.sum~year,data = see[see$Ring == 'R1',],type='b',pch=16,col=1,ylim=c(900,1900))
for(i in 2:6){
  # par(new=TRUE)
  points(GPP.sum~year,data = see[see$Ring == sprintf("R%s",i),],type='b',pch=16,col=i)
}

legend('top',legend = levels(see$Ring),col=palette(),pch=16,bty='n',horiz = TRUE)
