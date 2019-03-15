spots <- read.csv(unz("download/Gimeno_spot.zip", "data/Gimeno_spot_Eter_gasExchange6400.csv"))
levels(spots$Date)[levels(spots$Date) == "31/11/2012"] <- "31/10/2012"
spots$Date <- as.Date(as.character(spots$Date),"%d/%m/%Y")
spots <- spots[complete.cases(spots$Date),]
spots$CO2_Treat2 <- droplevels(spots$CO2_Treat2)

spot.sum <- summaryBy(Cond~ Date + Ring + Measurement + Campaign, data = spots,
                      FUM=mean, na.rm=T,keep.names = T)


spot.sum.treat <- summaryBy(Cond ~ Campaign + CO2_Treat2 + Measurement, data = spots,
                            FUN=c(mean,sd), na.rm=T,keep.names = T,id=~ Date)


par(mfrow=c(1,1))
palette(c("brown1","cadetblue1","cadetblue3",
          "brown2","brown4","deepskyblue"
))
plot(Cond~Date,data = spot.sum[spot.sum$Measurement == 'Anet_aft',],pch=16,col=palette()[Ring])
plot(Cond~VpdL,data = spots,pch=16,col=palette()[Ring])

par(mfrow=c(3,1),mar=c(3,5,1,1))
palette(c('blue','red'))
plot(Cond.mean~Date,data = spot.sum.treat[spot.sum.treat$Measurement == 'Anet_aft',],pch=16,col=CO2_Treat2,main = "Aft",
     xlab='')

plot(Cond.mean~Date,data = spot.sum.treat[spot.sum.treat$Measurement == 'Anet_mor',],pch=16,col=CO2_Treat2,main = "Mor",
     xlab='')

library(tidyr)
temp.df <- spread(spot.sum.treat,CO2_Treat2,Cond.mean)
spot.long <- summaryBy(A + E ~ Campaign  ,
                       data = temp.df,FUN=mean,na.rm=T,id=~Date)
spot.long$gs.delta <- with(spot.long,(E.mean - A.mean) / A.mean) *100
plot(gs.delta~ Date,data = spot.long,pch=16,col='grey',ylab="% change of gs")
abline(h=0,lty='dashed')
