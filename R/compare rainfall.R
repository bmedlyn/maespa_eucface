d.start <- as.Date("2012-10-01")
d.end <- as.Date("2013-10-01")
  
#get ros rain 
rain.ros.df <- downloadTOA5("ROS_WS_Table15",
                            startDate = d.start,
                            endDate = d.end)

rain.ros.df.sum <- summaryBy(Rain_mm_Tot~Date,
                             data = rain.ros.df,
                             FUN=sum,
                             na.rm=TRUE)

names(rain.ros.df.sum)[2] <- "rain_mm_ros"

# get euc r4 rain
rain.df <- downloadTOA5("FACE_R4_T1_Rain",
                        startDate = d.start,
                        endDate = d.end)

rain.df.sum <- summaryBy(Rain_mm_Tot~Date,
                         data = rain.df,
                         FUN=sum,
                         na.rm=TRUE)

# get through fall
rain.under.df <- downloadTOA5("FACE_R4_B1_AirVars",
                              startDate = d.start,
                              endDate = d.end)



rain.under.df <- subset(rain.under.df,select = c(Date,TrghFlow_mm_Tot))

rain.under.df.sum <- summaryBy(TrghFlow_mm_Tot~Date,
                               data = rain.under.df,
                               FUN=sum,
                               na.rm=TRUE)

throu.df <- merge(rain.df.sum,rain.under.df.sum,by="Date")

rain.all.df <- merge(throu.df,rain.ros.df.sum,by="Date")


# 

plot(rain.all.df[,2]~rain.all.df$Date,
     type="s",
     col="cyan3",
     ylim=c(0,120),
     xlab="Date",
     ylab="rainfall/throughfall mm")
par(new=1)
plot(rain.all.df[,3]~rain.all.df$Date,
     type="s",
     col="darkgoldenrod1",
     ylim=c(0,120),
     xlab="Date",
     ylab="rainfall/throughfall mm")
par(new=1)
plot(rain.all.df[,4]~rain.all.df$Date,
     type="s",
     col="blue4",
     ylim=c(0,120),
     xlab="Date",
     ylab="rainfall/throughfall mm")


legend("topright",
       legend = c("rainfall R4","throughfall R4","rainfall ros"),
       col=c("cyan3","darkgoldenrod1","blue4"),
       lty=1,
       bty='n')

plot(rain.all.df$Rain_mm_Tot.sum~rain.all.df$rain_mm_ros,
     pch=16,
     col="cyan3",
     ylim=c(0,120),
     xlim=c(0,120),
     xlab="rainfall ros",
     ylab="rainfall r4")

abline(a=0,b=1)


plot(rain.all.df$TrghFlow_mm_Tot.sum/rain.all.df$Rain_mm_Tot.sum~rain.all.df$Date,
     pch=16,
     col="cyan3",
     ylim=c(0,1),
     xlab="Date",
     ylab="Rate of throughfall")


plot(rain.all.df$rain_mm_ros - rain.all.df$Rain_mm_Tot.sum~rain.all.df$Date,
     pch=16,
     col="cyan3",
     ylim=c(-10,10),
     xlab="Date",
     ylab="ROS-R4 rainfall")

