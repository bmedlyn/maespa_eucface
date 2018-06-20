data.both.sap.m.150 <- readRDS("output/vj16/400/mastra and sap.rds")
data.both.sap.p.150 <- readRDS("output/vj16/550/mastra and sap.rds")
data.both.sap.m.150$volRing[data.both.sap.m.150$volRing > 3] <- NA
data.both.sap.p.150$volRing[data.both.sap.p.150$volRing > 3] <- NA
library(lubridate)
data.both.sap.m.150$year <- year(data.both.sap.m.150$Date)
data.both.sap.p.150$year <- year(data.both.sap.p.150$Date)
library(doBy)
data.both.sap.m.sum <- summaryBy(GPP + Trans + LAI + APAR + volRing + TAIR + VPD + PAR~ 
                                   year + Ring,
                                 data = data.both.sap.m.150,
                                 FUN=c(mean,sum),na.rm=TRUE)

data.both.sap.p.sum <- summaryBy(GPP + Trans + LAI + APAR + volRing + TAIR + VPD + PAR~ 
                                   year + Ring,
                                 data = data.both.sap.p.150,
                                 FUN=c(mean,sum),na.rm=TRUE)

data.both.sap.m.annual <- data.both.sap.m.sum[,c("year" , "Ring" , 'GPP.sum' , 'Trans.sum',
                                                 'LAI.mean','APAR.sum','volRing.sum', 
                                                 'TAIR.mean' , 'VPD.mean','PAR.sum')]
names(data.both.sap.m.annual) <- c("year" , "Ring" , 'GPP.sum.400' , 'Trans.sum.400',
                                   'LAI.mean','APAR.sum','volRing.sum', 'TAIR' , 'VPD','PAR')

data.both.sap.p.annual <- data.both.sap.p.sum[,c("year" , "Ring" , 'GPP.sum' , 'Trans.sum')]
names(data.both.sap.p.annual) <- c("year" , "Ring" , 'GPP.sum.550' , 'Trans.sum.550')

data.ca.annual.df <- merge(data.both.sap.p.annual,data.both.sap.m.annual)
data.ca.annual.df$volRing.sum[data.ca.annual.df$year %in% 2015:2016] <- NA

data.ca.annual.df$c.response <- data.ca.annual.df$GPP.sum.550 / data.ca.annual.df$GPP.sum.400
data.ca.annual.df$et.response <- data.ca.annual.df$Trans.sum.550 / data.ca.annual.df$Trans.sum.400 - 1
data.ca.annual.df <- data.ca.annual.df[,c("year","Ring","GPP.sum.550","GPP.sum.400",'c.response',
                                          "Trans.sum.550","Trans.sum.400",'et.response',
                                          "LAI.mean","APAR.sum","volRing.sum",
                                          'VPD','TAIR','PAR')]
data.ca.annual.df$wue.550 <- data.ca.annual.df$GPP.sum.550 / data.ca.annual.df$Trans.sum.550
data.ca.annual.df$wue.400 <- data.ca.annual.df$GPP.sum.400 / data.ca.annual.df$Trans.sum.400
data.ca.annual.df$wue.response <- data.ca.annual.df$wue.550 / data.ca.annual.df$wue.400

data.ca.annual.df$lue.550 <- data.ca.annual.df$GPP.sum.550 / data.ca.annual.df$APAR.sum
data.ca.annual.df$lue.400 <- data.ca.annual.df$GPP.sum.400 / data.ca.annual.df$APAR.sum
data.ca.annual.df$lue.response <- data.ca.annual.df$lue.550 / data.ca.annual.df$lue.400

write.csv(data.ca.annual.df,"ca response.csv",row.names = F)
