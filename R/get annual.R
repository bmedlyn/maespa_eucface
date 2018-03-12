maespa.d.df <- readRDS("output/maestraVPD/mastra and sap.rds")
library(lubridate)
maespa.d.df <- maespa.d.df[year(maespa.d.df$Date) %in% seq(2013,2016),]
maespa.d.df$year <- year(maespa.d.df$Date)
maespa.d.df$c_treat <- NA

maespa.d.df$c_treat[maespa.d.df$Ring %in% paste0("R",c(1,4,5))] <-"E"
maespa.d.df$c_treat[maespa.d.df$Ring %in% paste0("R",c(2,3,6))] <-"A"
library(doBy)
maespa.sum.d.df <- summaryBy(GPP + Ra~year + Ring + c_treat,
                         data =maespa.d.df,
                         FUN=sum,keep.names=TRUE)
names(maespa.sum.d.df) <- c( "year","Ring","Treat",
                             "GPP.g.C.m-2.yr-1","Rfoliage.g.C.m-2.yr-1")
write.csv(maespa.sum.d.df,"maespa annual.csv",row.names = F)
