# 70080
startDate = "2013-01-01"
endDate = "2016-12-31"

ros15 <- downloadTOA5("ROS_WS_Table15",
                      startDate = startDate,
                      endDate = endDate)

ros15_30 <- as.data.frame(dplyr::summarize(group_by(ros15,DateTime=nearestTimeStep(DateTime,30)),
                                           Rain=sum(Rain_mm_Tot, na.rm=TRUE)))

CaData.ls <- list()

for (Ring in 1:6){

  fn <- sprintf("R%s_FCPLOGG_R",Ring)
  Rawdata <- downloadTOA5(fn, 
                          maxnfiles = 600, 
                          rowbind=FALSE,
                          startDate = startDate,
                          endDate = endDate)      
  
  #make all wrong format to numeric so that we can rbine
  Rawdata <- lapply(Rawdata, function(x){
    x$WindSpeed <- as.numeric(as.character(x$WindSpeed))
    return(x)
    
  })
  CO2Data <- do.call(rbind, Rawdata)
  
  #set limits (350-1000) for CA *baddata reports can be found in separet files
  CO2Data$Concentration.1Min[CO2Data$Concentration.1Min > 1000] <- 1000
  CO2Data$Concentration.1Min[CO2Data$Concentration.1Min < 350] <- 350
  
  CO2Data$DateTime <- nearestTimeStep(CO2Data$DateTime, 30, "ceiling")
  
  CO2Data$WindSpeed[CO2Data$WindSpeed < 0] <- NA
  
  CO2Data$IRGA.Pressure[CO2Data$IRGA.Pressure < 900] <- NA
  CO2Data$IRGA.Pressure[CO2Data$IRGA.Pressure > 1100] <- NA
  
  sumCO2 <- data.table(CO2Data)[,list(CO2=mean(Concentration.1Min, na.rm=TRUE),
                                      PRESS = mean(IRGA.Pressure, na.rm=TRUE),
                                      WindSpeed=mean(WindSpeed, na.rm=TRUE),
                                      PPFD=mean(PPFD, na.rm=TRUE),
                                      Tair=mean(Air.Temp, na.rm=TRUE),
                                      RH=mean(IRGA.Vapor.Pressure/saturate.vp.func(Air.Temp),na.rm=TRUE)
                                      
  ),
  by = DateTime]
  
  date.df <- data.frame(DateTime = 
                          seq.POSIXt(as.POSIXct('2013-01-01',tz = "UTC"),
                                     as.POSIXct('2016-12-31 23:30:00',tz = "UTC"),'30 min'))
  
  sumCO2 <- merge(date.df, sumCO2, all=TRUE)
  
  library(zoo)

  sumCO2$CO2 <- na.locf(sumCO2$CO2)
  sumCO2$WindSpeed[sumCO2$WindSpeed < 0] <- NA
  sumCO2$WindSpeed <- na.locf(sumCO2$WindSpeed)
  sumCO2$PPFD[sumCO2$PPFD < 0] <- 0
  sumCO2$PPFD <- na.locf(sumCO2$PPFD)
  sumCO2$PRESS <- na.locf(sumCO2$PRESS)
  sumCO2$PRESS <- sumCO2$PRESS * 100 #times 100 to convert unit from hPa to Pa
  # 
  sumCO2$CO2 <- na.locf(sumCO2$CO2,fromLast = TRUE)
  sumCO2$WindSpeed <- na.locf(sumCO2$WindSpeed,fromLast = TRUE)
  sumCO2$PPFD <- na.locf(sumCO2$PPFD,fromLast = TRUE)
  sumCO2$PRESS <- na.locf(sumCO2$PRESS,fromLast = TRUE)
  
  CaData.ls[[Ring]] <- sumCO2
  
  CaData.ls[[Ring]]$Ring <- Ring
}

ca.df <- do.call(rbind,CaData.ls)

ca.df$treat <- NA
ca.df$treat[ca.df$Ring %in% c(2,3,6)] <- 'A'
ca.df$treat[ca.df$Ring %in% c(1,4,5)] <- 'E'

# ca.df <- ca.df[as.Date(ca.df$DateTime) >= as.Date('2013-01-01') &
#                  as.Date(ca.df$DateTime) <= as.Date('2016-12-31'),]
ca.df <- ca.df[order(ca.df$DateTime),]

ca.ppt.df <- merge(ca.df,ros15_30)

# date.df <- data.frame(DateTime = rep(seq.POSIXt(
#   as.POSIXct('2013-01-01',tz = "UTC"),
#   as.POSIXct('2016-12-31 23:30:00',tz = "UTC"),'30 min'),each=6),
#   Ring = 1:6)

# ca.ppt.df.all <- merge(date.df,ca.ppt.df,all=TRUE,by=c('DateTime','Ring'))

ca.by.treat.df <- summaryBy(.~DateTime + treat,
                            data = ca.ppt.df, FUN = median,keep.names = TRUE,
                            na.rm=TRUE)
# ca.by.treat.df$treat <- droplevels(ca.by.treat.df$treat)
tem.df <- tidyr::spread(ca.by.treat.df,treat,CO2)

met.df <- summaryBy(.~ DateTime,
                    data = tem.df, 
                    FUN = mean,
                    keep.names = TRUE,
                    na.rm=TRUE)

met.full.date <- data.frame(DateTime=seq.POSIXt(as.POSIXct('2013-01-01',tz = "UTC"),
                                      as.POSIXct('2016-12-31 23:30:00',tz = "UTC"), by="30 min"))

met <- merge(met.full.date, met.df, all=TRUE)
na.days <- unique(as.Date(setdiff(as.character(met.full.date$DateTime), as.character(met.df$DateTime))))

for (i in seq_along(na.days)){
  
  met[as.Date(met$DateTime) == na.days[i],2:10] <- 
    subset(met[as.Date(met$DateTime) == as.Date('2015-07-21'),], select = -c(DateTime))
}

# RH is 0-1
met$RH <- met$RH / 100
met <- subset(met,select = -c(Ring))
names(met) <- c("Date","PRESS","WIND","PAR",
                   "TAIR","RH","PPT","Ca.A",'Ca.E')

met$Date <- as.Date(met$Date)
met$TAIR[met$TAIR > 50] <- NA
met$TAIR <- zoo::na.locf(met$TAIR)
met$RH[met$RH > 1] <- 1
met$RH <- zoo::na.locf(met$RH)

saveRDS(met,'cache/ca.df.rds')
