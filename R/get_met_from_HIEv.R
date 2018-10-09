make_met <- function(Ring,startDate= NULL,endDate = NULL,fix.ca,CA.in){
  # # choose only the days with lai######################################################################################
  DATES <- sm[[Ring]]$Date
  
  if(is.null(startDate)){
    startDate <- DATES[1]
  } else {
    startDate <- startDate
  }
  
  if(is.null(endDate)){
    endDate <- DATES[length(DATES)]
  } else {
    endDate <- as.Date(endDate)
  }
  
  # ROS weather data#####
  # get rainout shelter rainfall
  ros15 <- downloadTOA5("ROS_WS_Table15",
                        startDate = startDate,
                        endDate = endDate)
  ros15_30 <- as.data.frame(dplyr::summarize(group_by(ros15,DateTime=nearestTimeStep(DateTime,30)),
                                             Rain=sum(Rain_mm_Tot, na.rm=TRUE)))
  
  ros <- ros15_30
  
  #get CO2 par t vpd windspeed######################################################################################
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
  
  #data clean 
  CO2Data$Concentration.1Min[CO2Data$Concentration.1Min > 1000] <- 1000
  CO2Data$Concentration.1Min[CO2Data$Concentration.1Min < 350] <- 350
  CO2Data$DateTime <- nearestTimeStep(CO2Data$DateTime, 30, "ceiling")
  CO2Data$WindSpeed[CO2Data$WindSpeed < 0] <- NA
  CO2Data$IRGA.Pressure[CO2Data$IRGA.Pressure < 900] <- NA
  CO2Data$IRGA.Pressure[CO2Data$IRGA.Pressure > 1100] <- NA
  # get 30 min met
  sumCO2 <- data.table(CO2Data)[,list(CO2=mean(Concentration.1Min, na.rm=TRUE),
                                      PRESS = mean(IRGA.Pressure, na.rm=TRUE),
                                      WindSpeed=mean(WindSpeed, na.rm=TRUE),
                                      PPFD=mean(PPFD, na.rm=TRUE),
                                      Tair=mean(Air.Temp, na.rm=TRUE),
                                      RH=mean(IRGA.Vapor.Pressure/saturate.vp.func(Air.Temp), na.rm=TRUE)
  ),
  by = DateTime]
  
  # there is an issue with R4 T sensor on 2013-12-18
  sumCO2$Tair[as.Date(sumCO2$DateTime) == as.Date('2013-12-28')] <- rep(c(18.56816,18.55167,18.02500,17.30000,
                                                                          17.11167,17.08667,17.86333,19.48667,
                                                                          20.82500,22.13833,23.53000,24.76333,
                                                                          25.90833,27.16667,28.20333,29.24667,
                                                                          29.90167,28.93333,26.88833,24.88000,
                                                                          23.41000,22.63667,21.81833,20.89667),each=2)
  #,Fill,missing,values
  library(zoo)
  sumCO2$CO2 <- na.locf(sumCO2$CO2)
  sumCO2$PPFD[sumCO2$PPFD < 0] <- 0
  sumCO2$PPFD <- na.locf(sumCO2$PPFD)
  sumCO2$WindSpeed <- na.locf(sumCO2$WindSpeed)
  sumCO2$PRESS[sumCO2$PRESS < 900] <- NA
  sumCO2$PRESS[sumCO2$PRESS > 1100] <- NA
  sumCO2$PRESS <- na.locf(sumCO2$PRESS)
  sumCO2$PRESS <- sumCO2$PRESS * 100 #times 100 to convert unit from hPa to Pa
  sumCO2$Tair[sumCO2$Tair>50] <- NA
  sumCO2$Tair <- na.locf(sumCO2$Tair)
  sumCO2$RH[sumCO2$RH>100] <- 100
  sumCO2$RH <- na.locf(sumCO2$RH)
  CaData <- sumCO2
  
  #merge all the data ######################################################################################
  AllData <- merge(CaData,ros,by="DateTime")
  
  # Make sure there are no gaps!######################################################################################  
  # Merge with full timeseries...
  met <- data.frame(DateTime=seq.POSIXt(min(ros$DateTime), max(ros$DateTime), by="30 min"))
  met <- merge(met, AllData, all=TRUE)
  
  met$WindSpeed <- na.locf(met$WindSpeed)
  met$PRESS <- na.locf(met$PRESS)
  # RH is 0-1
  met$RH <- met$RH / 100
  
  names(met) <- c("Date","CA","PRESS","WIND",
                  "PAR","TAIR","RH","PPT")
  met$Date <- as.Date(met$Date)
  
  if(fix.ca==TRUE){met$CA = CA.in}
  
  return(met)
}