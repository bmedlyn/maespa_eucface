make_met <- function(Ring){
  ######################################################################################
  # ROS weather data
  ros15 <- downloadTOA5("ROS_WS_Table15")
  ros05 <- downloadTOA5("ROS_WS_Table05")
  
  ros05_30 <- as.data.frame(dplyr::summarize(group_by(ros05,DateTime=nearestTimeStep(DateTime,30)),
                                             PPFD=mean(PPFD_Avg, na.rm=TRUE),
                                             Tair=mean(AirTC_Avg, na.rm=TRUE),
                                             RH=mean(RH, na.rm=TRUE)))
  ros15_30 <- as.data.frame(dplyr::summarize(group_by(ros15,DateTime=nearestTimeStep(DateTime,30)),
                                             Rain=sum(Rain_mm_Tot, na.rm=TRUE)))
  ros <- merge(ros15_30, ros05_30)

  #replace below zero PAR with zero 
  ros$PPFD[ros$PPFD<0] <- 0

  ######################################################################################
  #get CO2
  #CO2data <- downloadTOA5("fcplogg", startDate="2012-9-10", endDate="2015-5-10")
  #"DateTime"    "Concentration.1Min"  

  fn <- sprintf("R%s_FCPLOGG_R",Ring)
  Rawdata <- downloadTOA5(fn, maxnfiles = 600, rowbind=FALSE)      
  
  #make all wrong format to numeric so that we can rbine
  Rawdata <- lapply(Rawdata, function(x){
    x$WindSpeed <- as.numeric(as.character(x$WindSpeed))
    return(x)
    
  })
  CO2Data <- do.call(rbind, Rawdata)

  #set limits (350-1000) for CA *baddata reports can be found in separet files
  CO2Data$Concentration.1Min[CO2Data$Concentration.1Min > 1000] <- 1000
  CO2Data$Concentration.1Min[CO2Data$Concentration.1Min < 350] <- 350
  
#Select original Co2data and save for comparition
myvars <- c("DateTime","Base.Concentration","Set.Point.Concentration",
            "Concentration.1Min","Concentration.5Min")

OriginalCO2 <- CO2Data[myvars]

write.table(summary(OriginalCO2),sprintf("R%summaryOfOriginalCO2.dat",Ring))
  ################################################################################################################
  CO2Data$DateTime <- nearestTimeStep(CO2Data$DateTime, 30, "ceiling")
  
  sumCO2 <- summarise(group_by(CO2Data,DateTime),
                      CO2 = mean(Concentration.1Min, na.rm=TRUE))
  
  # Fill missing values
  library(zoo)
  sumCO2$CO2 <- na.locf(sumCO2$CO2)
  
  #write the CO2 into a RDS file
  saveRDS(sumCO2,sprintf("R%sCO230Min.rds",Ring))
  
  # final issue!
  m <- data.frame(DateTime=seq(ymd_hm("2012-1-11 00:00"), ymd_hm("2015-5-11 23:30"),
                               by="30 min"))
  CaData <- merge(m, sumCO2)
  
  ######################################################################################
  #merge all the data 
  
  AllData <- merge(CaData,ros,by="DateTime")
  
  ######################################################################################  
  # Make sure there are no gaps!
  # Merge with full timeseries...
  met <- data.frame(DateTime=seq.POSIXt(min(ros$DateTime), max(ros$DateTime), by="30 min"))
  met <- merge(met, AllData, all=TRUE)
  
  
  # RH is 0-1
  met$RH <- met$RH / 100
  
  names(met) <- c("Date","CA","PPT","PAR","TAIR","RH")
  met$Date <- as.Date(met$Date)
  
  return(met)
  }

#########################################################################################
# ring is 1,2,...,6
run_maespa_eucface <- function(ring, endDate=NULL,runfolder.path){
  met <- list()
  output <- list()
  
  met[[ring]] <-  make_met(ring)
  # Smoothed LAI
  lais <- sm[[ring]]$LAIsmooth
  
  setwd(runfolder.path)
  on.exit(setwd(o))
 
  # Calculate individual tree leaf areas.
  DATES <- format(sm[[ring]]$Date, "%d/%m/%y")

  if(is.null(endDate)){
    endDate <- DATES[length(DATES)]
  } else {
    endDate <- format(as.Date(endDate), "%d/%m/%y")
  }
  
  # Set simulation dates
  replaceNameList("dates","confile.dat", vals=list(startdate=DATES[1],
                                                   enddate=endDate))
  if(hourly.data == TRUE) {
    replacePAR("confile.dat", "iohrly","control", newval=1)
    } else {
    replacePAR("confile.dat", "iohrly","control", newval=0)
  }
  
  # Toss met data before which we don't have LAI anyway (makes files a bit smaller)
  met[[ring]] <- met[[ring]][met[[ring]]$Date >= min(sm[[ring]]$Date),]
  # write.csv(met[[ring]],"met_ListOfAllVaule.csv")
  metnodate <- subset(met[[ring]], select = -Date)
  
  #fill missing value
  metnodate$PPT <- na.locf(metnodate$PPT)
  metnodate$PAR <- na.locf(metnodate$PAR)
  metnodate$TAIR <- na.locf(metnodate$TAIR)
  metnodate$RH <- na.locf(metnodate$RH)
  metnodate$CA <- na.locf(metnodate$CA)
  
  # set date range
  replacePAR("met.dat", "startdate", "metformat", format(min(met[[ring]]$Date),"%d/%m/%y"))
  replacePAR("met.dat", "enddate", "metformat", format(max(met[[ring]]$Date),"%d/%m/%y"))
  
  # place in met.dat
  replacemetdata(metnodate, "met.dat", columns=names(metnodate), 
                 newmetfile="met.dat", khrs=48, setdates=TRUE)
  
  # run maespa
  print(sprintf("Ring %s start",ring))
  shell("maespa64.exe")
  
  # read output
#   output[[ring]] <- readdayflux()
#   return(output)
}

eucGPP <- function(...){
  time.start <- Sys.time()
  update.tree.f(...)
  update.phy.f(...)
  for (ring in 1:6){
    
    run_maespa_eucface(ring = ring,runfolder.path = file.path(o,sprintf("Rings/Ring%s",ring),"runfolder/"))

  }
  time.used <- Sys.time() - time.start
  print(time.used)
}

