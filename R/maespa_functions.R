make_met <- function(Ring,startDate= NULL,endDate = NULL){
  
  ######################################################################################
  # # choose only the days with lai
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
 
  # ROS weather data
  # "FACE_R4_T1_Rain"
  # instead of ecuface met data, the model uses met data from ROS
  # i have tested rainfall and there is no difference at all
  # other paramteres should be the same
  
  ros15 <- downloadTOA5("ROS_WS_Table15",
                        startDate = startDate,
                        endDate = endDate)
  ros05 <- downloadTOA5("ROS_WS_Table05",
                        startDate = startDate,
                        endDate = endDate)
  
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
  # get real co2 data from hiev
  #CO2data <- downloadTOA5("fcplogg", startDate="2012-9-10", endDate="2015-5-10")
  #"DateTime"    "Concentration.1Min"  

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
  
#Select original Co2data and save for comparition
myvars <- c("DateTime","Base.Concentration","Set.Point.Concentration",
            "Concentration.1Min","Concentration.5Min")

OriginalCO2 <- CO2Data[myvars]

# write.table(summary(OriginalCO2),sprintf("R%summaryOfOriginalCO2.dat",Ring))
  ################################################################################################################
  CO2Data$DateTime <- nearestTimeStep(CO2Data$DateTime, 30, "ceiling")

  sumCO2 <- data.table(CO2Data)[,list(CO2=mean(Concentration.1Min, na.rm=TRUE)),
                                by = DateTime]
  
  # Fill missing values
  library(zoo)
  sumCO2$CO2 <- na.locf(sumCO2$CO2)
  
  #write the CO2 into a RDS file
  saveRDS(sumCO2,sprintf("R%sCO230Min.rds",Ring))
  
  # # final issue!
  # m <- data.frame(DateTime=seq(ymd_hm(paste0(startDate,"00:00")), 
  #                              ymd_hm(paste0(endDate,"23:30")),
  #                              by="30 min"))
  # CaData <- merge(m, sumCO2)
  CaData <- sumCO2
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
run_maespa_eucface <- function(ring,runfolder.path,startDate= NULL,endDate=NULL,hourly.data=FALSE){

  o <- getwd()
  setwd(runfolder.path)
  on.exit(setwd(o))

  # Smoothed LAI
  lais <- sm[[ring]]$LAIsmooth
  # Calculate individual tree leaf areas.
  DATES <- format(sm[[ring]]$Date, "%d/%m/%y")
  
  # set s and e days
  if(is.null(startDate)){
    startDate <- DATES[1]
  } else {
    startDate <- as.Date(startDate)
  }
  
  if(is.null(endDate)){
    endDate <- DATES[length(DATES)]
  } else {
    endDate <- as.Date(endDate)
  }
  
  #make met file
  met <- list()
  met[[ring]] <- make_met(ring,
                          startDate = startDate,
                          endDate = endDate)
  
  #get initial swc from hiev
  swc.df <- downloadTOA5("FACE_R1_B1_SoilVars",
                         startDate = startDate,
                         endDate = endDate)
  
  swc.df <- subset(swc.df,select = c("Date",
                                     "DateTime",
                                     "Theta5_1_Avg","Theta5_2_Avg",
                                     "Theta30_1_Avg","Theta30_2_Avg",
                                     "Theta75_1_Avg","Theta75_2_Avg"))
  swc.df <- swc.df[order(swc.df$DateTime),]
  swc.df$swc.0.30 <- (swc.df$Theta5_1_Avg + swc.df$Theta5_2_Avg + swc.df$Theta30_1_Avg +swc.df$Theta30_2_Avg)/4
  
  swc.df$swc.30.75 <- (swc.df$Theta75_1_Avg + swc.df$Theta75_2_Avg)/2
  
  # swc.day.df <- data.table(swc.df)[,list(swc.30=mean(swc.0.30, na.rm=TRUE),
  #                                        swc.75 = mean(swc.30.75, na.rm=TRUE)),
  #                                  by = Date]
  # 
  # swc.day.df$swc.30 <- swc.day.df$swc.30/100
  # swc.day.df$swc.75 <- swc.day.df$swc.75/100
  
  # Set simulation dates for given time
  replaceNameList("dates","confile.dat", vals=list(startdate=format(as.Date(startDate), "%d/%m/%y"),
                                                   enddate=format(as.Date(endDate), "%d/%m/%y")))
  # set date range
  replacePAR("met.dat", "startdate", "metformat", format(as.Date(startDate), "%d/%m/%y"))
  
  replacePAR("met.dat", "enddate", "metformat", format(as.Date(endDate), "%d/%m/%y"))
  
  if(hourly.data == TRUE) {
    replacePAR("confile.dat", "iohrly","control", newval=1)
    } else {
    replacePAR("confile.dat", "iohrly","control", newval=0)
    }
  # turn on resp simulation
  replacePAR("confile.dat", "ioresp","control", newval=1)

  # change simulation for TUzet model and add parameteres
  replacePAR("confile.dat", "modelgs","model", newval=6)
  # initial swc from Hiev 
  replacePAR("watpars.dat", "initwater","initpars", newval=c(swc.df$swc.0.30[1]/100,swc.df$swc.30.75[1]/100))
  # throughfall from Teresa's draft should be calculated from Tfall and rainfall measurements
  replacePAR("watpars.dat", "throughfall","wattfall", newval = 0.96)
  # Need to guess from throughfall and rainfall
  replacePAR("watpars.dat", "maxstorage","wattfall", newval = 0)
  
  # root propery based on Juan's data set
  # look at warpar.R for details
  replaceNameList("rootpars","watpars.dat", vals=list(rootrad = 0.0001,#m 
                                                      # rootdens = 500000,
                                                      # rootmasstot = 1000,
                                                      # nrootlayer = 9,
                                                      # fracroot = c(0.1,0.001,0.199,0.2,0.1,0.1,0.1,0.1,0.1)
                                                      # rootrad =0.0005,
                                                      rootdens = 5e5, #g m^3
                                                      rootmasstot = root.total[ring],
                                                      nrootlayer = layers.num,
                                                      fracroot = f.vec
                                                      ))
  # plant hydro conductance
  replaceNameList("plantpars","watpars.dat", vals=list(MINLEAFWP = -6,
                                                       MINROOTWP = -5,
                                                       PLANTK = 7 #leaf-specific (total) plant hydraulic conductance (mmol m-2 s-1 MPa-1)
  ))
  
  # key is porefraction which doesn't really change according to Cosby 1984
  replaceNameList("laypars","watpars.dat", vals=list(nlayer=layers.num + 6,
                                                     laythick=diff(depth.v)/100,
                                                     porefrac = c(0.42,0.40),
                                                     # porefrac = c(0.3,0.3),
                                                     fracorganic = c(0.8,0.2,0.1,0.02)
                                                     ))
  
  # key is usestand = 0 which mean only consider target trees
  replaceNameList("watcontrol","watpars.dat", vals=list(keepwet = 0,
                                                        SIMTSOIL = 1,
                                                        reassignrain = 0,
                                                        retfunction = 1,
                                                        equaluptake = 0,
                                                        WSOILMETHOD = 1,
                                                        usemeaset = 0,
                                                        usemeassw = 0,
                                                        USESTAND = 0
                                                        ))
  # par value from duursma 2008
  replaceNameList("soilret","watpars.dat", vals=list(bpar=c(4.26,6.77),
                                                     psie = c(-0.00036,-0.00132),
                                                     ksat = c(79.8,25.2)
                                                     ))
  #gs paramteres for tuzet
  replaceNameList("bbtuz","phy.dat", vals=list(g0 =0,
                                               # g1 = 3.15, #zhou 2013
                                               # sf = 1.82, #from zhou 2013
                                               # psiv=-1.66, #from zhou 2013
                                               g1 = 4.275, #Teresa
                                               sf = 0.82, #from p50 data brendon
                                               psiv=-3.6,#from p50 data brendon
                                               nsides=1,
                                               wleaf=0.02,
                                               gamma=0,
                                               VPDMIN=0.05
                                               ))

  # Toss met data before which we don't have LAI anyway (makes files a bit smaller)
  met[[ring]] <- met[[ring]][met[[ring]]$Date >= min(sm[[ring]]$Date),]
  # write.csv(met[[ring]],"met_ListOfAllVaule.csv")
  metnodate <- subset(met[[ring]], select = -Date)
  #
  #fill missing value
  metnodate$PPT <- na.locf(metnodate$PPT)
  metnodate$PAR <- na.locf(metnodate$PAR)
  metnodate$TAIR <- na.locf(metnodate$TAIR)
  metnodate$RH <- na.locf(metnodate$RH)
  metnodate$CA <- na.locf(metnodate$CA)

  # # s.date <- format(min(met[[ring]]$Date),"%d/%m/%y")
  # # e.date <- format(max(met[[ring]]$Date),"%d/%m/%y")
  # # s.date <- format(as.Date("2014/01/01"),"%d/%m/%y")
  # # e.date <- format(as.Date("2014/01/15"),"%d/%m/%y")
  # 
  # # # place in met.dat
  replacemetdata(metnodate, "met.dat", columns=names(metnodate),
                 newmetfile="met.dat", khrs=48, setdates=TRUE)
  
  # run maespa
  print(sprintf("Ring %s start",ring))
  shell("maespa64.exe")
  print(sprintf("Ring %s finished",ring))
  # read output
#   output[[ring]] <- readdayflux()
#   return(output)
}

eucGPP <- function(hourly.data = FALSE,startDate= NULL,endDate = NULL,rings = 1:6,...){
  time.start <- Sys.time()
  update.tree.f(...)
  update.phy.f(...)
  for (ring in rings){
    
    run_maespa_eucface(ring = ring,
                       runfolder.path = file.path(o,sprintf("Rings/Ring%s",ring),"runfolder/"),
                       hourly.data = hourly.data,
                       startDate= startDate,endDate = endDate)

  }
  time.used <- Sys.time() - time.start
  print(time.used)
  return(time.used)
}

