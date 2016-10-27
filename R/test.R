make_met <- function(){
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
  
  #fill missing value
 #ros$Rain <- na.locf(ros$Rain)
 #ros$PPFD <- na.locf(ros$PPFD)
 #ros$Tair <- na.locf(ros$Tair)
 #ros$RH <- na.locf(ros$RH)
  
  #replace below zero PAR with zero 
 ros$PPFD[ros$PPFD<0] <- 0
   
#summary(ros)
 
  ######################################################################################
  #get CO2
  
  library(data.table)
  library(lubridate)
  library(HIEv)
  
  #CO2data <- downloadTOA5("fcplogg", startDate="2012-9-10", endDate="2015-5-10")
  #"DateTime"    "Concentration.1Min"   
  
  Rawdata <- downloadTOA5("R1_FCPLOGG_R", maxnfiles = 6000, rowbind=FALSE)
                          #,
                          #topath = "C:/Jinyan(Jim)/Program/Dropbox/UWS project/MaespaGPPEucFACE")
  
  #make all wrong format to numeric so that we can rbine
  Rawdata <- lapply(Rawdata, function(x){
    x$WindSpeed <- as.numeric(as.character(x$WindSpeed))
    return(x)
    
  })
  CO2Data <- do.call(rbind, Rawdata)

#set limits (350-1000) for CA *baddata reports can be found in separet files
CO2Data$Concentration.1Min[CO2Data$Concentration.1Min > 1000] <- 1000
CO2Data$Concentration.1Min[CO2Data$Concentration.1Min < 350] <- 350

  ###############################################################################################################
  #SOme failed test codes 
  #fileList <- list.files(path="C:/Jinyan(Jim)/Program/Dropbox/UWS project", pattern="FACE_AUTO_R1_FCPLOGG_R_2014")
  #RawData <- sapply(fileList, readTOA5)
  #myvars <- c(DateTime, Concentration.1Min)
  #CO2data <- RawData[myvars]
  #trying to subset
  #CO2Data <- subset(RawData, select = "DateTime", "Concentration.1Min")
  ################################################################################################################
  library(HIEv)
  CO2Data$DateTime <- nearestTimeStep(CO2Data$DateTime, 30, "ceiling")

  #library(stringr)
  #CO2Data$Ring <- str_extract(CO2Data$Source, "R[0-9]{1}")
  
  library(dplyr)
  sumCO2 <- summarise(group_by(CO2Data,DateTime),
                      CO2 = mean(Concentration.1Min, na.rm=TRUE))
  
  # Fill missing values
  library(zoo)
  sumCO2$CO2 <- na.locf(sumCO2$CO2)
  
  #write the CO2 into a csv file
  write.csv(sumCO2,"CO2_r1.csv")
  
  
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
  
  # ... then fill missing values with last non-missing value
  #met$PPFD <- na.locf(met$PPFD)
  #met$Tair <- na.locf(met$Tair)
 # met$RH <- na.locf(met$RH)
 # met$Ring <- na.locf(met$Ring)
 # met$CO2 <- na.locf(met$CO2)
  
  # RH is 0-1
  met$RH <- met$RH / 100
  
  names(met) <- c("Date","CA","PPT","PAR","TAIR","RH")
  met$Date <- as.Date(met$Date)
  
  return(met)
  }
#########################################################################################

# Now we make a matrix of leaf areas for each date.
get_leafarea <- function(LAI, stocking=800){
  
  stocking <- stocking/10000
  LAtree <- round(LAI/stocking,2)
  
  return(LAtree)
}


# ring is 1,2,...,6
run_maespa_eucface <- function(ring, stocking=200, endDate=NULL){
  
  # Smoothed LAI
  lais <- sm[[1]]$LAIsmooth
  
  setwd(runfolder)
  on.exit(setwd(o))
  
#   # First set up the file.
#   r <- randomstand(LAI=mean(lais), height=25, cwcl=1, ALAC=0.5, stocking=stocking,
#                    edge=10, plotsize=c(50,50), crownshape="PARA")
  
  
  # Calculate individual tree leaf areas.####
  #calculatation of crown distribution based on allometric relationship
  #read data
  coord_H_D <- read.csv("C:/Jinyan(Jim)/Program/Dropbox/UWS project/EucData/dendrometers-n-coord2013_for_TEG.csv")
  
  #LA is given by the allometric relationship from Limousin et al. 2009
  coord_H_D$LA <- 9.1*10^(-2)*coord_H_D$DBH^1.875 
  
  #the ratio of LA of each individual is given by ring
  #subset the data by ring
  coord_H_D_byRing <- lapply(c(1:6),function(x){
    
    y <- coord_H_D[which(coord_H_D$Ring == x),]
    return(y)
    
  })
  
  for (i in 1:6){
    
    coord_H_D_byRing[[i]]$ratio <- coord_H_D_byRing[[i]]$LA/sum(coord_H_D_byRing[[i]]$LA)
  }
  #
  treela <- coord_H_D_byRing[[1]]$ratio*sm[[1]]
  ndays <- length(treela)
  ntrees <- length(coord_H_D_byRing[[1]]$ratio)
  
  m <- matrix(rep(treela, each=ntrees), ncol=ndays)
  DATES <- format(sm[[1]]$Date, "%d/%m/%y")
  
  if(is.null(endDate)){
    endDate <- DATES[length(DATES)]
  } else {
    endDate <- format(as.Date(endDate), "%d/%m/%y")
  }
  
  replaceNameList("indivlarea", "trees.dat", vals=list(nodates=ndays,
                                                       dates=DATES,
                                                      values=m))


  # Set simulation dates#####
  replaceNameList("dates","confile.dat", vals=list(startdate=DATES[1],
                                                   enddate=endDate))
  
  
  # Toss met data before which we don't have LAI anyway (makes files a bit smaller)
  met <- met[met$Date >= min(sm[[1]]$Date),]
  write.csv(met,"met_ListOfAllVaule.csv")
  metnodate <- subset(met, select = -Date)
  
  #fill missing value
  metnodate$PPT <- na.locf(metnodate$PPT)
  metnodate$PAR <- na.locf(metnodate$PAR)
  metnodate$TAIR <- na.locf(metnodate$TAIR)
  metnodate$RH <- na.locf(metnodate$RH)
  metnodate$CA <- na.locf(metnodate$CA)
  
  # set date range
  replacePAR("met.dat", "startdate", "metformat", format(min(met$Date),"%d/%m/%y"))
  replacePAR("met.dat", "enddate", "metformat", format(max(met$Date),"%d/%m/%y"))
  
  # place in met.dat
  replacemetdata(metnodate, "met.dat", columns=names(metnodate), 
                 newmetfile="met.dat", khrs=48, setdates=TRUE)
  
  # run maespa
  shell("maespa64.exe")
  
  # read output
  d <- readdayflux()
  # save(d, file="gpp_ring1.RData")
  
}





convert_to_gpp <- function(x, stocking=200){
  
  da <- summaryBy(. ~ DOY, FUN=mean, data=x, keep.names=TRUE)
  
  gpp <- da$totPs * 12 * stocking * 10^-4
  
  df <- data.frame(Date=sm[[1]]$Date[1:nrow(da)], GPP=gpp) #, LAI=lais)
  
  return(df)
}



