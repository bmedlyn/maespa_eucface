# rm(list=ls())
# cat("\014")
# the trunck hieght problems still there is ring 6

source("R/load.R")
source("R/warpar.R")

for (co2.increase in c(-150,150)){
  # co2.increase <- -150
  temp.increase <- 0
  
  if(co2.increase == -150){
    rings <- c(1,4,5)
  }
  if(co2.increase == 150){
    rings=c(2,3,6)
  }
  
  for (i in rings){
    fn <- sprintf("Rings/Ring%s/runfolder/confile.dat",i)
    replaceNameList(namelist="CCSCEN",datfile=fn,
                    vals=list(CO2INC = co2.increase,
                              TINC = temp.increase))
  }
  
  #test - lai sensitivity test
  test <- 1
  # lai in the model = measured - base 
  base <- 0.8
  # chose gs model: 4 is optbb; 6 is tuzet
  gs.model.num <- 4
  vc.vpd <- TRUE
  # vj.ratio
  vj.ratio.test = T
  vj.ratio = 2
  # make sure you want to do this first
  time.used <- eucGPP(startDate = as.Date("2013-01-01"),
                      endDate = as.Date("2017-01-01"),
                      lai.test = test,
                      lai.base = base,
                      rings = rings,
                      model.sel = gs.model.num,
                      hourly.data = TRUE,
                      vc.vpd = vc.vpd,
                      vj.ratio.test = vj.ratio.test,
                      vj.ratio = vj.ratio)
  
  # co2.vec <- c("&CCSCEN","CO2INC = 0","/")
  # 
  # for (i in 1:6){
  #   
  #   tf <- sprintf("Rings/Ring%s/runfolder/confile.dat",i)
  #   
  #   write(co2.vec,tf,append = TRUE, sep = "\t")
  #
  # }
  
  #analysis###################
  # read inputs and outputs
  # met <- list()
  input <- list()
  flux <- list()
  hr.flux <- list()
  for (i in 1:6){
    
    # met[[i]] <- Readmet(i)
    input[[i]] <- ReadInput(i)
    flux[[i]] <- ReadDayFlux(i)
    # hr.flux[[i]] <- ReadHourFlux(i)
    
  }
  
  
  # mean(Data$CA[Data$Ring == "1"],na.rm = 1)
  
  
  # make a df of all in and out puts togethter
  # AllRing <- getAllRings(DayFlux = flux,InputValue = input)
  Data <- getAllData(DayFlux = flux,InputValue = input)
  # Data$VPD.t <- getVPD(Data$RH,Data$TAIR)
  #make the ring total to per m2 ground 
  Monthly <- Data
  Monthly$Date <- as.Date(Data$Date)
  Monthly$LE <- Data$le/(pi*12.5^2) #mol h2o m-2 ground d-1
  Monthly$GPP <- 12*Data$GPP/(pi*12.5^2) #g C m-2 ground d-1
  Monthly$Ra <- 12*Data$Ra/(pi*12.5^2) #g C m-2 ground d-1
  Monthly$absPAR<- Data$absPAR/(pi*12.5^2) #MJ m-2 d-1
  Monthly$Date <- factor(format(Monthly$Date,'%Y-%m'))
  Monthly$mm <- Monthly$LE* 1.8 * 0.01 #mm h2o d-1
  Monthly$date.full <- Data$Date
  
  # sap
  data.sap <- subset(Monthly,select=c("date.full","Ring","GPP","Ra","mm",
                                      "VPD","PAR","LAI","TAIR","Rain","absPAR","CA"))
  names(data.sap) <- c("Date","Ring","GPP","Ra","Trans","VPD","PAR","LAI","TAIR","PPT","APAR","CA")
  
  
  for (i in 1:6){
    data.sap$Ring[which(data.sap$Ring == as.character(i))] <- sprintf("R%i",i)
  }
  
  data.sap$Ring <- as.factor(data.sap$Ring)
  sap.T <- readRDS("sap_daily.rds")  
  sap.T <- subset(sap.T,select = c("Date","Ring","volRing"))
  
  file.nm <- paste0("mastra and sap","_",test,"_",base,"_",co2.increase,"_",temp.increase,".rds")
  
  data.both.sap <- merge(data.sap,sap.T,by = intersect(names(data.sap), names(sap.T)))
  saveRDS(data.both.sap,file.nm)
  # data.both.sap<- readRDS(paste0("mastra and sap","_",test,"_",base,"_",co2.increase,"_",temp.increase,".rds"))
  
#   # move file to output folders####
#   
#   if(identical(vj.ratio.test,FALSE)){
#     file.rename(from=file.nm,
#                 to=file.path("vj16",file.nm))
#   }else{
#     file.rename(from=file.nm,
#                 to=file.path("vj2","maestra",file.nm))
#     
#   }
#   
}

