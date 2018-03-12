rm(list=ls())
cat("\014")
# load and set the model####
source("R/load.R")
source("R/warpar.R")
for (i in 1:6){
  #assign extwind: effciency of wind exponential decline with canopy depth 
  fn <- sprintf("Rings/Ring%s/runfolder/str.dat",i)
  replaceNameList(namelist="aero",datfile=fn,
                  vals=list(extwind = 0.0)
  )
}
#test - lai sensitivity test####
test <- 1.0
# lai in the model = measured - base 
# specify test parameters 
base <- 0.8 # brach and stem area to be substracted from LAI

# vj.ratio###
vj.ratio.test = FALSE
vj.ratio = 1.6
# change co2 and t test ####
co2.increase <-0 #elevated co2 test; ppm
temp.increase <- 0 # temperature increase test; celsius

for ( i in 1:6){
  fn <- sprintf("Rings/Ring%s/runfolder/confile.dat",i)
  replaceNameList(namelist="CCSCEN",datfile=fn,
                  vals=list(CO2INC = co2.increase,
                            TINC = temp.increase))
}

# chose gs model: 3: Leuning; 4 is optbb; 6 is tuzet####
gs.model.num <- 4
# vcmax-D function####
vc.vpd <- TRUE
# fire up the model####
########################################################TRUE
# check all the option above before launch the model####
########################################################
# make sure you want to do this first
time.used <- eucGPP(startDate = as.Date("2013-06-01"),
                    endDate = as.Date("2013-12-31"),
                    lai.test = test,
                    lai.base = base,
                    rings = 1:2,
                    model.sel = gs.model.num,
                    hourly.data = TRUE,
                    vc.vpd = vc.vpd,
                    vj.ratio.test = vj.ratio.test,
                    vj.ratio = vj.ratio)

#analysis###################
# read inputs and outputs
# met <- list()
input <- list()
flux <- list()
hr.flux <- list()
for (i in 1:6){
  input[[i]] <- ReadInput(i)
  flux[[i]] <- ReadDayFlux(i)
  hr.flux[[i]] <- ReadHourFlux(i)
  
}

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
data.sap <- subset(Monthly,select=c("date.full","Ring","GPP","Ra","mm","VPD","PAR","LAI","TAIR","Rain","absPAR"))
names(data.sap) <- c("Date","Ring","GPP","Ra","Trans","VPD","PAR","LAI","TAIR","PPT","APAR")

for (i in 1:6){
  data.sap$Ring[which(data.sap$Ring == as.character(i))] <- sprintf("R%i",i)
}

data.sap$Ring <- as.factor(data.sap$Ring)
sap.T <- readRDS("sap_daily.rds")  
sap.T <- subset(sap.T,select = c("Date","Ring","volRing"))

data.both.sap <- merge(data.sap,sap.T,by = intersect(names(data.sap), names(sap.T)))
saveRDS(data.both.sap,"mastra and sap.rds")
data.both.sap<- readRDS("mastra and sap.rds")

# get hrly data####
#
col.nm <- c("DOY","Tree","Spec","HOUR","hrPAR",
            "hrNIR","hrTHM","hrPs","hrRf","hrRmW",
            "hrLE","LECAN","Gscan","Gbhcan","hrH",
            "TCAN","ALMAX","PSIL","PSILMIN","CI",
            "TAIR","VPD","PAR","ZEN","AZ")

hr.sum <- list()
for (i in 1:6){

  names(hr.flux[[i]]) <- col.nm
  hr.flux[[i]]$time <- ceiling(hr.flux[[i]]$HOUR/2)
  # here we get the ring average and divided by ground area
  # Photo mumol m-2 ground s-1
  # trans l/hr
  # par MJ m-2 hr-1
  # vpd kpa
  # TAIR celsius
  # all the sum need to be divided by 2 as the fluxes are half hourly

  hr.sum[[i]] <- data.table(hr.flux[[i]])[,list(Photo = sum((hrPs+ hrRf) * 1800, na.rm=TRUE) * 10^-6 * 12/ (pi*12.5^2),
                                                NPP = sum((hrPs ) * 1800, na.rm=TRUE) * 10^-6 * 12/ (pi*12.5^2),
                                                # PAR = 4 * mean(PAR, na.rm=TRUE) * 10-6 * 3600,
                                                # hrle is on half-hourly base and is for each tree
                                                trans = sum(hrLE,na.rm = TRUE) * 1800 * 10^-3 * 18 * 10^-3 / (pi*12.5^2) ,
                                                VPD = mean(VPD, na.rm=TRUE),
                                                # TAIR = mean(TAIR, na.rm=TRUE),
                                                psiL =mean(PSIL, na.rm=TRUE)
                                                ),
                                          by = c("DOY","time")]
}
con.ls <- readLines("Rings/Ring1/runfolder/confile.dat")

sd <- gsub("startdate = ","",
           con.ls[grep("startdate", con.ls)])
sd <- gsub("'","",sd)
sd <- as.Date(sd,"%d/%m/%y")

ed <- gsub("enddate = ","",
           con.ls[grep("enddate", con.ls)])
ed <- gsub("'","",ed)
ed <- as.Date(ed,"%d/%m/%y")

s.date <- as.POSIXlt(sprintf("%s 00:00",sd),tz="UTC")
e.date <- as.POSIXlt(sprintf("%s 23:30",ed),tz="UTC")

in.hr.ls <- list()
for (i in 1:6){

  input[[i]]$DateTime <- rep(seq(s.date,e.date,by="hour"),each=2)

  in.hr.ls[[i]] <- data.table(input[[i]])[,list(CA=mean(CA, na.rm=TRUE),
                                                PAR = 3600*mean(PAR, na.rm=TRUE)*10^-6/4.57,
                                                RH = mean(RH, na.rm=TRUE),
                                                TAIR = mean(TAIR, na.rm=TRUE)),
                                          by = c("DateTime")]

}
in.05hr.ls <- list()
for (i in 1:6){
  
  input[[i]]$DateTime <- seq(s.date,e.date,by="30 min")
  
  in.05hr.ls[[i]] <- data.table(input[[i]])[,list(CA=mean(CA, na.rm=TRUE),
                                                PAR = 1800*mean(PAR, na.rm=TRUE)*10^-6/4.57,
                                                RH = mean(RH, na.rm=TRUE),
                                                TAIR = mean(TAIR, na.rm=TRUE)),
                                          by = c("DateTime")]
  
}

in.out.hrly.ls <- list()

for (i in 1:6){

  # input[[i]]$PAR <- input[[i]]$PAR*1800*10^-6/4.57
  hr.sum[[i]]$DateTime <- rep(seq(s.date,e.date,by="1 hour"),each=1)
  in.out.hrly.ls[[i]] <- merge(in.hr.ls[[i]],hr.sum[[i]],by="DateTime")
  in.out.hrly.ls[[i]]$Ring <- sprintf("R%s",i)
}

in.out.hrly.df <- do.call(rbind,in.out.hrly.ls)

in.out.05hrly.ls <- list()

for (i in 1:6){
  
  temp <- data.table(hr.flux[[i]])[,list(Photo = sum((hrPs+ hrRf) * 1800, na.rm=TRUE) * 10^-6 * 12/ (pi*12.5^2),
                                         NPP = sum((hrPs ) * 1800, na.rm=TRUE) * 10^-6 * 12/ (pi*12.5^2),
                                         # hrle is on half-hourly base and is for each tree
                                         trans = sum(hrLE,na.rm = TRUE) * 1800 * 10^-3 * 18 * 10^-3 / (pi*12.5^2) ,
                                         VPD = mean(VPD, na.rm=TRUE),
                                         psiL =mean(PSIL, na.rm=TRUE)
  ),by = c("DOY","HOUR")]
  temp$DateTime <- seq(s.date,e.date,by="30 min")
  in.out.05hrly.ls[[i]] <- merge(in.05hr.ls[[i]],temp,by="DateTime")
  in.out.05hrly.ls[[i]]$Ring <- sprintf("R%s",i)
}
# see <- hr.flux[[1]]
in.out.05hrly.df <- do.call(rbind,in.out.05hrly.ls)

sap.hr <- readRDS("sap_hrly.rds")

sap.hr <- subset(sap.hr,select = c("DateHour","Ring","volRing"))
names(sap.hr) <- c("DateTime","Ring","sap")
data.both.sap.hr <- merge(in.out.hrly.df,sap.hr,by = intersect(names(in.out.hrly.df), names(sap.hr)))

sap.05hr <- readRDS("sap_05hrly.rds")

sap.05hr <- subset(sap.05hr,select = c("halfHour","Ring","volRing"))
names(sap.05hr) <- c("DateTime","Ring","sap")
data.both.sap.05hr <- merge(in.out.05hrly.df,sap.05hr,
                            by = intersect(names(in.out.05hrly.df), names(sap.05hr)),
                            all.x =TRUE)
saveRDS(data.both.sap.hr,"mastra and sap hr.rds")
saveRDS(data.both.sap.05hr,"mastra and sap 05hr.rds")
source("R/compare swc.R")

# move file to output folders####
fn.vec <- c("maespa trans vs hp hrly.pdf",
           "mastra and sap hr.rds",
           "mastra and sap 05hr.rds",
           "all.hr.rds",
            "mastra and sap.rds",
            "maespa trans vs hp.pdf")

if(identical(gs.model.num,4)&identical(vj.ratio.test,FALSE)){

  if(identical(vc.vpd,TRUE)){
    file.rename(from=fn.vec,
                to=file.path("output","maestraVPD",fn.vec))
  }else{
    file.rename(from=fn.vec,
                to=file.path("output","maestra",fn.vec))
  }
}

if(identical(gs.model.num,4)&identical(vj.ratio.test,TRUE)){
  
  if(identical(vc.vpd,TRUE)){
    file.rename(from=fn.vec,
                to=file.path("output","maestraVPDVJ",fn.vec))
  }else{
    file.rename(from=fn.vec,
                to=file.path("output","maestraVJ",fn.vec))
  }
}


if(identical(gs.model.num,6)){
  if(identical(vc.vpd,TRUE)){
    file.rename(from=fn.vec,
                to=file.path("output","maespaVPD",fn.vec))
  }else{
    file.rename(from=fn.vec,
                to=file.path("output","maespa",fn.vec))
    }
}


if(identical(gs.model.num,4)){
    file.rename(from=fn.vec,
                to=file.path("output","leuning",fn.vec))
}