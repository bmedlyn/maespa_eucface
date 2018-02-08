rm(list=ls())
cat("\014")
# the trunck hieght problems still there is ring 6

source("R/load.R")
source("R/warpar.R")

co2.increase <- 0
temp.increase <- 0

for ( i in 1:6){
  fn <- sprintf("Rings/Ring%s/runfolder/confile.dat",i)
  replaceNameList(namelist="CCSCEN",datfile=fn,
                  vals=list(CO2INC = co2.increase,
                            TINC = temp.increase))
}


                
#test - lai sensitivity test
test <- 1
# lai in the model = measured - base 
base <- 0.8

# make sure you want to do this first
time.used <- eucGPP(startDate = as.Date("2013-01-01"),
                    endDate = as.Date("2013-12-31"),
                    lai.test = test,
                    lai.base = base,
                    rings = 4,
                    hourly.data = TRUE)


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
  hr.flux[[i]] <- ReadHourFlux(i)
  
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
data.sap <- subset(Monthly,select=c("date.full","Ring","GPP","Ra","mm","VPD","PAR","LAI","TAIR","Rain"))
names(data.sap) <- c("Date","Ring","GPP","Ra","Trans","VPD","PAR","LAI","TAIR","PPT")

for (i in 1:6){
  data.sap$Ring[which(data.sap$Ring == as.character(i))] <- sprintf("R%i",i)
}

data.sap$Ring <- as.factor(data.sap$Ring)
sap.T <- readRDS("sap_daily.rds")  
sap.T <- subset(sap.T,select = c("Date","Ring","volRing"))

data.both.sap <- merge(data.sap,sap.T,by = intersect(names(data.sap), names(sap.T)))
saveRDS(data.both.sap,paste0("mastra and sap","_",test,"_",base,".rds"))
data.both.sap<- readRDS(paste0("mastra and sap","_",test,"_",base,".rds"))

# get hrly data

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
  hr.sum[[i]] <- data.table(hr.flux[[i]])[,list(Photo = sum(hrPs + hrRf, na.rm=TRUE) / (pi*12.5^2) / 2,
                                                # PAR = 4 * mean(PAR, na.rm=TRUE) * 10-6 * 3600,
                                                # hrle is on half-hourly base and is for each tree
                                                trans = sum(hrLE,na.rm = TRUE) * 1800 * 10^-3 / (pi*12.5^2) * 1.8 * 0.01,
                                                VPD = mean(VPD, na.rm=TRUE)
                                                # TAIR = mean(TAIR, na.rm=TRUE)
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

in.out.hrly.ls <- list()

for (i in 1:6){
  
  # input[[i]]$PAR <- input[[i]]$PAR*1800*10^-6/4.57
  hr.sum[[i]]$DateTime <- rep(seq(s.date,e.date,by="1 hour"),each=1)
  in.out.hrly.ls[[i]] <- merge(in.hr.ls[[i]],hr.sum[[i]],by="DateTime")
  in.out.hrly.ls[[i]]$Ring <- sprintf("R%s",i)
}

in.out.hrly.df <- do.call(rbind,in.out.hrly.ls)

sap.hr <- readRDS("sap_hrly.rds")  

sap.hr <- subset(sap.hr,select = c("DateHour","Ring","volRing"))
names(sap.hr) <- c("DateTime","Ring","sap")
data.both.sap.hr <- merge(in.out.hrly.df,sap.hr,by = intersect(names(in.out.hrly.df), names(sap.hr)))

saveRDS(data.both.sap.hr,paste0("mastra and sap hr","_",test,"_",base,".rds"))


# read data
df.ctrl.day <- readRDS("mastra and sap_1_0.8.rds")
df.ctrl.hr <- readRDS("mastra and sap hr_1_0.8.rds")

df.1.0.day <- readRDS("mastra and sap_1_0.rds")
df.1.0.hr <- readRDS("mastra and sap hr_1_0.rds")

df.0.8.0.8.day <- readRDS("mastra and sap_0.8_0.8.rds")
df.0.8.0.8.hr <- readRDS("mastra and sap hr_0.8_0.8.rds")

df.1.2.0.8.day <- readRDS("mastra and sap_1.2_0.8.rds")
df.1.2.0.8.hr <- readRDS("mastra and sap hr_1.2_0.8.rds")

df.1.0.8.150.day <- readRDS("mastra and sap_1_0.8_150_0.rds")
df.1.0.8.150.hr <- readRDS("mastra and sap hr_1_0.8_150_0.rds")

# sum(df.ctrl.day$GPP[df.ctrl.day$Ring == "R4"])
# sum(df.0.8.0.8.day$GPP[df.0.8.0.8.day$Ring == "R4"])
# sum(df.1.2.0.8.day$GPP[df.1.2.0.8.day$Ring == "R4"])
# sum(df.1.0.day$GPP[df.1.0.day$Ring == "R4"])
# 
# 
# sum(df.ctrl.day$Trans[df.ctrl.day$Ring == "R4"])
# sum(df.0.8.0.8.day$Trans[df.0.8.0.8.day$Ring == "R4"])
# sum(df.1.2.0.8.day$Trans[df.1.2.0.8.day$Ring == "R4"])
# sum(df.1.0.day$Trans[df.1.0.day$Ring == "R4"])

#
pdf("change of lai.pdf",width = 10,height = 8)
par(mar=c(4,5,1,1),mfrow=c(3,1))
gpp.vec <- data.frame(control = sum(df.ctrl.day$GPP[df.ctrl.day$Ring == "R4"]),
                      lai.de.20 = sum(df.0.8.0.8.day$GPP[df.0.8.0.8.day$Ring == "R4"]),
                      lai.in.20 = sum(df.1.2.0.8.day$GPP[df.1.2.0.8.day$Ring == "R4"]),
                      lai.plus.0.8 = sum(df.1.0.day$GPP[df.1.0.day$Ring == "R4"]))
barplot(as.matrix(gpp.vec),
        ylab=expression(GPP~(g~C~m^-2~mon^-1)),
        beside=TRUE,
        names.arg = c("Control","LAI -20%","LAI+20%","LAI +0.8"),
        col=c("navy","lightskyblue","coral","brown"),
        border = NA,
        space = c(1,0.1,0,0),
        main = "2013-01-01 to 2013-02-01",
        cex.axis = 1)

# transpiration
trans.vec <- data.frame(hp = sum(df.ctrl.day$volRing[df.ctrl.day$Ring == "R4"]),
                        control = sum(df.ctrl.day$Trans[df.ctrl.day$Ring == "R4"]),
                        trans.de.20 = sum(df.0.8.0.8.day$Trans[df.0.8.0.8.day$Ring == "R4"]),
                        trans.in.20 = sum(df.1.2.0.8.day$Trans[df.1.2.0.8.day$Ring == "R4"]),
                        trans.plus.0.8 = sum(df.1.0.day$Trans[df.1.0.day$Ring == "R4"]))

barplot(as.matrix(trans.vec),
        ylab=expression(Transpiration~(mm~mon^-1)),
        beside=TRUE,
        names.arg = c("Measured","Control","LAI -20%","LAI+20%","LAI +0.8"),
        col=c("darkseagreen","navy","lightskyblue","coral","brown"),
        border = NA,
        space = c(0.1,0.1,0.1,0,0),
        # main = "2013-01-01 to 2013-02-01",
        cex.axis = 1)
# ring 4 with +150 Ca
# sum(df.ctrl.day$GPP[df.ctrl.day$Ring == "R4"],na.rm=TRUE)
# 
# sum(df.ctrl.day$Trans[df.ctrl.day$Ring == "R4"],na.rm=TRUE)

df.ctrl.day$lue <- df.ctrl.day$GPP / df.ctrl.day$APAR

# sum(df.1.0.8.150.day$GPP[df.1.0.8.150.day$Ring == "R4"],na.rm=TRUE)
# 
# sum(df.1.0.8.150.day$Trans[df.1.0.8.150.day$Ring == "R4"],na.rm=TRUE)

df.1.0.8.150.day$lue <- df.1.0.8.150.day$GPP/df.1.0.8.150.day$APAR

# mean(df.1.0.8.150.day$lue ,na.rm = TRUE)
# mean(df.ctrl.day$lue ,na.rm = TRUE)



# resp
resp.vec <- data.frame(control = sum(df.ctrl.day$Ra[df.ctrl.day$Ring == "R4"]),
                        trans.de.20 = sum(df.0.8.0.8.day$Ra[df.0.8.0.8.day$Ring == "R4"]),
                        trans.in.20 = sum(df.1.2.0.8.day$Ra[df.1.2.0.8.day$Ring == "R4"]),
                        trans.plus.0.8 = sum(df.1.0.day$Ra[df.1.0.day$Ring == "R4"]))

nce.vec <- gpp.vec - resp.vec
barplot(as.matrix(resp.vec),
        ylab=expression(Foliage~respiration~(g~C~yr^-1)),
        beside=TRUE,
        names.arg = c("Control","LAI -20%","LAI+20%","LAI +0.8"),
        col=c("navy","lightskyblue","coral","brown"),
        border = NA,
        space = c(0.1,0.1,0,0),
        # main = "2013-01-01 to 2013-02-01",
        cex.axis = 1)

dev.off()
