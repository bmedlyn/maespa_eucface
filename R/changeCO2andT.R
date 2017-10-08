rm(list=ls())
cat("\014")
# the trunck hieght problems still there is ring 6

source("R/load.R")
source("R/warpar.R")
co2.increase <- -150
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
time.used <- eucGPP(startDate = as.Date("2013-12-01"),
                    endDate = as.Date("2015-12-01"),
                    lai.test = test,
                    lai.base = base,
                    # rings = c(2,3,6),
                    rings = c(1,5),
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
data.sap <- subset(Monthly,select=c("date.full","Ring","GPP","Ra","mm","VPD","PAR","LAI","TAIR","Rain","absPAR"))
names(data.sap) <- c("Date","Ring","GPP","Ra","Trans","VPD","PAR","LAI","TAIR","PPT","APAR")


for (i in 1:6){
  data.sap$Ring[which(data.sap$Ring == as.character(i))] <- sprintf("R%i",i)
}

data.sap$Ring <- as.factor(data.sap$Ring)
sap.T <- readRDS("sap_daily.rds")  
sap.T <- subset(sap.T,select = c("Date","Ring","volRing"))

data.both.sap <- merge(data.sap,sap.T,by = intersect(names(data.sap), names(sap.T)))
saveRDS(data.both.sap,paste0("mastra and sap","_",test,"_",base,"_",co2.increase,"_",temp.increase,".rds"))
data.both.sap<- readRDS(paste0("mastra and sap","_",test,"_",base,"_",co2.increase,"_",temp.increase,".rds"))

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

saveRDS(data.both.sap.hr,paste0("mastra and sap hr","_",test,"_",base,"_",co2.increase,"_",temp.increase,".rds"))

# 
data.day.150 <- readRDS("mastra and sap_1_0.8_-150_0.rds")
data.day.150.r4 <- data.day.150[data.day.150$Ring == "R4",]
data.day.150.r4$lue <- data.day.150.r4$GPP / data.day.150.r4$APAR


data.day.0 <- readRDS("mastra and sap.rds")
data.day.0.r4 <- data.day.0[data.day.0$Ring == "R4",]

data.day.0.r4$lue <- data.day.0.r4$GPP / data.day.0.r4$APAR

ratio.vec <- data.day.0.r4$lue / data.day.150.r4$lue
# library(lubridate)
# data.day.0.r4$mon <- month(data.day.0.r4$Date)

pdf("lue for r4.pdf",width = 8,height = 6)
par(mar=c(5,5,1,5))

plot(ratio.vec ~ data.day.0.r4$Date,
     type="l",
     xlab=" ",
     ylab=" ",
     yaxt='n',
     col="coral",
     lwd=2)
axis(2,at=seq(1.1,1.4,0.1),labels = paste0(seq(10,40,10),"%"),
     col = "coral",col.axis = "coral")
mtext("Percentage increase",side=2,adj=0.5,line = 2,col = "coral")
# abline(h=mean(ratio.vec,na.rm = TRUE),
#        lty="dashed",
#        col="coral")
text(as.Date("2013-04-01"),1.4,expression("["*CO[2]*"]"*~(400~to~550~mu*mol~mol^-1*";"~37.5*"%")))
mtext("2013",side=1,adj=0,line = 2)
mtext("2014",side=1,adj=1,line = 2)

par(new = TRUE)

plot(data.day.0.r4$TAIR ~ data.day.0.r4$Date,
     type="l",
     xlab=" ",
     ylab=" ",
     yaxt='n',
     col="grey",
     ann=FALSE,
     axes=FALSE,
     ylim=c(-10,40))
axis(4,at=seq(10,40,10),labels = paste0(seq(10,40,10)))
mtext("Tair (Celsius)",side=4,adj=0.8,line = 2)

# plot LUE
par(mar=c(5,5,1,5),mfrow=c(1,1))
plot( data.day.150.r4$lue ~ data.day.150.r4$Date,
     type="l",
     xlab=" ",
     ylab=expression(LUE~(g~C~MJ^-1)), 
     # yaxt='n',
     col="coral",
     lwd=2)
# axis(2,at=seq(1.1,1.4,0.1),labels = paste0(seq(10,40,10),"%"),
#      col = "coral",col.axis = "coral")
# mtext("Percentage increase",side=2,adj=0.5,line = 2,col = "coral")
# abline(h=mean(ratio.vec,na.rm = TRUE),
#        lty="dashed",
#        col="coral")
# text(as.Date("2013-04-01"),1.4,expression("["*CO[2]*"]"*~(400~to~550~mu*mol~mol^-1*";"~37.5*"%")))
mtext("2013",side=1,adj=0,line = 2)
mtext("2014",side=1,adj=1,line = 2)

data.day.0.r4$iPAR <- data.day.0.r4$PAR *10^6*4.57 
lue.par <- data.day.0.r4$GPP/12*10^6 / data.day.0.r4$iPAR 

plot(lue.par~data.day.0.r4$Date,
     ylab=expression(LUE~(mu*mol~C~mu*mol^-1~PAR)),
     xlab=" ",
     main="Incoming PAR instead of APAR")
plot(lue.par~data.day.0.r4$VPD)
plot(data.day.0.r4$lue~data.day.0.r4$VPD)

plot(data.day.0.r4$APAR/data.day.0.r4$PAR~data.day.0.r4$Date)
# par(new = TRUE)
# 
# plot(data.day.0.r4$TAIR ~ data.day.0.r4$Date,
#      type="l",
#      xlab=" ",
#      ylab=" ",
#      yaxt='n',
#      col="grey",
#      ann=FALSE,
#      axes=FALSE,
#      ylim=c(-10,40))
# axis(4,at=seq(10,40,10),labels = paste0(seq(10,40,10)))
# mtext("Tair (Celsius)",side=4,adj=0.8,line = 2)
dev.off()


