rm(list=ls())
cat("\014")
# the trunck hieght problems still there is ring 6
source("R/load.R")
source("R/warpar.R")
#test - lai sensitivity test
test <- 1
# lai in the model = measured - base 
base <- 0.8

# make sure you want to do this first
time.used <- eucGPP(startDate = as.Date("2013-01-01"),
                    endDate = as.Date("2014-01-01"),
                    lai.test = test,
                    lai.base = base,
                    rings = 1:6,
                    hourly.data = TRUE)

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
saveRDS(data.both.sap,"mastra and sap.rds")
data.both.sap<- readRDS("mastra and sap.rds")

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

saveRDS(data.both.sap.hr,"mastra and sap hr.rds")

# with(data.both.sap.hr,plot(trans~sap,xlim=c(0,0.4),ylim=c(0,0.4)))
# 
# fit.see <- lm(trans~sap,data=data.both.sap.hr)
# summary(fit.see)
# abline(a=0,b=1,col="red")
# abline(fit.see)

source("R/compare swc.R")



# #get monthly total
# Months <- split(Monthly, Monthly$Ring)
# 
# sum <- lapply(Months,function(x){
# 
#     y <- data.table(x)[,list(GPP = sum(GPP, na.rm=TRUE),CA =mean(CA,na.rm=TRUE),
#                              absPAR = sum(absPAR, na.rm=TRUE),
#                              LAI = mean(LAI, na.rm=TRUE),LE =sum(LE,na.rm=TRUE),
#                              PAR = sum(PAR,na.rm=TRUE),GOL = sum(GOL,na.rm=TRUE),
#                              dL=sum(dL,na.rm=TRUE)),by = Date]
#   return(y)
# })
# 
# for (i in 1:6){sum[[i]]$Ring <- i
# sum[[i]]$Month <- as.Date(as.yearmon(sum[[i]]$Date, "%Y-%m"))}
# 
# #seperate E and A rings
# # E: 145 A :236
# Elevated_Sum <- rbind(sum[[1]],sum[[4]])
# Elevated_Sum <- rbind(Elevated_Sum,sum[[5]])
# 
# Ambient_Sum <- rbind(sum[[2]],sum[[3]])
# Ambient_Sum <- rbind(Ambient_Sum,sum[[6]])
# 
# Ambient_Sum$CO2Level <- "Ambient"
# Elevated_Sum$CO2Level <- "Elevated"
# E_A <- rbind(Elevated_Sum,Ambient_Sum)
# E_A$Month <- as.Date(as.yearmon(E_A$Date,"%Y-%m"))
# E_A$CO2Level <- as.factor(E_A$CO2Level)
# 
# # get average by treatment
# sumE <- data.table(E_A[E_A$CO2Level == "Elevated",])[,list(GPP = mean(GPP, na.rm=TRUE),
#                                                            GOL = mean(GOL, na.rm=TRUE),
#                                                            LAI = mean(LAI, na.rm=TRUE),
#                                                            absPAR = mean(absPAR, na.rm=TRUE),
#                                                            LE =mean(LE,na.rm=TRUE),
#                                                            PAR = mean(PAR,na.rm=TRUE),
#                                                            CO2Level = CO2Level[1],
#                                                            CA = mean(CA, na.rm=TRUE),
#                                                            dL=mean(dL,na.rm=TRUE)),by = Date]
# 
# 
# sumA <- data.table(E_A[E_A$CO2Level == "Ambient", ])[,list(GPP = mean(GPP, na.rm=TRUE),
#                                                            GOL = mean(GOL, na.rm=TRUE),
#                                                            LAI = mean(LAI, na.rm=TRUE),
#                                                            absPAR = mean(absPAR, na.rm=TRUE),
#                                                            LE =mean(LE,na.rm=TRUE),
#                                                            PAR = mean(PAR,na.rm=TRUE),
#                                                            CO2Level = CO2Level[2],
#                                                            CA = mean(CA, na.rm=TRUE),
#                                                            dL=mean(dL,na.rm=TRUE)),by = Date]
# sumEA <-rbind(sumA,sumE)
# sumEA$Month <- as.Date(as.yearmon(sumEA$Date, "%Y-%m"))
# sumEA$fAPAR <- sumEA$absPAR/sumEA$PAR
# sumEA$LUE <- sumEA$GPP/sumEA$absPAR
# 
# # statistics of the results
# EA <- summarySE(data=E_A, measurevar="GPP", groupvars=c("CO2Level","Date"))
# EA$Month <- as.Date(as.yearmon(EA$Date, "%Y-%m"))
# EA$Lci <- EA$GPP-EA$ci
# EA$Hci <- EA$GPP+EA$ci
# # get the co2 effect of gpp   
# GPP.diff <- EA[which(EA$CO2Level == "Ambient"),]
# library(reshape)
# GPP.diff <- rename(GPP.diff,c(GPP="GPP_Ambient"))
# GPP.diff <- subset(GPP.diff, select=c("Date", "GPP_Ambient","Month"))
# GPP.diff$GPP_Elevated <- EA$GPP[EA$CO2Level == "Elevated"]
# GPP.diff$Difference <- GPP.diff$GPP_Elevated-GPP.diff$GPP_Ambient
# GPP.diff$Ratio <- (GPP.diff$GPP_Elevated-GPP.diff$GPP_Ambient)/GPP.diff$GPP_Ambient
# 
# # #this need to be changed. need read directly from cloud
# # lprod<-read.csv("leaf production per month.csv")
# # lprod$lprodInC <- lprod$laprod*0.5*100^2/52.6 #g C m-2
# # LD <- data.frame(lprod$Date,lprod$lprodInC,lprod$treatment)
# # LD <- LD[order(LD$lprod.treatment),]
# # write.csv(LD,"LD.csv")
# # GD <- data.frame(sumEA$Month,sumEA$GPP,sumEA$CO2Level)
# # GD1214 <- GD[as.Date(GD$sumEA.Month) > as.Date("2012-10-31")&as.Date(GD$sumEA.Month) < as.Date("2014-07-3"),]
# # write.csv(GD1214,"GD.csv")
# # 
# # GLD<-read.csv("GD LD interaction.csv")
# # GLD$Date <- as.Date(GLD$lprod.Date, "%d/%m/%Y")
# 
# #get total of 2013/6 to 2014/6
# sumEA$Month <- as.Date(as.yearmon(sumEA$Date, "%Y-%m"))
# sum.201314 <- sumEA[sumEA$Month >= as.Date("2013-06-1") & sumEA$Month < as.Date("2014-06-1"),]
# 
# sum.201314 <- data.table(sum.201314)[,list(GPP = sum(GPP, na.rm=TRUE),
#                                            LE = 1.8 * 0.01 * sum(LE,na.rm=TRUE), #to get mol to mm
#                                            absPAR = sum(absPAR,na.rm=TRUE)),
#                                      by = CO2Level]
# sum.201314$WUE <- sum.201314$GPP / sum.201314$LE
# 
# # saveRDS(sum.201314,file = file.path("output",sprintf("sum of 2013 to 2014-laiBase %s.rds",lai.base)))
# # readRDS(file.path("output",sprintf("sum of 2013 to 2014-laiBase 0.8.rds")))
# # readRDS(file.path("output",sprintf("sum of 2013 to 2014-laiBase 0.rds")))
# 
# 
# data.both.sap <- data.both.sap[data.both.sap$Date < as.Date("2014-09-30"),]
# data.both.sap$t.level <- cut(data.both.sap$TAIR,breaks = c(5,15,25,35))
# 
# # pdf("MAESPA and heat pulse.pdf")
# # par(mar=c(5,5,2,2))
# # plot(data.both.sap$Trans~data.both.sap$volRing,
# #      xlim=c(0,4),
# #      ylim=c(0,4),
# #      pch=16,
# #      cex=0.8,
# #      xlab=expression(Sap~flow~from~heat~pulse~(L~d^-1)),
# #      ylab=expression(Sap~flow~MAESPA~(L~d^-1)),
# #      col=data.both.sap$t.level)
# # abline(a=0,b=1)
# # 
# # legend("bottomright",legend = levels(data.both.sap$t.level),
# #        col=unique(data.both.sap$t.level),
# #        pch=16,
# #        bty='n')
# # 
# # 
# # plot(data.both.sap$Trans[data.both.sap$Ring!="R1"]~data.both.sap$volRing[data.both.sap$Ring!="R1"],
# #      xlim=c(0,4),
# #      ylim=c(0,4),
# #      pch=16,
# #      cex=0.8,
# #      xlab=expression(Sap~flow~from~heat~pulse~(L~d^-1)),
# #      ylab=expression(Sap~flow~MAESPA~(L~d^-1)),
# #      col=data.both.sap$t.level[data.both.sap$Ring!="R1"])
# # abline(a=0,b=1)
# # fit.sap <- lm(data.both.sap$Trans[data.both.sap$Ring!="R1"]~data.both.sap$volRing[data.both.sap$Ring!="R1"])
# # abline(fit.sap,col="navy",lty="dashed")
# # adj.r.sqrt <- summary(fit.sap)$adj.r.squared
# # mylabel <- bquote(italic(R)^2 == .(format(adj.r.sqrt, digits = 2)))
# # text(x = 3.5, y = 3, labels = mylabel)
# # 
# # legend("bottomright",legend = levels(data.both.sap$t.level),
# #        col=unique(data.both.sap$t.level),
# #        pch=16,
# #        bty='n')
# # 
# # # plot(data.both.sap$Trans[data.both.sap$Ring!="R1"]~data.both.sap$VPD[data.both.sap$Ring!="R1"],
# # #      ylim=c(0,4),
# # #      pch=16,
# # #      cex=0.8,
# # #      xlab="VPD (kPa)",
# # #      ylab=expression(Sap~flow~MAESPA~(L~d^-1)),
# # #      col=data.both.sap$Ring[data.both.sap$Ring!="R1"])
# # # 
# # # legend("bottomright",legend = paste0(unique(data.both.sap$Ring)),
# # #        col=unique(data.both.sap$Ring),
# # #        pch=16,
# # #        bty='n')
# # # 
# # # plot(data.both.sap$Trans[data.both.sap$Ring!="R1"]~data.both.sap$PAR[data.both.sap$Ring!="R1"],
# # #      ylim=c(0,4),
# # #      pch=16,
# # #      cex=0.8,
# # #      xlab=expression(PAR~(MJ~m^-2~d^-1)),
# # #      ylab=expression(Sap~flow~MAESPA~(L~d^-1)),
# # #      col=data.both.sap$Ring[data.both.sap$Ring!="R1"])
# # # 
# # # legend("bottomright",legend = paste0(unique(data.both.sap$Ring)),
# # #        col=unique(data.both.sap$Ring),
# # #        pch=16,
# # #        bty='n')
# # 
# # 
# # plot(data.both.sap$Trans[data.both.sap$Ring!="R1"]~data.both.sap$VPD[data.both.sap$Ring!="R1"],
# #      ylim=c(0,4),
# #      pch=16,
# #      cex=0.8,
# #      xlab="VPD (kPa)",
# #      ylab=expression(Sap~flow~(L~d^-1)),
# #      col="coral")
# # par(new=TRUE)
# # plot(data.both.sap$volRing[data.both.sap$Ring!="R1"]~data.both.sap$VPD[data.both.sap$Ring!="R1"],
# #      ylim=c(0,4),
# #      pch=16,
# #      cex=0.8,
# #      xlab="VPD (kPa)",
# #      ylab=expression(Sap~flow~(L~d^-1)),
# #      col="navy")
# # 
# # legend("bottomright",legend = c("MAESPA","Heat Pulse"),
# #        col=c("coral","navy"),
# #        pch=16,
# #        bty='n')
# # dev.off()