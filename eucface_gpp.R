# the trunck hieght problems still there is ring 6
source("R/load.R")
#test - lai sensitivity test
test <- 1
# base-base lai
base <- 0.8

# make sure you want to do this first
hourly.data <- FALSE
eucGPP(lai.test = test,lai.base = base)

#analysis###################
# read inputs and outputs
met <- list()
input <- list()
flux <- list()
# hr.flux <- list()
for (i in 1:6){
  
  met[[i]] <- Readmet(i) 
  input[[i]] <- ReadInput(i)
  flux[[i]] <- ReadDayFlux(i)
  # hr.flux[[i]] <- ReadHourFlux(i)
  
}
# 
# col.nm <- c("DOY","Tree","Spec","HOUR","hrPAR",
#             "hrNIR","hrTHM","hrPs","hrRf","hrRmW",
#             "hrLE","LECAN","Gscan","Gbhcan","hrH",
#             "TCAN","ALMAX","PSIL","PSILMIN","CI",
#             "TAIR","VPD","PAR","ZEN","AZ")
# 
# for (i in 1:6){
#   
#   names(hr.flux[[i]]) <- col.nm
#   
# }

# make a df of all in and out puts togethter
AllRing <- getAllRings(DayFlux = flux,InputValue = input)

Data <- getAllData(DayFlux = flux,InputValue = input)

#make the ring total to per m2 ground 
Monthly <- Data
Monthly$Date <- as.Date(Data$Date)
Monthly$LE <- Data$le/(pi*12.5^2) #mol h2o m-2 ground
Monthly$GPP <- 12*Data$GPP/(pi*12.5^2) #g C m-2 ground
Monthly$absPAR<- Data$absPAR/(pi*12.5^2) #MJ m-2 d-1
Monthly$Date <- factor(format(Monthly$Date,'%Y-%m'))
Monthly$mm <- Monthly$LE* 1.8 * 0.01 #mm h2o d-1
Monthly$date.full <- Data$Date

# sap
data.sap <- subset(Monthly,select=c("date.full","Ring","GPP","mm","VPD","PAR","LAI","TAIR"))
names(data.sap) <- c("Date","Ring","GPP","Trans","VPD","PAR","LAI","TAIR")

for (i in 1:6){
  data.sap$Ring[which(data.sap$Ring == as.character(i))] <- sprintf("R%i",i)
}

data.sap$Ring <- as.factor(data.sap$Ring)
sap.T <- readRDS("sap_daily.rds")  
sap.T <- subset(sap.T,select = c("Date","Ring","volRing"))

data.both.sap <- merge(data.sap,sap.T,by = intersect(names(data.sap), names(sap.T)))
saveRDS(data.both.sap,"mastra and sap.rds")

#get monthly total
Months <- split(Monthly, Monthly$Ring)

sum <- lapply(Months,function(x){

    y <- data.table(x)[,list(GPP = sum(GPP, na.rm=TRUE),CA =mean(CA,na.rm=TRUE),
                             absPAR = sum(absPAR, na.rm=TRUE),
                             LAI = mean(LAI, na.rm=TRUE),LE =sum(LE,na.rm=TRUE),
                             PAR = sum(PAR,na.rm=TRUE),GOL = sum(GOL,na.rm=TRUE),
                             dL=sum(dL,na.rm=TRUE)),by = Date]
  return(y)
})

for (i in 1:6){sum[[i]]$Ring <- i
sum[[i]]$Month <- as.Date(as.yearmon(sum[[i]]$Date, "%Y-%m"))}

#seperate E and A rings
# E: 145 A :236
Elevated_Sum <- rbind(sum[[1]],sum[[4]])
Elevated_Sum <- rbind(Elevated_Sum,sum[[5]])

Ambient_Sum <- rbind(sum[[2]],sum[[3]])
Ambient_Sum <- rbind(Ambient_Sum,sum[[6]])

Ambient_Sum$CO2Level <- "Ambient"
Elevated_Sum$CO2Level <- "Elevated"
E_A <- rbind(Elevated_Sum,Ambient_Sum)
E_A$Month <- as.Date(as.yearmon(E_A$Date,"%Y-%m"))
E_A$CO2Level <- as.factor(E_A$CO2Level)

# get average by treatment
sumE <- data.table(E_A[E_A$CO2Level == "Elevated",])[,list(GPP = mean(GPP, na.rm=TRUE),
                                                           GOL = mean(GOL, na.rm=TRUE),
                                                           LAI = mean(LAI, na.rm=TRUE),
                                                           absPAR = mean(absPAR, na.rm=TRUE),
                                                           LE =mean(LE,na.rm=TRUE),
                                                           PAR = mean(PAR,na.rm=TRUE),
                                                           CO2Level = CO2Level[1],
                                                           CA = mean(CA, na.rm=TRUE),
                                                           dL=mean(dL,na.rm=TRUE)),by = Date]


sumA <- data.table(E_A[E_A$CO2Level == "Ambient", ])[,list(GPP = mean(GPP, na.rm=TRUE),
                                                           GOL = mean(GOL, na.rm=TRUE),
                                                           LAI = mean(LAI, na.rm=TRUE),
                                                           absPAR = mean(absPAR, na.rm=TRUE),
                                                           LE =mean(LE,na.rm=TRUE),
                                                           PAR = mean(PAR,na.rm=TRUE),
                                                           CO2Level = CO2Level[2],
                                                           CA = mean(CA, na.rm=TRUE),
                                                           dL=mean(dL,na.rm=TRUE)),by = Date]
sumEA <-rbind(sumA,sumE)
sumEA$Month <- as.Date(as.yearmon(sumEA$Date, "%Y-%m"))
sumEA$fAPAR <- sumEA$absPAR/sumEA$PAR
sumEA$LUE <- sumEA$GPP/sumEA$absPAR

# statistics of the results
EA <- summarySE(data=E_A, measurevar="GPP", groupvars=c("CO2Level","Date"))
EA$Month <- as.Date(as.yearmon(EA$Date, "%Y-%m"))
EA$Lci <- EA$GPP-EA$ci
EA$Hci <- EA$GPP+EA$ci
# get the co2 effect of gpp   
GPP.diff <- EA[which(EA$CO2Level == "Ambient"),]
library(reshape)
GPP.diff <- rename(GPP.diff,c(GPP="GPP_Ambient"))
GPP.diff <- subset(GPP.diff, select=c("Date", "GPP_Ambient","Month"))
GPP.diff$GPP_Elevated <- EA$GPP[EA$CO2Level == "Elevated"]
GPP.diff$Difference <- GPP.diff$GPP_Elevated-GPP.diff$GPP_Ambient
GPP.diff$Ratio <- (GPP.diff$GPP_Elevated-GPP.diff$GPP_Ambient)/GPP.diff$GPP_Ambient

# #this need to be changed. need read directly from cloud
# lprod<-read.csv("leaf production per month.csv")
# lprod$lprodInC <- lprod$laprod*0.5*100^2/52.6 #g C m-2
# LD <- data.frame(lprod$Date,lprod$lprodInC,lprod$treatment)
# LD <- LD[order(LD$lprod.treatment),]
# write.csv(LD,"LD.csv")
# GD <- data.frame(sumEA$Month,sumEA$GPP,sumEA$CO2Level)
# GD1214 <- GD[as.Date(GD$sumEA.Month) > as.Date("2012-10-31")&as.Date(GD$sumEA.Month) < as.Date("2014-07-3"),]
# write.csv(GD1214,"GD.csv")
# 
# GLD<-read.csv("GD LD interaction.csv")
# GLD$Date <- as.Date(GLD$lprod.Date, "%d/%m/%Y")

#get total of 2013/6 to 2014/6
sumEA$Month <- as.Date(as.yearmon(sumEA$Date, "%Y-%m"))
sum.201314 <- sumEA[sumEA$Month >= as.Date("2013-06-1") & sumEA$Month < as.Date("2014-06-1"),]

sum.201314 <- data.table(sum.201314)[,list(GPP = sum(GPP, na.rm=TRUE),
                                           LE = 1.8 * 0.01 * sum(LE,na.rm=TRUE), #to get mol to mm
                                           absPAR = sum(absPAR,na.rm=TRUE)),
                                     by = CO2Level]
sum.201314$WUE <- sum.201314$GPP / sum.201314$LE

# saveRDS(sum.201314,file = file.path("output",sprintf("sum of 2013 to 2014-laiBase %s.rds",lai.base)))
# readRDS(file.path("output",sprintf("sum of 2013 to 2014-laiBase 0.8.rds")))
# readRDS(file.path("output",sprintf("sum of 2013 to 2014-laiBase 0.rds")))


