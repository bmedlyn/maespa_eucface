
source("R/load.R")

# run ring 1 with abiemt co2

original.wd <- getwd()

r1.a.path <- file.path(original.wd,"test")

setwd(r1.a.path)

shell("maespa64.exe")

get.1.ring.func <- function(target.dir,original.wd){
  # setwd(file.path(original.wd,"test"))
setwd(target.dir)
on.exit(setwd(original.wd))
fn <- "met.dat"
met <- read.table(fn,head=FALSE, skip=24)

names(met)<-c("CA","PPT","PAR","TAIR","RH")

fn <- "met_ListOfAllVaule.csv"
InputValue <- read.csv(fn)

fn <- "Dayflx.dat"
DayFlux <- read.table(fn,head=FALSE, skip=22)

names(DayFlux)<-c("DOY", "Tree", "Spec", "absPAR", "absNIR", "absTherm", 
                  "totPs", "totRf", "netPs", "totLE1", "totLE2","totH")

# get daily average with in and out puts

DailyAverage.flux <- summaryBy(totPs + absPAR + totLE1 ~ DOY,
                                 data = DayFlux,
                                 FUN = sum,na.rm = TRUE)
names(DailyAverage.flux) <- c("DOY","GPP","absPAR","le")

DailyAverage.input <-GetAverage(InputValue)

DailyAverage.input$DOY <- c(1:nrow(DailyAverage.input)) 

DailyAverage.input <- subset(DailyAverage.input,DOY <= nrow(DailyAverage.flux))
  
DailyAverage.Ring <- merge(DailyAverage.flux ,DailyAverage.input)

# 
  
DailyAverage.Ring$Date <- as.Date(DailyAverage.Ring$Date)
  #get VPD
DailyAverage.Ring$VPD <- getVPD(DailyAverage.Ring$RH,DailyAverage.Ring$TAIR)
  # get lai
DailyAverage.Ring$LAI <- sm[[1]]$LAIsmooth 

DailyAverage.Ring$GPP.g <- 12*DailyAverage.Ring$GPP/(pi*12.5^2) #g C m-2 ground

DailyAverage.Ring$absPAR.m <- DailyAverage.Ring$absPAR/(pi*12.5^2) #MJ m-2 d-1

DailyAverage.Ring$mm <- DailyAverage.Ring$le/(pi*12.5^2) * 1.8 * 0.01 #mm h2o d-1
  return(DailyAverage.Ring)
  
}

r1.a <- get.1.ring.func(r1.a.path,original.wd)

r1.e.path <- "Rings/Ring1/runfolder"
  
r1.e <- get.1.ring.func(r1.e.path,original.wd)

plot(r1.e$mm~r1.a$mm,
     xlim=c(0,3),ylim=c(0,3),
     pch=16,cex=0.5,
     xlab="Transpiration at aCa",
     ylab="Transpiration at eCa")
abline(a=0,b=1,col='red')
abline(lm(r1.e$mm~r1.a$mm),col="blue",lty="dashed")
legend("topleft",legend = c("1:1","Linear regression"),
       lty=c("solid","dashed"),col=c("red","blue"),bty='n')
title("Ring 1 with ambient and elevated C treatment")


