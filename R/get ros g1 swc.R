library(HIEv)
library(car)
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#- This script downloads all the data from HIEv from Drke 2017 AFM
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------
#- check if the data and output directories exist. If they don't, create them.
dir.create(file.path("data"),showWarnings=F)
# dir.create(file.path("Output"),showWarnings=F)

#------------------------------------------------------------------------------------------------------------------
#- get the soil moisture data
downloadHIEv(searchHIEv("ROS_MD_PM_SOILMOIST-HANDHELD_L2.csv"),topath="data/")
downloadHIEv(searchHIEv("ROS_MD_PM_SOILMOIST-LOGGERS_L1.csv"),topath="data/")
downloadHIEv(searchHIEv("ROS_MD_PM_SOILMOIST-CODES.csv"),topath="data/")

#find all of the files with soil moisture at ROS
dfr <- downloadTOA5(filename="ROS_[0-9]_ROS[0-9][0-9]Table15min",maxnfiles=120,topath="data/VWC/",
                    startDate="2012-08-01",endDate="2013-12-1")
#------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------
#- get the leaf-level gas exchange data
downloadHIEv(searchHIEv("ROS_MD_GX-Asat_120410-130516_L1.csv"),topath="data/")
#------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------
#- get the leaf water potential data
downloadHIEv(searchHIEv("ROS_MD_PM_LWP_20121109-20130702_L1.csv"),topath="data/")
downloadHIEv(searchHIEv("ROS_MD_PM_LWP_2012-10-04_L1.csv"),topath="data/")
#read and process handheld TDR data
get.TDR.handheld <- function(filterBlanks=1){
  
  #- read in the data from HIEv
  TDR1 <- read.csv("Data/ROS_MD_PM_SOILMOIST-HANDHELD_L2.csv")
  
  #- Filter out the blanks and non-shelter measurements
  if (filterBlanks==1){
    TDRdat <- subset(TDR1, Species !="blank" & Pot < 700)
    TDRdat$Species <- droplevels(TDRdat$Species)} #remove data from blank pots, keep only data from shelters (pots 100-699)
  if (filterBlanks!=1){
    TDRdat <- TDR1
  }
  
  TDRdat$Treat <- tolower(TDRdat$Treat)
  TDRdat$Date <- as.Date(TDRdat$Date)
  
  #- convert from % to m3 m-3
  TDRdat$TDR <- TDRdat$TDR/100
  
  return(TDRdat)
}

returng1 <- function(){
  
  #get the gas exchange data
  ros <- return.gx.vwc()
  #ros <- return.gx.vwc.lwp() # note, this cuts out more than half the data.
  
  #--- na-fill some crazy cacu data from the dry-down
  ros$Cond[which(ros$Species=="cacu" & ros$gxDate==as.Date("2013-3-26") & ros$Treat=="dry" & ros$Cond >0.3)] <- NA
  ros$Cond[which(ros$Species=="cacu" & ros$gxDate==as.Date("2013-3-21") & ros$Treat=="dry" & ros$Cond >0.3)] <- NA
  ros$Cond[which(ros$Species=="cacu" & ros$gxDate==as.Date("2013-4-4") & ros$Treat=="dry" & ros$Cond >0.2)] <- NA
  
  
  #split data into a list, for each species and treatment on each date
  ros.date.list <- split(ros,paste(ros$Species,ros$gxDate,ros$Treat))
  
  #fit the optimal stomatal model for each set of data in the list
  fits.list <- list()
  Species <- vector(mode="character")
  Date <- as.Date(NA)
  Treat <- vector(mode="character")
  TDR <- c()
  for (i in 1:length(ros.date.list)){
    dat <- ros.date.list[[i]]
    fits.list[[i]] <- nls(Cond ~ 1.6*(1+g1/sqrt(VpdL))*(Photo/CO2S),start=list(g1=4),data=dat,algorithm="port",
                          lower=c(0),upper=(10))
    Date[i] <- as.Date(dat$gxDate[1])
    Species[i] <- as.character(dat$Species[1])
    Treat[i] <- as.character(dat$Treat[1])
    TDR[i] <- mean(dat$TDR)
  }
  #extract the model fits
  g1pars <-as.data.frame(do.call(rbind,(lapply(fits.list,FUN=coefficients))))
  g1pars$Species <- factor(Species)
  g1pars$Treat <- factor(Treat)
  g1pars$Treat <- relevel(g1pars$Treat,ref=2)
  g1pars$TDR <- TDR
  g1pars$Date <- as.Date(Date)
  
  
  #get confidence interval of g1 parameters
  g1conf <- as.data.frame(do.call(rbind,lapply(fits.list,FUN=confint)))
  g1pars$g1_2.5 <- g1conf[,1]
  g1pars$g1_97.5 <- g1conf[,2]
  g1pars$names <- with(g1pars,paste(Species,Treat,sep=": "))
  g1pars$TDRjit <- jitter(g1pars$TDR,factor=0.5)
  
  #gap fill the data with uncertain g1 values
  g1pars$g1[which(g1pars$g1_2.5<=0)] <- 0
  g1pars$g1_2.5[which(is.na(g1pars$g1_2.5))] <- 0
  g1pars$g1_97.5[which(is.na(g1pars$g1_97.5))] <- 0
  
  return(g1pars)
}

return.gx.vwc <- function(){
  
  gx <- read.gx.ROS()
  vwc <- na.omit(get.TDR.handheld())
  
  gx.dry <- subset(gx,Treat=="dry")
  vwc.dry <- subset(vwc,Treat=="dry")
  
  gx.wet <- subset(gx,Treat=="wet")
  vwc.wet <- subset(vwc,Treat=="wet")
  
  #-------------------------------------------------------------------------------------
  # Find the closest date when VWC was measured, to match these obsevations with the gx measurements.
  
  vwcdates.wet <- summaryBy(TDR~Date,FUN=length,data=vwc.wet,keep.names=TRUE)
  vwcdates.wet <- subset(vwcdates.wet,TDR>15) #get rid of the dates with few observaitons
  #get the dates gx and vwc was measured in the dry treatments
  gxdates.dry <- levels(as.factor(gx.dry$Date))
  vwcdates.dry <- levels(as.factor(vwc.dry$Date))
  #get the dates gx and vwc was measured in the wet treatments
  gxdates.wet <- levels(as.factor(gx.wet$Date))
  vwcdates.wet <- levels(as.factor(vwcdates.wet$Date))
  
  
  #this function returns the closest date for a searchDate, given a vector of dates to search (dateList)
  # I use this function to help merge the TDR data with the GX data, as these were often measured on adjacent days
  closestDate <- function(searchDate, dateList){
    dist2date <- abs(as.Date(dateList) - as.Date(searchDate))
    closest <- which(min(dist2date)==dist2date)
    
    if (length(closest) > 1){
      closest <- closest[2]
    }
    return(dateList[closest])
  }
  
  #find the closest date of TDR measurements to the gas exchange date.
  # Note that this is done separately for wet and dry, as vwc was measured less
  # often in the wet treatment
  gx.dry$VWCdate <- NA
  for (i in 1:nrow(gx.dry)){
    gx.dry$VWCdate[i] <- closestDate(searchDate=gx.dry$Date[i],dateList=vwcdates.dry)
  }
  gx.wet$VWCdate <- NA
  for (i in 1:nrow(gx.wet)){
    gx.wet$VWCdate[i] <- closestDate(searchDate=gx.wet$Date[i],dateList=vwcdates.wet)
  }
  #put the wet and dry subsets back together
  gx2 <- rbind(gx.wet,gx.dry)
  gx2$VWCdate <- as.POSIXct(gx2$VWCdate,format="%Y-%m-%d",tz="GMT")
  
  
  #merge vwc and gx datasets together, using a date by species by treatment average for the vwc data
  names(gx2)[4] <- "gxDate"
  names(gx2)[57] <- "Date"
  
  #average the VWC data
  vwc.treat.avg <- summaryBy(TDR~Date+Species+Treat,
                             FUN=mean,keep.names=TRUE,data=vwc)
  names(vwc.treat.avg)[4] <- "TDR.mean"
  
  #- merge the gas exchange data with the individual measurements of soil water content.
  #- This doesn't work for all data.
  gx4 <- merge(gx2,vwc,by=c("Date","Species","Treat","Pot"),all.x=T) #- 247 NA's
  
  #- fill NA's with the treatment*species*date mean
  gx5 <- merge(gx4,vwc.treat.avg,by=c("Date","Species","Treat"))
  gx5$TDR[which(is.na(gx5$TDR))] <- gx5$TDR.mean[which(is.na(gx5$TDR))]

  return(gx5)
}

read.gx.ROS <- function(){
  #--- alternatively, read in the data on HIEv
  dat.hiev <- read.csv("Data/ROS_MD_GX-Asat_120410-130516_L1.csv")
  
  #- remove first column (names)
  dat.hiev$X <- NULL
  
  #- remove a few outliers
  toremove214 <- which(dat.hiev$Pot == 214 & dat.hiev$Date == "2013-03-21") #these two observations are crazy outliers and will be removed
  toremove219 <- which(dat.hiev$Pot == 219 & dat.hiev$Date == "2013-03-21")
  dat.hiev[toremove214,] <- NA
  dat.hiev[toremove219,] <- NA
  dat3 <- subset(dat.hiev,is.na(Date)==FALSE)
  
  #- reformat variables
  dat3$Date <- as.Date(dat3$Date)
  
  return(dat3)
}

#- get the g1 fits
g1values <- returng1()

# NSLpars <- summaryBy(Photo+Cond+Ci+TDR~Species+gxDate+Treat,data=g1values,FUN=mean,keep.names=T)

# fit g1 

dat <- g1values
dat$TSW <- (dat$TDR-0)/(0.38-0) # normalize TDR data to estimate the transpirable soil water
type="g1"
startlist = list(Xlow = 0.0, Xhigh=0.5, q = 0.4)
#- split into list of species
dat.l <- split(dat,dat$Species)

eute.ros <- dat.l$eute
plot(g1~TDR,data = eute.ros)
saveRDS(eute.ros,'cache/ros.gs.rds')





# #- preallocate empty lists to catch output
# fit.sp <- newdat <-  list()
# 
# #--------------------------------------------------
# #-- NLS function that returns "1.0" above Xhigh
# fit_beta <- function(Xval,Xlow,Xhigh,q){
#   
#   #- predict the Yval given the Xval
#   Yval <- ((Xval-Xlow)/(Xhigh-Xlow))^q
#   
#   #- overwrite the values beyond Xhigh
#   Yval[which(Xval>Xhigh)] <- 1
#   
#   return(Yval)
# }
# #--------------------------------------------------
# 
# 
# #- loop over each element of the list, fit data
# for (i in 1:length(dat.l)){
#   dat.temp <- dat.l[[i]]
#   dat.temp$Species <- factor(dat.temp$Species)
#   dat.temp$Xval <- dat.temp$TSW
#   #- fit the model
#   if (type=="g1") dat.temp$Yval <- dat.temp$g1/max(dat.temp$g1)
#   if (type=="NSL") dat.temp$Yval <- dat.temp$NSL
#   fit.sp[[i]] <- nls(Yval ~ ((Xval-Xlow)/(Xhigh-Xlow))^q,start=startlist,data=dat.temp,algorithm="port",
#                      lower=c(0,0,0.01),upper=c(0.007,1,3))
#   #fit.sp[[i]] <- nls(Yval ~ fit_beta(Xval,Xlow,Xhigh,q),start=startlist,data=dat.temp,algorithm="port",
#   #                   lower=c(0,0,0.01),upper=c(0.005,1,3))
#   
#   
#   # get predicted values and 95% confidence intervals by bootstrapping
#   if (type=="g1") newdat[[i]] <- expand.grid(Species=levels(dat.temp$Species), Xval=seq(from=0.01,to=1,length.out=99),lower=NA,upper=NA)
#   if (type=="NSL") newdat[[i]] <- expand.grid(Species=levels(dat.temp$Species), TDR=seq(from=coef(fit.sp[[i]])["Xlow"]+0.01,to=coef(fit.sp[[i]])["Xhigh"],length.out=99),lower=NA,upper=NA)
#   newdat[[i]]$wpred <- predict(fit.sp[[i]],newdat[[i]],level=0,se.fit=T)
#   
#   
#   rm(b)
#   b <- bootCase(fit.sp[[i]],B=300)
#   for(j in 1:nrow(newdat[[i]])){
#     rm(b02)
#     b02 <- ((newdat[[i]]$Xval[j]-b[,"Xlow"])/(b[,"Xhigh"]-b[,"Xlow"]))^b[,"q"]
#     #b02 <- fit_beta(Xval=newdat[[i]]$Xval[j],Xlow=b[,"Xlow"],Xhigh=b[,"Xhigh"],q=b[,"q"])
#     newdat[[i]]$lower[j] <- unname(quantile(b02,probs=c(0.025,0.975)))[1]
#     newdat[[i]]$upper[j] <- unname(quantile(b02,probs=c(0.025,0.975)))[2]
#     
#   }
#   
# }
# newdatg1.TSW <- newdat
# fit.spg1.TSW <- fit.sp
# rm(newdat)
# rm(fit.sp)
# g1_TDR_beta.TSW <- list(fit.spg1.TSW,newdatg1.TSW)

