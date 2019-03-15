library(lubridate)
library(stringr)
library(plantecophys)
library(doBy)

# source("functions.R")

# Read EucFACE data
temp <- read.csv("data/E_teret_A-Ci_temperature_curves.csv",
                 stringsAsFactors=FALSE)
aci_euc <- read.csv("data/Aci.EucFACE.csv",
                    stringsAsFactors=FALSE)
aci_euc <- clean_eucface_aci(aci_euc)

# Extract info from EucFACE data
# returns df with info on each curve
Tpars <- get_Tpars(temp,"Curve")
euc <- get_JVmax(aci_euc,"Number",Tpars,400,550)

with(subset(euc,C.treat=='0C') ,plot(CiAmb,Citrans))
abline(a=0,b=1)
vj.bm.df <- subset(euc,C.treat=='0C')
vj.bm.sub <- vj.bm.df[vj.bm.df$Number %in% euc.1st.df$Number,]

vj.b.j.df <- merge(vj.bm.sub,euc.1st.df[,c('Number','par.transit')])

with(vj.b.j.df ,plot(c(CiAmb/Citrans))~par.transit)
plot(c(CiAmb/Citrans)~par.transit,data =vj.b.j.df,
     xlim=c(100,10000))
abline(v=1800)

my.fit <- euc.all.df[,c('Number','Vcmax','Jmax')]
names(my.fit) <- c('Number','Vcmax.jy','Jmax.jy')
my.fit <- my.fit[!duplicated(my.fit),]
comp.df <- merge(euc[,c('Number','Vcmax','Jmax')],my.fit)

plot(Vcmax~Vcmax.jy,data = comp.df)
plot(Jmax~Jmax.jy,data = comp.df)

#functions to fit ACi curves and return fitted parameters

#function to get R2 from Photosyn object
getr2 <- function(x){
  lmfit <- lm(Ameas ~ Amodel, data=x$df)
  summary(lmfit)$r.squared
}

getCitrans <- function(x) {
  unname(x$Ci_transition)
}

# Functon to make a dataframe with info about each fitted Jmax & Vcmax
makedata <- function(data, fit, byname) {
  
  ret <- coef(fit) 
  ret$Citrans <- sapply(fit,getCitrans)
  ret$Jmax[ret$Jmax > 1000] <- NA
  ret$rmse <- sapply(fit, "[[", "RMSE")
  ret$R2 <- sapply(fit, getr2)
  
  # extract A at fixed Ci, and average Tleaf
  ret$Ac250 <- sapply(fit, function(x)x$Photosyn(Ci=250)$Ac)
  ret$Aj250 <- sapply(fit, function(x)x$Photosyn(Ci=250)$Aj)
  ret$Jlim250 <- (ret$Aj250 < ret$Ac250)
  ret$A250 <- sapply(fit, function(x) min(x$Photosyn(Ci=250)$Ac,x$Photosyn(Ci=250)$Aj))
  ret$Ac360 <- sapply(fit, function(x)x$Photosyn(Ci=360)$Ac)
  ret$Aj360 <- sapply(fit, function(x)x$Photosyn(Ci=360)$Aj)
  ret$Jlim360 <- (ret$Aj360 < ret$Ac360)
  ret$A360 <- sapply(fit, function(x) min(x$Photosyn(Ci=360)$Ac,x$Photosyn(Ci=360)$Aj))
  ret$Tleaf <- sapply(fit,function(x)mean(x$df$Tleaf))
  
  # with(ret,plot(Actrans,Actrans))
  #merge with original data frame to get info on curve
  ret <- merge(ret,data,by=byname)
  ret <- ret[!duplicated(ret[,c(byname)]),]
  ret <- renameCol(ret,"Tleaf.x","Tleaf")
  
  return(ret)
}

# function to fit peaked temperature response of Vmax 
fitpeaked <- function(Tleaf,Vmax){
  
  TsK <- Tleaf + 273.15
  fVc <- as.formula(Vmax ~ k25 * exp((Ea*(TsK - 298.15))/(298.15*0.008314*TsK)) * 
                      (1+exp((298.15*delS - 200)/(298.15*0.008314))) / 
                      (1+exp((TsK*delS-200)/(TsK*0.008314))))
  
  Vc <- nls(fVc, start = list(k25=100, Ea=60, delS = 0.64))
  Vc2 <- summary(Vc)
  r2 <- cor(fitted(Vc),Vmax)^2
  
  #test for normality of residuals
  rest<-residuals(Vc)
  norm<-shapiro.test(rest)
  s<-norm$statistic
  pvalue<-norm$p.value
  
  # Calculate Topt
  res <- c(Vc2$coefficients[1:6])
  ToptK <- 200/(res[[3]]-0.008314*log(res[[2]]/(200-res[[2]])))
  Topt <- ToptK-273.15
  
  # make data frame to return
  param <- c(res,Topt,r2,s,pvalue)
  names(param)[1:10]<-c("k25","Ea","delS","k25.se","Ea.se","delS.se","Topt","R2","S","pvalue")
  return(param)
  
}

# Function that tidies EucFACE A-Ci
clean_eucface_aci <- function(dat) {
  
  # Correct dates
  dat$Date_licor <- str_replace(dat$Date,"Thr","Thu")
  dat$Date <- parse_date_time(dat$Date_licor,c("m d y","a m d y H M S"))
  dat$Date[is.na(dat$Date)] <- as.Date("2016-10-10")
  dat$Date <- as.Date(dat$Date)
  
  # remove unrealistic data
  dat <- dat[dat$Photo < 50,]
  dat <- dat[dat$Cond > 0 ,]
  dat <- dat[dat$Photo > -10,]
  dat <- dat[dat$Ci > 10,]
  
  # Get rid of A-Ci's with less than 5 points
  len <- summaryBy(Photo~Number,data=dat,FUN=length)
  nums <- len$Number[len$Photo.length > 4]
  good <- subset(dat,Number %in% nums)
  
  return(good)
  
}

# Function that tidies Duke A-Ci
clean_duke_aci <- function(dat) {
  
  # Correct dates
  dat$Date <- str_replace(dat$Date,"Sept","Sep")
  dat$d1 <- parse_date_time(dat$Date,"m d y")
  dat$d1[is.na(dat$d1)] <- parse_date_time(dat$Date[is.na(dat$d1)],"d m y")
  dat$Date <- dat$d1
  # make campaign field - month & year
  dat$Campaign <- paste0(month(dat$Date,label=TRUE),"_",year(dat$Date))
  
  # Put photosynthesis values on half-total leaf area basis
  dat$Photo <- dat$Photo/2
  
  # did not appear to be many unrealistic data
  
  # Get rid of A-Ci's with less than 5 points
  len <- summaryBy(Photo~Identity,data=dat,FUN=length)
  nums <- len$Identity[len$Photo.length < 5]
  good <- subset(dat,!Identity %in% nums)
  
  return(good)
  
}

# Function to extract Anet, Ci & gs at a given CO2 concentration
get_vals <- function(data, Ca, byname, label) {
  
  sub <- subset(data,CO2S > Ca-25 & CO2S < Ca+25)
  formula <- as.formula(paste0("Photo+Cond+CO2S+Ci~",byname))
  ret <- summaryBy(formula=formula, data=sub, FUN=mean)
  names(ret) <- c(byname, paste0("Photo",label), paste0("Cond",label),
                  paste0("CO2S",label), paste0("Ci",label))
  return(ret)
  
}

# get temperature parameters #####
get_Tpars <- function(temp,byname){
  
  # get temperature response from temp
  fitsT <- fitacis(temp, byname, fitmethod="bilinear", Tcorrect=FALSE)
  dfef <- makedata(temp, fitsT, byname=byname)
  
  # fit temperature response parameters
  vcmaxTpars <- data.frame(do.call(rbind,
                                   list(fitpeaked(dfef$Tleaf,dfef$Vcmax))))
  jmaxTpars <- data.frame(do.call(rbind,
                                  list(fitpeaked(dfef$Tleaf,dfef$Jmax))))
  
  Tpars <- cbind(t(vcmaxTpars),t(jmaxTpars))
  colnames(Tpars) <- c("Vcmax","Jmax")
  return(Tpars)
}

# fit A-Ci curves
get_JVmax <- function(dat,byname,Tpars,aCa,eCa){  
  
  dat.fit <- fitacis(dat,group=byname,Tcorrect=TRUE,fitmethod="bilinear",
                     EaV = Tpars["Ea","Vcmax"]*1000, EdVC = 2e+05,
                     delsC = Tpars["delS","Vcmax"]*1000, 
                     EaJ = Tpars["Ea","Jmax"]*1000, 
                     EdVJ = 2e+05, delsJ = Tpars["delS","Jmax"]*1000)
  all.fitted <- makedata(dat,dat.fit,byname=byname)
  
  # extract values at ambient and elevated ppm
  amb <- get_vals(dat,aCa,byname,"Amb")
  ele <- get_vals(dat,eCa,byname,"Ele")  
  all <- merge(all.fitted,amb,by=byname)
  all <- merge(all,ele,by=byname)
  all$delPhoto <- with(all,PhotoEle/PhotoAmb)
  
  return (all)
}





