# get theta and alpha from LRC######
lrc.df <- read.csv("download/euc data/FACE_P0069_RA_GASEX-LRC_20141009-20170209_L1-V2.csv")
# the arrhenius function
arrhenius <- function(k25 = 100, Ea = 60, Rgas = 0.008314, TTK = 293.15) {
  fn <- k25 * exp((Ea*(TTK - 298.15))/(298.15*Rgas*TTK)) 
  return(fn)
}

lrc.df <-lrc.df[lrc.df$Datatype ==  "compLRC",]
lrc.df$gammas <- arrhenius(42.75,37830.0/1000, TTK =c(lrc.df$Tleaf + 273.15))
lrc.df$apar <- lrc.df$PARi * (1 - 0.093 - 0.082)
lrc.df.low.par <- lrc.df[lrc.df$PARi < 100,]
fit.a.lrc <- lm(Photo~apar,
                data = lrc.df.low.par)
alpha.a <- coef(summary(fit.a.lrc))[2]

alpha.j <- mean(4*alpha.a*(lrc.df.low.par$Ci + 2*lrc.df.low.par$gammas)/
  (lrc.df.low.par$Ci - lrc.df.low.par$gammas))
  
restult.lrc <- coef(nls(Photo~
                          alpha.a * apar+ Pm - ((alpha.a * apar+ Pm)^2-4*alpha.a * apar*Pm*theta)^0.5/2/theta,
                        data=lrc.df,start = list(Pm = 30, theta = 0.5)))

restult.lrc$alpha.j <- alpha.j

restult.lrc <- as.data.frame(restult.lrc)

# functions for T response from dushan with modification####
#functions to fit ACi curves and return fitted parameters
# function to get R2 from Photosyn object
getr2 <- function(x){
  lmfit <- lm(Ameas ~ Amodel, data=x$df)
  summary(lmfit)$r.squared
}
#functions to fit ACi curves.
makecurves <- function(fname) {
  
  data <- read.csv(fname) 
  
  fits1 <- fitacis(data, "Curve", fitmethod="bilinear", Tcorrect=FALSE,useRd=FALSE, fitTPU=TRUE)
  # fits2 <- fitacis(data, "Curve", fitTPU=T, Tcorrect=FALSE)
  
  # pdf(paste0(dir,"/TPU_ornot.pdf"), width=9, height=5)
  # par(mfrow=c(1,2))
  # for(i in 1:length(fits1)){
  #   plot(fits1[[i]], main=paste(names(fits1)[i], "No TPU", sep=" - "))
  #   plot(fits2[[i]], main=paste(names(fits1)[i], "TPU", sep=" - "))
  # }
  # dev.off()
  
  return(fits1)
}

makedata<- function(fname, fit) {
  
  ret <- coef(fit) 
  ret$Jmax[ret$Jmax > 1000] <- NA
  ret$rmse <- sapply(fit, "[[", "RMSE")
  ret$R2 <- sapply(fit, getr2)
  
  ret$maxCi <- sapply(fit,function(x)max(x$df$Ci))
  ret$minCi <- sapply(fit,function(x)min(x$df$Ci))
  ret$Ts<-sapply(fit, function(x)mean(x$df$Tleaf))
  #merge with original data frame
  data <- read.csv(fname) 
  ret <- merge(ret,data,by="Curve")
  ret <- ret[!duplicated(ret[,c("Curve")]),]
  ret <- subset(ret,R2 > 0.99) #criteria to get best fitted curves 
  
  ret$TsK <- ret$Ts+273.15
  ret$Tsq <- ret$Ts * ret$Ts
  return(ret)
}

# function to fit temperature response of Vcmax and get CI of parameters
# peaked model (estimation of predictions and 95% CI dissabled to reduce impliment time)

fitpeaked<-function(dat){
  
  fVc <- as.formula(Vcmax ~ k25 * exp((Ea*(TsK - 298.15))/(298.15*0.008314*TsK)) * 
                      (1+exp((298.15*delS - 200)/(298.15*0.008314))) / 
                      (1+exp((TsK*delS-200)/(TsK*0.008314))))
  
  Vc<-nls(fVc, start = list(k25=100, Ea=60, delS = 0.64), data = dat)
  Vc2<-summary(Vc)
  res<-Vc2$coefficients[1:6]
  names(res)[1:6]<-c("Vcmax25","EaV","delsV","Vcmax25.se","EaV.se","delsV.se")
  
  #return(list(results,predicts.df))
  r<-cor(fitted(Vc),dat$Vcmax)
  r2<-r*r
  
  #test for normality of residuals
  rest<-residuals(Vc)
  norm<-shapiro.test(rest)
  s<-norm$statistic
  pvalue<-norm$p.value
  
  topt<-200/(res[[3]]-0.008314*log(res[[2]]/(200-res[[2]])))
  Topt<-topt-273.15
  
  param<-c(res,Topt,r2,s,pvalue)
  names(param)[1:10]<-c("Vcmax25","Ea","delS","Vcmax25.se","Ea.se","delS.se","Topt","R2","S","pvalue")
  return(param)
  
}

fitpeakedJ<-function(dat){
  
  fVJ <- as.formula(Jmax ~ k25 * exp((Ea*(TsK - 298.15))/(298.15*0.008314*TsK)) * 
                      (1+exp((298.15*delS - 200)/(298.15*0.008314))) / 
                      (1+exp((TsK*delS-200)/(TsK*0.008314))))
  
  try(Vj<-nls(fVJ, start = list(k25=60, Ea=60, delS = 0.63), data = dat))
  Vj2<-summary(Vj)
  res<-Vj2$coefficients[1:6]
  names(res)[1:6]<-c("Jmax25","Ea","delsJ","Jmax25.se","EaJ.se","delsJ.se")
  
  #return(list(results,predicts.df))
  r<-cor(fitted(Vj),dat$Jmax)
  r2<-r*r
  
  #test for normality of residuals
  rest<-residuals(Vj)
  norm<-shapiro.test(rest)
  s<-norm$statistic
  pvalue<-norm$p.value
  
  topt<-200/(res[[3]]-0.008314*log(res[[2]]/(200-res[[2]])))
  Topt<-topt-273.15
  
  param<-c(res,Topt,r2,s,pvalue)
  names(param)[1:10]<-c("Jmax25","Ea","delS","Jmax25.se","Ea.se","delS.se","Topt","R2","S","pvalue")
  return(param)
}

#t.response func
get.t.response.func <- function(data.path){
  
  fef <- makecurves(data.path)
  
  dfef <- makedata(data.path,fef)

  #fit temperature response of Vcmax and Jmax and pull out parameters
  #peaked Arrhenius model
  
  eucface_vcmax <- data.frame(do.call(rbind,list(fitpeaked(dfef))))
  
  eucface_jmax <- data.frame(do.call(rbind,list(fitpeakedJ(dfef))))
  
  see <- cbind(t(eucface_vcmax),t(eucface_jmax))
  colnames(see) <- c("Vcmax","Jmax")
  rownames(see)[1] <- "k25"
  rownames(see)[4] <- "k25.se"
  
  return(see)
}

# fit jmax and ncmax with t correction####
fit.aci.func <- function(euc.acis.df){
  # data clean
  euc.acis.df <- euc.acis.df[euc.acis.df$Photo < 50,]
  euc.acis.df <- euc.acis.df[euc.acis.df$Photo > -2,]
  euc.acis.df <- euc.acis.df[euc.acis.df$Cond > 0 ,]
  euc.acis.df <- euc.acis.df[complete.cases(euc.acis.df$Number),]
  
  euc.acis.df <- subset(euc.acis.df,Age!='lower')
  
  euc.acis.df$Date <- as.character(euc.acis.df$Date)
  euc.acis.df$Ring[is.na(euc.acis.df$Ring)] <- 3
  euc.acis.df$Date[nchar(euc.acis.df$Date) > 11 & nchar(euc.acis.df$Date) < 13] <- 
    substr(euc.acis.df$Date[nchar(euc.acis.df$Date) > 11 & nchar(euc.acis.df$Date) <13],2,12)
  euc.acis.df$Date[nchar(euc.acis.df$Date) > 13] <- 
    substr(euc.acis.df$Date[nchar(euc.acis.df$Date) > 13 ],5,15)
  euc.acis.df$Date[euc.acis.df$Date == "10-Oct-16"] <- "Oct 10 2016"
  euc.acis.df$Date <-  as.Date(euc.acis.df$Date,"%b %d %Y")
  
  euc.acis.df <- euc.acis.df[euc.acis.df$Date > as.Date('2013-1-1') & 
                               euc.acis.df$Date < as.Date('2016-12-31'),]
  # # # plot to check data
  # plot(Photo~Ci,data = euc.acis.df[euc.acis.df$Number == 478,])
  # library(plantecophys)
  euc.fit <- fitacis(euc.acis.df,group="Number",Tcorrect=TRUE,fitmethod = c("bilinear"),
                     varnames = list(ALEAF = "Photo",Tleaf = "Tleaf", 
                                     Ci = "Ci", PPFD = "PARi"),
                     EaV = t.response.df["Ea","Vcmax"]*1000, 
                     EdVC = 2e+05,
                     delsC = t.response.df["delS","Vcmax"]*1000, 
                     EaJ = t.response.df["Ea","Jmax"]*1000, 
                     EdVJ = 2e+05, delsJ = t.response.df["delS","Jmax"]*1000)
  
  see <- Filter(function(x) length(x)>1, euc.fit)
  euc.coef <- as.data.frame(do.call(rbind,sapply(see,function(x) out.df <- data.frame(coef(x)))))
  names(euc.coef) <- c("Vcmax","Jmax","Rd")
  euc.coef$Number <- as.numeric(names(see))
  
  # put fits and measurements together
  euc.all.df <- merge(euc.coef,euc.acis.df,by = "Number")
  # date format clean and change
  euc.all.df$Tree <- as.factor(euc.all.df$Tree)
  euc.all.df$C.treat <- as.factor(euc.all.df$C.treat)
  # euc.all.df$Date <- as.character(euc.all.df$Date)
  # euc.all.df$Ring[is.na(euc.all.df$Ring)] <- 3
  # euc.all.df$Date[nchar(euc.all.df$Date) > 11 & nchar(euc.all.df$Date) < 13] <- 
  #   substr(euc.all.df$Date[nchar(euc.all.df$Date) > 11 & nchar(euc.all.df$Date) <13],2,12)
  # 
  # euc.all.df$Date[nchar(euc.all.df$Date) > 13] <- 
  #   substr(euc.all.df$Date[nchar(euc.all.df$Date) > 13 ],5,15)
  # euc.all.df$Date[euc.all.df$Date == "10-Oct-16"] <- "Oct 10 2016"
  # euc.all.df$Date <-  as.Date(euc.all.df$Date,"%b %d %Y")
  
  euc.all.df <- euc.all.df[!duplicated(euc.all.df[,c("Number")]),]
  # get ring average
  euc.sub.df <- data.frame(Campaign = euc.all.df$Campaign,
                           Date = euc.all.df$Date,
                           Ring = euc.all.df$Ring,
                           Vcmax = euc.all.df$Vcmax,
                           Jmax = euc.all.df$Jmax)
  
  euc.sum.df <- summaryBy(Vcmax + Jmax ~ Ring + Campaign,
                          data = euc.sub.df,
                          id=~Date,
                          FUN=mean,na.rm = TRUE,
                          keep.names = TRUE)
  
  euc.sum.df$Vcmax <- round(euc.sum.df$Vcmax)
  euc.sum.df$Jmax <- round(euc.sum.df$Jmax)
  euc.sum.df <- euc.sum.df[order(euc.sum.df$Date),]
  euc.sum.df$Date <- format(as.Date(euc.sum.df$Date), format=c("%d/%m/%y")) 
  
  saveRDS(euc.sum.df,"cache/ecu_aci_sum.rds")
}
# functions for Rd T response####
fit.rd.t.function <- function(rdark.csv){
  # rdark.csv <- read.csv(fn)
  
  # rdark.ls <- split(rdark.csv,rdark.csv$CurveID)
  
  # Rd(T)=RD⋅exp(Q10F(TAIR–RTEMP))
  fit.rd.func <- function(df){
    nls(Photo~rd.25*exp(q10*(Tleaf - 25)),
        data = df,
        start=list(rd.25=-1,q10=0.05))
  }
  
  fit.all <- fit.rd.func(rdark.csv)
  coef.all <- coef(fit.all)
  # fit.ls <- lapply(rdark.ls,fit.rd.func)
  
  # coef.ls <- lapply(fit.ls,coef)
  # coef.ls <- lapply(coef.ls,function(df){t(as.data.frame(df))})
  # 
  # rd.par.df <- do.call(rbind,coef.ls)
  # row.names(rd.par.df) <- NULL
  # rd.par.df <- as.data.frame(rd.par.df)
  # 
  # rd.par.df$CurveID <- names(rdark.ls)
  # rd.par.df$ring <- (rdark.csv$Ring[rdark.csv$CurveID == rd.par.df$CurveID])
  # # apply(rd.par.df,1,function(x) unique(rdark.csv$Ring[rdark.csv$CurveID == x$CurveID]))
  # # rd.par.df$ring <- substr(rd.par.df$tree,2,2)
  # 
  # library(doBy)
  # 
  # rd.sum.df <- summaryBy(rd.25 + q10 ~ ring,
  #                        data = rd.par.df,
  #                        FUN = mean,na.rm=TRUE,
  #                        keep.names = TRUE)
  return(coef.all)
}

# fit g1####
fit.g1.func <- function(spots){
  # spots <- read.csv("data/Gimeno_spot_Eter_gasExchange6400.csv")
  spots <- spots[is.na(spots$Tree) != TRUE,]
  spots$Campaign <- droplevels(spots$Campaign)
  fit.spot <- fitBBs(spots,"Campaign",gsmodel = c("BBOpti"))
  spots.g1 <- merge(coef(fit.spot),unique(spots[,c("Campaign","Date")]),by.x="group",by.y="Campaign")
  spots.g1$Date <- as.Date(as.character(spots.g1$Date),"%d/%m/%Y")
  spots.g1 <- spots.g1[complete.cases(spots.g1),]
  spots.g1 <- spots.g1[order(spots.g1$Date),]
  # spots.g1$Date <- format(spots.g1$Date,format="%d/%m/%y")
  return(spots.g1)
}
# function that update phy.dat#####
update.phy.f <- function(lai.test,lai.base,vj.ratio.test,vj.ratio,swc.g1=FALSE,photo.acli = FALSE){
  # data need to be on hiev
  # get vcmax jmax t response####
  if(!file.exists("cache/ecu_t_response.rds")){
    t.response.df <- get.t.response.func("data/E_teret_A-Ci_temperature_curves.csv")
    saveRDS(t.response.df,"cache/ecu_t_response.rds")
  }
  t.response.df <- readRDS("cache/ecu_t_response.rds")
  # fit eucface aci to get vcmax and jmax####
  if(!file.exists("cache/ecu_aci_sum.rds")){
    euc.acis.df <- read.csv("data/Aci.EucFACE.csv")
    fit.aci.func(euc.acis.df)
    source('r/vj pheno.r')
  }
  euc.sum.df <- readRDS("cache/euc.vj.rds")
  euc.sum.df$Date <- format(euc.sum.df$date,'%d/%m/%y')
  # get Rd~T####
  # read rd T data
  rd.df <- read.csv("download/FACE_P0064_RA_GASEXCHANGE-RdarkT_20160215-L1.csv")
  # data clean
  rd.df$Ring <-substr(rd.df$Tree,2,2)
  rd.df <- rd.df[rd.df$Tleaf<40,]
  wrong.curve <- rd.df$CurveID[rd.df$Tleaf < 40 & rd.df$Photo < -4]
  rd.df <- rd.df[!rd.df$CurveID %in% wrong.curve,]
  # fit the t response
  rd.t.vec <- fit.rd.t.function(rd.df)
  rd.t.df <- data.frame(rd.25  = rep(rd.t.vec[[1]],6),
                        q10 = rep(rd.t.vec[[2]],6),
                        ring=1:6)
  
  # get g1######
  # g1 from Teresa's 2015 paper
  spots <- read.csv(unz("download/Gimeno_spot.zip", "data/Gimeno_spot_Eter_gasExchange6400.csv"))
  spots.g1 <- fit.g1.func(spots)

  if (swc.g1==TRUE){
    if(file.exists('cache/g1 swc.rds')){
      swc.50.df <- readRDS('cache/g1 swc.rds')
    }else{
      source("r/get fitted g1 with swc.R")
    }
  }else{
    swc.50.df <- spots.g1
  }
  #update eachRing####
  for (i in 1:6){
    #assign g1
    if (swc.g1==TRUE){
      g1.sub.df <- swc.50.df[swc.50.df$Ring == paste0('R',i),]
    }else{
      g1.sub.df <- swc.50.df
    }
    fn <- sprintf("Rings/Ring%s/runfolder/phy.dat",i)
    # g1.sub.df$g1 <- 4.3
    replaceNameList(namelist="bbmgs",datfile=fn,
                    vals=list(g0 = 0,
                              g1 = g1.sub.df$g1,
                              dates = format(g1.sub.df$Date,"%d/%m/%y"),
                              nsides = 1,
                              wleaf = 0.01, 
                              VPDMIN = 0.05,
                              gamma = 0#,
                              # SMD1 = -100, 
                              # SMD2 = 1
                              )
    )
    
    replaceNameList(namelist="bbgscon",datfile=fn,
                    vals=list(nodates = length(g1.sub.df$Date),
                              condunits = 'H2O'
                    ))
    
    # replaceNameList(namelist="bblgs",datfile=fn,
    #                 vals=list(g0 = 0,
    #                           g1 = rep(9,length(swc.50.df$Date)),
    #                           D0L = rep(1500,length(swc.50.df$Date)),
    #                           dates = spots.g1$Date,
    #                           nsides = 1,
    #                           wleaf = 0.01, 
    #                           VPDMIN = 0.05,
    #                           gamma = 0)
    # )
    
    # tuzet pars
    #gs paramteres for tuzet
    replaceNameList(namelist="bbtuz",datfile=fn, vals=list(g0 =0,
                                                           # g1 = 3.15, #zhou 2013
                                                           # sf = 1.82, #from zhou 2013
                                                           # psiv=-1.66, #from zhou 2013
                                                           
                                                           # g1 = 4.275, #Teresa
                                                           # sf = 0.82, #from p50 data brendon
                                                           # psiv=-3.6,#from p50 data brendon
                                                           
                                                           # g1 = 15, #Drake 2017
                                                           # sf = 1.49, #Drake 2017
                                                           # psiv=-0.95,#Drake 2017
                                                           
                                                           G1 = 15, 
                                                           SF = 24.728773, #Teresa 2016
                                                           PSIV=-3.990483,#Teresa 2016
                                                           
                                                           nsides=1,
                                                           wleaf=0.02,
                                                           gamma=0,
                                                           VPDMIN=0.05 
    ))
    
    #Vcmax and Jmax
    options(scipen=999)
    
    if(vj.ratio.test == FALSE){
      vcmax.use = euc.sum.df$Vcmax[euc.sum.df$Ring == i]
    }else{
      vcmax.use = vj.ratio * euc.sum.df$Vcmax[euc.sum.df$Ring == i]
    }
  

    
    if(photo.acli == FALSE){
      vj.df <- euc.sum.df[,c('Date','v.a','j.a')]
      names(vj.df) <- c('Date','v','j')
    }else{
      # if(i %in% c(1,4,5)){
      #   vj.df <- euc.sum.df[,c('Date','v.e','j.e')]
      # }else{
      #   vj.df <- euc.sum.df[,c('Date','v.a','j.a')]
      # }
      # names(vj.df) <- c('Date','v','j')
      vj.df <- euc.sum.df[,c('Date','v.e','j.e')]
      names(vj.df) <- c('Date','v','j')
    }
   
    
    replaceNameList(namelist="jmax",datfile=fn,
                    vals=list(values=vj.df$j,
                              dates =vj.df$Date))
    replaceNameList(namelist="Vcmax",datfile=fn,
                    vals=list(values=vj.df$v,
                              dates =vj.df$Date))
    
    replaceNameList(namelist="jmaxcon",datfile=fn,
                    vals=list(nolayers = 1,
                              noages = 1,
                              nodates = length(vj.df$Date)))
    
    replaceNameList(namelist="Vcmaxcon",datfile=fn,
                    vals=list(nolayers = 1,
                              noages = 1,
                              nodates = length(vj.df$Date)))
    
    
    replaceNameList(namelist="VCMAXPARS",datfile=fn,
                    vals=list(EAVC = t.response.df["Ea","Vcmax"]*1000,
                              EDVC = 200000,
                              DELSC = t.response.df["delS","Vcmax"]*1000
                    ))
    # THETA, EAVJ, EDVJ, DELSJ, AJQ, IECO 
    replaceNameList(namelist="JMAXPARS",datfile=fn,
                    vals=list(THETA =  restult.lrc$theta,
                              AJQ = restult.lrc$alpha.j,
                              EAVJ = t.response.df["Ea","Jmax"]*1000,
                              EDVJ = 200000,
                              DELSJ = t.response.df["delS","Jmax"]*1000
                    ))
    # rd t response 
    replaceNameList(namelist="rdpars",datfile=fn,
                    vals=list(rtemp = 25,
                              q10f = rd.t.df$q10[rd.t.df$ring == i],
                              dayresp = 1.0,
                              effyrf = 0.4
                    ))
    
    replaceNameList(namelist="RD",datfile=fn,
                    vals=list(VALUES = -rd.t.df$rd.25[rd.t.df$ring == i], 
                              DATES = '12/04/01'
                              
                    ))
    
    
    # make plot of used value #####################################################################
    # can be handy but not used for now############################################################
    ######
    # pdf("g1 vcmax jmax.pdf",width=12,height=9)
    # #plot g1 vcmax gmax for each ring####
    # par(mar=c(2,6,3,6), mfrow=c(3,1),cex = 0.8)
    # par(mar=c(0,6,2,6))
    # palette(c("chocolate2","cadetblue2","cadetblue3","chocolate3","chocolate4","cadetblue4"))
    # #Vcmax
    # with(temp[[1]],plot(Campaign,Vcmax,axes = FALSE,ylim = c(0,150),
    #                     ylab = expression(italic(Vc)[max]~(paste(mu,mol~m^-2~s^-1))),
    #                     xlab = " ",xaxt = 'n',col= "white"))
    # # axis(1,at=temp[[1]]$Campaign, lab=paste0(temp[[1]]$Campaign),col = "white")
    # axis(2,at=seq(0,150,50), lab=paste0(seq(0,150,50)),cex = 0.5)
    # abline(h=150)
    # abline(h=0)
    # abline(v=as.Date("2015-02-28"))
    # for(i in 1:6){
    #   par(new = TRUE)
    #   with(temp[[i]],plot(Campaign,Vcmax,axes = FALSE,ann = FALSE,
    #                       xlab = " ",xaxt = 'n',
    #                       ylim = c(0,150),type = "b",col= i))
    # }
    # legend("bottomright", xpd = TRUE,horiz = TRUE, bty = "n",cex = 1,
    #        legend = strwrap(c("R1","R2","R3","R4","R5","R6"), width = 2),
    #        lwd =2, col= c("chocolate2","cadetblue2","cadetblue3","chocolate3","chocolate4","cadetblue4"))
    # 
    # #jmax
    # par(mar=c(2,6,0.5,6))
    # with(temp[[i]],plot(Campaign,Jmax,ylim = c(0,250),
    #                     ylab = expression(italic(J)[max]~(paste(mu,mol~m^-2~s^-1))),
    #                     axes = FALSE,xlab = " ",col= "white"))
    # axis(1,at=temp[[i]]$Campaign, lab=paste0(temp[[i]]$Campaign),col = "white")
    # axis(2,at=seq(0,250,50), lab=paste0(seq(0,250,50)),cex = 0.5)
    # abline(h=250)
    # abline(h=0)
    # abline(v=as.Date("2015-02-28"))
    # for(i in 1:6){
    #   par(new = TRUE)
    #   with(temp[[i]],plot(Campaign,Jmax,axes = FALSE,ann = FALSE,
    #                       xlab = " ",
    #                       ylim = c(0,250),type = "b",col= i))
    # }
    # legend("bottomright", xpd = TRUE,horiz = TRUE, bty = "n",cex = 1,
    #        legend = strwrap(c("R1","R2","R3","R4","R5","R6"), width = 2),
    #        lwd =2, col= c("chocolate2","cadetblue2","cadetblue3","chocolate3","chocolate4","cadetblue4"))
    # par(mar=c(6,6,2,6))
    # # G1
    # with(test,plot(g1Date,g1,ylim = c(0,6),
    #                ylab = expression(italic(g)[1]~(kPa^0.5)),
    #                xlab = " ",col = "red",axes = FALSE,type = "b"))
    # axis(1,at=test$g1Date, lab=paste0(test$g1Date),col = "white")
    # axis(2,at=seq(0,6,2), lab=paste0(seq(0,6,2)),cex = 0.5)
    # abline(h=0)
    # abline(h=6)
    # abline(v=as.Date("2013-11-24"))
    # #####
    # dev.off()
  }
  
  writeLines("phy updated")
  
}

