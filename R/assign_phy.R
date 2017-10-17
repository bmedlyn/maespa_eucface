# functions from dushan with modification####

#functions to fit ACi curves and return fitted parameters
# function to get R2 from Photosyn object
getr2 <- function(x){
  lmfit <- lm(Ameas ~ Amodel, data=x$df)
  summary(lmfit)$r.squared
}
#functions to fit ACi curves.
makecurves <- function(fname) {
  
  data <- read.csv(fname) 
  
  fits1 <- fitacis(data, "Curve", fitmethod="bilinear", Tcorrect=FALSE,useRd=TRUE)
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
  
  #merge with original data frame
  data <- read.csv(fname) 
  ret <- merge(ret,data,by="Curve")
  ret <- ret[!duplicated(ret[,c("Curve")]),]
  ret <- subset(ret,R2 > 0.99) #criteria to get best fitted curves 
  
  ret$TsK <- ret$Tleaf+273.15
  ret$Tsq <- ret$Tleaf * ret$Tleaf
  
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

#fit ACi curves
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

# function that update phy.dat#####
update.phy.f <- function(lai.test,lai.base){
  # data need to be on hiev
  # get vcmax jmax t response$####
  t.response.df <- get.t.response.func("data/E_teret_A-Ci_temperature_curves.csv")
  
  # fit eucface aci to get vcmax and jmax####
  if(!file.exists("cache/ecu_aci_sum.rds")){
    euc.acis.df <- read.csv("data/P0020_EucFACE-Aci_MASTER.csv")
  
  # data clean
  euc.acis.df <- euc.acis.df[euc.acis.df$Photo < 50,]
  euc.acis.df <- euc.acis.df[euc.acis.df$Cond > 0 ,]
  # euc.acis.df$Curve_Number[euc.acis.df$Cond < 0]

 

  library(plantecophys)
  euc.fit <- fitacis(euc.acis.df,group="Curve_Number",Tcorrect=TRUE,
                     EaV = t.response.df["Ea","Vcmax"]*1000, EdVC = 200000,
                     delsC = t.response.df["delS","Vcmax"]*1000, 
                     EaJ = t.response.df["Ea","Jmax"]*1000, 
                     EdVJ = 2e+05, delsJ = t.response.df["delS","Jmax"]*1000)
  euc.coef <- coef(euc.fit)
  euc.coef$Tleaf <- sapply(euc.fit, function(x)mean(x$df$Tleaf))
  
  # put fits and measurements together
  euc.all.df <- merge(euc.coef,euc.acis.df,by = "Curve_Number")
  
  euc.all.df$Tree <- as.factor(euc.all.df$Tree)
  euc.all.df$C.treat <- as.factor(euc.all.df$C.treat)
  euc.all.df$Date_licor <-  as.character(euc.all.df$Date_licor)
  # get rid of white space in the front of some date
  trim.leading <- function (x)  sub("^\\s+", "", x)
  euc.all.df$Date_licor <- trim.leading(euc.all.df$Date_licor)
  
  euc.all.df$Date <- as.Date(as.character(euc.all.df$Date_licor),"%b %d %Y")
  
  euc.sub.df <- data.frame(Date = euc.all.df$Date,
                           Ring = euc.all.df$Ring,
                           Vcmax = euc.all.df$Vcmax,
                           Jmax = euc.all.df$Jmax)
  euc.sub.df <- euc.sub.df[order(euc.sub.df$Date),]

  euc.sub.df$Vcmax <- round(euc.sub.df$Vcmax)
  euc.sub.df$Jmax <- round(euc.sub.df$Jmax)
  
  euc.sum.df <- summaryBy(Vcmax + Jmax ~ Ring + Date,
                          data = euc.sub.df,
                          FUN=mean,na.rm = TRUE,
                          keep.names = TRUE)
  
  euc.sum.df$Vcmax <- round(euc.sum.df$Vcmax)
  euc.sum.df$Jmax <- round(euc.sum.df$Jmax)
  euc.sum.df$Date <- format(euc.sum.df$Date, format=c("%d/%m/%y")) 
  
  saveRDS(euc.sum.df,"cache/ecu_aci_sum.rds")
  }
  euc.sum.df <- readRDS("cache/ecu_aci_sum.rds")
  
  # functions for Rd T response####
  fit.rd.t.function <- function(fn){
    rdark.csv <- read.csv(fn)
    
    rdark.ls <- split(rdark.csv,rdark.csv$Tree)
    
    # Rd(T)=RD⋅exp(Q10F(TAIR–RTEMP))
    fit.rd.func <- function(df){
      nls(Photo~rd.25*exp(q10*(Tleaf - 25)),
          data = df,
          start=list(rd.25=-1,q10=0.05))
    }
    
    
    fit.all <- fit.rd.func(rdark.csv)
    
    fit.ls <- lapply(rdark.ls,fit.rd.func)
    
    coef.ls <- lapply(fit.ls,coef)
    coef.ls <- lapply(coef.ls,function(df){t(as.data.frame(df))})
    
    
    rd.par.df <- do.call(rbind,coef.ls)
    row.names(rd.par.df) <- NULL
    rd.par.df <- as.data.frame(rd.par.df)
    rd.par.df$tree <- names(rdark.ls)
    
    
    rd.par.df$ring <- substr(rd.par.df$tree,2,2)
    library(doBy)
    
    rd.sum.df <- summaryBy(rd.25 + q10 ~ ring,
                           data = rd.par.df,
                           FUN = mean,na.rm=TRUE,
                           keep.names = TRUE)
    return(rd.sum.df)
  }
  
  rd.t.df <- fit.rd.t.function("download/euc data/FACE_P0064_RA_GASEXCHANGE-RdarkT_20160215-L1.csv")
  

  
  # get g1######
  #g1 from Teresa's 2015 paper need to be on hiev
  g1 <- c(4.637, 3.564, 4.049, 4.084, 4.96, 3.413, 4.242)
  g1Date <- c('12/04/01','12/05/01','12/10/01','13/02/01','13/05/01','13/09/01','13/11/01') 
  g1Date <- as.Date(g1Date,format="%y/%m/%d")
  g1Date <- as.Date(g1Date,format="%d/%m/%y")
  test <- data.frame(g1,g1Date)
  test$Date <- format(test$g1Date,format="%d/%m/%y")
  
  #update eachRing####
  for (i in 1:6){
    #assign g1

    fn <- sprintf("Rings/Ring%s/runfolder/phy.dat",i)
    replaceNameList(namelist="bbmgs",datfile=fn,
                    vals=list(g0 = numeric(length(test$g1)),
                              g1 = g1,
                              dates = test$Date,
                              nsides = 1,
                              wleaf = 0.02, 
                              VPDMIN = 0.05,
                              gamma = 0)
    )
    
    replaceNameList(namelist="bbgscon",datfile=fn,
                    vals=list(nodates = length(test$Date),
                              condunits = 'h2o'
                    ))
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
    

    replaceNameList(namelist="jmax",datfile=fn,
                    vals=list(values=euc.sum.df$Jmax[euc.sum.df$Ring == i],
                              dates =euc.sum.df$Date[euc.sum.df$Ring == i]))
    replaceNameList(namelist="Vcmax",datfile=fn,
                    vals=list(values=euc.sum.df$Vcmax[euc.sum.df$Ring == i],
                              dates =euc.sum.df$Date[euc.sum.df$Ring == i]))
    
    replaceNameList(namelist="jmaxcon",datfile=fn,
                    vals=list(nolayers = 1,
                              noages = 1,
                              nodates = length(euc.sum.df$Date[euc.sum.df$Ring == i])))
    
    replaceNameList(namelist="Vcmaxcon",datfile=fn,
                    vals=list(nolayers = 1,
                              noages = 1,
                              nodates = length(euc.sum.df$Date[euc.sum.df$Ring == i])))


    replaceNameList(namelist="VCMAXPARS",datfile=fn,
                    vals=list(EAVC = t.response.df["Ea","Vcmax"]*1000,
                              EDVC = 200000,
                              DELSC = t.response.df["delS","Vcmax"]*1000
                              ))
    # THETA, EAVJ, EDVJ, DELSJ, AJQ, IECO 
    replaceNameList(namelist="JMAXPARS",datfile=fn,
                    vals=list(
                              # THETA =  ,
                              # AJQ = ,
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
    
    
    # make plot
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

