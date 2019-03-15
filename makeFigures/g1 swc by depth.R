plot.g1.swc.func <- function(depth.in){
  library(plantecophys)
  library(HIEv)
  library(doBy)
  # read neutron probe data ####
  swc.neutron.df <- read.csv("data/FACE_P0018_RA_NEUTRON_20120430-20180215_L1.csv")
  swc.neutron.df <- swc.neutron.df[swc.neutron.df$Location != "Outside",]
  swc.neutron.df$Date <- as.Date(as.character(swc.neutron.df$Date))
  swc.neutron.df$Ring <- as.character(swc.neutron.df$Ring)
  
  layers <- c(25,diff(unique(swc.neutron.df$Depth)[order(unique(swc.neutron.df$Depth))]))
  lay.df <- data.frame(Depth = unique(swc.neutron.df$Depth)[order(unique(swc.neutron.df$Depth))],
                       layers = layers )
  swc.neutron.df <- merge(swc.neutron.df,lay.df)
  
  get.mean.vwc.func <- function(swc.neutron.df,depth.in){
    test.df <- data.table(swc.neutron.df[swc.neutron.df$Depth <= depth.in,])[,list(
      VWC = sum(VWC * layers) / depth.in
    ),by = c('Date','Ring')]
    return(test.df)
  }

  swc.neutron.ring.df <- get.mean.vwc.func(swc.neutron.df,450)
  # use only numbers listed 
  # 25  50  75 100 125 150 200 250 300 350 400 450
  swc.neutron.ring.df$vwc.300 <- get.mean.vwc.func(swc.neutron.df,depth.in)$VWC
 
  # get g1####
  fit.g1.func <- function(spots,byRing = TRUE){
    # spots <- read.csv("data/Gimeno_spot_Eter_gasExchange6400.csv")
    spots <- spots[is.na(spots$Tree) != TRUE,]
    spots$Date <- as.character(spots$Date)
    spots$Date[spots$Date == "31/11/2012"] <- "31/10/2012"
    spots$Date <- as.Date(as.character(spots$Date),"%d/%m/%Y")
    spots$Date[spots$Date > as.Date("2013-09-08") & spots$Date < as.Date("2013-09-15")] <- as.Date("2013-09-10")
    spots$Campaign <- droplevels(spots$Campaign)
    
    if(byRing==TRUE){
      fit.spot <- fitBBs(spots,"group",gsmodel = c("BBOpti"))
      spots.g1 <- merge(coef(fit.spot),unique(spots[,c("Ring","Date","group","Campaign","Measurement")]),
                        by.x="group",by.y="group",all.y=FALSE)
      spots.g1 <- spots.g1[complete.cases(spots.g1),]
      spots.g1 <- spots.g1[order(spots.g1$Date),]
    }else{
      spots$Campaign <- as.character(spots$Campaign)
      fit.spot <- fitBBs(spots,'Campaign',gsmodel = c("BBOpti"))
      spots.g1 <- merge(coef(fit.spot),unique(spots[,c("Date","Campaign")]),
                        by.x="group",by.y="Campaign",all.y=FALSE)
      spots.g1 <- spots.g1[complete.cases(spots.g1),]
      spots.g1 <- spots.g1[order(spots.g1$Date),]
    }
    return(spots.g1)
  }
  
  spots <- read.csv(unz("download/Gimeno_spot.zip", 
                        "data/Gimeno_spot_Eter_gasExchange6400.csv"))
  spots <- spots[complete.cases(spots$Photo),]
  # remove pred treat data which doesn't has swc with them anyway
  spots <- spots[-(grep("2012Apr",as.character(spots$Campaign))),]
  spots <- spots[-(grep("2012May",as.character(spots$Campaign))),]
  spots$group <- paste0(as.character(spots$Campaign),"-",as.character(spots$Ring),"+",spots$Measurement)
  # spots$facotrs <- paste0(spots$Campaign,"+",spots$Measurement) 
  # spots <- spots[spots$group != "-",]
  spots.g1.ma <- fit.g1.func(spots)
  spots.g1.ma$Ring <- as.character(spots.g1.ma$Ring)
  spots.g1.ma <- tidyr::spread(spots.g1.ma,Measurement,g1)
  spots.g1.ma <- doBy::summaryBy(Anet_aft + Anet_mor ~ Date + Campaign + Ring,
                                 data = spots.g1.ma,keep.names = TRUE,FUN=mean,na.rm=TRUE)
  names(spots.g1.ma) <- c("Date","Campaign","Ring","g1.aft","g1.mor")
  
  spots$group <- paste0(as.character(spots$Campaign),"-",as.character(spots$Ring))
  spots.g1.one <- fit.g1.func(spots)
  spots.g1.one <- doBy::summaryBy(g1 ~ Date + Campaign + Ring,
                                  data = spots.g1.one,keep.names = TRUE,FUN=mean,na.rm=TRUE)
  
  spots.g1 <- merge(spots.g1.ma,spots.g1.one,by=c("Date","Campaign","Ring"))
  spots.g1$Date[spots.g1$Campaign == "2013Nov"] <- as.Date("2013-11-07")
  
  # 
  spots.by.campaign <- fit.g1.func(spots,byRing = FALSE)
  spots.by.campaign <- summaryBy(.~group,data = spots.by.campaign,
                                 FUN=mean,keep.names = T,id=~Date)
  # get tdr swc####
  if(file.exists('cache/swc.day.df.rds')){
    swc.day.df <- readRDS('cache/swc.day.df.rds')
  }else{
    source('r/process TDR.r')
  }
  
  # get leaf wp####
  leaf.wp.df <- downloadCSV("Gimeno_spots_Eter_EucFACE_waterPotential.csv")
  library(doBy)
  leaf.wp.df <- summaryBy(WP~Campaign+Ring+Measurement,
                          data = leaf.wp.df,FUN=mean,na.rm=TRUE,keep.names = TRUE,id=~CO2_Treat2)
  leaf.wp.df.long<- tidyr::spread(leaf.wp.df, "Measurement","WP")
  
  spots.g1.wp <- merge(spots.g1,leaf.wp.df.long,by=c("Campaign","Ring"),all.x=TRUE)
  
  leaf.wp.by.campaign <- summaryBy(Midday + Morning + Predawn~Campaign,
                                   data = leaf.wp.df.long,FUN=mean,na.rm=TRUE,keep.names = TRUE)
  
  spots.by.campaign.wp <- merge(spots.by.campaign,leaf.wp.by.campaign,by.y=c("Campaign"),by.x='group',all.x=TRUE)
  
  # put thing together####
  g1.swc.ls <- list()
  for(i in 1:6){
    g1.ring.df <- spots.g1.wp[spots.g1.wp$Ring == paste0("R",i),]
    
    swc.ring.df <- swc.neutron.ring.df[swc.neutron.ring.df$Ring == paste0("R",i),]
    
    r.out.ls <- list()
    
    for(j in 1:length(unique(g1.ring.df$Date))){
      diff.day <- swc.ring.df$Date - unique(g1.ring.df$Date)[j]
      min.diff <- min(abs(diff.day),na.rm=TRUE)
      if(min.diff < 14){
        sele.index <- which(abs(diff.day) == min.diff)[1]
      }else{
        sele.index <- NA
      }
      
      if(length(sele.index) > 0){
        r.out.ls[[j]] <- swc.ring.df[sele.index,]
        r.out.ls[[j]]$g1 <- g1.ring.df$g1[j]
        r.out.ls[[j]]$Campaign <- g1.ring.df$Campaign[j]
      }else{
        r.out.ls[[j]] <- NULL
      }
    }
    g1.swc.ls[[i]] <- do.call(rbind,r.out.ls)
  }
  
  # 
  see <- merge(spots.g1.wp,do.call(rbind,g1.swc.ls),by=c("Campaign","Ring","g1"),all=TRUE)
  names(see)[names(see) == 'Date.x'] <- "Date"
  names(see)[names(see) == 'Date.y'] <- "Date.neutron"
  g1.swc.df <- merge(see,swc.day.df,by =c("Date","Ring"))
  g1.swc.df$swc.0.30.tdr <- (g1.swc.df$swc.tdr.5 + g1.swc.df$swc.tdr.30)/2
  g1.swc.df <- g1.swc.df[!duplicated(g1.swc.df),]
  g1.swc.df$Campaign <- droplevels(g1.swc.df$Campaign)
  g1.swc.df$swc.100 <- g1.swc.df$vwc.300 / 100
  
  neutron.by.campaign <- summaryBy(swc.100 + vwc.300~Campaign,data = g1.swc.df,
                                   FUN=mean,na.rm=TRUE,keep.names = TRUE)
  
  g1.wp.swc.df.by.campaign <- merge(spots.by.campaign.wp,neutron.by.campaign,by.x='group',by.y='Campaign')
  g1.wp.swc.df.by.campaign$group <- droplevels(g1.wp.swc.df.by.campaign$group)

  # #####
  g1.swc.df.sub <- g1.swc.df#[g1.swc.df$Campaign != "2013Nov",]
  if(!file.exists('cache/ros.gs.rds')){
    source("r/get ros g1 swc.r")
  }else{
    eute.ros <- readRDS('cache/ros.gs.rds')
  }
  
  # 
  eute.ros$g1.norm <- eute.ros$g1 / (max(eute.ros$g1))

  
  eute.ros$g1.norm <- (eute.ros$g1 - min(eute.ros$g1)) / (max(eute.ros$g1) - min(eute.ros$g1))
  g1.swc.df.sub$g1.norm <- (g1.swc.df.sub$g1 - min(g1.swc.df.sub$g1)) / 
    (max(g1.swc.df.sub$g1) - min(g1.swc.df.sub$g1))
  
  # # fitting####
  startlist = list(swc.max=0.5,swc.min=0 ,q = 1)
  
  
  fit.nl.g1norm <- nls(g1.norm~((swc.100 - swc.min) / (swc.max - swc.min))^q, 
                       data = g1.swc.df.sub,
                       start=startlist,algorithm="port",
                       lower=c(0.1,0,0.01),upper=c(0.6,0.0001,3))
  
  test.df <- data.frame(g1 = c(eute.ros$g1,g1.swc.df.sub$g1),
                        swc = c(eute.ros$TDR,g1.swc.df.sub$swc.100))
  
  test.df <- test.df[test.df$g1 > 0,]
  test.df <- test.df[complete.cases(test.df),]
  
  fit.nl.tdr <- nls(g1 ~ 4.3*((swc - swc.min) / (swc.max - swc.min))^q, 
                    data = test.df,
                    start=startlist,algorithm="port",
                    lower=c(0.1,0,0.1),upper=c(2,0.1,3))
  
  test.df$g1.pred <- predict(fit.nl.tdr)
  test.df$g1.actual.pred <- test.df$g1.pred * (max(test.df$g1)) 
  
  # plot######
  par(mar=c(5,5,1,1))
  plot(g1~swc.100,data = g1.swc.df.sub,pch=16,col=Campaign,xlim=c(0,0.5),ylim=c(0,6),
       xlab=expression(theta),ylab=expression(g[1]~(kPa^0.5)))
  points(g1~(TDR),data =eute.ros,pch=16,col="grey")
  lines(g1.pred ~ swc,data=test.df[order(test.df$swc),],col="black",lwd=2)
  theta.vec <- seq(0,0.5,0.001)
  g1.drake <- (theta.vec/0.6)^0.32*(max(test.df$g1) - min(test.df$g1)) + min(test.df$g1)
  lines(g1.drake ~ theta.vec,
        col="black",lwd=2,lty='dashed')
  legend("bottomright",legend = c("Drake et al., 2017a","All"),lty=c("dashed","solid",lwd=2),bty='n')
  legend("topleft",legend = c(levels(g1.swc.df.sub$Campaign)),
         col=palette(),pch=c(16,16,16,16),ncol = 2,bty='n')
  
  title(paste0("Top ",depth.in,' cm'))
}

depth.vec <- c(25,50,75,100,200,300)
par(mfrow=c(2,3))
for(i in seq_along(depth.vec)){
  plot.g1.swc.func(depth.vec[i])
}

