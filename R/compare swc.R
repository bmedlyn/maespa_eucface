ring.sel <- 4

# get teresa's spot measurements
spots.df <- read.csv("Gimeno_spot_Eter_gasExchange6400.csv")
# spots.r1.df <- spots.df[spots.df$Ring == "R1",]
spots.r1.df <- spots.df

# read daily fluxes############
data.both.sap<- readRDS("mastra and sap.rds")
# get start and end day
con.ls <- readLines("Rings/Ring1/runfolder/confile.dat")

sd <- gsub("startdate = ","",
           con.ls[grep("startdate", con.ls)])
sd <- gsub("'","",sd)

s.date <- as.Date(sd,"%d/%m/%y")

ed <- gsub("enddate = ","",
           con.ls[grep("enddate", con.ls)])
ed <- gsub("'","",ed)

e.date <- as.Date(ed,"%d/%m/%y")

# s.date <- as.Date("2013-11-26")
# e.date <- as.Date("2014-03-26")

# 
# get swc from hiev
swc.df <- downloadTOA5("FACE_R1_B1_SoilVars",
                       startDate = s.date,
                       endDate = e.date)

swc.df <- subset(swc.df,select = c("Date",
                                   "DateTime",
                                   "Theta5_1_Avg","Theta5_2_Avg",
                                   "Theta30_1_Avg","Theta30_2_Avg",
                                   "Theta75_1_Avg","Theta75_2_Avg"))

swc.df$swc.0.5 <- (swc.df$Theta5_1_Avg + swc.df$Theta5_2_Avg)/2

swc.df$swc.5.30 <- (swc.df$Theta30_1_Avg +swc.df$Theta30_2_Avg)/2

swc.df$swc.30.75 <- (swc.df$Theta75_1_Avg + swc.df$Theta75_2_Avg)/2

swc.day.df <- data.table(swc.df)[,list(swc.5 = mean(swc.0.5, na.rm=TRUE),
                                       swc.30 = mean(swc.5.30, na.rm=TRUE),
                                       swc.75 = mean(swc.30.75, na.rm=TRUE)),
                                 by = Date]

swc.day.df$swc.5 <- swc.day.df$swc.30/100
swc.day.df$swc.30 <- swc.day.df$swc.30/100
swc.day.df$swc.75 <- swc.day.df$swc.75/100
# vwc.df <- read.csv("FACE_RA_P0037_DAILYMET_20110619-20151026_L2.csv")
# vwc.df <- na.omit(vwc.df)
# vwc.df$Date <- as.Date(vwc.df$Date)

# sap.hr <- readRDS("sap_hrly.rds")  
# sap.hr <- subset(sap.hr,select = c("DateHour","Ring","volRing"))

# read watbal
fn <- sprintf("Rings/Ring%s/runfolder/watbal.dat",ring.sel)
watbal <- read.table(fn,head=FALSE, skip=37)

# note the et here is evapotranspiration not just tranpiration
names(watbal) <- c("day", "hour", "wsoil", "wsoilroot", "ppt", "canopystore",
                   "evapstore", "drainstore", "tfall", "et", "etmeas",
                   "discharge", "overflow",  "weightedswp", "ktot", "drythick", "soilevap",
                   "soilmoist", "fsoil", "qh", "qe", "qn", "qc", "rglobund",
                   "rglobabv", "radinterc", "rnet", "totlai", "tair", "soilt1", "soilt2",
                   "fracw1", "fracw2", "fracaPAR")

# watbal.sub <- subset(watbal,select = c("day", "hour", "et"))
# names(watbal.sub) <- c("day", "half.hour", "et")
# watbal.sub$Hour <- rep(rep(seq(1,24),each=2),
#                        length(watbal.sub$half.hour)/48)
# 
# watbal.sub <- data.table(watbal.sub)[,list(et=mean(et,na.rm=TRUE)),
#                                  by = c("day","Hour")]
# 
# watbal.sub$DateHour <- rep(seq.POSIXt(as.POSIXlt("2012-10-26 00:00"),
#                                   as.POSIXlt("2013-03-26 24:00"),
#                                   by = "hour"),each=2)

# get daily water balance
watbal.sum <- data.table(watbal)[,list(swc.1=mean(fracw1, na.rm=TRUE),
                                       swc.2=mean(fracw2, na.rm=TRUE),
                                       tfall = sum(tfall, na.rm=TRUE),
                                       et = sum(et, na.rm=TRUE),
                                       discharge = sum(discharge, na.rm=TRUE),
                                       ppt = sum(ppt, na.rm=TRUE),
                                       soil.w = mean(wsoil,na.rm = TRUE),
                                       r.soil.w = mean(wsoilroot,na.rm = TRUE),
                                       soil.e = sum(soilevap,na.rm=TRUE)),
                                 by = day]
watbal.sum$Date <- seq(s.date,e.date,by="1 day")

# put daily data together
data.both.sap.war <- merge(data.both.sap,watbal.sum,by = intersect(names(data.both.sap), names(watbal.sum)))

data.both.sap.war <- merge(data.both.sap.war,swc.day.df,by = intersect(names(data.both.sap.war), names(swc.day.df)))

data.both.sap.war <- data.both.sap.war[data.both.sap.war$volRing < 100,]

data.both.sap.war <- data.both.sap.war[data.both.sap.war$Ring == paste0("R",ring.sel),]

data.both.sap.war$t.level <- cut(data.both.sap.war$TAIR,breaks = c(15,20,25,30,35))


#####
# get hrly data##############
data.both.sap.hr <- readRDS("mastra and sap hr.rds")

watbal$HOUR <- ceiling(watbal$hour/2)

watbal.hr <- data.table(watbal)[,list(swc.1=mean(fracw1, na.rm=TRUE),
                                      swc.2=mean(fracw2, na.rm=TRUE),
                                      tfall = sum(tfall, na.rm=TRUE),
                                      et = sum(et, na.rm=TRUE),
                                      ppt = sum(ppt, na.rm=TRUE),
                                      discharge = sum(discharge, na.rm=TRUE),
                                      r.soil.w = mean(wsoilroot,na.rm = TRUE),
                                      soil.w = mean(wsoil,na.rm = TRUE),
                                      soil.e = sum(soilevap,na.rm=TRUE)),
                                by = c("day","HOUR")]

s.dt <- sprintf("%s 00:00",s.date)
e.dt <- sprintf("%s 23:00",e.date)

watbal.hr$DateTime <- rep(seq(ymd_hm(s.dt),ymd_hm(e.dt),by="1 hour"),each=1)
watbal.hr$Ring <- paste0("R",ring.sel)

# get hrly swc 
swc.df$Date.Time <- ymd_h(substr(as.character(swc.df$DateTime),1,13))

swc.hr.df <- data.table(swc.df)[,list(swc.5 = mean(swc.0.5, na.rm=TRUE),
                                      swc.30 = mean(swc.5.30, na.rm=TRUE),
                                      swc.75 = mean(swc.30.75, na.rm=TRUE)),
                                 by = Date.Time]
names(swc.hr.df)[1] <- "DateTime"

swc.hr.df$swc.5 <- swc.hr.df$swc.5/100
swc.hr.df$swc.30 <- swc.hr.df$swc.30/100
swc.hr.df$swc.75 <- swc.hr.df$swc.75/100

# put hr data together
temp <- merge(data.both.sap.hr,
              watbal.hr,
              by = intersect(names(data.both.sap.hr), names(watbal.hr)))
# watbal.hr$DateTime[1] - data.both.sap.hr$DateTime[data.both.sap.hr$Ring == "R1"][1]

data.all.war <- merge(temp,swc.hr.df,by = intersect(names(temp), names(swc.hr.df)))

data.all.war <- data.all.war[data.all.war$sap < 100,]

data.all.war <- data.all.war[data.all.war$Ring == paste0("R",ring.sel),]
# plot(data.both.sap.war$trans~data.both.sap.war$volRing,
#      xlim=c(0,4),
#      ylim=c(0,4))
# abline(a=0,b=1)

data.all.war$t.level <- cut(data.all.war$TAIR,breaks = c(9,18,27,36,45))
data.all.war$Date <- as.Date(data.all.war$DateTime)
lai.r1.df <- sm[[ring.sel]][,1:2]
names(lai.r1.df) <- c("Date","LAI")

data.all.war <- merge(data.all.war,lai.r1.df,by="Date")

data.all.war$a.leaf.m <- data.all.war$Photo * data.all.war$LAI

saveRDS(data.all.war,"all.hr.rds")

# df = data.all.war
#####
# plot hourly##########
plot.hrly.func <- function(df,pdf.fn = "maespa trans vs hp hrly.pdf"){
  on.exit(dev.off())
  pdf(pdf.fn,width = 8,height = 6)
# df = data.all.war
  # plot trans
  # plot with date
  par(mar=c(5,5,2,5))
  plot(df$trans~df$Date,
       ylim=c(0,0.3),col="red",pch=16,
       xlab=" ",
       ylab=" ",
       cex=0.5)
  par(new=1)
  plot(df$sap~df$Date,
       xlab="Date",
       ylab="Sap flow (L/hr)",
       ylim=c(0,0.3),
       col="blue",
       pch=16,
       cex=0.5)
  
  legend("topright",legend = c("MAESPA","Heat Pulse"),pch=16,
         col=c("red","blue"),bty='n')
  
  title(sprintf("sap flow from %s to %s",as.character(s.date),as.character(e.date)))

  # scatter plot with r2
  palette(c("cadetblue1","cyan2","darkgoldenrod1","brown3"))
  plot(df$trans~df$sap,
       xlim=c(0,0.3),
       ylim=c(0,0.3),
       pch=16,
       cex=0.8,
       xlab=expression(Sap~flow~from~heat~pulse~(L~hr^-1)),
       ylab=expression(Sap~flow~MAESPA~(L~hr^-1)),
       col=df$t.level)
  abline(a=0,b=1)
  fit.sap <- lm(df$trans~df$sap)
  abline(fit.sap,col="navy",lty="dashed")
  adj.r.sqrt <- summary(fit.sap)$adj.r.squared
  mylabel <- bquote(italic(R)^2 == .(format(adj.r.sqrt, digits = 2)))
  text(x = 0.2, y = 0.3, labels = mylabel)
  
  legend("bottomright",legend = levels(df$t.level),
         col=palette(),
         pch=16,
         bty='n',
         title = "Tair")
  
  # tran vs vpd
  plot(df$trans~df$VPD,
       xlim=c(0,10),
       ylim=c(0,0.3),col="red",pch=16,cex=0.5,
       xlab="VPD (kPa)",
       ylab="Transpiration (L/hr)")
  
  par(new=1)
  plot(df$sap~df$VPD,
       xlim=c(0,10),
       ylim=c(0,0.3),col="blue",pch=16,cex=0.5,
       xlab=" ",
       ylab=" ")
  
  legend("topright",legend = c("MAESPA","Heat Pulse"),pch=16,
         col=c("red","blue"),bty='n')
  title("Trans vs VPD (Hourly)")
  # tran vs par
  plot(df$trans~df$PAR,
       xlim=c(0,2),
       ylim=c(0,0.3),col="red",pch=16,cex=0.5,
       xlab=expression(PAR~(MJ~m^-2~hr^1)),
       ylab="Transpiration (L/hr)")
  
  par(new=1)
  plot(df$sap~df$PAR,
       xlim=c(0,2),
       ylim=c(0,0.3),col="blue",pch=16,cex=0.5,
       xlab=" ",
       ylab=" ")
  
  legend("topright",legend = c("MAESPA","Heat Pulse"),pch=16,
         col=c("red","blue"),bty='n')
  title("Trans vs PAR (Hourly)")
  
  # tran vs tair
  plot(trans~TAIR,
       data=df,
       xlim=c(9,45),
       ylim=c(0,0.3),col="red",pch=16,cex=0.5,
       xlab=expression("T"[air]~(degree*C)),
       ylab="Transpiration (L/hr)")
  
  par(new=1)
  plot(sap~TAIR,
       data=df,
       xlim=c(9,45),
       ylim=c(0,0.3),col="blue",pch=16,cex=0.5,
       xlab=" ",
       ylab=" ")
  
  legend("topright",legend = c("MAESPA","Heat Pulse"),pch=16,
         col=c("red","blue"),bty='n')
  title("Trans vs Tair (Hourly)")
  
  
 

  # photo vs t##############
  df.sort <- df[order(df$TAIR),]
  df.sort$a.leaf.m[df.sort$a.leaf.m < 1] <- NA
  plot(df.sort$a.leaf.m~df.sort$TAIR,
       xlab=expression("T"[air]~(degree*C)),
       ylab=expression("Assimilation rate"~(mu*mol~C~m^-2~leaf~s^-1)),
       xlim=c(10,40),
       ylim=c(0,40),
       col="darksalmon",
       pch=16,
       cex=0.2)
  par(new=1)
  plot(spots.r1.df$Photo~spots.r1.df$Tair,
       xlab=expression("T"[air]~(degree*C)),
       ylab=expression("Assimilation rate"~(mu*mol~C~m^-2~leaf~s^-1)),
       xlim=c(10,40),
       ylim=c(0,40),
       col="dodgerblue4",
       pch=16,
       cex=0.5
       )
  par(new=1)
  # df.sort <- df.sort[hour(df.sort$DateTime) %in% seq(8,17),]
  df.sub <- df.sort[is.na(df.sort$a.leaf.m) == 0,]
  maespa.fit <- gam(a.leaf.m~s(TAIR),data = df.sub)
  maespa.pre <- predict(maespa.fit,data=df.sub$TAIR)
  plot(maespa.pre~(df.sub$TAIR),
       type="l",
       lwd = 2,
       col="darkred",
       xlab="",
       ylab="",
       xlim=c(10,40),
       ylim=c(0,40))
  par(new=1)
  spots.r1.df <- spots.r1.df[order(spots.r1.df$Tair),]
  spots.fit <- gam(Photo~s(Tair),data = spots.r1.df)
  spots.pre <- predict(spots.fit,data=spots.r1.df$Tair)
  plot(spots.pre~na.omit(spots.r1.df$Tair),
       type="l",
       lwd = 2,
       col="navy",
       xlab="",
       ylab="",
       xlim=c(10,40),
       ylim=c(0,40))
  
  legend("topright",legend = c("MAESPA","Spots"),pch=16,
         col=c("darkred","navy"),bty='n')
  title("Photo vs Tair (Hourly)")
  
  # plot swc############
par(mfrow=c(3,1),mar=c(0,5,1,1))
  # maespa top 2 layers
  # par(new=1)
  plot(df$swc.1~df$DateTime,
       ylim=c(0,0.4),
       col="red",
       type="l",
       lwd =1.2,
       xlab="Date",
       ylab="Volumetric SWC",
       cex=0.6,
       ann=FALSE,
       axes = FALSE
  )
  # measured 0-5 5-30 and 30-57cm
  par(new=1)
  plot(df$swc.5~df$DateTime,
       xlab="Date",
       ylab="Volumetric SWC",
       ylim=c(0,0.4),
       col="darkgreen",
       type="l",
       cex=0.6,
       ann=FALSE,
       axes = FALSE)

  par(new=1)
  plot(df$swc.30~df$DateTime,
       xlab="Date",
       ylab="Volumetric SWC",
       ylim=c(0,0.4),
       col="darkseagreen",
       type="l",
       cex=0.6,
       ann=FALSE,
       axes = FALSE)
  axis(2,at = seq(0,0.4,0.1),paste0(seq(0,0.4,0.1)))
  abline(h=0,col="grey90")
  abline(h=0.2,col="grey90")
  abline(h=0.4,col="grey90")
  mtext("Volumetric SWC (top; %)",side = 2,line = 3,col="black",cex=0.9)
  legend("topright",
         legend = c("MAESPA 50cm","Measured 5 cm","Measured 30 cm"),
         pch=16,
         col=c("red","darkgreen","darkseagreen"),
         bty='n',
         horiz = TRUE)
  title(sprintf("SWC of the top 75cm from %s to %s",as.character(s.date),as.character(e.date)))
  # deeper layer
  # par(new=1)
  plot(df$swc.2~df$DateTime,
       ylim=c(0,0.4),
       col="coral",
       pch=16,
       xlab="Date",
       ylab="Volumetric SWC",
       type="l",
       lwd=1.2,
       ann=FALSE,
       axes = FALSE
  )
  
  par(new=1)
  plot(df$swc.75~df$DateTime,
       xlab="Date",
       ylab="Volumetric SWC",
       ylim=c(0,0.4),
       col="darkolivegreen1",
       type="l",
       cex=0.6,
       ann=FALSE,
       axes = FALSE)
  
  axis(2,at = seq(0,0.4,0.1),paste0(seq(0,0.4,0.1))) 
  abline(h=0,col="grey90")
  abline(h=0.2,col="grey90")
  abline(h=0.4,col="grey90")
  mtext("Volumetric SWC (deep; %)",side = 2,line = 3,col="black",cex=0.9)
  
  legend("topright",
         legend = c("MAESPA 100cm","Measured 75 cm"),
         pch=16,
         col=c("coral","darkolivegreen1"),
         bty='n',
         horiz = TRUE)
  
  par(mar=c(5,5,1,1))
  # hourly rainfall 
  plot(df$ppt * 2~df$DateTime,
       xlab="Date",
       ylab="PPT",
       type="s",
       ylim=c(0,50),
       col="deepskyblue",
       pch=16,
       ann=0,
       yaxt = "n")
  axis(2,at=seq(0,50,10),labels = paste0(seq(0,50,10)),col="deepskyblue",col.axis="deepskyblue")
  mtext("PPT (mm/hr)",side = 2,line = 3,col="deepskyblue",cex=0.9)

}
# df = data.all.war[hour(data.all.war$DateTime) %in% seq(7,17),]
# df = df[df$Ring == "R1",]
plot.hrly.func(data.all.war)

plot(Photo~DateTime,data=data.all.war[data.all.war$Date == data.all.war$Date[100]],type="l")

# plot.hrly.func(data.all.war)
# sum(df$sap)/2
# sum(df$et)/2
# df <- data.all.war[as.Date(data.all.war$DateTime) %in% c(as.Date("2014-01-02"),as.Date("2014-01-03")),]
# par(mar=c(5,5,2,5))
# plot(df$et~df$Date,
#      ylim=c(0,0.5),
#      col="red",
#      pch=16,
#      xlab=" ",
#      ylab=" ",
#      cex=0.5)
# par(new=1)
# plot(df$sap~df$Date,
#      xlab="Date",
#      ylab="Sap flow (L/hr)",
#      ylim=c(0,0.5),
#      col="blue",
#      pch=16,
#      cex=0.5)
# 
# legend("topright",legend = c("MAESPA","Heat Pulse"),pch=16,
#        col=c("red","blue"),bty='n')
# title("sap flow")

#####
# plot daily###############
plot.daily.func <- function(data.both.sap.war){

  on.exit(dev.off())
  pdf("maespa trans vs hp.pdf")
  # plot trans
  # plot with date
  par(mar=c(5,5,2,5))
  plot(data.both.sap.war$et~data.both.sap.war$Date,
       ylim=c(0,4),col="red",pch=16,
       xlab=" ",
       ylab=" ",
       cex=0.5)
  par(new=1)
  plot(data.both.sap.war$volRing~data.both.sap.war$Date,
       xlab="Date",
       ylab="Sap flow (L d-1)",
       ylim=c(0,4),
       col="blue",
       pch=16,
       cex=0.5)
  
  legend("topright",legend = c("MAESPA","Heat Pulse"),pch=16,
         col=c("red","blue"),bty='n')
  title("sap flow")
  # scatter plot with r2
  palette(c("cadetblue1","cyan2","darkgoldenrod1","brown3"))
  plot(data.both.sap.war$et~data.both.sap.war$volRing,
       xlim=c(0,4),
       ylim=c(0,4),
       pch=16,
       cex=0.8,
       xlab=expression(Sap~flow~from~heat~pulse~(L~d^-1)),
       ylab=expression(Sap~flow~MAESPA~(L~d^-1)),
       col=data.both.sap.war$t.level)
  abline(a=0,b=1)
  fit.sap <- lm(data.both.sap.war$et~data.both.sap.war$volRing)
  abline(fit.sap,col="navy",lty="dashed")
  adj.r.sqrt <- summary(fit.sap)$adj.r.squared
  mylabel <- bquote(italic(R)^2 == .(format(adj.r.sqrt, digits = 2)))
  text(x = 3.5, y = 3, labels = mylabel)
  
  legend("bottomright",legend = levels(data.both.sap.war$t.level),
         col=palette(),
         pch=16,
         bty='n',
         title = "Tair")
  
  # tran vs vpd
  plot(data.both.sap.war$et~data.both.sap.war$VPD,
       xlim=c(0,3),
       ylim=c(0,4),col="red",pch=16,cex=0.5,
       xlab="VPD (kPa)",
       ylab="Transpiration (mm/d)")
  
  par(new=1)
  plot(data.both.sap.war$volRing~data.both.sap.war$VPD,
       xlim=c(0,3),
       ylim=c(0,4),col="blue",pch=16,cex=0.5,
       xlab=" ",
       ylab=" ")
  
  legend("topright",legend = c("MAESPA","Heat Pulse"),pch=16,
         col=c("red","blue"),bty='n')
  title("Trans vs VPD (Daily)")
  
  
  
  # plot swc
  # maespa top 2 layers
  plot(data.both.sap.war$swc.1~data.both.sap.war$Date,
       ylim=c(0,0.45),
       col="red",
       pch=16,
       xlab="Date",
       ylab="Volumetric SWC",
       cex=0.6
  )
  par(new=1)
  plot(data.both.sap.war$swc.2~data.both.sap.war$Date,
       ylim=c(0,0.45),
       col="coral",
       pch=16,
       xlab="Date",
       ylab="Volumetric SWC",
       cex=0.6
  )
  # measured 0-30 and 30-57cm
  par(new=1)
  plot(data.both.sap.war$swc.30~data.both.sap.war$Date,
       xlab="Date",
       ylab="Volumetric SWC",
       ylim=c(0,0.45),
       col="darkgreen",
       pch=16,
       cex=0.6)
  
  par(new=1)
  plot(data.both.sap.war$swc.75~data.both.sap.war$Date,
       xlab="Date",
       ylab="Volumetric SWC",
       ylim=c(0,0.45),
       col="darkolivegreen1",
       pch=16,
       cex=0.6)
  # # plot mean swc
  # par(new=1)
  # plot((4*data.both.sap.war$swc.30 + 2*data.both.sap.war$swc.75)/6~data.both.sap.war$Date,
  #      xlab="Date",
  #      ylab="Volumetric SWC",
  #      ylim=c(0,0.45),
  #      col="black",
  #      type="l",
  #      cex=0.6)
  
  # rainfall
  par(new=1)
  plot(data.both.sap.war$ppt~data.both.sap.war$Date,
       xlab="Date",
       ylab="PPT",
       type="s",
       ylim=c(0,120),
       col="deepskyblue",
       pch=16,
       ann=0,
       axes=0)
  axis(4,at=seq(0,120,30),labels = paste0(seq(0,120,30)),col="deepskyblue",col.axis="deepskyblue")
  mtext("PPT (mm)",side=4,line = 3,col="deepskyblue")
  
  legend("topright",
         legend = c("MAESPA top1","MAESPA top2","Measured 5&30 cm","Measured 75 cm"),
         pch=16,
         col=c("red","coral","darkgreen","darkolivegreen1"),
         bty='n')
  title(sprintf("SWC of the top 75cm from %s to %s",as.character(s.date),as.character(e.date)))
  
  # plot(data.both.sap.war$swc.v~data.both.sap.war$VWC,
  #      xlim=c(0,0.3),
  #      ylim=c(0,0.3),
  #      xlab="Measured",
  #      ylab="MAESPA")
  # title("SWC of the top 30cm")
}

plot.daily.func(data.both.sap.war)




#####
# with(watbal,plot(et~radinterc,pch=16,cex=0.5))##############
# with(data.all.war,plot(et~trans,pch=16,cex=0.5,xlim=c(0,0.2),ylim=c(0,0.2)))
# abline(a=0,b=1,col="red")

# with(data.all.war,plot(Photo~trans,pch=16,cex=0.5))
# data.all.war$et/data.all.war$trans



pdf("maespa.photosyn.pdf")
library(plantecophys)
# spots measuremenres and modelled photo with met from spots
spots <- read.csv("Gimeno_spot_Eter_gasExchange6400.csv")

spots <- spots[which(is.na(spots$Photo) == FALSE),]
modelled <- Photosyn(VPD = spots$VpdL, 
                     Ca = spots$CO2R, 
                     PPFD = spots$PARi, 
                     Tleaf = spots$Tleaf,
                     gsmodel = c("BBOpti"), 
                     g1 = 4,
                     theta = 0.4, 
                     Jmax = 133, 
                     Vcmax = 88, 
                     Rd = 0.92, 
                     Q10 = 0.067,
                     TrefR = 25, 
                     EaV = 47590, 
                     EdVC = 2e+05, 
                     delsC = 640, 
                     EaJ = 37259,
                     EdVJ = 2e+05, 
                     delsJ = 640)

plot(modelled$ALEAF~modelled$Tleaf,
     xlim=c(15,40),ylim=c(5,40),
     xlab="Tleaf",ylab="Photo",
     pch=16,col="coral",
     cex=0.5)
par(new=TRUE)
plot(spots$Photo~spots$Tleaf,
     xlim=c(15,40),ylim=c(5,40),
     ann=FALSE,axes=FALSE,
     pch=16,col="lightskyblue")
legend("topright",legend = c("Photosyn","Spots (Teresa)"),pch=c(16),col=c("coral","lightskyblue"))

# photosyn with maespa met and spots
photo.syn <- Photosyn(VPD = data.all.war$VPD, 
                      Ca = data.all.war$CA, 
                      PPFD = data.all.war$PAR * 4.56 * 1e6 / 3600, 
                      Tleaf = data.all.war$TAIR,
                      gsmodel = c("BBOpti"), 
                      g1 = 4.2,
                      theta = 0.4, 
                      Jmax = 113, 
                      Vcmax = 82, 
                      Rd = 0.92, 
                      Q10 = 0.067,
                      TrefR = 25, 
                      EaV = 47590, 
                      EdVC = 2e+05, 
                      delsC = 640, 
                      EaJ = 37259,
                      EdVJ = 2e+05, 
                      delsJ = 640)

plot(ALEAF~Tleaf,
     data = photo.syn,
     xlim=c(15,40),ylim=c(5,40),
     col="coral",pch=16,
     cex=0.5)
par(new=TRUE)
plot(Photo~Tleaf,
     data = spots.df,
     xlim=c(15,40),
     ylim=c(5,40),
     col="lightskyblue",
     pch=16,
     ann=FALSE,
     axes=FALSE)
legend("topright",legend = c("Photosyn","Spots (Teresa)"),pch=c(16),col=c("coral","lightskyblue"))

# maespa photo and spots
df <- data.all.war
df.sort <- df[order(df$TAIR),]
df.sort$a.leaf.m[df.sort$a.leaf.m < 1] <- NA
plot(df.sort$a.leaf.m~df.sort$TAIR,
     xlab=expression("T"[air]~(degree*C)),
     ylab=expression("Assimilation rate"~(mu*mol~C~m^-2~leaf~s^-1)),
     xlim=c(15,40),
     ylim=c(5,40),
     col="red",
     pch=16,
     cex=0.5)
par(new=1)
plot(spots.r1.df$Photo~spots.r1.df$Tair,
     xlab=expression("T"[air]~(degree*C)),
     ylab=expression("Assimilation rate"~(mu*mol~C~m^-2~leaf~s^-1)),
     xlim=c(15,40),
     ylim=c(5,40),
     col="lightskyblue",
     pch=16,
     cex=1)
par(new=1)
# df.sort <- df.sort[hour(df.sort$DateTime) %in% seq(8,17),]
df.sub <- df.sort[is.na(df.sort$a.leaf.m) == 0,]
maespa.fit <- gam(a.leaf.m~s(TAIR),data = df.sub)
maespa.pre <- predict(maespa.fit,data=df.sub$TAIR)
plot(maespa.pre~(df.sub$TAIR),
     type="l",
     col="red",
     xlab="",
     ylab="",
     xlim=c(15,40),
     ylim=c(5,40))
par(new=1)
spots.r1.df <- spots.r1.df[order(spots.r1.df$Tair),]
spots.fit <- gam(Photo~s(Tair),data = spots.r1.df)
spots.pre <- predict(spots.fit,data=spots.r1.df$Tair)
plot(spots.pre~na.omit(spots.r1.df$Tair),
     type="l",
     col="lightskyblue",
     xlab="",
     ylab="",
     xlim=c(15,40),
     ylim=c(5,40))

legend("topright",legend = c("MAESPA","Spots"),pch=16,
       col=c("red","lightskyblue"),bty='n')
title("Photo vs Tair (Hourly)")


peaked <- function(k25 = 113, Ea = 47.59, Ed = 200, 
                   Rgas = 0.008314, delS = 0.640, TTK = 293.15) {
  fn <- k25 * exp((Ea*(TTK - 298.15))/(298.15*Rgas*TTK)) * 
    (1+exp((298.15*delS - Ed)/(298.15*Rgas))) / 
    (1+exp((TTK*delS-Ed)/(TTK*Rgas)))
  return(fn)
}


t.vec <- seq(5,40)
tk.vec <- 298.15 + t.vec


j.vec <- peaked(k25 = 150,Ea = 37.27,TTK = tk.vec)
plot(j.vec~t.vec)
dev.off()