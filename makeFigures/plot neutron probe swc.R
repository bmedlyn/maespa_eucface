
swc.neutron.df <- read.csv("data/FACE_P0018_RA_NEUTRON_20120430-20180215_L1.csv")
swc.neutron.df <- swc.neutron.df[lubridate::year(as.Date(swc.neutron.df$Date)) %in% seq(2013,2016),]
swc.neutron.df$Depth <- -swc.neutron.df$Depth
swc.neutron.df <- swc.neutron.df[swc.neutron.df$Location != "Outside",]
swc.neutron.df$Date <- as.Date(as.character(swc.neutron.df$Date))
swc.neutron.df$Ring <- as.character(swc.neutron.df$Ring)

swc.neutron.ring.df <- doBy::summaryBy(VWC ~ Date + Depth,
                                       data = swc.neutron.df,FUN=mean,na.rm=TRUE,
                                       keep.names = TRUE,id=~CO2)

depth.df <- data.frame(Date = rep(unique(swc.neutron.ring.df$Date),each=length(seq(-450,-25,1))),
                       Depth = rep(seq(-450,-25,1),length(unique(swc.neutron.ring.df$Date))))

swc.depth.df <- merge(depth.df,swc.neutron.ring.df,all=TRUE)
swc.depth.df$VWC <- zoo::na.approx(swc.depth.df$VWC)
# swc.depth.df$Depth.rev <-  - swc.depth.df$Depth

see <- matrix(swc.depth.df$VWC,
              ncol = length(unique(swc.depth.df$Date)),
              nrow = length(unique(swc.depth.df$Depth)))

# library(raster)
# par(mfrow=c(1,1),
#     oma=c(1,5,1,1),
#     mar=c(5,5,5,5))
raster.col.fun <- colorRampPalette(c("coral","lightskyblue"))
brks <- pretty(0:35,4)
arg <- list(at=brks, labels=paste0((brks)))
plot(raster(see[nrow(see):1,], xmn=0, xmx=length(unique(swc.depth.df$Date)),
            ymn=-450, ymx=-25),
     asp = 66/426 * 2/8,
     breaks=brks,
     lab.breaks =  brks,
     col=c(raster.col.fun(length(brks) - 1)),
     box=FALSE,xaxt='n',
     axis.args = arg)
# axis(side=1,at=seq(0,1,length.out = 98),labels = paste0(seq(0,97,1)))
# axis(side=2,at=seq(-450,-25,50),labels = paste0(seq(-450,-25,50)))
par(mfrow=c(2,2))
# layout(matrix(c(1,2,1,2),nrow=2,ncol=2),widths = rep(3,4),heights = rep(3,4))
layout.show(n = 1)
par(mar=c(2,4,1,1),fin=c(8,6))
raster.col.fun <- colorRampPalette(c("coral","lightskyblue"))
Depth.vec <- rev(-unique(swc.depth.df$Depth))
filled.contour(x = unique(swc.depth.df$Date),
              y = unique(swc.depth.df$Depth),
               z = t(see),
               nlevels = 7,
               col=(raster.col.fun(7)),
              ylab="Depth (cm)",
              key.title = title(main = "SWC (%)",cex.main=1,font.main= 1))

# 
swc.neutron.byring.df <- doBy::summaryBy(VWC ~ Date + Ring ,
                                       data = swc.neutron.df,FUN=mean,na.rm=TRUE,
                                       keep.names = TRUE,id=~CO2)
swc.neutron.byring.df$Ring <- as.factor(swc.neutron.byring.df$Ring)
palette(c("brown1","cadetblue1","cadetblue3",
          "coral","brown4","deepskyblue"
))
plot(VWC~Date,data = swc.neutron.byring.df,col=Ring,pch=16)
legend("top",legend = levels(swc.neutron.byring.df$Ring),col=palette(),pch=16,
       horiz = T,bty='n',xpd=T,inset = -0.1)




# 

# HIEv R package will download files to here:
library(HIEv)
download.path <- file.path("download/")
setToPath(download.path)

# read daily fluxes############
data.both.sap<- readRDS("output/maespaVPD/mastra and sap.rds")
# get start and end day
con.ls <- readLines("Rings/Ring4/runfolder/confile.dat")

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
swc.df.ls <- list()
for(i in 1:6){
  temp.df <- downloadTOA5(sprintf("FACE_R%s_B1_SoilVars",i),
                       startDate = s.date,
                       endDate = e.date)
  
  # from remko
  meanVWC <- function(dfr){
    vwccols <- grep("VWC_",names(dfr))
    dfr <- dfr[,vwccols]
    dfr[dfr > 1] <- NA
    rowMeans(dfr, na.rm=TRUE)
  }
  
 
  swc.df.ls[[i]] <- subset(temp.df,select = c("Date",
                                         "DateTime"))
  
  swc.df.ls[[i]]$swc.tdr <- meanVWC(temp.df) * 100
  
  swc.df.ls[[i]]$Ring <- paste0('R',i)
}

swc.tdr.df <- do.call(rbind,swc.df.ls)
swc.tdr.df$Ring <- as.factor(swc.tdr.df$Ring )

palette(c("brown1","cadetblue1","cadetblue3",
          "coral","brown4","deepskyblue"))

plot(swc.tdr~Date,data = swc.tdr.df[year(swc.tdr.df$Date) == 2013,],col=Ring,pch=16)
legend("top",legend = levels(swc.neutron.byring.df$Ring),col=palette(),pch=16,
       horiz = T,bty='n',xpd=T,inset = -0.1)
# 
swc.50.df$Ring <- as.factor(swc.50.df$Ring)
plot(g1~Date,data = swc.50.df[year(swc.50.df$Date) == 2013,],col=Ring,pch=16)
legend("top",legend = levels(swc.neutron.byring.df$Ring),col=palette(),pch=16,
       horiz = T,bty='n',xpd=T,inset = -0.1)
plot(VWC~Date,data = swc.50.df[year(swc.50.df$Date) == 2013,],col=Ring,pch=16)
legend("top",legend = levels(swc.neutron.byring.df$Ring),col=palette(),pch=16,
       horiz = T,bty='n',xpd=T,inset = -0.1)
