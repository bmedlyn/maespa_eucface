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
swc.df <- downloadTOA5("FACE_R4_B1_SoilVars",
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

swc.day.df$swc.5 <- swc.day.df$swc.5/100
swc.day.df$swc.30 <- swc.day.df$swc.30/100
swc.day.df$swc.75 <- swc.day.df$swc.75/100
swc.day.df <- swc.day.df[order(swc.day.df$Date),]
plot(swc.5~Date,data = swc.day.df,ylim=c(0,0.4),type="s",col="red",
     ylab=expression(SWC))
points(swc.30~Date,data = swc.day.df,type="s",col="navy")


fill.func <- function(swc.5,swc.30,out.len){
  swc.df <- data.frame(swc.5=swc.5,
                       swc.30=swc.30)
  swc.df[swc.df < 0] <- 0
  swx.m <- apply(swc.df,1,function(x)seq(x[1],x[2],length.out = out.len))
  # seq(swc.5[1],swc.5[1],length.out = out.len)
  return(swx.m)
}

see <- fill.func(swc.day.df$swc.5,swc.day.df$swc.30,30)
library(raster)
par(mfrow=c(1,1),
    oma=c(1,5,1,1),
    mar=c(5,5,5,5))
raster.col.fun <- colorRampPalette(c("coral","lightskyblue"))
brks <- seq(0,0.4,length.out = 9)
arg <- list(at=brks, labels=paste0((brks)))
plot(raster(see, xmn=0, xmx=1458, ymn=-30, ymx=0),asp = 1458/30/3,
     breaks=brks,
     lab.breaks =  seq(0,0.4,0.1),
     col=c(raster.col.fun(length(brks) - 1)),
     axis.args = arg)
axis(side=1,at=seq(0,1458,365),labels = paste0(seq(2013,2016)))
