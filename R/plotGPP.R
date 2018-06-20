rings=2
# read daily fluxes############
data.both.sap<- readRDS("output/maestraVPD/mastra and sap.rds")
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

rm(sd)
years <- as.numeric(e.date-s.date)/365.25
# plot
# e 1 4 5 
# a 2 3 6
pdf("gpp.plot.pdf",width = 8,height = 6)

# par(mar=c(4,5,2,5))
palette(c("brown1","brown2","brown4","cadetblue1","cadetblue3","deepskyblue"))
par(mfrow=c(2,1))


# plot monthly gpp####
# with(data.both.sap,
#      plot(rep(-10,nrow(data.both.sap))~Date,
#           ylim=c(0,10),
#           ylab=expression(GPP~(g~C~m^-2~d^-1)),
#           type="l",
#           col=1,
#           xaxt="n"
#      ))

# for (i in 1:6){
#   par(new=1)
#   rings <- sprintf("R%s",i)
#   with(data.both.sap[data.both.sap$Ring == rings,],
#        plot(GPP~Date,
#             ylim=c(0,10),
#             ylab=" ",
#             type="l",
#             col=i,
#             xaxt="n"
#        ))
#   
# }
# legend("top",legend = paste0("R",1:6),col=1:6,pch=15,horiz = 1,bty = 'n',cex=0.8)
library(lubridate)
data.both.sap$mon <- month(data.both.sap$Date)
data.both.sap$mon <- as.factor(data.both.sap$mon)
levels(data.both.sap$mon ) <- month.abb[1:12]
# data.sub.df <- data.both.sap[,c("GPP","mon")]
library(doBy)
data.sub.df.tp <- summaryBy(GPP + Trans ~mon + Ring,
                         data = data.both.sap[,c("GPP","Trans","mon","Ring")],
                         FUN = mean,na.rm=TRUE,
                         keep.names = TRUE)
data.all.sum <- data.sub.df.tp
data.sub.df.sd <- summaryBy(GPP + Trans ~ mon + Ring,
                            data = data.both.sap[,c("GPP","Trans","mon","Ring")],
                            FUN = sd,na.rm=TRUE)

# plot gpp
data.sub.df.sd.spread <- spread(data.sub.df.sd[,c("GPP.sd","mon","Ring")],"mon","GPP.sd")
data.sub.df.sd <- data.sub.df.tp[,-1][c(1,4,5,2,3,6),]
library(tidyr)

data.sub.df.tp <- spread(data.sub.df.tp[,c("GPP","mon","Ring")],"mon","GPP")
data.sub.df <- data.sub.df.tp[,-1][c(1,4,5,2,3,6),]
par(mar=c(0,5,1,1))
# barplot(as.matrix(data.sub.df),
#         ylab=expression(GPP~(g~C~m^-2~d^-1)), 
#         xlab=" ",
#         beside=TRUE,
#         names.arg = rep(" ",12),
#         col=palette())


barCenters <- barplot(as.matrix(data.sub.df),
                      ylab=expression(GPP~(g~C~m^-2~d^-1)), 
                      xlab=" ",
                      beside=TRUE,
                      names.arg = rep(" ",12),
                      col=palette())
legend("top",legend = paste0("R",c(1,4,5,2,3,6)),col=1:6,pch=15,horiz = 1,bty = 'n',cex=0.8)

tabbedMeans <- as.vector(as.matrix(data.sub.df))
tabbedSE <- as.vector(as.matrix(data.sub.df.sd)) / sqrt(years) / 30.5
segments(barCenters, tabbedMeans - tabbedSE , barCenters,
         tabbedMeans + tabbedSE, lwd = 1)

arrows(barCenters, tabbedMeans - tabbedSE * 2, barCenters,
       tabbedMeans + tabbedSE * 2, lwd = 1, angle = 90,
       code = 3, length = 0.02)


# plot trans

data.sub.df.sd.trans.spread <- spread(data.sub.df.sd[,c("Trans.sd","mon","Ring")],"mon","Trans.sd")
data.sub.df.sd <- data.sub.df.tp[,-1][c(1,4,5,2,3,6),]
library(tidyr)

data.sub.df.tp.trans <- spread(data.all.sum[,c("Trans","mon","Ring")],"mon","Trans")
data.sub.df.trans <- data.sub.df.tp.trans[,-1][c(1,4,5,2,3,6),]
par(mar=c(5,5,1,1))


barCenters <- barplot(as.matrix(data.sub.df.trans),
                      ylab=expression(Transpiration~(kg~m^-2~d^-1)), 
                      xlab=" ",
                      beside=TRUE,
                      names.arg = month.abb[1:12],
                      col=palette())
# legend("top",legend = paste0("R",c(1,4,5,2,3,6)),col=1:6,pch=15,horiz = 1,bty = 'n',cex=0.8)

tabbedMeans <- as.vector(as.matrix(data.sub.df.trans))
tabbedSE <- as.vector(as.matrix(data.sub.df.sd)) / sqrt(years) / 30.5
segments(barCenters, tabbedMeans - tabbedSE , barCenters,
         tabbedMeans + tabbedSE, lwd = 1)

arrows(barCenters, tabbedMeans - tabbedSE*2 , barCenters,
       tabbedMeans + tabbedSE *2, lwd = 1, angle = 90,
       code = 3, length = 0.02)

# plot lai####
par(mar=c(5,5,1,1))
# barplot(as.matrix(data.sub.df),
#         ylab=expression(GPP~(g~C~m^-2~d^-1)), 
#         xlab=" ",
#         beside=TRUE,
#         names.arg = rep(" ",12),
#         col=palette())
# legend("top",legend = paste0("R",c(1,4,5,2,3,6)),col=1:6,pch=15,horiz = 1,bty = 'n',cex=0.8)



with(data.both.sap,
     plot(rep(-10,nrow(data.both.sap))~Date,
          ylim=c(0,2),
          ylab=expression(LAI~(m^2~m^-2)),
          # ylab=" ",
          type="l",
          xlab=" ",
          col=1#,
          # xaxt="n"#,
          # yaxt="n"
     ))

# axis(1,at=seq(min(data.both.sap$Date),
#               max(data.both.sap$Date),
#               "3 month"),
#      labels = paste0(
#        zoo::as.yearmon(seq(min(data.both.sap$Date),max(data.both.sap$Date),"3 month"))
#      ))
# axis(2,at=seq(1,2,0.25),
#      labels = paste0(seq(1,2,0.25)
#      ))

# mtext("LAI",side=2,line=3,adj=0.8)

for (i in 1:6){
  palette(c("brown1","cadetblue1","cadetblue3","brown2","brown4","deepskyblue"))
  par(new=1)
  rings <- sprintf("R%s",i)
  with(data.both.sap[data.both.sap$Ring == rings,],
       plot(LAI-0.8~Date,
            ylim=c(0,2),
            ylab=" ",
            xlab=" ",
            type="l",
            col=palette()[i],
            xaxt="n",
            yaxt="n"
       ))
  
}

# plot rainfall#####
par(mar=c(4,5,1,1))
with(data.both.sap[data.both.sap$Ring == "R4",],
     plot(PPT~Date,
          ylim=c(0,60),
          # ylab=expression("Precipitation"(mm~d^-1)),
          ylab=expression("Precipitation"~(mm~d^-1)),
          xlab=" ",
          type="l",
          col="grey90"
          # xaxt="n"#,
          # yaxt="n"#
     ))
# mtext("Precipitation (mm/d)",side=4,line=3,adj=0.2)

# axis(4,at=seq(0,40,10),
#      labels = paste0(seq(0,40,10)
#      ))


# abline(h=50)
# # bar plot function
# bar.plot.func <- function(df)
par(mfrow=c(1,1))
par(mar=c(4,5,2,5))
# bar plot for gpp#####
gpp.vec.e <- c(sum(data.both.sap$GPP[data.both.sap$Ring == "R1"])/years,
               sum(data.both.sap$GPP[data.both.sap$Ring == "R4"])/years,
               sum(data.both.sap$GPP[data.both.sap$Ring == "R5"])/years)

gpp.vec.a <- c(sum(data.both.sap$GPP[data.both.sap$Ring == "R2"])/years,
               sum(data.both.sap$GPP[data.both.sap$Ring == "R3"])/years,
               sum(data.both.sap$GPP[data.both.sap$Ring == "R6"])/years)

gpp.df <- data.frame(gpp=c(sum(data.both.sap$GPP[data.both.sap$Ring == "R1"])/years,
                           sum(data.both.sap$GPP[data.both.sap$Ring == "R2"])/years,
                           sum(data.both.sap$GPP[data.both.sap$Ring == "R3"])/years,
                           sum(data.both.sap$GPP[data.both.sap$Ring == "R4"])/years,
                           sum(data.both.sap$GPP[data.both.sap$Ring == "R5"])/years,
                           sum(data.both.sap$GPP[data.both.sap$Ring == "R6"])/years))

palette(c("brown1","brown2","brown4","cadetblue1","cadetblue3","deepskyblue"))
barplot(as.matrix(gpp.df),
        ylab=expression(GPP~(g~C~m^-2~yr^-1)), 
        beside=TRUE,
        names.arg = paste0("R",seq(1,6)),
        col=palette())
legend("topleft", 
       c("R1","R4","R5","R2","R3","R6"), 
       cex=1, 
       fill=palette(),
       xpd=TRUE,
       horiz = TRUE,
       inset = -0.06,
       bty = 'n')

abline(h=mean(gpp.vec.e),col="red",lty= "dashed")
abline(h=mean(gpp.vec.a),col="lightskyblue",lty= "dashed")

# resp####
resp.vec.e <- c(sum(data.both.sap$Ra[data.both.sap$Ring == "R1"])/years,
               sum(data.both.sap$Ra[data.both.sap$Ring == "R4"])/years,
               sum(data.both.sap$Ra[data.both.sap$Ring == "R5"])/years)

resp.vec.a <- c(sum(data.both.sap$Ra[data.both.sap$Ring == "R2"])/years,
               sum(data.both.sap$Ra[data.both.sap$Ring == "R3"])/years,
               sum(data.both.sap$Ra[data.both.sap$Ring == "R6"])/years)

resp.df <- data.frame(gpp=c(sum(data.both.sap$Ra[data.both.sap$Ring == "R1"])/years,
                           sum(data.both.sap$Ra[data.both.sap$Ring == "R2"])/years,
                           sum(data.both.sap$Ra[data.both.sap$Ring == "R3"])/years,
                           sum(data.both.sap$Ra[data.both.sap$Ring == "R4"])/years,
                           sum(data.both.sap$Ra[data.both.sap$Ring == "R5"])/years,
                           sum(data.both.sap$Ra[data.both.sap$Ring == "R6"])/years))

barplot(as.matrix(resp.df),
        ylab=expression(R[leaf]~(g~C~m^-2~yr^-1)), 
        beside=TRUE,
        names.arg = paste0("R",seq(1,6)),
        col=palette())
legend("topleft", 
       c("R1","R2","R3","R4","R5","R6"), 
       cex=1, 
       fill=palette(),
       xpd=TRUE,
       horiz = TRUE,
       inset = -0.06,
       bty = 'n')

abline(h=mean(resp.vec.e),col="red",lty= "dashed")
abline(h=mean(resp.vec.a),col="lightskyblue",lty= "dashed")

# nce####
data.both.sap$npp <- data.both.sap$GPP - data.both.sap$Ra
npp.vec.e <- c(sum(data.both.sap$npp[data.both.sap$Ring == "R1"])/years,
                sum(data.both.sap$npp[data.both.sap$Ring == "R4"])/years,
                sum(data.both.sap$npp[data.both.sap$Ring == "R5"])/years)

npp.vec.a <- c(sum(data.both.sap$npp[data.both.sap$Ring == "R2"])/years,
                sum(data.both.sap$npp[data.both.sap$Ring == "R3"])/years,
                sum(data.both.sap$npp[data.both.sap$Ring == "R6"])/years)

npp.df <- data.frame(gpp=c(sum(data.both.sap$npp[data.both.sap$Ring == "R1"])/years,
                            sum(data.both.sap$npp[data.both.sap$Ring == "R2"])/years,
                            sum(data.both.sap$npp[data.both.sap$Ring == "R3"])/years,
                            sum(data.both.sap$npp[data.both.sap$Ring == "R4"])/years,
                            sum(data.both.sap$npp[data.both.sap$Ring == "R5"])/years,
                            sum(data.both.sap$npp[data.both.sap$Ring == "R6"])/years))

barplot(as.matrix(npp.df),
        ylab=expression(GPP*"-"*R[leaf]~(g~C~m^-2~yr^-1)), 
        beside=TRUE,
        names.arg = paste0("R",seq(1,6)),
        col=palette())
legend("topleft", 
       c("R1","R2","R3","R4","R5","R6"), 
       cex=1, 
       fill=palette(),
       xpd=TRUE,
       horiz = TRUE,
       inset = -0.06,
       bty = 'n')

abline(h=mean(npp.vec.e),col="red",lty= "dashed")

abline(h=mean(npp.vec.a),col="lightskyblue",lty= "dashed")

# WUE####
data.both.sap$wue <- (data.both.sap$GPP) / (data.both.sap$Trans)
# 
# plot(GPP ~ APAR,col=Ring,data=data.both.sap[data.both.sap$Ring == "R2",])
# with(data.both.sap,
#      plot(wue~Date,
#           ylim=c(0,70),
#           ylab=" ",
#           type="l",
#           col=i,
#           xaxt="n"
#      ))


with(data.both.sap,
     plot(rep(-10,nrow(data.both.sap))~Date,
          ylim=c(0,70),
          ylab=expression(WUE~(g~C~mm^-1)),
          type="l",
          col=1,
          xaxt="n"
     ))

axis(1,at=seq(min(data.both.sap$Date),
              max(data.both.sap$Date),
              "3 month"),
     labels = paste0(
       zoo::as.yearmon(seq(min(data.both.sap$Date),max(data.both.sap$Date),"3 month"))
     ))

for (i in 1:6){
  par(new=1)
  rings <- sprintf("R%s",i)
  with(data.both.sap[data.both.sap$Ring == rings,],
       plot(wue~Date,
            ylim=c(0,70),
            ylab=" ",
            type="l",
            col=i,
            xaxt="n"
       ))
  
}
legend("bottom",legend = paste0("R",1:6),col=1:6,pch=15,horiz = 1,bty = 'n',cex=0.8)

par(new=TRUE)
# plot(PPT~Date,
#      data = data.both.sap,
#      type="s",
#      ann=FALSE,axes=FALSE,
#      ylim=c(-100,100))
# axis(4,at=seq(0,100,25),labels = paste0(seq(0,100,25)))
# mtext("Precipitation (mm/d)",side = 4,line = 3,adj = 0.8)


plot(APAR~Date,
     data = data.both.sap,
     type="s",
     ann=FALSE,axes=FALSE,
     ylim=c(-7,7),
     col="gold")
axis(4,at=seq(0,8,2),labels = paste0(seq(0,8,2)))
mtext(expression(APAR~(MJ~m^-2~ground~d^-1)),side = 4,line = 3,adj = 0.8)

# bar for wue####
wue.vec.e <- c(mean(data.both.sap$wue[data.both.sap$Ring == "R1"],na.rm = TRUE),
               mean(data.both.sap$wue[data.both.sap$Ring == "R4"],na.rm = TRUE),
               mean(data.both.sap$wue[data.both.sap$Ring == "R5"],na.rm = TRUE))

wue.vec.a <- c(mean(data.both.sap$wue[data.both.sap$Ring == "R2"],na.rm = TRUE),
               mean(data.both.sap$wue[data.both.sap$Ring == "R3"],na.rm = TRUE),
               mean(data.both.sap$wue[data.both.sap$Ring == "R6"],na.rm = TRUE))

wue.df <- data.frame(gpp=c(mean(data.both.sap$wue[data.both.sap$Ring == "R1"],na.rm = TRUE),
                           mean(data.both.sap$wue[data.both.sap$Ring == "R2"],na.rm = TRUE),
                           mean(data.both.sap$wue[data.both.sap$Ring == "R3"],na.rm = TRUE),
                           mean(data.both.sap$wue[data.both.sap$Ring == "R4"],na.rm = TRUE),
                           mean(data.both.sap$wue[data.both.sap$Ring == "R5"],na.rm = TRUE),
                           mean(data.both.sap$wue[data.both.sap$Ring == "R6"],na.rm = TRUE)))

barplot(as.matrix(wue.df),
        ylab=expression(WUE~(g~C~mm^-1)), 
        beside=TRUE,
        names.arg = paste0("R",seq(1,6)),
        col=palette())
legend("topleft", 
       c("R1","R2","R3","R4","R5","R6"), 
       cex=1, 
       fill=palette(),
       xpd=TRUE,
       horiz = TRUE,
       inset = -0.06,
       bty = 'n')

abline(h=mean(wue.vec.e),col="red",lty= "dashed")

abline(h=mean(wue.vec.a),col="lightskyblue",lty= "dashed")

inc.perc <- format(round((mean(wue.vec.e)/mean(wue.vec.a) - 1) * 100, 3), nsmall = 2)
text(6.5,5.5,paste0(inc.perc,"%"))

# bar for LUE#####
data.both.sap$lue <- data.both.sap$GPP / data.both.sap$APAR
lue.vec.e <- c(mean(data.both.sap$lue[data.both.sap$Ring == "R1"],na.rm = TRUE),
               mean(data.both.sap$lue[data.both.sap$Ring == "R4"],na.rm = TRUE),
               mean(data.both.sap$lue[data.both.sap$Ring == "R5"],na.rm = TRUE))

lue.vec.a <- c(mean(data.both.sap$lue[data.both.sap$Ring == "R2"],na.rm = TRUE),
               mean(data.both.sap$lue[data.both.sap$Ring == "R3"],na.rm = TRUE),
               mean(data.both.sap$lue[data.both.sap$Ring == "R6"],na.rm = TRUE))

lue.df <- data.frame(gpp=c(mean(data.both.sap$lue[data.both.sap$Ring == "R1"],na.rm = TRUE),
                           mean(data.both.sap$lue[data.both.sap$Ring == "R2"],na.rm = TRUE),
                           mean(data.both.sap$lue[data.both.sap$Ring == "R3"],na.rm = TRUE),
                           mean(data.both.sap$lue[data.both.sap$Ring == "R4"],na.rm = TRUE),
                           mean(data.both.sap$lue[data.both.sap$Ring == "R5"],na.rm = TRUE),
                           mean(data.both.sap$lue[data.both.sap$Ring == "R6"],na.rm = TRUE)))

barplot(as.matrix(lue.df),
        ylab=expression(LUE~(g~C~MJ^-1)), 
        beside=TRUE,
        names.arg = paste0("R",seq(1,6)),
        col=palette())
legend("topleft", 
       c("R1","R2","R3","R4","R5","R6"), 
       cex=1, 
       fill=palette(),
       xpd=TRUE,
       horiz = TRUE,
       inset = -0.06,
       bty = 'n')

abline(h=mean(lue.vec.e),col="red",lty= "dashed")

abline(h=mean(lue.vec.a),col="lightskyblue",lty= "dashed")

inc.perc <- format(round((mean(lue.vec.e)/mean(lue.vec.a) - 1) * 100, 3), nsmall = 2)
text(2,2,paste0(inc.perc,"%"))

# bar for transpiration####
data.both.sap$tran.leaf <- data.both.sap$Trans #/ data.both.sap$LAI

trans.vec.e <- c(sum(data.both.sap$tran.leaf[data.both.sap$Ring == "R1"])/years,
                 sum(data.both.sap$tran.leaf[data.both.sap$Ring == "R4"])/years,
                 sum(data.both.sap$tran.leaf[data.both.sap$Ring == "R5"])/years)

trans.vec.a <- c(sum(data.both.sap$tran.leaf[data.both.sap$Ring == "R2"])/years,
                 sum(data.both.sap$tran.leaf[data.both.sap$Ring == "R3"])/years,
                 sum(data.both.sap$tran.leaf[data.both.sap$Ring == "R6"])/years)

Trans.df <- data.frame(gpp=c(sum(data.both.sap$tran.leaf[data.both.sap$Ring == "R1"])/years,
                             sum(data.both.sap$tran.leaf[data.both.sap$Ring == "R2"])/years,
                             sum(data.both.sap$tran.leaf[data.both.sap$Ring == "R3"])/years,
                             sum(data.both.sap$tran.leaf[data.both.sap$Ring == "R4"])/years,
                             sum(data.both.sap$tran.leaf[data.both.sap$Ring == "R5"])/years,
                             sum(data.both.sap$tran.leaf[data.both.sap$Ring == "R6"])/years))

barplot(as.matrix(Trans.df),
        ylab=expression(Transpiration~(MAESPA*";"~mm~yr^-1)), 
        beside=TRUE,
        names.arg = paste0("R",seq(1,6)),
        col=palette())
legend("topleft", 
       c("R1","R2","R3","R4","R5","R6"), 
       cex=1, 
       fill=palette(),
       xpd=TRUE,
       horiz = TRUE,
       inset = -0.06,
       bty = 'n')

abline(h=mean(trans.vec.e),col="red",lty= "dashed")

abline(h=mean(trans.vec.a),col="lightskyblue",lty= "dashed")

inc.perc <- format(round((mean(trans.vec.e)/mean(trans.vec.a) - 1) * 100, 3), nsmall = 2)
text(6.5,300,paste0(inc.perc,"%"))

# bar for sap
data.both.sap$sap.leaf <- data.both.sap$volRing #/ data.both.sap$LAI
volRing.vec.e <- c(sum(data.both.sap$sap.leaf[data.both.sap$Ring == "R1"]),
                 sum(data.both.sap$sap.leaf[data.both.sap$Ring == "R4"]),
                 sum(data.both.sap$sap.leaf[data.both.sap$Ring == "R5"]))

volRing.vec.a <- c(sum(data.both.sap$sap.leaf[data.both.sap$Ring == "R2"]),
                 sum(data.both.sap$sap.leaf[data.both.sap$Ring == "R3"]),
                 sum(data.both.sap$sap.leaf[data.both.sap$Ring == "R6"]))

volRing.df <- data.frame(gpp=c(sum(data.both.sap$sap.leaf[data.both.sap$Ring == "R1"]),
                             sum(data.both.sap$sap.leaf[data.both.sap$Ring == "R2"]),
                             sum(data.both.sap$sap.leaf[data.both.sap$Ring == "R3"]),
                             sum(data.both.sap$sap.leaf[data.both.sap$Ring == "R4"]),
                             sum(data.both.sap$sap.leaf[data.both.sap$Ring == "R5"]),
                             sum(data.both.sap$sap.leaf[data.both.sap$Ring == "R6"])))

barplot(as.matrix(volRing.df),
        ylab=expression(Transpiration~(estimated*";"~mm~yr^-1)), 
        beside=TRUE,
        names.arg = paste0("R",seq(1,6)),
        col=palette())
legend("topleft", 
       c("R1","R2","R3","R4","R5","R6"), 
       cex=1, 
       fill=palette(),
       xpd=TRUE,
       horiz = TRUE,
       inset = -0.06,
       bty = 'n')

abline(h=mean(volRing.vec.e),col="red",lty= "dashed")

abline(h=mean(volRing.vec.a),col="lightskyblue",lty= "dashed")

inc.perc <- format(round((mean(volRing.vec.e)/mean(volRing.vec.a) - 1) * 100, 3), nsmall = 2)
text(6.5,400,paste0(inc.perc,"%"))


# difference between sap and maespa
data.both.sap$diff <- data.both.sap$Trans - data.both.sap$volRing
diff.vec.e <- c(sum(data.both.sap$diff[data.both.sap$Ring == "R1"]),
                sum(data.both.sap$diff[data.both.sap$Ring == "R4"]),
                sum(data.both.sap$diff[data.both.sap$Ring == "R5"]))

diff.vec.a <- c(sum(data.both.sap$diff[data.both.sap$Ring == "R2"]),
                sum(data.both.sap$diff[data.both.sap$Ring == "R3"]),
                sum(data.both.sap$diff[data.both.sap$Ring == "R6"]))

diff.df <- data.frame(gpp=c(sum(data.both.sap$diff[data.both.sap$Ring == "R1"]),
                            sum(data.both.sap$diff[data.both.sap$Ring == "R2"]),
                            sum(data.both.sap$diff[data.both.sap$Ring == "R3"]),
                            sum(data.both.sap$diff[data.both.sap$Ring == "R4"]),
                            sum(data.both.sap$diff[data.both.sap$Ring == "R5"]),
                            sum(data.both.sap$diff[data.both.sap$Ring == "R6"])))

barplot(as.matrix(diff.df),
        ylab=expression(Residual~("MAESPA - estimates;"~mm~yr^-1)), 
        beside=TRUE,
        names.arg = paste0("R",seq(1,6)),
        col=palette())
legend("topright", 
       c("R1","R2","R3","R4","R5","R6"), 
       cex=1, 
       fill=palette(),
       xpd=TRUE,
       horiz = TRUE,
       inset = -0.06,
       bty = 'n')

abline(h=mean(diff.vec.e),col="red",lty= "dashed")

abline(h=mean(diff.vec.a),col="lightskyblue",lty= "dashed")

inc.perc <- format(round((mean(diff.vec.e)/mean(diff.vec.a) - 1) * 100, 3), nsmall = 2)

text(6.5,400,paste0(inc.perc,"%"))

dev.off()
