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

# read day flux and met   
data.both.sap<- readRDS("mastra and sap.rds")

# plot
# e 1 4 5 
# a 2 3 6
pdf("gpp.plot.pdf",width = 8,height = 6)

par(mar=c(4,5,2,5))
palette(c("brown1","cadetblue1","cadetblue3","brown2","brown4","deepskyblue"))

with(data.both.sap,
     plot(rep(-10,nrow(data.both.sap))~Date,
          ylim=c(0,10),
          ylab=expression(GPP~(g~C~m^-2~d^-1)),
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
       plot(GPP~Date,
            ylim=c(0,10),
            ylab=" ",
            type="l",
            col=i,
            xaxt="n"
       ))
  
}
legend("top",legend = paste0("R",1:6),col=1:6,pch=15,horiz = 1,bty = 'n',cex=0.8)

# # bar plot function
# bar.plot.func <- function(df)

# bar plot for gpp
gpp.vec.e <- c(sum(data.both.sap$GPP[data.both.sap$Ring == "R1"]),
               sum(data.both.sap$GPP[data.both.sap$Ring == "R4"]),
               sum(data.both.sap$GPP[data.both.sap$Ring == "R5"]))

gpp.vec.a <- c(sum(data.both.sap$GPP[data.both.sap$Ring == "R2"]),
               sum(data.both.sap$GPP[data.both.sap$Ring == "R3"]),
               sum(data.both.sap$GPP[data.both.sap$Ring == "R6"]))

gpp.df <- data.frame(gpp=c(sum(data.both.sap$GPP[data.both.sap$Ring == "R1"]),
                           sum(data.both.sap$GPP[data.both.sap$Ring == "R2"]),
                           sum(data.both.sap$GPP[data.both.sap$Ring == "R3"]),
                           sum(data.both.sap$GPP[data.both.sap$Ring == "R4"]),
                           sum(data.both.sap$GPP[data.both.sap$Ring == "R5"]),
                           sum(data.both.sap$GPP[data.both.sap$Ring == "R6"])))


barplot(as.matrix(gpp.df),
        ylab=expression(GPP~(g~C~m^-2~yr^-1)), 
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

abline(h=mean(gpp.vec.e),col="red",lty= "dashed")
abline(h=mean(gpp.vec.a),col="lightskyblue",lty= "dashed")

# resp
resp.vec.e <- c(sum(data.both.sap$Ra[data.both.sap$Ring == "R1"]),
               sum(data.both.sap$Ra[data.both.sap$Ring == "R4"]),
               sum(data.both.sap$Ra[data.both.sap$Ring == "R5"]))

resp.vec.a <- c(sum(data.both.sap$Ra[data.both.sap$Ring == "R2"]),
               sum(data.both.sap$Ra[data.both.sap$Ring == "R3"]),
               sum(data.both.sap$Ra[data.both.sap$Ring == "R6"]))

resp.df <- data.frame(gpp=c(sum(data.both.sap$Ra[data.both.sap$Ring == "R1"]),
                           sum(data.both.sap$Ra[data.both.sap$Ring == "R2"]),
                           sum(data.both.sap$Ra[data.both.sap$Ring == "R3"]),
                           sum(data.both.sap$Ra[data.both.sap$Ring == "R4"]),
                           sum(data.both.sap$Ra[data.both.sap$Ring == "R5"]),
                           sum(data.both.sap$Ra[data.both.sap$Ring == "R6"])))

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

# npp
data.both.sap$npp <- data.both.sap$GPP - data.both.sap$Ra
npp.vec.e <- c(sum(data.both.sap$npp[data.both.sap$Ring == "R1"]),
                sum(data.both.sap$npp[data.both.sap$Ring == "R4"]),
                sum(data.both.sap$npp[data.both.sap$Ring == "R5"]))

npp.vec.a <- c(sum(data.both.sap$npp[data.both.sap$Ring == "R2"]),
                sum(data.both.sap$npp[data.both.sap$Ring == "R3"]),
                sum(data.both.sap$npp[data.both.sap$Ring == "R6"]))

npp.df <- data.frame(gpp=c(sum(data.both.sap$npp[data.both.sap$Ring == "R1"]),
                            sum(data.both.sap$npp[data.both.sap$Ring == "R2"]),
                            sum(data.both.sap$npp[data.both.sap$Ring == "R3"]),
                            sum(data.both.sap$npp[data.both.sap$Ring == "R4"]),
                            sum(data.both.sap$npp[data.both.sap$Ring == "R5"]),
                            sum(data.both.sap$npp[data.both.sap$Ring == "R6"])))

barplot(as.matrix(npp.df),
        ylab=expression(NPP~(g~C~m^-2~yr^-1)), 
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

# WUE
data.both.sap$wue <- (data.both.sap$GPP) / (data.both.sap$Trans)

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
plot(data.both.sap$PPT~data.both.sap$Date,
     type="s",
     ann=FALSE,axes=FALSE,
     ylim=c(-100,100))
axis(4,at=seq(0,100,25),labels = paste0(seq(0,100,25)))
mtext("Precipitation (mm/d)",side = 4,line = 3,adj = 0.8)

# bar for wue
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
text(6.5,6,paste0(inc.perc,"%"))

# bar for transpiration
trans.vec.e <- c(sum(data.both.sap$Trans[data.both.sap$Ring == "R1"]),
                 sum(data.both.sap$Trans[data.both.sap$Ring == "R4"]),
                 sum(data.both.sap$Trans[data.both.sap$Ring == "R5"]))

trans.vec.a <- c(sum(data.both.sap$Trans[data.both.sap$Ring == "R2"]),
                 sum(data.both.sap$Trans[data.both.sap$Ring == "R3"]),
                 sum(data.both.sap$Trans[data.both.sap$Ring == "R6"]))

Trans.df <- data.frame(gpp=c(sum(data.both.sap$Trans[data.both.sap$Ring == "R1"]),
                             sum(data.both.sap$Trans[data.both.sap$Ring == "R2"]),
                             sum(data.both.sap$Trans[data.both.sap$Ring == "R3"]),
                             sum(data.both.sap$Trans[data.both.sap$Ring == "R4"]),
                             sum(data.both.sap$Trans[data.both.sap$Ring == "R5"]),
                             sum(data.both.sap$Trans[data.both.sap$Ring == "R6"])))

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
text(6.5,400,paste0(inc.perc,"%"))

# bar for sap
volRing.vec.e <- c(sum(data.both.sap$volRing[data.both.sap$Ring == "R1"]),
                 sum(data.both.sap$volRing[data.both.sap$Ring == "R4"]),
                 sum(data.both.sap$volRing[data.both.sap$Ring == "R5"]))

volRing.vec.a <- c(sum(data.both.sap$volRing[data.both.sap$Ring == "R2"]),
                 sum(data.both.sap$volRing[data.both.sap$Ring == "R3"]),
                 sum(data.both.sap$volRing[data.both.sap$Ring == "R6"]))

volRing.df <- data.frame(gpp=c(sum(data.both.sap$volRing[data.both.sap$Ring == "R1"]),
                             sum(data.both.sap$volRing[data.both.sap$Ring == "R2"]),
                             sum(data.both.sap$volRing[data.both.sap$Ring == "R3"]),
                             sum(data.both.sap$volRing[data.both.sap$Ring == "R4"]),
                             sum(data.both.sap$volRing[data.both.sap$Ring == "R5"]),
                             sum(data.both.sap$volRing[data.both.sap$Ring == "R6"])))

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
