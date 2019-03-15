pdf("gpp lue wue.pdf",width = 8,height = 6)
source('r/load.r')
library(dplyr)
library(scales)
library(doBy)
par(mar=c(5,5,2,2))
# read daily fluxes############
# data.both.sap.m.150 <- readRDS("mastra and sap_1_0.8_-150_0.rds")
# data.both.sap.p.150 <- readRDS("mastra and sap_1_0.8_150_0.rds")
# data.both.sap.ctrl <- readRDS("output/maestraVPD/mastra and sap.rds")
# data.both.sap.m.150 <- readRDS("output/vj16/400/mastra and sap hr.rds")
# data.both.sap.p.150 <- readRDS("output/vj16/550/mastra and sap hr.rds")

data.both.sap.150 <- readRDS("output/150/mastra and sap.rds")
data.both.sap <- readRDS("output/maestraVPD/mastra and sap.rds")
all.e.df <- rbind(data.both.sap.150[data.both.sap.150$Ring %in% paste0('R',c(2,3,6)),],
                  data.both.sap[data.both.sap$Ring %in% paste0('R',c(1,4,5)),])
all.e.df$year <- year(all.e.df$Date)
all.a.df <- rbind(data.both.sap.150[data.both.sap.150$Ring %in% paste0('R',c(1,4,5)),],
                  data.both.sap[data.both.sap$Ring %in% paste0('R',c(2,3,6)),])
all.a.df$year <- year(all.a.df$Date)
rm(sd)



names(all.e.df) <- paste0(names(all.e.df),'.e')
all.ae.df <- merge(all.e.df,all.a.df[,c('Date','Ring','GPP','Trans')],
                   by.x=c('Date.e','Ring.e'),by.y=c('Date','Ring'))

ae.yr.df <- summaryBy(.~year.e + Ring.e, data = all.ae.df,
                      FUN=sum,na.rm=T,keep.names = T)
ae.yr.df$VPD <- ae.yr.df$VPD.e / 365
ae.yr.df$LAI <- ae.yr.df$LAI.e / 365
ae.yr.df <- subset(ae.yr.df,select = -c(LAI.e,VPD.e))
write.csv(ae.yr.df,'ca response.csv',row.names = F)

mean(ae.yr.df$GPP.e[ae.yr.df$Ring.e %in% paste0('R',c(1,4,5))])/

mean(ae.yr.df$GPP[ae.yr.df$Ring.e %in% paste0('R',c(2,3,6))])
all.ae.day.df <- summaryBy(.~Ring.e + Date.e,data = all.ae.df,
                           FUN=c(sum,mean,sd),na.rm=TRUE)

# all.ae.day.df <- all.ae.day.df[,c('year.e','Ring.e','GPP.e')]
  
  
all.ae.day.df$c.response <- all.ae.day.df$GPP.e.sum / all.ae.day.df$GPP.sum


plot(c((c.response-1)*100)~Date.e,data = all.ae.day.df,ylim=c(0,15))

a.sum <- summaryBy(.~ year + Ring,data = all.a.df,FUN=c(sum),na.rm=T,keep.names = T)
a.annual.sd <- summaryBy(.~ Ring,data = a.sum,FUN=c(sd),na.rm=T)
range(a.annual.sd$GPP.sd)
mean(e.sum$GPP)/mean(a.sum$GPP)

a.ring.sd <- summaryBy(.~ year,data = a.sum,FUN=c(sd),na.rm=T)
range(a.ring.sd$GPP.sd)
e.sum <- summaryBy(.~ year.e + Ring.e,data = all.e.df,FUN=c(sum),na.rm=T,keep.names = T)
ae.sum <- merge(e.sum,a.sum[c('year','Ring','GPP','Trans')],by.x=c('year.e','Ring.e'),by.y=c('year','Ring'))
ae.sum$change <- ae.sum$GPP.e - ae.sum$GPP
range(ae.sum$change)
# get start and end day
con.ls <- readLines("Rings/Ring1/runfolder/confile.dat")

sd <- gsub("startdate = ","",
           con.ls[grep("startdate", con.ls)])
sd <- gsub("'","",sd) %>%  as.Date("%d/%m/%y")


ed <- gsub("enddate = ","",
           con.ls[grep("enddate", con.ls)])
ed <- gsub("'","",ed) %>%  as.Date("%d/%m/%y")

# years <- as.numeric(ed-sd)/365.25
years <- 4
rm(sd)
# annual sum gpp 
gpp.df.e <- data.frame(gpp=c(sum(data.both.sap$GPP[data.both.sap$Ring == "R1"])/years,
                             sum(data.both.sap$GPP[data.both.sap$Ring == "R4"])/years,
                             sum(data.both.sap$GPP[data.both.sap$Ring == "R5"])/years,
                             
                             sum(data.both.sap.150$GPP[data.both.sap.150$Ring == "R2"])/years,
                             sum(data.both.sap.150$GPP[data.both.sap.150$Ring == "R3"])/years,
                             sum(data.both.sap.150$GPP[data.both.sap.150$Ring == "R6"])/years))

gpp.df.a <- data.frame(gpp=c(sum(data.both.sap.150$GPP[data.both.sap.150$Ring == "R1"])/years,
                             sum(data.both.sap.150$GPP[data.both.sap.150$Ring == "R4"])/years,
                             sum(data.both.sap.150$GPP[data.both.sap.150$Ring == "R5"])/years,
                             
                             sum(data.both.sap$GPP[data.both.sap$Ring == "R2"])/years,
                             sum(data.both.sap$GPP[data.both.sap$Ring == "R3"])/years,
                             sum(data.both.sap$GPP[data.both.sap$Ring == "R6"])/years))

mean(c(gpp.df.e$gpp / gpp.df.a$gpp))

# palette(c("brown1","brown2","brown4",
#           "cadetblue1","cadetblue3","deepskyblue"
# ))
# barplot(as.matrix(gpp.df.e),
#         ylab=expression(GPP~(g~C~m^-2~yr^-1)), 
#         beside=TRUE,
#         names.arg = paste0("R",c(1,4,5,2,3,6)),
#         # col=palette(),
#         col=alpha(palette(),0.3),
#         border = NA,
#         space = c(0,0,0,0.5,0,0),
#         ylim = c(0,1800))
# # legend("topleft", 
# #        c("R1","R2","R3","R4","R5","R6"), 
# #        cex=1, 
# #        fill=palette(),
# #        xpd=TRUE,
# #        horiz = TRUE,
# #        inset = -0.06,
# #        bty = 'n')
# 
# abline(h=mean(gpp.df.e$gpp),col="red",lty= "dashed")
# abline(h=mean(gpp.df.a$gpp),col="lightskyblue",lty= "dashed")
# 
# # palette(c("brown1","brown2","brown4",
# #           "cadetblue1","cadetblue3","deepskyblue"))
# 
# par(new=TRUE)
# barplot(as.matrix(gpp.df.a),
#         ylab=expression(GPP~(g~C~m^-2~yr^-1)), 
#         beside=TRUE,
#         names.arg = paste0("R",c(1,4,5,2,3,6)),
#         # col=alpha(palette(),0.5),
#         col=palette(),
#         border = NA,
#         space = c(0,0,0,0.5,0,0),
#         ylim = c(0,1800))
# clip(0,3, -100, 100000)
# abline(h=mean(gpp.df.e$gpp[1:3]),col="red",lty= "solid",lwd = 3)
# clip(3.5,6.5, -100, 100000)
# abline(h=mean(gpp.df.a$gpp[4:6]),col="blue",lty= "solid",lwd =3)
# 
# var.perc <- format(round((sum(data.both.sap$GPP[data.both.sap$Ring %in% paste0('R',c(1,4,5))]) / 
#                             (sum(data.both.sap$GPP[data.both.sap$Ring %in% paste0('R',c(2,3,6))])) - 1) * 
#                             100, 1), nsmall = 1)
# 
# inc.perc <- format(round((mean(gpp.df.e$gpp)/mean(gpp.df.a$gpp) - 1) * 100, 1), nsmall = 1)
# text(6,1350,paste0(inc.perc,"%"))

# new figure 1####
# palette(c("brown1","brown2","brown4",
#           "cadetblue1","cadetblue3","deepskyblue"
# ))
par(mar=c(5,5,1,1))
palette(c(c("cadetblue1","cadetblue3","deepskyblue"),
          alpha(c("cadetblue1","cadetblue3","deepskyblue"),.5)
))
barplot(as.matrix(gpp.df.a),
        ylab=expression(GPP~(g~C~m^-2~yr^-1)), 
        beside=TRUE,
        names.arg = paste0("R",c(1,4,5,2,3,6)),
        col=palette(),
        # col=alpha(palette(),0.3),
        border = NA,
        space = c(0,0,0,0.5,0,0),
        ylim = c(0,2200))
# legend("topleft", 
#        c("R1","R2","R3","R4","R5","R6"), 
#        cex=1, 
#        fill=palette(),
#        xpd=TRUE,
#        horiz = TRUE,
#        inset = -0.06,
#        bty = 'n')

# abline(h=mean(gpp.df.e$gpp),col="red",lty= "dashed")
abline(h=mean(gpp.df.a$gpp),col="lightskyblue",lty= "dashed")

# palette(c("brown1","brown2","brown4",
#           "cadetblue1","cadetblue3","deepskyblue"))

# par(new=TRUE)
# barplot(as.matrix(gpp.df.a),
#         ylab=expression(GPP~(g~C~m^-2~yr^-1)), 
#         beside=TRUE,
#         names.arg = paste0("R",c(1,4,5,2,3,6)),
#         # col=alpha(palette(),0.5),
#         col=palette(),
#         border = NA,
#         space = c(0,0,0,0.5,0,0),
#         ylim = c(0,1800))
# clip(0,3, -100, 100000)
# abline(h=mean(gpp.df.e$gpp[1:3]),col="red",lty= "solid",lwd = 3)

# clip(3.5,6.5, -100, 100000)
# abline(h=mean(gpp.df.a$gpp[4:6]),col="blue",lty= "solid",lwd =3)

# var.perc <- format(round((sum(data.both.sap$GPP[data.both.sap$Ring %in% paste0('R',c(1,4,5))]) / 
#                             (sum(data.both.sap$GPP[data.both.sap$Ring %in% paste0('R',c(2,3,6))])) - 1) * 
#                            100, 1), nsmall = 1)

# inc.perc <- format(round((mean(gpp.df.e$gpp)/mean(gpp.df.a$gpp) - 1) * 100, 1), nsmall = 1)
# text(6,1350,paste0(inc.perc,"%"))

palette(c("brown1","brown2","brown4",
          alpha(c("brown1","brown2","brown4"),0.8)
))
par(mar=c(5,5,1,1))
barplot(as.matrix(gpp.df.e),
        ylab=expression(GPP~(g~C~m^-2~yr^-1)), 
        beside=TRUE,
        names.arg = paste0("R",c(1,4,5,2,3,6)),
        col=palette(),
        # col=alpha(palette(),0.3),
        border = NA,
        space = c(0,0,0,0.5,0,0),
        ylim = c(0,2200))
abline(h=mean(gpp.df.e$gpp),col="red",lty= "dashed")
abline(h=mean(gpp.df.a$gpp),col="lightskyblue",lty= "dashed")

c.impact <- format((mean(gpp.df.e$gpp) / mean(gpp.df.a$gpp) - 1)*100,digits = 2)

legend(4.5,1900,legend = paste0(c.impact,'%'),bty = 'n')
# plot c response####
# c.df <- gpp.df.e / gpp.df.a








# lue #####
apar.df <- data.frame(apar=c(sum(data.both.sap$APAR[data.both.sap$Ring == "R1"])/years,
                            sum(data.both.sap$APAR[data.both.sap$Ring == "R4"])/years,
                            sum(data.both.sap$APAR[data.both.sap$Ring == "R5"])/years,
                            
                            sum(data.both.sap$APAR[data.both.sap$Ring == "R2"])/years,
                            sum(data.both.sap$APAR[data.both.sap$Ring == "R3"])/years,
                            sum(data.both.sap$APAR[data.both.sap$Ring == "R6"])/years))

# apar.df.e <-data.frame(apar=c(sum(data.both.sap.ctrl$APAR[data.both.sap.ctrl$Ring == "R1"]),
#                               sum(data.both.sap.ctrl$APAR[data.both.sap.ctrl$Ring == "R4"]),
#                               sum(data.both.sap.ctrl$APAR[data.both.sap.ctrl$Ring == "R5"]),
#                               
#                               sum(data.both.sap.m.150$APAR[data.both.sap.m.150$Ring == "R2"]),
#                               sum(data.both.sap.m.150$APAR[data.both.sap.m.150$Ring == "R3"]),
#                               sum(data.both.sap.m.150$APAR[data.both.sap.m.150$Ring == "R6"])))

lue.a <- gpp.df.a/apar.df
lue.e <- gpp.df.e/apar.df


palette(c("brown1","brown2","brown4",
          "cadetblue1","cadetblue3","deepskyblue"
))
barplot(as.matrix(lue.e),
        ylab=expression(LUE~(g~C~MJ^-1)), 
        beside=TRUE,
        names.arg = paste0("R",c(1,4,5,2,3,6)),
        # col=palette(),
        col=alpha(palette(),0.3),
        border = NA,
        space = c(0,0,0,0.5,0,0),
        ylim = c(0,2))
# legend("topleft", 
#        c("R1","R2","R3","R4","R5","R6"), 
#        cex=1, 
#        fill=palette(),
#        xpd=TRUE,
#        horiz = TRUE,
#        inset = -0.06,
#        bty = 'n')

abline(h=mean(lue.e$gpp),col="red",lty= "dashed")
abline(h=mean(lue.a$gpp),col="lightskyblue",lty= "dashed")


# palette(c("brown1","brown2","brown4",
#           "cadetblue1","cadetblue3","deepskyblue"))


par(new=TRUE)
barplot(as.matrix(lue.a),
        ylab=" ", 
        beside=TRUE,
        names.arg = paste0("R",c(1,4,5,2,3,6)),
        # col=alpha(palette(),0.5),
        col=palette(),
        border = NA,
        space = c(0,0,0,0.5,0,0),
        ylim = c(0,2))

inc.perc <- format(round((mean(lue.e$gpp)/mean(lue.a$gpp) - 1) * 100, 1), nsmall = 1)
text(6,1.6,paste0(inc.perc,"%"))

clip(0,3, -100, 100000)
abline(h=mean(lue.e$gpp[1:3]),col="red",lty= "solid",lwd = 3)
clip(3.5,6.5, -100, 100000)
abline(h=mean(lue.a$gpp[4:6]),col="blue",lty= "solid",lwd =3)

# wue #####
tran.df.a <-data.frame(apar=c(sum(data.both.sap.m.150$Trans[data.both.sap.m.150$Ring == "R1"])/years,
                              sum(data.both.sap.m.150$Trans[data.both.sap.m.150$Ring == "R4"])/years,
                              sum(data.both.sap.m.150$Trans[data.both.sap.m.150$Ring == "R5"])/years,
                              
                              sum(data.both.sap.ctrl$Trans[data.both.sap.ctrl$Ring == "R2"])/years,
                              sum(data.both.sap.ctrl$Trans[data.both.sap.ctrl$Ring == "R3"])/years,
                              sum(data.both.sap.ctrl$Trans[data.both.sap.ctrl$Ring == "R6"])/years))

tran.df.e <-data.frame(apar=c(sum(data.both.sap.ctrl$Trans[data.both.sap.ctrl$Ring == "R1"])/years,
                              sum(data.both.sap.ctrl$Trans[data.both.sap.ctrl$Ring == "R4"])/years,
                              sum(data.both.sap.ctrl$Trans[data.both.sap.ctrl$Ring == "R5"])/years,
                              
                              sum(data.both.sap.p.150$Trans[data.both.sap.p.150$Ring == "R2"])/years,
                              sum(data.both.sap.p.150$Trans[data.both.sap.p.150$Ring == "R3"])/years,
                              sum(data.both.sap.p.150$Trans[data.both.sap.p.150$Ring == "R6"])/years))

wue.a <- gpp.df.a/tran.df.a
wue.e <- gpp.df.e/tran.df.e


palette(c("brown1","brown2","brown4",
          # "brown1","brown2","brown4"
          "cadetblue1","cadetblue3","deepskyblue"
))
barplot(as.matrix(wue.e),
        ylab=expression(WUE~(g~C~Kg^-1~H[2]*O)), 
        beside=TRUE,
        names.arg = paste0("R",c(1,4,5,2,3,6)),
        # col=palette(),
        col=alpha(palette(),0.3),
        border = NA,
        space = c(0,0,0,0.5,0,0),
        ylim = c(0,5))
# legend(0.5,6, 
#        c("R1","R2","R3","R4","R5","R6"), 
#        cex=1, 
#        fill=palette(),
#        xpd=TRUE,
#        horiz = TRUE,
#        inset = -0.06,
#        bty = 'n')

abline(h=mean(wue.e$gpp),col="red",lty= "dashed")
abline(h=mean(wue.a$gpp),col="lightskyblue",lty= "dashed")

# palette(c("brown1","brown2","brown4",
#           "cadetblue1","cadetblue3","deepskyblue"))


par(new=TRUE)
barplot(as.matrix(wue.a),
        ylab=" ", 
        beside=TRUE,
        names.arg = paste0("R",c(1,4,5,2,3,6)),
        # col=alpha(palette(),0.5),
        col=palette(),
        border = NA,
        space = c(0,0,0,0.5,0,0),
        ylim = c(0,5))

inc.perc <- format(round((mean(wue.e$gpp)/mean(wue.a$gpp) - 1) * 100, 1), nsmall = 1)
text(6,4.5,paste0(inc.perc,"%"))

clip(0,3, -100, 100000)
abline(h=mean(wue.e$gpp[1:3]),col="red",lty= "solid",lwd = 3)
clip(3.5,6.5, -100, 100000)
abline(h=mean(wue.a$gpp[4:6]),col="blue",lty= "solid",lwd =3)

# trans####
palette(c("brown1","brown2","brown4",
          # "brown1","brown2","brown4"
          "cadetblue1","cadetblue3","deepskyblue"
))


barplot(as.matrix(tran.df.a),
        ylab=" ", 
        beside=TRUE,
        names.arg = paste0("R",c(1,4,5,2,3,6)),
        col=alpha(palette(),0.3),
        # col=palette(),
        border = NA,
        space = c(0,0,0,0.5,0,0),
        ylim = c(0,400))

par(new=TRUE)

barplot(as.matrix(tran.df.e),
        ylab=expression(Transpiration~(mm~yr^-1)), 
        beside=TRUE,
        names.arg = paste0("R",c(1,4,5,2,3,6)),
        col=palette(),
        # col=alpha(palette(),0.3),
        border = NA,
        space = c(0,0,0,0.5,0,0),
        ylim = c(0,400))
# legend("topleft", 
#        c("R1","R2","R3","R4","R5","R6"), 
#        cex=1, 
#        fill=palette(),
#        xpd=TRUE,
#        horiz = TRUE,
#        inset = -0.06,
#        bty = 'n')

abline(h=mean(tran.df.e$apar),col="red",lty= "dashed")
abline(h=mean(tran.df.a$apar),col="blue",lty= "dashed")

inc.perc <- format(round((mean(tran.df.e$apar)/mean(tran.df.a$apar) - 1) * 100, 3), nsmall = 2)
text(6,300,paste0(inc.perc,"%"))

clip(0,3, -100, 100000)
abline(h=mean(tran.df.e$apar[1:3]),col="red",lty= "solid",lwd = 3)
clip(3.5,6.5, -100, 100000)
abline(h=mean(tran.df.a$apar[4:6]),col="blue",lty= "solid",lwd =3)

# # bar for sap#####
# data.both.sap.ctrl$sap.leaf <- data.both.sap.ctrl$volRing #/ data.both.sap$LAI
# 
# volRing.vec.e <- c(sum(data.both.sap.ctrl$sap.leaf[data.both.sap.ctrl$Ring == "R1"])/years,
#                    sum(data.both.sap.ctrl$sap.leaf[data.both.sap.ctrl$Ring == "R4"])/years,
#                    sum(data.both.sap.ctrl$sap.leaf[data.both.sap.ctrl$Ring == "R5"])/years)
# 
# volRing.vec.a <- c(sum(data.both.sap.ctrl$sap.leaf[data.both.sap.ctrl$Ring == "R2"])/years,
#                    sum(data.both.sap.ctrl$sap.leaf[data.both.sap.ctrl$Ring == "R3"])/years,
#                    sum(data.both.sap.ctrl$sap.leaf[data.both.sap.ctrl$Ring == "R6"])/years)
# 
# volRing.df <- data.frame(gpp=c(sum(data.both.sap.ctrl$sap.leaf[data.both.sap.ctrl$Ring == "R1"])/years,
#                                sum(data.both.sap.ctrl$sap.leaf[data.both.sap.ctrl$Ring == "R4"])/years,
#                                sum(data.both.sap.ctrl$sap.leaf[data.both.sap.ctrl$Ring == "R5"])/years,
# 
#                                sum(data.both.sap.ctrl$sap.leaf[data.both.sap.ctrl$Ring == "R2"])/years,
#                                sum(data.both.sap.ctrl$sap.leaf[data.both.sap.ctrl$Ring == "R3"])/years,
#                                sum(data.both.sap.ctrl$sap.leaf[data.both.sap.ctrl$Ring == "R6"])/years))
# 
# barplot(as.matrix(volRing.df),
#         ylab=expression("Transpiration"~(estimated*";"~mm~yr^-1)),
#         beside=TRUE,
#         names.arg = paste0("R",c(1,4,5,2,3,6)),
#         col=palette(),
#         # col=palette(),
#         border = NA,
#         space = c(0,0,0,0.5,0,0),
#         ylim = c(0,400))
# # legend(0.1,490,
# #        c("R1","R2","R3","R4","R5","R6"),
# #        cex=1,
# #        fill=palette(),
# #        xpd=TRUE,
# #        horiz = TRUE,
# #        inset = -0.06,
# #        bty = 'n')
# 
# abline(h=mean(volRing.vec.e),col="red",lty= "dashed")
# abline(h=mean(volRing.vec.a),col="lightskyblue",lty= "dashed")
# inc.perc <- format(round((mean(volRing.vec.e)/mean(volRing.vec.a) - 1) * 100, 1), nsmall = 1)
# text(6,300,paste0(inc.perc,"%"))
# 
# # clip(0,3, -100, 100000)
# # abline(h=mean(volRing.df$gpp[1:3]),col="red",lty= "solid",lwd = 3)
# # clip(3.5,6.5, -100, 100000)
# # abline(h=mean(volRing.df$gpp[4:6]),col="blue",lty= "solid",lwd =3)
# 

# # difference of trans######
# 
# data.both.sap.ctrl$diff <- data.both.sap.ctrl$Trans - data.both.sap.ctrl$volRing
# 
# diff.df <- data.frame(gpp=c(sum(data.both.sap.ctrl$diff[data.both.sap.ctrl$Ring == "R1"]),
#                             sum(data.both.sap.ctrl$diff[data.both.sap.ctrl$Ring == "R4"]),
#                             sum(data.both.sap.ctrl$diff[data.both.sap.ctrl$Ring == "R5"]),
#                             
#                             sum(data.both.sap.ctrl$diff[data.both.sap.ctrl$Ring == "R2"]),
#                             sum(data.both.sap.ctrl$diff[data.both.sap.ctrl$Ring == "R3"]),
#                             sum(data.both.sap.ctrl$diff[data.both.sap.ctrl$Ring == "R6"])))
# 
# 
# barplot(as.matrix(diff.df),
#         ylab=expression(Residual~("MAESPA - estimates;"~mm~yr^-1)), 
#         beside=TRUE,
#         names.arg = paste0("R",c(1,4,5,2,3,6)),
#         col=palette(),
#         # col=palette(),
#         border = NA,
#         space = c(0,0,0,0.5,0,0))
# # legend("topleft", 
# #        c("R1","R2","R3","R4","R5","R6"), 
# #        cex=1, 
# #        fill=palette(),
# #        xpd=TRUE,
# #        horiz = TRUE,
# #        inset = -0.06,
# #        bty = 'n')

######
dev.off()
