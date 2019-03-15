pdf("400to550.pdf",width = 8,height = 6)

library(dplyr)
library(scales)
par(mar=c(5,5,2,2))
# read daily fluxes############
# data.both.sap.m.150 <- readRDS("mastra and sap_1_0.8_-150_0.rds")
# data.both.sap.p.150 <- readRDS("mastra and sap_1_0.8_150_0.rds")
# data.both.sap.ctrl <- readRDS("output/maestraVPD/mastra and sap.rds")
data.both.sap.m.150 <- readRDS("output/vj16/400/mastra and sap.rds")
data.both.sap.p.150 <- readRDS("output/vj16/550/mastra and sap.rds")
data.both.sap.p.150$photo <- data.both.sap.p.150$GPP/(data.both.sap.p.150$LAI-0.8)
data.both.sap.m.150$photo <- data.both.sap.m.150$GPP/(data.both.sap.m.150$LAI-0.8)

# get start and end day
con.ls <- readLines("Rings/Ring1/runfolder/confile.dat")
sd <- gsub("startdate = ","",
           con.ls[grep("startdate", con.ls)])
sd <- gsub("'","",sd) %>%  as.Date("%d/%m/%y")
ed <- gsub("enddate = ","",
           con.ls[grep("enddate", con.ls)])
ed <- gsub("'","",ed) %>%  as.Date("%d/%m/%y")
years <- as.numeric(ed-sd)/365

# annual sum photo ########

photo.df.e <- data.frame(photo=c(sum(data.both.sap.p.150$photo[data.both.sap.p.150$Ring == "R1"])/years,
                                 sum(data.both.sap.p.150$photo[data.both.sap.p.150$Ring == "R4"])/years,
                                 sum(data.both.sap.p.150$photo[data.both.sap.p.150$Ring == "R5"])/years,
                                 
                                 sum(data.both.sap.p.150$photo[data.both.sap.p.150$Ring == "R2"])/years,
                                 sum(data.both.sap.p.150$photo[data.both.sap.p.150$Ring == "R3"])/years,
                                 sum(data.both.sap.p.150$photo[data.both.sap.p.150$Ring == "R6"])/years))

photo.df.a <- data.frame(photo=c(sum(data.both.sap.m.150$photo[data.both.sap.m.150$Ring == "R1"])/years,
                                 sum(data.both.sap.m.150$photo[data.both.sap.m.150$Ring == "R4"])/years,
                                 sum(data.both.sap.m.150$photo[data.both.sap.m.150$Ring == "R5"])/years,
                                 
                                 sum(data.both.sap.m.150$photo[data.both.sap.m.150$Ring == "R2"])/years,
                                 sum(data.both.sap.m.150$photo[data.both.sap.m.150$Ring == "R3"])/years,
                                 sum(data.both.sap.m.150$photo[data.both.sap.m.150$Ring == "R6"])/years))
# gpp.df.e/gpp.df.a
palette(c("brown1","brown2","brown4",
          # "brown1","brown2","brown4"
          "cadetblue1","cadetblue3","deepskyblue"
))
barplot(as.matrix(photo.df.e),
        ylab=' ', 
        beside=TRUE,
        names.arg = paste0("R",c(1,4,5,2,3,6)),
        # col=palette(),
        col=alpha(palette(),0.3),
        border = NA,
        space = c(0,0,0,0.5,0,0),
        ylim = c(0,1600))
# legend("topleft", 
#        c("R1","R2","R3","R4","R5","R6"), 
#        cex=1, 
#        fill=palette(),
#        xpd=TRUE,
#        horiz = TRUE,
#        inset = -0.06,
#        bty = 'n')

abline(h=mean(photo.df.e$photo),col="red",lty= "dashed")
abline(h=mean(photo.df.a$photo),col="lightskyblue",lty= "dashed")

# palette(c("brown1","brown2","brown4",
#           "cadetblue1","cadetblue3","deepskyblue"))


par(new=TRUE)
barplot(as.matrix(photo.df.a),
        ylab=expression(Photo~(g~C~m^-2~yr^-1)), 
        beside=TRUE,
        names.arg = paste0("R",c(1,4,5,2,3,6)),
        # col=alpha(palette(),0.5),
        col=palette(),
        border = NA,
        space = c(0,0,0,0.5,0,0),
        ylim = c(0,1600))
clip(0,3, -100, 100000)
abline(h=mean(photo.df.e$photo[1:3]),col="red",lty= "solid",lwd = 3)
clip(3.5,6.5, -100, 100000)
abline(h=mean(photo.df.a$photo[4:6]),col="blue",lty= "solid",lwd =3)


inc.perc <- format(round((mean(photo.df.e$photo)/mean(photo.df.a$photo) - 1) * 100, 1), nsmall = 1)
text(6,820,paste0(inc.perc,"%"))

# annual GPP####
gpp.df.e <- data.frame(gpp=c(sum(data.both.sap.p.150$GPP[data.both.sap.p.150$Ring == "R1"])/years,
                             sum(data.both.sap.p.150$GPP[data.both.sap.p.150$Ring == "R4"])/years,
                             sum(data.both.sap.p.150$GPP[data.both.sap.p.150$Ring == "R5"])/years,
                             
                             sum(data.both.sap.p.150$GPP[data.both.sap.p.150$Ring == "R2"])/years,
                             sum(data.both.sap.p.150$GPP[data.both.sap.p.150$Ring == "R3"])/years,
                             sum(data.both.sap.p.150$GPP[data.both.sap.p.150$Ring == "R6"])/years))

gpp.df.a <- data.frame(gpp=c(sum(data.both.sap.m.150$GPP[data.both.sap.m.150$Ring == "R1"])/years,
                             sum(data.both.sap.m.150$GPP[data.both.sap.m.150$Ring == "R4"])/years,
                             sum(data.both.sap.m.150$GPP[data.both.sap.m.150$Ring == "R5"])/years,
                             
                             sum(data.both.sap.m.150$GPP[data.both.sap.m.150$Ring == "R2"])/years,
                             sum(data.both.sap.m.150$GPP[data.both.sap.m.150$Ring == "R3"])/years,
                             sum(data.both.sap.m.150$GPP[data.both.sap.m.150$Ring == "R6"])/years))
# gpp.df.e/gpp.df.a
palette(c("brown1","brown2","brown4",
          # "brown1","brown2","brown4"
          "cadetblue1","cadetblue3","deepskyblue"
))
barplot(as.matrix(gpp.df.e),
        ylab=' ', 
        beside=TRUE,
        names.arg = paste0("R",c(1,4,5,2,3,6)),
        # col=palette(),
        col=alpha(palette(),0.3),
        border = NA,
        space = c(0,0,0,0.5,0,0),
        ylim = c(0,1600))

abline(h=mean(gpp.df.e$gpp),col="red",lty= "dashed")
abline(h=mean(gpp.df.a$gpp),col="lightskyblue",lty= "dashed")

# palette(c("brown1","brown2","brown4",
#           "cadetblue1","cadetblue3","deepskyblue"))


par(new=TRUE)
barplot(as.matrix(gpp.df.a),
        ylab=expression(GPP~(g~C~m^-2~yr^-1)), 
        beside=TRUE,
        names.arg = paste0("R",c(1,4,5,2,3,6)),
        # col=alpha(palette(),0.5),
        col=palette(),
        border = NA,
        space = c(0,0,0,0.5,0,0),
        ylim = c(0,1600))
clip(0,3, -100, 100000)
abline(h=mean(gpp.df.e$gpp[1:3]),col="red",lty= "solid",lwd = 3)
clip(3.5,6.5, -100, 100000)
abline(h=mean(gpp.df.a$gpp[4:6]),col="blue",lty= "solid",lwd =3)


inc.perc <- format(round((sum(gpp.df.e$gpp)/sum(gpp.df.a$gpp) - 1) * 100, 1), nsmall = 1)
text(6,820,paste0(inc.perc,"%"))
# lue #####
gpp.stand.df.e <- data.frame(gpp=c(sum(data.both.sap.p.150$GPP[data.both.sap.p.150$Ring == "R1"])/years,
                                   sum(data.both.sap.p.150$GPP[data.both.sap.p.150$Ring == "R4"])/years,
                                   sum(data.both.sap.p.150$GPP[data.both.sap.p.150$Ring == "R5"])/years,
                                   
                                   sum(data.both.sap.p.150$GPP[data.both.sap.p.150$Ring == "R2"])/years,
                                   sum(data.both.sap.p.150$GPP[data.both.sap.p.150$Ring == "R3"])/years,
                                   sum(data.both.sap.p.150$GPP[data.both.sap.p.150$Ring == "R6"])/years))

gpp.stand.df.a <- data.frame(gpp=c(sum(data.both.sap.m.150$GPP[data.both.sap.m.150$Ring == "R1"])/years,
                                   sum(data.both.sap.m.150$GPP[data.both.sap.m.150$Ring == "R4"])/years,
                                   sum(data.both.sap.m.150$GPP[data.both.sap.m.150$Ring == "R5"])/years,
                                   
                                   sum(data.both.sap.m.150$GPP[data.both.sap.m.150$Ring == "R2"])/years,
                                   sum(data.both.sap.m.150$GPP[data.both.sap.m.150$Ring == "R3"])/years,
                                   sum(data.both.sap.m.150$GPP[data.both.sap.m.150$Ring == "R6"])/years))

apar.df <-data.frame(apar=c(sum(data.both.sap.m.150$APAR[data.both.sap.m.150$Ring == "R1"])/years,
                            sum(data.both.sap.m.150$APAR[data.both.sap.m.150$Ring == "R4"])/years,
                            sum(data.both.sap.m.150$APAR[data.both.sap.m.150$Ring == "R5"])/years,
                            
                            sum(data.both.sap.m.150$APAR[data.both.sap.m.150$Ring == "R2"])/years,
                            sum(data.both.sap.m.150$APAR[data.both.sap.m.150$Ring == "R3"])/years,
                            sum(data.both.sap.m.150$APAR[data.both.sap.m.150$Ring == "R6"])/years))


lue.a <- gpp.stand.df.a/apar.df
lue.e <- gpp.stand.df.e/apar.df


palette(c("brown1","brown2","brown4",
          # "brown1","brown2","brown4"
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
                              
                              sum(data.both.sap.m.150$Trans[data.both.sap.m.150$Ring == "R2"])/years,
                              sum(data.both.sap.m.150$Trans[data.both.sap.m.150$Ring == "R3"])/years,
                              sum(data.both.sap.m.150$Trans[data.both.sap.m.150$Ring == "R6"])/years))

tran.df.e <-data.frame(apar=c(sum(data.both.sap.p.150$Trans[data.both.sap.p.150$Ring == "R1"])/years,
                              sum(data.both.sap.p.150$Trans[data.both.sap.p.150$Ring == "R4"])/years,
                              sum(data.both.sap.p.150$Trans[data.both.sap.p.150$Ring == "R5"])/years,
                              
                              sum(data.both.sap.p.150$Trans[data.both.sap.p.150$Ring == "R2"])/years,
                              sum(data.both.sap.p.150$Trans[data.both.sap.p.150$Ring == "R3"])/years,
                              sum(data.both.sap.p.150$Trans[data.both.sap.p.150$Ring == "R6"])/years))

wue.a <- gpp.stand.df.a/tran.df.a
wue.e <- gpp.stand.df.e/tran.df.e


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
        ylim = c(0,6))

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
        ylim = c(0,6))

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

abline(h=mean(tran.df.e$apar),col="red",lty= "dashed")
abline(h=mean(tran.df.a$apar),col="blue",lty= "dashed")

inc.perc <- format(round((mean(tran.df.e$apar)/mean(tran.df.a$apar) - 1) * 100, 3), nsmall = 2)
text(6,300,paste0(inc.perc,"%"))

clip(0,3, -100, 100000)
abline(h=mean(tran.df.e$apar[1:3]),col="red",lty= "solid",lwd = 3)
clip(3.5,6.5, -100, 100000)
abline(h=mean(tran.df.a$apar[4:6]),col="blue",lty= "solid",lwd =3)


dev.off()
