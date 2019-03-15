alpha <- function (colour, alpha = NA) {
  col <- col2rgb(colour, TRUE)/255
  if (length(colour) != length(alpha)) {
    if (length(colour) > 1 && length(alpha) > 1) {
      stop("Only one of colour and alpha can be vectorised")
    }
    if (length(colour) > 1) {
      alpha <- rep(alpha, length.out = length(colour))
    }
    else if (length(alpha) > 1) {
      col <- col[, rep(1, length(alpha)), drop = FALSE]
    }
  }
  alpha[is.na(alpha)] <- col[4, ][is.na(alpha)]
  new_col <- rgb(col[1, ], col[2, ], col[3, ], alpha)
  new_col[is.na(colour)] <- NA
  new_col
}

maespa.dayflx.df <- readRDS("output/maestraVPD/mastra and sap.rds")

years <- length(unique((maespa.dayflx.df$Date)))/365.25

# original####
# GPP
gpp.df <- data.frame(gpp=c(sum(maespa.dayflx.df$GPP[maespa.dayflx.df$Ring == "R1"])/years,
                             sum(maespa.dayflx.df$GPP[maespa.dayflx.df$Ring == "R4"])/years,
                             sum(maespa.dayflx.df$GPP[maespa.dayflx.df$Ring == "R5"])/years,
                             
                             sum(maespa.dayflx.df$GPP[maespa.dayflx.df$Ring == "R2"])/years,
                             sum(maespa.dayflx.df$GPP[maespa.dayflx.df$Ring == "R3"])/years,
                             sum(maespa.dayflx.df$GPP[maespa.dayflx.df$Ring == "R6"])/years))


par(mfcol=c(2,2))
# gpp.df.e/gpp.df.a
palette(c("brown1","brown2","brown4",
          # "brown1","brown2","brown4"
          "cadetblue1","cadetblue3","deepskyblue"
))
par(mar=c(0,5,5,1))
barplot(as.matrix(gpp.df),
        ylab=expression(GPP~(g~C~m^-2~yr^-1)), 
        beside=TRUE,
        names.arg = " ",
        # col=palette(),
        col=(palette()),
        border = NA,
        yaxt='n',
        space = c(0,0,0,0.5,0,0),
        ylim = c(0,1600))
axis(2,at=seq(0,1500,500),labels = seq(0,1500,500))
legend("topleft",legend = "(a)",bty='n')


clip(0,3, -100, 100000)
abline(h=mean(gpp.df$gpp[1:3]),col="red",lty= "solid",lwd = 3)
clip(3.5,6.5, -100, 100000)
abline(h=mean(gpp.df$gpp[4:6]),col="lightskyblue",lty= "solid",lwd =3)

inc.perc <- format(round((mean(gpp.df$gpp[1:3])/mean(gpp.df$gpp[4:6]) - 1) * 100, 1), nsmall = 1)
text(6,1300,paste0(inc.perc,"%"))

# et
et.df <- data.frame(Trans=c(sum(maespa.dayflx.df$Trans[maespa.dayflx.df$Ring == "R1"])/years,
                           sum(maespa.dayflx.df$Trans[maespa.dayflx.df$Ring == "R4"])/years,
                           sum(maespa.dayflx.df$Trans[maespa.dayflx.df$Ring == "R5"])/years,
                           
                           sum(maespa.dayflx.df$Trans[maespa.dayflx.df$Ring == "R2"])/years,
                           sum(maespa.dayflx.df$Trans[maespa.dayflx.df$Ring == "R3"])/years,
                           sum(maespa.dayflx.df$Trans[maespa.dayflx.df$Ring == "R6"])/years))
# gpp.df.e/gpp.df.a
palette(c("brown1","brown2","brown4",
          # "brown1","brown2","brown4"
          "cadetblue1","cadetblue3","deepskyblue"
))
par(mar=c(5,5,1,1))
barplot(as.matrix(et.df),
        ylab=expression(Transpiration~(kg~m^-2~yr^-1)), 
        beside=TRUE,
        names.arg = paste0("R",c(1,4,5,2,3,6)),
        # col=palette(),
        col=(palette()),
        border = NA,
        yaxt='n',
        space = c(0,0,0,0.5,0,0),
        ylim = c(0,400))
axis(2,at=seq(0,300,100),labels = seq(0,300,100))
legend("topleft",legend = "(b)",bty='n')


inc.perc <- format(round((mean(et.df$Trans[1:3])/mean(et.df$Trans[4:6]) - 1) * 100, 3), nsmall = 2)
text(6,350,paste0(inc.perc,"%"))

clip(0,3, -100, 100000)
abline(h=mean(et.df$Trans[1:3]),col="red",lty= "solid",lwd = 3)
clip(3.5,6.5, -100, 100000)
abline(h=mean(et.df$Trans[4:6]),col="blue",lty= "solid",lwd =3)

# test ####
data.both.sap.m.150 <- readRDS("output/vj16/400/mastra and sap.rds")
data.both.sap.p.150 <- readRDS("output/vj16/550/mastra and sap.rds")

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
par(mar=c(0,1,5,5))
barplot(as.matrix(gpp.df.e),
        ylab=' ', 
        beside=TRUE,
        names.arg = '',
        # col=palette(),
        col=alpha(palette(),0.3),
        border = NA,
        yaxt='n',
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
        names.arg = '',
        # col=alpha(palette(),0.5),
        col=palette(),
        border = NA,
        yaxt='n',
        space = c(0,0,0,0.5,0,0),
        ylim = c(0,1600))
axis(4,at=seq(0,1500,500),labels =seq(0,1500,500))
legend("topleft",legend = "(c)",bty='n')
# clip(0,3, -100, 100000)
# abline(h=mean(gpp.df.e$gpp[1:3]),col="red",lty= "solid",lwd = 3)
# clip(3.5,6.5, -100, 100000)
# abline(h=mean(gpp.df.a$gpp[4:6]),col="blue",lty= "solid",lwd =3)


inc.perc <- format(round((sum(gpp.df.e$gpp)/sum(gpp.df.a$gpp) - 1) * 100, 1), nsmall = 1)
text(6,1300,paste0(inc.perc,"%"))

# trans####
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
palette(c("brown1","brown2","brown4",
          # "brown1","brown2","brown4"
          "cadetblue1","cadetblue3","deepskyblue"
))

par(mar=c(5,1,1,5))
barplot(as.matrix(tran.df.a),
        ylab=" ", 
        beside=TRUE,
        names.arg = paste0("R",c(1,4,5,2,3,6)),
        col=alpha(palette(),0.3),
        # col=palette(),
        border = NA,
        yaxt='n',
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
        yaxt='n',
        space = c(0,0,0,0.5,0,0),
        ylim = c(0,400))
axis(4,at=seq(0,300,100),labels = seq(0,300,100))
legend("topleft",legend = "(d)",bty='n')

abline(h=mean(tran.df.e$apar),col="red",lty= "dashed")
abline(h=mean(tran.df.a$apar),col="blue",lty= "dashed")

inc.perc <- format(round((mean(tran.df.e$apar)/mean(tran.df.a$apar) - 1) * 100, 3), nsmall = 2)
text(6,300,paste0(inc.perc,"%"))

# clip(0,3, -100, 100000)
# abline(h=mean(tran.df.e$apar[1:3]),col="red",lty= "solid",lwd = 3)
# clip(3.5,6.5, -100, 100000)
# abline(h=mean(tran.df.a$apar[4:6]),col="blue",lty= "solid",lwd =3)

