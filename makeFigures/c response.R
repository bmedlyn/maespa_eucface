data.both.sap.ctrl <- readRDS("output/maestraVPD/mastra and sap.rds")
data.both.sap.m.150 <- readRDS("mastra and sap_1_0.8_-150_0.rds")
data.both.sap.p.150 <- readRDS("mastra and sap_1_0.8_150_0.rds")
# data.both.sap.m.150 <- data.both.sap.m.150[year(data.both.sap.m.150$Date) != 2017,]
# data.both.sap.p.150 <- data.both.sap.p.150[year(data.both.sap.p.150$Date) != 2017,]
# get start and end day
con.ls <- readLines("Rings/Ring1/runfolder/confile.dat")

sd <- gsub("startdate = ","",
           con.ls[grep("startdate", con.ls)])
sd <- gsub("'","",sd) %>%  as.Date("%d/%m/%y")


ed <- gsub("enddate = ","",
           con.ls[grep("enddate", con.ls)])
ed <- gsub("'","",ed) %>%  as.Date("%d/%m/%y")
years <- as.numeric(ed-sd)/365


data.both.sap.ctrl$c_treat <- NA
data.both.sap.ctrl$c_treat[data.both.sap.ctrl$Ring %in% c("R1","R4","R5")] <- "E"
data.both.sap.ctrl$c_treat[data.both.sap.ctrl$Ring %in% c("R2","R3","R6")] <- "A"
data.both.sap.ctrl$c_treat <- as.factor(data.both.sap.ctrl$c_treat)
plot((GPP/APAR)~Date, data = data.both.sap.ctrl,
     pch=16,cex=0.5,
     ylab=expression(LUE~(g~C~MJ^-1)),col=c_treat)

Mode <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}


plot.ring.c.inc.func <- function(data.both.sap.ctrl,data.both.sap.m.150,ring){
  df.r1 <- data.frame(Date = unique(data.both.sap.ctrl$Date),
                      GPP.e = data.both.sap.ctrl$GPP[data.both.sap.ctrl$Ring == ring],
                      GPP.a = data.both.sap.m.150$GPP[data.both.sap.m.150$Ring == ring],
                      apar = data.both.sap.m.150$APAR[data.both.sap.m.150$Ring == ring])
  df.r1$c.inc <- df.r1$GPP.e/df.r1$GPP.a
  plot(c.inc~Date,data = df.r1,
       ylab="CO2 response %",
       col="darkgrey",pch=16,cex=0.5)
  legend("topleft",legend=ring,bty='n')
  # abline(h=mean(df.r1$c.inc,na.rm = T),lty="solid",col="grey80",lwd=2)
  abline(h=median(Mode(df.r1$c.inc)),lty="solid",col="grey80",lwd=2)
}

pdf("c response 2jv.pdf",width = 8,height = 10)
par(mfrow=c(2,1))
par(mar=c(5,5,3,3))
plot.ring.c.inc.func(data.both.sap.ctrl,data.both.sap.m.150,"R1")
# plot.ring.c.inc.func(data.both.sap.ctrl,data.both.sap.m.150,"R4")
# plot.ring.c.inc.func(data.both.sap.ctrl,data.both.sap.m.150,"R5")

plot.ring.c.inc.func(data.both.sap.p.150,data.both.sap.ctrl,"R2")
# plot.ring.c.inc.func(data.both.sap.p.150,data.both.sap.ctrl,"R3")

met.df <- readRDS("output/maestraVPD/mastra and sap hr.rds")
met.2014.df <- met.df[year(met.df$DateTime) == 2014,]
met.r1.df <- met.2014.df[met.2014.df$Ring == "R1",]

library(lubridate)
plot((CA/(CA-150))~DateTime,data = met.r1.df[met.r1.df$PAR >0.05,],type="s")
plot((CA)~DateTime,data = met.r1.df[met.r1.df$PAR >0.05,],type="s",ylim=c(350,600))
legend("topleft",legend="R1",bty='n')
abline(h=550,lty="dashed",col="grey")
abline(h=mean(met.r1.df$CA[met.r1.df$PAR >0.05]))
points((CA-150)~DateTime,data = met.r1.df[met.r1.df$PAR >0.05,],type="s",col="grey")
abline(h=mean(met.r1.df$CA[met.r1.df$PAR >0.05]-150))

met.r2.df <- met.2014.df[met.2014.df$Ring == "R2",]

plot(CA~DateTime,data = met.r2.df[hour(met.r2.df$DateTime) %in% seq(9,16),],type="s",ylim=c(300,500))
legend("topleft",legend="R2",bty='n')
abline(h=400,lty="dashed",col="grey")
abline(h=median(met.r2.df$CA))

dev.off()
