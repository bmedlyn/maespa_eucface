data.both.sap.ctrl <- readRDS("E:/maespa test/maespa-eucface-gpp/output/maestraVPD/mastra and sap.rds")
data.both.sap.m.150 <- readRDS("E:/maespa test/maespa-eucface-gpp/mastra and sap_1_0.8_-150_0.rds")
data.both.sap.p.150 <- readRDS("E:/maespa test/maespa-eucface-gpp/mastra and sap_1_0.8_150_0.rds")

data.both.sap.p.150.a <- data.both.sap.p.150[data.both.sap.p.150$Ring %in% c("R2","R3","R6"),]
data.both.sap.ctrl.a <- data.both.sap.ctrl[data.both.sap.ctrl$Ring %in% c("R2","R3","R6"),]

library(doBy)

data.both.sap.p.150.a.mean <- summaryBy(GPP~Date,data = data.both.sap.p.150.a,
                                        FUN = mean, na.rm=TRUE)
names(data.both.sap.p.150.a.mean) <- c("Date","GPP.e")


data.both.sap.ctrl.a.mean <- summaryBy(GPP~Date,data = data.both.sap.ctrl.a,
                                       FUN = mean, na.rm=TRUE)
names(data.both.sap.ctrl.a.mean) <- c("Date","GPP.a")
data.amb.df <- merge(data.both.sap.ctrl.a.mean,data.both.sap.p.150.a.mean,by="Date",all=TRUE)

amb.mean.inc <- sum(data.both.sap.p.150.a.mean$GPP.e, na.rm=TRUE)/sum(data.both.sap.ctrl.a.mean$GPP.a, na.rm=TRUE) 



# get evelated response 
data.both.sap.m.150.e <- data.both.sap.m.150[data.both.sap.m.150$Ring %in% c("R1","R4","R5"),]
data.both.sap.ctrl.e <- data.both.sap.ctrl[data.both.sap.ctrl$Ring %in% c("R1","R4","R5"),]

library(doBy)

data.both.sap.m.150.e.mean <- summaryBy(GPP~Date,data = data.both.sap.m.150.e,
                                        FUN = mean, na.rm=TRUE)
names(data.both.sap.m.150.e.mean) <- c("Date","GPP.a")
range(data.both.sap.ctrl.e.mean$GPP.e/data.both.sap.m.150.e.mean$GPP.a,na.rm = T)

data.both.sap.ctrl.e.mean <- summaryBy(GPP~Date,data = data.both.sap.ctrl.e,
                                       FUN = mean, na.rm=TRUE)
names(data.both.sap.ctrl.e.mean) <- c("Date","GPP.e")

ele.inc <- sum(data.both.sap.ctrl.e.mean$GPP.e, na.rm=TRUE)/sum(data.both.sap.m.150.e.mean$GPP.a, na.rm=TRUE)

pdf("c response.pdf",width = 8,height = 8)
par(mfrow=c(2,1))
par(mar=c(5,5,3,3))
plot((GPP.e/GPP.a)~Date,
     data = data.amb.df,
     xlab="Date",ylab="C response %",
     pch=16,col="grey",cex=0.5)
legend("topleft",legend = "Abmient rings",bty='n')
abline(h=amb.mean.inc)

plot((data.both.sap.ctrl.e.mean$GPP.e/data.both.sap.m.150.e.mean$GPP.a)~
       data.both.sap.m.150.e.mean$Date,
     xlab="Date",ylab="C response %",
     pch=16,col="grey",cex=0.5)
legend("topleft",legend = "Elevated rings",bty='n')
abline(h=ele.inc)
dev.off()