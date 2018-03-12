euc.acis.df <- read.csv("data/Aci.EucFACE.csv")
# see <-  read.csv("data/E_teret_A-Ci_temperature_curves.csv")
# data clean
euc.acis.df <- euc.acis.df[euc.acis.df$Photo < 50,]
euc.acis.df <- euc.acis.df[euc.acis.df$Photo > -2,]
euc.acis.df <- euc.acis.df[euc.acis.df$Cond > 0 ,]
euc.acis.df <- euc.acis.df[complete.cases(euc.acis.df$Number),]
# # plot to check data
# plot(Photo~Ci,data = euc.acis.df[euc.acis.df$Number == 478,])
library(plantecophys)
euc.fit <- fitacis(euc.acis.df,group="Number",Tcorrect=TRUE,fitmethod = c("default"),
                   varnames = list(ALEAF = "Photo",Tleaf = "Tleaf", 
                                   Ci = "Ci", PPFD = "PARi"),
                   EaV = t.response.df["Ea","Vcmax"]*1000, 
                   EdVC = 2e+05,
                   delsC = t.response.df["delS","Vcmax"]*1000, 
                   EaJ = t.response.df["Ea","Jmax"]*1000, 
                   EdVJ = 2e+05, delsJ = t.response.df["delS","Jmax"]*1000)

see <- Filter(function(x) length(x)>1, euc.fit)
euc.coef <- as.data.frame(do.call(rbind,sapply(see,function(x) out.df <- data.frame(coef(x)))))
names(euc.coef) <- c("Vcmax","Jmax","Rd")
euc.coef$Number <- as.numeric(names(see))

# put fits and measurements together
euc.all.df <- merge(euc.coef,euc.acis.df,by = "Number")
# date format clean and change
euc.all.df$Tree <- as.factor(euc.all.df$Tree)
euc.all.df$C.treat <- as.factor(euc.all.df$C.treat)
euc.all.df$Date <- as.character(euc.all.df$Date)
euc.all.df$Date[nchar(euc.all.df$Date) > 11 & nchar(euc.all.df$Date) < 13] <- 
  substr(euc.all.df$Date[nchar(euc.all.df$Date) > 11 & nchar(euc.all.df$Date) <13],2,12)

euc.all.df$Date[nchar(euc.all.df$Date) > 13] <- 
  substr(euc.all.df$Date[nchar(euc.all.df$Date) > 13 ],5,15)
euc.all.df$Date[euc.all.df$Date == "10-Oct-16"] <- "Oct 10 2016"
euc.all.df$Date <-  as.Date(euc.all.df$Date,"%b %d %Y")
euc.all.df$Ring[is.na(euc.all.df$Ring)] <- 3
euc.all.df$c_treat <- NA
euc.all.df$c_treat[as.character(euc.all.df$Ring) %in% c(1,4,5)] <- "E"
euc.all.df$c_treat[as.character(euc.all.df$Ring) %in% c(2,3,6)] <- "A"

euc.treat.df <- summaryBy(Vcmax + Jmax ~ c_treat+Campaign,
                          data = euc.all.df,
                          id=~Date,
                          FUN=c(mean,sd),na.rm = TRUE,
                          keep.names = TRUE)
# euc.treat.df <- euc.treat.df[complete.cases(euc.treat.df$c_treat),]

euc.treat.df$c_treat <- as.factor(euc.treat.df$c_treat)


par(mfrow=c(2,1))
palette(c("red","navy","lightskyblue","coral","brown1","cornflowerblue"))
par(mar=c(0,5,5,5))
plot(c(Jmax/Vcmax)~Date,data = euc.sum.df,col=Ring,pch=16,
     xlab="",xaxt='n', ylab = "Jmax/Vcmax ratio")
legend("top",legend = levels(euc.sum.df$Ring),pch=16,
       col=palette(),bty='n',horiz = TRUE)
par(mar=c(5,5,0,5))
plot(Jmax~Date,data = euc.sum.df,col=Ring,pch=16,ylim=c(50,200),
     ylab = "Jmax and Vcmax")

points(Vcmax~Date,data = euc.sum.df,col=Ring,pch=3)

euc.treat.df <- euc.treat.df[order(euc.treat.df$Date),]

par(mfrow=c(1,1))
palette(c("navy","red"))

plot(Jmax.mean~Date,data = euc.treat.df[euc.treat.df$c_treat == "A",],
     col="navy",type="l",ylim=c(50,200),ann=FALSE,axes=FALSE)
par(new=TRUE)
plot(Jmax.mean~Date,data = euc.treat.df[euc.treat.df$c_treat == "E",],
     col="red",type="l",ylim=c(50,200),ann=FALSE,axes=FALSE)
par(new=TRUE)
plot(Vcmax.mean~Date,data = euc.treat.df[euc.treat.df$c_treat == "E",],
     col="red",type="l",ylim=c(50,200),ann=FALSE,axes=FALSE)
par(new=TRUE)
plot(Vcmax.mean~Date,data = euc.treat.df[euc.treat.df$c_treat == "A",],
     col="navy",type="l",ylim=c(50,200),ann=FALSE,axes=FALSE)
par(new=TRUE)
plot(Jmax.mean~Date,data = euc.treat.df,col=c_treat,pch=16,ylim=c(50,200),
     ylab = "Jmax and Vcmax")
arrows(euc.treat.df$Date,euc.treat.df$Jmax.mean - euc.treat.df$Jmax.sd,
       euc.treat.df$Date,euc.treat.df$Jmax.mean + euc.treat.df$Jmax.sd,
       col = euc.treat.df$c_treat,
       length=0.01, angle=90, code=3)
points(Vcmax.mean~Date,data = euc.treat.df,col=c_treat,pch=17)

arrows(euc.treat.df$Date,euc.treat.df$Vcmax.mean - euc.treat.df$Vcmax.sd,
       euc.treat.df$Date,euc.treat.df$Vcmax.mean + euc.treat.df$Vcmax.sd,
       col = euc.treat.df$c_treat,
       length=0.01, angle=90, code=3)
