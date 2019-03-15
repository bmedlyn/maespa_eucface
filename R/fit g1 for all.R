# get 1st points of aci curves
clean_eucface_aci <- function(dat) {
  
  # Correct dates
  dat$Date_licor <- str_replace(dat$Date,"Thr","Thu")
  dat$Date <- parse_date_time(dat$Date_licor,c("m d y","a m d y H M S"))
  dat$Date[is.na(dat$Date)] <- as.Date("2016-10-10")
  dat$Date <- as.Date(dat$Date)
  
  # remove unrealistic data
  dat <- dat[dat$Photo < 50,]
  dat <- dat[dat$Cond > 0 ,]
  dat <- dat[dat$Photo > -10,]
  dat <- dat[dat$Ci > 10,]
  
  # Get rid of A-Ci's with less than 5 points
  len <- summaryBy(Photo~Number,data=dat,FUN=length)
  nums <- len$Number[len$Photo.length > 4]
  good <- subset(dat,Number %in% nums)
  
  return(good)
  
}
t.response.df <- readRDS("cache/ecu_t_response.rds")
euc.acis.df <- read.csv("data/Aci.EucFACE.csv")
euc.acis.df <- clean_eucface_aci(euc.acis.df)
swc.neutron.ring.df <- readRDS('cache/swc.rds')
# # # plot to check data
# plot(Photo~Ci,data = euc.acis.df[euc.acis.df$Number == 478,])
# library(plantecophys)
euc.fit <- fitacis(euc.acis.df,group="Number",Tcorrect=TRUE,fitmethod = c("bilinear"),
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
euc.all.df$Ring[is.na(euc.all.df$Ring)] <- 3
euc.all.df$Date[nchar(euc.all.df$Date) > 11 & nchar(euc.all.df$Date) < 13] <- 
  substr(euc.all.df$Date[nchar(euc.all.df$Date) > 11 & nchar(euc.all.df$Date) <13],2,12)

euc.all.df$Date[nchar(euc.all.df$Date) > 13] <- 
  substr(euc.all.df$Date[nchar(euc.all.df$Date) > 13 ],5,15)
euc.all.df$Date[euc.all.df$Date == "10-Oct-16"] <- "Oct 10 2016"

euc.amb.df <- euc.all.df#[euc.all.df$Ring %in% c(2,3,6),]

euc.1st.df <- euc.amb.df[firstobs(~Number,data=euc.all.df),] 
euc.1st.df <- euc.1st.df[!is.na(euc.1st.df$Number),]
euc.1st.df <- euc.1st.df[euc.1st.df$CO2R < 450 & euc.1st.df$CO2R > 370,]
euc.1st.df$Campaign <- droplevels(euc.1st.df$Campaign)
euc.1st.df$group <- (paste(euc.1st.df$Campaign,euc.1st.df$Ring))

temp.ls <- split(euc.1st.df,euc.1st.df$group)

for (i in 1:length(temp.ls)){
  if(nrow(temp.ls[[i]]) < 2){
    temp.ls[[i]] <- NA
  }
}

euc.long.df <- do.call(rbind,temp.ls)
euc.long.df <- euc.long.df[!is.na(euc.long.df$Number),]
library(plantecophys)
fit.first <- fitBBs(euc.long.df,"group",gsmodel = c("BBOpti"))
first.g1 <- merge(coef(fit.first),euc.long.df,by.x="group",by.y="group")
first.g1 <- first.g1[order(first.g1$Number),]
first.g1 <- first.g1[first.g1$Date > as.Date('2013-01-01'),]
first.g1$Campaign <- droplevels(first.g1$Campaign)
first.g1$Campaign <- factor(c('ACi'))

first.g1 <- first.g1[,c('Date', 'Campaign', 'Ring','g1')]
first.g1$Ring <- paste0('R',first.g1$Ring)

all.g1.euc.df <- merge(spots.g1.one,first.g1,all=TRUE)

g1.swc.ls <- list()
for(i in 1:6){
  g1.ring.df <- all.g1.euc.df[all.g1.euc.df$Ring == paste0("R",i),]
  
  swc.ring.df <- swc.neutron.ring.df[swc.neutron.ring.df$Ring == paste0("R",i),]
  
  r.out.ls <- list()
  
  for(j in 1:length(unique(g1.ring.df$Date))){
    diff.day <- swc.ring.df$Date - unique(g1.ring.df$Date)[j]
    min.diff <- min(abs(diff.day),na.rm=TRUE)
    if(min.diff < 14){
      sele.index <- which(abs(diff.day) == min.diff)[1]
    }else{
      sele.index <- NA
    }
    
    if(length(sele.index) > 0){
      r.out.ls[[j]] <- swc.ring.df[sele.index,]
      r.out.ls[[j]]$g1 <- g1.ring.df$g1[j]
      r.out.ls[[j]]$Campaign <- g1.ring.df$Campaign[j]
    }else{
      r.out.ls[[j]] <- NULL
    }
  }
  g1.swc.ls[[i]] <- do.call(rbind,r.out.ls)
}


g1.swc.df <- do.call(rbind,g1.swc.ls)
g1.swc.df <- g1.swc.df[complete.cases(g1.swc.df),]
g1.swc.df$swc.100 <- g1.swc.df$vwc.neu / 100
# 
if(!file.exists('cache/ros.gs.rds')){
  source("r/get ros g1 swc.r")
}else{
  eute.ros <- readRDS('cache/ros.gs.rds')
}
# 
eute.ros$g1.norm <- (eute.ros$g1 - min(eute.ros$g1)) / (max(eute.ros$g1) - min(eute.ros$g1))

g1.swc.df$g1.norm <- (g1.swc.df$g1 - min(g1.swc.df$g1)) / 
  (max(g1.swc.df$g1) - min(g1.swc.df$g1))

# # fitting
startlist = list(swc.max=0.5,swc.min=0 ,q = 1)

test.df <- data.frame(g1 = c(eute.ros$g1,g1.swc.df.sub$g1),
                      swc = c(eute.ros$TDR,g1.swc.df.sub$swc.100))

test.df <- test.df[test.df$g1 > 0,]
test.df <- test.df[complete.cases(test.df),]


fit.nl.tdr <- nls(g1 ~ 4.3*((swc - swc.min) / (swc.max - swc.min))^q, 
                  data = test.df,
                  start=startlist,algorithm="port",
                  lower=c(0.1,0,0.1),upper=c(2,0.1,3))

test.df$g1.pred <- predict(fit.nl.tdr)
test.df$g1.actual.pred <- test.df$g1.pred * (max(test.df$g1)) 

test.df$vwc <- test.df$swc*100
# library(ochRe)
# col.oz <- ochre_palettes$olsen_qual[7:2]
# # palette(tran.func(col.oz))
# palette(col.oz)
par(mar=c(5,5,1,1))
plot(g1~vwc.neu,data = g1.swc.df,pch=16,col='coral',xlim=c(0,50),ylim=c(0,6),
     xlab=expression(theta~('%')),ylab=expression(g[1]~(kPa^0.5)))
points(g1~c(100*TDR),data =eute.ros,pch=16,col="lightskyblue")
lines(g1.pred ~ vwc,data=test.df[order(test.df$swc),],col="blueviolet",lwd=2)
theta.vec <- seq(0,0.35,0.001)
g1.drake <- (theta.vec/0.6)^0.32*(max(test.df$g1) - min(test.df$g1)) + min(test.df$g1)
lines(g1.drake ~ c(theta.vec*100),
      col="blue",lwd=2,lty='dashed')
# legend("topleft",legend = c(paste0("xmax = ",format(coef(fit.nl.tdr)[[1]],digits = 2)),
#                             "xmin = 0",
#                             paste0("q = ",format(coef(fit.nl.tdr)[[3]],digits = 2))))
legend("bottomright",legend = c("Drake et al., 2017a","EucFACE"),
       pch=c(16),bty='n',
       col=c('lightskyblue','coral'))


