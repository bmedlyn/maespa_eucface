library(HIEv)
library(doBy)
setToken()

#  get gs and lwp from hiev####
downloadHIEv(searchHIEv('FACE_PMULTI_RA_gasexchange_L1_20120402-20181127_1'),
             topath = 'download/')

downloadHIEv(searchHIEv('FACE_PMULTI_RA_waterpotential_L1_'),
             topath = 'download/')

gas.df <- read.csv('download/FACE_PMULTI_RA_gasexchange_L1_20120402-20181127_1.csv')


# wp.df <- read.csv('download/FACE_PMULTI_RA_waterpotential_L1_20120508-20181127_1.csv')

# get addtional aci data for the first points
aci.df <- readRDS('cache/euc.1stAci.rds')
aci.df$Ring <- as.factor(paste0('R',aci.df$Ring))

# combine gax ex data and data cln#####
gas.df$source <- 'spots'
aci.df$source <- 'aci'

gas.df <- gas.df[,c("Date","Campaign",'Ring','source',
                    "Photo","Cond","VpdL","CO2S","RH_S")]
gas.df$Campaign <- as.character(gas.df$Campaign)

aci.df <- aci.df[,c("Date","Campaign",'Ring','source',
                    "Photo","Cond","VpdL","CO2S","RH_S")]
aci.df$Campaign <- as.character(aci.df$Campaign)

aci.df$Campaign <- unlist(lapply(strsplit(aci.df$Campaign,'_'),
                                 function(str.vec) paste0(str.vec[2],str.vec[1])))

aci.df <- aci.df[aci.df$Campaign != '2017May',]
gx.df <- rbind(gas.df,aci.df)
gx.df$Date <- as.Date(as.character(gx.df$Date))
gx.df$Date[is.na(gx.df$Date) & gx.df$Campaign == '2012Oct'] <- '2012-10-30'

gx.df$treat <- NA
gx.df$treat[gx.df$Ring %in% paste0('R',c(1,4,5))] <- 'E'
gx.df$treat[gx.df$Ring %in% paste0('R',c(2,3,6))] <- 'A'

gx.df$group <- paste0(gx.df$Campaign,'_',gx.df$treat)

gx.df <- gx.df[gx.df$Cond >0,]
gx.df <- gx.df[!gx.df$Campaign %in% c('2012May','2016Feb'),]

# fit g1####

# fit.first <- fitBBs(gx.df[gx.df$source == 'aci',],"group",gsmodel = c("BBOpti"),
#                     varnames = list(ALEAF = "Photo", GS = "Cond", VPD = "VpdL", 
#                                     Ca = "CO2S", RH = "RH_S"))
# 
# first.se.ls <- lapply(fit.first,function(df) coef(summary(df$fit)))
# first.se.df <- do.call(rbind,first.se.ls)[,c('Estimate','Std. Error')]
# first.result <- merge(coef(fit.first),first.se.df,by.x='g1',by.y='Estimate')
# 
# first.g1 <- merge(first.result,gx.df[gx.df$source == 'aci',],by.x="group",by.y="group")
# first.g1 <- first.g1[,c('Date',"Campaign",'treat','source','g1')]

# fit.spots <- fitBBs(gx.df[gx.df$source == 'spots' ,],"group",gsmodel = c("BBOpti"),
#                     varnames = list(ALEAF = "Photo", GS = "Cond", VPD = "VpdL", 
#                                     Ca = "CO2S", RH = "RH_S"))
# 
# spots.se.ls <- lapply(fit.spots,function(df) coef(summary(df$fit)))
# spots.se.df <- do.call(rbind,spots.se.ls)[,c('Estimate','Std. Error')]
# spots.result <- merge(coef(spots.se.ls),spots.se.df,by.x='g1',by.y='Estimate')
# 
# spots.g1 <- merge(coef(fit.spots),gx.df[gx.df$source == 'spots',],by.x="group",by.y="group")
# spots.g1 <- spots.g1[,c('Date',"Campaign",'treat','source','g1')]
library(plantecophys)
get.g1.se.func <- function(gx.df){
  
  lens <- summaryBy(Cond~group,data=gx.df,FUN=length)
  keep <- lens$group[lens$Cond.length > 4]
  gx.df <- subset(gx.df,group %in% keep)
  
  fit.first <- fitBBs(gx.df,"group",gsmodel = c("BBOpti"),
                      varnames = list(ALEAF = "Photo", GS = "Cond", VPD = "VpdL", 
                                      Ca = "CO2S", RH = "RH_S"))
  
  first.se.ls <- lapply(fit.first,function(df) coef(summary(df$fit)))
  first.se.df <- do.call(rbind,first.se.ls)[,c('Estimate','Std. Error')]
  first.result <- merge(coef(fit.first),first.se.df,by.x='g1',by.y='Estimate')
  
  first.g1 <- merge(first.result,gx.df,by.x="group",by.y="group")
  first.g1 <- first.g1[,c('Date',"Campaign",'treat','source','g1','Std. Error')]
  
  return(first.g1)
}
# first.g1 <- get.g1.se.func(gx.df[gx.df$source == 'aci',])
# spots.g1 <- get.g1.se.func(gx.df[gx.df$source == 'spots',])
g1.euc.df <- get.g1.se.func(gx.df)

# g1.euc.df <- rbind(first.g1,spots.g1)
g1.euc.df <- g1.euc.df[!duplicated(g1.euc.df),]
names(g1.euc.df)[names(g1.euc.df)=='Std. Error'] <- 'stdv'
library(doBy)
g1.euc.df <- summaryBy(Date + g1 + stdv~Campaign+treat,data = g1.euc.df,FUN = mean.POSIXct,keep.names = T)
library(zoo)
g1.euc.df$Date <- as.Date(summaryBy(Date~.,data = g1.euc.df,FUN = mean.POSIXct)$Date)

# # lwp####
# wp.df$Date <- as.Date(as.character(wp.df$Date))
# wp.df$Campaign <- as.character(wp.df$Campaign)

# put swc and g1 together#####
# get swc
swc.df <- readRDS('cache/swc.all.rds')
swc.df$treat <- NA 
swc.df$treat[swc.df$Ring%in% paste0('R',c(1,4,5))] <- 'E' 
swc.df$treat[swc.df$Ring%in% paste0('R',c(2,3,6))] <- 'A' 
swc.df.sum.tmp <- summaryBy(VWC~Date + treat + probe + Depth,
                            data = swc.df,FUN=mean,keep.names = T)

# see <- swc.df.sum.tmp[swc.df.sum.tmp$probe == 'norm',]
# library(data.table)
# np.sum.df <- data.table(np.df)[,list(s.min=min(VWC,na.rm = T),
#                                      s.max=max(VWC,na.rm = T)),
#                                by = c('Probe.ID','Depth','Ring')]
# 
# np.std.df <- merge(np.df,np.sum.df,all=TRUE)
# np.std.df$vwc.norm <- (np.std.df$VWC - np.std.df$s.min) /(np.std.df$s.max - np.std.df$s.min)
# np.std.df$treat <- NA 
# np.std.df$treat[np.std.df$Ring%in% paste0('R',c(1,4,5))] <- 'E' 
# np.std.df$treat[np.std.df$Ring%in% paste0('R',c(2,3,6))] <- 'A' 
# np.std.df.ring <- summaryBy(vwc.norm~ Date + treat + Depth, data = np.std.df,
#                             FUN=mean,na.rm=TRUE,keep.names = TRUE)
# 
# # np.std.df.ring$probe <- 'neu.norm'
# 
# names(np.std.df.ring)[names(np.std.df.ring) == 'Depth'] <- 'depth'

# swc.df.sum <- rbind(swc.df.sum.tmp,np.std.df.ring)
swc.df.sum <- swc.df.sum.tmp
# get the swc for the g1
find.swc.func <- function(g1.euc.df,swc.df.sum,treat.in,probe.in){
  g1.ring.df <- g1.euc.df[g1.euc.df$treat == treat.in,]
  swc.ring.df <- swc.df.sum[swc.df.sum$treat == treat.in &
                              swc.df.sum$probe == probe.in,]
  # j=2
  r.out.ls <- list()
  for(j in 1:length(unique(g1.ring.df$Date))){
    diff.day <- swc.ring.df$Date - unique(g1.ring.df$Date)[j]
    min.diff <- min(abs(diff.day),na.rm=TRUE)
    if(min.diff < 14){
      sele.index <- which(abs(diff.day) == min.diff)[1]
    }else{
      sele.index <- NA
    }
    
    if(is.na(sele.index) != TRUE){
      r.out.ls[[j]] <- swc.ring.df[swc.ring.df$Date == swc.ring.df$Date[sele.index],]
      r.out.ls[[j]]$g1 <- g1.ring.df$g1[j]
      r.out.ls[[j]]$stdv <- g1.ring.df$stdv[j]
      r.out.ls[[j]]$Campaign <- g1.ring.df$Campaign[j]
      # r.out.ls[[j]]$source <- g1.ring.df$source[j]
      
      # r.out.ls[[j]] <-cbind(g1.ring.df[g1.ring.df$Date == unique(g1.ring.df$Date)[j],],
      #                       swc.ring.df[sele.index,])
      
    }else{
      r.out.ls[[j]] <- NULL
    }
  }
  return(do.call(rbind,r.out.ls))
}

g1.swc.ls <- list()

g1.swc.ls[[1]] <- find.swc.func(g1.euc.df,swc.df.sum,'E','tdr')
g1.swc.ls[[2]] <- find.swc.func(g1.euc.df,swc.df.sum,'E','neutron')
g1.swc.ls[[3]] <- find.swc.func(g1.euc.df,swc.df.sum,'A','neutron')
g1.swc.ls[[4]] <- find.swc.func(g1.euc.df,swc.df.sum,'A','tdr')
g1.swc.ls[[5]] <- find.swc.func(g1.euc.df,swc.df.sum,'E','norm')
g1.swc.ls[[6]] <- find.swc.func(g1.euc.df,swc.df.sum,'A','norm')

g1.swc.df <- do.call(rbind,g1.swc.ls)

g1.swc.df$treat <- as.factor(g1.swc.df$treat)

g1.swc.df.sum <- summaryBy(.~ treat + probe + Campaign + Depth ,
                           data = g1.swc.df,FUN=mean,na.rm=TRUE,keep.names = TRUE,id=~Date)

g1.swc.df.sum <- g1.swc.df.sum[complete.cases(g1.swc.df.sum),]
# g1.swc.df.sum$vwc <- g1.swc.df.sum$VWC
# names(g1.swc.df.sum)[names(g1.swc.df.sum) =='VWC'] <- 'vwc'
saveRDS(g1.swc.df.sum,'g1.swc.df.rds')

# startlist = list(swc.max=0.3,swc.min=0.001 ,q = 1)

fit.df <- g1.swc.df.sum[g1.swc.df.sum$probe =='neutron' &
                          g1.swc.df.sum$Depth == 150 ,]


test.df <- data.frame(g1 = fit.df$g1,
                      # g1.norm = dat$g1.norm,
                      swc = fit.df$VWC/100)

# fit.nl.np <- nls(g1 ~ 5*((swc - swc.min) / (swc.max - swc.min))^q, 
#                   data = test.df,
#                   start=startlist,algorithm="port",
#                   lower=c(0.1,0.001,0.1),upper=c(0.8,0.001,3))

startlist = list(swc.max=0.4,swc.min=0.1 ,q = 1)

fit.nl.tdr <- nls(g1 ~ 5*((swc - swc.min) / (swc.max - swc.min))^q, 
                  data = test.df,
                  start=startlist,algorithm="port",
                  lower=c(0.2,0.05,0.1),upper=c(0.6,0.109,3))


saveRDS(fit.nl.tdr,"cache/fit.g1.swc.rds")

saveRDS(g1.swc.df,"cache/g1.swc.df.rds")
# 
# saveRDS(fit.nl.tdr,"cache/fit.g1.swc.rds")
# 
# saveRDS(test.df,"cache/g1.euc.ros.rds")





