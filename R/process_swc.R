library(data.table)
# depth.in <- 25
# read neutron probe data ####
# swc.neutron.df <- read.csv("data/FACE_P0018_RA_NEUTRON_20120430-20180215_L1.csv")
swc.neutron.df <- readRDS('data/npsoilwater.RDS')

swc.neutron.df <- swc.neutron.df[swc.neutron.df$Location != "Outside",]
swc.neutron.df$Date <- as.Date(as.character(swc.neutron.df$Date))
swc.neutron.df$Ring <- as.character(swc.neutron.df$Ring)

layers.vec <- c(25,diff(unique(swc.neutron.df$Depth)[order(unique(swc.neutron.df$Depth))]))
lay.df <- data.frame(Depth = unique(swc.neutron.df$Depth)[order(unique(swc.neutron.df$Depth))],
                     layers = layers.vec )
swc.neutron.df <- merge(swc.neutron.df,lay.df)

neu.max.min.df <- data.table(swc.neutron.df)[,list(
                  VWC.min = min(VWC,na.rm = T),
                  VWC.max = max(VWC,na.rm = T)
                  ),by = c('Probe.ID','Depth')]

swc.neutron.df <- merge(swc.neutron.df,neu.max.min.df)
swc.neutron.df$VWC.norm <- (swc.neutron.df$VWC - swc.neutron.df$VWC.min) / 
  (swc.neutron.df$VWC.max - swc.neutron.df$VWC.min)
get.mean.vwc.func <- function(swc.neutron.df,depth.in){
  test.df <- data.table(swc.neutron.df[swc.neutron.df$Depth <= depth.in,])[,list(
    VWC = sum(VWC * layers,na.rm = T) / sum(layers,na.rm = T)
  ),by = c('Date','Ring')]
  return(test.df)
}
# get.mean.vwc.func(swc.neutron.df,50)
depth.vec <- unique(swc.neutron.df$Depth)[order(unique(swc.neutron.df$Depth))]

swc.ls <- list()
for(i in seq_along(depth.vec)){
  # if (i == 1){
  #   swc.ls[[i]] <- get.mean.vwc.func(swc.neutron.df,depth.vec[i])
  #   names(swc.ls[[i]])[names(swc.ls[[i]]) == 'VWC'] <- paste0('neutron',depth.vec[i])
  # }else{
  #   swc.ls[[i]] <- data.frame(VWC=get.mean.vwc.func(swc.neutron.df,depth.vec[i])$VWC)
  #   names(swc.ls[[i]]) <- paste0('neutron',depth.vec[i])
  
  swc.ls[[i]] <- get.mean.vwc.func(swc.neutron.df,depth.vec[i])
  # }
  swc.ls[[i]]$Depth <-  depth.vec[i]
}

swc.neu.ad.df <- do.call(rbind,swc.ls)
# library(tidyr)
# 
# swc.neu.ad.df.long <- gather(swc.neu.ad.df, depth,vwc,
#                              neutron25:neutron450)
# 
# swc.neu.ad.df.long$depth <- as.numeric(gsub('[[:alpha:]]','',swc.neu.ad.df.long$depth))
# 
# swc.neu.ad.df.long <- swc.neu.ad.df.long[complete.cases(swc.neu.ad.df.long$vwc),]
swc.neu.ad.df.long <- swc.neu.ad.df
swc.neu.ad.df.long$probe <- 'neutron'
# 
# 
# get norm
get.mean.vwc.norm.func <- function(swc.neutron.df,depth.in){
  test.df <- data.table(swc.neutron.df[swc.neutron.df$Depth <= depth.in,])[,list(
    VWC = sum(VWC.norm * layers,na.rm = T) / sum(layers,na.rm = T)
  ),by = c('Date','Ring')]
  return(test.df)
}

depth.vec <- unique(swc.neutron.df$Depth)[order(unique(swc.neutron.df$Depth))]

swc.nm.ls <- list()
for(i in seq_along(depth.vec)){
  # if (i == 1){
  #   swc.nm.ls[[i]] <- get.mean.vwc.norm.func(swc.neutron.df,depth.vec[i])
  #   names(swc.nm.ls[[i]])[names(swc.nm.ls[[i]]) == 'VWC.norm'] <- paste0('norm',depth.vec[i])
  # }else{
  #   swc.nm.ls[[i]] <- data.frame(VWC=get.mean.vwc.norm.func(swc.neutron.df,depth.vec[i])$VWC)
  #   names(swc.nm.ls[[i]]) <- paste0('norm',depth.vec[i])
  # }
  swc.nm.ls[[i]] <- get.mean.vwc.norm.func(swc.neutron.df,depth.vec[i])
  swc.nm.ls[[i]]$Depth <-  depth.vec[i]
}

swc.neu.norm.df <- do.call(rbind,swc.nm.ls)
library(tidyr)

# swc.neu.norm.df.long <- gather(swc.neu.norm.df, depth,vwc,
#                              norm25:norm450)
# 
# swc.neu.norm.df.long$depth <- as.numeric(gsub('[[:alpha:]]','',swc.neu.norm.df.long$depth))
# 
# swc.neu.norm.df.long <- swc.neu.norm.df.long[complete.cases(swc.neu.norm.df.long$vwc),]
swc.neu.norm.df.long <- swc.neu.norm.df
swc.neu.norm.df.long$probe <- 'norm'
# swc.neu.ad.df <- swc.neu.ad.df[complete.cases(swc.neu.ad.df)]

# plot(vwc~Date,data = swc.neu.ad.df.long[swc.neu.ad.df.long$Ring=='R1' &
#                                      swc.neu.ad.df.long$depth == 25,])

# saveRDS(swc.neu.ad.df.long,'cache/neu.ad.rds')
swc.neu.all.df <- rbind(swc.neu.norm.df.long,swc.neu.ad.df.long)


# read tdr#####
swc.day.df <- readRDS('cache/swc.day.df.rds')
swc.tdr.df <- swc.day.df[,c('Date','Ring','swc.tdr')]
swc.tdr.df$Depth <- 50
swc.tdr.df$probe <- 'tdr'
names(swc.tdr.df)[names(swc.tdr.df) == 'swc.tdr'] <- 'VWC'
# swc.tdr.df <- subset(swc.tdr.df,select = names(swc.neu.all.df))
# swc.tdr.df[,names(swc.neu.all.df)]


swc.all.df <- rbind(swc.neu.all.df,swc.tdr.df)
saveRDS(swc.all.df,'cache/swc.all.rds')
