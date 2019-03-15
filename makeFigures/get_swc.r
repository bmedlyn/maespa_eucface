library(plantecophys)
library(HIEv)
library(doBy)
# read neutron probe data ####
swc.neutron.df <- read.csv("data/FACE_P0018_RA_NEUTRON_20120430-20180215_L1.csv")
swc.neutron.df <- swc.neutron.df[swc.neutron.df$Location != "Outside",]
swc.neutron.df$Date <- as.Date(as.character(swc.neutron.df$Date))
swc.neutron.df$Ring <- as.character(swc.neutron.df$Ring)

layers <- c(25,diff(unique(swc.neutron.df$Depth)[order(unique(swc.neutron.df$Depth))]))
lay.df <- data.frame(Depth = unique(swc.neutron.df$Depth)[order(unique(swc.neutron.df$Depth))],
                     layers = layers )
swc.neutron.df <- merge(swc.neutron.df,lay.df)
# swc.neutron.ring.df.300 <- doBy::summaryBy(VWC~Date+Ring,
#                                            data = swc.neutron.df[swc.neutron.df$Depth <= 300,],
#                                            FUN=mean,na.rm=TRUE,
#                                            keep.names = TRUE,id= ~CO2)

get.mean.vwc.func <- function(swc.neutron.df,depth.in){
  test.df <- data.table(swc.neutron.df[swc.neutron.df$Depth <= depth.in,])[,list(
    VWC = sum(VWC * layers) / depth.in
  ),by = c('Date','Ring')]
  return(test.df)
}

# swc.neutron.ring.df.300$VWC[swc.neutron.ring.df.300$VWC < 0] <- 0
# swc.neutron.ring.df.300$vwc.300 <- swc.neutron.ring.df.300$VWC
# 
# swc.neutron.ring.df.400 <- doBy::summaryBy(VWC~Date+Ring,
#                                            data = swc.neutron.df[swc.neutron.df$Depth <= 400,],
#                                            FUN=mean,na.rm=TRUE,
#                                            keep.names = TRUE,id=~CO2)
# swc.neutron.ring.df.400$VWC[swc.neutron.ring.df.400$VWC < 0] <- 0
# swc.neutron.ring.df.400$vwc.400 <- swc.neutron.ring.df.400$VWC
# 
# swc.neutron.ring.df <- merge(swc.neutron.ring.df.300[,c('Date','Ring','vwc.300')],swc.neutron.ring.df.400)

swc.neutron.ring.df <- get.mean.vwc.func(swc.neutron.df,450)
# use only numbers listed 
# 25  50  75 100 125 150 200 250 300 350 400 450

swc.neutron.ring.df$vwc.neu <- get.mean.vwc.func(swc.neutron.df,151)$VWC
swc.neutron.ring.df <- swc.neutron.ring.df[order(swc.neutron.ring.df$Date),]
saveRDS(swc.neutron.ring.df,'cache/swc.rds')
