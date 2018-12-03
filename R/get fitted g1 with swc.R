# get g1~swc
if(file.exists("cache/fit.g1.swc.rds")){
  fit.nl.both <- readRDS("cache/fit.g1.swc.rds")
}else{
  source("r/g1_swc.r")
}

if(file.exists("cache/g1.euc.ros.rds")){
  test.df <- readRDS("cache/g1.euc.ros.rds")
}else{
  source("r/g1_swc.r")
}

fit.nl.both <- readRDS("cache/fit.g1.swc.rds")

test.df <- readRDS("cache/g1.euc.ros.rds")
# source("r/process neutron.r")
swc.neutron.ring.df <- readRDS('cache/swc.rds')
# # source("r/process TDR.r")
# # swc.depth.df.all <- swc.day.df
# # swc.depth.df.all$VWC <- swc.depth.df.all$swc.tdr
# swc.depth.df.all <- swc.neutron.ring.df
# # (swc.depth.df.all$swc.tdr.5 + swc.depth.df.all$swc.tdr.30) / 2
beta.func <- function(swc,swc.max,swc.min,q){
  4.3 * ((swc - swc.min) / (swc.max - swc.min))^q
}
# 
# norm2obs.func <- function(g1.norm,g1.vec){
#   g1.norm * max(g1.vec)
# }

library(doBy)
swc.50.df <- summaryBy(vwc.neu ~ Date + Ring,
                       data = swc.neutron.ring.df,
                       FUN=mean,na.rm=TRUE,keep.names = TRUE)

# pred.norm <- beta.func(swc.50.df$VWC/100,coef(fit.nl.tdr)[[1]],coef(fit.nl.tdr)[[2]],coef(fit.nl.tdr)[[3]])
# swc.50.df$g1 <- norm2obs.func(pred.norm,test.df$g1)

swc.50.df$g1 <- beta.func(swc.50.df$vwc.neu/100,coef(fit.nl.tdr)[[1]],coef(fit.nl.tdr)[[2]],coef(fit.nl.tdr)[[3]])
swc.50.df <- swc.50.df[complete.cases(swc.50.df),]
saveRDS(swc.50.df,'cache/g1 swc.rds')
# plot(g1~VWC,data = swc.50.df,type='p',pch=16)
swc.50.df <- readRDS('cache/g1 swc.rds')
