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
source("r/process TDR.r")
swc.depth.df.all <- swc.day.df
swc.depth.df.all$VWC <- (swc.depth.df.all$swc.tdr.5 + swc.depth.df.all$swc.tdr.30) / 2
beta.func <- function(swc,swc.max,swc.min,q){
  ((swc - swc.min) / (swc.max - swc.min))^q
}

norm2obs.func <- function(g1.norm,g1.vec){
  g1.norm * max(g1.vec)
}

library(doBy)
swc.50.df <- summaryBy(VWC ~ Date + Ring,
                       data = swc.depth.df.all,
                       FUN=mean,na.rm=TRUE,keep.names = TRUE)

pred.norm <- beta.func(swc.50.df$VWC/100,coef(fit.nl.both)[[1]],coef(fit.nl.both)[[2]],coef(fit.nl.both)[[3]])
swc.50.df$g1 <- norm2obs.func(pred.norm,test.df$g1)
saveRDS(swc.50.df,'cache/g1 swc.rds')
# plot(g1~VWC,data = swc.50.df,type='p',pch=16)
