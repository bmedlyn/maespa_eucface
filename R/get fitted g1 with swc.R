# get g1~swc
if(file.exists("cache/fit.g1.swc.rds")){
  fit.nl.both <- readRDS("cache/fit.g1.swc.rds")
}else{
  source("r/g1_np_swc.r")
}

# if(file.exists("cache/g1.euc.ros.rds")){
#   test.df <- readRDS("cache/g1.euc.ros.rds")
# }else{
#   source("r/g1_np_swc.r")
# }

fit.nl.both <- readRDS("cache/fit.g1.swc.rds")

# g1.swc.df <- readRDS("cache/g1.swc.df.rds")

# test.df <- readRDS("cache/g1.euc.ros.rds")
# source("r/process neutron.r")
swc.df <- readRDS('cache/swc.all.rds')
swc.df$treat <- NA 
swc.df$treat[swc.df$Ring%in% paste0('R',c(1,4,5))] <- 'E' 
swc.df$treat[swc.df$Ring%in% paste0('R',c(2,3,6))] <- 'A' 
swc.df.sum <- summaryBy(VWC~Date + treat + probe + depth,
                        data = swc.df,FUN=mean,keep.names = T)
# # source("r/process TDR.r")
# # swc.depth.df.all <- swc.day.df
# # swc.depth.df.all$VWC <- swc.depth.df.all$swc.tdr
# swc.depth.df.all <- swc.neutron.ring.df
# # (swc.depth.df.all$swc.tdr.5 + swc.depth.df.all$swc.tdr.30) / 2
beta.func <- function(swc,swc.max,swc.min,q){
  5 * ((swc - swc.min) / (swc.max - swc.min))^q
}
# 
# norm2obs.func <- function(g1.norm,g1.vec){
#   g1.norm * max(g1.vec)
# }

library(doBy)
swc.50.df <- summaryBy(VWC ~ Date + Ring,
                       data = swc.df.sum[swc.df.sum$probe == 'neutron' & swc.df.sum$Depth == 150,],
                       FUN=mean,na.rm=TRUE,keep.names = TRUE)

# pred.norm <- beta.func(swc.50.df$VWC/100,coef(fit.nl.tdr)[[1]],coef(fit.nl.tdr)[[2]],coef(fit.nl.tdr)[[3]])
# swc.50.df$g1 <- norm2obs.func(pred.norm,test.df$g1)

swc.50.df$g1 <- beta.func(swc.50.df$VWC/100,coef(fit.nl.both)[[1]],coef(fit.nl.both)[[2]],coef(fit.nl.both)[[3]])
swc.50.df <- swc.50.df[complete.cases(swc.50.df),]
saveRDS(swc.50.df,'cache/g1 swc.rds')
# plot(g1~VWC,data = swc.50.df,type='p',pch=16)
swc.50.df <- readRDS('cache/g1 swc.rds')
# swc.g1.old <- readRDS('E:/maespa test/maespa_high_d_backup/cache/g1 swc.rds')

# plot(g1~VWC.neu,data = swc.g1.old)
# points(g1~VWC,data = swc.50.df,pch=16,col='red')
# 
# plot(g1~Date,data = swc.g1.old)
# points(g1~Date,data = swc.50.df,pch=16,col='red')
