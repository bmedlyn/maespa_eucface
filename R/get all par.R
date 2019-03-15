met.in <- readRDS('cache/ca.df.rds')
euc.sum.df <- readRDS("cache/euc.vj.rds")

names(euc.sum.df)[1] <- 'Date'
met.vj.df <- merge(met.in,euc.sum.df,all.x=TRUE,by='Date')
library(zoo)
met.vj.df$v.a <- na.locf0(met.vj.df$v.a)
met.vj.df$v.a <- na.locf0(met.vj.df$v.a,fromLast = T)

met.vj.df$j.a <- na.locf0(met.vj.df$j.a)
met.vj.df$j.a <- na.locf0(met.vj.df$j.a,fromLast = T)

met.vj.df$j.e <- na.locf0(met.vj.df$j.e)
met.vj.df$j.e <- na.locf0(met.vj.df$j.e,fromLast = T)

met.vj.df$v.a <- na.locf0(met.vj.df$v.a)
met.vj.df$v.a <- na.locf0(met.vj.df$v.a,fromLast = T)

lai.df <- do.call(rbind,sm)
library(doBy)
lai.mean.df <- summaryBy(LAIsmooth~Date,data = lai.df,FUN=mean,
                         na.rm=TRUE,keep.names = T)
lai.mean.df$lai <- lai.mean.df$LAIsmooth -0.8

met.vj.lai.df <- merge(met.vj.df,lai.mean.df[,c('Date','lai')])
mean(met.vj.lai.df$lai)

swc.50.df <- readRDS('cache/g1 swc.rds')
mean(swc.50.df$g1)
write.csv(met.vj.lai.df,'euc_met.csv',row.names = F)
