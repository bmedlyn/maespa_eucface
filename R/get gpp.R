library(lubridate)
library(doBy)
get.annual.func <- function(maespa.df){
  # maespa.df <- readRDS("output/ambient/mastra and sap.rds")
  maespa.df$year <- year(maespa.df$Date)
  maespa.df$volRing[maespa.df$volRing > 2.5] <- NA
  see.flx <- summaryBy(GPP + Ra + Trans + volRing + APAR~ year + Ring,data = maespa.df,FUN=sum,na.rm=T)
  see.lai <- summaryBy(LAI ~ year + Ring,data = maespa.df,FUN=mean)
  see <- merge(see.flx,see.lai)
  
  # see <- see[see$year %in% c(2014,2015,2016),]
  see.amb <- see[see$Ring %in% c(paste0("R",c(2,3,6))),]
  
  see$c.treat <- NA
  see$c.treat[see$Ring %in% c(paste0("R",c(2,3,6)))] <- "A"
  see$c.treat[see$Ring %in% c(paste0("R",c(1,4,5)))] <- "E"
  
  
  rm(sd)
  see.ring.sum <- summaryBy(GPP.sum + Ra.sum + Trans.sum  + volRing.sum + APAR.sum~ year+Ring,data = see,FUN=mean)
  return(see.ring.sum)
}
maespa.df <- readRDS("output/ambient/mastra and sap.rds")
amb.df <- get.annual.func(maespa.df)

maespa.df.e <- readRDS("output/elevated/mastra and sap.rds")
ele.df <- get.annual.func(maespa.df.e)

maespa.df.accli <- readRDS("output/accli/mastra and sap.rds")
accli.df <- get.annual.func(maespa.df.accli)

a.e.df <- amb.df
a.e.df$gpp.e <- ele.df$GPP.sum.mean
a.e.df$gpp.accli <- accli.df$GPP.sum.mean
a.e.df$c.rate <- a.e.df$gpp.e / a.e.df$GPP.sum.mean
mean(a.e.df$c.rate)

a.e.df$accli.rate <- a.e.df$gpp.e / a.e.df$gpp.accli

names(a.e.df)[names(a.e.df)=="GPP.sum.mean"] <- 'gpp.a'
write.csv(a.e.df,'euc_gpp_2013_2016.csv',row.names = FALSE)
# 



mean(a.e.df$accli.rate[a.e.df$Ring %in% paste0('R',c(1,4,5))])
mean(a.e.df$GPP.sum.mean)
mean(a.e.df$gpp.e)
mean(a.e.df$gpp.a)
range(a.e.df$gpp.a)
range(a.e.df$gpp.e)

mean(a.e.df$gpp.e[a.e.df$Ring %in% paste0('R',c(1,4,5))])/
mean(a.e.df$gpp.a[a.e.df$Ring %in% paste0('R',c(2,3,6))])

gpp.yr.df <- summaryBy(.~year, data = a.e.df,
                       FUN=c(mean,sd),keep.names = T)

gpp.yr.df$sd.per <- gpp.yr.df$gpp.a.sd / gpp.yr.df$gpp.a.mean
mean(gpp.yr.df$sd.per)
gpp.ring.df <- summaryBy(.~Ring, data = a.e.df,
                       FUN=c(mean,sd),keep.names = T)

# 
# maespa.df <- readRDS("output/ambient/mastra and sap.rds")
# maespa.df$year <- year(maespa.df$Date)
# maespa.df$volRing[maespa.df$volRing > 2.5] <- NA
# see.flx <- summaryBy(GPP + Ra + Trans + soil.e + volRing + APAR~ year + Ring,data = maespa.df,FUN=sum,na.rm=T)
# see.lai <- summaryBy(LAI ~ year + Ring,data = maespa.df,FUN=mean)
# see <- merge(see.flx,see.lai)
# 
# # see <- see[see$year %in% c(2014,2015,2016),]
# see.amb <- see[see$Ring %in% c(paste0("R",c(2,3,6))),]
# 
# see$c.treat <- NA
# see$c.treat[see$Ring %in% c(paste0("R",c(2,3,6)))] <- "A"
# see$c.treat[see$Ring %in% c(paste0("R",c(1,4,5)))] <- "E"
# 
# 
# rm(sd)
# see.ring.sum <- summaryBy(GPP.sum + Ra.sum + Trans.sum + soil.e.sum + volRing.sum + APAR.sum~ year+Ring,data = see,FUN=mean)
# write.csv(see.ring.sum,'annual fluxes.csv',row.names = F)
