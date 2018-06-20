# maespa.vpd.df <- readRDS("output/maestraVPD/mastra and sap 05hr.rds")
# maespa.s.e.vpd.df <- readRDS("output/maestraVPD/all.hr.rds")

get.soile.func <- function(maespa.vpd.df,maespa.s.e.vpd.df){
  maespa.s.e.vpd.df <- maespa.s.e.vpd.df[,c("DateTime",'Ring',"soil.e","swc.5","swc.30")]
  time.seq <- data.frame(DateTime =seq.POSIXt(min(maespa.s.e.vpd.df$DateTime),
                                              max(maespa.s.e.vpd.df$DateTime),"30 min"))
  maespa.s.e.vpd.05hr.df <- merge(time.seq,maespa.s.e.vpd.df,by="DateTime",all=TRUE)
  maespa.s.e.vpd.05hr.df$soil.e <- zoo::na.locf(maespa.s.e.vpd.05hr.df$soil.e)
  maespa.vpd.df.all <- merge(maespa.vpd.df,maespa.s.e.vpd.05hr.df,by=c("DateTime"),all.x=TRUE)
  return(maespa.vpd.df.all)
}

maespa.vpd.df.all <-get.soile.func(readRDS("output/maestraVPD/mastra and sap 05hr.rds"),
                                   readRDS("output/maestraVPD/all.hr.rds"))

maespa.vpd.df.all$year <- year(maespa.vpd.df.all$DateTime)
library(doBy)
et.yr <- summaryBy(Photo + NPP + trans + soil.e~ year + Ring,data =maespa.vpd.df.all,
                   FUN=c(sum),na.rm=TRUE,keep.names = T)

et.yr$Ra <- et.yr$Photo - et.yr$NPP

write.csv(et.yr,"annual flx.csv",row.names = FALSE)