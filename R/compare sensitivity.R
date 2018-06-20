get.maespa.out.func <- function(maespa.df){
  # maespa.df <- readRDS("E:/maespa test/maespa-eucface-gpp/output/maestraVPD/mastra and sap.rds")
  maespa.df$year <- lubridate::year(maespa.df$Date)
  see <- doBy::summaryBy(GPP + Ra + Trans~year + Ring,data = maespa.df,FUN=sum)
  see <- see[see$year %in% c(2014,2015,2016),]
  see.amb <- see[see$Ring %in% c(paste0("R",c(2,3,6))),]
  
  see$c.treat <- NA
  see$c.treat[see$Ring %in% c(paste0("R",c(2,3,6)))] <- "A"
  see$c.treat[see$Ring %in% c(paste0("R",c(1,4,5)))] <- "E"
  
  see.amb.sum <- doBy::summaryBy(GPP.sum + Ra.sum + Trans.sum~c.treat,data = see,FUN=mean)
  return(see.amb.sum)
}

out.400.df <- get.maespa.out.func(readRDS("E:/maespa test/maespa-eucface-gpp/output/vj16/400/mastra and sap.rds"))
out.550.df <- get.maespa.out.func(readRDS("E:/maespa test/maespa-eucface-gpp/output/vj16/550/mastra and sap.rds"))
out.l.p.df <- get.maespa.out.func(readRDS("E:/maespa test/maespa-eucface-gpp/output/lai1.25/mastra and sap.rds"))
out.l.m.df <- get.maespa.out.func(readRDS("E:/maespa test/maespa-eucface-gpp/output/lai0.75/mastra and sap.rds"))
out.vc.p.df <- get.maespa.out.func(readRDS("E:/maespa test/maespa-eucface-gpp/output/vc0.71875/mastra and sap.rds"))
out.vc.m.df <- get.maespa.out.func(readRDS("E:/maespa test/maespa-eucface-gpp/output/vc0.53125/mastra and sap.rds"))
out.lv.m.df <- get.maespa.out.func(readRDS("E:/maespa test/maespa-eucface-gpp/output/LV/mastra and sap.rds"))

out.lv.new.m.df <- get.maespa.out.func(readRDS("E:/maespa test/maespa-eucface-gpp/output/LV1.0740.57875/mastra and sap.rds"))
out.l.p.new.df <- get.maespa.out.func(readRDS("E:/maespa test/maespa-eucface-gpp/output/lai1.074/mastra and sap.rds"))
out.l.m.new.df <- get.maespa.out.func(readRDS("E:/maespa test/maespa-eucface-gpp/output/lai0.926/mastra and sap.rds"))
