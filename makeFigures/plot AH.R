data.both.sap <- readRDS("output/maestraVPD/mastra and sap hr.rds")
r1.df <- data.both.sap[data.both.sap$Ring == 'R1',]

e.sat.func <- function(TAC){
  613.75*exp(17.502*TAC/(240.97+TAC))
}

r1.df$e.abs <- r1.df$RH * e.sat.func(r1.df$TAIR) 

r1.df$AH <- r1.df$e.abs / 461.5 / r1.df$TAIR / 10


plot(e.abs~DateTime,data = r1.df[as.Date(r1.df$DateTime) %in% 
                                   as.Date(c('2013-11-07','2013-11-08')),])


plot(RH~DateTime,data = r1.df[as.Date(r1.df$DateTime) %in% 
                                   as.Date(c('2013-11-07','2013-11-08')),])

plot(TAIR~DateTime,data = r1.df[as.Date(r1.df$DateTime) %in% 
                                as.Date(c('2013-11-07','2013-11-08')),])

plot(AH~DateTime,data = r1.df[as.Date(r1.df$DateTime) %in% 
                                  as.Date(c('2013-11-07','2013-11-08')),],
     ylab=expression(AH~(kg~m^-3)),
     main = "7 and 8 Nov 2013")

par(mfrow=c(2,2),mar=c(5,5,1,1))

# plot(AH~DateTime,data = r1.df,ylim=c(0,0.2),ylab=expression(AH~(kg~m^-3)))


plot(e.abs~DateTime,data = r1.df[as.Date(r1.df$DateTime) %in% 
                                   as.Date(c('2013-11-07','2013-11-08')),])


plot(RH~DateTime,data = r1.df[as.Date(r1.df$DateTime) %in% 
                                as.Date(c('2013-11-07','2013-11-08')),])

plot(TAIR~DateTime,data = r1.df[as.Date(r1.df$DateTime) %in% 
                                  as.Date(c('2013-11-07','2013-11-08')),])


plot(AH~DateTime,data = r1.df[as.Date(r1.df$DateTime) %in% 
                                as.Date(c('2013-09-09','2013-09-10','2013-09-11','2013-09-12')),],
     ylab=expression(AH~(kg~m^-3)),
     main = "9 and 11 Sep 2013")
