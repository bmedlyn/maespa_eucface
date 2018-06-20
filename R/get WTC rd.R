# Note that the WTC data is focused on getting rd 25 and thus has very small T range  (24-27)
library(HIEv)
# get WTC Rd 
WTC.rd.df <- downloadCSV("WTC_TEMP_CM_GX-Rdark25_20130617-20140402_L2")
WTC.rd.df.amb <-WTC.rd.df[WTC.rd.df$T_treatment == "amb" & WTC.rd.df$Water_treatment == "control",]
WTC.rd.df.amb$campaign <- as.factor(WTC.rd.df.amb$date)

# get EUCFACE rd
rd.df <- downloadCSV("FACE_P0064_RA_GASEXCHANGE-RdarkT_20160215-L1.csv")
# data clean
rd.df$Ring <-substr(rd.df$Tree,2,2)
rd.df <- rd.df[rd.df$Tleaf<40,]
wrong.curve <- rd.df$CurveID[rd.df$Tleaf < 40 & rd.df$Photo < -4]
rd.df <- rd.df[!rd.df$CurveID %in% wrong.curve,]

# make plots 
palette(rainbow(10))
plot(Photo~Tleaf,data = rd.df,col="grey",pch=3)
points(Photo~Tleaf,data = WTC.rd.df.amb,col=campaign,pch=16)
legend("topright",legend = levels(WTC.rd.df.amb$campaign),col=palette(),pch=16,bty='n')
legend("bottomleft",legend = c("WTC","EUCFACE"),col="black",pch=c(16,3),bty='n')

plot(Photo~Tleaf,data = WTC.rd.df.amb,col=campaign,pch=16)
legend("topright",legend = levels(WTC.rd.df.amb$campaign),col=palette(),pch=16,bty='n')

# get rd 25 and q10f for maespa 
# utility functions
fit.rd.func <- function(df){
  nls(Photo~rd.25*exp(q10*(Tleaf - 25)),
      data = df,
      start=list(rd.25=-1,q10=0.05))
}
fit.rd.t.function <- function(rdark.csv){
  fit.all <- fit.rd.func(rdark.csv)
  coef.all <- coef(fit.all)
  return(coef.all)
}

# fit the t response
# note that only one r25 and q10 is used for all ther ings at all time
# the reason is after cleaning some rings do not has measurements!!!
rd.t.vec <- fit.rd.t.function(rd.df)

