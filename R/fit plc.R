tere.plc.df <- read.csv("data/EucTereticornis.csv")

fit.df <- fitplc(tere.plc.df,varnames = c(PLC = "PLC", WP = "water.potential..MPa."),model = "Weibull")

coef(fit.df)[1]

plot(PLC~water.potential..MPa.,data = tere.plc.df,pch=16)
wp.vec <- seq(-7,0,0.1)
lines(x = wp.vec,y = 100*(1-fweibull(-wp.vec,coef(fit.df)[1],coef(fit.df)[2])),
     type='l')




