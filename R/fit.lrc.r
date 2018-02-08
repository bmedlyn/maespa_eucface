lrc.df <- read.csv("download/euc data/FACE_P0069_RA_GASEX-LRC_20141009-20170209_L1-V2.csv")
arrhenius <- function(k25 = 100, Ea = 60, Rgas = 0.008314, TTK = 293.15) {
  fn <- k25 * exp((Ea*(TTK - 298.15))/(298.15*Rgas*TTK)) 
  return(fn)
}

lrc.df <-lrc.df[lrc.df$Datatype ==  "compLRC",]

lrc.df$gammas <- arrhenius(42.75,37830.0/1000, TTK =c(lrc.df$Tleaf + 273.15))
lrc.df$apar <- lrc.df$PARi * (1- 0.093 -0.082)
lrc.df.low.par <- lrc.df[lrc.df$PARi < 100,]
fit.a.lrc <- lm(Photo~apar,
                data = lrc.df.low.par)
alpha.a <- coef(summary(fit.a.lrc))[2]

alpha.j <- mean(4*alpha.a*(lrc.df.low.par$Ci + 2*lrc.df.low.par$gammas)/
                  (lrc.df.low.par$Ci - lrc.df.low.par$gammas))

restult.lrc <- coef(nls(Photo~
                          alpha.a * apar+ Pm - ((alpha.a * apar+ Pm)^2-4*alpha.a * apar*Pm*theta)^0.5/2/theta,
                        data=lrc.df,start = list(Pm = 30, theta = 0.5)))

restult.lrc$alpha.j <- alpha.j

restult.lrc <- as.data.frame(restult.lrc)
p.func <- function(apar,Pm = restult.lrc$Pm,theta = restult.lrc$theta){
  alpha.a * apar+ Pm - ((alpha.a * apar+ Pm)^2-4*alpha.a * apar*Pm*theta)^0.5/2/theta
}
curve(p.func,from = 10,to=1000,
      xlab = "APAR",
      ylab="Photo")
points(Photo~apar,data = lrc.df,
       pch=16,col="grey80",
       cex=0.5)
legend("bottomright",legend = c(paste0("alpha.a = ",format(alpha.a,digits = 2)),
                                paste0("alpha.j = ",format(restult.lrc$alpha.j,digits = 2)),
                                paste0("theta = ",format(restult.lrc$theta,digits = 2)),
                                paste0("Pm = ",format(restult.lrc$Pm,digits = 2))),
       bty='n')
