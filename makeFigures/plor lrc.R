# get theta and alpha from LRC######
lrc.df <- read.csv("download/euc data/FACE_P0069_RA_GASEX-LRC_20141009-20170209_L1-V2.csv")
# the arrhenius function
arrhenius <- function(k25 = 100, Ea = 60, Rgas = 0.008314, TTK = 293.15) {
  fn <- k25 * exp((Ea*(TTK - 298.15))/(298.15*Rgas*TTK)) 
  return(fn)
}

lrc.df <-lrc.df[lrc.df$Datatype ==  "compLRC",]
lrc.df$gammas <- arrhenius(42.75,37830.0/1000, TTK =c(lrc.df$Tleaf + 273.15))
lrc.df$apar <- lrc.df$PARi * (1- 0.093 -0.082)
lrc.df.low.par <- lrc.df[lrc.df$PARi < 100,]
fit.amb.lrc <- lm(Photo~apar,
                data = lrc.df.low.par[lrc.df.low.par$C.treat == 'amb',])

fit.ele.lrc <- lm(Photo~apar,
                data = lrc.df.low.par[lrc.df.low.par$C.treat == 'elev',])

fit.a.lrc <- lm(Photo~apar,
                data = lrc.df.low.par)

alpha.a <- coef(summary(fit.a.lrc))[2]

alpha.j <- mean(4*alpha.a*(lrc.df.low.par$Ci + 2*lrc.df.low.par$gammas)/
                  (lrc.df.low.par$Ci - lrc.df.low.par$gammas))

restult.lrc <- coef(nls(Photo~
                          alpha.a * apar+ Pm - ((alpha.a * apar+ Pm)^2-4*alpha.a * apar*Pm*theta)^0.5/2/theta,
                        data=lrc.df,start = list(Pm = 30, theta = 0.5)))

restult.lrc.e <- coef(nls(Photo~
                          alpha.a * apar+ Pm - ((alpha.a * apar+ Pm)^2-4*alpha.a * apar*Pm*theta)^0.5/2/theta,
                        data=lrc.df[lrc.df$C.treat == 'elev',],start = list(Pm = 30, theta = 0.5)))

restult.lrc.a <- coef(nls(Photo~
                          alpha.a * apar+ Pm - ((alpha.a * apar+ Pm)^2-4*alpha.a * apar*Pm*theta)^0.5/2/theta,
                        data=lrc.df[lrc.df$C.treat == 'amb',],start = list(Pm = 30, theta = 0.5)))

restult.lrc <- data.frame(theta = mean(c(restult.lrc.e[[2]],restult.lrc.a[[2]])))

restult.lrc$alpha.j <- alpha.j

# restult.lrc <- as.data.frame(restult.lrc)


palette(c('lightskyblue','red'))
plot(Photo~apar,data = lrc.df,pch=16,
     xlab = expression(Q[L]~(mu*mol~m^-2~s^-1)),
     ylab = expression(A[net]~(mu*mol~m^-2~s^-1)),
     col=as.factor(C.treat),cex=0.6)
Pm <- restult.lrc.a[[1]]
theta <- restult.lrc.a[[2]]
photo.pred.a <- with(lrc.df[lrc.df$C.treat == 'amb',],
                     alpha.a * apar+ Pm - ((alpha.a * apar+ Pm)^2-4*alpha.a * apar*Pm*theta)^0.5/2/theta)
Pm <- restult.lrc.e[[1]]
theta <- restult.lrc.e[[2]]
photo.pred.e <- with(lrc.df[lrc.df$C.treat == 'elev',],
                     alpha.a * apar+ Pm - ((alpha.a * apar+ Pm)^2-4*alpha.a * apar*Pm*theta)^0.5/2/theta)
plot.df <- data.frame(x = lrc.df$apar[lrc.df$C.treat == 'amb'],
                      y= photo.pred.a)
plot.df <- plot.df[order(plot.df$x),]
points(y~x,data = plot.df,type='l',lwd=2,col='lightskyblue')


plot.df <- data.frame(x = lrc.df$apar[lrc.df$C.treat == 'elev'],
                      y= photo.pred.e)
plot.df <- plot.df[order(plot.df$x),]
points(y~x,data = plot.df,type='l',lwd=2,col='red')
legend('bottomright',legend = c('A','E'),pch=16,col=palette(),bty='n')


