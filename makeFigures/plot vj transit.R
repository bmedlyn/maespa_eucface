euc.sum.df <- readRDS("cache/ecu_aci_sum.rds")
euc.sum.df$vj <- euc.sum.df$Jmax / euc.sum.df$Vcmax
euc.sum.df$Date <- as.Date(euc.sum.df$Date,'%d/%m/%y')
euc.sum.df <- euc.sum.df[year(euc.sum.df$Date) %in% c(2013:2016),]
palette(c("coral","navy","cadetblue3",
          "brown2","brown4","deepskyblue"
))
plot(vj~Date,data = euc.sum.df,pch=16,col=Ring,
     xlab='',ylab='J : V')
abline(h = mean(euc.sum.df$vj[euc.sum.df$Ring %in% c(1,4,5)]),lty ='dashed',lwd=3,col = 'red')
abline(h = mean(euc.sum.df$vj[euc.sum.df$Ring %in% c(2,3,6)]),lty ='dashed',lwd=3,col = 'navy')
legend("topleft",pch=16,legend = paste0('R',1:6),col=palette(),bty='n',horiz = TRUE)

library(plantecophys)
get.vj.trans.func <- function(par){
  a.out <- Photosyn(g1 = 4.3,
                    Vcmax = 90,
                    Jmax = 90*1.7,
                    
                    Ca = 400,
                    VPD = 1.5,
                    PPFD = par,
                    Tleaf= tleaf,
                    
                    alpha = 0.3, theta = 0.4756,
                    
                    EaV = 74189, 
                    EdVC = 2e+05, 
                    delsC = 641.989, 
                    
                    EaJ = 39513,
                    EdVJ = 2e+05, 
                    delsJ = 640.2658)
  a.vj.diff <- a.out$Aj - a.out$Ac
  return(abs(a.vj.diff))
}

calc_d <- function(tair,rh){
  a = 613.75 # correct units
  b = 17.502
  c = 240.97
  # calc_esat <- function(tair) a * exp( (b * tair) / (c + tair) )
  esat = a * exp( (b * tair) / (c + tair) )
  
  kpa_2_pa = 1000.
  pa_2_kpa = 1.0 / kpa_2_pa
  
  
  # esat = calc_esat(1:25)
  ea = rh / 100. * esat
  vpd = (esat - ea) * pa_2_kpa
  return(vpd)
}


# par = 1200
# tleaf <- 15
t.vec <- seq(5,30,1)
d.vec <- calc_d(t.vec,rh=0.6)
par.vec <- c()
for (i in 1:length(t.vec)){
  
  tleaf <- t.vec[i]
  
  par.vec[i] <- optim(800,get.vj.trans.func,method = 'L-BFGS-B',lower = 200,upper = 10000)$par
}

par.vec.0.85 <- c()
for (i in 1:length(t.vec)){
  
  get.vj.trans.func <- function(par){
    a.out <- Photosyn(g1 = 4.3,
                      Vcmax = 90,
                      Jmax = 90*2,
                      
                      Ca = 400,
                      VPD = 1.5,
                      PPFD = par,
                      Tleaf=tleaf,
                      
                      alpha = 0.3, theta = 0.4756,
                      
                      EaV = 74189, 
                      EdVC = 2e+05, 
                      delsC = 641.989, 
                      
                      EaJ = 39513,
                      EdVJ = 2e+05, 
                      delsJ = 640.2658)
    a.vj.diff <- a.out$Aj - a.out$Ac
    return(abs(a.vj.diff))
  }
  tleaf <- t.vec[i]
  
  par.vec.0.85[i] <- optim(800,get.vj.trans.func,method = 'L-BFGS-B',lower = 200,upper = 4000)$par
}

par.vec.apar <- par.vec * (1 - 0.093 - 0.082)
par.vec.0.85.apar <- par.vec.0.85 * (1 - 0.093 - 0.082)

par(mar=c(5,5,1,1))
plot(log10(par.vec)~t.vec,xlim=c(4,31),type='l',
     xlab=expression(T[leaf]~(degree*C)),
     ylab=expression(PAR~(mu*mol~m^-2~s^-1)),
     yaxt='n',ylim=c(2,log10(4000)))
points(log10(par.vec.0.85)~t.vec,type='l',col='red')
axis(2,at = log10(c(100,1000,2000,4000)),labels = c(100,1000,2000,4000))
abline(h=log10(1800),lty='dashed',lwd=3)
# abline(h=1800,lty='dashed',lwd=3,col='green')
legend('topleft',legend = c('2','1.7'),
       title = expression(J*V~ratio),lty = 'solid',col=c('red','black'))

# 
# 
# 
# Photosyn(g1 = 3.8,
#          Vcmax = 90,
#          Jmax = 90*1.7,
#          
#          Ca = 400,
#          VPD = 1,
#          PPFD = 1800,
#          Tleaf = 25)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# Photosyn(g1 = 4.3,
#          Vcmax = 90,
#          Jmax = 90*1.7,
# 
#          Ci = 250,
#          VPD = 1.3,
#          PPFD = 1800,
#          Tleaf=25.2,
# 
#          alpha = 0.3, theta = 0.4756,
# 
#          EaV = 74189.7435218429,
#          EdVC = 2e+05,
#          delsC = 641.989,
# 
#          EaJ = 39513,
#          EdVJ = 2e+05,
#          delsJ = 640.2658,
#          Rd = 1.3*0.7)
# 
# 
# 
# euc.acis.df <- euc.acis.df[complete.cases(euc.acis.df$Photo),]
# 
# see <- Photosyn(g1 = 4.3,
#                 Vcmax = 90,
#                 Jmax = 90*1.7,
#                 
#                 Ci = euc.acis.df$Ci,
#                 VPD = 1.5,
#                 PPFD = 1800,
#                 Tleaf=euc.acis.df$Tleaf,
#                 
#                 alpha = 0.3, theta = 0.4756,
#                 
#                 EaV = 74189.7435218429,
#                 EdVC = 2e+05,
#                 delsC = 641.989,
#                 
#                 EaJ = 39513,
#                 EdVJ = 2e+05,
#                 delsJ = 640.2658,
#                 Rd = 1.3*0.7)
# 
# 
# plot(see$ALEAF~euc.acis.df$Photo)
# abline(a=0,b=1)
# 
# 
# plot(Photo~Ci,data = euc.acis.df,ylim=c(5,55),
#      pch=16,col='black')
# axis(1,at = seq(0,1600,100),labels = seq(0,1600,100))
# points(ALEAF~Ci,data = see[order(see$Ci),],type='l',col='red')
# 
# # plot(c(Ac~Aj),data = see,ylim=c(5,55),
# #      pch=16,col='black')
# # 
# # abline(a=0,b=1)
# # 
# 
# euc.acis.df <- read.csv("data/Aci.EucFACE.csv")
