peaked <- function(k25 = 100, Ea = 60, Ed = 200, 
                   Rgas = 0.008314, delS = 0.640, TTK = 293.15) {
  fn <- k25 * exp((Ea*(TTK - 298.15))/(298.15*Rgas*TTK)) * 
    (1+exp((298.15*delS - Ed)/(298.15*Rgas))) / 
    (1+exp((TTK*delS-Ed)/(TTK*Rgas)))
  return(fn)
}

# from kristine's email 
# &jmaxpars	
# theta = 0.7	
# eavj = 19640	
# edvj = 200000	
# delsj = 628	
# ajq = 0.324	
# /	
#   
#   &vcmaxpars	
# eavc = 56310	
# edvc = 200000	
# delsc = 632	

t.seq <- seq(5,50,0.01) + 273.15
jmax.seq <- peaked(k25 = 100, Ea = 19.64, Ed = 200, 
                   Rgas = 0.008314, delS = 0.628, TTK = t.seq)

vcmax.seq <- peaked(k25 = 100, Ea = 56.31, Ed = 200, 
                   Rgas = 0.008314, delS = 0.632, TTK = t.seq)
pdf("t response of vc and j in maespa.pdf",width = 10,height = 7)
par(mar=c(5,5,5,5),mfrow=c(1,1))
plot(vcmax.seq~c(t.seq - 273.15),type="l",col="coral",ylim=c(0,200),
     xlab="T (Celsius)",ylab="Vcmax/Jmax")
abline(v = c(t.seq - 273.15)[which(vcmax.seq == max(vcmax.seq))],
       lty="dashed")
par(new=TRUE)
plot(jmax.seq~c(t.seq - 273.15),type="l",col="navy",ylim=c(0,200),
     xlab=" ",ylab=" ")
abline(v = c(t.seq - 273.15)[which(jmax.seq == max(jmax.seq))],
       lty="dashed")
legend("topleft",legend = c("Vcmax","Jmax"),
       lty = "solid",col=c("coral","navy"),cex=0.6,
       title = "Jmax25 = Vcmax25 = 100",bty='n')
dev.off()