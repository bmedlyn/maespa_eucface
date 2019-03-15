# coord_H_D <- read.csv("data/dendrometers-n-coord2013_for_TEG.csv")
# coord_H_D <- na.omit(coord_H_D)
# 
# #DBH################################################################
# coord_H_D$DBH <- coord_H_D$DIAM01.2013
# coord_H_D$BA <- (coord_H_D$DIAM01.2013/100 / 2)^2 * pi
# 
# library(doBy)
# 
# ba.ring.df <- summaryBy(BA~Ring,data = coord_H_D,
#                         FUN=sum,na.rm=T,keep.names = T)
# 
# ba.ring.df$ba.m2.ha <- ba.ring.df$BA / (12.5^2*pi) * 1e4
# ba.ring.df$Ring <- paste0('R',ba.ring.df$Ring)
# 
ring.yr.df <- read.csv('euc_gpp_2013_2016.csv' )
# ring.yr.df$Ring <- ring.yr.df$Ring.e
# ring.yr.df.ba <- merge(ring.yr.df,ba.ring.df[,c('Ring','ba.m2.ha')],by='Ring',all.x=TRUE)

ring.yr.df.ba.sum <- doBy::summaryBy(.~Ring, data  = ring.yr.df.ba,FUN=mean,na.rm=TRUE,keep.names = TRUE)
ring.yr.df.ba.sum$Ring <- as.factor(ring.yr.df.ba.sum$Ring)
ring.yr.df.ba.sum$lai.i <- c(1.0787,1.1701,1.1300,1.0823,1.2045,1.2233) - 0.8

# ring.yr.df.ba.sum$lai.i <-  lai.mean.df$LA

# ring.yr.df.ba.sum$lai.i <-  lai.un.smooth.df$LAI
# 
# palette(c("coral","cadetblue1","cadetblue3",
#           "brown2","brown4","deepskyblue"
# ))
# 
# pdf('GPP_BA.pdf',width = 8,height = 5)
# par(mar=c(5,5,1,1))
# symbol.vec <- c(16,3,3,16,16,3)
# plot(GPP~ba.m2.ha,data = ring.yr.df.ba.sum,col=Ring,pch=symbol.vec[Ring],ylim=c(1000,2000),
#      xlab=expression(BA~(m^2~ha^-1)),
#      ylab=expression(GPP~(g~C~m^-2~yr^-1)))
# points(GPP.e~ba.m2.ha,data = ring.yr.df.ba.sum,col=Ring,pch=symbol.vec[Ring])
# legend('bottomright',legend = c('Treatment','Control','E','A'),
#        pch=c(16,3,15,15),col=c('black','black','red','blue'),bty='n',ncol = 2)
# 
# legend("top",pch=16,legend = paste0('R',1:6),col=palette(),bty='n',horiz = TRUE)
# dev.off()



par(mar=c(5,5,1,1))
symbol.vec <- c(16,3,3,16,16,3)

palette(c("coral","navy","cadetblue3",
          "brown2","brown4","deepskyblue"
))

plot(gpp.a~lai.i,data = ring.yr.df.ba.sum,
     col=Ring,pch=symbol.vec[Ring],
     # xlim=c(1.05,1.3),
     ylim=c(1000,2000),
     xlab=expression(LAI[i]~(m^2~m^-2)),
     ylab=expression(GPP~(g~C~m^-2~yr^-1)))
points(gpp.e~lai.i,data = ring.yr.df.ba.sum,col=Ring,pch=symbol.vec[Ring])
legend('bottomright',legend = c('Treatment','Control','E','A'),
       pch=c(16,3,15,15),col=c('black','black','red','blue'),bty='n',ncol = 2)


legend("top",pch=16,legend = paste0('R',1:6),col=palette(),bty='n',horiz = TRUE)

abline(h = mean(ring.yr.df.ba.sum$gpp.a),lty="dashed",col='lightskyblue')
abline(h = mean(ring.yr.df.ba.sum$gpp.e) ,lty="dashed",col='coral')
clip(min(ring.yr.df.ba.sum$lai.i[c(2,3,6)]),max(ring.yr.df.ba.sum$lai.i[c(2,3,6)]), 100, 2000)
abline(h = mean(ring.yr.df.ba.sum$gpp.a[ring.yr.df.ba.sum$Ring %in% paste0('R',c(2,3,6))]),
       lty="solid",col='blue')
clip(min(ring.yr.df.ba.sum$lai.i[c(1,4,5)]),max(ring.yr.df.ba.sum$lai.i[c(1,4,5)]), 100, 2000)
abline(h =  mean(ring.yr.df.ba.sum$gpp.e[ring.yr.df.ba.sum$Ring %in% paste0('R',c(1,4,5))]),
       lty="solid",col='red')

points(.4,mean(c(1441,1558)),pch='%',xlim=c())
points(.395,mean(c(1441,1558)),pch='7',xlim=c())
arrows(.39, 1441,
       .39, 1558,
       length=0.11, angle=30, code=3 ,col="black"
)

points(.35,mean(c(1409,1586)),pch='%',xlim=c())
points(.343,mean(c(1409,1586)),pch='1',xlim=c())
points(.345,mean(c(1409,1586)),pch='3',xlim=c())
arrows(.34, 1409,
       .34, 1586,
       length=0.11, angle=30, code=3 ,col="black"
)



# accordingro belinda's suggestion


# palette(c("coral","navy","cadetblue3",
#           "brown2","brown4","deepskyblue"
# ))

par(mar=c(5,5,1,1))
palette(c('red','blue'))
symbol.vec <- c(3,15,15,3,3,15)

plot(gpp.a~lai.i,data = ring.yr.df.ba.sum,
     col='blue',pch=symbol.vec[Ring],
     # xlim=c(1.05,1.3),
     ylim=c(1200,2200),
     xlab=expression(LAI[i]~(m^2~m^-2)),
     ylab=expression(GPP~(g~C~m^-2~yr^-1)))

symbol.vec <- c(15,3,3,15,15,3)
points(gpp.e~lai.i,data = ring.yr.df.ba.sum,col='red',pch=symbol.vec[Ring])
legend('bottomright',legend = c('Field','Modelled','E','A'),
       pch=c(15,3,16,16),col=c('black','black','red','blue'),bty='n',ncol = 2)


# legend("top",pch=16,legend = paste0('R',1:6),col=palette(),bty='n',horiz = TRUE)


gpp.mean.a <- mean(ring.yr.df.ba.sum$gpp.a)
gpp.mean.e <- mean(ring.yr.df.ba.sum$gpp.e)
gpp.mean.a.a <- mean(ring.yr.df.ba.sum$gpp.a[ring.yr.df.ba.sum$Ring %in% paste0('R',c(2,3,6))])
gpp.mean.e.e <- mean(ring.yr.df.ba.sum$gpp.e[ring.yr.df.ba.sum$Ring %in% paste0('R',c(1,4,5))])


abline(h = gpp.mean.a,lty="dashed",col='lightskyblue')
abline(h = gpp.mean.e ,lty="dashed",col='coral')
clip(min(ring.yr.df.ba.sum$lai.i[c(2,3,6)]),max(ring.yr.df.ba.sum$lai.i[c(2,3,6)]), 100, 2000)
abline(h = gpp.mean.a.a ,lty="solid",col='blue')
clip(min(ring.yr.df.ba.sum$lai.i[c(1,4,5)]),max(ring.yr.df.ba.sum$lai.i[c(1,4,5)]), 100, 2000)
abline(h = gpp.mean.e.e  ,lty="solid",col='red')

points(.4,mean(c(gpp.mean.a.a,gpp.mean.e.e)),pch='%',xlim=c())
points(.395,mean(c(gpp.mean.a.a,gpp.mean.e.e)),pch='9',xlim=c())
arrows(.39, gpp.mean.a.a,
       .39, gpp.mean.e.e,
       length=0.11, angle=30, code=3 ,col="black"
)

points(.35,mean(c(gpp.mean.a,gpp.mean.e)),pch='%',xlim=c())
points(.343,mean(c(gpp.mean.a,gpp.mean.e)),pch='1',xlim=c())
points(.345,mean(c(gpp.mean.a,gpp.mean.e)),pch='3',xlim=c())
arrows(.34, gpp.mean.a,
       .34, gpp.mean.e,
       length=0.11, angle=30, code=3 ,col="black"
)



