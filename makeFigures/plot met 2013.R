
par(mfrow=c(2,2),oma=c(5,1,1,1))
par(mar=c(0,5,0,0))


# hist(maespa.r1.day$VPD.max,freq = F)

# a
# polygon(c(0,0,400,400),c(0,100,100,0),col="grey",border=NA)
plot(VPD.max~Date,data = maespa.r1.day,type="s",xaxt='n',xlab='',yaxt='n',ylab='',ylim=c(0,7),col="grey90")
lines(VPD.min~Date,data = maespa.r1.day,type="s",xaxt='n',xlab='',yaxt='n',ylab='',col="grey90")
lines(VPD.mean~Date,data = maespa.r1.day,type="s",xaxt='n',xlab='',yaxt='n',ylab='')
axis(2,at = seq(1,9,2),label=paste0(seq(1,9,2)))
mtext(expression(D~(kPa)),side = 2,line = 3)
legend("topleft",legend = "(a)",bty='n')
# abline(v = 365,lty="dashed",lwd=2,col="grey")
# abline(v = 365+366,lty="dashed",lwd=2,col="grey")
# b
par(mar=c(0,0,0,5))
plot(TAIR.max~Date,data = maespa.r1.day,type="s",xaxt='n',xlab='',yaxt='n',ylab='',ylim=c(5,45),col="grey90")
lines(TAIR.min~Date,data = maespa.r1.day,type="s",xaxt='n',xlab='',yaxt='n',ylab='',col="grey90")
lines(TAIR.mean~Date,data = maespa.r1.day,type="s",xaxt='n',xlab='',yaxt='n',ylab='')
mtext(expression(T[air]~(degree*C)),side = 4,line = 3)
axis(4,at = seq(5,45,10),label=paste0(seq(5,45,10)))
legend("topleft",legend = "(b)",bty='n')
# abline(v = 365,lty="dashed",lwd=2,col="grey")
# abline(v = 365+366,lty="dashed",lwd=2,col="grey")
# c
par(mar=c(0,5,0,0))
plot((PAR.sum)~Date,data = maespa.r1.day,type="s",xaxt='n',xlab='',yaxt='n',ylab='')
mtext(expression(PAR~(MJ~m^-2~d^-1)),side = 2,line = 3)
axis(2,at = seq(0,15,5),label=paste0(seq(0,15,5)))
legend("topleft",legend = "(c)",bty='n')
# axis(1,at = seq(30,4*365,30),label=levels(met.df.r1.sum$month)[seq(2,length(levels(met.df.r1.sum$month))-1,4)])
axis(1,at = seq(as.Date('2013-01-01'),
                as.Date('2013-12-01'),by='mon'),
     label=rep(c("J","F","M","A","M","J","J",'A',"S","O","N","D")))
mtext("2013",side=1,line=2.5,adj=0.5)
# mtext("2014",side=1,line=2.5,adj=0.25)
# mtext("2015",side=1,line=2.5,adj=0.5)
# mtext("2016",side=1,line=2.5,adj=0.75)
# abline(v = 365,lty="dashed",lwd=2,col="grey")
# abline(v = 365+366,lty="dashed",lwd=2,col="grey")
# d
par(mar=c(0,0,0,5))
plot(met.df.r1.sum$PPT,type="s",xaxt='n',xlab='',yaxt='n',ylab='')
mtext(expression(Precipitation~(mm~mon^-1)),side = 4,line = 3)
axis(4,at = seq(0,320,100),label=paste0(seq(0,320,100)))
legend("topleft",legend = "(d)",bty='n')
# my.lab=levels(met.df.r1.sum$month)[seq(2,length(levels(met.df.r1.sum$month))-1,4)]
# axis(1,at = seq(2,12*4,4),label=my.lab)
axis(1,at = seq(1,12*4,1),label=rep(c("J","F","M","A","M","J","J",'A',"S","O","N","D"),4))
mtext("2013",side=1,line=2.5,adj=0.5)
# mtext("2014",side=1,line=2.5,adj=0.25)
# mtext("2015",side=1,line=2.5,adj=0.5)
# mtext("2016",side=1,line=2.5,adj=0.75)
# abline(v = 13,lty="dashed",lwd=2,col="grey")
# abline(v = 13+12,lty="dashed",lwd=2,col="grey")

