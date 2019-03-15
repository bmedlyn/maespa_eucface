

plot.df <- met.df[met.df$Date >as.Date('2016-01-01') &
                    met.df$Date < as.Date('2016-02-11'), ]
plot.df <- doBy::summaryBy(PPT~Date,data = plot.df,
                           FUN=sum,na.rm=T,keep.names=T)

days.vec <- as.Date(c('2016-2-1','2016-2-2','2016-2-3','2016-2-8','2016-2-9'))

pdf('2016JAN rain.pdf',width = 10,height = 6.18)

plot(PPT~Date,type="s",data =plot.df,
     xlab='',ylab = 'PPT (mm/d)')
legend('topright',legend = 'Total in Jan 2016 = 303 mm')
points(x=days.vec,y=rep(2,5),pch=25,col='black')

plot((PAR.sum)~Date,data = maespa.r1.day[maespa.r1.day$Date >as.Date('2016-01-01') &
                                           maespa.r1.day$Date < as.Date('2016-02-11'),],
     type="s",ylab='')
mtext(expression(PAR~(MJ~m^-2~d^-1)),side = 2,line = 3)

points(x=days.vec,y=rep(2,5),pch=25,col='black')
dev.off()