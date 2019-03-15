euc.sum.df <- readRDS("cache/ecu_aci_sum.rds")
euc.sum.df$vj <- euc.sum.df$Jmax / euc.sum.df$Vcmax
euc.sum.df$Date <- as.Date(euc.sum.df$Date,'%d/%m/%y')
euc.sum.df <- euc.sum.df[year(euc.sum.df$Date) %in% c(2013:2016),]
palette(c("coral","navy","cadetblue3",
          "brown2","brown4","deepskyblue"
))
plot(vj~Date,data = euc.sum.df,pch=16,col=Ring,
     xlab='',ylab='JV ratio')
abline(h = mean(euc.sum.df$vj[euc.sum.df$Ring %in% c(1,4,5)]),lty ='dashed',lwd=3,col = 'red')
abline(h = mean(euc.sum.df$vj[euc.sum.df$Ring %in% c(2,3,6)]),lty ='dashed',lwd=3,col = 'navy')
legend("topleft",pch=16,legend = paste0('R',1:6),col=palette(),bty='n',horiz = TRUE)