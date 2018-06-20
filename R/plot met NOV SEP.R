day.df <- hr.df.all[as.Date(hr.df.all$DateTime) %in% as.Date(c("2013-09-11","2013-11-07"))]

day.df$day <- NA
day.df$day[as.Date(day.df$DateTime) == as.Date(c("2013-11-08"))] <- "Nov"
day.df$day[as.Date(day.df$DateTime) == as.Date(c("2013-09-11"))] <- "Sep"  
day.df$day <- as.factor(day.df$day)

pdf("met.NOV.SPE.pdf",width = 6,height = 6)
par(mfrow=c(2,2),mar=c(5,5,1,1))
palette(c("red","blue"))
plot(RH~time,data = day.df,col=day,pch=16)
legend("topright",legend = levels(day.df$day),col=palette(),pch=16)

plot(PAR~time,data = day.df,col=day,pch=16)

plot(VPD~time,data = day.df,col=day,pch=16)

plot(TAIR~time,data = day.df,col=day,pch=16)

# plot(sap~time,data = day.df,col=day,pch=16)
dev.off()