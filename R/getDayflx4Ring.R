r2.df.a <- readdayflux("Rings/Ring2/runfolder/Dayflx.dat")
r2.df.a.sum <- summaryBy(totPs + totRf + absPAR + totLE1~DOY,
                                    data = r2.df.a,
                                    FUN = sum,na.rm = TRUE)
names(r2.df.a.sum) <- c("DOY","GPP","Ra","absPAR","le")
r2.df.a.sum$Date <- seq(as.Date("2013-01-01"),
                        as.Date("2014-01-01"),
                        by="day")

r2.df.a.sum$LE <- r2.df.a.sum$le/(pi*12.5^2)*1.8/100 #mm h2o m-2 ground d-1
r2.df.a.sum$GPP <- 12*r2.df.a.sum$GPP/(pi*12.5^2) #g C m-2 ground d-1
r2.df.a.sum$Ra <- 12*r2.df.a.sum$Ra/(pi*12.5^2) #g C m-2 ground d-1
r2.df.a.sum$absPAR<- r2.df.a.sum$absPAR/(pi*12.5^2) #MJ m-2 d-1
r2.df.a.sum$Date <- factor(format(r2.df.a.sum$Date,'%Y-%m'))
# r2.df.a.sum$mm <- r2.df.a.sum$LE* 1.8 * 0.01 #mm h2o d-1
# r2.df.a.sum date.full <- r2.df.a.sum$Date
r2.df.a.sum <- r2.df.a.sum[,c("Date","GPP","Ra","absPAR","LE")]
write.csv(r2.df.a.sum,"r2.amb.csv")

# #############
r2.df.e <- readdayflux("Rings/Ring2/runfolder/Dayflx.dat")
r2.df.e.sum <- summaryBy(totPs + totRf + absPAR + totLE1~DOY,
                         data = r2.df.e,
                         FUN = sum,na.rm = TRUE)
names(r2.df.e.sum) <- c("DOY","GPP","Ra","absPAR","le")
r2.df.e.sum$Date <- seq(as.Date("2013-01-01"),
                        as.Date("2014-01-01"),
                        by="day")

r2.df.e.sum$LE <- r2.df.e.sum$le/(pi*12.5^2)*1.8/100 #mm h2o m-2 ground d-1
r2.df.e.sum$GPP <- 12*r2.df.e.sum$GPP/(pi*12.5^2) #g C m-2 ground d-1
r2.df.e.sum$Ra <- 12*r2.df.e.sum$Ra/(pi*12.5^2) #g C m-2 ground d-1
r2.df.e.sum$absPAR<- r2.df.e.sum$absPAR/(pi*12.5^2) #MJ m-2 d-1
r2.df.e.sum$Date <- factor(format(r2.df.e.sum$Date,'%Y-%m'))
# r2.df.a.sum$mm <- r2.df.a.sum$LE* 1.8 * 0.01 #mm h2o d-1
# r2.df.a.sum date.full <- r2.df.a.sum$Date
r2.df.e.sum <- r2.df.e.sum[,c("Date","GPP","Ra","absPAR","LE")]
write.csv(r2.df.e.sum,"r2.ele.csv")

sum(r2.df.e.sum$LE)/sum(r2.df.a.sum$LE)
sum(r2.df.e.sum$GPP)/sum(r2.df.a.sum$GPP)
