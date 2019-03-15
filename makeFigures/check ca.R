see.df <- readdayflux(filename = "Rings/Ring2/runfolder/dayflx.dat")
see.hf <- readhrflux(filename = "Rings/Ring2/runfolder/hrflux.dat")

sum.day <- doBy::summaryBy(totPs~DOY,data = see.df, FUN=sum,na.rm=TRUE,keep.names = TRUE)
plot(totPs~DOY,data=sum.day)
sum.hr <- doBy::summaryBy(hrPs~DOY+HOUR,data = see.hf, FUN=sum,na.rm=TRUE,keep.names = TRUE)
plot(totPs~DOY,data=sum.day)


see.m.df <- readdayflux(filename = "Rings/Ring2/runfolder/dayflx.dat")
see.m.hf <- readhrflux(filename = "Rings/Ring2/runfolder/hrflux.dat")

sum.m.day <- doBy::summaryBy(totPs~DOY,data = see.m.df, FUN=sum,na.rm=TRUE,keep.names = TRUE)
plot(totPs~DOY,data=sum.m.day)
sum.m.hr <- doBy::summaryBy(hrPs~DOY+HOUR,data = see.m.hf, FUN=sum,na.rm=TRUE,keep.names = TRUE)


plot(totPs~DOY,data=sum.day)
points(totPs~DOY,data=sum.m.day,pch=16,col="red")


sum.day$totPs/sum.m.day$totPs

met.m.df <- readmet(filename = "Rings/Ring2/runfolder/met.dat")
met.df <- readmet(filename = "Rings/Ring2/runfolder/met.dat")
plot(met.df$CA[met.df$PAR>0])
mean(met.df$CA[met.df$PAR>0])
mean(met.m.df$CA[met.m.df$PAR>0])
