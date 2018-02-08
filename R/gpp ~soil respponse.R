data.all.war$ppfd <- (data.all.war$PAR * 4.57 / 10^-6 / 3600)

hr.df.sub <- data.all.war[data.all.war$ppfd > 1000,]
hr.df.sub$swc.level <- cut(hr.df.sub$swc.30,3)
  

r2b.func <- colorRampPalette(c("red","blue"))
plot(Photo~VPD,
     data = hr.df.sub,
     col = r2b.func(3)[hr.df.sub$swc.level])
levels(hr.df.sub$swc.level)

# 
library(doBy)
data.sum <- summaryBy(Photo~HOUR,
                      data = data.all.war[month(data.all.war$DateTime) %in% c(12,1,2),],
                      FUN=c(mean),
                      keep.names=T,
                      na.rm = T)

data.sum.other <- summaryBy(Photo~HOUR,
                            data = data.all.war[!(month(data.all.war$DateTime) %in% c(12,1,2)),],
                            FUN=c(mean),
                            keep.names=T,
                            na.rm = T)

plot(Photo~HOUR,
     data = data.all.war[month(data.all.war$DateTime) %in% c(12,1,2),])


plot(Photo~HOUR,
     data = data.sum,
     type="l")

plot(Photo~HOUR,
     data = data.sum.other,
     type="l")
