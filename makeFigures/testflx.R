par.flux <- read.table("E:/test/testflx.dat",head=FALSE, skip=22)

temp <- readLines("E:/test/testflx.dat",n=21)
temp.vec <- strsplit(temp[21]," ")[[1]]

name.vec <- temp.vec[nchar(temp.vec) >=1]
names(par.flux) <- name.vec
par.flux <- par.flux[par.flux$SUNLA > 0,]
unique(par.flux$HR[par.flux$SUNLA ==0])

par(mfrow=c(2,2))
par(mar=c(5,5,2,5))
col.func <- colorRampPalette(c("lightskyblue","red","navy"))
palette(col.func(10))
plot(SUNLA~Z,data = par.flux,pch=16,col=HR)

legend_order <- matrix(1:10,ncol=5,byrow = TRUE)
legend("right",legend=unique(par.flux$HR),
       pch=16,
       bty="n", 
       col=palette(),
       ncol=2,
       xpd = TRUE,inset = -0.15,
       title = "Hour of day")

plot(SUNLA~HR,data = par.flux[par.flux$Z ==10,],pch=16,main="10m")
plot(SUNLA~HR,data = par.flux[par.flux$Z ==15,],pch=16,main="15m")
plot(SUNLA~HR,data = par.flux[par.flux$Z ==20,],pch=16,main="20m")
