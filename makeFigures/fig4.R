library(mgcv)

get.smooth.func <- function(Date,GPP.mean){
  num.day <- as.numeric(Date)
  GPP.mean <- zoo::na.locf(GPP.mean)
  fit.a.gam <- gam(GPP.mean~s(num.day,k=10))
  gpp.a.smooth <- predict(fit.a.gam)
  return(gpp.a.smooth)
}

tran.func <- function(cols,alpha=0.1){
  targe.vec <- col2rgb(cols)
  out.col <- apply(targe.vec,2,FUN = function(x){
    rgb(red = x["red"]/255, green=x["green"]/255, blue=x["blue"]/255,alpha=alpha)})
  return(out.col)
}

data.both.sap.m.150 <- readRDS("output/vj16/400/mastra and sap.rds")
data.both.sap.p.150 <- readRDS("output/vj16/550/mastra and sap.rds")

sum.a.df <- doBy::summaryBy(GPP + Trans ~ Date,data = data.both.sap.m.150,
                            FUN=c(mean,sd),id=~LAI)

sum.e.df <- doBy::summaryBy(GPP + Trans ~ Date,data = data.both.sap.p.150,
                            FUN=c(mean,sd),id=~LAI)

# make plots#################
par(mfcol=c(2,2))
par(mar=c(1,5,1,1))
# plot GPP
smooth.a.gpp <- get.smooth.func(sum.a.df$Date,sum.a.df$GPP.mean)
smooth.e.gpp <- get.smooth.func(sum.e.df$Date,sum.e.df$GPP.mean)
plot(smooth.a.gpp~sum.a.df$Date,type="l",col="lightskyblue",
     xlab='',ylab=expression(GPP~(g~C~m^-2~d^-1)),
     ylim=c(2,4.5),
     xaxt='n')
lines(smooth.e.gpp~sum.e.df$Date,col="coral")

polygon(x = c(sum.a.df$Date,
              rev(sum.a.df$Date)),
        y = c(c(smooth.a.gpp-sum.a.df$GPP.sd/4),
              rev(smooth.a.gpp+sum.a.df$GPP.sd/4)),
        col = tran.func("lightskyblue",alpha = 0.1),
        border = NA)

polygon(x = c(sum.e.df$Date,
              rev(sum.e.df$Date)),
        y = c(c(smooth.e.gpp-sum.e.df$GPP.sd/4),
              rev(smooth.e.gpp+sum.e.df$GPP.sd/4)),
        col = tran.func("coral",alpha = 0.1),
        border = NA)
legend("top",legend = c("550","400"),lty="solid",col=c("coral","lightskyblue"),bty='n')
# plot trans
par(mar=c(1,5,0,1))
smooth.a.trans <- get.smooth.func(sum.a.df$Date,sum.a.df$Trans.mean)
smooth.e.trans <- get.smooth.func(sum.e.df$Date,sum.e.df$Trans.mean)
plot(smooth.a.trans~sum.a.df$Date,type="l",col="lightskyblue",
     xlab='',ylab=expression(Transpiration~(kg~m^-2~d^-1)),
     ylim=c(0.3,1.5),
     xaxt='n')
lines(smooth.e.trans~sum.e.df$Date,col="coral")

polygon(x = c(sum.a.df$Date,
              rev(sum.a.df$Date)),
        y = c(c(smooth.a.trans-sum.a.df$Trans.sd/2),
              rev(smooth.a.trans+sum.a.df$Trans.sd/2)),
        col = tran.func("lightskyblue",alpha = 0.1),
        border = NA)

polygon(x = c(sum.e.df$Date,
              rev(sum.e.df$Date)),
        y = c(c(smooth.e.trans-sum.e.df$Trans.sd/2),
              rev(smooth.e.trans+sum.e.df$Trans.sd/2)),
        col = tran.func("coral",alpha = 0.1),
        border = NA)

# plot response of GPP
par(mar=c(2,5,0,1))
sum.e.df$gpp.inc <- sum.e.df$GPP.mean / sum.a.df$GPP.mean
sum.e.df$smooth.response <- get.smooth.func(sum.e.df$Date,sum.e.df$gpp.inc)
length(sum.e.df$smooth.response )
plot((sum.e.df$smooth.response-1)*100~sum.e.df$Date,
     xlab='',ylab = "GPP response %",ylim=c(5,20),
     type="l",col="grey",lwd=3)

sum.e.df$tran.dec <- sum.e.df$Trans.mean / sum.a.df$Trans.mean
sum.e.df$smooth.et.response <- get.smooth.func(sum.e.df$Date,sum.e.df$tran.dec)

plot((sum.e.df$smooth.et.response-1)*100~sum.e.df$Date,
     xlab='',ylab = "Trans response %",
     type="l",col="grey",lwd=3)
