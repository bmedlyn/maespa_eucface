# read all hourly data
data.all.war <- readRDS("all.hr.rds")

# calculate the change in observed sw per hour
# change is mm per hour
data.all.war$change.5 <- c(0,diff(data.all.war$swc.5)) * 0.1 * 1 #0-10cm
data.all.war$change.30 <- c(0,diff(data.all.war$swc.30)) * 0.4 * 1 #10-50cm
data.all.war$change.75 <- c(0,diff(data.all.war$swc.75)) * 0.5 * 1 #50-100cm

data.all.war$change.tot <- (data.all.war$change.75 + data.all.war$change.30 + data.all.war$change.5) * 1000 #convert m to mm

# get the same for the top two layer in maespa
data.all.war$change.1 <- c(0,diff(data.all.war$swc.1)) * 0.5 * 1 #0-50cm
data.all.war$change.2 <- c(0,diff(data.all.war$swc.2)) * 0.5 * 1 #50-100cm

data.all.war$change.maes.tot <- (data.all.war$change.1 + data.all.war$change.2) * 1000 #convert m to mm

# sw change in rooted layers in maespa
data.all.war$change.sw <- c(0,diff(data.all.war$r.soil.w))

# sw change in all layers in maespa
data.all.war$change.sw.all <- c(0,diff(data.all.war$soil.w))

# get the residual for modelle and observed data
res.maes <- data.all.war$change.sw + data.all.war$et + data.all.war$soil.e

res.maes.all <- data.all.war$change.sw.all + data.all.war$et + data.all.war$soil.e

res.obs <- data.all.war$change.tot + data.all.war$sap

# get the residual for modelle and observed data
res.maes <- data.all.war$change.sw + data.all.war$et + data.all.war$soil.e - data.all.war$ppt + data.all.war$discharge

res.maes.all <- data.all.war$change.sw.all + data.all.war$et + data.all.war$soil.e- data.all.war$ppt + data.all.war$discharge

res.obs <- data.all.war$change.tot + data.all.war$sap - data.all.war$ppt #+ data.all.war$discharge

# put the residuals in df
data.all.war$res.obs <- res.obs

data.all.war$res.maes <- res.maes

data.all.war$res.maes.all <- res.maes.all

# make plot
with(data.all.war,plot(res.obs~DateTime,
                       ylim=c(-0.5,0.5),
                       pch=16,
                       cex=0.8,
                       col="coral",
                       xlab=" ",
                       ylab=" "
))

par(new=TRUE)
with(data.all.war,plot(res.maes.all~DateTime,
                       ylim=c(-0.5,0.5),
                       pch=16,
                       cex=0.8,
                       col="red",
                       xlab=" ",
                       ylab=" "
))

par(new=TRUE)
with(data.all.war,plot(res.maes~DateTime,
                       ylim=c(-0.5,0.5),
                       pch=16,
                       cex=0.8,
                       col="navy",
                       xlab="Date",
                       ylab="Residual (mm hr-1)"
))



legend("bottom",legend = c("MAESPA rooted layers","MAESPA all layers","Obsrvation"),pch=16,col=c("navy","red","coral"),horiz = TRUE)

#####################

watbal.day.df <- data.both.sap.war

watbal.day.df$change.sw <- c(0,diff(watbal.day.df$soil.w))

-watbal.day.df$change.sw - watbal.day.df$et - watbal.day.df$discharge - watbal.day.df$soil.e + watbal.day.df$tfall

plot(-watbal.day.df$change.sw - watbal.day.df$et - watbal.day.df$discharge - watbal.day.df$soil.e + watbal.day.df$tfall)
abline(h=0)
