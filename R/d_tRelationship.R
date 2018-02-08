# range(data.all.war$RH)
# data.all.war$TAIR
# data.all.war$VPD
# nls(VPD ~ a*TAIR^b,start= list(a=0.000605,b=2.39),data = data.all.war[complete.cases(data.all.war)])
# summary(nls(VpdL ~ a*Tleaf^b,start= list(a=0.000605,b=2.39),data = spot.amb))

data.all.war <- readRDS("output/maespa/all.hr.rds")

data.all.war$RH_level <- cut(data.all.war$RH,
                         breaks = seq(0,100,20)/100,
                         labels = paste0("<",seq(20,100,20),"%"))
par(mfrow=c(1,1),
    mar=c(5,5,5,5))
palette(c("red","orange","darkseagreen","lightskyblue","navy"))
vpd.t.func <- function(t,a = 0.0001175,b =2.9696469) a*t^b
plot(VPD~TAIR,data = data.all.war,
     pch=16,
     col=data.all.war$RH_level,
     xlim=c(10,40),
     ylim=c(0,8),
     cex=0.2)
legend("topleft",
       legend = levels(data.all.war$RH_level),
       col = palette(),
       bty='n',
       pch=16,
       title = "RH %")
par(new=TRUE)
curve(vpd.t.func,from =10, to = 50,
      xlim=c(10,40),
      ylim=c(0,8),
      ann=F,axes=F,
      lwd=2,
      col="grey80")
vpd.t.func.spots <- function(t,
                             # a = 2.994e-05,b =3.334
                             a = 0.000605, b= 2.39) a*t^b
par(new=TRUE)
curve(vpd.t.func.spots,from =10, to = 50,
      xlim=c(10,40),
      ylim=c(0,8),
      ann=F,axes=F,
      lwd=1,
      col="grey80",
      lty="dashed")

legend("top",
       legend = c("ROS","AFM 2014"),
       lty=c("solid","dashed"),
       col="grey80",
       bty='n')


