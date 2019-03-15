source('r/load.r')
# read data####
# lai.base <- c(0.777,0.788,0.697,0.736,1.007,0.641)
lai.base <- rep(0.8,6)
for (i in 1:6){
  sm[[i]]$LA <-  (sm[[i]]$LAIsmooth - lai.base[i])
  # sm[[i]] <- sm[[i]][sm[[i]]$Date<= as.Date('2013-12-31') &
  #                      sm[[i]]$Date >= as.Date('2013-01-01'),]
}
# # 
# swc.neutron.df <- read.csv("data/FACE_P0018_RA_NEUTRON_20120430-20180215_L1.csv")
# swc.neutron.df <- swc.neutron.df[swc.neutron.df$Location != "Outside",]
# swc.neutron.df$Date <- as.Date(as.character(swc.neutron.df$Date))
# swc.neutron.df$Ring <- as.character(swc.neutron.df$Ring)
# swc.neutron.ring.df <- doBy::summaryBy(VWC ~ Date + Depth + Ring,
#                                        data = swc.neutron.df,FUN=mean,na.rm=TRUE,
#                                        keep.names = TRUE,id=~CO2)
# 
# swc.ls <- split(swc.neutron.ring.df,swc.neutron.ring.df$Ring)
# 
# for(i in seq_len(length(swc.ls))){
#   swc.ls[[i]]$layer.thick <- c(25,diff(swc.ls[[i]]$Depth))
#   swc.ls[[i]]$layer.thick[swc.ls[[i]]$layer.thick<0] <- 25
# }
# swc.ring.df <- do.call(rbind,swc.ls)
# swc.ring.df <- swc.ring.df[swc.ring.df$Depth<51,]
# swc.ring.df$weight.swc <- swc.ring.df$VWC * swc.ring.df$layer.thick / 50
# 
# swc.neutron.mean.df <- doBy::summaryBy(weight.swc ~ Date + Ring,
#                                        data = swc.ring.df,FUN=sum,na.rm=TRUE,
#                                        keep.names = TRUE)
# 
# swc.neutron.mean.df <- swc.neutron.mean.df[swc.neutron.mean.df$Date <= as.Date('2016-12-31') &
#                                              swc.neutron.mean.df$Date >= as.Date('2013-01-01'),]

swc.neutron.mean.df <- readRDS('cache/swc.rds')
                                                  
swc.neutron.mean.df <- swc.neutron.mean.df[swc.neutron.mean.df$Date <= as.Date('2016-12-31') &
                                                  swc.neutron.mean.df$Date >= as.Date('2013-01-01'),]

swc.neutron.mean.df <- swc.neutron.mean.df[!is.na(swc.neutron.mean.df$vwc.neu),]
# par(mfrow=c(2,1))
# # 
# plot(LAIsmooth~Date,data = sm[[1]],col=palette()[1],type='l',ylim=c(1,2.5),ylab="LAI",xlab="")
# for (i in 2:6){
#   points(LAIsmooth~Date,data = sm[[i]],col=palette()[i],type='l')
# }
# legend("top",pch=16,legend = paste0('R',1:6),col=palette(),bty='n',horiz = TRUE)

# 
par(mfrow=c(2,1))
palette(c("brown1","cadetblue1","cadetblue3",
          "brown2","brown4","deepskyblue"
))
# plot VWC
par(mar=c(1,5,1,1))

plot(vwc.neu~Date,
     data = swc.neutron.mean.df[swc.neutron.mean.df$Ring =='R1',],
     col=palette()[1],type='l',ylim=c(0,70),ylab=expression(theta~('%')),xaxt='n')
for (i in 2:6){
  
  points(vwc.neu~Date,
         data = swc.neutron.mean.df[swc.neutron.mean.df$Ring == paste0('R',i),],
         col=palette()[i],type='l')
}
legend('topleft',legend = '(a)',bty='n')
# plot lai
par(mar=c(2,5,0,1))
plot(LA~Date,data = sm[[1]],col=palette()[1],type='l',
     ylim=c(0.3,1.8),ylab=expression("LAI"~(m^2~m^-2)),
     xlab="")
for (i in 2:6){
  points(LA~Date,data = sm[[i]],col=palette()[i],type='l')
}
legend("top",pch=16,legend = paste0('R',1:6),col=palette(),bty='n',horiz = TRUE)
legend('topleft',legend = '(b)',bty='n')





# plot 2013

# # 
# par(mfrow=c(2,1))
# palette(c("brown1","cadetblue1","cadetblue3",
#           "brown2","brown4","deepskyblue"
# ))
# # plot lai
# par(mar=c(1,5,1,1))
# plot(LA~Date,data = sm[[1]],col=palette()[1],type='l',
#      ylim=c(0.3,1.8),
#      xlim=c(as.Date('2013-1-1'),as.Date('2013-12-31')),
#      ylab=expression("LAI"~(m^2~m^-2)),
#      xlab="",
#      xaxt='n')
# for (i in 2:6){
#   points(LA~Date,data = sm[[i]],col=palette()[i],type='l')
# }
# legend("top",pch=16,legend = paste0('R',1:6),col=palette(),bty='n',horiz = TRUE)
# legend('topleft',legend = '(a)',bty='n')
# # plot VWC
# par(mar=c(2,5,0,1))
# plot(vwc.neu~Date,
#      xlim=c(as.Date('2013-1-1'),as.Date('2013-12-31')),
#      data = swc.neutron.mean.df[swc.neutron.mean.df$Ring =='R1',],
#      col=palette()[1],type='l',ylim=c(0,70),ylab=expression(theta~('%')),xlab="")
# for (i in 2:6){
#   
#   points(vwc.neu~Date,
#          data = swc.neutron.mean.df[swc.neutron.mean.df$Ring == paste0('R',i),],
#          col=palette()[i],type='l')
# }
# legend('topleft',legend = '(b)',bty='n')
# 
# par(mfrow=c(2,1))
# palette(c("brown1","cadetblue1","cadetblue3",
#           "brown2","brown4","deepskyblue"
# ))
# plot VWC
par(mar=c(1,5,1,1))

plot(vwc.neu~Date,
     data = swc.neutron.mean.df[swc.neutron.mean.df$Ring =='R1',],
     xlim=c(as.Date('2013-1-1'),as.Date('2013-12-31')),
     col=palette()[1],type='l',ylim=c(0,70),ylab=expression(theta~('%')),xaxt='n')
for (i in 2:6){
  
  points(vwc.neu~Date,
         data = swc.neutron.mean.df[swc.neutron.mean.df$Ring == paste0('R',i),],
         col=palette()[i],type='l')
}
legend('topleft',legend = '(a)',bty='n')
# plot lai
par(mar=c(2,5,0,1))
plot(LA~Date,data = sm[[1]],col=palette()[1],type='l',
     ylim=c(0.3,1.8),ylab=expression("LAI"~(m^2~m^-2)),
     xlim=c(as.Date('2013-1-1'),as.Date('2013-12-31')),
     xlab="")
for (i in 2:6){
  points(LA~Date,data = sm[[i]],col=palette()[i],type='l')
}
legend("top",pch=16,legend = paste0('R',1:6),col=palette(),bty='n',horiz = TRUE)
legend('topleft',legend = '(b)',bty='n')