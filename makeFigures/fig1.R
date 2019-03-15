
# get swc############
swc.neutron.df <- read.csv("data/FACE_P0018_RA_NEUTRON_20120430-20180215_L1.csv")
swc.neutron.df <- swc.neutron.df[lubridate::year(as.Date(swc.neutron.df$Date)) %in% seq(2013,2016),]
swc.neutron.df$Depth <- -swc.neutron.df$Depth
swc.neutron.df <- swc.neutron.df[swc.neutron.df$Location != "Outside",]
swc.neutron.df$Date <- as.Date(as.character(swc.neutron.df$Date))
swc.neutron.df$Ring <- as.character(swc.neutron.df$Ring)

swc.neutron.ring.df <- doBy::summaryBy(VWC ~ Date + Depth,
                                       data = swc.neutron.df,FUN=mean,na.rm=TRUE,
                                       keep.names = TRUE,id=~CO2)
depth.df <- data.frame(Date = rep(unique(swc.neutron.ring.df$Date),each=length(seq(-450,-25,1))),
                       Depth = rep(seq(-450,-25,1),length(unique(swc.neutron.ring.df$Date))))

swc.depth.df <- merge(depth.df,swc.neutron.ring.df,all=TRUE)
swc.depth.df$VWC <- zoo::na.approx(swc.depth.df$VWC)

days.df <- data.frame(Date = rep(seq(as.Date("2013-01-01"),as.Date("2016-12-31"),by="day"),each=length(seq(-450,-25,1))),
                       Depth = rep(seq(-450,-25,1),length(seq(as.Date("2013-01-01"),as.Date("2016-12-31"),by="day"))))
# swc.depth.df$Depth.rev <-  - swc.depth.df$Depth
swc.depth.df.all <- merge(days.df,swc.depth.df,all=TRUE)
swc.depth.df.all$Depth <- as.factor(swc.depth.df.all$Depth)
swc.depth.df.all$VWC[swc.depth.df.all$VWC < 0] <- 0
see.ls <- split(swc.depth.df.all,f=swc.depth.df.all$Depth)
for(i in 1:length(see.ls)){
  see.ls[[i]]$VWC <- zoo::na.spline(see.ls[[i]]$VWC)
}
swc.depth.df.all <- do.call(rbind,see.ls)

swc.depth.df.all$Depth <- as.numeric(as.character(swc.depth.df.all$Depth))

see <- matrix(swc.depth.df.all$VWC,
              ncol = length(unique(swc.depth.df.all$Date)),
              nrow = length(unique(swc.depth.df.all$Depth)),byrow = TRUE)

see[which(see<0)] <- 0

par(mfrow=c(2,1))
par(mar=c(0,5,1,5))
# # plot lai####
# data.both.sap <- readRDS("output/maestraVPD/mastra and sap.rds")
# with(data.both.sap,
#      plot(rep(-10,nrow(data.both.sap))~Date,
#           ylim=c(0,2),
#           ylab=expression(LAI~(m^2~m^-2)),
#           # ylab=" ",
#           type="l",
#           xlab=" ",
#           xaxt="n",
#           col=1
#      ))
# 
# 
# for (i in 1:6){
#   palette(c("brown1","cadetblue1","cadetblue3","brown2","brown4","deepskyblue"))
#   par(new=1)
#   rings <- sprintf("R%s",i)
#   with(data.both.sap[data.both.sap$Ring == rings,],
#        plot(LAI-0.8~Date,
#             ylim=c(0,2),
#             ylab=" ",
#             xlab=" ",
#             type="l",
#             col=palette()[i],
#             xaxt="n",
#             yaxt="n"
#        ))
# 
# }
# legend('top',legend = paste0("R",1:6),pch=15,col=palette(),
#        horiz = TRUE,bty='n')
# 
# # tran.func <- function(cols,alpha=0.1){
# #   targe.vec <- col2rgb(cols)
# #   out.col <- apply(targe.vec,2,FUN = function(x){
# #     rgb(red = x["red"]/255, green=x["green"]/255, blue=x["blue"]/255,alpha=0.1)})
# #   return(out.col)
# # }
# # lai.df <- Reduce(function(x, y) merge(x, y, all=TRUE), sm)
# # 
# # lai.df$C_treat = NA
# # 
# # lai.df$C_treat[lai.df$Ring %in% paste0("R",c(1,4,5))] = "E"
# # lai.df$C_treat[lai.df$Ring %in% paste0("R",c(2,3,6))] = "A"
# # lai.df <- lai.df[lubridate::year(lai.df$Date) %in% seq(2013,2016),]
# # # head(lai.df)
# # library(doBy)
# # rm(sd)
# # lai.sum <- summaryBy(LAIsmooth~Date + C_treat,
# #                      data = lai.df,
# #                      FUN=c(mean,sd),na.rm=TRUE)
# # stem.area = 0.8
# # plot(LAIsmooth.mean-stem.area~Date,data = lai.sum[lai.sum$C_treat == "E",],
# #      type='l',col="red",
# #      ylim=c(0.4,1.6),xaxt="n",
# #      ylab=expression(LAI~(m^2~m^-2)),xlab='')
# # polygon(x = c(lai.sum[lai.sum$C_treat == "E",]$Date,
# #               rev(lai.sum[lai.sum$C_treat == "E",]$Date)),
# #         y = c(c(lai.sum[lai.sum$C_treat == "E",]$LAIsmooth.mean-stem.area
# #                 -lai.sum[lai.sum$C_treat == "E",]$LAIsmooth.sd),
# #               rev(lai.sum[lai.sum$C_treat == "E",]$LAIsmooth.mean-stem.area
# #                   +lai.sum[lai.sum$C_treat == "E",]$LAIsmooth.sd)),
# #         col = tran.func("red"),
# #         border = NA)
# # points(LAIsmooth.mean-stem.area~Date,data = lai.sum[lai.sum$C_treat == "A",],
# #        type='l',col="blue")
# # 
# # polygon(x = c(lai.sum[lai.sum$C_treat == "A",]$Date,
# #               rev(lai.sum[lai.sum$C_treat == "A",]$Date)),
# #         y = c(c(lai.sum[lai.sum$C_treat == "A",]$LAIsmooth.mean-stem.area
# #                 -lai.sum[lai.sum$C_treat == "A",]$LAIsmooth.sd),
# #               rev(lai.sum[lai.sum$C_treat == "A",]$LAIsmooth.mean-stem.area
# #                   +lai.sum[lai.sum$C_treat == "A",]$LAIsmooth.sd)),
# #         col = tran.func("blue"),
# #         border = NA)
# # legend("topleft",legend = c("A",'E'),lty='solid',col=c("blue","red"),bty='n')

# read daily fluxes############
data.both.sap<- readRDS("output/maestraVPD/mastra and sap.rds")

# plot rainfall#####
par(mar=c(0,5,1,5))
with(data.both.sap[data.both.sap$Ring == "R4",],
     plot(PPT~Date,
          ylim=c(0,60),
          ylab=expression("Precipitation"~(mm~d^-1)),
          xlab=" ",
          type="l",
          col="grey90",
          xaxt="n"
     ))

# plot swc############
library(raster)
par(mar=c(2,5,1,5))
plot(1000,xlim=c(0,ncol(see)),ylim=c(-450,-25),xaxt='n',
     ylab=" Depth (m)",axes=FALSE)
axis(1,at=seq(1,ncol(see),30.5),labels = unique(format(unique(swc.depth.df.all$Date),"%b-%y")))
axis(2,at=pretty(-450:-25,5),labels = pretty(seq(-4.5,-0.25,0.01),5))
raster.col.fun <- colorRampPalette(c("coral","lightskyblue"))
brks <- pretty(0:35,5)
arg <- list(at=brks, labels=paste0((brks)))

plot(raster(see[nrow(see):1,], 
            xmn=0, xmx=ncol(see),
            ymn=-425, ymx=-25),
     asp = NA,
     breaks=brks,
     lab.breaks =  brks,
     col=c(raster.col.fun(length(brks) - 1)),
     box=FALSE,xaxt='n',
     axis.args = arg,
     ylab=" ",
     legend=T,
     add=TRUE)

