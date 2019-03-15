see <- readRDS("output/maestra/mastra and sap hr.rds")
see.day <- readRDS("output/maestra/mastra and sap.rds")

map.val <- mean(see.day$PPT[see.day$Ring == "R1"]) * 365.25 * 2

see.r1 <- see[see$Ring == "R1",] 
see.r1$rh.level <- cut(see.r1$RH,seq(0,1,0.2),labels = paste0("<",seq(0.2,1,0.2)*100,"%"))
see.r1.day <- see.r1[see.r1$time %in% seq(9,15),]


pdf("vpd2t.pdf",width = 6,height = 4)
par(mar=c(5,5,5,5))
palette(rev(c("navy","lightskyblue","darkseagreen","coral","red")))
plot(VPD~TAIR,data=see.r1.day,col=rh.level,pch=16,cex=0.2,
     xlab=expression("T"[air]~(degree*C)),ylab = expression(D~(kPa)))
legend("topleft",legend = levels(see.r1.day$rh.level),pch=16,col=palette(),title = "RH",bty='n')

fit.ros <- nls(VPD~a*TAIR^b, data=see.r1.day, start = list(a = 0.0006, b = 2.4))

pred.df <- data.frame(vpd=coef(fit.ros)[[1]] * seq(10,60)^coef(fit.ros)[[2]],
                      tair = seq(10,60))

points(vpd~tair,data = pred.df,type="l",lwd=2)

amf.df <- data.frame(vpd=0.000605 * seq(10,60)^2.39,
                      tair = seq(10,60))

points(vpd~tair,data =amf.df,type="l",lwd=2,lty="dashed",col="grey")
legend("top",legend = c("Observed","Duursma et al. 2014"),col=c("black","grey"),lty=c("solid","dashed"),bty='n')
dev.off()