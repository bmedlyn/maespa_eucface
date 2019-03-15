source('r/load.r')
source('r/get aci response.r')
# gpp.response.df <- data.frame(facotrs = c('Leaf','Field','Ac','Aj',"Simu",'Acli','Net'),
#                               change= c(19,10,26,7.5,13,-6,7),
#                               numbers = 1:7)
# 
# plot(numbers~change,data = gpp.response.df,pch=16,
#      yaxt='n',xlab=expression(C[a]~response~('%')),
#      ylab="")
# axis(2,at = 1:7,labels = gpp.response.df$facotrs)
# abline(h=2.5)
# 
# 
# gpp.response.df <- data.frame(facotrs = c('Leaf','Field','Net','LAI','Acli',"Canopy",'Ac','Aj'),
#                               change= c(19,7.5,7,0,-6,14,26,10),
#                               numbers = 1:8)
# 
# plot(numbers~change,data = gpp.response.df,pch=16,
#      yaxt='n',xlab=expression(C[a]~response~('%')),
#      ylab="")
# axis(2,at = 1:8,labels = gpp.response.df$facotrs)
# abline(h=2.5)
# 
# barplot(gpp.response.df$change,horiz = T,
#         names.arg = gpp.response.df$facotrs)
# abline(v=0)

# 
# get leaf response
# spots <- read.csv("data/Gimeno_spot_Eter_gasExchange6400.csv")
# spots <- spots[is.na(spots$Tree) != TRUE,]
# spots$Date <- as.character(spots$Date)
# spots$Date[spots$Date == "31/11/2012"] <- "31/10/2012"
# spots$Date <- as.Date(as.character(spots$Date),"%d/%m/%Y")
# spots$Date[spots$Date > as.Date("2013-09-08") & spots$Date < as.Date("2013-09-15")] <- as.Date("2013-09-10")
# 
# spots <- spots[!spots$Campaign %in% c('2012Apr','2012May','2012Oct'),]
# spots$Campaign <- droplevels(spots$Campaign)
# 
# spot.ls <- split(spots,spots$Campaign)
# 
# each.yr.ls <- lapply(spot.ls,function(spots){mean(spots$Photo[spots$CO2_Treat2 == 'E']) / 
#     mean(spots$Photo[spots$CO2_Treat2 == 'A'])})
# 
# each.e.ls <- lapply(spot.ls,function(spots){mean(spots$Photo[spots$CO2_Treat2 == 'E'])})
# each.a.ls <- lapply(spot.ls,function(spots){mean(spots$Photo[spots$CO2_Treat2 == 'A'])})
#   
# fit.leaf <- lm((unlist(each.e.ls))~(unlist(each.a.ls)))
# see <- exp(confint(fit.leaf))
# 
# leaf.ci <- sd(unlist(each.yr.ls)) / 2 * 1.96*100
# leaf.response <- (mean((unlist(each.yr.ls)))-1)*100
# exp(0.8987)
# data from ellesworth 2018 14.5% and 24.0%
leaf.response <- 19
leaf.ci <- 4.5
# get LAI

facelai$LAI.obs <- facelai$LAI-0.8
lai.sum.df <- summaryBy(LAI.obs~treatment + Date,data = facelai,FUN = mean,na.rm=TRUE)
lai.sum.df.long <- reshape(lai.sum.df, idvar = "Date", timevar = "treatment", direction = "wide")
lai.sum.df.long$LAI.obs.mean.ambient
fit.lai <- lm(LAI.obs.mean.elevated~LAI.obs.mean.ambient+0,
              data = lai.sum.df.long)

lai.base <- rep(0.8,6)
for (i in 1:6){
  sm[[i]]$LA <-  (sm[[i]]$LAIsmooth - lai.base[i])
  sm[[i]]$ring <- i
}
lai.df <- do.call(rbind,sm)
lai.df$treat <- NA
lai.df$treat[lai.df$ring %in% c(1,4,5)] <- 'E' 
lai.df$treat[lai.df$ring %in% c(2,3,6)] <- 'A'
lai.df$year <- year(lai.df$Date)
lai.ls <- split(lai.df,lai.df$year)

lai.yr.ls <- lapply(lai.ls,function(spots){mean(spots$LA[spots$treat == 'E']) / 
    mean(spots$LA[spots$treat == 'A'])})

lai.e.ls <- lapply(lai.ls,function(spots){mean(spots$LA[spots$treat == 'E'])})
lai.a.ls <- lapply(lai.ls,function(spots){mean(spots$LA[spots$treat == 'A'])})

# fit.lai <- lm(log(unlist(lai.e.ls)+20)~log(unlist(lai.a.ls)+20)+0)

fit.lai <- lm((unlist(lai.e.ls))~(unlist(lai.a.ls))+0)
# confint(fit.lai,level = 0.9)
lai.mean <- ((as.vector(fit.lai$coefficients)-1) ) * 100

lai.ci <- (as.vector(confint(fit.lai,level = 0.9))-1) * 100

# lai.mean <- (mean(unlist(lai.yr.ls))-1)*100
# lai.ci <- sd(unlist(lai.yr.ls)) / sqrt(length(lai.yr.ls)) * 1.96 * 100

# get c
# maespa.df.a <- readRDS("output/ambient/mastra and sap.rds")
# maespa.df$year <- year(maespa.df$Date)
# 
# see.flx <- summaryBy(GPP + APAR~ year + Ring,data = maespa.df,FUN=sum,na.rm=T)
# see.lai <- summaryBy(LAI ~ year + Ring,data = maespa.df,FUN=mean)
# see <- merge(see.flx,see.lai)
# 
# see.ring.sum <- summaryBy(GPP.sum + APAR.sum ~ 
#                             year+Ring,data = see,FUN=mean)
# 
# see.ring.sum$c.treat <- NA
# see.ring.sum$c.treat[see.ring.sum$Ring %in% c(paste0("R",c(2,3,6)))] <- "A"
# see.ring.sum$c.treat[see.ring.sum$Ring %in% c(paste0("R",c(1,4,5)))] <- "E"
# 
# gpp.ls <- split(see.ring.sum,see.ring.sum$year)
# gpp.yr.ls <- lapply(gpp.ls,function(spots){mean(spots$GPP.sum.mean[spots$c.treat == 'E']) / 
#     mean(spots$GPP.sum.mean[spots$c.treat == 'A'])})
# 
# 
# gpp.e.ls <- lapply(gpp.ls,function(spots){mean(spots$GPP.sum.mean[spots$c.treat == 'E'])})
# gpp.a.ls <- lapply(gpp.ls,function(spots){mean(spots$GPP.sum.mean[spots$c.treat == 'A'])})

a.e.df <- read.csv('euc_gpp_2013_2016.csv')

gpp.direct <- format((sum(a.e.df$gpp.e)/sum(a.e.df$gpp.a)-1)*100,digits = 3)
gpp.a.a.vec <- a.e.df$gpp.a[a.e.df$Ring %in% paste0('R',c(2,3,6))]
gpp.e.e.vec <- a.e.df$gpp.e[a.e.df$Ring %in% paste0('R',c(1,4,5))]

fit.gpp <- lm(gpp.e.e.vec~gpp.a.a.vec+0, data = a.e.df)

gpp.mean <- ((as.vector(fit.gpp$coefficients))-1) * 100

# fit.gpp.log <- lm(log(unlist(gpp.e.ls))~log(unlist(gpp.a.ls))+0)

gpp.ci <- (as.vector(confint(fit.gpp,level = 0.9)) - 1) * 100

gpp.accli <- format((sum(a.e.df$gpp.accli[a.e.df$Ring %in% c(paste0("R",c(1,4,5)))]) / 
  sum(a.e.df$gpp.a[a.e.df$Ring %in% c(paste0("R",c(1,4,5)))])-1)*100,digits = 3)
  
  format(mean(a.e.df$accli.rate-1)*100,digits = 3)

# # 
# 
# fit.gpp <- lm((unlist(gpp.e.ls))~(unlist(gpp.a.ls))+0)
# 
# gpp.mean <- ((as.vector(fit.gpp$coefficients))-1) * 100
# 
# # fit.gpp.log <- lm(log(unlist(gpp.e.ls))~log(unlist(gpp.a.ls))+0)
# 
# gpp.ci <- (as.vector(confint(fit.gpp,level = 0.95)) - 1) * 100

# gpp.mean <- (mean(unlist(gpp.yr.ls))-1)*100
# gpp.ci <- sd(unlist(gpp.yr.ls)) / sqrt(length(gpp.yr.ls))* 1.96 * 100

# gpp.response.df <- data.frame(facotrs = c('Leaf','Field','Net','LAI','Acli',"Canopy",'Ac','Aj'),
#                               change= c(19,7.5,7,0,-6,14,26,10),
#                               numbers = 1:8)

pdf("c response by factor.pdf",width = 10,height = 6.2)
gpp.response.df <- data.frame(facotrs = c('Field','LAI','GPPccli','Leaf',"Canopy",     'AJ','Ac', 'Aaci'),
                              change=   c( 0,       0,  as.numeric(gpp.accli),  0,   as.numeric(gpp.direct),    10,  26,  0),
                              numbers = 1:8)

ylab.vec <- c(expression(GPP[field]),expression(LAI),expression(GPP[Accli]),expression(A[long]),
              expression(GPP[direct]),expression(A[J]),expression(A[c]),expression(A[inst]))
barplot(gpp.response.df$change,horiz = T,
        names.arg = NULL,
        xlim=c(-12,40),
        xlab="Response (%)")
abline(v=0)


gpp.response.df <- data.frame(facotrs = c('Field','LAI','Leaf',"Aaci"),
                              change=   c(gpp.mean,lai.mean,leaf.response,aci.c.rate*100),
                              numbers = c(1,2,4,9))

points(numbers~change,data = gpp.response.df,pch=16,
     yaxt='n',xlab=expression(C[a]~response~('%')),
     ylab="")

for (i in 1:8){
  legend(-15,i+0.2*i,legend = ylab.vec[i],bty='n',xpd=T)
}

# add ci
arrows(gpp.ci[1],1,gpp.ci[2],1, code=3, length=0.02, angle = 90)
arrows(lai.ci[1],2,lai.ci[2],2, code=3, length=0.02, angle = 90)
arrows(24,4,14.5,4, code=3, length=0.02, angle = 90)
arrows((aci.c.rate - aci.ci)*100,9,(aci.c.rate + aci.ci)*100,9, code=3, length=0.02, angle = 90)

# par(fig = c(13/35,1,0.8/7,4.2/7), new = T,cex=0.8)
# 
# inset.df <- data.frame(facotrs = c('Aobs','Aaci'),
#                        change=   c(19,aci.c.rate*100),
#                        numbers = c(1,2))
# 
# plot(numbers~change,data = inset.df,pch=16,
#        yaxt='n',xlab='',
#        ylab="",xlim=c(13,35),ylim=c(0.5,2.5),xaxt='n',xpd=T)
# axis(2,at=c(1,2),labels = c(expression(A[obs]),expression(A[ACi])))
# 
# arrows(24,1,14.5,1, code=3, length=0.02, angle = 90,xpd=T)
# arrows((aci.c.rate - aci.ci)*100,2,(aci.c.rate + aci.ci)*100,2, code=3, length=0.02, angle = 90)
dev.off()

