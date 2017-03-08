# a <- 1
# write(a)
# write.table(a,file="see.dat")

root.euc <- read.csv("EucFACE Fine root HIEv.csv",header = 1)
na.omit(root.euc)
root.euc <- root.euc[colSums(!is.na(root.euc)) > 0]

library(doBy)
root.sum <- summaryBy(Fine.Root.Biomass...g.m2.10.30.cm~ Ring.ID,
                     data=root.euc,FUN=mean,na.rm=TRUE)

top.30.accum <- root.sum$Fine.Root.Biomass...g.m2.10.30.cm.mean * 1.5

beta<- 0.99
f.r.30 <- (1-beta^30)/(1-beta^500)

root.total <- top.30.accum/f.r.30

# based on the function from jackson 1996, 
# the fraction of root in each layer
# root depth in cm
depth.v <- seq(0,520,40)
f.accu.v <- c()

for(i in 1:length(depth.v)){
  f.accu.v[i] <- (1-beta^depth.v[i])/(1-beta^max(depth.v))
}

f.vec <- diff(f.accu.v)

layers.num <- length(f.vec)
# f.vec<- c(0.096,0.166,0.376,0.362)