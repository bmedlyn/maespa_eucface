# a <- 1
# write(a)
# write.table(a,file="see.dat")
root.depth <- 75 #cm
# value of 0.99 is used by remko;
# 0.96 is from Jakson 1996
beta <- 0.96

root.euc <- read.csv("EucFACE Fine root HIEv.csv",header = 1)
# na.omit(root.euc)
root.euc <- root.euc[colSums(!is.na(root.euc)) > 0]

library(doBy)
root.sum <- summaryBy(Fine.Root.Biomass...g.m2..0.30.cm ~ Ring.ID,
                     data=root.euc,FUN=mean,na.rm=TRUE)

# here juan suggest half the roots are from eucs. No idea what the actual proportion is. 
d.v <- c(0,10,30,root.depth)

f.v <- c()

for(i in 1:length(d.v)){
  f.v[i] <- (1-beta^d.v[i])/(1-beta^max(d.v))
}

f.d.vec <- diff(f.v)

# top.30.accum <- root.sum$Fine.Root.Biomass...g.m2.10.30.cm.mean * 0.5 / f.d.vec[2] * (f.d.vec[1] + f.d.vec[2])
top.30.accum <- root.sum$Fine.Root.Biomass...g.m2..0.30.cm.mean
# that's the default value from Jackson 1996 the model is very sensitive to this number

f.r.30 <- (1-beta^30)/(1-beta^root.depth)

root.total <- top.30.accum/f.r.30

# based on the function from jackson 1996, 
# the fraction of root in each layer
# root depth in cm

# depth.v <- c(0,50,seq(100,root.depth,100))
depth.v <- c(0,50,root.depth)
f.accu.v <- c()

for(i in 1:length(depth.v)){
  f.accu.v[i] <- (1-beta^depth.v[i])/(1-beta^max(depth.v))
}

f.vec <- diff(f.accu.v)

layers.num <- length(f.vec)
# f.vec<- c(0.096,0.166,0.376,0.362)
