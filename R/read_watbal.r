
ring.sel = 4
fn <- sprintf("Rings/Ring%s/runfolder/watbal.dat",ring.sel)
watbal <- read.table(fn,head=FALSE, skip=37)

# note the et here is evapotranspiration not just tranpiration
names(watbal) <- c("day", "hour", "wsoil", "wsoilroot", "ppt", "canopystore",
                   "evapstore", "drainstore", "tfall", "et", "etmeas",
                   "discharge", "overflow",  "weightedswp", "ktot", "drythick", "soilevap",
                   "soilmoist", "fsoil", "qh", "qe", "qn", "qc", "rglobund",
                   "rglobabv", "radinterc", "rnet", "totlai", "tair", "soilt1", "soilt2",
                   "fracw1", "fracw2", "fracaPAR")

sum(watbal$ppt,na.rm=TRUE)
sum(watbal$et,na.rm=TRUE)
sum(watbal$discharge,na.rm=TRUE)/2 +
  sum(watbal$soilevap,na.rm=TRUE)


watbal$ppt[2333]
watbal$et[2333]
watbal$discharge[2333]
watbal$soilevap[2333]
watbal$wsoil[2333] - watbal$wsoil[2334] 
