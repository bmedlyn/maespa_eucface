if(!dir.exists("figures"))dir.create("figures")
for (i in 1:6){
  if(!dir.exists(sprintf("Rings/ring%s",i)))dir.create(sprintf("Rings/ring%s",i))
}

library(Maeswrap)

# choose ring number to plot
plot.stand.func <- function(ring.nm){
  roo.dr <- getwd()
  on.exit(setwd(roo.dr))
  
  setwd(sprintf("Rings/Ring%s/runfolder",ring.nm))
  library(Maeswrap)
  Plotstand()
}

# ring <-  4

for (ring in 1:6){
  par3d(windowRect = c(1000,1000,2000,2000))
  plot.stand.func(ring)
  # rgl.viewpoint(theta = 90, phi = 30)
  rgl.snapshot(sprintf("figures/newring%s.png",ring))

  rgl.close() 
}


# update.tree.f(lai.test = test,lai.base = base)
# plot.stand.func(2)
