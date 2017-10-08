update.tree.f <- function(lai.test,lai.base,radius = 12.5){
   #ring radius; m
  zero.position <- radius 

  # Update trees and confile with real coordinations and la
  #calculatation of crown distribution based on allometric relationship######
  #read data
  # need to be on hiev
  coord_H_D <- read.csv("dendrometers-n-coord2013_for_TEG.csv")
  coord_H_D <- na.omit(coord_H_D)
  
  #DBH################################################################
  coord_H_D$DBH <- coord_H_D$DIAM01.2013
  
  #Assume crown size is defined by leaf area. That is assuming a half spheric crown the surface of which ####
  #equals or proportional to LA.
  #The assumption will likely overestimate the crown size.
  #But that's what I got for now.
  #the acutrual LA and LAI is not given by this equation but comes from real measurments.
  #the ratio of how the real LAI is distributed to each individual tree however, 
  
  #is given by the ratio of LA realstionship here.
  #hypothetical LA is given by the allometric relationship from Limousin et al. 2009 and Rambal et al. 2004
  #hypothetical LA is used for following purposes:
  coord_H_D$LA <- 9.1*10^(-2)*coord_H_D$DBH^1.875
  
  #since the area of the half sphere = LA = 4*pi*radius^2
  # r is consider as both x and y direction radius
  coord_H_D$r <- (0.5*pi^-1*coord_H_D$LA)^0.5
  
  #the ratio of LA of each individual is given by ring
  #subset the data by ring
  
  #subset the data by ring
  coord_H_D_byRing <- lapply(c(1:6),function(x){
    
    y <- coord_H_D[which(coord_H_D$Ring == x),]
    return(y)
    
  })
  
  for (i in 1:6){
    
    coord_H_D_byRing[[i]]$ratio <- coord_H_D_byRing[[i]]$LA/sum(coord_H_D_byRing[[i]]$LA)
  }
  
  #alternatively we can use the ratio to divide the total ground area of the ring
  #again this will overestimate the radius as the canopy is not closed
  for (i in 1:6){  
    coord_H_D_byRing[[i]]$radius_byRatio <- radius * coord_H_D_byRing[[i]]$ratio^0.5
  }
  # The two values are not too different. I will use the r given by LA for now.
  
  #the crown height can also be ralated to r for now, CH = 2r,th =h-r
  for (i in 1:6){
    coord_H_D_byRing[[i]]$CH <- 0.5*coord_H_D_byRing[[i]]$Height072013
  }
  
  #TH
  for (i in 1:6){
    
    coord_H_D_byRing[[i]]$TH <- 0.5*coord_H_D_byRing[[i]]$Height072013
  }
  
  #the LAI is given by the product of actual LAI of the ring and ratio
  for (i in 1:6){
    sm[[i]]$LA <- (lai.test * sm[[i]]$LAIsmooth - lai.base)*pi*radius^2
    sm[[i]]$Date <- format(sm[[i]]$Date, "%d/%m/%y")
    
  }
  
  #LA of each tree
  #Real LA is atrributed to each tree based on the tree's allometric LA ratio
  #create the tree LA matrix based on data
  treela <- list()
  for (i in 1:6) {
    
    treela[[i]] <- matrix(0.01,nrow=length(coord_H_D_byRing[[i]]$ratio),
                          ncol=length(sm[[i]]$LA))
    
  }
  
  #distribute LA to each tree
  for (j in 1:6){
    
    for (i in 1:length(sm[[j]]$LA)) {
      
      treela[[j]][,i] <- coord_H_D_byRing[[j]]$ratio*sm[[j]]$LA[i]
      
    }
    treela[[j]] <- round(treela[[j]]*1000)/1000
  }
  
  totalDays <- length(sm[[1]]$Date)
  
  #be careful with the directory as the replacePAR fun need that to work 
  #convert the coordinate. From 0,0 in the centre to 0,0 in the corner
  coord_H_D$y <- coord_H_D$Coord1.N + zero.position
  coord_H_D$x <- coord_H_D$Coord2.E + zero.position
  

  
  Rxy <- list()
  Rcoord <- list()
  temp <- c()
  for (i in 1:6){
    
    Rcoord[[i]] <- c()
  }
  
  
  for (j in 1:6){
    
    Rxy[[j]] <- list(coord_H_D$x[coord_H_D$Ring == j],coord_H_D$y[coord_H_D$Ring == j])
    n <- length(Rxy[[j]][[1]])
    
    for( i in 1:n ){
      temp <- c(temp,Rxy[[j]][[1]][i],Rxy[[j]][[2]][i])
      Rcoord[[j]] <- temp 
    }
    temp <- c()
  }
  
  Coord <- list()
  
  for (j in 1:6){
    Coord[[j]] <- matrix(numeric(length(Rcoord[[j]])),ncol = 2)
    for (i in 1:(0.5*length(Rcoord[[j]]))){
      
      Coord[[j]][i,1] <- Rcoord[[j]][2*i-1]
      Coord[[j]][i,2] <- Rcoord[[j]][2*i]
      
    }
  }
 
#   #tree coord outside the ring

  surrd.tr.coor.func <- function(r.dist){
    angel.t <- 2 * asin(1.25/15)
    # sin(angel.t) * 15
    # cos(angel.t) * 15
    
    num <- ceiling(pi/angel.t/2)
    
    cr.n.ls <- list()
    cr.e.ls <- list()

      c.n.q <- c()
      c.e.q <- c()
      ang <- 0
      for (i in 1:num){
        c.n.q[i] <-  cos(ang) * r.dist
        c.e.q[i] <-  sin(ang) * r.dist
        ang <- ang + angel.t
      }
        cr.n.vec <- c(c.n.q,-c.n.q,-c.n.q,c.n.q)
        cr.e.vec <- c(c.e.q,c.e.q,-c.e.q,-c.e.q)
      ls <- list()
      ls[[1]] <- cr.n.vec
      ls[[2]] <- cr.e.vec
    return(ls)
  }
  
  ls.15 <- surrd.tr.coor.func(15)
  ls.17 <- surrd.tr.coor.func(17)
  ls.19 <- surrd.tr.coor.func(19)
  ls.21 <- surrd.tr.coor.func(21)

  
    coor.N <- c(ls.15[[2]],ls.17[[2]],ls.19[[2]],ls.21[[2]]) + zero.position
    coor.E <- c(ls.15[[1]],ls.17[[1]],ls.19[[1]],ls.21[[1]]) + zero.position  
    
  add <- matrix(c(coor.E,coor.N),ncol = 2)
  
  #replace old tree data
  options(scipen=999)
  for (i in 1:6){
    # assign confile####
    tf <- sprintf("Rings/Ring%s/runfolder/Trees.dat",i)
    replaceNameList(namelist = "xy", datfile = tf,
                    val = list(xycoords=rbind(Coord[[i]],add)))
  }

  srrounding.tree <- length(coor.N)
  
  #generate uniforn trees #####
  meanH <- mean(coord_H_D$Height072013)
  meanD <- mean(coord_H_D$DIAM01.2013)
  meanR <- mean((9.1*10^(-2)*coord_H_D$DBH^1.875)^0.5)
  meanLA<-c()
  for (i in 1: length(treela[[1]][1,])){meanLA[i]<- mean(treela[[1]][,i])}
  meanla <- matrix(rep(meanLA,srrounding.tree),ncol=srrounding.tree)
  meanCH <- 0.5*meanH
  meanTH <- meanH - meanCH 
  
  ##replace data in confi file####
  for (i in 1:6){
    # assign confile####
    cf <- sprintf("Rings/Ring%s/runfolder/confile.dat",i)
    replacePAR(cf, "itargets", "treescon", seq(1,length(treela[[i]][,1]),1))
    replacePAR(cf, "notrees", "treescon", length(coord_H_D_byRing[[i]]$ratio)+srrounding.tree)
    
    #update tree.dat#####
    #crown radius x
    tf <- sprintf("Rings/Ring%s/runfolder/Trees.dat",i)
    replaceNameList(namelist = "indivradx", datfile = tf,
                    val = list(nodates = 1,
                               values = matrix(c(coord_H_D_byRing[[i]]$r,rep(meanR,srrounding.tree)),ncol=1)))
    #crown radius y
    replaceNameList(namelist = "indivrady", datfile = tf,
                    val = list(nodates = 1,
                               values = matrix(c(coord_H_D_byRing[[i]]$r,rep(meanR,srrounding.tree)),ncol=1)))
    
    #DBH
    replaceNameList(namelist = "indivdiam", datfile = tf,
                    val = list(nodates = 1,
                               values = matrix(c(coord_H_D_byRing[[i]]$DBH*0.01,rep(meanD*0.01,srrounding.tree)),ncol=1)))
    
    
    #crown height
    replaceNameList(namelist = "indivhtcrown", datfile = tf,
                    val = list(nodates = 1,
                               values = matrix(c(coord_H_D_byRing[[i]]$CH,rep(meanCH,srrounding.tree)),ncol=1)))
    #trunk height
    replaceNameList(namelist = "indivhttrunk", datfile = tf,
                    val = list(nodates = 1,
                               values = matrix(c(coord_H_D_byRing[[i]]$TH,rep(meanTH,srrounding.tree)),ncol=1)))
    
    # the trunck hieght problems still there is ring 6
    tf6 <- sprintf("Rings/Ring%s/runfolder/Trees.dat",6)
    replaceNameList(namelist = "indivhttrunk", datfile = tf6,
                    val = list(nodates = 1,
                               values = matrix(c(rep(5,length(coord_H_D_byRing[[6]]$Tree)),
                                                 rep(meanTH,srrounding.tree)),ncol=1)))
    #LA
    replaceNameList("indivlarea", tf, 
                    vals=list(nodates = totalDays,
                              dates = sm[[i]]$Date,
                              values = rbind(treela[[i]],t(meanla))
                    ))
    #plot
    replaceNameList("plot", tf, 
                    vals=list(x0  =	0,
                              y0	=	0,
                              xmax	=	sqrt(pi*radius^2),
                              ymax	=	sqrt(pi*radius^2),
                              xslope	=	0,
                              yslope	=	0,
                              bearing	=	-90,
                              notrees	=	length(coord_H_D_byRing[[i]]$Tree)+srrounding.tree))
  }
  
  writeLines("trees updated")
  
  
}

