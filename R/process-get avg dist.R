coord_H_D <- read.csv("dendrometers-n-coord2013_for_TEG.csv")
coord_H_D.1 <- coord_H_D[which(coord_H_D$Ring == 1),]

dist.df <- data.frame(coord_H_D.1$Coord1.N,coord_H_D.1$Coord2.E)

dis <- dist(dist.df,"euclidean")
# matrix(mean(dis),ncol = nrow(dist.df),byrow = FALSE)
dis.m <- as.matrix(dis)
dis.m[which(dis.m == 0)] <- 10000
shortest <- apply(dis.m[,1:25],1,min)
ave.dis <-mean(shortest)




# 
angel.t <- 2 * asin(1.25/15)
sin(angel.t) * 15
cos(angel.t) * 15

num <- floor(pi/angel.t/2)

c.n.q <- c()
c.e.q <- c()

ang <- 0
r.dist <- c(15,17.5)
cr.n.ls <- list()
cr.e.ls <- list()
for( radi in r.dist){
  c.n.q <- c()
  c.e.q <- c()
  ang <- 0
  for (i in 1:num){
    c.n.q[i] <-  cos(ang) * radi
    c.e.q[i] <-  sin(ang) * radi
    ang <- ang + angel.t
  }
  cr.n.ls[[length(cr.n.ls) + 1]] <- c(c.n.q,-c.n.q,-c.n.q,c.n.q)
  cr.e.ls[[length(cr.e.ls) + 1]] <- c(c.e.q,c.e.q,-c.e.q,-c.e.q)
}



coor.N <- c(cr.n.ls[[1]],cr.n.ls[[2]]) 
coor.E <- c(cr.e.ls[[1]],cr.e.ls[[2]]) 

add <- matrix(c(coor.E,coor.N),ncol = 2)

plot(add)


# 
surrd.tr.coor.func <- function(r.dist){
  angel.t <- 2 * asin(1.25/15)
  sin(angel.t) * 15
  cos(angel.t) * 15
  
  num <- floor(pi/angel.t/2)
  
  c.n.q <- c()
  c.e.q <- c()
  
  
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

#   
#   angel.t <- 2 * asin(1.25/15)
#   sin(angel.t) * 15
#   cos(angel.t) * 15
#   
#   num <- floor(pi/angel.t/2)
#   
#   c.n.q <- c()
#   c.e.q <- c()
#   
#   r.dist <- c(15,17.5)
#   cr.n.ls <- list()
#   cr.e.ls <- list()
#   for( radi in r.dist){
#     c.n.q <- c()
#     c.e.q <- c()
#     ang <- 0
#     for (i in 1:num){
#       c.n.q[i] <-  cos(ang) * radi
#       c.e.q[i] <-  sin(ang) * radi
#       ang <- ang + angel.t
#     }
#     cr.n.ls[[length(cr.n.ls) + 1]] <- c(c.n.q,-c.n.q,-c.n.q,c.n.q)
#     cr.e.ls[[length(cr.e.ls) + 1]] <- c(c.e.q,c.e.q,-c.e.q,-c.e.q)
#   }
#   
#   
#   
#   coor.N <- c(cr.n.ls[[1]],cr.n.ls[[2]]) 
#   coor.E <- c(cr.e.ls[[1]],cr.e.ls[[2]]) 
# #   
# #   cr.n.vec <- c(c.n.q,-c.n.q,-c.n.q,c.n.q,
# #                 c.n.q+1.5,-(c.n.q+1.5),-(c.n.q+1.5),c.n.q+1.5)
# #   cr.e.vec <- c(c.e.q,c.e.q,-c.e.q,-c.e.q,
# #                c.e.q+1.5,c.e.q+1.5,-(c.e.q+1.5),-(c.e.q+1.5))
# #   
# #     cr.n.vec <- c(c.n.q,-c.n.q,-c.n.q,c.n.q,
# #                   c.n.q+1.5,-(c.n.q+1.5),-(c.n.q+1.5),c.n.q+1.5)
# #     cr.e.vec <- c(c.e.q,c.e.q,-c.e.q,-c.e.q,
# #                  c.e.q+1.5,c.e.q+1.5,-(c.e.q+1.5),-(c.e.q+1.5))
# #   coor.N <- cr.n.vec + zero.position
# #   coor.E <- cr.e.vec + zero.position  

coor.N <- c(ls.15[[2]],ls.17[[2]])
coor.E <- c(ls.15[[1]],ls.17[[1]]) 

add <- matrix(c(coor.N,coor.E),ncol = 2)
