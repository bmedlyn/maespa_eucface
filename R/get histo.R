# read the histo.dat

out.ls <- list()
for (ring.num in 1:6){
  # define the files
  fn <- sprintf('Rings/Ring%s/runfolder/histo.dat',ring.num)
  
  # get the number of trees and index
  con <- file(fn, open = "r")
  index.vec <- grep('TREE NO:',readLines(con))
  
  # read in the first 12 data points of the histo out put 
  # this will give hist from 300 to 2500 which is far enough
  r.t.hist.ls <- list()
  for (i in seq_along(index.vec)){
    tmp.df <- read.table(fn,header = FALSE, 
                         skip= c(2 + index.vec[i]),
                         nrow = c(12))
    
    names(tmp.df) <- c('PAR','m2.hr')
    
    tmp.df$Ring = ring.num
    tmp.df$tree <- i
    
    tmp.df$fraction <- tmp.df$m2.hr / sum(tmp.df$m2.hr)
    
    r.t.hist.ls[[i]] <- tmp.df
    
  }
  r.t.hist.df <- do.call(rbind,r.t.hist.ls)
  out.ls[[ring.num]] <- r.t.hist.df
}
# the out put hist data
histo.df <- do.call(rbind,out.ls)

library(doBy)
histo.sum <- summaryBy(m2.hr~PAR,data = histo.df,
                       FUN=sum,keep.names = TRUE)


histo.sum$fraction <- histo.sum$m2.hr / sum(histo.sum$m2.hr)

barplot(histo.sum$fraction,names.arg = histo.sum$PAR,
        ylab='Frequency',
        xlab= expression(PAR~(mu*mol~m^-2~s^-1)))


# 
# # i=1
# 
# 
# 
# 
# read.table(con,header = FALSE, 
#            skip = 10,
#            nrow = 1)
# 
# 
# 
# # while(grepl('TREE',temp.l)){
#   # Tree.nm <- gsub("[^[:digit:]]","",temp.l)
#   # col.nm <- readLines(con, 1)
#   tmp.df <- read.table(fn,header = FALSE, skip= 1 + index.vec[i],nrow = 12 + index.vec[i])
#   
#   names(tmp.df) <- c('PAR','fraction')
#   
#   r.t.df <- data.frame(ring = 1,
#                         tree.no = rep(temp.l,12)
#   )
#   
#   r.t.hist.ls[[length(r.t.hist.ls)+1]] <- cbind(r.t.df,tmp.df)
#   # # histo.df <- readLines(con,12)
#   # 
#   # # gsub("[[:digit:]]","",histo.df)
#   # histo.t.df <- na.omit(as.numeric(do.call(rbind,strsplit(histo.df,' '))))
#   # histo.df <-as.data.frame(histo.df)
#   # names(histo.df) <- strsplit(col.nm,':         ')[[1]]
# 
# # }
# 
# r.t.hist.df <- do.call(rbind,r.t.hist.ls)
# 
# 
# 
# 
# readLines(con,12)
# 
# readNameList('Rings/Ring1/runfolder/histo.dat','TREE NO:      1')
