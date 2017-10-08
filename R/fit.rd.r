rdark.csv <- read.csv("download/euc data/FACE_P0064_RA_GASEXCHANGE-RdarkT_20160215-L1.csv")

rdark.ls <- split(rdark.csv,rdark.csv$Tree)

# Rd(T)=RD⋅exp(Q10F(TAIR–RTEMP))
fit.rd.func <- function(df){
  nls(Photo~rd.25*exp(q10*(Tleaf - 25)),
      data = df,
      start=list(rd.25=-1,q10=0.05))
}


fit.all <- fit.rd.func(rdark.csv)

fit.ls <- lapply(rdark.ls,fit.rd.func)

coef.ls <- lapply(fit.ls,coef)
coef.ls <- lapply(coef.ls,function(df){t(as.data.frame(df))})


rd.par.df <- do.call(rbind,coef.ls)
row.names(rd.par.df) <- NULL
rd.par.df <- as.data.frame(rd.par.df)
rd.par.df$tree <- names(rdark.ls)


rd.par.df$ring <- substr(rd.par.df$tree,2,2)
library(doBy)

rd.sum.df <- summaryBy(rd.25 + q10 ~ ring,
                       data = rd.par.df,
                       FUN = mean,na.rm=TRUE,
                       keep.names = TRUE)
write.csv(rd.par.df,"rd25_q10_each_tree.csv")




# light response curve fit
photo.par.csv$PARi
photo.par.csv <- read.csv("download/euc data/FACE_P0069_RA_GASEXCHANGE-LRC_20141009-20160516-L1.csv")

fit.lrc.func <- function(df){
  nls(Photo~rd.25*exp(q10*(Tleaf - 25)),
      data = df,
      start=list(rd.25=-1,q10=0.05))
  
  nls(Photo~rd.25*exp(q10*(Tleaf - 25)),
      data = df,
      start=list(rd.25=-1,q10=0.05))
}