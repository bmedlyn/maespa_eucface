source('r/clean aci.r')



aci.amb.df <- euc.all.df[euc.all.df$Ring %in% c(2,3,6),]
aci.amb.df <- subset(aci.amb.df,Age!='lower')


aci.a.ls <- split(aci.amb.df,aci.amb.df$Number)
df=aci.a.ls[[1]]


get.a.e.aci.func <- function(df){
  a.vec <- mean(df$Photo[df$CO2R > 390 & df$CO2R < 450],na.rm=TRUE)
  e.vec <- df$Photo[df$CO2R > 500 & df$CO2R < 600]
  if(length(a.vec)<1){
    a.vec <- NA
  }
  if(length(e.vec)<1){
    e.vec <- NA
  }
  
  return(data.frame(photo.a = a.vec,
                    photo.e = e.vec))
  
}

photo.4.5.ls <- lapply(aci.a.ls,get.a.e.aci.func)
photo.4.5.df <- do.call(rbind,photo.4.5.ls)
photo.4.5.df <- photo.4.5.df[complete.cases(photo.4.5.df),]
aci.c.rate <- exp(mean(log(photo.4.5.df$photo.e / photo.4.5.df$photo.a)))-1
rate.sd <- exp(sd(log(photo.4.5.df$photo.e / photo.4.5.df$photo.a))) -1
rate.n <- length(photo.4.5.df$photo.e / photo.4.5.df$photo.a)
aci.ci <- 1.96 * rate.sd /sqrt(rate.n)


