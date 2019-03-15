histo.a.df <- readRDS('output/ambient/histo.rds')
# histo.a.df <- histo.a.df[histo.a.df$m2.hr >0,]
histo.e.df <- readRDS('output/elevated/histo.rds')
histo.c.df <- readRDS('output/accli/histo.rds')

plot(aj~PAR,data = histo.a.df,type='l',col='navy')
points(ac~PAR,data = histo.a.df,type='l',col='coral')

barplot(histo.a.df$aj,col='red')

barplot(histo.a.df$ac,add = T)

sum(histo.a.df$aj)/
(sum(histo.a.df$ac) + sum(histo.a.df$aj))

sum(histo.e.df$aj)/
  (sum(histo.e.df$ac) + sum(histo.e.df$aj) )

sum(histo.c.df$aj)/
  sum(histo.c.df$ac)

sum(histo.a.df$aj) * 12 / (pi * 12.5^2) / 6 / 2
sum(histo.a.df$aj) * 12 / (pi * 12.5^2) * 1e-6 * 1800 / 6 / 4
sum(histo.a.df$ac) * 12 / (pi * 12.5^2) * 1e-6 * 1800 / 6 / 4
sum(histo.a.df$ac) / (pi * 12.5^2) * 12  / 6 / 2

histo.a.df$gpp.j <- histo.a.df$aj* 12 / (pi * 12.5^2) / 6 / 4 * 1e-6 * 1800 
histo.a.df$gpp.c <- histo.a.df$ac* 12 / (pi * 12.5^2) / 6 / 4 * 1e-6 * 1800 
tran.func <- function(cols,alpha=0.1){
  targe.vec <- col2rgb(cols)
  out.col <- apply(targe.vec,2,FUN = function(x){
    rgb(red = x["red"]/255, green=x["green"]/255, blue=x["blue"]/255,alpha=alpha)})
  return(out.col)
}

par(mar=c(5,5,1,1))
barplot(histo.a.df$gpp.j,col= tran.func('black',alpha=0.7),
        names.arg = NULL,border=F,
        xlab = expression(absorbed~PAR~(mu*mol~m^-2~s^-1)),
        ylab = expression(Annual~photosynthesis~(g~c~m^-2~yr^-1)))
axis(1,at = seq(1,75,10),labels = c(0,250,500,750,1000,1250,1500,1750))

barplot(histo.a.df$gpp.c,add = T,col= tran.func('red',alpha=0.6),border=F)
legend('topright',legend = c('RuBP',"Rubisco"),pch=15,
       col=c(tran.func('black',alpha=0.7),tran.func('red',alpha=0.6)),
       bty='n')

