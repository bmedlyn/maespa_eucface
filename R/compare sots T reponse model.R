spots.df <- read.csv("Gimeno_spot_Eter_gasExchange6400.csv")
spots.df <- spots.df[spots.df$CO2R > 400 & spots.df$CO2R <450,]
spots.df$c.level <- cut(spots.df$CO2R,breaks = c(0,600))
euc.acis.df <- read.csv("data/P0020_EucFACE-Aci_MASTER.csv")
euc.spots.df <- euc.acis.df[euc.acis.df$CO2R > 400 & euc.acis.df$CO2R <450,]
euc.spots.df$c.level <- cut(euc.spots.df$CO2R,breaks = c(0,600))
t.response.df <- read.csv("data/E_teret_A-Ci_temperature_curves.csv")
t.spots.df <- t.response.df[t.response.df$CO2R >400 & t.response.df$CO2R <450,]
t.spots.df$c.level <- cut(t.spots.df$CO2R,breaks = c(0,600))


library(plantecophys)
modelled.df <- Photosyn(VPD = t.spots.df$VpdL,
                        Ca = t.spots.df$CO2R,
                        PPFD = t.spots.df$PARi,
                        Tleaf = t.spots.df$Tleaf,
                        gsmodel = "BBOpti",
                        
                        Jmax = 150, 
                        Vcmax = 100,
                        
                        EaV = 74189.7435218429, 
                        EdVC = 2e+05, 
                        delsC = 641.989, 
                        
                        EaJ = 39513,
                        EdVJ = 2e+05, 
                        delsJ = 640.2658)

modelled.df$c.level <- cut(modelled.df$Ca,breaks = c(0,420,600))
# 
plot(Photo~Tleaf,
     data = t.spots.df,
     xlim=c(20,45),
     ylim=c(0,35),
     pch=c(16)[t.spots.df$c.level],
     col="red")
par(new=TRUE)
plot(Photo~Tleaf,
     data = spots.df,
     xlim=c(20,45),
     ylim=c(0,35),
     pch=c(16)[spots.df$c.level],
     cex=0.8,
     col="blue")
par(new=TRUE)
plot(ALEAF~Tleaf,
     data = modelled.df,
     xlim=c(20,45),
     ylim=c(0,35),
     type="p",
     pch=c(16)[modelled.df$c.level],
     col="grey",
     ann=FALSE,
     axes=FALSE)

legend("topright",
       legend = c("Spots","New T response","Photosyn"),
       pch=c(15,15,15),
       col=c("blue","red","grey"),
       bty='n')
# legend("top",
#        legend = c("400","590"),
#        pch=c(0,16),
#        col=c("black"),
#        bty='n',
#        title = "Ca",
#        horiz = TRUE)

plot(t.spots.df$VpdL~t.spots.df$Tleaf,
     col="red",
     pch=16,
     xlim=c(20,45),
     ylim=c(0,6),
     xlab="T",
     ylab="VPD")
par(new=T)
plot(VpdL~Tleaf,
     data = spots.df,
     col="blue",
     pch=16,
     xlim=c(20,45),
     ylim=c(0,6),
     ann=F,
     axes=F)
legend("topleft",
       legend = c("Spots","New T response"),
       pch=c(16),
       col=c("blue","red"),
       bty='n')


plot(Photo~VpdL,
     data = t.spots.df,
     col="red",
     pch=16,
     ylim=c(0,30),
     xlim=c(0,6),
     xlab="VPD",
     ylab="Photo")
par(new=T)
plot(Photo~VpdL,
     data = spots.df,
     col="blue",
     pch=16,
     ylim=c(0,30),
     xlim=c(0,6),
     ann=F,
     axes=F)
legend("topleft",
       legend = c("Spots","New T response"),
       pch=c(16),
       col=c("blue","red"),
       bty='n')

