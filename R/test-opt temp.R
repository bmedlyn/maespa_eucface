
photo.df <- Photosyn(VPD = 1.5, Ca = 400, PPFD = 1800, Tleaf = seq(10,50,by=0.01), 
         gsmodel = c("BBOpti"), g1 = 4.28,
         g0 = 0, gk = 0.5, vpdmin = 0.05, D0 = 5, GS = NULL, alpha = 0.24,
         theta = 0.85, Jmax = 150, Vcmax = 90, gmeso = NULL, Rd0 = 0.92,
         Q10 = 1.92, Rd = NULL, TrefR = 25, Rdayfrac = 1, EaV = 56310,
         EdVC = 200000, delsC = 632, EaJ = 19640, EdVJ = 200000,
         delsJ = 628)

with(photo.df,plot(ALEAF~Tleaf,type="l"))
abline(v=photo.df$Tleaf[photo.df$ALEAF == max(photo.df$ALEAF)],
       lty="dashed")