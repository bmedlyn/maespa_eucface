spots <- read.csv("Gimeno_spot_Eter_gasExchange6400.csv")

spots <- spots[which(is.na(spots$Photo) == FALSE),]
modelled <- Photosyn(VPD = spots$VpdL, 
                     Ca = spots$CO2R, 
                     PPFD = spots$PARi, 
                     Tleaf = spots$Tleaf,
                     gsmodel = c("BBOpti"), 
                     g1 = 4,
                     theta = 0.4, 
                     Jmax = 133, 
                     Vcmax = 88, 
                     Rd = 0.92, 
                     Q10 = 0.067,
                     TrefR = 25, 
                     EaV = 47590, 
                     EdVC = 2e+05, 
                     delsC = 640, 
                     EaJ = 37259,
                     EdVJ = 2e+05, 
                     delsJ = 640)