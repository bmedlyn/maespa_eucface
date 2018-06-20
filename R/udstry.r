# get data from hiev####
downloadHIEv(searchHIEv("EucFACE_P0061_Pathare"),topath = "download/")
ud.gx.df <- read.csv(unz("download/EucFACE_P0061_Pathare_understorey_photosynthesis_nutrients_biomass.zip", 
                         "data/FACE_P0061_RA_GASEXCHANGE_NITROGEN_C3_L2_20130201-20160430.csv"))
ud.bim.df <- downloadCSV("FACE_P0061_RA_PATHARE_UNDERSTORY_ABOVEGROUND")
# get microlaena subset and average over date and ring####
microlaena.gx.df <- ud.gx.df[ud.gx.df$species == "Microlaena stipoides",]
library(doBy)
sum.df <- summaryBy(vcmax_insitu + jmax_insitu + leaf_mass_per_area ~
                      campaign +ring,
                    data = ud.gx.df,id=~date,
                    FUN = c(mean),na.rm=TRUE)
sum.df <- sum.df[complete.cases(sum.df),]

# biomass####
sum.bim.df <- summaryBy((grasses_live_g_per_m2 + forbs_g_per_m2)~ring,
                    data = ud.bim.df,
                    FUN = c(mean),na.rm=TRUE)

merger.df <- merge(sum.df,sum.bim.df,by="ring")
# get lai####
# get lma g m-2
LMA <- merger.df$leaf_mass_per_area.mean 
LMA <- 1/ (131.96 * 10^-4) #number from mingkai with unit from cm2 g-1 to g m-2
# LMA <- 70 #number esimated by Mingkai
merger.df$lai <- merger.df$`(grasses_live_g_per_m2 + forbs_g_per_m2).mean` / LMA

# fit g1#####
library(plantecophys)
merger.df$g1 <-  coef(fitBB(data = microlaena.gx.df,
                            varnames = list(ALEAF="photo",GS="cond",
                                            VPD = "vpdl",Tleaf="tleaf",Ca = "co2r")))[[2]]
# fix the date
library(lubridate)
merger.df$Day <- parse_date_time(as.character(merger.df$date), "B d Y")
merger.df$Day[19] <- as.Date("2013-01-24")
merger.df$Day <- format(merger.df$Day,"%d/%m/%y")
# read rd T data####
# currently using canopy value
rd.df <- read.csv("download/FACE_P0064_RA_GASEXCHANGE-RdarkT_20160215-L1.csv")
# data clean
rd.df$Ring <-substr(rd.df$Tree,2,2)
rd.df <- rd.df[rd.df$Tleaf<40,]
wrong.curve <- rd.df$CurveID[rd.df$Tleaf < 40 & rd.df$Photo < -4]
rd.df <- rd.df[!rd.df$CurveID %in% wrong.curve,]
# fit the t response
rd.t.vec <- fit.rd.t.function(rd.df)
rd.t.df <- data.frame(rd.25  = rep(rd.t.vec[[1]],6),
                      q10 = rep(rd.t.vec[[2]],6),
                      ring=1:6)
# upday phyUdi which is the understory physiological parameters############
und.phy.func <- function(){
  for (i in 1:6){
    #assign g1
    fn <- sprintf("Rings/Ring%s/runfolder/phyUds.dat",i)
    replaceNameList(namelist="bbmgs",datfile=fn,
                    vals=list(g0 = 0,
                              g1 = merger.df$g1,
                              dates = merger.df$Day,
                              nsides = 1,
                              wleaf = 0.01, 
                              VPDMIN = 0.05,
                              gamma = 0)
    )
    
    replaceNameList(namelist="bbgscon",datfile=fn,
                    vals=list(nodates = length(merger.df$Day),
                              condunits = 'H2O'
                    ))
    
    #Vcmax and Jmax
    options(scipen=999)
    
    # if(vj.ratio.test == FALSE){
      jmax.use = merger.df$jmax_insitu.mean[merger.df$ring == i]
    # }else{
    #   jmax.use = vj.ratio * euc.sum.df$Vcmax[euc.sum.df$Ring == i]
    # }
    
    replaceNameList(namelist="jmax",datfile=fn,
                    vals=list(values=jmax.use,
                              dates =merger.df$Day[merger.df$ring == i]))
    replaceNameList(namelist="Vcmax",datfile=fn,
                    vals=list(values=merger.df$vcmax_insitu.mean[merger.df$ring == i],
                              dates =merger.df$Day[merger.df$ring == i]))
    
    replaceNameList(namelist="jmaxcon",datfile=fn,
                    vals=list(nolayers = 1,
                              noages = 1,
                              nodates = length(merger.df$Day[merger.df$ring == i])))
    
    replaceNameList(namelist="Vcmaxcon",datfile=fn,
                    vals=list(nolayers = 1,
                              noages = 1,
                              nodates = length(merger.df$Day[merger.df$ring == i])))
    
    t.response.df <- readRDS("cache/ecu_t_response.rds")
    replaceNameList(namelist="VCMAXPARS",datfile=fn,
                    vals=list(EAVC = t.response.df["Ea","Vcmax"]*1000,
                              EDVC = 200000,
                              DELSC = t.response.df["delS","Vcmax"]*1000
                    ))
    # THETA, EAVJ, EDVJ, DELSJ, AJQ, IECO 
    replaceNameList(namelist="JMAXPARS",datfile=fn,
                    vals=list(THETA =  restult.lrc$theta,
                              AJQ = restult.lrc$alpha.j,
                              EAVJ = t.response.df["Ea","Jmax"]*1000,
                              EDVJ = 200000,
                              DELSJ = t.response.df["delS","Jmax"]*1000
                    ))
    # rd t response 
    replaceNameList(namelist="rdpars",datfile=fn,
                    vals=list(rtemp = 25,
                              q10f = rd.t.df$q10[rd.t.df$ring == i],
                              dayresp = 1.0,
                              effyrf = 0.4
                    ))
    
    replaceNameList(namelist="RD",datfile=fn,
                    vals=list(VALUES = -rd.t.df$rd.25[rd.t.df$ring == i], 
                              DATES = '12/04/01'
                              
                    ))
  }
}