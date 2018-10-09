rm(list=ls())
cat("\014")
# load and set the model####
source("R/load.R")
source("R/warpar.R")
for (i in 1:6){
  #assign extwind: effciency of wind exponential decline with canopy depth 
  fn <- sprintf("Rings/Ring%s/runfolder/str.dat",i)
  replaceNameList(namelist="aero",
                  datfile=fn,
                  vals=list(extwind = 0.0)
  )
}
#test - lai sensitivity test#
test <- 1.0
# lai in the model = measured - base 
# specify test parameters 
# base <- c(0.69,0.79,0.63,0.59,0.82,0.60) # brach and stem area to be substracted from LAI
base <- rep(0.8,6)
# vj.ratio#
vj.ratio.test = FALSE
vj.ratio = 1.6
# change co2 and t test ##
co2.increase <-0 #elevated co2 test; ppm
temp.increase <- 0 # temperature increase test; celsius

for ( i in 1:6){
  fn <- sprintf("Rings/Ring%s/runfolder/confile.dat",i)
  replaceNameList(namelist="CCSCEN",datfile=fn,
                  vals=list(CO2INC = co2.increase,
                            TINC = temp.increase))
}

# chose gs model: 3: Leuning; 4 is optbb; 6 is tuzet#
gs.model.num <- 4
# vcmax-D function#
vc.vpd <- FALSE
# fire up the model####
########################################################TRUE
# check all the option above before launch the model####
########################################################
# make sure you want to do this first
time.used <- eucGPP(startDate = as.Date("2013-01-01"),
                    endDate = as.Date("2016-12-31"),
                    lai.test = test,
                    lai.base = base,#note that this number is not used for now and s
                    rings = 1:6,
                    model.sel = gs.model.num,
                    hourly.data = TRUE,
                    vc.vpd = vc.vpd,
                    vj.ratio.test = vj.ratio.test,
                    vj.ratio = vj.ratio,
                    swc.g1 = TRUE,
                    ca.change = FALSE)

# analysis###################
source("R/get flux.r")

# move file to output folders####
fn.vec <- c("maespa trans vs hp hrly.pdf",
           "mastra and sap hr.rds",
           "mastra and sap 05hr.rds",
           "all.hr.rds",
            "mastra and sap.rds",
            "maespa trans vs hp.pdf")

if(identical(gs.model.num,4)&identical(vj.ratio.test,FALSE)){

  if(identical(vc.vpd,TRUE)){
    file.rename(from=fn.vec,
                to=file.path("output","maestraVPD",fn.vec))
  }else{
    file.rename(from=fn.vec,
                to=file.path("output","maestra",fn.vec))
  }
}

if(identical(gs.model.num,4)&identical(vj.ratio.test,TRUE)){

  if(identical(vc.vpd,TRUE)){
    file.rename(from=fn.vec,
                to=file.path("output","maestraVPDVJ",fn.vec))
  }else{
    file.rename(from=fn.vec,
                to=file.path("output","maestraVJ",fn.vec))
  }
}


if(identical(gs.model.num,6)){
  if(identical(vc.vpd,TRUE)){
    file.rename(from=fn.vec,
                to=file.path("output","maespaVPD",fn.vec))
  }else{
    file.rename(from=fn.vec,
                to=file.path("output","maespa",fn.vec))
    }
}



