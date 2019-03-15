rm(list=ls())
cat("\014")
# load and set the model####
source("R/load.R")
# source("R/warpar.R")

for (i in 1:6){
  #assign extwind: effciency of wind exponential decline with canopy depth 
  fn <- sprintf("Rings/Ring%s/runfolder/str.dat",i)
  replaceNameList(namelist="aero",
                  datfile=fn,
                  vals=list(extwind = 0.0))
  
  replaceNameList(namelist="lia",
                  datfile=fn,
                  vals=list(NALPHA = 1, ELP = 1.0))
}
#test - lai sensitivity test#
test <- 1.0
# lai in the model = measured - base 
# specify test parameters 
# base <- c(0.69,0.79,0.63,0.59,0.82,0.60) # brach and stem area to be substracted from LAI
base <- rep(0.8,6)
# # vj.ratio#
# vj.ratio.test = FALSE
# vj.ratio = 1.6
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
# gs.model.num <- 4
# inputs for running scenarios
ca.e <- TRUE
photo.acli <- FALSE
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
                    model.sel = 4,
                    hourly.data = TRUE,
                    vc.vpd = TRUE,
                    vj.ratio.test = FALSE,
                    vj.ratio = 0,
                    swc.g1 = TRUE,
                    ca.change = ca.e,
                    photo.acli = photo.acli)


# stop('end here')
# analysis###################
source("R/get flux.r")
source("R/get histo.r")
# move file to output folders####
fn.vec <- c(#"maespa trans vs hp hrly.pdf",
            # "mastra and sap hr.rds",
            #"mastra and sap 05hr.rds",
            #"all.hr.rds",
            "mastra and sap.rds",
            # "maespa trans vs hp.pdf",
            'histo.rds')

if(identical(photo.acli,TRUE) & identical(ca.e,TRUE)){
  file.rename(from=fn.vec,
              to=file.path("output","accli",fn.vec))
}else{
  if(identical(ca.e,TRUE)){
    file.rename(from=fn.vec,
                to=file.path("output","elevated",fn.vec))
  }else{
    file.rename(from=fn.vec,
                to=file.path("output","ambient",fn.vec))
  }
}

