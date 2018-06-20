rm(list=ls())
cat("\014")
# load and set the model####
source("R/load.R")
source("R/warpar.R")
for(i in c((1-0.074),(1+0.074))){
  #test - lai sensitivity test####
  test <- i
  # lai in the model = measured - base 
  # specify test parameters 
  base <- 0.8 # brach and stem area to be substracted from LAI
  
  # vj.ratio###
  vj.ratio.test = FALSE
  vj.ratio = 1.6
  
  # chose gs model: 3: Leuning; 4 is optbb; 6 is tuzet####
  gs.model.num <- 4
  # vcmax-D function####
  vc.vpd <- TRUE
  # ca########
  fix.ca=TRUE
  CA.in=550
  # fire up the model####
  ########################################################TRUE
  # check all the option above before launch the model####
  ########################################################
  # make sure you want to do this first
  time.used <- eucGPP(startDate = as.Date("2013-01-01"),
                      endDate = as.Date("2016-12-31"),
                      lai.test = test,
                      lai.base = base,
                      rings = c(2,3,6),
                      model.sel = gs.model.num,
                      hourly.data = TRUE,
                      vc.vpd = vc.vpd,
                      vj.ratio.test = vj.ratio.test,
                      vj.ratio = vj.ratio,
                      fix.ca = fix.ca,
                      CA.in = CA.in)
  
  
  #analysis###################
  source("R/get flux.r")
  
  # move file to output folders####
  fn.vec <- c("maespa trans vs hp hrly.pdf",
              "mastra and sap hr.rds",
              "mastra and sap 05hr.rds",
              "all.hr.rds",
              "mastra and sap.rds",
              "maespa trans vs hp.pdf")
  dir.create(file.path("output",paste0("lai",test)))
  file.rename(from=fn.vec,
              to=file.path("output",paste0("lai",test),fn.vec))
}
