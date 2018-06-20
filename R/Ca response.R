rm(list=ls())
cat("\014")

source("R/load.R")
source("R/warpar.R")
for(test.ca in c(400,550)){
  #test - lai sensitivity test
  test <- 1
  # lai in the model = measured - base 
  base <- 0.8
  # chose gs model: 4 is optbb; 6 is tuzet
  gs.model.num <- 4
  vc.vpd <- TRUE
  # vj.ratio
  vj.ratio.test = TRUE
  vj.ratio = 0.5
  # fix CA
  fix.ca=TRUE
  CA.in=test.ca
  # make sure you want to do this first
  time.used <- eucGPP(startDate = as.Date("2013-01-01"),
                      endDate = as.Date("2016-12-31"),
                      lai.test = test,
                      lai.base = base,
                      rings = 1:6,
                      model.sel = gs.model.num,
                      hourly.data = TRUE,
                      vc.vpd = vc.vpd,
                      vj.ratio.test = vj.ratio.test,
                      vj.ratio = vj.ratio,
                      fix.ca=fix.ca,
                      CA.in=CA.in)
  
  

  # move file to output folders####
  file.nm <- c("maespa trans vs hp hrly.pdf",
               "mastra and sap hr.rds",
               "mastra and sap 05hr.rds",
               "all.hr.rds",
               "mastra and sap.rds",
               "maespa trans vs hp.pdf")
  dir.create("output/vj16")
  dir.create("output/vj16/field")
  dir.create("output/vj2")
  dir.create("output/vj2/field")
  
  
  if(fix.ca==TRUE){
    if(identical(vj.ratio.test,FALSE)){
      
      dir.create(file.path("output","vj16",paste0(CA.in)))
      
      file.rename(from=file.nm,
                  to=file.path("output","vj16",paste0(CA.in),file.nm))
    }else{
      dir.create(file.path("output","vj2",paste0(CA.in)))
      file.rename(from=file.nm,
                  to=file.path("output","vj2",paste0(CA.in),file.nm))
      
    }
  }else{
    if(identical(vj.ratio.test,FALSE)){
      dir.create(file.path("output","vj16",paste0(CA.in)))
      file.rename(from=file.nm,
                  to=file.path("output","vj16","field",file.nm))
    }else{
      dir.create(file.path("output","vj2",paste0(CA.in)))
      file.rename(from=file.nm,
                  to=file.path("output","vj2","field",file.nm))
      
    }
  }
}

