rm(list=ls())
cat("\014")
# the trunck hieght problems still there is ring 6

source("R/load.R")
source("R/warpar.R")

for (i in 1:6){
  fn <- sprintf("Rings/Ring%s/runfolder/confile.dat",i)
  replaceNameList(namelist="CCSCEN",datfile=fn,
                  vals=list(CO2INC = 0,
                            TINC = 0))
}
# 
# for (co2.increase in c(150,-150)){
#   # co2.increase <- -150
#   temp.increase <- 0
#   
#   if(co2.increase == -150){
#     ca.change <- c(1,4,5)
#   }
#   if(co2.increase == 150){
#     rings <-  c(2,3,6)
#   }
#   

  
  #test - lai sensitivity test
  test <- 1
  # lai in the model = measured - base 
  # base <- c(0.777,0.788,0.697,0.736,1.007,0.641)
  base <-rep(0.8,6)
  # chose gs model: 4 is optbb; 6 is tuzet
  gs.model.num <- 4
  vc.vpd <- TRUE
  # vj.ratio
  vj.ratio.test = FALSE
  vj.ratio = 2
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
                      swc.g1 = TRUE,ca.change = TRUE)
# }
# analysis###################
source("R/get flux.r")

# move file to output folders####
fn.vec <- c("maespa trans vs hp hrly.pdf",
            "mastra and sap hr.rds",
            "mastra and sap 05hr.rds",
            "all.hr.rds",
            "mastra and sap.rds",
            "maespa trans vs hp.pdf")

dir.create(file.path("output",paste0(150)))

file.rename(from=fn.vec,
            to=file.path("output",paste0(150),fn.vec))
