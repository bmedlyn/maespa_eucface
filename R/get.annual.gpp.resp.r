annual.sum <- list()
for (i in 1:6){
  
  hr.flux[[i]]$ring <- i
}


annual.sum <- do.call(rbind,hr.flux)
# annual.sum$hrPs / (pi*12.5^2) / 2
# annual.sum$hou

# getting ring total
annual.summary <- summaryBy(hrPs + hrRf ~ DOY + HOUR + ring ,
                            data=annual.sum,
                            FUN=sum,na.rm=TRUE,keep.names = TRUE)

# average by ground area 
annual.summary$gpp.half.hr <- (annual.summary$hrPs + annual.summary$hrRf) * 1e-6 * 12 / (pi*12.5^2) * 1800  
annual.summary$resp.half.hr <- (annual.summary$hrRf) * 1e-6 * 12 / (pi*12.5^2) * 1800  


gpp.vec <- c()
resp.vec <- c()  
  for (i in 1:6){
    gpp.vec[i] <- sum(annual.summary$gpp.half.hr[annual.summary$ring == i])
    resp.vec[i] <- sum(annual.summary$resp.half.hr[annual.summary$ring == i])
}
# sum(annual.summary$gpp.half.hr[annual.summary$ring == 1])
# sum(annual.summary$gpp.half.hr[annual.summary$ring == 2])
# sum(annual.summary$gpp.half.hr[annual.summary$ring == 3])
# sum(annual.summary$gpp.half.hr[annual.summary$ring == 4])
# sum(annual.summary$gpp.half.hr[annual.summary$ring == 5])
# sum(annual.summary$gpp.half.hr[annual.summary$ring == 6])

out.df <- data.frame(Date = "2013-01-01 to 2013-12-31",
                     Ring = seq(1,6),
                     c_treat = c("E","A","A","E","E","A"),
                     GPP.mg.m2.d = gpp.vec * 1000 /365,
                     Respiration.mg.m2.d = resp.vec * 1000/365)

write.csv(out.df,"2013_maespa_gpp_respiration.csv")
