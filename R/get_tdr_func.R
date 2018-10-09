# need hiev, data.table packages
meanVWC <- function(dfr){
  vwccols <- grep("VWC_",names(dfr))
  dfr <- dfr[,vwccols]
  dfr[dfr > 1] <- NA
  rowMeans(dfr, na.rm=TRUE)
}
# this function get the theta and tdr value for each day in each ring
get.tdr.func <- function(s.date,e.date){
  swc.day.ls <- list()
  for (i in 1:6){
    swc.df <- downloadTOA5(sprintf("FACE_R%s_B1_SoilVars",i),
                           startDate = s.date,
                           endDate = e.date)
    # get tdr value
    swc.df$swc.tdr <- meanVWC(swc.df) * 100
    
    swc.df <- subset(swc.df,select = c("Date",
                                       "DateTime",
                                       "Theta5_1_Avg","Theta5_2_Avg",
                                       "Theta30_1_Avg","Theta30_2_Avg",
                                       "Theta75_1_Avg","Theta75_2_Avg",
                                       'swc.tdr'))
    # get theta value
    swc.df$swc.0.5 <- (swc.df$Theta5_1_Avg + swc.df$Theta5_2_Avg)/2
    
    swc.df$swc.5.30 <- (swc.df$Theta30_1_Avg +swc.df$Theta30_2_Avg)/2
    
    swc.df$swc.30.75 <- (swc.df$Theta75_1_Avg + swc.df$Theta75_2_Avg)/2
    
    swc.day.ls[[i]] <- data.table(swc.df)[,list(Ring = paste0("R",i),
                                                swc.theta.5 = mean(swc.0.5, na.rm=TRUE),
                                                swc.theta.30 = mean(swc.5.30, na.rm=TRUE),
                                                swc.theta.75 = mean(swc.30.75, na.rm=TRUE),
                                                swc.tdr = mean(swc.tdr, na.rm=TRUE)),
                                          
                                          by = Date]
    
    
  }
  
  swc.day.df <- do.call(rbind,swc.day.ls)
  
  return(swc.day.df)
}

