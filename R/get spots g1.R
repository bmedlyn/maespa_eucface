######
# get g1####
fit.g1.func <- function(spots,byRing = TRUE){
  # spots <- read.csv("data/Gimeno_spot_Eter_gasExchange6400.csv")
  spots <- spots[is.na(spots$Tree) != TRUE,]
  spots$Date <- as.character(spots$Date)
  spots$Date[spots$Date == "31/11/2012"] <- "31/10/2012"
  spots$Date <- as.Date(as.character(spots$Date),"%d/%m/%Y")
  spots$Date[spots$Date > as.Date("2013-09-08") & spots$Date < as.Date("2013-09-15")] <- as.Date("2013-09-10")
  spots$Campaign <- droplevels(spots$Campaign)
  
  if(byRing==TRUE){
    fit.spot <- fitBBs(spots,"group",gsmodel = c("BBOpti"))
    spots.g1 <- merge(coef(fit.spot),unique(spots[,c("Ring","Date","group","Campaign","Measurement")]),
                      by.x="group",by.y="group",all.y=FALSE)
    spots.g1 <- spots.g1[complete.cases(spots.g1),]
    spots.g1 <- spots.g1[order(spots.g1$Date),]
  }else{
    spots$Campaign <- as.character(spots$Campaign)
    fit.spot <- fitBBs(spots,'Campaign',gsmodel = c("BBOpti"))
    spots.g1 <- merge(coef(fit.spot),unique(spots[,c("Date","Campaign")]),
                      by.x="group",by.y="Campaign",all.y=FALSE)
    spots.g1 <- spots.g1[complete.cases(spots.g1),]
    spots.g1 <- spots.g1[order(spots.g1$Date),]
  }
  return(spots.g1)
}

spots <- read.csv(unz("download/Gimeno_spot.zip", 
                      "data/Gimeno_spot_Eter_gasExchange6400.csv"))
spots <- spots[complete.cases(spots$Photo),]
# remove pred treat data which doesn't has swc with them anyway
spots <- spots[-(grep("2012Apr",as.character(spots$Campaign))),]
spots <- spots[-(grep("2012May",as.character(spots$Campaign))),]
spots$group <- paste0(as.character(spots$Campaign),"-",as.character(spots$Ring),"+",spots$Measurement)
# spots$facotrs <- paste0(spots$Campaign,"+",spots$Measurement) 
# spots <- spots[spots$group != "-",]
spots.g1.ma <- fit.g1.func(spots)
spots.g1.ma$Ring <- as.character(spots.g1.ma$Ring)
spots.g1.ma <- tidyr::spread(spots.g1.ma,Measurement,g1)
spots.g1.ma <- doBy::summaryBy(Anet_aft + Anet_mor ~ Date + Campaign + Ring,
                               data = spots.g1.ma,keep.names = TRUE,FUN=mean,na.rm=TRUE)
names(spots.g1.ma) <- c("Date","Campaign","Ring","g1.aft","g1.mor")

spots$group <- paste0(as.character(spots$Campaign),"-",as.character(spots$Ring))
spots.g1.one <- fit.g1.func(spots)
spots.g1.one <- doBy::summaryBy(g1 ~ Date + Campaign + Ring,
                                data = spots.g1.one,keep.names = TRUE,FUN=mean,na.rm=TRUE)

