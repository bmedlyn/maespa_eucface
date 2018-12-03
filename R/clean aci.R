clean_eucface_aci <- function(dat) {
  
  # Correct dates
  dat$Date_licor <- str_replace(dat$Date,"Thr","Thu")
  dat$Date <- parse_date_time(dat$Date_licor,c("m d y","a m d y H M S"))
  dat$Date[is.na(dat$Date)] <- as.Date("2016-10-10")
  dat$Date <- as.Date(dat$Date)
  
  # remove unrealistic data
  dat <- dat[dat$Photo < 50,]
  dat <- dat[dat$Cond > 0 ,]
  dat <- dat[dat$Photo > -10,]
  dat <- dat[dat$Ci > 10,]
  
  # Get rid of A-Ci's with less than 5 points
  len <- summaryBy(Photo~Number,data=dat,FUN=length)
  nums <- len$Number[len$Photo.length > 4]
  good <- subset(dat,Number %in% nums)
  
  return(good)
  
}
euc.acis.df <- read.csv("data/Aci.EucFACE.csv")
euc.acis.df <- clean_eucface_aci(euc.acis.df)

# # put fits and measurements together
euc.all.df <- euc.acis.df
# date format clean and change
euc.all.df$Tree <- as.factor(euc.all.df$Tree)
euc.all.df$C.treat <- as.factor(euc.all.df$C.treat)
euc.all.df$Date <- as.character(euc.all.df$Date)
euc.all.df$Ring[is.na(euc.all.df$Ring)] <- 3
euc.all.df$Date[nchar(euc.all.df$Date) > 11 & nchar(euc.all.df$Date) < 13] <- 
  substr(euc.all.df$Date[nchar(euc.all.df$Date) > 11 & nchar(euc.all.df$Date) <13],2,12)

euc.all.df$Date[nchar(euc.all.df$Date) > 13] <- 
  substr(euc.all.df$Date[nchar(euc.all.df$Date) > 13 ],5,15)
euc.all.df$Date[euc.all.df$Date == "10-Oct-16"] <- "Oct 10 2016"