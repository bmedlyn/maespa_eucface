update.phy.f <- function(lai.test,lai.base){
  # need to be on hiev
  means <- read.csv("NewFlatfile_Ring_means_Euc_ACi_summary_all-campaigns.csv")
  old <- means[which(means$Agecls == "old"),]
  
  temp<-lapply(c(1:6),function(i){
    
    Ring <- old[which(old$Ring==i),]
    library(lubridate)
    Ring$Campaign <-as.Date(dmy(Ring$Campaign),format="%y/%m/%d")
    Ring <- subset(Ring,select = c("Vcmax","Jmax","Campaign"))
    Ring<- Ring[order(Ring$Campaign, decreasing = FALSE),]
    Ring$Campaign <- as.Date(Ring$Campaign,format="%y/%m/%d")
    Ring$Date<- format(Ring$Campaign, format=c("%d/%m/%y")) 
    return(Ring)
  })
  
  #g1 from Teresa's 2015 paper need to be on hiev
  g1 <- c(4.637, 3.564, 4.049, 4.084, 4.96, 3.413, 4.242)
  g1Date <- c('12/04/01','12/05/01','12/10/01','13/02/01','13/05/01','13/09/01','13/11/01') 
  g1Date <- as.Date(g1Date,format="%y/%m/%d")
  g1Date <- as.Date(g1Date,format="%d/%m/%y")
  test <- data.frame(g1,g1Date)
  test$Date <- format(test$g1Date,format="%d/%m/%y")
  
  for (i in 1:6){
    #assign g1
    fn <- sprintf("Rings/Ring%s/runfolder/phy.dat",i)
    replaceNameList(namelist="bbmgs",datfile=fn,
                    vals=list(g0 = numeric(length(test$g1)),
                              g1 = g1,
                              dates = test$Date,
                              nsides = 1,
                              wleaf = 0.02, 
                              VPDMIN = 0.05,
                              gamma = 0)
    )
    
    replaceNameList(namelist="bbgscon",datfile=fn,
                    vals=list(nodates = length(test$Date),
                              condunits = 'h2o'
                    ))
    
    #Vcmax and Jmax
    options(scipen=999)
    
    #Ring1
    replaceNameList(namelist="jmax",datfile=fn,
                    vals=list(values=temp[[i]]$Jmax,
                              dates = temp[[i]]$Date))
    replaceNameList(namelist="Vcmax",datfile=fn,
                    vals=list(values=temp[[i]]$Vcmax,
                              dates = temp[[i]]$Date))
    
    replaceNameList(namelist="jmaxcon",datfile=fn,
                    vals=list(nolayers = 1,
                              noages = 1,
                              nodates = length(temp[[i]]$Date)))
    replaceNameList(namelist="Vcmaxcon",datfile=fn,
                    vals=list(nolayers = 1,
                              noages = 1,
                              nodates = length(temp[[i]]$Date)))
    
    # make plot
    ######
    pdf("g1 vcmax jmax.pdf",width=12,height=9)
    #plot g1 vcmax gmax for each ring####
    par(mar=c(2,6,3,6), mfrow=c(3,1),cex = 0.8)
    par(mar=c(0,6,2,6))
    palette(c("chocolate2","cadetblue2","cadetblue3","chocolate3","chocolate4","cadetblue4"))
    #Vcmax
    with(temp[[1]],plot(Campaign,Vcmax,axes = FALSE,ylim = c(0,150),
                        ylab = expression(italic(Vc)[max]~(paste(mu,mol~m^-2~s^-1))),
                        xlab = " ",xaxt = 'n',col= "white"))
    # axis(1,at=temp[[1]]$Campaign, lab=paste0(temp[[1]]$Campaign),col = "white")
    axis(2,at=seq(0,150,50), lab=paste0(seq(0,150,50)),cex = 0.5)
    abline(h=150)
    abline(h=0)
    abline(v=as.Date("2015-02-28"))
    for(i in 1:6){
      par(new = TRUE)
      with(temp[[i]],plot(Campaign,Vcmax,axes = FALSE,ann = FALSE,
                          xlab = " ",xaxt = 'n',
                          ylim = c(0,150),type = "b",col= i))
    }
    legend("bottomright", xpd = TRUE,horiz = TRUE, bty = "n",cex = 1,
           legend = strwrap(c("R1","R2","R3","R4","R5","R6"), width = 2),
           lwd =2, col= c("chocolate2","cadetblue2","cadetblue3","chocolate3","chocolate4","cadetblue4"))
    
    #jmax
    par(mar=c(2,6,0.5,6))
    with(temp[[i]],plot(Campaign,Jmax,ylim = c(0,250),
                        ylab = expression(italic(J)[max]~(paste(mu,mol~m^-2~s^-1))),
                        axes = FALSE,xlab = " ",col= "white"))
    axis(1,at=temp[[i]]$Campaign, lab=paste0(temp[[i]]$Campaign),col = "white")
    axis(2,at=seq(0,250,50), lab=paste0(seq(0,250,50)),cex = 0.5)
    abline(h=250)
    abline(h=0)
    abline(v=as.Date("2015-02-28"))
    for(i in 1:6){
      par(new = TRUE)
      with(temp[[i]],plot(Campaign,Jmax,axes = FALSE,ann = FALSE,
                          xlab = " ",
                          ylim = c(0,250),type = "b",col= i))
    }
    legend("bottomright", xpd = TRUE,horiz = TRUE, bty = "n",cex = 1,
           legend = strwrap(c("R1","R2","R3","R4","R5","R6"), width = 2),
           lwd =2, col= c("chocolate2","cadetblue2","cadetblue3","chocolate3","chocolate4","cadetblue4"))
    par(mar=c(6,6,2,6))
    # G1
    with(test,plot(g1Date,g1,ylim = c(0,6),
                   ylab = expression(italic(g)[1]~(kPa^0.5)),
                   xlab = " ",col = "red",axes = FALSE,type = "b"))
    axis(1,at=test$g1Date, lab=paste0(test$g1Date),col = "white")
    axis(2,at=seq(0,6,2), lab=paste0(seq(0,6,2)),cex = 0.5)
    abline(h=0)
    abline(h=6)
    abline(v=as.Date("2013-11-24"))
    #####
    dev.off()
  }
  
  writeLines("phy updated")
  
}

