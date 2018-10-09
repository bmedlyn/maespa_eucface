# get swc############
swc.neutron.df <- read.csv("data/FACE_P0018_RA_NEUTRON_20120430-20180215_L1.csv")
swc.neutron.df <- swc.neutron.df[lubridate::year(as.Date(swc.neutron.df$Date)) %in% seq(2013,2016),]
swc.neutron.df$Depth <- -swc.neutron.df$Depth
swc.neutron.df <- swc.neutron.df[swc.neutron.df$Location != "Outside",]
swc.neutron.df$Date <- as.Date(as.character(swc.neutron.df$Date))
swc.neutron.df$Ring <- as.character(swc.neutron.df$Ring)

swc.neutron.ring.df <- doBy::summaryBy(VWC ~ Date + Ring + Depth ,
                                       data = swc.neutron.df,FUN=mean,na.rm=TRUE,
                                       keep.names = TRUE,id=~CO2)


swc.depth.df <- swc.neutron.ring.df

days.df <- data.frame(Date = rep(seq(as.Date("2013-01-01"),as.Date("2016-12-31"),by="day"),
                                 each=length(unique(swc.neutron.ring.df$Depth)) *
                                   length(unique(swc.neutron.ring.df$Ring))),
                      Ring = rep(rep(paste0("R",1:6),length(seq(as.Date("2013-01-01"),as.Date("2016-12-31"),by="day"))),
                                 each=length(unique(swc.neutron.ring.df$Depth))),
                      Depth = rep(unique(swc.neutron.ring.df$Depth),
                                  length(seq(as.Date("2013-01-01"),as.Date("2016-12-31"),by="day")) *
                                    length(unique(swc.neutron.ring.df$Ring)))
                      )

# swc.depth.df$Depth.rev <-  - swc.depth.df$Depth
swc.depth.df.all <- merge(days.df,swc.depth.df,all=TRUE)
swc.depth.df.all$Depth <- as.factor(swc.depth.df.all$Depth)
swc.depth.df.all$VWC[swc.depth.df.all$VWC < 0] <- 0
see.ls <- split(swc.depth.df.all,f=swc.depth.df.all$Depth)
for(i in 1:length(see.ls)){

  temp <- split(see.ls[[i]],see.ls[[i]]$Ring)

  for(j in seq_len(length(temp))){
    temp[[j]]$VWC <- zoo::na.spline(temp[[j]]$VWC)
  }
  see.ls[[i]] <- do.call(rbind, temp)
}
swc.depth.df.all <- do.call(rbind,see.ls)

# swc.depth.df.all$Depth <- as.numeric(as.character(swc.depth.df.all$Depth))


# read neutron probe data ####
swc.neutron.df <- read.csv("data/FACE_P0018_RA_NEUTRON_20120430-20180215_L1.csv")
swc.neutron.df <- swc.neutron.df[swc.neutron.df$Location != "Outside",]
swc.neutron.df$Date <- as.Date(as.character(swc.neutron.df$Date))
swc.neutron.df$Ring <- as.character(swc.neutron.df$Ring)

swc.neutron.ring.df <- doBy::summaryBy(VWC~Date+Ring,
                                       data = swc.neutron.df[swc.neutron.df$Depth <= 4000,],
                                       FUN=mean,na.rm=TRUE,
                                       keep.names = TRUE,id=~CO2)

swc.neutron.ring.df$VWC[swc.neutron.ring.df$VWC<0] <- 0