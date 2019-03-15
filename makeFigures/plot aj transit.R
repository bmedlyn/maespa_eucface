# 
t.response.df <- readRDS("cache/ecu_t_response.rds")
euc.acis.df <- read.csv("data/Aci.EucFACE.csv")
# data clean
euc.acis.df <- euc.acis.df[euc.acis.df$Photo < 50,]
euc.acis.df <- euc.acis.df[euc.acis.df$Photo > -2,]
euc.acis.df <- euc.acis.df[euc.acis.df$Cond > 0 ,]
euc.acis.df <- euc.acis.df[complete.cases(euc.acis.df$Number),]
# # # plot to check data
# plot(Photo~Ci,data = euc.acis.df[euc.acis.df$Number == 478,])
# library(plantecophys)
euc.fit <- fitacis(euc.acis.df,group="Number",Tcorrect=TRUE,fitmethod = c("bilinear"),
                   varnames = list(ALEAF = "Photo",Tleaf = "Tleaf", 
                                   Ci = "Ci", PPFD = "PARi"),
                   EaV = t.response.df["Ea","Vcmax"]*1000, 
                   EdVC = 2e+05,
                   delsC = t.response.df["delS","Vcmax"]*1000, 
                   EaJ = t.response.df["Ea","Jmax"]*1000, 
                   EdVJ = 2e+05, delsJ = t.response.df["delS","Jmax"]*1000,
                   alpha = 0.3,
                   theta = 0.475)

see <- Filter(function(x) length(x)>1, euc.fit)
euc.coef <- as.data.frame(do.call(rbind,sapply(see,function(x) out.df <- data.frame(coef(x)))))
names(euc.coef) <- c("Vcmax","Jmax","Rd")
euc.coef$Number <- as.numeric(names(see))

# put fits and measurements together
euc.all.df <- merge(euc.coef,euc.acis.df,by = "Number")
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

euc.amb.df <- euc.all.df[euc.all.df$Ring %in% c(2,3,6),]

euc.1st.df <- euc.amb.df[firstobs(~Number,data=euc.amb.df),] 
euc.1st.df <- euc.1st.df[euc.1st.df$CO2R < 450 & euc.1st.df$CO2R > 370,]




# plot()


library(plantecophys)
get.vj.trans.func <- function(par,i){
  a.out <- Photosyn(#g1 = 4.3,
                    Vcmax = euc.1st.df$Vcmax[i],
                    Jmax = euc.1st.df$Jmax[i],
                    
                    Ci = euc.1st.df$Ci[i],
                    VPD = euc.1st.df$VpdL[i],
                    PPFD = par,
                    Tleaf = euc.1st.df$Tleaf[i],
                    
                    alpha = 0.3, theta = 0.4756,
                    
                    EaV = 65469,#74189,
                    EdVC = 2e+05,
                    delsC = 639.52,
                    
                    EaJ = 34463,#39513,
                    EdVJ = 2e+05,
                    delsJ = 639.01,
                    Rd = 1.3)
  a.vj.diff <- a.out$Aj - a.out$Ac
  return(abs(a.vj.diff))
}
# 6.395

par.vec <- c()
for (i in 1:nrow(euc.1st.df)){
  
  tleaf <- euc.1st.df$Tleaf[i]
  
  par.vec[i] <- optim(800,get.vj.trans.func,i=i,method = 'L-BFGS-B',lower = 200,upper = 10000)$par
}


euc.1st.df$par.transit <- par.vec






plot(par.vec~euc.1st.df$Tleaf,pch=16,type='p',ylim=c(0,10000),ylab='PAR',
     xlab='Tleaf')




euc.1st.df$jv <- euc.1st.df$Jmax / euc.1st.df$Vcmax

plot(par.vec~euc.1st.df$jv ,pch=16,type='p',ylim=c(0,10000),ylab='PAR',
     xlab='J:V')



see <- Photosyn(#g1 = 4.3,
                Vcmax = euc.1st.df$Vcmax,
                Jmax = euc.1st.df$Jmax,
                
                # Ca = euc.1st.df$CO2S,
                Ci = euc.1st.df$Ci,
                VPD = euc.1st.df$VpdL,
                PPFD = 1800,
                Tleaf=euc.1st.df$Tleaf,
                
                alpha = 0.3, theta = 0.5756,
                
                EaV = 74189,
                EdVC = 2e+05,
                delsC = 641.989,
                
                EaJ = 39513,
                EdVJ = 2e+05,
                delsJ = 640.2658,
                Rd = 1.3)

plot(Aj~Tleaf,data = see,pch=16,type='p',ylim=c(10,40),ylab='Photo',
     xlab='Tleaf')

points(Ac~Tleaf,data = see,pch=16,type='p',col='red')

legend('topright',legend = c('Ac','Aj'),pch=16,col=c('red','black'))
# euc.all.df$Date <-  as.Date(euc.all.df$Date,"%b %d %Y")

# euc.all.df <- euc.all.df[!duplicated(euc.all.df[,c("Number")]),]
# # get ring average
# euc.sub.df <- data.frame(Campaign = euc.all.df$Campaign,
#                          Date = euc.all.df$Date,
#                          Ring = euc.all.df$Ring,
#                          Vcmax = euc.all.df$Vcmax,
#                          Jmax = euc.all.df$Jmax)
# 
# euc.sum.df <- summaryBy(Vcmax + Jmax ~ Ring + Campaign,
#                         data = euc.sub.df,
#                         id=~Date,
#                         FUN=mean,na.rm = TRUE,
#                         keep.names = TRUE)
# 
# euc.sum.df$Vcmax <- round(euc.sum.df$Vcmax)
# euc.sum.df$Jmax <- round(euc.sum.df$Jmax)
# euc.sum.df <- euc.sum.df[order(euc.sum.df$Date),]
# euc.sum.df$Date <- format(as.Date(euc.sum.df$Date), format=c("%d/%m/%y")) 