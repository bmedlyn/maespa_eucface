euc.sum.df <- readRDS("cache/ecu_aci_sum.rds")
euc.sum.df <- euc.sum.df[complete.cases(euc.sum.df),]

euc.sum.df$treat <- NA
euc.sum.df$treat[euc.sum.df$Ring %in% c(1,4,5)] <- "E"
euc.sum.df$treat[euc.sum.df$Ring %in% c(2,3,6)] <- "A"


mean(euc.sum.df$Vcmax[euc.sum.df$treat =="E"]) / mean(euc.sum.df$Vcmax[euc.sum.df$treat =="A"])
mean(euc.sum.df$Jmax[euc.sum.df$treat =="E"]) / mean(euc.sum.df$Jmax[euc.sum.df$treat =="A"])

anova(lm(Vcmax ~ treat,data = euc.sum.df))
anova(lm(Jmax ~ treat,data = euc.sum.df))
