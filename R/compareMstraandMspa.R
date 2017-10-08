maespa.hr.df <- readRDS("output/maespa/mastra and sap hr.rds")
maestra.hr.df <- readRDS("output/maestra/mastra and sap hr.rds")

# get ring 4 
maespa.hr.df.r4 <- maespa.hr.df[maespa.hr.df$Ring == "R4",]
maestra.hr.df.r4 <- maestra.hr.df[maespa.hr.df$Ring == "R4",]


plot(c(maespa.hr.df.r4$Photo/3600 /12 * 10^6)~maespa.hr.df.r4$VPD,
     ylim=c(0,30))

plot(c(maestra.hr.df.r4$Photo/3600 /12 * 10^6)~maestra.hr.df.r4$VPD,
     ylim=c(0,30))

plot(c(maestra.hr.df.r4$Photo/3600 /12 * 10^6)~
       c(maespa.hr.df.r4$Photo/3600 /12 * 10^6),
     pch=16,cex=0.5)
abline(a=0,b=1,col="red")

plot(maestra.hr.df.r4$Photo~
       c(maespa.hr.df.r4$Photo),
     pch=16,cex=0.5)
abline(a=0,b=1,col="red")

plot(c(maestra.hr.df.r4$trans)~
       c(maespa.hr.df.r4$trans),
     pch=16,cex=0.5,
     ylim=c(0,0.3),
     xlim=c(0,0.3),col="coral")
abline(a=0,b=1,col="red")

points(c(maestra.hr.df.r4$sap)~
       c(maespa.hr.df.r4$trans),
     pch=16,cex=0.5,
     ylim=c(0,0.3),
     xlim=c(0,0.3),
     col="navy")
