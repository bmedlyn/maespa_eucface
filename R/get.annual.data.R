# run the model######
rm(list=ls())
cat("\014")
co2.increase <- -150
source("R/changeCO2andT.R")

rm(list=ls())
cat("\014")
co2.increase <- 150
source("R/changeCO2andT.R")

source("eucface_gpp.R")
rm(list=ls())
cat("\014")
source("makeFigures/plotEA.R")
# read daily fluxes############
library(dplyr)
library(scales)
data.both.sap.m.150 <- readRDS("mastra and sap_1_0.8_-150_0.rds")
data.both.sap.p.150 <- readRDS("mastra and sap_1_0.8_150_0.rds")
data.both.sap.ctrl <- readRDS("output/maestraVPD/mastra and sap.rds")

write.csv(data.both.sap.m.150[data.both.sap.m.150$Ring %in% c("R1","R4","R5"),],
          "elevated rings with ambient runs.csv",row.names = FALSE)

write.csv(data.both.sap.p.150[data.both.sap.p.150$Ring %in% c("R2","R3","R6"),],
          "ambient rings with elevated runs.csv",row.names = FALSE)

write.csv(data.both.sap.ctrl,
          "rings with field condition runs.csv",row.names = FALSE)

# get start and end day#
con.ls <- readLines("Rings/Ring1/runfolder/confile.dat")

sd <- gsub("startdate = ","",
           con.ls[grep("startdate", con.ls)])
sd <- gsub("'","",sd) %>%  as.Date("%d/%m/%y")


ed <- gsub("enddate = ","",
           con.ls[grep("enddate", con.ls)])
ed <- gsub("'","",ed) %>%  as.Date("%d/%m/%y")

years <- as.numeric(ed-sd)/365
# annual sum gpp 
out.same.df <- data.both.sap.ctrl[,c("Date","Ring","LAI","TAIR","PPT","APAR","volRing")]
out.e.df <- data.frame(ring=c(1,4,5,2,3,6),
                       treat.model = "E",
                       treat.field = c("E","E","E","A","A","A"),
                       
                       gpp=c(sum(data.both.sap.ctrl$GPP[data.both.sap.ctrl$Ring == "R1"])/years,
                             sum(data.both.sap.ctrl$GPP[data.both.sap.ctrl$Ring == "R4"])/years,
                             sum(data.both.sap.ctrl$GPP[data.both.sap.ctrl$Ring == "R5"])/years,
                             
                             sum(data.both.sap.p.150$GPP[data.both.sap.p.150$Ring == "R2"])/years,
                             sum(data.both.sap.p.150$GPP[data.both.sap.p.150$Ring == "R3"])/years,
                             sum(data.both.sap.p.150$GPP[data.both.sap.p.150$Ring == "R6"])/years),
                       
                       trans=c(sum(data.both.sap.ctrl$Trans[data.both.sap.m.150$Ring == "R1"])/years,
                               sum(data.both.sap.ctrl$Trans[data.both.sap.m.150$Ring == "R4"])/years,
                               sum(data.both.sap.ctrl$Trans[data.both.sap.m.150$Ring == "R5"])/years,
                               
                               sum(data.both.sap.p.150$Trans[data.both.sap.ctrl$Ring == "R2"])/years,
                               sum(data.both.sap.p.150$Trans[data.both.sap.ctrl$Ring == "R3"])/years,
                               sum(data.both.sap.p.150$Trans[data.both.sap.ctrl$Ring == "R6"])/years),
                       
                       resp = c(sum(data.both.sap.ctrl$Ra[data.both.sap.m.150$Ring == "R1"])/years,
                                sum(data.both.sap.ctrl$Ra[data.both.sap.m.150$Ring == "R4"])/years,
                                sum(data.both.sap.ctrl$Ra[data.both.sap.m.150$Ring == "R5"])/years,
                                
                                sum(data.both.sap.p.150$Ra[data.both.sap.ctrl$Ring == "R2"])/years,
                                sum(data.both.sap.p.150$Ra[data.both.sap.ctrl$Ring == "R3"])/years,
                                sum(data.both.sap.p.150$Ra[data.both.sap.ctrl$Ring == "R6"])/years)
)

out.a.df <- data.frame(ring=c(1,4,5,2,3,6),
                       treat.model = "A",
                       treat.field = c("E","E","E","A","A","A"),
                       
                       gpp=c(sum(data.both.sap.m.150$GPP[data.both.sap.ctrl$Ring == "R1"])/years,
                             sum(data.both.sap.m.150$GPP[data.both.sap.ctrl$Ring == "R4"])/years,
                             sum(data.both.sap.m.150$GPP[data.both.sap.ctrl$Ring == "R5"])/years,
                             
                             sum(data.both.sap.ctrl$GPP[data.both.sap.p.150$Ring == "R2"])/years,
                             sum(data.both.sap.ctrl$GPP[data.both.sap.p.150$Ring == "R3"])/years,
                             sum(data.both.sap.ctrl$GPP[data.both.sap.p.150$Ring == "R6"])/years),
                       
                       trans=c(sum(data.both.sap.m.150$Trans[data.both.sap.m.150$Ring == "R1"])/years,
                               sum(data.both.sap.m.150$Trans[data.both.sap.m.150$Ring == "R4"])/years,
                               sum(data.both.sap.m.150$Trans[data.both.sap.m.150$Ring == "R5"])/years,
                               
                               sum(data.both.sap.ctrl$Trans[data.both.sap.ctrl$Ring == "R2"])/years,
                               sum(data.both.sap.ctrl$Trans[data.both.sap.ctrl$Ring == "R3"])/years,
                               sum(data.both.sap.ctrl$Trans[data.both.sap.ctrl$Ring == "R6"])/years),
                       
                       resp = c(sum(data.both.sap.m.150$Ra[data.both.sap.m.150$Ring == "R1"])/years,
                                sum(data.both.sap.m.150$Ra[data.both.sap.m.150$Ring == "R4"])/years,
                                sum(data.both.sap.m.150$Ra[data.both.sap.m.150$Ring == "R5"])/years,
                                
                                sum(data.both.sap.ctrl$Ra[data.both.sap.ctrl$Ring == "R2"])/years,
                                sum(data.both.sap.ctrl$Ra[data.both.sap.ctrl$Ring == "R3"])/years,
                                sum(data.both.sap.ctrl$Ra[data.both.sap.ctrl$Ring == "R6"])/years)
)




out.annual.df <- rbind(out.a.df,out.e.df)
write.csv(out.annual.df,"annual average of MAPESPA 20130101_20170101.csv")



# make copy in dropbox# #####
file.nm <- c("annual average of MAPESPA 20130101_20170101.csv",
             "elevated rings with ambient runs.csv",
             "ambient rings with elevated runs.csv",
             "rings with field condition runs.csv",
             "gpp lue wue.pdf")

drobox.loc <- "C:/Jinyan(Jim)/Program/Dropbox/Jim- Shared folder/maespa out"

file.copy(from=file.nm, to=drobox.loc, 
          overwrite = TRUE, 
          copy.mode = TRUE)

for(i in 1:6){
  dropbox.ring <- file.path(drobox.loc,sprintf("Ring%s",i))
  if(!dir.exists(dropbox.ring))dir.create(dropbox.ring)
  
  file.copy(from=sprintf("Rings/Ring%s/runfolder/phy.dat",i), 
            to=dropbox.ring, 
            overwrite = TRUE, 
            copy.mode = TRUE)
  
  file.copy(from=sprintf("Rings/Ring%s/runfolder/Trees.dat",i), 
            to=dropbox.ring, 
            overwrite = TRUE, 
            copy.mode = TRUE)
  
  file.copy(from=sprintf("Rings/Ring%s/runfolder/str.dat",i), 
            to=dropbox.ring, 
            overwrite = TRUE, 
            copy.mode = TRUE)
  
  file.copy(from=sprintf("Rings/Ring%s/runfolder/confile.dat",i), 
            to=dropbox.ring, 
            overwrite = TRUE, 
            copy.mode = TRUE)
  
}


