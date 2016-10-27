# Have to have RTools installed
# http://cran.r-project.org/bin/windows/Rtools/
# Newest version of Maeswrap: 
# devtools::install_bitbucket("remkoduursma/maeswrap")
library(Maeswrap)
library(mgcv)
library(dplyr)
library(zoo)
library(doBy)
library(data.table)
library(lubridate)
library(HIEv)
library(plyr)
library(dplyr)
library(reshape)
#prepare the folders
o <- getwd()
if(!dir.exists("download"))dir.create("download")
if(!dir.exists("output"))dir.create("output")
if(!dir.exists("cache"))dir.create("cache")
if(!dir.exists("Rings"))dir.create("Rings")
download.path <- file.path("download/")
# HIEv R package will download files to here:
setToPath(download.path)

# More functions
source("R/functions.R")
source("R/maespa_functions.R")
source("R/assign_trees.R")
source("R/assign_phy.R")
# LAI
facelai <- downloadCSV("FACE_P0037_RA_GAPFRACLAI_20121026-20150322_L2.csv")
facelai$Date <- as.Date(facelai$Date)

# list of smooth LAIs.
sm <- makesmoothLAI(facelai, how="byring", timestep="1 day")



