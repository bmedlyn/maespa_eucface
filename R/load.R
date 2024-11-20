# Have to have RTools installed
# http://cran.r-project.org/bin/windows/Rtools/
# Newest version of Maeswrap: 
# devtools::install_bitbucket("remkoduursma/maeswrap")

# Libraries
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
library(plantecophys)
# library(XLConnect) # will need if on Windows

# set token for hiev
if (file.exists("c:/hiev/token.txt")){
  setToken()
} else if (file.exists("~/Dropbox/Repos/Eucface Repos/eucface_environ/token.txt")){
  setToken(tokenfile="~/Dropbox/Repos/Eucface Repos/eucface_environ/token.txt")
} else {
  stop(c("Token needs to be in c:/hiev/token.txt"))
}

#prepare the folders
o <- getwd()
if(!dir.exists("download"))dir.create("download")
if(!dir.exists("output"))dir.create("output")
if(!dir.exists("cache"))dir.create("cache")
if(!dir.exists("Rings"))dir.create("Rings")
if(!dir.exists("output/elevated"))dir.create("output/elevated")
if(!dir.exists("output/ambient"))dir.create("output/ambient")

# These are directories that Jim used for various scenarios
# if(!dir.exists("output/maespa"))dir.create("output/maespa")
# if(!dir.exists("output/leuning"))dir.create("output/leuning")
# if(!dir.exists("output/maespaVPD"))dir.create("output/maespaVPD")
# if(!dir.exists("output/maestra"))dir.create("output/maestra")
# if(!dir.exists("output/maestraVPD"))dir.create("output/maestraVPD")
# if(!dir.exists("output/maestraVPDVJ"))dir.create("output/maestraVPDVJ")
# if(!dir.exists("output/maestraVJ"))dir.create("output/maestraVJ")
# if(!dir.exists("output/accli"))dir.create("output/accli")

# HIEv R package will download files to here:
download.path <- file.path("download/")
setToPath(download.path)

# Load files with functions
source("R/functions.R")
source("R/maespa_functions.R")
source("R/assign_trees.R")
source("R/assign_phy.R")

# LAI
# get lai from hiev
facelai <- downloadTOA5("FACE_P0037_RA_GAPFRACLAI_OPEN_L2.dat")
# names(facelai) <- c("TIMESTAMP","Ring","Date","Gapfraction.mean",
#                    "Rain_mm_Tot.mean","Gapfraction.sd","Rain_mm_Tot.sd",
#                    "Gapfraction.n","Rain_mm_Tot.n","treatment","maxSDring","LAI")


facelai$Date <- as.Date(facelai$Date)

# list of smooth LAIs.
sm <- makesmoothLAI(facelai, how="byring", timestep="1 day")

# get data from Hiev
downloadCSV(("FACE_P0064_RA_GASEXCHANGE-RdarkT_20160215-L1.csv"),topath = "download/")
downloadHIEv(searchHIEv("EucFACE Fine root HIEv"),topath = "download/")
downloadCSV(("FACE_P0045_RA_TREE_HEIGHT-DIAMETER_ RAW_082016_V1.csv"),topath = "download/euc data/")
# get data from Gimeno 2016
teresa.url <- paste0("http://research-data.westernsydney.edu.au/",
                     "redbox/verNum1.9/published/detail/",
                     "d879c312dcb2b23571b1dccdedb87c86/",
                     "Gimeno_spots_Eter_EucFACE.zip?preview=true")
# curl::curl_download(teresa.url,"download/Gimeno_spot.zip")

