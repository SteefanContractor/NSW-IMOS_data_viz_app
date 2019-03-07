# A script for pre processing data for the NSW-IMOS data viz app

# Exploratory script for climatologies

library(ncdf4)
library(zoo)
library(lubridate)
library(readODS)
library(maps)

# change local to True when developing locally
local = system("uname -n", intern = T) == "matht250"#T

if (local) {
  basePath <- "~/Documents/GIT_REPOS/NSW-IMOS_data_viz_app/Simple_shiny_Climatology_dashboard_app/"#"~/ownCloud/Working_Directory/Postdoc-SchoolOfMathsStats/Scripts/Simple_shiny_Climatology_dashboard_app/data/"
              #"~/sci-maths-ocean/shared/PEOPLE/Steefan/climatology/data/"
} else {
  basePath <- "./"
}

setwd(basePath)

filename <- "A.P1D.20190304T000000Z.aust.sst.NSW.nc"
nc <- nc_open(paste0("./data/", filename))
sst_latest <- ncvar_get(nc, varid = "sst")
sst_latest <- sst_latest[,dim(sst_latest)[2]:1]
lon <- ncvar_get(nc, varid = "longitude")
lat <- rev(ncvar_get(nc, varid = "latitude"))
nc_close(nc)

image(lon, lat, sst_latest)
map("world", "Australia", add = T)

# read in exact coordinates for the moorings
stationLocs <- read_ods("./data/NSW-IMOS_assets_2018-10-30.ods")
colnames(stationLocs) <- c("site_code", "avg_lat", "avg_lon")

library(tidyverse)

stationLocs <- stationLocs %>% mutate(grid_lat_index = sapply(avg_lat, FUN = function(x) {which.min(abs(x - lat))}),
                                      grid_lon_index = sapply(avg_lon, FUN = function(x) {which.min(abs(x - lon))})) 

avgBox <- function(dataArray, indexLon, indexLat, numUnits, maxLon = length(lon), maxLat = length(lat)) {
  # function to average values over a square region around a given index of a 2D array
  # dataArray: 2D array being averaged
  # indexLon: first index of location specifying center of square
  # indexLat: second index of location specifying center of square
  # numUnits: Distance (units) of square boundary from the center (half of the side length)
  firstLon = max(indexLon - numUnits, 1)
  lastLon = min(indexLon + numUnits, maxLon)
  firstLat = max(indexLat - numUnits, 1)
  lastLat = min(indexLat + numUnits, maxLat)
  return(mean(dataArray[firstLon:lastLon, firstLat:lastLat], na.rm = T))
}

stationLocs <- stationLocs %>% mutate(avgSST = sapply(1:10, FUN = function(n) {
  avgBox(sst_latest, grid_lon_index[n], grid_lat_index[n], numUnits = 8)})
)

# Get date from file name
date <- unlist(strsplit(unlist(strsplit(filename, "[.]"))[3], "[T]"))[1]

save(stationLocs, file = paste0("./data/avgSST_",date,".RData"))
