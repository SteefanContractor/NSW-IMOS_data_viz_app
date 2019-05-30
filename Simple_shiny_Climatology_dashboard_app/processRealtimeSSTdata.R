# Script for process realtime SST observations
# Author: Steefan Contractor

library(ncdf4)
library(lubridate)
library(maps)
library(dplyr)
library(raster) #

# working directory is where the script is
basePath <- "./"

files <- list.files(paste0(basePath,"data/SST/"), pattern = glob2rx("*.nc"))
date_times <- ymd_hms(substr(files, 1,14))
df <- data.frame(date_time = date_times, filename = files)
df <- arrange(df, desc(date_time))

# read in first nc
nc <- nc_open(paste0(basePath,"data/SST/",df$filename[1]))
lon <- ncvar_get(nc, "lon")
lat <- rev(ncvar_get(nc, "lat"))
qflag <- ncvar_get(nc, "quality_level")
sst <- ncvar_get(nc, "sea_surface_temperature")
sst[which(qflag < 4)] <- NA
nc_close(nc)

# Now systematically go through previous sst fields and fill in gaps
for (t in 6:48){
  # t = 5
  nc <- nc_open(paste0(basePath,"data/SST/",df$filename[t]))
  qflag_prev <- ncvar_get(nc, "quality_level")
  sst_prev <- ncvar_get(nc, "sea_surface_temperature")
  sst_prev[which(qflag_prev < 4)] <- NA
  nc_close(nc)
  sst[which(is.na(sst) & qflag_prev >= 4)] <- sst_prev[which(is.na(sst) & qflag_prev >= 4)]
}
# convert to raster
sst <- raster(t(sst), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
              crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# save data
save(sst, file = paste0(basePath,"data/SST/latestSST.Rdata"))

# image(lon, lat, sst)
# image(lon, lat, qflag)
# map("world", resolution = 0, add = T)
# 
# library(plotly)
# 
# sst.df <- data.frame(expand.grid(lon,lat), sst = c(sst))
# colnames(sst.df) <- c("lon", "lat", "sst")
# plot_ly(data = sst.df, x = lon, y = lat, z = t(sst), type = "heatmap")
# image(lon, lat, qflag)
