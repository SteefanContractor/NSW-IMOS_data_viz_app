# Script for process realtime SST observations
# Author: Steefan Contractor

library(ncdf4)
library(lubridate)
library(maps)

# change local to True when developing locally
local = system("uname -n", intern = T) == "matht250"#T

if (local) {
  basePath <- "~/Documents/GIT_REPOS/NSW-IMOS_data_viz_app/data/"#"~/ownCloud/Working_Directory/Postdoc-SchoolOfMathsStats/Scripts/Simple_shiny_Climatology_dashboard_app/data/"
  #"~/sci-maths-ocean/shared/PEOPLE/Steefan/climatology/data/"
} else {
  basePath <- "./data/"
}

files <- list.files(basePath, pattern = glob2rx("*.nc"))
date_times <- ymd_hms(substr(files, 1,14))
df <- data.frame(date_time = date_times, filename = files)
df <- arrange(df, desc(date_time))

# read in first nc
nc <- nc_open(paste0(basePath,df$filename[1]))
lon <- ncvar_get(nc, "lon")
lat <- rev(ncvar_get(nc, "lat"))
qflag <- ncvar_get(nc, "quality_level")
qflag <- qflag[,length(lat):1]
sst <- ncvar_get(nc, "sea_surface_temperature")
sst <- sst[,dim(sst)[2]:1]
sst[which(qflag < 4)] <- NA
qflag[which(qflag < 4)] <- NA
nc_close(nc)

# Now systematically go through previous sst fields and fill in gaps
for (t in 2:48){
  nc <- nc_open(paste0(basePath,df$filename[t]))
  qflag_prev <- ncvar_get(nc, "quality_level")
  qflag_prev <- qflag[,length(lat):1]
  sst_prev <- ncvar_get(nc, "sea_surface_temperature")
  sst_prev <- sst_prev[,dim(sst)[2]:1]
  sst_prev[which(qflag_prev < 4)] <- NA
  # qflag_prev[which(qflag_prev < 4)] <- NA
  nc_close(nc)
  sst[which(is.na(sst) & qflag_prev >= 4)] <- sst_prev[which(is.na(sst) & qflag_prev >= 4)]
  qflag[which(is.na(sst) & qflag_prev >= 4)] <- qflag_prev[which(is.na(sst) & qflag_prev >= 4)]
}
qflag[is.na(sst)] <- NA
save(lon, lat, sst, qflag, file = paste0(basePath,"latestSST.Rdata"))


# image(lon, lat, sst)
# image(lon, lat, qflag)
# map("world", resolution = 0, add = T)
# 
# library(plotly)
# 
# sst.df <- data.frame(expand.grid(lon,lat), sst = c(sst))
# colnames(sst.df) <- c("lon", "lat", "sst")
# plot_ly(data = sst.df, x = lon, y = lat, z = t(sst), type = "contour")
# image(lon, lat, qflag)
