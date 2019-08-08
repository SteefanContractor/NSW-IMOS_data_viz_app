# Script for process realtime SST observations
# Author: Steefan Contractor

library(ncdf4)
library(lubridate)
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
sst <- sst - 273.15
sst <- raster(t(sst), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
              crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# 90th percentile
clim_90 <- brick("./data/SSTAARS_nsw.nc", varname = "TEMP_90th_perc")
clim_90 <- setZ(clim_90, z = ymd(strsplit(system("cdo showdate data/SSTAARS_nsw.nc", intern = T), split = "  ")[[1]][-1]))
clim.index <- which.min(abs(yday(format(df[1,'date_time'], format = "%y-%m-%d")) - yday(getZ(clim_90))))
# 10th percentile
clim_10 <- brick("./data/SSTAARS_nsw.nc", varname = "TEMP_10th_perc")
clim_10 <- setZ(clim_10, z = ymd(strsplit(system("cdo showdate data/SSTAARS_nsw.nc", intern = T), split = "  ")[[1]][-1]))

sst_90 <- sst
sst_90[which(sst_90[] < clim_90[[clim.index]][])] <- NA
sst_10 <- sst
sst_10[which(sst_10[] > clim_10[[clim.index]][])] <- NA

sst_normal <- sst
sst_normal[which(sst_normal[] > clim_90[[clim.index]][])] <- NA
sst_normal[which(sst_normal[] < clim_10[[clim.index]][])] <- NA

# save data
save(sst, sst_10, sst_90, sst_normal, clim_90, clim_10, df, file = paste0(basePath,"data/SST/latestSST.Rdata"))

##########################
# HF Radar data
##########################

fname <- list.files(paste0(basePath, "data/HFRadar/NEWC"), pattern = glob2rx("*.nc"))

nc <- nc_open(paste0(basePath, "data/HFRadar/NEWC/", fname))
ucur <- ncvar_get(nc, "UCUR")
vcur <- ncvar_get(nc, "VCUR")
lat <- ncvar_get(nc, "LATITUDE")
lon <- ncvar_get(nc, "LONGITUDE")
ucur_qc <- ncvar_get(nc, "UCUR_quality_control")
vcur_qc <- ncvar_get(nc, "VCUR_quality_control")
nc_close(nc)

lat <- apply(lat, 2, mean)
lon <- apply(lon, 1, mean)
ucur[which(!ucur_qc %in% c(1,2) & !vcur_qc %in% c(1,2) & is.na(vcur))] <- NA
vcur[which(!ucur_qc %in% c(1,2) & !vcur_qc %in% c(1,2) & is.na(ucur))] <- NA

lonlat <- expand.grid(lon, lat)
w = 0.5 # scaling factor for arrows
uv_cart_df <- data.frame(lon0 = lonlat[,1], lat0 = lonlat[,2], lon1 = lonlat[,1]+c(ucur)*w, lat1 = lonlat[,2]+c(vcur)*w)
uv_cart_df <- uv_cart_df %>% filter(!is.na(lon1))
save(ucur, vcur, uv_cart_df, file = paste0(basePath, "data/HFRadar/NEWC/NEWC_HFRadar.RData"))

