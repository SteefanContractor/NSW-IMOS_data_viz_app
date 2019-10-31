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
clim_90 <- brick("./data/SSTAARS_nsw_narrow.nc", varname = "TEMP_90th_perc")
clim_90 <- setZ(clim_90, z = ymd(strsplit(system("cdo showdate data/SSTAARS_nsw_narrow.nc", intern = T), split = "  ")[[1]][-1]))
clim.index <- which.min(abs(yday(format(df[1,'date_time'], format = "%y-%m-%d")) - yday(getZ(clim_90))))
# 10th percentile
clim_10 <- brick("./data/SSTAARS_nsw_narrow.nc", varname = "TEMP_10th_perc")
clim_10 <- setZ(clim_10, z = ymd(strsplit(system("cdo showdate data/SSTAARS_nsw_narrow.nc", intern = T), split = "  ")[[1]][-1]))

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

# NEWC
fname <- list.files(paste0(basePath, "data/HFRadar/NEWC"), pattern = glob2rx("*.nc"))

if (length(fname) > 0) {
  nc <- nc_open(paste0(basePath, "data/HFRadar/NEWC/", fname))
  ucur_newc <- ncvar_get(nc, "UCUR")
  vcur_newc <- ncvar_get(nc, "VCUR")
  lat <- ncvar_get(nc, "LATITUDE")
  lon <- ncvar_get(nc, "LONGITUDE")
  ucur_qc <- ncvar_get(nc, "UCUR_quality_control")
  vcur_qc <- ncvar_get(nc, "VCUR_quality_control")
  nc_close(nc)
  
  lat <- apply(lat, 2, mean)
  lon <- apply(lon, 1, mean)
  ucur_newc[which(!(ucur_qc %in% c(1,2) & vcur_qc %in% c(1,2) & !is.na(vcur_newc)))] <- NA
  vcur_newc[which(!(ucur_qc %in% c(1,2) & vcur_qc %in% c(1,2) & !is.na(ucur_newc)))] <- NA
  
  lonlat <- expand.grid(lon, lat)
  w = 0.1 # scaling factor for arrows
  uv_cart_df_newc <- data.frame(lon0 = lonlat[,1], lat0 = lonlat[,2], lon1 = lonlat[,1]+c(ucur_newc)*w, lat1 = lonlat[,2]+c(vcur_newc)*w)
  uv_cart_df_newc <- uv_cart_df_newc %>% filter(!is.na(lon1))
} else {
  ucur_newc <- c()
  vcur_newc <- c()
  uv_cart_df_newc <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("lon0", "lat0", "lon1", "lat1"))
}
# save(ucur_newc, vcur_newc, uv_cart_df_newc, file = paste0(basePath, "data/HFRadar/NEWC/NEWC_HFRadar.RData"))

# COFH
fname <- list.files(paste0(basePath, "data/HFRadar/COFH"), pattern = glob2rx("*.nc"))

if (length(fname) > 0) {
  nc <- nc_open(paste0(basePath, "data/HFRadar/COFH/", fname))
  ucur_cofh <- ncvar_get(nc, "UCUR")
  vcur_cofh <- ncvar_get(nc, "VCUR")
  lat <- ncvar_get(nc, "LATITUDE")
  lon <- ncvar_get(nc, "LONGITUDE")
  ucur_qc <- ncvar_get(nc, "UCUR_quality_control")
  vcur_qc <- ncvar_get(nc, "VCUR_quality_control")
  nc_close(nc)
  
  # lat <- apply(lat, 2, mean)
  # lon <- apply(lon, 1, mean)
  ucur_cofh[which(!(ucur_qc %in% c(1) & vcur_qc %in% c(1) & !is.na(vcur_cofh)))] <- NA
  vcur_cofh[which(!(ucur_qc %in% c(1) & vcur_qc %in% c(1) & !is.na(ucur_cofh)))] <- NA
  
  lonlat <- expand.grid(lon, lat)
  w = 0.1 # scaling factor for arrows
  uv_cart_df_cofh <- data.frame(lon0 = lonlat[,1], lat0 = lonlat[,2], lon1 = lonlat[,1]+c(ucur_cofh)*w, lat1 = lonlat[,2]+c(vcur_cofh)*w)
  uv_cart_df_cofh <- uv_cart_df_cofh %>% filter(!is.na(lon1))
} else {
  ucur_cofh <- c()
  vcur_cofh <- c()
  uv_cart_df_cofh <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("lon0", "lat0", "lon1", "lat1"))
}

ucur <- c(ucur_newc, ucur_cofh)
vcur <- c(vcur_newc, vcur_cofh)
uv_cart_df <- rbind(uv_cart_df_newc, uv_cart_df_cofh)
save(ucur, vcur, uv_cart_df, file = paste0(basePath, "data/HFRadar/HFRadar.RData"))

#####################
# CHL_OC3 data
#####################

# Get list of downloaded files
fname <- list.files(paste0(basePath, "data/CHL_OC3"), pattern = glob2rx("*.nc"))

# Get the latest file
fname <- sort(fname, decreasing = T)[1]

# Open and read file
nc <- nc_open(paste0(basePath, "data/CHL_OC3/", fname))
chl_oc3 <- raster(paste0(basePath, "./data/CHL_OC3/", fname), varname = "chl_oc3")
dec2bin <- function(x) paste(as.integer(rev(intToBits(x))), collapse = "")
save(chl_oc3, file = paste0(basePath, "data/CHL_OC3/CHL_OC3.RData"))

#################################
# Save ggplotly plots as widgets
#################################
library(ggplot2)
library(htmlwidgets)
library(plotly)

p <- ggplot() +
  geom_raster(data = sst_df, aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis_c(option="A")+
  coord_quickmap()
gp <- ggplotly(p)

saveWidget(gp, file="homemap_ggplotly_SSTonly.html", 
           libdir = paste0(basePath, "homelibdir"),
           selfcontained = F)

p <- ggplot() +
  geom_raster(data = sst_10_df, aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis_c(option="A")+
  coord_quickmap()
gp <- ggplotly(p)

saveWidget(gp, file="homemap_ggplotly_coldSSTs.html", 
           libdir = paste0(basePath, "homelibdir"),
           selfcontained = F)

p <- ggplot() +
  geom_raster(data = sst_90_df, aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis_c(option="A")+
  coord_quickmap()
gp <- ggplotly(p)

saveWidget(gp, file="homemap_ggplotly_warmSSTs.html", 
           libdir = paste0(basePath, "homelibdir"),
           selfcontained = F)

p2 <- ggplot() +
  geom_raster(data = chl_oc3_df, aes(x=x, y=y, fill=Chlorophyll.Concentration..OC3.Algorithm)) +
  scale_fill_viridis_c(name="Chlorophyll-a OC3", breaks = c(0.1, 0.5, 1, 5, 10, 100, 150), trans="log")+
  coord_quickmap()
gp2 <- ggplotly(p2)

saveWidget(gp2, file="homemap_ggplotly_CHL-OC3.html", 
           libdir = paste0(basePath, "homelibdir"),
           selfcontained = F)

system("rm -r www/figures/homemap*.html www/figures/homelibdir")
system("mv *.html homelibdir www/figures/")
