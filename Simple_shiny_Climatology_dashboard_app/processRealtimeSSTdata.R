# Script for process realtime SST observations
# Author: Steefan Contractor

library(ncdf4)
library(lubridate)
library(dplyr)
library(raster) #
library(leaflet)
library(htmltools) # not on server
library(htmlwidgets) # not on server

# working directory is where the script is
basePath <- paste0(normalizePath("./"),"/")

##############################################################################
# Read and process entire month of ocean colour and sst data
##############################################################################

#sst
files <- list.files(paste0(basePath,"data/SST/"), pattern = glob2rx("*.nc"))
dates <- ymd(substr(files, 1,8))
df <- data.frame(date = dates, filename = files)
df <- arrange(df, desc(date))

# sst climatology
# 90th percentile
clim_90 <- brick("./data/SSTAARS_NSW_remapcon2.nc", varname = "TEMP_90th_perc")
clim_90 <- setZ(clim_90, z = ymd(strsplit(system("cdo showdate data/SSTAARS_NSW_remapcon2.nc", intern = T), split = "  ")[[1]][-1]))
clim.index <- which.min(abs(yday(format(df[1,'date'], format = "%y-%m-%d")) - yday(getZ(clim_90))))
# 10th percentile
clim_10 <- brick("./data/SSTAARS_NSW_remapcon2.nc", varname = "TEMP_10th_perc")
clim_10 <- setZ(clim_10, z = ymd(strsplit(system("cdo showdate data/SSTAARS_NSW_remapcon2.nc", intern = T), split = "  ")[[1]][-1]))

#oc
files <- list.files(paste0(basePath,"data/CHL_OC3/"), pattern = glob2rx("*.nc"))
dates <- ymd(substr(files, 7,7+8-1))
df_OC <- data.frame(date = dates, OC_filename = files)
df_OC <- arrange(df_OC, desc(date))

# merge with SST dataframe
df <- base::merge(df, df_OC, by = "date", all.x = T)

# Read and process every file in df
sst_month <- sapply(paste(df$date),function(x) NULL)
sst_10_month <- sapply(paste(df$date),function(x) NULL)
sst_90_month <- sapply(paste(df$date),function(x) NULL)
oc_month <- sapply(paste(df$date),function(x) NULL)

for (d in 1:nrow(df)) {
  # sst
  nc <- nc_open(paste0(basePath,"data/SST/",df$filename[d]))
  lon <- ncvar_get(nc, "lon")
  lat <- rev(ncvar_get(nc, "lat"))
  sst <- ncvar_get(nc, "analysed_sst")
  nc_close(nc)
  
  # convert to raster
  sst <- sst - 273.15
  sst <- raster(t(sst[,length(lat):1]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
                crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  sst_90 <- sst
  sst_90[which(sst_90[] < clim_90[[clim.index]][])] <- NA
  sst_10 <- sst
  sst_10[which(sst_10[] > clim_10[[clim.index]][])] <- NA
  
  sst_month[[d]] <- sst
  sst_10_month[[d]] <- sst_10
  sst_90_month[[d]] <- sst_90
  
  # chl_oc3
  if (!is.na(df$OC_filename[d])) {
    nc <- nc_open(paste0(basePath,"data/CHL_OC3/",df$OC_filename[d]))
    dimnames <- names(nc$dim)
    lon <- ncvar_get(nc, ifelse("longitude" %in% dimnames, "longitude", "lon"))
    lat <- rev(ncvar_get(nc, ifelse("latitude" %in% dimnames, "latitude", "lat")))
    chl_oc3 <- ncvar_get(nc, "chl_oc3")
    nc_close(nc)
    
    chl_oc3 <- raster(t(chl_oc3[,length(lat):1]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
                      crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    # Mask  values above 10
    chl_oc3[which(values(chl_oc3) > 10)] <- NA
    # Set values between >5 and 10 as 5
    chl_oc3[which(values(chl_oc3) > 5)] <- 5
    
    oc_month[[d]] <- chl_oc3
  } else {
    oc_month[[d]] <- NA
  }
}

# save data
save(sst_month, sst_10_month, sst_90_month, oc_month, clim_90, clim_10, df, file = paste0(basePath,"data/SST/processedSSTandOC.Rdata"))

##########################
# HF Radar data
##########################

UVCart_month <- sapply(paste(df$date),function(x) NULL)

for (d in 1:nrow(df)) {
  date <- format(df$date[d], format = "%Y%m%d")
  fnames <- list.files(paste0(basePath, "data/HFRadar/NEWC"), pattern = glob2rx(paste0("*",date,"*.nc")))
  
  if (length(fnames) > 2) {
    # first hour (latest hour minus 2)
    nc <- nc_open(paste0(basePath, "data/HFRadar/NEWC/", fnames[1]))
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
    w = 0.2 # scaling factor for arrows
    uv_cart_df_newc <- data.frame(lon0 = lonlat[,1], lat0 = lonlat[,2], lon1 = lonlat[,1]+c(ucur_newc)*w, lat1 = lonlat[,2]+c(vcur_newc)*w)
    uv_cart_df_newc <- uv_cart_df_newc %>% filter(!is.na(lon1))
    
    # second hour (latest hour minus 1)
    nc <- nc_open(paste0(basePath, "data/HFRadar/NEWC/", fnames[2]))
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
    # w = 1 # scaling factor for arrows
    uv_cart_df <- data.frame(lon0 = lonlat[,1], lat0 = lonlat[,2], ucur = c(ucur_newc)*w, vcur = c(vcur_newc)*w)
    uv_cart_df <- uv_cart_df %>% filter(!is.na(ucur))
    
    uv_cart_df_newc <- merge(uv_cart_df_newc, uv_cart_df, by=c("lon0", "lat0"))
    uv_cart_df_newc <- uv_cart_df_newc %>% mutate(lon2 = lon1 + ucur, lat2 = lat1 + vcur) %>% dplyr::select(-ucur, -vcur)
    
    # latest hour
    nc <- nc_open(paste0(basePath, "data/HFRadar/NEWC/", fnames[3]))
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
    # w = 1 # scaling factor for arrows
    uv_cart_df <- data.frame(lon0 = lonlat[,1], lat0 = lonlat[,2], ucur = c(ucur_newc)*w, vcur = c(vcur_newc)*w)
    uv_cart_df <- uv_cart_df %>% filter(!is.na(ucur))
    
    uv_cart_df_newc <- merge(uv_cart_df_newc, uv_cart_df, by=c("lon0", "lat0"))
    uv_cart_df_newc <- uv_cart_df_newc %>% mutate(lon3 = lon2 + ucur, lat3 = lat2 + vcur) %>% dplyr::select(-ucur, -vcur)
  } else {
    ucur_newc <- c()
    vcur_newc <- c()
    uv_cart_df_newc <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("lon0", "lat0", "lon1", "lat1", "lon2", "lat2","lon3", "lat3"))
  }
  # save(ucur_newc, vcur_newc, uv_cart_df_newc, file = paste0(basePath, "data/HFRadar/NEWC/NEWC_HFRadar.RData"))

  # COFH
  fnames <- list.files(paste0(basePath, "data/HFRadar/COFH"), pattern = glob2rx(paste0("*",date,"*.nc")))
  
  if (length(fnames) > 2) {
    # first hour (latest hour minus 2)
    nc <- nc_open(paste0(basePath, "data/HFRadar/COFH/", fnames[1]))
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
    w = 0.2 # scaling factor for arrows
    uv_cart_df_cofh <- data.frame(lon0 = lonlat[,1], lat0 = lonlat[,2], lon1 = lonlat[,1]+c(ucur_cofh)*w, lat1 = lonlat[,2]+c(vcur_cofh)*w)
    uv_cart_df_cofh <- uv_cart_df_cofh %>% filter(!is.na(lon1))
    
    # second hour (latest hour minus 1)
    nc <- nc_open(paste0(basePath, "data/HFRadar/COFH/", fnames[2]))
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
    # w = 0.11 # scaling factor for arrows
    uv_cart_df <- data.frame(lon0 = lonlat[,1], lat0 = lonlat[,2], ucur = c(ucur_cofh)*w, vcur = c(vcur_cofh)*w)
    uv_cart_df <- uv_cart_df %>% filter(!is.na(ucur))
    
    uv_cart_df_cofh <- merge(uv_cart_df_cofh, uv_cart_df, by=c("lon0", "lat0"))
    uv_cart_df_cofh <- uv_cart_df_cofh %>% mutate(lon2 = lon1 + ucur, lat2 = lat1 + vcur) %>% dplyr::select(-ucur, -vcur)
    
    # latest hour
    nc <- nc_open(paste0(basePath, "data/HFRadar/COFH/", fnames[3]))
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
    # w = 0.11 # scaling factor for arrows
    uv_cart_df <- data.frame(lon0 = lonlat[,1], lat0 = lonlat[,2], ucur = c(ucur_cofh)*w, vcur = c(vcur_cofh)*w)
    uv_cart_df <- uv_cart_df %>% filter(!is.na(ucur))
    
    uv_cart_df_cofh <- merge(uv_cart_df_cofh, uv_cart_df, by=c("lon0", "lat0"))
    uv_cart_df_cofh <- uv_cart_df_cofh %>% mutate(lon3 = lon2 + ucur, lat3 = lat2 + vcur) %>% dplyr::select(-ucur, -vcur)
  } else {
    ucur_cofh <- c()
    vcur_cofh <- c()
    uv_cart_df_cofh <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("lon0", "lat0", "lon1", "lat1"))
  }

  ucur <- c(ucur_newc, ucur_cofh)
  vcur <- c(vcur_newc, vcur_cofh)
  uv_cart_df <- rbind(uv_cart_df_newc, uv_cart_df_cofh)
  
  UVCart_month[[d]] <- uv_cart_df
}
save(UVCart_month, file = paste0(basePath, "data/HFRadar/HFRadar.RData"))

##################################
# save leaflet map on home page as html widget
##################################

# create leaflet map

# n <- 27
# date <- df$date[n]
# sst <- sst_month[[n]]
# sst_10 <- sst_10_month[[n]]
# sst_90 <- sst_90_month[[n]]
# oc <- oc_month[[n]]
# 
# # determine colourmapping for sst raster image
# pal <- colorNumeric(
#   palette = "magma",
#   domain = values(sst),
#   na.color = "#00000000",
#   reverse = F)
# 
# # determine colourmapping for oc raster image
# palOC <- colorNumeric(
#   palette = "viridis",
#   domain = log(values(oc)),
#   na.color = "transparent",
#   reverse = F)
# 
# # load javascript plugin 
# curveplugin <- htmlDependency("leaflet.curve", "0.5.2",
#                               src = file.path(normalizePath(basePath),"www"),
#                               script = "leaflet.curve.js")
# 
# # A function that takes a plugin htmlDependency object and adds
# # it to the map. This ensures that however or whenever the map
# # gets rendered, the plugin will be loaded into the browser.
# registerPlugin <- function(map, plugin) {
#   map$dependencies <- c(map$dependencies, list(plugin))
#   map
# }
# 
# m <- leaflet() %>% addTiles() #%>% setView(lng = 153.4, lat = -30.5, zoom = 9)
# 
#  m %>% addRasterImage(x = sst, colors = pal, group = "SST",opacity = 0.8) %>%
#   addLegend(pal = pal, values = values(sst), opacity = 0.7, #labFormat = labelFormat(transform = function(x) {sort(x, decreasing = T)}),
#             title = "Surface temp", group = c("SST"), position = "topleft") %>% #, labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
#   addRasterImage(x = sst_10, colors = pal, group = "Cold SSTs", opacity = 0.8) %>%
#   addRasterImage(x = sst_90, colors = pal, group = "Warm SSTs", opacity = 0.8) %>%
#   addLabelOnlyMarkers(lng = 151.4, lat = -27.9, label = HTML(paste("Date:<br>",date)),
#                       labelOptions = labelOptions(noHide = T, direction = "bottom", textsize = "15px")) %>%
#   addRasterImage(x = log(oc), colors = palOC, group = "Ocean Colour", opacity = 0.8) %>%
#   addLegend(pal = palOC, values = rev(log(values(oc))), labFormat = labelFormat(transform = exp), opacity = 0.7,
#             title = "Ocean colour \n(Chl-a)", group = "Ocean Colour", position = "topleft",) %>% 
#   # addMarkers(data = stationLocs %>% filter(site_code == "CH100"), lat = ~avg_lat, lng = ~avg_lon,
#   #                                       label = HTML(paste(sep = "<br/>", stationLocs %>% dplyr::filter(site_code == "CH100") %>% dplyr::select(site_code), paste(round(rTemps[1],1), "degrees"))),
#   #                                       labelOptions = labelOptions(noHide = T, direction = "bottom", textsize = "15px",
#   #                                                                   style = list("background-color" = rBG[1])),
#   #                                       group = "Moorings") %>%
#   # addMarkers(data = stationLocs %>% dplyr::filter(site_code == "SYD100"), lat = ~avg_lat, lng = ~avg_lon,
#   #            label = HTML(paste(sep = "<br/>", stationLocs %>% filter(site_code == "SYD100") %>% dplyr::select(site_code), paste(round(rTemps[2],1), "degrees"))),
#   #            labelOptions = labelOptions(noHide = T, direction = "right", textsize = "15px",
#   #                                        style = list("background-color" = rBG[2])),
#   #            group = "Moorings") %>%
#   # addMarkers(data = stationLocs %>% dplyr::filter(site_code == "PH100"), lat = ~avg_lat, lng = ~avg_lon,
# #            label = HTML(paste(sep = "<br/>",
# #                               a(paste(stationLocs %>% dplyr::filter(site_code == "PH100") %>% dplyr::select(site_code)), onclick = "openTab('PH100_Clim')", href="#"),
# #                               paste(round(rTemps[3],1), "degrees"))),
# #            labelOptions = labelOptions(noHide = T, direction = "bottom", textsize = "15px",
# #                                        style = list("background-color" = rBG[3],
# #                                                     "pointer-events" = "auto")),
# #            group = "Moorings") %>%
# # addMarkers(data = stationLocs %>% dplyr::filter(site_code == "BMP120"), lat = ~avg_lat, lng = ~avg_lon,
# #            label = HTML(paste(sep = "<br/>", stationLocs %>% dplyr::filter(site_code == "BMP120") %>% dplyr::select(site_code), paste(round(rTemps[4],1), "degrees"))),
# #            labelOptions = labelOptions(noHide = T, direction = "bottom", textsize = "15px",
# #                                        style = list("background-color" = rBG[4])),
# #            group = "Moorings") %>%
# #
# # Layers control
# addLayersControl(
#   baseGroups = c("SST", "Cold SSTs", "Warm SSTs", "Ocean Colour"),
#   # overlayGroups = c("SST", "Ocean Colour"),
#   options = layersControlOptions(collapsed = FALSE, autoZIndex = T),
#   position = "topleft"
# )# %>% addFlows(uv_cart_df$lon0, uv_cart_df$lat0, uv_cart_df$lon1, uv_cart_df$lat1, maxThickness = 0.5)
# 
# m <- m %>% # Register ESRI plugin on this map instance
#   registerPlugin(curveplugin) %>%
#   # Add your custom JS logic here. The `this` keyword
#   # refers to the Leaflet (JS) map object.
#   onRender(paste("function(el, x) {",
#                  paste0("L.curve(['M', [", uv_cart_df$lat0, ",", uv_cart_df$lon0, 
#                         "], 'C', [", uv_cart_df$lat1, ",", uv_cart_df$lon1, "], [", 
#                         uv_cart_df$lat2, ",", uv_cart_df$lon2[], "], [",
#                         uv_cart_df$lat3, ",", uv_cart_df$lon3[], "]], ",
#                         "{weight: 0.5, color: 'white', animate: {duration: 1500, iterations: Infinity}}).addTo(this);", sep = " ", collapse = "\n"),
#                  "}",sep = "\n"))
# 
# # save leaflet map as html widget
# system("if [ ! -d www/figures ]; then mkdir www/figures; fi")
# # system("if [ ! -d www/figures/libdir ]; then mkdir www/figures/libdir; fi")
# f <- "www/figures/home_leaflet_map.html"
# saveWidget(m, file=file.path(normalizePath(dirname(f)),basename(f)), libdir = "libdir",
#            selfcontained = T)

