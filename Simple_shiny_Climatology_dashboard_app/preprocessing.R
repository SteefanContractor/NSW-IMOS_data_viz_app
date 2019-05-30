# Script to do data preprocessing for climatology app
# Author: Steefan Contractor

library(ncdf4)
library(zoo)
library(lubridate)
library(readODS)
library(tidyverse)

# working directory is the data/ dir where relative to script loc
basePath <- "./data/"

nc <- nc_open(filename = paste0(basePath,"IMOS_NSW_TZ_S19530531040000Z_PH100NRSPHB_FV02_CLIMATOLOGY_TEMP_E20181206212730Z_C20190405161810Z.nc"))
Temp_clim_mean <- ncvar_get(nc, varid = "TEMP_AVE")
Temp_clim_med <- ncvar_get(nc, varid = "TEMP_MED")
Temp_clim_std <- ncvar_get(nc, varid = "TEMP_STD")
Temp_clim_P90 <- ncvar_get(nc, varid = "TEMP_PER90")
Temp_clim_P10 <- ncvar_get(nc, varid = "TEMP_PER10")
Temp_clim_nobs <- ncvar_get(nc, varid = "NOBS")
Temp_clim_nyrs <- ncvar_get(nc, varid = "NYRS")
pressures <- ncvar_get(nc, varid = "PRES")
time <- ncvar_get(nc, varid = "TIME")
nc_close(nc)

dates <- ymd("19500101") + time
yearday <- yday(dates)

dimnames(Temp_clim_mean) <- list(paste(pressures), paste(yearday))
dimnames(Temp_clim_med) <- list(paste(pressures), paste(yearday))
dimnames(Temp_clim_std) <- list(paste(pressures), paste(yearday))
dimnames(Temp_clim_P90) <- list(paste(pressures), paste(yearday))
dimnames(Temp_clim_P10) <- list(paste(pressures), paste(yearday))
dimnames(Temp_clim_nobs) <- list(paste(pressures), paste(yearday))
dimnames(Temp_clim_nyrs) <- list(paste(pressures), paste(yearday))

########################
# load yearly data
get_fnames <- function(years) {
  # function to fetch file names with a particular year given a list of years
  fnames <- c()
  for (y in years) {
    fname <- list.files(path = paste0(basePath, "phyearlyfilesdata/"), 
                        pattern = glob2rx(paste0("*S",y,"*FV02_AVERAGE_TEMP*E",y,"*")))
    fnames <- append(fnames, fname)
  }
  return(fnames)
}

read_yearly_data <- function(fname) {
  # function to read yearly data give a vector of nc file names
  if(fname == "") stop("No file name provided")
  nc <- nc_open(filename = paste0(basePath, "phyearlyfilesdata/",
                                  fname))
  Temp_Avg <- ncvar_get(nc, varid = "TEMP_AVERAGE")
  nc_close(nc)
  dimnames(Temp_Avg) <- list(paste(pressures), paste(yearday))
  return(Temp_Avg)
}

is.anomalous <- function(temp_ts, Temp_clim_P90_ts, Temp_clim_P10_ts) {
  # function that checks yearly_data to see if temperatures are outside the 90th and 10th pc
  temp_ts > Temp_clim_P90_ts | temp_ts < Temp_clim_P10_ts
}

is.hot.anomaly <- function(temp_ts, Temp_clim_P90_ts) {
  # function that checks yearly_data to see if temperatures are outside the 90th and 10th pc
  temp_ts > Temp_clim_P90_ts
}

is.cold.anomaly <- function(temp_ts, Temp_clim_P10_ts) {
  # function that checks yearly_data to see if temperatures are outside the 90th and 10th pc
  temp_ts < Temp_clim_P10_ts
}

num.True.runLen <- function(boolTS, runLen = 2) {
  length(which(rle(boolTS)$lengths[which(rle(boolTS)$values == 1)] >= runLen))
}

num.complete.runs <- function(TS, runLen = 2) {
  boolTS <- !is.na(TS)
  num.True.runLen(boolTS = boolTS, runLen = runLen)
}

possibleRuns <- function(n, runLen = 2) {
  if(n < runLen){
    return(0)
  } else if (n == runLen) {
    return(1)
  } else {
    return(2*possibleRuns(n-1, runLen = runLen)+1)
  }
}

totalPossibleRuns <- function(TS, runLen = 2) {
  boolTS <- !is.na(TS)
  runs <- rle(boolTS)$length[which(rle(boolTS)$values == T)]
  sum(unlist(sapply(runs, FUN = possibleRuns, runLen = runLen)))
}


create.num.True.runLen.TS <- function(yearly_data, hotAnomaly = T, depth = pressures[1], runLen = 2) {
  sapply(lapply(yearly_data, FUN = function(x) {
    if (hotAnomaly) {
      is.hot.anomaly(temp_ts = x[paste(depth),], 
                     Temp_clim_P90_ts = Temp_clim_P90[paste(depth), ])
    } else {
      is.cold.anomaly(temp_ts = x[paste(depth),], 
                      Temp_clim_P10_ts = Temp_clim_P10[paste(depth), ])
    }
  }), FUN = num.True.runLen, runLen = runLen)
}

create.num.complete.runs.TS <- function(yearly_data, depth = pressures[1], runLen = 2) {
  #sapply(l
  sapply(yearly_data, FUN = function(x, RL) {
    totalPossibleRuns(x[paste(depth),], runLen = RL)#, 
    # Temp_clim_P90_ts = Temp_clim_P90[paste(depth), ], 
    # Temp_clim_P10_ts = Temp_clim_P10[paste(depth), ])
  }, RL = runLen)#, FUN = num.complete.runs, runLen = runLen)
}

years <- seq(1954,2018)
fnames <- get_fnames(years)
yearly_data <- lapply(fnames, FUN = read_yearly_data)
years <- as.numeric(substring(fnames, regexpr("*TZ_S", fnames) + 4, regexpr("*TZ_S", fnames) + 7))
names(yearly_data) <- years

# bool <- is.anomalous(yearly_data$`2015`[,1], 
#                      Temp_Clim_P90_ts = Temp_Clim_P90_grid[1,], 
#                      Temp_Clim_P10_ts = Temp_Clim_P10_grid[1,])

# Heatwaves
# runLen = 2
num.heatwaves.RL2 <- lapply(pressures, FUN = create.num.True.runLen.TS, yearly_data = yearly_data, runLen = 2, hotAnomaly = T)
num.complete.runs.TS.RL2 <- lapply(pressures, FUN = create.num.complete.runs.TS, yearly_data = yearly_data, runLen = 2)
# runLen = 3
num.heatwaves.RL3 <- lapply(pressures, FUN = create.num.True.runLen.TS, yearly_data = yearly_data, hotAnomaly = T, runLen = 3)
num.complete.runs.TS.RL3 <- lapply(pressures, FUN = create.num.complete.runs.TS, yearly_data = yearly_data, runLen = 3)
# runLen = 4
num.heatwaves.RL4 <- lapply(pressures, FUN = create.num.True.runLen.TS, yearly_data = yearly_data, hotAnomaly = T, runLen = 4)
num.complete.runs.TS.RL4 <- lapply(pressures, FUN = create.num.complete.runs.TS, yearly_data = yearly_data, runLen = 4)
# runLen = 5
num.heatwaves.RL5 <- lapply(pressures, FUN = create.num.True.runLen.TS, yearly_data = yearly_data, hotAnomaly = T, runLen = 5)
num.complete.runs.TS.RL5 <- lapply(pressures, FUN = create.num.complete.runs.TS, yearly_data = yearly_data, runLen = 5)

# Coldwaves
# runLen = 2
num.coldwaves.RL2 <- lapply(pressures, FUN = create.num.True.runLen.TS, yearly_data = yearly_data, hotAnomaly = F, runLen = 2)
# runLen = 3
num.coldwaves.RL3 <- lapply(pressures, FUN = create.num.True.runLen.TS, yearly_data = yearly_data, hotAnomaly = F, runLen = 3)
# runLen = 4
num.coldwaves.RL4 <- lapply(pressures, FUN = create.num.True.runLen.TS, yearly_data = yearly_data, hotAnomaly = F, runLen = 4)
# runLen = 5
num.coldwaves.RL5 <- lapply(pressures, FUN = create.num.True.runLen.TS, yearly_data = yearly_data, hotAnomaly = F, runLen = 5)

stationLocs <- read_ods(paste0(basePath, "NSW-IMOS_assets_2018-10-30.ods"))

rm(nc); gc()
d <- format(Sys.Date(), "%Y%m%d")
objects <- ls()
objects <- objects[!objects %in% c("basePath", "objects")]
save(list = objects, file = paste0(basePath,"prerundata_",d,".RData"))
