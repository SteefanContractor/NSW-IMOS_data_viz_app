#!/bin/bash

# Get today's day, month, and year
d=$(( `date +%d` - 3 ))
d=`printf "%02d" $d`
m=`date +%m`
y=`date +%Y`

echo Downloading sst data for $d-$m-$y

#wget http://thredds.aodn.org.au/thredds/fileServer/IMOS/SRS/OC/gridded/aqua/P1D/"$y"/"$m"/A.P1D."$y$m$d"T000000Z.aust.sst.nc -O data/A.P1D."$y$m$d"T000000Z.aust.sst.nc
cdo sellonlatbox,150,159,-37,-28 data/A.P1D."$y$m$d"T000000Z.aust.sst.nc data/A.P1D."$y$m$d"T000000Z.aust.sst.NSW.nc
#wget http://thredds.aodn.org.au/thredds/fileServer/IMOS/SRS/OC/gridded/aqua/P1D/"$y"/"$m"/A.P1D."$y$m$d"T000000Z.aust.sst_quality.nc -O data/A.P1D."$y$m$d"T000000Z.aust.sst_quality.nc
cdo sellonlatbox,150,159,-37,-28 data/A.P1D."$y$m$d"T000000Z.aust.sst_quality.nc data/A.P1D."$y$m$d"T000000Z.aust.sst_quality.NSW.nc
