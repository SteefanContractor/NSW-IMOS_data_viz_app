#!/bin/bash

if [ ! -d data/SST ]; then mkdir data/SST; fi
cd data/SST
# a file to distinguish old from new files
touch lastwatch

# delete old files
if [ -f fnames.txt ]; then rm fnames.txt; fi
if [ -f fnames_lastyear.txt ]; then rm fnames_lastyear.txt; fi
thisyear=`date -u +%Y`
lastyear=$(( thisyear - 1 ))

# Get the names of latest 28 files of daily data from RAMSSA
wget -q -O - http://thredds.aodn.org.au/thredds/catalog/IMOS/SRS/SST/ghrsst/L4/RAMSSA/"$thisyear"/catalog.html | grep -oP  "(?<=<tt>).*.nc(?=</tt>)" | tail -n 28 >> fnames.txt

# Download files in fnames.txt
for f in `cat fnames.txt`; do
	wget -q -nc -A.nc http://thredds.aodn.org.au/thredds/fileServer/IMOS/SRS/SST/ghrsst/L4/RAMSSA/"$thisyear"/"$f"
done

# If there aren't 28 files in the current year then get the remaining files from last year
totfiles=`cat fnames.txt | wc -l`
if [ $totfiles -lt 28 ]; then
	remainfiles=$(( 28 - $totfiles ))		
	wget -q -O - http://thredds.aodn.org.au/thredds/catalog/IMOS/SRS/SST/ghrsst/L4/RAMSSA/"$lastyear"/catalog.html | grep -oP  "(?<=<tt>).*.nc(?=</tt>)" | tail -n "$remainfiles" > fnames_lastyear.txt
	for f in `cat fnames_lastyear.txt`; do
        	wget -q -nc -A.nc http://thredds.aodn.org.au/thredds/fileServer/IMOS/SRS/SST/ghrsst/L4/RAMSSA/"$lastyear"/"$f"
	done
	cat fnames_lastyear.txt >> fnames.txt
fi

# Remove old files or nc files not in fnames.txt
rm `ls *.nc | grep -v -f fnames.txt`

# file with list of new nc files
find -cnewer lastwatch -name "*.nc" -exec basename {} \; > newfiles.txt

# Use CDO to subset to the region of interest
if [ -s newfiles.txt ]; then 
	for f in `cat newfiles.txt`; do
		fname="${f%.*}"
		cdo sellonlatbox,149.5,155.5,-28,-37.5 "$f" "$fname"_Aus-NSW.nc
		cdo copy "$fname"_Aus-NSW.nc "$f"
		rm "$fname"_Aus-NSW.nc
	done
fi

rm lastwatch

# save dates for HFRadar data
latestdate=`ls *.nc | cut -c1-8 | tail -n 1`
year=${latestdate:0:4}
month=${latestdate:4:2}
day=${latestdate:6:2}
latesttime=`ls *.nc | cut -c9-14 | tail -n 1`
latesthour=${latesttime:0:2}
latesthourminus1=$(( latesthour - 1 ))
latesthourminus2=$(( latesthour - 2 ))

cd ../

###############################################################
# Download oceancolour data for the same month as SST above
###############################################################


if [ ! -d CHL_OC3 ]; then mkdir CHL_OC3; fi
cd CHL_OC3
# a file to distinguish old from new files
touch lastwatch

cat ../SST/fnames.txt | cut -c 1-8 > dates.txt

for d in `cat dates.txt`; do
  y=`echo "$d" | cut -c1-4`
  m=`echo "$d" | cut -c5-6`
  d=`echo "$d" | cut -c7-8`
  wget -q -nc -A.nc http://thredds.aodn.org.au/thredds/fileServer/IMOS/SRS/OC/gridded/aqua/P1D/"$y"/"$m"/A.P1D."$y$m$d"T000000Z.aust.chl_oc3.nc
  echo A.P1D."$y$m$d"T000000Z.aust.chl_oc3.nc >> fnames.txt
done  

# Remove old files or nc files not in fnames.txt
rm `ls *.nc | grep -v -f fnames.txt`

# file with list of new nc files
find -cnewer lastwatch -name "*.nc" -exec basename {} \; > newfiles.txt

# Use CDO to subset to the region of interest
if [ -s newfiles.txt ]; then 
	for f in `cat newfiles.txt`; do
		fname="${f%.*}"
		cdo sellonlatbox,149.5,155.5,-28,-37.5 "$f" "$fname"_Aus-NSW.nc
		cdo remapcon2,SSTgrid.des "$fname"_Aus-NSW.nc "$f"
		rm "$fname"_Aus-NSW.nc
	done
fi

rm lastwatch

cd ../

###############################################################
# Download HF Radar data for the latest day for NEWC and COFH
###############################################################

#if [ 10#$latesthour -eq 0 ]; then 
#	latesthourminus1=23
#	day=`printf "%02d" $(( 10#$day - 1 ))`
#else
#	latesthourminus1=`printf "%02d" $(( 10#$latesthour - 1 ))`
#fi

if [ ! -d HFRadar ]; then mkdir HFRadar; fi
cd HFRadar
if [ ! -d NEWC ]; then mkdir NEWC; fi
if [ ! -d COFH ]; then mkdir COFH; fi

cp ../CHL_OC3/dates.txt ./

cd NEWC
rm *.nc
for d in `cat ../dates.txt`; do 
  year=`echo "$d" | cut -c1-4`
  month=`echo "$d" | cut -c5-6`
  day=`echo "$d" | cut -c7-8`
  wget -q -nc -A.nc http://thredds.aodn.org.au/thredds/fileServer/IMOS/ACORN/gridded_1h-avg-current-map_non-QC/NEWC/"$year"/"$month"/"$day"/IMOS_ACORN_V_"$d"T150000Z_NEWC_FV00_1-hour-avg.nc 
  wget -q -nc -A.nc http://thredds.aodn.org.au/thredds/fileServer/IMOS/ACORN/gridded_1h-avg-current-map_non-QC/NEWC/"$year"/"$month"/"$day"/IMOS_ACORN_V_"$d"T120000Z_NEWC_FV00_1-hour-avg.nc 
  wget -q -nc -A.nc http://thredds.aodn.org.au/thredds/fileServer/IMOS/ACORN/gridded_1h-avg-current-map_non-QC/NEWC/"$year"/"$month"/"$day"/IMOS_ACORN_V_"$d"T090000Z_NEWC_FV00_1-hour-avg.nc 
done
cd ..

cd COFH
rm *.nc
for d in `cat ../dates.txt`; do 
  year=`echo "$d" | cut -c1-4`
  month=`echo "$d" | cut -c5-6`
  day=`echo "$d" | cut -c7-8`
  wget -q -nc -A.nc http://thredds.aodn.org.au/thredds/fileServer/IMOS/ACORN/gridded_1h-avg-current-map_non-QC/COF/"$year"/"$month"/"$day"/IMOS_ACORN_V_"$d"T153000Z_COF_FV00_1-hour-avg.nc 
  wget -q -nc -A.nc http://thredds.aodn.org.au/thredds/fileServer/IMOS/ACORN/gridded_1h-avg-current-map_non-QC/COF/"$year"/"$month"/"$day"/IMOS_ACORN_V_"$d"T123000Z_COF_FV00_1-hour-avg.nc 
  wget -q -nc -A.nc http://thredds.aodn.org.au/thredds/fileServer/IMOS/ACORN/gridded_1h-avg-current-map_non-QC/COF/"$year"/"$month"/"$day"/IMOS_ACORN_V_"$d"T093000Z_COF_FV00_1-hour-avg.nc 
done
cd ..

cd ../..
