#!/bin/bash

if [ ! -d data/SST ]; then mkdir data/SST; fi
cd data/SST
# a file to distinguish old from new files
touch lastwatch

# delete old files
if [ -f months.txt ]; then rm months.txt; fi
if [ -f fnames.txt ]; then rm fnames.txt; fi
if [ -f fnames_lastmon.txt ]; then rm fnames_lastmon.txt; fi

# Get the latest two available months
wget -q -O - http://opendap.bom.gov.au:8080/thredds/catalog/abom_imos_ghrsst_archive-1/v02.0fv03/Continental/L3C-01hour/ABOM-L3C_GHRSST-SSTskin-AxIH08/2019/catalog.html | grep -oP  "(?<=href=')[0-9][0-9](?=/catalog.html')" | sort -nr >> months.txt
thismon=`head -n 1 months.txt`
lastmon=`head -n 2 months.txt | tail -n 1`

# Get the names of latest 48 files of hourly data from this month
wget -q -O - http://opendap.bom.gov.au:8080/thredds/catalog/abom_imos_ghrsst_archive-1/v02.0fv03/Continental/L3C-01hour/ABOM-L3C_GHRSST-SSTskin-AxIH08/2019/"$thismon"/catalog.html | grep -oP  "(?<=ABOM-L3C_GHRSST-SSTskin-AxIH08/2019/"$thismon"/).*.nc(?='><tt>)" | tail -n 48 >> fnames.txt

# Download files in fnames.txt
for f in `cat fnames.txt`; do
	wget -nc -A.nc http://opendap.bom.gov.au:8080/thredds/fileServer/abom_imos_ghrsst_archive-1/v02.0fv03/Continental/L3C-01hour/ABOM-L3C_GHRSST-SSTskin-AxIH08/2019/"$thismon"/"$f"
done

# If there aren't 48 files in the current month then get the remaining files from last month
totfiles=`cat fnames.txt | wc -l`
if [ $totfiles -lt 48 ]; then
	remainfiles=$(( 48 - $totfiles ))		
	wget -q -O - http://opendap.bom.gov.au:8080/thredds/catalog/abom_imos_ghrsst_archive-1/v02.0fv03/Continental/L3C-01hour/ABOM-L3C_GHRSST-SSTskin-AxIH08/2019/"$lastmon"/catalog.html | grep -oP  "(?<=ABOM-L3C_GHRSST-SSTskin-AxIH08/2019/"$lastmon"/).*.nc(?='><tt>)" | tail -n "$remainfiles" > fnames_lastmon.txt
	for f in `cat fnames_lastmon.txt`; do
        	wget -nc -A.nc http://opendap.bom.gov.au:8080/thredds/fileServer/abom_imos_ghrsst_archive-1/v02.0fv03/Continental/L3C-01hour/ABOM-L3C_GHRSST-SSTskin-AxIH08/2019/"$lastmon"/"$f"
	done
	cat fnames_lastmon.txt >> fnames.txt
fi

# Remove old files or nc files not in fnames.txt
rm `ls *.nc | grep -v -f fnames.txt`

# file with list of new nc files
find -cnewer lastwatch -name "*.nc" -exec basename {} \; > newfiles.txt

# Use CDO to subset to the region of interest
if [ -s newfiles.txt ]; then 
	for f in `cat newfiles.txt`; do
		fname="${f%.*}"
		cdo sellonlatbox,149.5,160,-28,-37.5 "$f" "$fname"_Aus-NSW.nc
		cdo copy "$fname"_Aus-NSW.nc "$f"
		rm "$fname"_Aus-NSW.nc
	done
fi

rm lastwatch
cd ../..
