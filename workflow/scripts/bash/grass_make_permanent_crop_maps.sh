#!/bin/bash

g.region region=igp_0.002778Deg

# Detect permanent crops
for YEAR in {2014..2018}
do
    # Reset region
    g.region region=igp_0.002778Deg
    g.region -p
    eval `g.region -g`
	
    # Maps from 2016-2018 have slightly different naming convention
    LC_BASENAME=ESACCI-LC-L4-LCCS-Map-300m-P1Y-${YEAR}-v2.0.7
    if [[ $YEAR -ge 2016 ]]
    then
	LC_BASENAME=C3S-LC-L4-LCCS-Map-300m-P1Y-${YEAR}-v2.1.1
    fi
    
    # Resample to current region settings to handle any discrepancies
    # N.B. specifying resolution/method is probably not necessary
    # but included to handle any precision errors
    echo $ewres $nsres
    gdalwarp \
	-te $w $s $e $n \
	-tr $ewres $nsres \
	-r near \
	-co "COMPRESS=lzw" \
	-overwrite \
	"${ESACCIDIR}"/"${LC_BASENAME}".tif \
	"${AUXDIR}"/lai/"${LC_BASENAME}"_India.tif
    
    # Import data to GRASS
    r.external input="${AUXDIR}"/lai/"${LC_BASENAME}"_India.tif output=lc_${YEAR}_India --overwrite
    
    # Identify all cropland (10, 11, 20)
    r.mapcalc \
	"esa_cropland_${YEAR} = if((lc_${YEAR}_India == 10) || (lc_${YEAR}_India == 11) || (lc_${YEAR}_India == 20), 1, null())" \
	--overwrite

    # Identify rainfed cropland (classes 10, 11)
    r.mapcalc \
	"esa_rainfed_cropland_${YEAR} = if((lc_${YEAR}_India == 10) || (lc_${YEAR}_India == 11), 1, null())" \
	--overwrite
    
    # Identify irrigated cropland (class 20)
    r.mapcalc \
	"esa_irrigated_cropland_${YEAR} = if((lc_${YEAR}_India == 20), 1, null())" \
	--overwrite

    # Identify natural cropland (NOT classes 10, 11, 20)
    r.mapcalc \
	"esa_natural_${YEAR} = if((lc_${YEAR}_India == 10) || (lc_${YEAR}_India == 11) || (lc_${YEAR}_India == 20), null(), 1)" \
	--overwrite
	
done

# identify permanent cropland and permanent irrigated cropland
# by adding maps for each year in the study region and
# selecting non-null cells
r.mapcalc \
    "tmp0 = esa_cropland_2014+esa_cropland_2015+esa_cropland_2016+esa_cropland_2017+esa_cropland_2018" \
    --overwrite
r.mapcalc \
    "tmp1 = esa_rainfed_cropland_2014+esa_rainfed_cropland_2015+esa_rainfed_cropland_2016+esa_rainfed_cropland_2017+esa_rainfed_cropland_2018" \
    --overwrite
r.mapcalc \
    "tmp2 = esa_irrigated_cropland_2014+esa_irrigated_cropland_2015+esa_irrigated_cropland_2016+esa_irrigated_cropland_2017+esa_irrigated_cropland_2018" \
    --overwrite
r.mapcalc \
    "tmp3 = esa_natural_2014+esa_natural_2015+esa_natural_2016+esa_natural_2017+esa_natural_2018" \
    --overwrite

r.mapcalc \
    "permanent_cropland = tmp0 * 0 + 1" \
    --overwrite
r.mapcalc \
    "permanent_rainfed_cropland = tmp1 * 0 + 1" \
    --overwrite
r.mapcalc \
    "permanent_irrigated_cropland = tmp2 * 0 + 1" \
    --overwrite
r.mapcalc \
    "permanent_natural_vegetation = tmp3 * 0 + 1" \
    --overwrite

g.remove -f type=raster name=tmp0,tmp1
