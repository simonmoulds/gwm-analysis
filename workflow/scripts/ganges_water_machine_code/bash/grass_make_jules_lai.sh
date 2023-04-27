#!/bin/bash

r.mask -r
r.external.out -r

# The aim of this script is to create LAI maps which can be used in JULES

# # Declare model resolutions
# declare -a RGNS=(0.5 0.25 0.1 0.0833333333333)

r.external.out \
    directory=${AUXDIR}/lai \
    format="GTiff" \
    options="COMPRESS=DEFLATE"    

for YEAR in {2014..2019}
do    
    for MONTH in {01..12}
    do
	NDAY=`cal $MONTH $YEAR | awk 'NF {DAYS = $NF}; END {print DAYS}'`
	for DAY in 10 20 $NDAY
	do
	    echo "Processing ESA LAI map for ${DAY}/${MONTH}/${YEAR}"

	    # ======================================================= #
	    # Combined natural/cropland                               #
	    # ======================================================= #
	    # Average to 4km (equivalent to MODIS, for ANTS)
	    g.region region=igp_0.041667Deg
	    r.resamp.stats \
	    	input=lai_${YEAR}_${MONTH}_${DAY}_igp_0.002778Deg.tif \
	    	output=lai_combined_${YEAR}_${MONTH}_${DAY}_igp_0.041667Deg.tif \
	    	method=average \
	    	--overwrite
	    g.region region=igp_0.002778Deg
	    
	    r.mapcalc \
	    	"lai_natural_${YEAR}_${MONTH}_${DAY}_igp_0.002778Deg.tif = lai_${YEAR}_${MONTH}_${DAY}_igp_0.002778Deg.tif * permanent_natural_vegetation" \
	    	--overwrite

	    # ======================================================= #
	    # Natural only                                            #
	    # ======================================================= #	    
	    g.region region=igp_0.041667Deg
	    r.resamp.stats \
	    	input=lai_natural_${YEAR}_${MONTH}_${DAY}_igp_0.002778Deg.tif \
	    	output=lai_natural_${YEAR}_${MONTH}_${DAY}_igp_0.041667Deg.tif \
	    	method=average \
	    	--overwrite
	    g.region region=igp_0.002778Deg	    
	    
	    # ======================================================= #
	    # Rainfed cropland (select using permanent cropland maps  #
	    # ======================================================= #	    
	    # r.mapcalc \
	    # 	"lai_rainfed_cropland_${YEAR}_${MONTH}_${DAY}_igp_0.002778Deg.tif = lai_${YEAR}_${MONTH}_${DAY}_igp_0.002778Deg.tif * permanent_rainfed_cropland" \
	    # 	--overwrite
	    # g.region region=igp_0.041667Deg
	    # r.resamp.stats \
	    # 	input=lai_rainfed_cropland_${YEAR}_${MONTH}_${DAY}_igp_0.002778Deg.tif \
	    # 	output=lai_rainfed_cropland_${YEAR}_${MONTH}_${DAY}_igp_0.041667Deg.tif \
	    # 	method=average \
	    # 	--overwrite	   
	    # g.region region=igp_0.002778Deg	    
	    
	    if [[ $YEAR == 2019 ]]
	    then
	    	XYEAR=2018
	    else
	    	XYEAR=$YEAR
	    fi
	    
	    # ======================================================= #
	    # Rainfed cropland
	    # ======================================================= #	    	    
	    r.mapcalc \
		"lai_rainfed_cropland_${YEAR}_${MONTH}_${DAY}_igp_0.002778Deg.tif = lai_${YEAR}_${MONTH}_${DAY}_igp_0.002778Deg.tif * rainfed_${XYEAR}" \
		--overwrite	    
	    g.region region=igp_0.041667Deg
	    r.resamp.stats \
		input=lai_rainfed_cropland_${YEAR}_${MONTH}_${DAY}_igp_0.002778Deg.tif \
		output=lai_rainfed_cropland_${YEAR}_${MONTH}_${DAY}_igp_0.041667Deg.tif \
		method=average \
		--overwrite
	    g.region region=igp_0.002778Deg
	    
	    # ======================================================= #
	    # Fallow cropland
	    # ======================================================= #	    	    
	    r.mapcalc \
		"lai_fallow_cropland_${YEAR}_${MONTH}_${DAY}_igp_0.002778Deg.tif = lai_${YEAR}_${MONTH}_${DAY}_igp_0.002778Deg.tif * fallow_${XYEAR}" \
		--overwrite	    
	    g.region region=igp_0.041667Deg
	    r.resamp.stats \
		input=lai_fallow_cropland_${YEAR}_${MONTH}_${DAY}_igp_0.002778Deg.tif \
		output=lai_fallow_cropland_${YEAR}_${MONTH}_${DAY}_igp_0.041667Deg.tif \
		method=average \
		--overwrite
	    g.region region=igp_0.002778Deg
	    
	    # ======================================================= #
	    # Irrigated cropland
	    # ======================================================= #

	    r.mapcalc \
	    	"lai_irrigated_cropland_c_${YEAR}_${MONTH}_${DAY}_igp_0.002778Deg.tif = lai_${YEAR}_${MONTH}_${DAY}_igp_0.002778Deg.tif * irrigated_continuous_${XYEAR}" \
	    	--overwrite
	    
	    r.mapcalc \
	    	"lai_irrigated_cropland_1_${YEAR}_${MONTH}_${DAY}_igp_0.002778Deg.tif = lai_${YEAR}_${MONTH}_${DAY}_igp_0.002778Deg.tif * irrigated_single_season_${XYEAR}" \
	    	--overwrite

	    r.mapcalc \
	    	"lai_irrigated_cropland_2_${YEAR}_${MONTH}_${DAY}_igp_0.002778Deg.tif = lai_${YEAR}_${MONTH}_${DAY}_igp_0.002778Deg.tif * irrigated_double_season_${XYEAR}" \
	    	--overwrite

	    r.mapcalc \
	    	"lai_irrigated_cropland_3_${YEAR}_${MONTH}_${DAY}_igp_0.002778Deg.tif = lai_${YEAR}_${MONTH}_${DAY}_igp_0.002778Deg.tif * irrigated_triple_season_${XYEAR}" \
	    	--overwrite

	    for SEASON in c 1 2 3
	    do
	    	g.region region=igp_0.041667Deg
	    	r.resamp.stats \
	    	    input=lai_irrigated_cropland_${SEASON}_${YEAR}_${MONTH}_${DAY}_igp_0.002778Deg.tif \
	    	    output=lai_irrigated_cropland_${SEASON}_${YEAR}_${MONTH}_${DAY}_igp_0.041667Deg.tif \
	    	    method=average \
	    	    --overwrite
	    	g.region region=igp_0.002778Deg
	    done	    
	done
    done
done

r.external.out -r 	    

		


