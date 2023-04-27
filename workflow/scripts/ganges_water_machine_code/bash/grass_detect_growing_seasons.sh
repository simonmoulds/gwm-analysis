#!/bin/bash

r.mask -r

# Now test r.seasons
installed=`g.extension -a | grep r.seasons | wc -l`
if [[ $installed == 0 ]]
then    
    g.extension extension=r.seasons
fi

START_MONTH=7
for YEAR in {2014..2018}
do
    g.region region=igp_0.002778Deg
    r.mask raster=esa_cropland_${YEAR} --overwrite
    # r.mask raster=permanent_cropland --overwrite
    
    TMPFILE=/tmp/file_${YEAR}.txt
    if [[ -f $TMPFILE ]]
    then
	rm -f $TMPFILE
    fi    
    # Loop through LAI maps (three images per month, 2014--2019)
    for MONTH in {1..12}
    do
	MONTH=$(echo "$MONTH + $START_MONTH - 1" | bc)
	if [[ $MONTH -gt 12 ]]
	then
	    MONTH=$(echo "$MONTH - 12" | bc)
	    XYEAR=$(echo "$YEAR + 1" | bc)
	else
	    XYEAR=$YEAR
	fi	
	NDAY=`cal $MONTH $XYEAR | awk 'NF {DAYS = $NF}; END {print DAYS}'`
	for DAY in 10 20 $NDAY
	do
	    echo lai_${XYEAR}_$(printf "%02d" $MONTH)_${DAY}_igp_0.002778Deg.tif >> $TMPFILE
	done
    done

    # N.B. threshold value corresponds to value of 1.25 (experimenting)
    # As there are three LAI images per month, min_length=6 corresponds
    # to a duration of 2 months
    g.region region=igp_0.002778Deg
    r.seasons \
	file=/tmp/file_${YEAR}.txt \
	prefix=lai_season_${YEAR} \
	n=5 \
	nout=lai_season_${YEAR} \
	threshold_value=0.8 \
	min_length=6 \
	max_gap=1 \
	--overwrite

    # Here we attempt to identify continuous crops (e.g. sugarcane). We
    # do this by setting min_length=21 (7 months)
    r.seasons \
	file=/tmp/file_${YEAR}.txt \
	prefix=lai_season_${YEAR} \
	n=5 \
	nout=lai_continuous_season_${YEAR} \
	threshold_value=0.8 \
	min_length=6 \
	max_gap=1 \
	--overwrite
	
    r.mapcalc \
	"rainfed_${YEAR} = if((esa_rainfed_cropland_${YEAR}==1) &&& (lai_season_${YEAR}==1),1,null())" \
	--overwrite

    r.mapcalc \
	"irrigated_continuous_${YEAR} = if(((esa_irrigated_cropland_${YEAR}==1) &&& (lai_season_${YEAR}==1) &&& (lai_continuous_season_${YEAR}>0)),1,null())" \
	--overwrite

    # Exclude pixels identified as continuous crops
    r.mapcalc \
	"irrigated_single_season_${YEAR} = if(((esa_irrigated_cropland_${YEAR}==1) &&& (lai_season_${YEAR}==1) &&& (isnull(irrigated_continuous_${YEAR}))) ||| ((esa_rainfed_cropland_${YEAR}==1) &&& (lai_season_${YEAR}>1)),1,null())" \
	--overwrite
    # r.mapcalc \
    # 	"irrigated_single_season_${YEAR} = if(((esa_irrigated_cropland_${YEAR}==1) &&& (lai_season_${YEAR}==1)) ||| ((esa_rainfed_cropland_${YEAR}==1) &&& (lai_season_${YEAR}>1)),1,null())" \
    # 	--overwrite
    
    r.mapcalc \
	"irrigated_double_season_${YEAR} = if((esa_irrigated_cropland_${YEAR}==1) &&& (lai_season_${YEAR}==2),1,null())" \
	--overwrite
    
    r.mapcalc \
	"irrigated_triple_season_${YEAR} = if((esa_irrigated_cropland_${YEAR}==1) &&& (lai_season_${YEAR}>=3),1,null())" \
	--overwrite
    
    # We need to include fallow land for pixels which r.seasons does
    # not recognise as having a growing season
    r.mapcalc \
	"fallow_${YEAR} = if(((isnull(irrigated_continuous_${YEAR}) &&& isnull(irrigated_single_season_${YEAR}) &&& isnull(irrigated_double_season_${YEAR}) &&& isnull(irrigated_triple_season_${YEAR}) &&& isnull(rainfed_${YEAR})) &&& (esa_cropland_${YEAR} == 1)), 1, null())" \
	--overwrite

    # Remove mask for this stage of the analysis
    r.mask -r

    # cropland fraction is already written:
    # ${AUXDIR}/frac/esacci_lc_${YEAR}_cropland_igp_0.008333Deg.tif
    # Here we calculate the fraction of cropland devoted to each of the crop classes
    for LANDUSE in irrigated_continuous irrigated_single_season irrigated_double_season irrigated_triple_season rainfed fallow
    do
	# Set region to native region of land fraction maps
	g.region region=igp_0.002778Deg
    	# make a working copy
	r.mapcalc "${LANDUSE}_${YEAR}_tmp0 = ${LANDUSE}_${YEAR}" --overwrite
	# set null values to zero
    	r.null map=${LANDUSE}_${YEAR}_tmp0 null=0
	# multiply by esa_cropland_${YEAR} so that all non-cropland
	# cells are set to null - this ensures that the computed
	# fractions reflect the proportion of cropland, not the
	# proportion of the entire cell
	r.mapcalc \
	    "${LANDUSE}_${YEAR}_tmp = ${LANDUSE}_${YEAR}_tmp0 * esa_cropland_${YEAR}" \
	    --overwrite
	# now set region to coarse resolution to resample
    	g.region region=igp_0.008333Deg
    	r.resamp.stats \
    	    input=${LANDUSE}_${YEAR}_tmp \
    	    output=${LANDUSE}_${YEAR}_igp_0.008333Deg \
    	    method=average \
    	    --overwrite
	# divide into c4 and c3 fractions [TODO: could we assume that continuous crops are entirely C4?]
    	r.mapcalc \
    	    "c4_${LANDUSE}_${YEAR}_igp_0.008333Deg = ${LANDUSE}_${YEAR}_igp_0.008333Deg * c4_crop_frac_globe_0.008333Deg" \
    	    --overwrite
    	r.mapcalc \
    	    "c3_${LANDUSE}_${YEAR}_igp_0.008333Deg = ${LANDUSE}_${YEAR}_igp_0.008333Deg * (1 - c4_crop_frac_globe_0.008333Deg)" \
    	    --overwrite
	# write output
    	r.out.gdal \
    	    input=${LANDUSE}_${YEAR}_igp_0.008333Deg \
    	    output=${AUXDIR}/frac/lc_${LANDUSE}_${YEAR}_igp_0.008333Deg.tif \
    	    format="GTiff" \
    	    createopt="COMPRESS=DEFLATE" \
    	    --overwrite	
    	r.out.gdal \
    	    input=${LANDUSE}_${YEAR}_igp_0.008333Deg \
    	    output=${AUXDIR}/frac/lc_c4_${LANDUSE}_${YEAR}_igp_0.008333Deg.tif \
    	    format="GTiff" \
    	    createopt="COMPRESS=DEFLATE" \
    	    --overwrite	
    	r.out.gdal \
    	    input=${LANDUSE}_${YEAR}_igp_0.008333Deg \
    	    output=${AUXDIR}/frac/lc_c3_${LANDUSE}_${YEAR}_igp_0.008333Deg.tif \
    	    format="GTiff" \
    	    createopt="COMPRESS=DEFLATE" \
    	    --overwrite

	# clean up
	g.remove -f type=raster name=${LANDUSE}_${YEAR}_tmp0,${LANDUSE}_${YEAR}_tmp
	
    	# Reset region to native region of land cover maps
    	g.region region=igp_0.002778Deg

    done    
done

r.mask -r	    

