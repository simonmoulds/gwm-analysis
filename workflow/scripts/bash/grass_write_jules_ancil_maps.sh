#!/bin/bash

export GRASS_MESSAGE_FORMAT=plain
g.region region=${REGION}

# Create file to store JULES output files
TEMPFILE=${OUTDIR}/geotiff/filenames.txt
if [[ -f $TEMPFILE ]]
then
    rm -f $TEMPFILE
fi
touch $TEMPFILE

# ######################################################### #
# ######################################################### #
#
# JULES_LAND_FRAC
#
# ######################################################### #
# ######################################################### #

g.region region=$REGION

CUSTOM_LAND_FRAC_OUTFN=${OUTDIR}/geotiff/jamr_custom_land_frac_${REGION}.tif
r.in.gdal \
    -a \
    input=$FILE \
    output=land_frac_${REGION} \
    --overwrite
r.mapcalc \
    "custom_land_frac_${REGION} = if(land_frac_${REGION}>0,1,0)" \
    --overwrite
r.out.gdal \
    input=custom_land_frac_${REGION} \
    output=${CUSTOM_LAND_FRAC_OUTFN} \
    createopt="COMPRESS=DEFLATE" \
    --overwrite
g.remove -f type=raster name=land_frac_${REGION} 2> /dev/null
LAND_FRAC_MAP=custom_land_frac_${REGION}
echo "JULES_LAND_FRAC_FN=$CUSTOM_LAND_FRAC_OUTFN" >> ${TEMPFILE}
echo "PRODUCT=CUSTOM" >> ${TEMPFILE}	

# ######################################################### #
# ######################################################### #
#
# JULES_FRAC
#
# ######################################################### #
# ######################################################### #

g.region region=${REGION}

# declare -a YEARS=({1992..2015})
declare -a YEARS=(2015)

if [[ ${NINEPFT} == 1 || ${FIVEPFT} == 1 ]]
then
    
    # Loop through JULES land cover fractions
    for LC in tree_broadleaf tree_needleleaf shrub c4_grass c3_grass urban water bare_soil snow_ice tree_broadleaf_evergreen_tropical tree_broadleaf_evergreen_temperate tree_broadleaf_deciduous tree_needleleaf_evergreen tree_needleleaf_deciduous shrub_evergreen shrub_deciduous
    do
	# Here we decide whether or not we need to process the particular
	# lc type, depending on whether the user has specified to calculate
	# five PFT, nine PFT, or both types
	declare -a FIVEPFTONLY=(tree_broadleaf tree_needleleaf shrub)
	declare -a NINEPFTONLY=(tree_broadleaf_evergreen_tropical tree_broadleaf_evergreen_temperate tree_broadleaf_deciduous tree_needleleaf_evergreen tree_needleleaf_deciduous shrub_evergreen shrub_deciduous)
	declare -a LANDUSES=(combined natural rainfed irrigated)
	
	COMPUTE_LC=1
	if [[ $NINEPFT == 1 && $FIVEPFT == 0 ]]
	then
	    # https://stackoverflow.com/a/28032613
	    INARRAY=$(echo ${FIVEPFTONLY[@]} | grep -o "$LC" | wc -w)
	    if [[ $INARRAY == 1 ]]
	    then
		COMPUTE_LC=0
	    fi
	    
	elif [[ $NINEPFT == 0 && $FIVEPFT == 1 ]]
	then
	    INARRAY=$(echo ${NINEPFTONLY[@]} | grep -o "$LC" | wc -w)
	    if [[ $INARRAY == 1 ]]
	    then
		COMPUTE_LC=0
	    fi
	fi	

	if [[ $COMPUTE_LC == 1 ]]
	then	    
	    for YEAR in "${YEARS[@]}"
	    do
		for LU in "${LANDUSES[@]}"
		do		    
		    LC_OUTFN=${OUTDIR}/geotiff/jamr_${FRAC_VARNM}_${LC}_${LU}_${YEAR}_${REGION}.tif
		    if [[ ! -f $LC_OUTFN || $OVERWRITE == '--overwrite' ]]
		    then
			r.external \
			    -a \
			    input=${FRAC_BASENM}_${LC}_${LU}_${YEAR}_igp_0.008333Deg.tif \
			    output=${FRAC_VARNM}_${LC}_${LU}_${YEAR} \
			    --overwrite

			# average fraction of lc
			r.resamp.stats \
			    -w \
			    input=${FRAC_VARNM}_${LC}_${LU}_${YEAR} \
			    output=${FRAC_VARNM}_${LC}_${LU}_${YEAR}_${REGION} \
			    method=average \
			    --overwrite

			# fill in missing data using nearest neighbour (set mask
			# so that only cells in model domain are filled)
			r.mask \
			    raster=${LAND_FRAC_MAP} \
			    --overwrite
			r.grow.distance \
			    input=${FRAC_VARNM}_${LC}_${LU}_${YEAR}_${REGION} \
			    value=${FRAC_VARNM}_${LC}_${LU}_${YEAR}_${REGION}_filled0 \
			    --overwrite		    
			r.mask -r

			# set values outside model region to null
			r.mapcalc \
			    "${FRAC_VARNM}_${LC}_${LU}_${YEAR}_${REGION}_filled = if(${LAND_FRAC_MAP}==1,${FRAC_VARNM}_${LC}_${LU}_${YEAR}_${REGION}_filled0,null())" \
			    --overwrite
			
			# write output maps
			r.out.gdal \
			    input=${FRAC_VARNM}_${LC}_${LU}_${YEAR}_${REGION}_filled \
			    output=${LC_OUTFN} \
			    createopt="COMPRESS=DEFLATE" \
			    --overwrite

			# clean up
			g.remove \
			    -f \
			    type=raster \
			    name=${FRAC_VARNM}_${LC}_${LU}_${YEAR}_${REGION},${FRAC_VARNM}_${LC}_${LU}_${YEAR}_${REGION}_filled0,${FRAC_VARNM}_${LC}_${LU}_${YEAR}_${REGION}_filled 2> /dev/null			
		    fi		
		    echo "LC_${LC^^}_${LU^^}_${YEAR}_FN=$LC_OUTFN" >> $TEMPFILE
		done		
	    done
	fi	
    done
fi

# Cropland
for YEAR in "${YEARS[@]}"
do    
    CROPLAND_OUTFN=${OUTDIR}/geotiff/jamr_${FRAC_VARNM}_cropland_${YEAR}_${REGION}.tif
    if [[ ! -f $LC_OUTFN || $OVERWRITE == '--overwrite' ]]
    then
	r.external \
	    -a \
	    input=${AUXDIR}/frac/esacci_lc_${YEAR}_cropland_igp_0.008333Deg.tif \
	    output=cropland_${YEAR}_igp_0.008333Deg \
	    --overwrite    
	r.resamp.stats \
	    -w \
	    input=cropland_${YEAR}_igp_0.008333Deg \
	    output=cropland_${YEAR}_${REGION}0 \
	    method=average \
	    --overwrite
	r.mapcalc \
	    "cropland_${YEAR}_${REGION} = if(${LAND_FRAC_MAP}==1,cropland_${YEAR}_${REGION}0,null())" \
	    --overwrite
	r.out.gdal \
	    input=cropland_${YEAR}_${REGION} \
	    output=${CROPLAND_OUTFN} \
	    createopt="COMPRESS=DEFLATE" \
	    --overwrite
	echo "LC_CROPLAND_YEAR_FN=$CROPLAND_OUTFN" >> $TEMPFILE
    fi    
done

# Additional land covers
for LC in irrigated_continuous c3_irrigated_continuous c4_irrigated_continuous irrigated_triple_season c3_irrigated_triple_season c4_irrigated_triple_season irrigated_double_season c3_irrigated_double_season c4_irrigated_double_season irrigated_single_season c3_irrigated_single_season c4_irrigated_single_season rainfed c3_rainfed c4_rainfed fallow c3_fallow c4_fallow
do
    for YEAR in "${YEARS[@]}"
    do
	LC_OUTFN=${OUTDIR}/geotiff/jamr_${FRAC_VARNM}_${LC}_${YEAR}_${REGION}.tif
	if [[ ! -f $LC_OUTFN || $OVERWRITE == '--overwrite' ]]
	then
	    r.external \
		-a \
		input=${FRAC_BASENM}_${LC}_${YEAR}_igp_0.008333Deg.tif \
		output=${FRAC_VARNM}_${LC}_${YEAR} \
		--overwrite

	    # average fraction of lc
	    r.resamp.stats \
		-w \
		input=${FRAC_VARNM}_${LC}_${YEAR} \
		output=${FRAC_VARNM}_${LC}_${YEAR}_${REGION}_tmp \
		method=average \
		--overwrite

	    r.null \
		map=${FRAC_VARNM}_${LC}_${YEAR}_${REGION}_tmp \
		null=0
	    
	    # this should automatically set values outside model region to
	    # null, by multiplying by the cropland map where this is already
	    # the case
	    r.mapcalc \
		"${FRAC_VARNM}_${LC}_${YEAR}_${REGION} = ${FRAC_VARNM}_${LC}_${YEAR}_${REGION}_tmp * cropland_${YEAR}_${REGION}" \
		--overwrite
	    
	    # write output maps
	    r.out.gdal \
		input=${FRAC_VARNM}_${LC}_${YEAR}_${REGION} \
		output=${LC_OUTFN} \
		createopt="COMPRESS=DEFLATE" \
		--overwrite
	    
	    # # average fraction of lc
	    # r.resamp.stats \
	    # 	-w \
	    # 	input=${FRAC_VARNM}_${LC}_${YEAR} \
	    # 	output=${FRAC_VARNM}_${LC}_${YEAR}_${REGION} \
	    # 	method=average \
	    # 	--overwrite
	    # # fill in missing data
	    # r.mask \
	    # 	raster=${LAND_FRAC_MAP} \
	    # 	--overwrite

	    # # for land cover maps, use nearest neighbour
	    # r.grow.distance \
	    # 	input=${FRAC_VARNM}_${LC}_${YEAR}_${REGION} \
	    # 	value=${FRAC_VARNM}_${LC}_${YEAR}_${REGION}_filled \
	    # 	--overwrite		    

	    # # write output maps
	    # r.out.gdal \
	    # 	input=${FRAC_VARNM}_${LC}_${YEAR}_${REGION}_filled \
	    # 	output=${LC_OUTFN} \
	    # 	createopt="COMPRESS=DEFLATE" \
	    # 	--overwrite

	    # # remove mask
	    # r.mask -r

	    # clean up
	    g.remove \
		-f \
		type=raster \
		name=${FRAC_VARNM}_${LC}_${YEAR}_${REGION},${FRAC_VARNM}_${LC}_${YEAR}_${REGION}_tmp 2> /dev/null
	    # g.remove \
	    # 	-f \
	    # 	type=raster \
	    # 	name=${FRAC_VARNM}_${LC}_${YEAR}_${REGION}_filled 2> /dev/null
	fi		
	echo "LC_${LC^^}_${YEAR}_FN=$LC_OUTFN" >> $TEMPFILE
    done		
done

# Clean up
g.remove -f type=raster name=custom_land_frac_${REGION} 2> /dev/null
