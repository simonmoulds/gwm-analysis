#!/usr/bin/env python3

import os

from grass_session import Session
from grass.script import core as gcore
import grass.script as gscript
import grass.script.setup as gsetup

# Import grass python libraries
from grass.pygrass.modules.shortcuts import general as g
from grass.pygrass.modules.shortcuts import raster as r
from grass.pygrass.modules.shortcuts import vector as v
from grass.pygrass.modules.shortcuts import temporal as t

from constants import (jules_5pft_only,
                       jules_9pft_only,
                       jules_common_pft,
                       jules_land_uses,
                       jules_land_covers)

# TODO get this from snakefile
npft = 5

# Now we can create a grass session instance
my_gisdb = os.path.join(os.getenv('HOME'), 'grassdata')
my_location = 'latlong'
my_mapset = 'gwm'

user = Session()
user.open(gisdb=my_gisdb, location=my_location, mapset=my_mapset)

# ######################################################### #
# ######################################################### #
#
# JULES_LAND_FRAC
#
# ######################################################### #
# ######################################################### #

input_map = 'results/WFD-EI-LandFraction2d_IGP.tif'
region = 'igp'
g.region(region=region)
land_frac_map = f'custom_land_frac_{region}'
r.in_gdal(
    flags="a",
    input=input_map,
    output=f'land_frac_{region}'
)
r.mapcalc(
    f'{land_frac_map} = if(land_frac_{region} > 0, 1, 0)',
    overwrite=True
)
r.out_gdal(
    input=f'{land_frac_map}',
    output=f'results/{land_frac_map}.tif',
    createopt='COMPRESS=LZW',
    overwrite=True
)
g.remove(flags='f', type='raster', name=f'land_frac_{region}')
# LAND_FRAC_MAP=custom_land_frac_${REGION}
# echo "JULES_LAND_FRAC_FN=$CUSTOM_LAND_FRAC_OUTFN" >> ${TEMPFILE}
# echo "PRODUCT=CUSTOM" >> ${TEMPFILE}

# ######################################################### #
# ######################################################### #
#
# JULES_FRAC
#
# ######################################################### #
# ######################################################### #

g.region(region=region)
years = [2015]


for lc in jules_land_covers:
    compute = False
    if npft == 9 & lc in jules_9pft:
        compute = True
    elif npft == 5 & lc in jules_5pft:
        compute = True

    if compute:
        for year in years:
            for lu in jules_land_uses:
                grass_input_map = f'lc_{lc}_{lu}_{year}_igp_0.008333Deg'
                grass_resamp_map = f'lc_{lc}_{lu}_{year}_{region}'

                # Average fraction of lc
                r.resamp_stats(
                    flags='w',
                    input=grass_input_map,
                    output=grass_resamp_map,
                    method='average',
                    overwrite=True
                )

                # Fill in missing data using nearest neighbour. We set
                # a mask so that only cells in the model domain are filled.
                r.mask(raster=land_frac_map, overwrite=True)
                r.grow_distance(
                    input=grass_resamp_map,
                    value=f'{grass_resamp_map}_filled0',
                    overwrite=True
                )
                r.mask(flags="r")

                # Set values outside the model region to null
                r.mapcalc(
                    f'{grass_resamp_map}_filled = if({land_frac_map}==1, {grass_resamp_map}_filled0, null())',
                    overwrite=True
                )

                output_map = f'lc_{lc}_{lu}_{year}_{region}.tif'
                r.out_gdal(
                    input=f'{grass_resamp_map}_filled',
                    output=os.path.join(outputdir, output_map),
                    createopt='COMPRESS=LZW',
                    overwrite=True
                )

                # Clean up
                maps_to_remove = [
                    grass_resamp_map,
                    f'{grass_resamp_map}_filled0',
                    f'{grass_resamp_map}_filled'
                ]
                g.remove(
                    flags="f",
                    type='raster',
                    name=','.join(maps_to_remove)
                )


for year in years:

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
