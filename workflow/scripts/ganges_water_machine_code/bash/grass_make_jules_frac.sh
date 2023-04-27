#!/bin/bash

r.mask -r
r.external.out -r

# ===========================================================
# Import tropical forest area, infill
# ===========================================================

unzip -o ${DATADIR}/official_teow.zip -d ${AUXDIR}
gdal_rasterize \
    -at -te -180 -90 180 90 \
    -ts 43200 21600 \
    -a BIOME \
    ../data/aux/official/wwf_terr_ecos.shp \
    ../data/aux/wwf_terr_ecos_0.008333Deg.tif
g.region region=globe_0.008333Deg
r.in.gdal \
    -a \
    input=${AUXDIR}/wwf_terr_ecos_0.008333Deg.tif \
    output=wwf_terr_ecos_globe_0.008333Deg \
    ${OVERWRITE}
r.null map=wwf_terr_ecos_globe_0.008333Deg setnull=0
r.grow.distance \
    input=wwf_terr_ecos_globe_0.008333Deg \
    value=wwf_terr_ecos_globe_0.008333Deg_interp \
    ${OVERWRITE}
r.mapcalc \
    "tropical_broadleaf_forest_globe_0.008333Deg = if((wwf_terr_ecos_globe_0.008333Deg_interp == 1) | (wwf_terr_ecos_globe_0.008333Deg_interp == 2), 1, 0)" \
    ${OVERWRITE}

# ===========================================================
# Read C4 fraction data
# ===========================================================

# Run R script to make C4 fraction:
NATFILE=${AUXDIR}/c4_nat_veg_frac_0.008333Deg.tif
CROPFILE=${AUXDIR}/c4_crop_frac_0.008333Deg.tif
if [[ ! -f ${NATFILE} || ! -f ${CROPFILE} || ${OVERWRITE} == '--overwrite' ]]   
then    
    Rscript $SRCDIR/rscript/make_c4_fraction.R
    g.region region=globe_0.008333Deg
    r.in.gdal \
	-a \
	input=${AUXDIR}/c4_nat_veg_frac_0.008333Deg.tif \
	output=c4_nat_veg_frac_globe_0.008333Deg \
	--overwrite
    r.in.gdal \
	-a \
	input=${AUXDIR}/c4_crop_frac_0.008333Deg.tif \
	output=c4_crop_frac_globe_0.008333Deg \
	--overwrite
fi

# ===========================================================
# Make JULES land cover fraction maps
# ===========================================================

chmod 755 $SRCDIR/bash/make_land_cover_fraction_lookup_tables.sh
bash $SRCDIR/bash/make_land_cover_fraction_lookup_tables.sh

# declare -a YEARS=({1992..2015})
declare -a YEARS=(2015)

g.region region=igp_0.002778Deg

# Setting mask will ensure values outside land mask are set to null
# esacci_land_frac_globe_0.002778Deg computed in grass_make_jules_land_frac.sh
# r.external -a input=${AUXDIR}/dem/merit_dem_globe_0.002778Deg.tif output=merit_dem_globe_0.002778Deg --overwrite
r.mask raster=esacci_land_frac_globe_0.002778Deg
# r.mapcalc \
#     "merit_dem_globe_0.002778Deg_surf_hgt = merit_dem_globe_0.002778Deg" \
#     --overwrite

# land sea mask @ 0.002778Deg
r.mapcalc \
    "land_sea_mask = esacci_land_frac_globe_0.002778Deg" \
    --overwrite
r.mask -r

# Loop through years and create land cover fractions/surface heights
for YEAR in "${YEARS[@]}"
do
    g.region region=igp_0.002778Deg
    
    r.in.gdal \
	-a \
	input=${ESACCIDIR}/ESACCI-LC-L4-LCCS-Map-300m-P1Y-${YEAR}-v2.0.7.tif \
	output=esacci_lc_${YEAR}_w_sea \
	$OVERWRITE

    # Mask sea values in esacci_lc_${YEAR}_w_sea by multiplying by
    # land_sea_mask, in which non-land cells are null.    
    r.mapcalc \
    	"esacci_lc_${YEAR} = esacci_lc_${YEAR}_w_sea * land_sea_mask" \
    	$OVERWRITE
    	    
    # tree broadleaf evergreen
    # ########################	
    g.region region=igp_0.002778Deg
    r.reclass \
	input=esacci_lc_${YEAR} \
	output=esacci_lc_${YEAR}_tree_broadleaf_evergreen_x1000 \
	rules=$AUXDIR/tree_broadleaf_evergreen_reclass.txt \
	$OVERWRITE
    
    g.region region=igp_0.008333Deg
    r.resamp.stats \
	input=esacci_lc_${YEAR}_tree_broadleaf_evergreen_x1000 \
	output=esacci_lc_${YEAR}_tree_broadleaf_evergreen_igp_0.008333Deg_x1000 \
	method=average \
	$OVERWRITE
    
    r.mapcalc \
	"esacci_lc_${YEAR}_tree_broadleaf_evergreen_igp_0.008333Deg = esacci_lc_${YEAR}_tree_broadleaf_evergreen_igp_0.008333Deg_x1000 / 1000" \
	$OVERWRITE
    
    # tree broadleaf deciduous
    # ########################
    g.region region=igp_0.002778Deg
    r.reclass \
	input=esacci_lc_${YEAR} \
	output=esacci_lc_${YEAR}_tree_broadleaf_deciduous_x1000 \
	rules=$AUXDIR/tree_broadleaf_deciduous_reclass.txt \
	$OVERWRITE
    
    g.region region=igp_0.008333Deg
    r.resamp.stats \
	input=esacci_lc_${YEAR}_tree_broadleaf_deciduous_x1000 \
	output=esacci_lc_${YEAR}_tree_broadleaf_deciduous_igp_0.008333Deg_x1000 \
	method=average \
	$OVERWRITE
    
    r.mapcalc \
	"esacci_lc_${YEAR}_tree_broadleaf_deciduous_igp_0.008333Deg = esacci_lc_${YEAR}_tree_broadleaf_deciduous_igp_0.008333Deg_x1000 / 1000" \
	$OVERWRITE

    # tree needleleaf evergreen
    # #########################
    g.region region=igp_0.002778Deg
    r.reclass \
	input=esacci_lc_${YEAR} \
	output=esacci_lc_${YEAR}_tree_needleleaf_evergreen_x1000 \
	rules=$AUXDIR/tree_needleleaf_evergreen_reclass.txt \
	$OVERWRITE
    
    g.region region=igp_0.008333Deg
    r.resamp.stats \
	input=esacci_lc_${YEAR}_tree_needleleaf_evergreen_x1000 \
	output=esacci_lc_${YEAR}_tree_needleleaf_evergreen_igp_0.008333Deg_x1000 \
	method=average \
	$OVERWRITE
    
    r.mapcalc \
	"esacci_lc_${YEAR}_tree_needleleaf_evergreen_igp_0.008333Deg = esacci_lc_${YEAR}_tree_needleleaf_evergreen_igp_0.008333Deg_x1000 / 1000" \
	$OVERWRITE

    # tree needleleaf deciduous
    # #########################
    g.region region=igp_0.002778Deg
    r.reclass \
	input=esacci_lc_${YEAR} \
	output=esacci_lc_${YEAR}_tree_needleleaf_deciduous_x1000 \
	rules=$AUXDIR/tree_needleleaf_deciduous_reclass.txt \
	$OVERWRITE
    
    g.region region=igp_0.008333Deg
    r.resamp.stats \
	input=esacci_lc_${YEAR}_tree_needleleaf_deciduous_x1000 \
	output=esacci_lc_${YEAR}_tree_needleleaf_deciduous_igp_0.008333Deg_x1000 \
	method=average \
	$OVERWRITE
    
    r.mapcalc \
	"esacci_lc_${YEAR}_tree_needleleaf_deciduous_igp_0.008333Deg = esacci_lc_${YEAR}_tree_needleleaf_deciduous_igp_0.008333Deg_x1000 / 1000" \
	$OVERWRITE

    # shrub broadleaf evergreen
    # #########################
    g.region region=igp_0.002778Deg
    r.reclass \
	input=esacci_lc_${YEAR} \
	output=esacci_lc_${YEAR}_shrub_broadleaf_evergreen_x1000 \
	rules=$AUXDIR/shrub_broadleaf_evergreen_reclass.txt \
	$OVERWRITE
    
    g.region region=igp_0.008333Deg
    r.resamp.stats \
	input=esacci_lc_${YEAR}_shrub_broadleaf_evergreen_x1000 \
	output=esacci_lc_${YEAR}_shrub_broadleaf_evergreen_igp_0.008333Deg_x1000 \
	method=average \
	$OVERWRITE
    
    r.mapcalc \
	"esacci_lc_${YEAR}_shrub_broadleaf_evergreen_igp_0.008333Deg = esacci_lc_${YEAR}_shrub_broadleaf_evergreen_igp_0.008333Deg_x1000 / 1000" \
	$OVERWRITE

    # shrub broadleaf deciduous
    # #########################
    g.region region=igp_0.002778Deg
    r.reclass \
	input=esacci_lc_${YEAR} \
	output=esacci_lc_${YEAR}_shrub_broadleaf_deciduous_x1000 \
	rules=$AUXDIR/shrub_broadleaf_deciduous_reclass.txt \
	$OVERWRITE
    
    g.region region=igp_0.008333Deg
    r.resamp.stats \
	input=esacci_lc_${YEAR}_shrub_broadleaf_deciduous_x1000 \
	output=esacci_lc_${YEAR}_shrub_broadleaf_deciduous_igp_0.008333Deg_x1000 \
	method=average \
	$OVERWRITE
    
    r.mapcalc \
	"esacci_lc_${YEAR}_shrub_broadleaf_deciduous_igp_0.008333Deg = esacci_lc_${YEAR}_shrub_broadleaf_deciduous_igp_0.008333Deg_x1000 / 1000" \
	$OVERWRITE

    # shrub needleleaf evergreen
    # ##########################
    g.region region=igp_0.002778Deg
    r.reclass \
	input=esacci_lc_${YEAR} \
	output=esacci_lc_${YEAR}_shrub_needleleaf_evergreen_x1000 \
	rules=$AUXDIR/shrub_needleleaf_evergreen_reclass.txt \
	$OVERWRITE
    
    g.region region=igp_0.008333Deg
    r.resamp.stats \
	input=esacci_lc_${YEAR}_shrub_needleleaf_evergreen_x1000 \
	output=esacci_lc_${YEAR}_shrub_needleleaf_evergreen_igp_0.008333Deg_x1000 \
	method=average \
	$OVERWRITE
    
    r.mapcalc \
	"esacci_lc_${YEAR}_shrub_needleleaf_evergreen_igp_0.008333Deg = esacci_lc_${YEAR}_shrub_needleleaf_evergreen_igp_0.008333Deg_x1000 / 1000" \
	$OVERWRITE

    # shrub neefleleaf deciduous
    # ##########################
    g.region region=igp_0.002778Deg
    r.reclass \
	input=esacci_lc_${YEAR} \
	output=esacci_lc_${YEAR}_shrub_needleleaf_deciduous_x1000 \
	rules=$AUXDIR/shrub_needleleaf_deciduous_reclass.txt \
	$OVERWRITE
    
    g.region region=igp_0.008333Deg
    r.resamp.stats \
	input=esacci_lc_${YEAR}_shrub_needleleaf_deciduous_x1000 \
	output=esacci_lc_${YEAR}_shrub_needleleaf_deciduous_igp_0.008333Deg_x1000 \
	method=average \
	$OVERWRITE
    
    r.mapcalc \
	"esacci_lc_${YEAR}_shrub_needleleaf_deciduous_igp_0.008333Deg = esacci_lc_${YEAR}_shrub_needleleaf_deciduous_igp_0.008333Deg_x1000 / 1000" \
	$OVERWRITE

    # natural grass
    # #############
    g.region region=igp_0.002778Deg
    r.reclass \
	input=esacci_lc_${YEAR} \
	output=esacci_lc_${YEAR}_natural_grass_x1000 \
	rules=$AUXDIR/natural_grass_reclass.txt \
	$OVERWRITE
    
    g.region region=igp_0.008333Deg
    r.resamp.stats \
	input=esacci_lc_${YEAR}_natural_grass_x1000 \
	output=esacci_lc_${YEAR}_natural_grass_igp_0.008333Deg_x1000 \
	method=average \
	$OVERWRITE
    
    r.mapcalc \
	"esacci_lc_${YEAR}_natural_grass_igp_0.008333Deg = esacci_lc_${YEAR}_natural_grass_igp_0.008333Deg_x1000 / 1000" \
	$OVERWRITE
    
    # managed grass
    # #############
    g.region region=igp_0.002778Deg
    r.reclass \
	input=esacci_lc_${YEAR} \
	output=esacci_lc_${YEAR}_managed_grass_x1000 \
	rules=$AUXDIR/managed_grass_reclass.txt \
	$OVERWRITE
    
    g.region region=igp_0.008333Deg
    r.resamp.stats \
	input=esacci_lc_${YEAR}_managed_grass_x1000 \
	output=esacci_lc_${YEAR}_managed_grass_igp_0.008333Deg_x1000 \
	method=average \
	$OVERWRITE
    
    r.mapcalc \
	"esacci_lc_${YEAR}_managed_grass_igp_0.008333Deg = esacci_lc_${YEAR}_managed_grass_igp_0.008333Deg_x1000 / 1000" \
	$OVERWRITE

    # urban
    # #####
    g.region region=igp_0.002778Deg
    r.reclass \
	input=esacci_lc_${YEAR} \
	output=esacci_lc_${YEAR}_urban_x1000 \
	rules=$AUXDIR/urban_reclass.txt \
	$OVERWRITE
    
    g.region region=igp_0.008333Deg
    r.resamp.stats \
	input=esacci_lc_${YEAR}_urban_x1000 \
	output=esacci_lc_${YEAR}_urban_igp_0.008333Deg_x1000 \
	method=average \
	$OVERWRITE
    
    r.mapcalc \
	"esacci_lc_${YEAR}_urban_igp_0.008333Deg = esacci_lc_${YEAR}_urban_igp_0.008333Deg_x1000 / 1000" \
	$OVERWRITE

    # bare soil
    # #########
    g.region region=igp_0.002778Deg
    r.reclass \
	input=esacci_lc_${YEAR} \
	output=esacci_lc_${YEAR}_bare_soil_x1000 \
	rules=$AUXDIR/bare_soil_reclass.txt \
	$OVERWRITE
    
    g.region region=igp_0.008333Deg
    r.resamp.stats \
	input=esacci_lc_${YEAR}_bare_soil_x1000 \
	output=esacci_lc_${YEAR}_bare_soil_igp_0.008333Deg_x1000 \
	method=average \
	$OVERWRITE
    
    r.mapcalc \
	"esacci_lc_${YEAR}_bare_soil_igp_0.008333Deg = esacci_lc_${YEAR}_bare_soil_igp_0.008333Deg_x1000 / 1000" \
	$OVERWRITE
    
    # water
    # #####
    g.region region=igp_0.002778Deg
    r.reclass \
	input=esacci_lc_${YEAR} \
	output=esacci_lc_${YEAR}_water_x1000 \
	rules=$AUXDIR/water_reclass.txt \
	$OVERWRITE
    g.region region=igp_0.008333Deg
    r.resamp.stats \
	input=esacci_lc_${YEAR}_water_x1000 \
	output=esacci_lc_${YEAR}_water_igp_0.008333Deg_x1000 \
	method=average \
	$OVERWRITE
    r.mapcalc \
	"esacci_lc_${YEAR}_water_igp_0.008333Deg = esacci_lc_${YEAR}_water_igp_0.008333Deg_x1000 / 1000" \
	$OVERWRITE

    # snow/ice
    # ########
    g.region region=igp_0.002778Deg
    r.reclass \
	input=esacci_lc_${YEAR} \
	output=esacci_lc_${YEAR}_snow_ice_x1000 \
	rules=$AUXDIR/snow_ice_reclass.txt \
	$OVERWRITE
    
    g.region region=igp_0.008333Deg
    r.resamp.stats \
	input=esacci_lc_${YEAR}_snow_ice_x1000 \
	output=esacci_lc_${YEAR}_snow_ice_igp_0.008333Deg_x1000 \
	method=average \
	$OVERWRITE
    
    r.mapcalc \
	"esacci_lc_${YEAR}_snow_ice_igp_0.008333Deg = esacci_lc_${YEAR}_snow_ice_igp_0.008333Deg_x1000 / 1000" \
	$OVERWRITE
    
    # no data
    # #######
    g.region region=igp_0.002778Deg
    r.reclass \
	input=esacci_lc_${YEAR} \
	output=esacci_lc_${YEAR}_nodata_x1000 \
	rules=$AUXDIR/nodata_reclass.txt \
	$OVERWRITE
    
    g.region region=igp_0.008333Deg
    r.resamp.stats \
	input=esacci_lc_${YEAR}_nodata_x1000 \
	output=esacci_lc_${YEAR}_nodata_igp_0.008333Deg_x1000 \
	method=average \
	$OVERWRITE
    
    r.mapcalc \
	"esacci_lc_${YEAR}_nodata_igp_0.008333Deg = esacci_lc_${YEAR}_nodata_igp_0.008333Deg_x1000 / 1000" \
	$OVERWRITE

    # Set up external output
    r.external.out \
	directory=${AUXDIR}/frac \
	format="GTiff" \
	options="COMPRESS=DEFLATE"
    
    # Additional classes:
    # ###################

    # These are the classes we use to compute agricultural land
    
    # Rainfed cropland (classes 10, 11)
    g.region region=igp_0.002778Deg
    r.mapcalc \
    	"esacci_lc_${YEAR}_rainfed_cropland.tif = if((esacci_lc_${YEAR} == 10) || (esacci_lc_${YEAR} == 11), 1, 0)" \
    	$OVERWRITE
    
    g.region region=igp_0.008333Deg
    r.resamp.stats \
	input=esacci_lc_${YEAR}_rainfed_cropland.tif \
	output=esacci_lc_${YEAR}_rainfed_cropland_igp_0.008333Deg.tif \
	method=average \
	$OVERWRITE	
    
    # Irrigated cropland (class 20)
    g.region region=igp_0.002778Deg
    r.mapcalc \
    	"esacci_lc_${YEAR}_irrigated_cropland.tif = if((esacci_lc_${YEAR} == 20), 1, 0)" \
    	$OVERWRITE

    g.region region=igp_0.008333Deg
    r.resamp.stats \
	input=esacci_lc_${YEAR}_irrigated_cropland.tif \
	output=esacci_lc_${YEAR}_irrigated_cropland_igp_0.008333Deg.tif \
	method=average \
	$OVERWRITE	
    
    # Combined:
    r.mapcalc \
	"esacci_lc_${YEAR}_cropland_igp_0.008333Deg.tif = esacci_lc_${YEAR}_rainfed_cropland_igp_0.008333Deg.tif + esacci_lc_${YEAR}_irrigated_cropland_igp_0.008333Deg.tif" \
	$OVERWRITE
        
    # ===================================================== #
    # Convert to JULES land cover types
    # ===================================================== #
    g.region region=igp_0.008333Deg

    # ###########
    # (i) 5 PFT 
    # ###########
    
    # tree broadleaf
    r.mapcalc \
	"lc_tree_broadleaf_combined_${YEAR}_igp_0.008333Deg.tif = esacci_lc_${YEAR}_tree_broadleaf_evergreen_igp_0.008333Deg + esacci_lc_${YEAR}_tree_broadleaf_deciduous_igp_0.008333Deg" \
	$OVERWRITE
    r.mapcalc \
	"lc_tree_broadleaf_natural_${YEAR}_igp_0.008333Deg.tif = lc_tree_broadleaf_combined_${YEAR}_igp_0.008333Deg.tif" \
	$OVERWRITE
    r.mapcalc \
	"lc_tree_broadleaf_rainfed_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE
    r.mapcalc \
	"lc_tree_broadleaf_irrigated_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE    

    # tree needleleaf
    r.mapcalc \
	"lc_tree_needleleaf_combined_${YEAR}_igp_0.008333Deg.tif = esacci_lc_${YEAR}_tree_needleleaf_evergreen_igp_0.008333Deg + esacci_lc_${YEAR}_tree_needleleaf_deciduous_igp_0.008333Deg" \
	$OVERWRITE
    r.mapcalc \
	"lc_tree_needleleaf_natural_${YEAR}_igp_0.008333Deg.tif = lc_tree_needleleaf_combined_${YEAR}_igp_0.008333Deg.tif" \
	$OVERWRITE
    r.mapcalc \
	"lc_tree_needleleaf_rainfed_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE
    r.mapcalc \
	"lc_tree_needleleaf_irrigated_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE    

    # shrub
    r.mapcalc \
	"lc_shrub_combined_${YEAR}_igp_0.008333Deg.tif = esacci_lc_${YEAR}_shrub_broadleaf_evergreen_igp_0.008333Deg + esacci_lc_${YEAR}_shrub_broadleaf_deciduous_igp_0.008333Deg + esacci_lc_${YEAR}_shrub_needleleaf_evergreen_igp_0.008333Deg + esacci_lc_${YEAR}_shrub_needleleaf_deciduous_igp_0.008333Deg" \
	$OVERWRITE
    r.mapcalc \
	"lc_shrub_natural_${YEAR}_igp_0.008333Deg.tif = lc_shrub_combined_${YEAR}_igp_0.008333Deg.tif" \
	$OVERWRITE
    r.mapcalc \
	"lc_shrub_rainfed_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE
    r.mapcalc \
	"lc_shrub_irrigated_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE    

    # c4 grass
    r.mapcalc \
	"lc_c4_grass_combined_${YEAR}_igp_0.008333Deg.tif = (esacci_lc_${YEAR}_natural_grass_igp_0.008333Deg * c4_nat_veg_frac_globe_0.008333Deg) + (esacci_lc_${YEAR}_managed_grass_igp_0.008333Deg * c4_crop_frac_globe_0.008333Deg)" \
	$OVERWRITE
    r.mapcalc \
	"lc_c4_grass_natural_${YEAR}_igp_0.008333Deg.tif = lc_c4_grass_combined_${YEAR}_igp_0.008333Deg.tif - (esacci_lc_${YEAR}_cropland_igp_0.008333Deg.tif * c4_crop_frac_globe_0.008333Deg)" \
	$OVERWRITE
    r.mapcalc \
	"lc_c4_grass_rainfed_${YEAR}_igp_0.008333Deg.tif = (esacci_lc_${YEAR}_rainfed_cropland_igp_0.008333Deg.tif * c4_crop_frac_globe_0.008333Deg)" \
	$OVERWRITE
    r.mapcalc \
	"lc_c4_grass_irrigated_${YEAR}_igp_0.008333Deg.tif = (esacci_lc_${YEAR}_irrigated_cropland_igp_0.008333Deg.tif * c4_crop_frac_globe_0.008333Deg)" \
	$OVERWRITE    
    
    # c3 grass
    r.mapcalc \
	"lc_c3_grass_combined_${YEAR}_igp_0.008333Deg.tif = (esacci_lc_${YEAR}_natural_grass_igp_0.008333Deg * (1-c4_nat_veg_frac_globe_0.008333Deg)) + (esacci_lc_${YEAR}_managed_grass_igp_0.008333Deg * (1-c4_crop_frac_globe_0.008333Deg))" \
	$OVERWRITE
    r.mapcalc \
	"lc_c3_grass_natural_${YEAR}_igp_0.008333Deg.tif = lc_c3_grass_combined_${YEAR}_igp_0.008333Deg.tif - (esacci_lc_${YEAR}_cropland_igp_0.008333Deg.tif * (1-c4_crop_frac_globe_0.008333Deg))" \
	$OVERWRITE
    r.mapcalc \
	"lc_c3_grass_rainfed_${YEAR}_igp_0.008333Deg.tif = (esacci_lc_${YEAR}_rainfed_cropland_igp_0.008333Deg.tif * (1-c4_crop_frac_globe_0.008333Deg))" \
	$OVERWRITE
    r.mapcalc \
	"lc_c3_grass_irrigated_${YEAR}_igp_0.008333Deg.tif = (esacci_lc_${YEAR}_irrigated_cropland_igp_0.008333Deg.tif * (1-c4_crop_frac_globe_0.008333Deg))" \
	$OVERWRITE    

    # urban
    r.mapcalc \
	"lc_urban_combined_${YEAR}_igp_0.008333Deg.tif = esacci_lc_${YEAR}_urban_igp_0.008333Deg" \
	$OVERWRITE
    r.mapcalc \
	"lc_urban_natural_${YEAR}_igp_0.008333Deg.tif = lc_urban_combined_${YEAR}_igp_0.008333Deg.tif" \
	$OVERWRITE
    r.mapcalc \
	"lc_urban_rainfed_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE
    r.mapcalc \
	"lc_urban_irrigated_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE    

    # water
    r.mapcalc \
	"lc_water_combined_${YEAR}_igp_0.008333Deg.tif = esacci_lc_${YEAR}_water_igp_0.008333Deg" \
	$OVERWRITE
    r.mapcalc \
	"lc_water_natural_${YEAR}_igp_0.008333Deg.tif = lc_water_combined_${YEAR}_igp_0.008333Deg.tif" \
	$OVERWRITE
    r.mapcalc \
	"lc_water_rainfed_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE
    r.mapcalc \
	"lc_water_irrigated_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE    

    # bare soil
    r.mapcalc \
	"lc_bare_soil_combined_${YEAR}_igp_0.008333Deg.tif = esacci_lc_${YEAR}_bare_soil_igp_0.008333Deg" \
	$OVERWRITE
    r.mapcalc \
	"lc_bare_soil_natural_${YEAR}_igp_0.008333Deg.tif = lc_bare_soil_combined_${YEAR}_igp_0.008333Deg.tif" \
	$OVERWRITE
    r.mapcalc \
	"lc_bare_soil_rainfed_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE
    r.mapcalc \
	"lc_bare_soil_irrigated_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE    

    # snow/ice
    r.mapcalc \
	"lc_snow_ice_combined_${YEAR}_igp_0.008333Deg.tif = esacci_lc_${YEAR}_snow_ice_igp_0.008333Deg" \
	$OVERWRITE
    r.mapcalc \
	"lc_snow_ice_natural_${YEAR}_igp_0.008333Deg.tif = lc_snow_ice_combined_${YEAR}_igp_0.008333Deg.tif" \
	$OVERWRITE
    r.mapcalc \
	"lc_snow_ice_rainfed_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE
    r.mapcalc \
	"lc_snow_ice_irrigated_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE    

    # # weighted elevation
    # for LC in tree_broadleaf tree_needleleaf shrub c4_grass c3_grass urban water bare_soil snow_ice
    # do
    # 	r.mapcalc \
    # 	    "lc_${LC}_${YEAR}_igp_0.008333Deg_weighted_elev = lc_${LC}_${YEAR}_igp_0.008333Deg * merit_dem_igp_0.008333Deg_surf_hgt" \
    # 	    $OVERWRITE
    # done

    # ############
    # (ii) 9 PFT
    # ############
    
    # tree broadleaf evergreen tropical
    r.mapcalc \
	"lc_tree_broadleaf_evergreen_tropical_combined_${YEAR}_igp_0.008333Deg.tif = esacci_lc_${YEAR}_tree_broadleaf_evergreen_igp_0.008333Deg * tropical_broadleaf_forest_globe_0.008333Deg" \
	$OVERWRITE
    r.mapcalc \
	"lc_tree_broadleaf_evergreen_tropical_natural_${YEAR}_igp_0.008333Deg.tif = lc_tree_broadleaf_evergreen_tropical_combined_${YEAR}_igp_0.008333Deg.tif" \
	$OVERWRITE
    r.mapcalc \
	"lc_tree_broadleaf_evergreen_tropical_rainfed_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE
    r.mapcalc \
	"lc_tree_broadleaf_evergreen_tropical_irrigated_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE    

    # tree broadleaf evergreen temperate
    r.mapcalc \
	"lc_tree_broadleaf_evergreen_temperate_combined_${YEAR}_igp_0.008333Deg.tif = esacci_lc_${YEAR}_tree_broadleaf_evergreen_igp_0.008333Deg * (1-tropical_broadleaf_forest_globe_0.008333Deg)" \
	$OVERWRITE
    r.mapcalc \
	"lc_tree_broadleaf_evergreen_temperate_natural_${YEAR}_igp_0.008333Deg.tif = lc_tree_broadleaf_evergreen_temperate_combined_${YEAR}_igp_0.008333Deg.tif" \
	$OVERWRITE
    r.mapcalc \
	"lc_tree_broadleaf_evergreen_temperate_rainfed_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE
    r.mapcalc \
	"lc_tree_broadleaf_evergreen_temperate_irrigated_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE    

    # tree broadleaf deciduous
    r.mapcalc \
	"lc_tree_broadleaf_deciduous_combined_${YEAR}_igp_0.008333Deg.tif = esacci_lc_${YEAR}_tree_broadleaf_deciduous_igp_0.008333Deg" \
	$OVERWRITE
    r.mapcalc \
	"lc_tree_broadleaf_deciduous_natural_${YEAR}_igp_0.008333Deg.tif = lc_tree_broadleaf_deciduous_combined_${YEAR}_igp_0.008333Deg.tif" \
	$OVERWRITE
    r.mapcalc \
	"lc_tree_broadleaf_deciduous_rainfed_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE
    r.mapcalc \
	"lc_tree_broadleaf_deciduous_irrigated_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE    

    # tree needleleaf evergreen
    r.mapcalc \
	"lc_tree_needleleaf_evergreen_combined_${YEAR}_igp_0.008333Deg.tif = esacci_lc_${YEAR}_tree_needleleaf_evergreen_igp_0.008333Deg" \
	$OVERWRITE
    r.mapcalc \
	"lc_tree_needleleaf_evergreen_natural_${YEAR}_igp_0.008333Deg.tif = lc_tree_needleleaf_evergreen_combined_${YEAR}_igp_0.008333Deg.tif" \
	$OVERWRITE
    r.mapcalc \
	"lc_tree_needleleaf_evergreen_rainfed_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE
    r.mapcalc \
	"lc_tree_needleleaf_evergreen_irrigated_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE    

    # tree needleleaf deciduous
    r.mapcalc \
	"lc_tree_needleleaf_deciduous_combined_${YEAR}_igp_0.008333Deg.tif = esacci_lc_${YEAR}_tree_needleleaf_deciduous_igp_0.008333Deg" \
	$OVERWRITE
    r.mapcalc \
	"lc_tree_needleleaf_deciduous_natural_${YEAR}_igp_0.008333Deg.tif = lc_tree_needleleaf_deciduous_combined_${YEAR}_igp_0.008333Deg.tif" \
	$OVERWRITE
    r.mapcalc \
	"lc_tree_needleleaf_deciduous_rainfed_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE
    r.mapcalc \
	"lc_tree_needleleaf_deciduous_irrigated_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE    

    # shrub evergreen
    r.mapcalc \
	"lc_shrub_evergreen_combined_${YEAR}_igp_0.008333Deg.tif = esacci_lc_${YEAR}_shrub_broadleaf_evergreen_igp_0.008333Deg + esacci_lc_${YEAR}_shrub_needleleaf_evergreen_igp_0.008333Deg" \
	$OVERWRITE
    r.mapcalc \
	"lc_shrub_evergreen_natural_${YEAR}_igp_0.008333Deg.tif = lc_shrub_evergreen_combined_${YEAR}_igp_0.008333Deg.tif" \
	$OVERWRITE
    r.mapcalc \
	"lc_shrub_evergreen_rainfed_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE
    r.mapcalc \
	"lc_shrub_evergreen_irrigated_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE    

    # shrub deciduous
    r.mapcalc \
	"lc_shrub_deciduous_combined_${YEAR}_igp_0.008333Deg.tif = esacci_lc_${YEAR}_shrub_broadleaf_deciduous_igp_0.008333Deg + esacci_lc_${YEAR}_shrub_needleleaf_deciduous_igp_0.008333Deg" \
	$OVERWRITE
    r.mapcalc \
	"lc_shrub_deciduous_natural_${YEAR}_igp_0.008333Deg.tif = lc_shrub_deciduous_combined_${YEAR}_igp_0.008333Deg.tif" \
	$OVERWRITE
    r.mapcalc \
	"lc_shrub_deciduous_rainfed_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE
    r.mapcalc \
	"lc_shrub_deciduous_irrigated_${YEAR}_igp_0.008333Deg.tif = 0" \
	$OVERWRITE    

    # # weighted elevation
    # for LC in tree_broadleaf_evergreen_tropical tree_broadleaf_evergreen_temperate tree_broadleaf_deciduous tree_needleleaf_evergreen tree_needleleaf_deciduous shrub_evergreen shrub_deciduous
    # do
    # 	r.mapcalc \
    # 	    "lc_${LC}_${YEAR}_igp_0.008333Deg_weighted_elev = lc_${LC}_${YEAR}_igp_0.008333Deg * merit_dem_igp_0.008333Deg_surf_hgt" \
    # 	    $OVERWRITE
    # done

    r.external.out -r
    
done
