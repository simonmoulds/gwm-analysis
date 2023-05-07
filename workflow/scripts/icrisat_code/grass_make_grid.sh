#!/bin/bash

# This script is run from
# 'run_grass_process_village_census.sh'

export GRASS_MESSAGE_FORMAT=plain
r.mask -r

# bounding box (set to India extent)
NORTH=40
SOUTH=0
EAST=100
WEST=60

# region settings
g.region \
    n=$NORTH \
    s=$SOUTH \
    e=$EAST \
    w=$WEST \
    res=0:00:30 \
    save=india_0.008333Deg \
    --overwrite

g.region \
    n=$NORTH \
    s=$SOUTH \
    e=$EAST \
    w=$WEST \
    res=0:05 \
    save=india_0.083333Deg \
    --overwrite

g.region \
    n=$NORTH \
    s=$SOUTH \
    e=$EAST \
    w=$WEST \
    res=0:15 \
    save=india_0.250000Deg \
    --overwrite

g.region region=india_0.083333Deg
r.mapcalc "template = 1" --overwrite

# ######################################################### #
# Import ICRISAT district map
# ######################################################### #

v.in.ogr \
    -o \
    input=$WD/code/irrigated_area_source.gpkg \
    layer=x2000 \
    output=icrisat_dists \
    snap=1e-12 \
    --overwrite

g.region region=india_0.008333Deg
v.to.rast \
    input=icrisat_dists \
    output=icrisat_poly_ids \
    use=attr \
    attribute_column=POLY_ID \
    --overwrite

r.out.gdal \
    input=icrisat_poly_ids \
    output=../data/icrisat_poly_ids.tif \
    --overwrite

r.mapcalc \
    "india_rgn_0.008333Deg = if(icrisat_poly_ids>0,1,null())" \
    --overwrite

r.mapcalc \
    "not_india_rgn_0.008333Deg = if(isnull(india_rgn_0.008333Deg),1,null())" \
    --overwrite

# ######################################################### #
# Create district fraction maps
# ######################################################### #

# Get unique values
r.stats -n icrisat_poly_ids > /tmp/icrisat_ids.txt
while read line
do
    # Set all cells belonging to current district to 1,
    # all other cells to zero
    g.region region=india_0.008333Deg
    r.mapcalc "tmp0 = if(icrisat_poly_ids==$line,1,0)" --overwrite

    # Change to coarse region and resample
    g.region region=india_0.083333Deg    
    r.resamp.stats \
	-w \
	input=tmp0 \
	output=tmp1 \
	method=average \
	--overwrite

    # Set zero values to null, then zoom to non-null cells
    # (this keeps file size down)
    r.null map=tmp1 setnull=0
    g.region zoom=tmp1
    r.out.gdal \
	input=tmp1 \
	output=$OUTDIR/dist_fraction/district_frac_$line.tif \
	--overwrite

    # Set back to fine resolution
    g.region region=india_0.008333Deg
    # Zero values to null, zoom to non-null cells
    r.null map=tmp0 setnull=0
    g.region zoom=tmp0
    # Write fine resolution district fraction (belong/not belong)
    r.out.gdal \
	input=tmp0 \
	output=$OUTDIR/dist_fraction/district_frac_1km_$line.tif \
	--overwrite    
done < /tmp/icrisat_ids.txt

# ######################################################### #
# Import canal command area map
# ######################################################### #

v.in.ogr \
    -o \
    input=$WD/data-raw/command_areas.shp \
    output=command_areas \
    snap=1e-12 \
    --overwrite

g.region region=india_0.008333Deg
v.to.rast \
    input=command_areas \
    output=command_areas_ids \
    use=attr \
    attribute_column=ID \
    --overwrite

r.mapcalc \
    "indus_command_areas_ids = command_areas_ids * not_india_rgn_0.008333Deg" \
    --overwrite

r.out.gdal \
    input=command_areas_ids \
    output=../data/command_areas_ids.tif \
    --overwrite

r.mapcalc \
    "cmd_rgn_0.008333Deg = if(command_areas_ids>0,1,null())" \
    --overwrite

r.out.gdal \
    input=indus_command_areas_ids \
    output=../data/indus_command_areas_ids.tif \
    --overwrite

r.mapcalc \
    "indus_cmd_rgn_0.008333Deg = if(indus_command_areas_ids>0,1,null())" \
    --overwrite

# ######################################################### #
# Create command area fraction maps
# ######################################################### #

# Get unique values [for Indus only]
r.stats -n indus_command_areas_ids > /tmp/indus_command_areas_ids.txt
while read line
do
    # Set all cells belonging to current command_area to 1,
    # all other cells to zero
    g.region region=india_0.008333Deg
    r.mapcalc "tmp0 = if(indus_command_areas_ids==$line,1,0)" --overwrite

    # Change to coarse region and resample
    g.region region=india_0.083333Deg    
    r.resamp.stats \
	-w \
	input=tmp0 \
	output=tmp1 \
	method=average \
	--overwrite

    # Set zero values to null, then zoom to non-null cells
    # (this keeps file size down)
    r.null map=tmp1 setnull=0
    g.region zoom=tmp1
    r.out.gdal \
	input=tmp1 \
	output=$OUTDIR/dist_fraction/command_area_frac_$line.tif \
	--overwrite

    # Set back to fine resolution
    g.region region=india_0.008333Deg
    # Zero values to null, zoom to non-null cells
    r.null map=tmp0 setnull=0
    g.region zoom=tmp0
    # Write fine resolution command_area fraction (belong/not belong)
    r.out.gdal \
	input=tmp0 \
	output=$OUTDIR/dist_fraction/command_area_frac_1km_$line.tif \
	--overwrite    
done < /tmp/indus_command_areas_ids.txt

# # Get unique values
# r.stats -n command_areas_ids > /tmp/command_areas_ids.txt
# while read line
# do
#     # Set all cells belonging to current command_area to 1,
#     # all other cells to zero
#     g.region region=india_0.008333Deg
#     r.mapcalc "tmp0 = if(command_areas_ids==$line,1,0)" --overwrite

#     # Change to coarse region and resample
#     g.region region=india_0.083333Deg    
#     r.resamp.stats \
# 	-w \
# 	input=tmp0 \
# 	output=tmp1 \
# 	method=average \
# 	--overwrite

#     # Set zero values to null, then zoom to non-null cells
#     # (this keeps file size down)
#     r.null map=tmp1 setnull=0
#     g.region zoom=tmp1
#     r.out.gdal \
# 	input=tmp1 \
# 	output=$OUTDIR/dist_fraction/command_area_frac_$line.tif \
# 	--overwrite

#     # Set back to fine resolution
#     g.region region=india_0.008333Deg
#     # Zero values to null, zoom to non-null cells
#     r.null map=tmp0 setnull=0
#     g.region zoom=tmp0
#     # Write fine resolution command_area fraction (belong/not belong)
#     r.out.gdal \
# 	input=tmp0 \
# 	output=$OUTDIR/dist_fraction/command_area_frac_1km_$line.tif \
# 	--overwrite    
# done < /tmp/command_areas_ids.txt

# ######################################################### #
# Cropland data (IIASA-IFPRI)
# ######################################################### #

# Set mask for remaining analysis
g.region region=india_0.008333Deg

# extract IIASA data
if [ -d $WD/data/iiasa-ifpri-cropland-map ]
then
    rm -r $WD/data/iiasa-ifpri-cropland-map
fi
unzip $WD/data-raw/cropland_hybrid_10042015v9.zip -d $WD/data/iiasa-ifpri-cropland-map

# convert to geotiff, restrict to current study region
gdal_translate \
    -of GTiff \
    $WD/data/iiasa-ifpri-cropland-map/Hybrid_10042015v9.img \
    $WD/data/iiasa-ifpri-cropland-map/Hybrid_10042015v9.tif

gdalwarp \
    -te $WEST $SOUTH $EAST $NORTH \
    -overwrite \
    $WD/data/iiasa-ifpri-cropland-map/Hybrid_10042015v9.tif \
    $WD/data/iiasa-ifpri-cropland-map/Hybrid_10042015v9_cropped.tif

# import data to GRASS
r.in.gdal \
    -a \
    input=$WD/data/iiasa-ifpri-cropland-map/Hybrid_10042015v9_cropped.tif \
    output=tmp \
    --overwrite
r.null map=tmp null=0
    
# convert to fraction such that each pixel represents the
# fractional coverage of cropland in that grid cell).
r.mapcalc \
    "iiasa_ifpri_crop_map_india = (tmp / 100) * india_rgn_0.008333Deg" \
    --overwrite

r.mapcalc \
    "iiasa_ifpri_crop_map_cmd = (tmp / 100) * cmd_rgn_0.008333Deg" \
    --overwrite

r.mapcalc \
    "iiasa_ifpri_crop_map = (tmp / 100)" \
    --overwrite

g.remove -f type=raster name=tmp

# write output to file
r.out.gdal \
    input=iiasa_ifpri_crop_map_india \
    output=$OUTDIR/iiasa_ifpri_cropland_map_india_0.008333Deg.tif \
    --overwrite    

# write output to file
r.out.gdal \
    input=iiasa_ifpri_crop_map_cmd \
    output=$OUTDIR/iiasa_ifpri_cropland_map_cmd_0.008333Deg.tif \
    --overwrite    

# write output to file
r.out.gdal \
    input=iiasa_ifpri_crop_map \
    output=$OUTDIR/iiasa_ifpri_cropland_map_0.008333Deg.tif \
    --overwrite    

# ######################################################### #
# GIAM
# ######################################################### #

gdalwarp \
    -te $WEST $SOUTH $EAST $NORTH \
    -tr 0.008333333333 0.008333333333 \
    -r mode \
    -overwrite \
    $WD/data-raw/giam_500_m_30classes_south_asia_tif/giam_500_m_30classes_south_asia_tif.tif \
    $WD/data/giam_500_m_30classes_south_asia_tif_resamp.tif \

# ######################################################### #
# Irrigated area data (Ambika et al. 2016)
# ######################################################### #

# Spread around 2010
for YR in {2000..2014}
do
    MAP=$YR-$((YR+1))
    # nearest neighbour resampling to study resolution
    gdalwarp \
	-te $WEST $SOUTH $EAST $NORTH \
	-tr 0.008333333333 0.008333333333 \
	-r near \
	-overwrite \
	$WD/data-raw/SCIENTIFIC_DATA_IRRIGATION_MAP_2000_2015/$MAP.tif \
	$WD/data/${MAP}_resamp.tif

    g.region region=india_0.008333Deg
    NEWMAP=$(echo $MAP | sed 's/-/_/g')
    r.in.gdal \
	-a \
	input=$WD/data/${MAP}_resamp.tif \
	output=ambika_$NEWMAP \
	--overwrite
    r.null map=ambika_$NEWMAP null=0

    # This calc has the side-effect of setting all cells
    # outside India to null
    r.mapcalc \
	"irrigated_area_${NEWMAP} = ambika_$NEWMAP * iiasa_ifpri_crop_map_india" \
	--overwrite

    # write output
    r.out.gdal \
	input=irrigated_area_${NEWMAP} \
	output=$OUTDIR/irrigated_area_${NEWMAP}_india_0.008333Deg.tif \
	--overwrite
    
done

# ######################################################### #
# Winter cropped area (SEDAC)
# ######################################################### #

if [ -f $WD/data/India_cropped-area_1km_2001-16_resamp.tif ]
then
    rm -f $WD/data/India_cropped-area_1km_2001-16_resamp.tif
fi

for BAND in {1..16}
do    
    # YEAR=$((BAND+=2000))
    
    # first separate band from multiband file - otherwise
    # the reprojection doesn't work
    gdal_translate \
	-b $BAND \
	$WD/data-raw/India_cropped-area_1km_2001-16.tif \
	$WD/data/India_cropped-area_1km_2001-16_${BAND}.tif
    
    # now reproject so that the map has the same spatial
    # attributes as the current region
    gdalwarp \
	-te $WEST $SOUTH $EAST $NORTH \
	-tr 0.008333333333 0.008333333333 \
	-r bilinear \
	-overwrite \
	$WD/data/India_cropped-area_1km_2001-16_${BAND}.tif \
	$WD/data/India_cropped-area_1km_2001-16_${BAND}_resamp.tif

    # import to grass, convert percent to fraction
    g.region region=india_0.008333Deg
    r.in.gdal \
	-a \
	input=$WD/data/India_cropped-area_1km_2001-16_${BAND}_resamp.tif \
	output=tmp \
	--overwrite
    r.null map=tmp null=0
    
    r.mapcalc \
    	"winter_cropped_area_$BAND = (tmp / 100) * india_rgn_0.008333Deg" \
    	--overwrite

    # write output
    r.out.gdal \
	input=winter_cropped_area_$BAND \
	output=$OUTDIR/winter_cropped_area_$((BAND+=2000))_india_0.008333Deg.tif \
	--overwrite
    
done

# # import district level data
# # for YR in {1966..2016}
# for YR in {2001..2015}
# do
#     v.in.ogr \
# 	input=irrigated_area_source.gpkg \
# 	layer=x${YR} \
# 	output=irrigated_area_source_${YR} \
# 	snap=1e-12 \
# 	--overwrite
    
#     # create a vector map representation of a
#     # regular coordinate grid which corresponds
#     # with the resolution and extent of the
#     # intended raster map    
#     g.region region=india_0.008333Deg
#     g.region -p
#     v.mkgrid \
# 	map=grid_${YR} \
# 	type=area \
# 	--overwrite

#     # add columns for cropland pct and whether
#     # the grid cell is irrigated or not
#     v.db.addcolumn \
# 	map=grid_${YR} \
# 	columns="cropland_pct double precision,winter_cropland_pct double precision,irrigated_cropland_pct double precision"
    
#     # net cropland pct
#     v.what.rast \
#     	map=grid_${YR} \
#     	type=centroid \
#     	raster=iiasa_ifpri_crop_map_india \
#     	column=cropland_pct
    
#     # winter cropped area pct
#     v.what.rast \
#     	map=grid_${YR} \
#     	type=centroid \
#     	raster=winter_cropped_area_${YEAR} \
#     	column=winter_cropland_pct

#     # net irrigated cropland pct
#     v.what.rast \
#     	map=grid_${YR} \
#     	type=centroid \
# 	raster=irrigated_area_${YR}_$((YR+=1)) \
# 	column=irrigated_cropland_pct

#     # ###################################################### #
#     # Intersect the census map with grid map
#     # ###################################################### #
    
#     # This divides the areas in ainput
#     # according to the area boundaries in
#     # binput (see man page for more details)
#     v.overlay \
#         ainput=irrigated_area_source_${YR} \
#         binput=grid_${YR} \
#         operator=and \
#         output=irrigated_area_source_${YR}_grid \
#         snap=1e-8 \
#         --overwrite

#     # Export/import - THIS IS A HACK, but resolves some problems with vector dataset
#     v.out.ogr \
#         input=irrigated_area_source_${YR}_grid \
#         output=tmp.gpkg \
#         format=GPKG \
#         --overwrite
#     v.in.ogr \
#         input=tmp.gpkg \
#         output=irrigated_area_source_${YR}_grid \
#         --overwrite
#     rm tmp.gpkg             

#     # `v.to.db` populates attribute columns from vector features
#     # (in this case area)
#     v.db.addcolumn \
#         map=irrigated_area_source_${YR}_grid \
#         columns="area_size double precision"
#     v.to.db \
#         map=irrigated_area_source_${YR}_grid \
#         option=area \
#         columns=area_size \
#         units=meters

#     v.db.addcolumn \
# 	map=irrigated_area_source_${YR}_grid \
# 	columns="cropland_area double precision,winter_cropland_area double precision,irrigated_cropland_area double precision"
    
#     v.db.update \
#         map=irrigated_area_source_${YR}_grid \
#         col=cropland_area \
#         qcol="b_cropland_pct * area_size"
    
#     v.db.update \
#         map=irrigated_area_source_${YR}_grid \
#         col=winter_cropland_pct \
#         qcol="b_winter_cropland_pct * area_size"
    
#     v.db.update \
#         map=irrigated_area_source_${YR}_grid \
#         col=irrigated_area \
#         qcol="b_irrigated_cropland_pct * area_size"

#     # Now sum up
#     v.vect.stats \
#         points=irrigated_area_source_${YR}_grid \
#         type=centroid \
#         areas=irrigated_area_source_${YR} \
#         points_column=cropland_area \
#         count_column=num_points \
#         method=sum \
#         stats_column=total_cropland_area    

#     v.vect.stats \
#         points=irrigated_area_source_${YR}_grid \
#         type=centroid \
#         areas=irrigated_area_source_${YR} \
#         points_column=cropland_area \
#         count_column=num_points \
#         method=sum \
#         stats_column=total_cropland_area    

#     v.vect.stats \
#         points=irrigated_area_source_${YR}_grid \
#         type=centroid \
#         areas=irrigated_area_source_${YR} \
#         points_column=winter_cropland_area \
#         count_column=num_points \
#         method=sum \
#         stats_column=total_winter_cropland_area    
    
#     v.vect.stats \
#         points=irrigated_area_source_${YR}_grid \
#         type=centroid \
#         areas=irrigated_area_source_${YR} \
#         points_column=irrigated_cropland_area \
#         count_column=num_points \
#         method=sum \
#         stats_column=total_irrigated_cropland_area

#     v.out.ogr \
# 	-u \
# 	input=irrigated_area_source_${YR} \
# 	layer=x${YR}_updated \
# 	output=irrigated_area_source.gpkg \
# 	--overwrite
        
#     # In GRASS:
#     # #########
#     # (i)   compute area by multiplying polygon area by pct - DONE
    
#     # (ii)  calculate total cropland area, total irrigated
#     #       area by overlaying districts - DONE

#     # (iii) write to file - DONE

#     # (iv)  create map of districts where each pixel
#     #       indicates the unique district id (use this in R
#     #       to get fractional district coverage at different
#     #       resolutions)

#     # In R:
#     # #####
#     # (v)   get statistical relationship between reported
#     #       area and remote sensing data

#     # (vi)  use statistical relationships to get complete
#     #       time series

#     # (vii) develop allocation routine???    
# done

