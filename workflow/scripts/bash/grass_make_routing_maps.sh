#!/bin/bash

export GRASS_MESSAGE_FORMAT=plain

installed=`g.extension -a | grep r.stream.basins | wc -l`
if [[ $installed == 0 ]]
then    
    g.extension extension=r.stream.basins
fi

# ===========================================================
# Process MERIT data (from Dai Yamazaki)
# ===========================================================

for DIR in glb_01min_d8 glb_03min_d8 glb_05min_d8 glb_06min_d8 glb_15min_d8
do
    res=$(echo ${DIR} | sed 's/\(glb\)_\(.*\)_d8/\2/')
    for map in basin bsncol catmpx ctmare downxy elevtn flwdir grdare lonlat lsmask nextxy rivseq uparea upgrid upixel width
    do
	# First convert GRADS binary to netCDF
	NCFILE=${AUXDIR}/hydro/${map}_${res}.nc
	if [[ ! -f ${NCFILE} || $OVERWRITE == '--overwrite' ]]
	then	    
    	    cdo -f nc import_binary ${CAMADIR}/${DIR}/${map}.ctl ${NCFILE}
	fi
	# Then transfer netCDF file to geotiff
	IMGFILE=${AUXDIR}/hydro/${map}_${res}.tif
	if [[ ! -f ${IMGFILE} || $OVERWRITE == '--overwrite' ]]
	then	    
    	    gdal_translate -of GTiff -a_srs EPSG:4326 ${NCFILE} ${IMGFILE}
	fi	
    done
done

# ===========================================================
# MERIT: Flow direction, accumulation
# ===========================================================

declare -a MERIT_RGNS=(0.25 0.1 0.0833333333333 0.05 0.016666666666)

r.in.gdal \
    -a \
    -o \
    input=${AUXDIR}/hydro/flwdir_15min.tif \
    output=merit_draindir_trip_igp_0.250000Deg_init \
    --overwrite

r.in.gdal \
    -a \
    -o \
    input=${AUXDIR}/hydro/flwdir_06min.tif \
    output=merit_draindir_trip_igp_0.100000Deg_init \
    --overwrite

r.in.gdal \
    -a \
    -o \
    input=${AUXDIR}/hydro/flwdir_05min.tif \
    output=merit_draindir_trip_igp_0.083333Deg_init \
    --overwrite

r.in.gdal \
    -a \
    -o \
    input=${AUXDIR}/hydro/flwdir_03min.tif \
    output=merit_draindir_trip_igp_0.050000Deg_init \
    --overwrite

r.in.gdal \
    -a \
    -o \
    input=${AUXDIR}/hydro/flwdir_01min.tif \
    output=merit_draindir_trip_igp_0.016667Deg_init \
    --overwrite

# change values of -1 (inland depression) to 0
for MERIT_RGN in "${MERIT_RGNS[@]}"
do
    MERIT_RGN_STR=igp_$(printf "%0.6f" ${MERIT_RGN})Deg
    g.region region=${MERIT_RGN_STR}    
    r.mapcalc \
	"merit_draindir_trip_${MERIT_RGN_STR} = if(merit_draindir_trip_${MERIT_RGN_STR}_init==-1,0,merit_draindir_trip_${MERIT_RGN_STR}_init)" \
	--overwrite
done

if [ -f /tmp/draindir_rcl.txt ]
then
    rm -f /tmp/draindir_rcl.txt
fi

# reclass rules (TRIP -> GRASS)
echo "0 = 0
1       = 2
2       = 1
3       = 8
4       = 7
5       = 6
6       = 5
7       = 4
8       = 3" > /tmp/draindir_rcl.txt

for MERIT_RGN in "${MERIT_RGNS[@]}"
do
    MERIT_RGN_STR=igp_$(printf "%0.6f" ${MERIT_RGN})Deg
    g.region region=${MERIT_RGN_STR}

    # reclass flow direction map so that it meets the
    # requirements of r.accumulate:
    # https://grass.osgeo.org/grass76/manuals/addons/r.accumulate.html 
    # 3 2 1
    # 4 X 8
    # 5 6 7
    r.reclass \
	input=merit_draindir_trip_${MERIT_RGN_STR} \
	output=merit_draindir_grass_${MERIT_RGN_STR} \
	rules=/tmp/draindir_rcl.txt \
	--overwrite
    # run accumulation algorithm, which is based solely on
    # flow direction map.

    # N.B. `r.accumulate` is an addon - to use it must be installed
    # from the grass command prompt with the following command:
    # g.extension extension=r.accumulate    
    r.accumulate \
	direction=merit_draindir_grass_${MERIT_RGN_STR} \
	format=45degree \
	accumulation=merit_accum_cell_${MERIT_RGN_STR}_init \
	--overwrite


    # subtract 1, because JULES requires that the accumulation
    # does not include the cell itself
    r.mapcalc \
	"merit_accum_cell_${MERIT_RGN_STR} = merit_accum_cell_${MERIT_RGN_STR}_init - 1" \
	--overwrite

    # write output
    r.out.gdal \
	input=merit_accum_cell_${MERIT_RGN_STR} \
	output=${DATADIR}/merit_accum_cell_${MERIT_RGN_STR}.tif \
	--overwrite

    r.out.gdal \
	input=merit_draindir_grass_${MERIT_RGN_STR} \
	output=${DATADIR}/merit_draindir_grass_${MERIT_RGN_STR}.tif \
	--overwrite

    g.remove -f type=raster name=merit_accum_cell_${MERIT_RGN_STR}_init
done    

# Make basins
g.region region=igp_0.100000Deg

# NB the `sed` command skips the first line
sed 1d "${DATADIR}/command_inlets_0.100000Deg.txt" | while IFS=" " read -r id lon lat
do
    r.water.outlet \
	input=merit_draindir_grass_igp_0.100000Deg \
	output=command_${id}_upstream_area \
	coordinates=$lon,$lat \
	--overwrite
    
    r.out.gdal \
	input=command_${id}_upstream_area \
	output=${DATADIR}/command_${id}_upstream_area_0.100000Deg.tif \
	--overwrite    
done

# # OLD:
# v.in.ogr \
#     input=${DATADIR}/command_inlets_0.100000Deg.gpkg \
#     output=command_inlets_0_100000Deg \
#     type=point \
#     key=LPJID \
#     --overwrite

# g.region region=igp_0.100000Deg

# r.stream.basins \
#     direction=merit_draindir_grass_igp_0.100000Deg \
#     points=command_inlets_0_100000Deg \
#     basins=command_upstream_basin_igp_0.100000Deg \
#     --overwrite

# r.out.gdal \
#     input=command_upstream_basin_igp_0.100000Deg \
#     output=${DATADIR}/command_upstream_basin_igp_0.100000Deg.tif \
#     --overwrite

# # ===========================================================
# # HydroSHEDS: 1km flow direction, accumulation 
# # ===========================================================

# HYDRO_RGN=0.0083333333333
# HYDRO_RGN_STR=globe_$(printf "%0.6f" ${HYDRO_RGN})Deg
# g.region region=${HYDRO_RGN_STR}

# # Process raw HydroSHEDS data
# declare -a CONTINENTS=(af as au ca eu na sa)
# for CONTINENT in "${CONTINENTS[@]}"
# do
#     for PRODUCT in dir acc
#     do
# 	OUTFILE=${AUXDIR}/dem/${CONTINENT}_${PRODUCT}_30s.tif
# 	if [[ ! -f $OUTFILE || $OVERWRITE == '--overwrite' ]]
# 	then	    
# 	    unzip -o ${HYDRODIR}/${CONTINENT}_${PRODUCT}_30s_grid.zip -d ${HYDRODIR}
# 	    gdal_translate -of GTiff ${HYDRODIR}/${CONTINENT}_${PRODUCT}_30s/${CONTINENT}_${PRODUCT}_30s $OUTFILE
# 	fi	
#     done
# done

# # Mosaic above two files

# # ##################################### #
# # Direction
# # ##################################### #

# VRTFILE=${AUXDIR}/dem/hydrosheds_dir_0.008333Deg.vrt
# if [[ ! -f $VRTFILE || $OVERWRITE == '--overwrite' ]]
# then
#     if [ -f /tmp/hydrosheds_dir_filenames.txt ]
#     then
# 	rm -f /tmp/hydrosheds_dir_filenames.txt
#     fi
    
#     find \
# 	${AUXDIR}/dem \
# 	-regextype posix-extended \
# 	-regex ".*/[a-z]+_dir_30s.tif$" \
# 	> /tmp/hydrosheds_dir_filenames.txt
#     if [ -f ${VRTFILE} ]
#     then
# 	rm -f ${VRTFILE}
#     fi    
#     gdalbuildvrt \
# 	-overwrite \
# 	-te -180 -90 180 90 \
# 	-tr 0.0083333333333 0.0083333333333 \
# 	-input_file_list /tmp/hydrosheds_dir_filenames.txt \
# 	${VRTFILE}
# fi

# # import to GRASS
# r.in.gdal \
#     -a \
#     -o \
#     input=${VRTFILE} \
#     output=hydrosheds_dir_${HYDRO_RGN_STR}_init \
#     $OVERWRITE
# # r.external -a input=${VRTFILE} output=hydrosheds_dir_${HYDRO_RGN_STR}_init $OVERWRITE

# # reclass rules (HydroSHEDS -> TRIP)
# if [ -f /tmp/trip_draindir_rcl.txt ]
# then
#     rm -f /tmp/trip_draindir_rcl.txt
# fi

# echo "0 = 0
# -1      = 0
# 1       = 3
# 2       = 4
# 4       = 5
# 8       = 6
# 16      = 7
# 32      = 8
# 64      = 1
# 128     = 2" > /tmp/trip_draindir_rcl.txt

# r.reclass \
#     input=hydrosheds_dir_${HYDRO_RGN_STR}_init \
#     output=hydrosheds_dir_trip_${HYDRO_RGN_STR}_init \
#     rules=/tmp/trip_draindir_rcl.txt \
#     $OVERWRITE

# r.mapcalc \
#     "hydrosheds_draindir_trip_${HYDRO_RGN_STR} = hydrosheds_dir_trip_${HYDRO_RGN_STR}_init" \
#     $OVERWRITE

# # ##################################### #
# # Accumulation
# # ##################################### #

# VRTFILE=${AUXDIR}/dem/hydrosheds_acc_0.008333Deg.vrt
# if [[ ! -f $VRTFILE || $OVERWRITE == '--overwrite' ]]
# then
#     if [ -f /tmp/hydrosheds_acc_filenames.txt ]
#     then
# 	rm -f /tmp/hydrosheds_acc_filenames.txt
#     fi
    
#     find \
# 	${AUXDIR}/dem \
# 	-regextype posix-extended \
# 	-regex ".*/[a-z]+_acc_30s.tif$" \
# 	> /tmp/hydrosheds_acc_filenames.txt
#     if [ -f ${VRTFILE} ]
#     then
# 	rm -f ${VRTFILE}
#     fi    
#     gdalbuildvrt \
# 	-overwrite \
# 	-te -180 -90 180 90 \
# 	-tr 0.0083333333333 0.0083333333333 \
# 	-input_file_list /tmp/hydrosheds_acc_filenames.txt \
# 	${VRTFILE}
# fi

# r.in.gdal \
#     -a \
#     input=${VRTFILE} \
#     output=hydrosheds_acc_${HYDRO_RGN_STR}_init \
#     $OVERWRITE
# # r.external -a input=${VRTFILE} output=hydrosheds_acc_${HYDRO_RGN_STR}_init $OVERWRITE
# r.mapcalc \
#     "hydrosheds_accum_cell_${HYDRO_RGN_STR} = hydrosheds_acc_${HYDRO_RGN_STR}_init - 1" \
#     $OVERWRITE
