#!/usr/bin/env python3

import os
import subprocess

from grass_session import Session
from grass.script import core as gcore
import grass.script as gscript
import grass.script.setup as gsetup

# Import grass python libraries
from grass.pygrass.modules.shortcuts import general as g
from grass.pygrass.modules.shortcuts import raster as r
from grass.pygrass.modules.shortcuts import vector as v
from grass.pygrass.modules.shortcuts import temporal as t

# Remove mask if present
r.mask(flags="r")

esacci_datadir = snakemake.config['esacci_datadir']

# We construct land frac using ESA CCI data
#
# There is some discrepancy between ESA_CCI_WB and ESA_CCI_LC. To get
# around this we implement a two-step procedure

# (i)  Aggregate 150m data to 300m by taking the minimum value.
#      As ocean=0, land=1, inland=2, this means that coarse grid
#      cells containing any number of fine resolution ocean grid
#      cells will also be classified as ocean.
input_map = os.path.join(
    esacci_datadir,
    'ESACCI-LC-L4-WB-Ocean-Land-Map-150m-P13Y-2000-v4.0.tif'
)
output_map = os.path.join(
    'results/input',
    'water_bodies_min_${RGN_STR}.tif'
)
res = 1 / 360.
gdalwarp_command = (
    f'gdalwarp -overwrite -te -180 -90 180 90 -tr {res} {res} '
    f'-r min {input_map} {output_map}'
)
subprocess.call(gdalwarp_command)

# Import the map to our grass session
r.in_gdal(
    flags='a', input=output_map,
    output='water_bodies_min', overwrite=True
)
rgn_str = f'globe_{res:.6f}Deg'
g.region(region=rgn_str)
r.mapcalc(
    'ocean_min = if(water_bodies_min==0, 1, 0)',
    overwrite=True
)

# (ii) Import land cover map (use 2015 as base year), and simplify
#      to land/water mask (water is code 210)
lc_reference_year = 2015
input_map = os.path.join(
    esacci_datadir,
    f'ESACCI-LC-L4-LCCS-Map-300m-P1Y-${lc_reference_year}-v2.0.7.tif'
)
output_map = f'esacci_lc_{lc_reference_year}'
r.in_gdal(
    flags='a', input=input_map,
    output=output_map, overwrite=True
)
r.mapcalc(
    f'esacci_lc_water = if({output_map}==210, 1, 0)',
    overwrite=True
)

# Calculate ocean grid cells as cells in which ESA CCI LC is classified as
# water *and* ESA CCI WB (@ 300m) is classified as ocean.
r.mapcalc(
    'ocean = if((ocean_min==1 && esacci_lc_water==1), 1, 0)',
    overwrite=True
)
r.mapcalc(
    f'esacci_land_frac_{rgn_str} = 1 - ocean',
    overwrite=True
)

# Write output at multiple resolutions (used in the jules_frac routine)
rgns = [0.25, 0.1, 1/12., 0.05, 1/60.]
for rgn in rgns:
    new_rgn_str = f'globe_{rgn:.6f}Deg'
    g.region(region=rgn_str)
    r.resamp_stats(
        flags='w', input=f'esacci_land_frac_{rgn_str}',
        output='tmp', method='average', overwrite=True
    )
    r.mapcalc(
        f'esacci_land_frac_{new_rgn_str} = if(tmp>0, 1, 0)',
        overwrite=True
    )
    g.remove(flags='f', type='raster', name='tmp')


# Bash version:
#
# # ===========================================================
# # Region based on ESA CCI data
# # ===========================================================

# # There is some discrepancy between ESA_CCI_WB and ESA_CCI_LC. To get
# # around this we implement a two-step procedure:
# # (i)  Aggregate by taking the minimum value, which will in effect
# #      classify the 300m grid square as ocean if *any* fine resolution
# #      grid squares are classified as ocean.
# # (ii) Use the 2015 land cover map to identify ocean cells *if* the
# #      LC map contains water (code 210) *and* the map created in (i)
# #      is classified as ocean.

# RGN=0.002777777777777777
# RGN_STR=globe_$(printf "%0.6f" ${RGN})Deg

# # (i)  Aggregate 150m data to 300m by taking the minimum value.
# #      As ocean=0, land=1, inland=2, this means that coarse grid
# #      cells containing any number of fine resolution ocean grid
# #      cells will also be classified as ocean.
# gdalwarp \
#     -overwrite \
#     -te -180 -90 180 90 \
#     -tr 0.002777777777777777777 0.002777777777777777777 \
#     -r min \
#     $ESACCIDIR/ESACCI-LC-L4-WB-Ocean-Land-Map-150m-P13Y-2000-v4.0.tif \
#     $AUXDIR/land_frac/water_bodies_min_${RGN_STR}.tif

# # Import these external data sources
# r.in.gdal \
#     -a \
#     input=$AUXDIR/land_frac/water_bodies_min_${RGN_STR}.tif \
#     output=water_bodies_min \
#     $OVERWRITE

# g.region region=globe_0.002778Deg
# g.region -p
# r.mapcalc "ocean_min = if(water_bodies_min == 0, 1, 0)" $OVERWRITE

# # (ii) Import land cover map (use 2015 as base year), and simplify
# #      to land/water mask (water is code 210)
# YEAR=2015
# r.in.gdal \
#     -a \
#     input=$ESACCIDIR/ESACCI-LC-L4-LCCS-Map-300m-P1Y-${YEAR}-v2.0.7.tif \
#     output=esacci_lc_${YEAR} \
#     $OVERWRITE

# g.region region=globe_0.002778Deg
# g.region -p
# r.mapcalc "esacci_lc_water = if(esacci_lc_${YEAR} == 210, 1, 0)" $OVERWRITE

# # Calculate ocean grid cells as cells in which ESA CCI LC is classified as
# # water *and* ESA CCI WB (@ 300m) is classified as ocean.
# r.mapcalc "ocean = if((ocean_min==1 && esacci_lc_water==1), 1, 0)" $OVERWRITE
# r.mapcalc \
#     "esacci_land_frac_${RGN_STR} = 1 - ocean" \
#     $OVERWRITE

# # Write output at multiple resolutions (used in the jules_frac routine)
# declare -a RGNS=(0.25 0.1 0.083333333333333 0.05 0.01666666666666 0.008333333333333)
# for RGN in "${RGNS[@]}"
# do
#     RGN_STR=globe_$(printf "%0.6f" ${RGN})Deg
#     g.region region=${RGN_STR}
#     r.resamp.stats \
# 	-w \
# 	input=esacci_land_frac_globe_0.002778Deg \
# 	output=esacci_land_frac_${RGN_STR}_tmp \
# 	method=average \
# 	--overwrite
#     r.mapcalc \
# 	"esacci_land_frac_${RGN_STR} = if(esacci_land_frac_${RGN_STR}_tmp>0,1,0)" \
# 	--overwrite
#     g.remove -f type=raster name=esacci_land_frac_${RGN_STR}_tmp
# done
