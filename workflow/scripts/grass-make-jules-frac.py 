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

from utils import write_reclass_rules
from constants import (esa_land_covers,
                       jules_land_covers,
                       jules_land_cover_components)

# Write the reclass rules that we need
write_reclass_rules()

# Directory where ESA CCI LC data is stored
esacci_datadir = snakemake.config['esacci_datadir']

# Now we can create a grass session instance
my_gisdb = os.path.join(os.getenv('HOME'), 'grassdata')
my_location = 'latlong'
my_mapset = 'gwm'

user = Session()
user.open(gisdb=my_gisdb, location=my_location, mapset=my_mapset)

try:
    r.mask(flags="r")
except:
    pass

# Import tropical forest area and infill
input_map = 'resources/wwf_terr_ecos.shp'
output_map = 'results/intermediate/wwf_terr_ecos_globe_0.008333Deg.tif'
gdal_rasterize_command = (
    'gdal_rasterize -at -te -180 -90 180 90 -ts 43200 21600 -a BIOME '
    f'{input_map} {output_map}'
)
os.system(gdal_rasterize_command)

grass_input_map = 'wwf_terr_ecos_globe_0.008333Deg'
r.in_gdal(flags="a", input=output_map, output=grass_input_map, overwrite=True)
r.null(map=grass_input_map, setnull=0)
r.grow_distance(
    input=grass_input_map,
    value=grass_input_map + '_interp',
    overwrite=True
)
r.mapcalc(
    f'tropical_broadleaf_forest_globe_0.008333Deg = if(({grass_input_map}_interp == 1) | ({grass_input_map}_interp == 2), 1, 0)'
)

# Read C4 fraction data
r.in_gdal(
    flags='a',
    input='results/intermediate/c4_nat_veg_frac_globe_0.008333Deg.tif',
    output='c4_nat_veg_frac_globe_0.008333Deg',
    overwrite=True
)
r.in_gdal(
    flags='a',
    input='results/intermediate/c4_crop_frac_globe_0.008333Deg.tif',
    output='c4_crop_frac_globe_0.008333Deg',
    overwrite=True
)

# Make JULES land cover fraction maps
lc_years = [2015]
g.region(region='igp_0.002778Deg')
r.mask(raster='esacci_land_frac_globe_0.002778Deg')

# Land sea mask @ 0.002778Deg
r.mapcalc('land_sea_mask = esacci_land_frac_globe_0.002778Deg', overwrite=True)
r.mask(flags='r')

for (year in lc_years):
    g.region(region='igp_0.002778Deg')
    input_map = os.path.join(
        esacci_datadir,
        f'ESACCI-LC-L4-LCCS-Map-300m-P1Y-{year}-v2.0.7.tif'
    )
    esa_lc_w_sea = f'esacci_lc_{year}_w_sea'
    r.in_gdal(flags='a', input=input_map, output=esa_lc_w_sea, overwrite=True)

    # Mask sea values in esacci_lc_${YEAR}_w_sea by multiplying by
    # land_sea_mask, in which non-land cells are null.
    esa_lc = f'esacci_lc_{year}'
    r.mapcalc(f'{esa_lc} = {esa_lc_w_sea} * land_sea_mask', overwrite=True)

    # ############################################################### #
    # Resample ESA land cover maps to get fractional cover
    # ############################################################### #

    for lc in esa_land_covers:
        # Recode at native resolution
        g.region(region='igp_0.002778Deg')
        r.reclass(
            input=esa_lc,
            output=f'esacci_lc_{year}_{lc}_x1000',
            rules=os.path.join('results/intermediate', f'{lc}_reclass.txt'),
            overwrite=True
        )
        # Resample at target resolution
        g.region(region='igp_0.008333Deg')
        r.resamp_stats(
            input=f'esacci_lc_{year}_{lc}_x1000',
            output=f'esacci_lc_{year}_{lc}_igp_0.008333Deg_x1000',
            method='average',
            overwrite=True
        )
        r.mapcalc(
            f'esacci_lc_{year}_{lc}_igp_0.008333Deg = esacci_lc_{year}_{lc}_igp_0.008333Deg_x1000 / 1000',
            overwrite=True
        )

    # Set up external output - this is more memory efficient
    r.external_out(
        directory='results/intermediate/frac',
	    format='GTiff', options='COMPRESS=DEFLATE'
    )

    # ############################################################### #
    # Additional agricultural land cover classes
    # ############################################################### #

    # Rainfed cropland (classes 10, 11)
    g.region(region='igp_0.0027778Deg')
    rainfed_cropland_map = f'esacci_lc_{year}_rainfed_cropland_igp_0.008333Deg.tif'
    irrigated_cropland_map = f'esacci_lc_{year}_irrigated_cropland_igp_0.008333Deg.tif'
    combined_cropland_map = f'esacci_lc_{year}_combined_igp_0.008333Deg.tif'

    r.mapcalc(
        f'esacci_lc_{year}_rainfed_cropland.tif = if(({esa_lc} == 10) || ({esa_lc} == 11), 1, 0)',
        overwrite=True
    )
    g.region(region='igp_0.008333Deg')
    r.resamp_stats(
        input=f'esacci_lc_{year}_rainfed_cropland.tif',
        output=rainfed_cropland_map,
        method='average',
        overwrite=True
    )

    # Irrigated cropland (class 20)
    g.region(region='igp_0.0027778Deg')
    r.mapcalc(
        f'esacci_lc_{year}_irrigated_cropland.tif = if(({esa_lc} == 20), 1, 0)',
        overwrite=True
    )
    g.region(region='igp_0.008333Deg')
    r.resamp_stats(
        input=f'esacci_lc_{year}_irrigated_cropland.tif',
        output=irrigated_cropland_map,
        method='average',
        overwrite=True
    )

    # Combined (rainfed + irrigated)
    r.mapcalc(
        f'{combined_cropland_map} = {rainfed_cropland_map} + {irrigated_cropland_map}'
        overwrite=True
    )

    # ############################################################### #
    # Write JULES land cover maps
    # ############################################################### #

    for lc in jules_land_covers:
        combined_map = f'lc_{lc}_combined_{year}_igp_0.008333Deg.tif'
        natural_map = f'lc_{lc}_natural_{year}_igp_0.008333Deg.tif'
        rainfed_map = f'lc_{lc}_rainfed_{year}_igp_0.008333Deg.tif'
        irrigated_map = f'lc_{lc}_irrigated_{year}_igp_0.008333Deg.tif'

        if lc in ['c4_grass', 'c3_grass']:
            # Here we divide C3/C4 grass into rainfed/irrigated/natural C3/C4 grass
            natural_grass_map = f'esacci_lc_{year}_natural_grass_igp_0.008333Deg'
            managed_grass_map = f'esacci_lc_{year}_managed_grass_igp_0.008333Deg'
            c4_nat_veg_frac_map = f'c4_nat_veg_frac_globe_0.008333Deg'
            c4_crop_frac_map = f'c4_crop_frac_globe_0.008333Deg'
            if lc == 'c4_grass':
                r.mapcalc(
                    f'{combined_map} = ({natural_grass_map} * {c4_nat_veg_frac_map}) + ({managed_grass_map} * {c4_crop_frac_map})',
                    overwrite=True
                )
                r.mapcalc(
                    f'{natural_map} = {combined_map} - ({combined_cropland_map} * {c4_crop_frac_map}',
                    overwrite=True
                )
                r.mapcalc(
                    f'{rainfed_map} = {rainfed_cropland_map} * {c4_crop_frac_map}',
                    overwrite=True
                )
                r.mapalc(
                    f'{irrigated_map} = {irrigated_cropland_map} * {c4_crop_frac_map}',
                    overwrite=True
                )
            else:
                r.mapcalc(
                    f'{combined_map} = ({natural_grass_map} * (1 - {c4_nat_veg_frac_map})) + ({managed_grass_map} * (1 - {c4_crop_frac_map}))',
                    overwrite=True
                )
                r.mapcalc(
                    f'{natural_map} = {combined_map} - ({combined_cropland_map} * (1 - {c4_crop_frac_map}))',
                    overwrite=True
                )
                r.mapcalc(
                    f'{rainfed_map} = {rainfed_cropland_map} * (1 - {c4_crop_frac_map})',
                    overwrite=True
                )
                r.mapalc(
                    f'{irrigated_map} = {irrigated_cropland_map} * (1 - {c4_crop_frac_map})',
                    overwrite=True
                )

        if lc in ['tree_broadleaf_evergreen_tropical', 'tree_broadleaf_evergreen_temperate']:
            if lc == 'tree_broadleaf_evergreen_tropical':
                r.mapcalc(
                    f'{combined_map} = esacci_lc_{year}_tree_broadleaf_evergreen_igp_0.008333Deg * tropical_broadleaf_forest_globe_0.008333Deg'
                )
            else:
                r.mapcalc(
                    f'{combined_map} = esacci_lc_{year}_tree_broadleaf_evergreen_temperate * (1 - tropical_broadleaf_forest_globe_0.008333Deg)'
                )

            r.mapcalc(f'{natural_map} = {combined_map}', overwrite=True)
            r.mapcalc(f'{rainfed_map} = 0', overwrite=True)
            r.mapcalc(f'{irrigated_map} = 0', overwrite=True)

        else:
            parts = jules_land_cover_components[lc]
            parts = [f'esacci_lc_{year}_{part}_igp_0.008333Deg' for part in parts]
            r_mapcalc_eqn = f'{combined_map} = ' + ' + '.join(parts)
            r.mapcalc(r_mapcalc_eqn, overwrite=True)
            r.mapcalc(f'{natural_map} = {combined_map}', overwrite=True)
            r.mapcalc(f'{rainfed_map} = 0', overwrite=True)
            r.mapcalc(f'{irrigated_map} = 0', overwrite=True)

    # Turn off external output
    r.external_out(flags="r")


