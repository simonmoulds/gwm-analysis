#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import click
import numpy as np
import netCDF4

from write_jules_frac import write_jules_frac_ants

# Read environment variables from parent
ONE_D = False
NINEPFT = bool(int(os.environ['NINEPFT']))
FIVEPFT = bool(int(os.environ['FIVEPFT']))
REGION = str(os.environ['REGION'])
PRODUCT = str(os.environ['PRODUCT'])
OUTDIR = str(os.environ['OUTDIR'])

# names of JULES land cover types
LC_NAMES_5PFT = [
    'tree_broadleaf_natural',
    'tree_needleleaf_natural',
    'c3_grass_natural',
    'c4_grass_natural',
    'shrub_natural',
    'rainfed',
    'irrigated_single_season',
    'irrigated_double_season',
    'irrigated_triple_season',
    'irrigated_continuous',
    'fallow',
    'urban_natural',
    'water_natural',
    'bare_soil_natural',
    'snow_ice_natural'
]
LC_NAMES_9PFT = [
    'tree_broadleaf_evergreen_tropical_natural',
    'tree_broadleaf_evergreen_temperate_natural',
    'tree_broadleaf_deciduous_natural',
    'tree_needleleaf_evergreen_natural',
    'tree_needleleaf_deciduous_natural',
    'c3_grass_natural',
    'c4_grass_natural',
    'shrub_evergreen_natural',
    'shrub_deciduous_natural',
    'rainfed',
    'irrigated_single_season',
    'irrigated_double_season',
    'irrigated_triple_season',
    'irrigated_continuous',
    'fallow',
    'urban_natural',
    'water_natural',
    'bare_soil_natural',
    'snow_ice_natural'
]

@click.command()
@click.option(
    '-d', 'destdir', nargs=1, default='.', type=str,
    help='Destination directory.'
)
def main(destdir):
    file_suffix = PRODUCT + '_' + REGION + '.nc'    
    years = [2015]
    for year in years:
        if FIVEPFT:
            frac_fn = os.path.join(
                destdir,
                'jules_frac_5pft_ants_' + str(year) + '_' + file_suffix
            )
            write_jules_frac_ants(
                year,
                LC_NAMES_5PFT,
                frac_fn
            )

        if NINEPFT:
            frac_fn = os.path.join(
                destdir,
                'jules_frac_9pft_ants_' + str(year) + '_' + file_suffix
            )
            write_jules_frac_ants(
                year,
                LC_NAMES_9PFT,
                frac_fn
            )

if __name__ == '__main__':
    main()
