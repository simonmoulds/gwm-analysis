#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import click
import numpy as np
import netCDF4

from write_jules_frac import write_jules_frac, write_jules_frac_ants
# from write_jules_land_frac import write_jules_land_frac
# from write_jules_latlon import write_jules_latlon
# from write_jules_overbank_props import write_jules_overbank_props
# from write_jules_pdm import write_jules_pdm
# from write_jules_rivers_props import write_jules_rivers_props
# from write_jules_soil_props import write_jules_soil_props
# from write_jules_top import write_jules_top

# Read environment variables from parent
ONE_D = False
# ONE_D = bool(int(os.environ['ONE_D']))
# PDM = bool(int(os.environ['PDM']))
# TOPMODEL = bool(int(os.environ['TOPMODEL']))
# ROUTING = bool(int(os.environ['ROUTING']))
# OVERBANK = bool(int(os.environ['OVERBANK']))
NINEPFT = bool(int(os.environ['NINEPFT']))
FIVEPFT = bool(int(os.environ['FIVEPFT']))
ANTSFORMAT = True
# ANTSFORMAT = bool(int(os.environ['ANTSFORMAT']))
# LAND_FRAC = bool(int(os.environ['LAND_FRAC']))
# LATLON = bool(int(os.environ['LATLON']))
# COSBY = bool(int(os.environ['COSBY']))
# TOMAS = bool(int(os.environ['TOMASELLA']))
# ROSETTA = bool(int(os.environ['ROSETTA']))
REGION = str(os.environ['REGION'])
PRODUCT = str(os.environ['PRODUCT'])
OUTDIR = str(os.environ['OUTDIR'])

# names of JULES land cover types
LC_NAMES_5PFT = [
    'tree_broadleaf', 'tree_needleleaf',
    'shrub', 'c4_grass', 'c3_grass',
    'urban', 'water', 'bare_soil', 'snow_ice'
]
LC_NAMES_9PFT = [
    'tree_broadleaf_evergreen_tropical',
    'tree_broadleaf_evergreen_temperate',
    'tree_broadleaf_deciduous',
    'tree_needleleaf_evergreen',
    'tree_needleleaf_deciduous', 'shrub_evergreen',
    'shrub_deciduous', 'c4_grass', 'c3_grass',
    'urban', 'water', 'bare_soil', 'snow_ice'
]
LU_NAMES = ['combined', 'natural', 'rainfed', 'irrigated']

@click.command()
@click.option(
    '-d', 'destdir', nargs=1, default='.', type=str,
    help='Destination directory.'
)
def main(destdir):
    file_suffix = PRODUCT + '_' + REGION + '.nc'    
    years = [2015]
    for year in years:
        for landuse in LU_NAMES:
            if FIVEPFT:
                lc_names = [lc + '_' + landuse for lc in LC_NAMES_5PFT]
                if ANTSFORMAT:
                    frac_fn = os.path.join(
                        destdir,
                        'jules_frac_' + landuse + '_5pft_ants_' + str(year) + '_' + file_suffix
                    )
                    write_jules_frac_ants(
                        year,
                        lc_names,
                        # LC_NAMES_5PFT,
                        frac_fn
                    )

                frac_fn = os.path.join(
                    destdir,
                    'jules_frac_' + landuse + '_5pft_' + str(year) + '_' + file_suffix
                )
                write_jules_frac(
                    year,
                    lc_names,
                    # LC_NAMES_5PFT,
                    frac_fn,
                    ONE_D
                )

            if NINEPFT:            
                lc_names = [lc + landuse for lc in LC_NAMES_9PFT]
                if ANTSFORMAT:
                    frac_fn = os.path.join(
                        destdir,
                        'jules_frac' + landuse + '_9pft_ants_' + str(year) + '_' + file_suffix
                    )
                    write_jules_frac_ants(
                        year,
                        lc_names,
                        # LC_NAMES_9PFT,
                        frac_fn
                    )

                frac_fn = os.path.join(
                    destdir,
                    'jules_frac' + landuse + '_9pft_' + str(year) + '_' + file_suffix
                )
                write_jules_frac(
                    year,
                    lc_names,
                    # LC_NAMES_9PFT,
                    frac_fn,
                    ONE_D)

if __name__ == '__main__':
    main()
