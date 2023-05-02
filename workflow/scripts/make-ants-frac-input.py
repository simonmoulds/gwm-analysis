#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import click
import shutil
import numpy as np
import netCDF4

from write_jules_frac import write_jules_frac_ants

# Read environment variables from parent
ONE_D = False
NINEPFT = bool(int(os.environ['NINEPFT']))
FIVEPFT = bool(int(os.environ['FIVEPFT']))
ANTSFORMAT = True
REGION = str(os.environ['REGION'])
PRODUCT = str(os.environ['PRODUCT'])
OUTDIR = str(os.environ['OUTDIR'])

LC_NAMES_5PFT = [
    'tree_broadleaf',
    'tree_needleleaf',
    'c3_grass',
    'c4_grass',
    'shrub',
    'urban',
    'water',
    'bare_soil',
    'snow_ice'
]
LC_NAMES_9PFT = [
    'tree_broadleaf_evergreen_tropical',
    'tree_broadleaf_evergreen_temperate',
    'tree_broadleaf_deciduous',
    'tree_needleleaf_evergreen',
    'tree_needleleaf_deciduous',
    'c3_grass',
    'c4_grass',
    'shrub_evergreen',
    'shrub_deciduous',
    'urban',
    'water',
    'bare_soil',
    'snow_ice'
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
                frac_fn = os.path.join(
                    destdir,
                    'jules_frac_' + landuse + '_5pft_ants_' + str(year) + '_' + file_suffix
                )
                write_jules_frac_ants(
                    year,
                    lc_names,
                    frac_fn
                )
                if landuse in ['rainfed', 'irrigated']:
                    # create a file which assumes all crops are c3
                    frac_wo_c4_fn = os.path.join(
                        destdir,
                        'jules_frac_' + landuse + '_no_c4_crops_5pft_ants_' + str(year) + '_' + file_suffix
                        )
                    shutil.copy2(frac_fn, frac_wo_c4_fn)
                    nco = netCDF4.Dataset(frac_wo_c4_fn, 'r+')
                    frac = nco['land_cover_lccs'][:].copy()
                    frac[2, ...] += frac[3, ...]
                    frac[3, ...] = 0
                    nco['land_cover_lccs'][:] = frac
                    nco.close()

                if landuse in ['rainfed']:
                    # to create the fallow input we copy the data for
                    # rainfed as the proportion devoted to c3/c4 will
                    # be the same
                    fallow_frac_fn = os.path.join(
                        destdir,
                        'jules_frac_fallow_5pft_ants_' + str(year) + '_' + file_suffix
                    )
                    shutil.copy2(frac_fn, fallow_frac_fn)
                    fallow_frac_wo_c4_fn = os.path.join(
                        destdir,
                        'jules_frac_fallow_no_c4_crops_5pft_ants_' + str(year) + '_' + file_suffix
                    )
                    shutil.copy2(frac_wo_c4_fn, fallow_frac_wo_c4_fn)                        
                
            if NINEPFT:            
                lc_names = [lc + landuse for lc in LC_NAMES_9PFT]
                frac_fn = os.path.join(
                    destdir,
                    'jules_frac_' + landuse + '_9pft_ants_' + str(year) + '_' + file_suffix
                )
                write_jules_frac_ants(
                    year,
                    lc_names,
                    frac_fn
                )
                if landuse in ['rainfed', 'irrigated']:
                    # create a file which assumes all crops are c3
                    frac_wo_c4_fn = os.path.join(
                        destdir,
                        'jules_frac_' + landuse + '_no_c4_crops_9pft_ants_' + str(year) + '_' + file_suffix
                        )
                    shutil.copy2(frac_fn, frac_wo_c4_fn)
                    nco = netCDF4.Dataset(frac_wo_c4_fn, 'r+')
                    frac = nco['land_cover_lccs'][:].copy()
                    frac[5, ...] += frac[6, ...]
                    frac[6, ...] = 0
                    nco['land_cover_lccs'][:] = frac
                    nco.close()
                
                if landuse in ['rainfed']:
                    # as above, we copy rainfed data to fallow 
                    fallow_frac_fn = os.path.join(
                        destdir,
                        'jules_frac_fallow_9pft_ants_' + str(year) + '_' + file_suffix
                    )
                    shutil.copy2(frac_fn, fallow_frac_fn)
                    fallow_frac_wo_c4_fn = os.path.join(
                        destdir,
                        'jules_frac_fallow_no_c4_crops_9pft_ants_' + str(year) + '_' + file_suffix
                    )
                    shutil.copy2(frac_wo_c4_fn, fallow_frac_wo_c4_fn)                        

if __name__ == '__main__':
    main()
