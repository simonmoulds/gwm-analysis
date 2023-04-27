#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import shutil
import numpy as np
import netCDF4
import rasterio

DATADIR = '/home/sm510/projects/ganges-water-machine/data'

# The purpose of this script is to adjust the calculated
# agricultural land cover fractions with fractions derived
# from ICRISAT.
# 
# Notes:
# ======
# 1. Double, triple and continuous irrigated areas are derived
#    from time series of vegetation indices
# 2. Currently we add triple and continuous irrigated areas
#    to double irrigated areas

# ##################################### #
# Define some constants
# ##################################### #

# '**' = irrigated
land_covers = [
    'tree_broadleaf',           # 0  | 1
    'tree_needleleaf',          # 1  | 2
    'c3_grass',                 # 2  | 3
    'c4_grass',                 # 3  | 4
    'shrub',                    # 4  | 5
    'rainfed',                  # 5  | 6
    'irrigated_single_season',  # 6  | 7 **  [kharif]
    'irrigated_double_season',  # 7  | 8 **  [kharif, rabi]
    'irrigated_triple_season',  # 8  | 9 **  [kharif, rabi, zaid]
    'irrigated_continuous',     # 9  | 10 ** [continuous]
    'fallow',                   # 10 | 11
    'urban',                    # 11 | 12
    'water',                    # 12 | 13
    'bare_soil',                # 13 | 14
    'snow_ice'                  # 14 | 15
]

# Agricultural land covers
agri_land_covers = [
    'rainfed',
    'irrigated_single_season',
    'irrigated_double_season',
    'irrigated_triple_season',
    'irrigated_continuous',
    'fallow'
]

# Land covers which can change to accommodate
# changes in agricultural area
can_vary = [
    'tree_broadleaf',
    'tree_needleleaf',
    'c3_grass',
    'c4_grass',
    'bare_soil'
]

# Land covers which cannot be changed
cannot_vary = [
    'urban',
    'water',
    'snow_ice'
]

# ##################################### #
# Loop through years
# ##################################### #

def read_icrisat_frac(yr, nm):
    fn = "icrisat_" + nm + "_frac_" + str(yr) + "_india_0.500000Deg.tif"
    fpath = os.path.join(DATADIR, "irrigated_area_maps", fn)
    with rasterio.open(fpath) as ds:
        x = ds.read(1, masked=True).squeeze()
    return x

india_frac_fpath = os.path.join(
    DATADIR,
    "irrigated_area_maps",
    "icrisat_india_frac.tif"
)
with rasterio.open(india_frac_fpath) as ds:
    india_frac = ds.read(1, masked=True).squeeze()
    india_mask = (india_frac > 0) * 1.

ICRISAT_START_YEAR = 1979
ICRISAT_END_YEAR = 2015
icrisat_yrs = np.arange(
    ICRISAT_START_YEAR,
    ICRISAT_END_YEAR + 1
)

for yr in icrisat_yrs:
    
    # Load the new irrigated area fractions with which
    # we are to replace the current JULES fractions
    target_rain_frac = read_icrisat_frac(yr, "rainfed")
    target_irr1_frac = read_icrisat_frac(yr, "irrigated_single")
    target_irr2_frac = read_icrisat_frac(yr, "irrigated_double")
    target_irr3_frac = read_icrisat_frac(yr, "irrigated_triple")
    target_cont_frac = read_icrisat_frac(yr, "irrigated_continuous")

    target_sum = np.sum(
        np.ma.stack(
            [target_rain_frac,
             target_irr1_frac,
             target_irr2_frac,
             target_irr3_frac,
             target_cont_frac], axis=0
        ), axis=0
    )

    # Convert the target fractions to fractions
    # of the total cropland area
    def div_fun(x):
        res = np.divide(
            x.data,
            target_sum.data,
            out=np.zeros_like(target_sum.data),
            where=target_sum>0
        )
        res = np.ma.array(res, mask=x.mask)
        return res
    
    target_rain_frac = div_fun(target_rain_frac)
    target_irr1_frac = div_fun(target_irr1_frac)
    target_irr2_frac = div_fun(target_irr2_frac)
    target_irr3_frac = div_fun(target_irr3_frac)
    target_cont_frac = div_fun(target_cont_frac)
    
    # Preallocate dict to store new land cover fractions
    target = {}
    index = {}
    for idx, val in enumerate(land_covers):
        target[val] = np.zeros_like(target_sum)
        index[val] = idx

    # Copy JULES input file
    jules_frac_fn = os.path.join(
        DATADIR, 'netcdf', 'jules_frac_5pft_ants_2015_CUSTOM_igp.nc'
    )
    new_jules_frac_fn = os.path.join(
        DATADIR,
        'jules_frac_5pft_ants_' + str(yr) + '_CUSTOM_igp_adjusted.nc'
    )    
    # Copy file
    shutil.copyfile(jules_frac_fn, new_jules_frac_fn)
    ds = netCDF4.Dataset(new_jules_frac_fn, 'r+')
    
    # Extract the portion of the data covered by ICRISAT data
    current_frac = ds['land_cover_lccs'][:]
    current_frac_india = current_frac * india_frac
    current_frac_not_india = current_frac * (1. - india_frac)

    grass_idx = tuple(index[nm] for nm in ['c3_grass', 'c4_grass'])
    c3_grass_idx = grass_idx[0]
    c4_grass_idx = grass_idx[1]

    # TODO: implement changing irrigated area in Pakistan, Bangladesh, Nepal
    
    # EXPERIMENT: remove this section [i.e. keep agricultural land]
    # *For now*, remove agricultural land from outside India,
    # and add them to C3/C4 grass
    # Add fallow to natural grass
    # grass_idx = tuple(index[nm] for nm in ['c3_grass', 'c4_grass'])
    # c3_grass_idx = grass_idx[0]
    # c4_grass_idx = grass_idx[1]
    # grass_sum = np.sum(current_frac_not_india[grass_idx, ...], axis=0)
    # c3_grass_frac = np.divide(
    #     current_frac_not_india[c3_grass_idx, ...].data,
    #     grass_sum.data,
    #     out=np.ones_like(grass_sum.data),
    #     where=grass_sum>0
    # )
    # agri_idx = tuple(index[nm] for nm in agri_land_covers)
    # agri_not_india = np.sum(current_frac_not_india[agri_idx, ...], axis=0)
    # current_frac_not_india[c3_grass_idx, ...] += (
    #     agri_not_india * c3_grass_frac
    # )
    # current_frac_not_india[c4_grass_idx, ...] += (
    #     agri_not_india * (1 - c3_grass_frac)
    # )    
    # current_frac_not_india[agri_idx, ...] *= 0.  # set to zero
    
    # Get relative quantities, and work with these
    current_frac_india_sum = np.sum(
        current_frac_india, axis=0
    )
    # np.divide(...) does not preserve mask, so work
    # with data only then reapply mask
    current_frac_india_rel = np.divide(
        current_frac_india.data,
        current_frac_india_sum.data,
        out=np.zeros_like(current_frac_india.data),
        where=current_frac_india_sum.data>0
    )
    current_frac_india_rel = np.ma.array(
        current_frac_india_rel,
        mask=current_frac.mask
    )
    india_rel_sum = np.sum(
        current_frac_india_rel,
        axis=0
    )    
    # These fractions must stay the same
    cannot_vary_idx = tuple([index[nm] for nm in cannot_vary])
    cannot_vary_sum = np.sum(
        current_frac_india_rel[cannot_vary_idx, ...],
        axis=0
    )

    # The maximum fraction available for agriculture is the
    # total (i.e. 1.) minus the sum of land covers which are
    # not allowed to change
    max_agri_frac = np.clip(india_rel_sum - cannot_vary_sum, 0, 1.)
    target_sum_adj = np.clip(target_sum, 0., max_agri_frac)
    # Multiply target fractions by the revised target sum
    target_rain_frac *= target_sum_adj
    target_irr1_frac *= target_sum_adj
    target_irr2_frac *= target_sum_adj
    target_irr3_frac *= target_sum_adj
    target_cont_frac *= target_sum_adj

    # Calculate the area remaining after having
    # allocated unvarying and agricultural land covers
    remaining_area = np.clip(india_rel_sum - cannot_vary_sum - target_sum, 0., 1.)

    # Add fallow to natural grass
    fallow_idx = index['fallow']
    grass_sum = np.sum(current_frac_india_rel[grass_idx, ...], axis=0)
    c3_grass_frac = np.divide(
        current_frac_india_rel[c3_grass_idx, ...].data,
        grass_sum.data,
        out=np.ones_like(grass_sum.data),
        where=grass_sum>0
    )
    current_frac_india_rel[c3_grass_idx, ...] += (
        current_frac_india_rel[fallow_idx, ...] * c3_grass_frac
    )
    current_frac_india_rel[c4_grass_idx, ...] += (
        current_frac_india_rel[fallow_idx, ...] * (1 - c3_grass_frac)
    )
    current_frac_india_rel[fallow_idx, ...] *= 0.
    
    # Find the current fractions of land covers which can vary
    can_vary_idx = tuple(index[nm] for nm in can_vary)
    other_frac = current_frac_india_rel[can_vary_idx, ...]
    other_frac_sum = np.sum(other_frac, axis=0)
    other_frac_rel = np.divide(
        other_frac,
        other_frac_sum[None, ...],
        out=np.zeros_like(other_frac),
        where=other_frac_sum>0
    )

    # Calculate whether the target agricultural fraction
    # is greater or less than the current agricultural fraction
    agri_idx = tuple(index[nm] for nm in agri_land_covers)
    agri_frac = current_frac_india_rel[agri_idx, ...]
    agri_frac_sum = np.sum(agri_frac, axis=0)
    # The target agri area is less than the current agri area
    lt_index = target_sum <= agri_frac_sum
    # The target agri area is greater than the current agri area
    gt_index = target_sum > agri_frac_sum
    # Modify values
    for frac_idx, frac_nm in enumerate(can_vary):
        idx = index[frac_nm]
        # In the case where target sum is less than the
        # current agricultural fraction, we will use
        # natural c3/c4 grass as fill, to avoid increasing
        # the forest area without justification
        target[frac_nm][lt_index] = current_frac_india_rel[idx, ...][lt_index]
        # Otherwise, scale all natural land covers
        target[frac_nm][gt_index] = (
            other_frac_rel[frac_idx, ...]
            * remaining_area
        )[gt_index]

    diff = remaining_area - np.sum(np.stack(target.values()), axis=0)
    target['c3_grass'][lt_index] += (diff * c3_grass_frac)[lt_index]
    target['c4_grass'][lt_index] += (diff * (1. - c3_grass_frac))[lt_index]
        
    # Add other land covers
    for frac_idx, frac_nm in enumerate(cannot_vary):
        idx = index[frac_nm]
        target[frac_nm] = current_frac_india_rel[idx, ...]
        
    # Add agricultural land covers
    target['rainfed'] = target_rain_frac
    target['irrigated_single_season'] = target_irr1_frac
    target['irrigated_double_season'] = target_irr2_frac
    target['irrigated_triple_season'] = target_irr3_frac
    target['irrigated_continuous'] = target_cont_frac

    updated_india_frac = np.ma.stack(target.values())

    # Multiply by india_frac to convert relative values to actual values
    updated_india_frac *= india_frac

    # Add to current_frac_not_india to combine datasets
    updated_frac = (
        current_frac_not_india + updated_india_frac
    )

    # THIS IS WHERE THE PROBLEM OCCURS
    # It happens because `current_frac_not_india` actually contains values > 0 for grid cells in India, because ICRISAT is incomplete

    # FIXME (we can definitely improve this method)
    # Add triple and continuous irrigated area to double
    updated_frac[7, ...] += updated_frac[8, ...]
    updated_frac[7, ...] += updated_frac[9, ...]
    updated_frac[8, ...] *= 0.
    updated_frac[9, ...] *= 0.

    # Cover any precision errors by dividing by sum
    updated_frac_sum = np.sum(updated_frac, axis=0)
    if np.any(~np.isclose(updated_frac_sum, 1.0)):
        raise ValueError    
    updated_frac = np.divide(
        updated_frac.data,
        updated_frac_sum.data,
        out=np.zeros_like(updated_frac.data),
        where=updated_frac_sum.data > 0
    )
    updated_frac = np.ma.array(
        updated_frac,
        mask=current_frac.mask
    )    
    # Now add target back to netCDF4
    ds['land_cover_lccs'][:] = updated_frac
    ds.close()
