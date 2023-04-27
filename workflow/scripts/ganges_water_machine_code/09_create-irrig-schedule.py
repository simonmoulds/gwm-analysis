#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import numpy as np
import netCDF4
import rioxarray

def main():

    # Study region
    land_fn = '../data/wfdei/ancils/WFD-EI-LandFraction2d_igp.nc'
    ds = netCDF4.Dataset(land_fn, 'r')
    land = ds['lsmask'][:]

    # ================================= #
    # Load LAI data to use as template
    # ================================= #

    # We do this because the irrigation schedule
    # is also a prescribed dataset with the same
    # temporal resolution (i.e. daily)
    
    lai_fn = '../data/wfdei/ancils/jules_5pft_w_crops_veg_func_igp_wfdei_interp.nc'
    lai = netCDF4.Dataset(lai_fn, 'r')
    nt = 366
    ntype = 15
    ny = 40
    nx = 80

    # ================================= #
    # Create irrigation mask
    # ================================= #
    
    irr_index = [6, 7, 8, 9]    # NB zero-indexing
    years = np.arange(1979, 2015+1)
    frac_sum = np.zeros((ny, nx))
    for year in years:
        frac_fn = os.path.join(
            '../data/wfdei/ancils',
            'jules_5pft_w_crops_veg_frac_' + str(year) + '_igp_wfdei.nc'
        )        
        nc = netCDF4.Dataset(frac_fn, 'r')
        frac = nc['land_cover_lccs'][:][irr_index, ...]#.data
        frac_data = frac.data
        frac_data[frac.mask] = 0.0
        frac_sum = frac_sum + np.sum(frac_data, axis=0)
        nc.close()

    # Irrigated mask
    irr_mask = (frac_sum > 0) * 1.
    irr_mask *= land 

    # ================================= #
    # Load monsoon onset data
    # ================================= #
    
    # APHRODITE-based monsoon onset data
    # onset_fn = '../data-raw/median.onset.wet.season.ap.igp.nc'    
    # onset = netCDF4.Dataset(onset_fn, 'r')
    # medons = onset['medons'][:]
    # mask = medons.mask
    # data = medons.data 
    onset_fn = '../data/igp_wet_season_onset.tif'
    onset = rioxarray.open_rasterio(onset_fn)
    onset = onset.data.squeeze()
    onset = np.flipud(onset) * irr_mask

    # ================================= #
    # Create irrigation schedule
    # ================================= #
    
    # These are taken from Biemans et al. 2016:

    # "In the analysis of seasonal irrigation  demand,  we
    # therefore distinguish three seasons: kharif, from June
    # until October; rabi, from November until March; and a
    # dry “summer” season from April to May.  This dry
    # pre-monsoon  summer  season  is  sometimes also
    # called Zaid season."
    
    # Irrigation single [Jun-Sep (end), i.e. assume Kharif only]
    # 153-304
    # Irrigation double [Nov-Mar (end)]
    # 305-90
    # Irrigation triple [Apr-May (end)]
    # 91-151
    # NB: currently not considering Zaid season
    jd = (np.arange(0, nt) + 1)
    jd = jd[:,None,None] * np.ones((ny, nx))
    irrigation_single_idx = (jd >= onset) & (jd <= 304)
    irrigation_double_idx = (irrigation_single_idx | (jd >= 305) | (jd <= 90))
    # irrigation_triple_idx = (irrigation_double_idx | ((jd >= 91) & (jd <= 151)))
    irrigation_continuous_idx = (jd >= 1)

    irrig_schedule = np.zeros((nt, ntype, ny, nx))
    irrig_schedule[:,6,:,:][irrigation_single_idx] = 1
    irrig_schedule[:,7,:,:][irrigation_double_idx] = 1
    # irrig_schedule[:,8,:,:][irrigation_triple_idx] = 1
    irrig_schedule[:,9,:,:][irrigation_continuous_idx] = 1

    # TEST - ensure Kharif season has a different number
    irrig_schedule2 = np.copy(irrig_schedule)
    irrig_schedule2[irrig_schedule == 1] = 2
    irrig_schedule2[:,6,:,:][irrigation_single_idx] = 1
    irrig_schedule2[:,7,:,:][irrigation_single_idx] = 1
    # irrig_schedule2[:,8,:,:][~irrigation_single_idx] = 1
    irrig_schedule2[:,9,:,:][irrigation_single_idx] = 1

    # Multiply by mask
    irrig_schedule = irrig_schedule * irr_mask[None,None,:,:]
    irrig_schedule2 = irrig_schedule2 * irr_mask[None,None,:,:]

    # ================================= #
    # Write to file
    # ================================= #

    # Use a land cover file as a template
    frac_fn = '../data/wfdei/ancils/jules_5pft_w_crops_veg_frac_2015_igp_wfdei.nc'
    frac = netCDF4.Dataset(frac_fn, 'r')
    
    ncout = netCDF4.Dataset(
        '../data/wfdei/ancils/jules_5pft_w_crops_irrig_schedule.nc',
        'w'
    )
    ncout.createDimension('tstep', None)        
    ncout.createDimension('dim0', len(frac['pseudo_level'][:]))
    ncout.createDimension('lat', len(lai['lat'][:]))
    ncout.createDimension('lon', len(lai['lon'][:]))
    var = ncout.createVariable('tstep', 'i4', ('tstep',))
    var.units = lai['tstep'].units
    var.calendar = lai['tstep'].calendar
    var[:] = lai['tstep'][:]
    var = ncout.createVariable('pseudo_level', 'i4', ('dim0',))
    var.units = frac['pseudo_level'].units
    var.long_name = frac['pseudo_level'].long_name
    var[:] = frac['pseudo_level'][:]
    var = ncout.createVariable('lat', np.float32, ('lat',))
    var.units = 'degrees North'
    var[:] = frac['lat'][:]
    var = ncout.createVariable('lon', np.float32, ('lon',))
    var.units = 'degrees East'
    var[:] = frac['lon'][:]
    var = ncout.createVariable('irr_schedule', 'i4', ('tstep', 'dim0', 'lat', 'lon'))
    var.standard_name = 'irr_schedule'
    var.units = '1'
    var[:] = irrig_schedule
    ncout.close()

    ncout = netCDF4.Dataset(
        '../data/wfdei/ancils/jules_5pft_w_crops_irrig_schedule_policy.nc',
        'w'
    )
    ncout.createDimension('tstep', None)
    ncout.createDimension('dim0', len(frac['pseudo_level'][:]))
    ncout.createDimension('lat', len(lai['lat'][:]))
    ncout.createDimension('lon', len(lai['lon'][:]))
    var = ncout.createVariable('tstep', 'i4', ('tstep',))
    var.units = lai['tstep'].units
    var.calendar = lai['tstep'].calendar
    var[:] = lai['tstep'][:]
    var = ncout.createVariable('pseudo_level', 'i4', ('dim0',))
    var.units = frac['pseudo_level'].units
    var.long_name = frac['pseudo_level'].long_name
    var[:] = frac['pseudo_level'][:]
    var = ncout.createVariable('lat', np.float32, ('lat',))
    var.units = 'degrees North'
    var[:] = frac['lat'][:]
    var = ncout.createVariable('lon', np.float32, ('lon',))
    var.units = 'degrees East'
    var[:] = frac['lon'][:]
    var = ncout.createVariable('irr_schedule', 'i4', ('tstep', 'dim0', 'lat', 'lon'))
    var.standard_name = 'irr_schedule'
    var.units = '1'
    var[:] = irrig_schedule2
    ncout.close()

    # Close other datasets
    lai.close()
    frac.close()

if __name__ == '__main__':
    main()
