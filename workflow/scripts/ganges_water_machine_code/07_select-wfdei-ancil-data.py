#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import shutil
# import click
import numpy as np
import netCDF4
import xarray
import rioxarray
import pandas as pd

DATADIR='../data-raw/wfdei_ancils'
OUTDIR='../data/wfdei/ancils'
try:
    os.makedirs(OUTDIR)
except OSError:
    pass

XMIN=60.0
XMAX=100.0
YMIN=20.0
YMAX=40.0

def main():
    
    # Extract lat vals from raw met file
    latlon = xarray.open_dataset(
        "/mnt/scratch/scratch/data/WFDEI/WFDEI_3h/LWdown_WFDEI/LWdown_WFDEI_197901.nc",
        decode_times=False
    )
    lat_vals = latlon['lat'].values
    lon_vals = latlon['lon'].values
    latlon.close()

    # ##################################### #
    # Topographic Index
    # ##################################### #

    fname = 'topoidx_WFDEI_0p5_2D_global.nc'
    # Copy file
    shutil.copy2(os.path.join(DATADIR, fname), OUTDIR)

    # Rename x, y to lon, lat
    cmd = 'ncrename -d .x,lon -d .y,lat ' + os.path.join(OUTDIR, fname)
    os.system(cmd)

    # Add lon, lat variables
    nc = netCDF4.Dataset(os.path.join(OUTDIR, fname), 'r+')
    var = nc.createVariable('lat', np.float32, ('lat',))
    var.units = 'degrees North'
    var[:] = lat_vals
    var = nc.createVariable('lon', np.float32, ('lon',))
    var.units = 'degrees East'
    var[:] = lon_vals
    nc.close()

    fname_new = os.path.splitext(fname)[0] + '_igp.nc'
    cmd = 'ncks -O -d lon,' + str(XMIN) + ',' + str(XMAX) + ' -d lat,' + str(YMIN) + ',' + str(YMAX) + ' ' + os.path.join(OUTDIR, fname) + ' ' + os.path.join(OUTDIR, fname_new)
    os.system(cmd)
    
    # ##################################### #
    # WFDEI-long-lat-2d
    # ##################################### #
    
    fname = 'WFDEI-long-lat-2d.nc'
    # Copy file
    shutil.copy2(os.path.join(DATADIR, fname), OUTDIR)

    # Rename x, y to lon, lat
    cmd = 'ncrename -d .x,lon -d .y,lat ' + os.path.join(OUTDIR, fname)
    os.system(cmd)

    # Add lon, lat variables
    nc = netCDF4.Dataset(os.path.join(OUTDIR, fname), 'r+')
    var = nc.createVariable('lat', np.float32, ('lat',))
    var.units = 'degrees North'
    var[:] = lat_vals
    var = nc.createVariable('lon', np.float32, ('lon',))
    var.units = 'degrees East'
    var[:] = lon_vals
    nc.close()

    fname_new = os.path.splitext(fname)[0] + '_igp.nc'
    cmd = 'ncks -O -d lon,' + str(XMIN) + ',' + str(XMAX) + ' -d lat,' + str(YMIN) + ',' + str(YMAX) + ' ' + os.path.join(OUTDIR, fname) + ' ' + os.path.join(OUTDIR, fname_new)
    os.system(cmd)
    
    # ##################################### #
    # WFD-EI-LandFraction2D.nc 
    # ##################################### #

    fname = 'WFD-EI-LandFraction2d.nc'
    # Copy file
    shutil.copy2(os.path.join(DATADIR, fname), OUTDIR)

    # Rename x, y to lon, lat
    cmd = 'ncrename -d .x,lon -d .y,lat ' + os.path.join(OUTDIR, fname)
    os.system(cmd)

    # Add lon, lat variables
    nc = netCDF4.Dataset(os.path.join(OUTDIR, fname), 'r+')
    var = nc.createVariable('lat', np.float32, ('lat',))
    var.units = 'degrees North'
    var[:] = lat_vals
    var = nc.createVariable('lon', np.float32, ('lon',))
    var.units = 'degrees East'
    var[:] = lon_vals
    nc.close()

    fname_new = os.path.splitext(fname)[0] + '_south_asia.nc'
    cmd = 'ncks -O -d lon,' + str(XMIN) + ',' + str(XMAX) + ' -d lat,' + str(YMIN) + ',' + str(YMAX) + ' ' + os.path.join(OUTDIR, fname) + ' ' + os.path.join(OUTDIR, fname_new)
    os.system(cmd)

    ds = rioxarray.open_rasterio("../data/igp_basins.tif")
    basins = np.flipud(ds.values.squeeze())  # flipud to have increasing lat
    ds.close()
    
    fname_new_new = os.path.splitext(fname)[0] + '_igp.nc'
    with netCDF4.Dataset(os.path.join(OUTDIR, fname_new)) as src, netCDF4.Dataset(os.path.join(OUTDIR, fname_new_new), 'w') as dst:
        # copy global attributes all at once via dictionary
        dst.setncatts(src.__dict__)
        # copy dimensions
        for name, dimension in src.dimensions.items():
            dst.createDimension(
                name, (len(dimension) if not dimension.isunlimited() else None))
        # copy all file data except for the excluded
        for name, variable in src.variables.items():
            x = dst.createVariable(name, variable.datatype, variable.dimensions)
            dst[name][:] = src[name][:]
            # copy variable attributes all at once via dictionary
            dst[name].setncatts(src[name].__dict__)            
        dst['lsmask'][:] = dst['lsmask'][:] * basins
        
    # ##################################### #
    # qrparm.veg.frac2d.nc
    # ##################################### #

    fname = 'qrparm.veg.frac2d.nc'
    
    # Copy file
    shutil.copy2(os.path.join(DATADIR, fname), OUTDIR)
    
    # Rename x, y to lon, lat
    # cmd = 'ncrename -d .x,lon -d .y,lat ' + os.path.join(OUTDIR, fname)
    cmd = 'ncrename -d .x,lon -d .y,lat -d .z,dim0 ' + os.path.join(OUTDIR, fname)
    os.system(cmd)

    # Add lon, lat variables
    nc = netCDF4.Dataset(os.path.join(OUTDIR, fname), 'r+')
    var = nc.createVariable('lat', np.float32, ('lat',))
    var.units = 'degrees North'
    var[:] = lat_vals
    var = nc.createVariable('lon', np.float32, ('lon',))
    var.units = 'degrees East'
    var[:] = lon_vals
    nc.close()

    fname_new = os.path.splitext(fname)[0] + '_igp.nc'
    cmd = 'ncks -O -d lon,' + str(XMIN) + ',' + str(XMAX) + ' -d lat,' + str(YMIN) + ',' + str(YMAX) + ' ' + os.path.join(OUTDIR, fname) + ' ' + os.path.join(OUTDIR, fname_new)
    os.system(cmd)    

    # ##################################### #
    # qrparm.veg.func2d.nc
    # ##################################### #

    fname = 'qrparm.veg.func2d.nc'
    
    # Copy file
    shutil.copy2(os.path.join(DATADIR, fname), OUTDIR)
    
    # Rename x, y to lon, lat
    cmd = 'ncrename -d .x,lon -d .y,lat -d .z,dim1 ' + os.path.join(OUTDIR, fname)
    os.system(cmd)

    # Add lon, lat variables
    nc = netCDF4.Dataset(os.path.join(OUTDIR, fname), 'r+')
    var = nc.createVariable('lat', np.float32, ('lat',))
    var.units = 'degrees North'
    var[:] = lat_vals
    var = nc.createVariable('lon', np.float32, ('lon',))
    var.units = 'degrees East'
    var[:] = lon_vals
    nc.close()

    fname_new = os.path.splitext(fname)[0] + '_igp.nc'
    cmd = 'ncks -O -d lon,' + str(XMIN) + ',' + str(XMAX) + ' -d lat,' + str(YMIN) + ',' + str(YMAX) + ' ' + os.path.join(OUTDIR, fname) + ' ' + os.path.join(OUTDIR, fname_new)
    os.system(cmd)    
    
    # ##################################### #
    # qrparm.soil_HWSD_class3_van_genuchten2d.nc
    # ##################################### #
    
    fname = 'qrparm.soil_HWSD_class3_van_genuchten2d.nc'
    # Copy file
    shutil.copy2(os.path.join(DATADIR, fname), OUTDIR)
    
    # Rename x, y to lon, lat
    cmd = 'ncrename -d .x,lon -d .y,lat ' + os.path.join(OUTDIR, fname)
    os.system(cmd)

    # Add lon, lat variables
    nc = netCDF4.Dataset(os.path.join(OUTDIR, fname), 'r+')
    var = nc.createVariable('lat', np.float32, ('lat',))
    var.units = 'degrees North'
    var[:] = lat_vals
    var = nc.createVariable('lon', np.float32, ('lon',))
    var.units = 'degrees East'
    var[:] = lon_vals
    nc.close()

    fname_new = os.path.splitext(fname)[0] + '_igp.nc'
    cmd = 'ncks -O -d lon,' + str(XMIN) + ',' + str(XMAX) + ' -d lat,' + str(YMIN) + ',' + str(YMAX) + ' ' + os.path.join(OUTDIR, fname) + ' ' + os.path.join(OUTDIR, fname_new)
    os.system(cmd)
    
    # ##################################### #
    # qrparm.soil_HWSD_class3_van_genuchtenNew_NewSoilAlbedo-rfu-2D-LatLon-grid.nc
    # ##################################### #
    
    fname = 'qrparm.soil_HWSD_class3_van_genuchtenNew_NewSoilAlbedo-rfu-2D-LatLon-grid.nc'

    # Copy file
    shutil.copy2(os.path.join(DATADIR, fname), OUTDIR)
    # This file already contains lat/lon dimensions/variables,
    # so no need to make any adjustments

    fname_new = os.path.splitext(fname)[0] + '_igp.nc'
    cmd = 'ncks -O -d lon,' + str(XMIN) + ',' + str(XMAX) + ' -d lat,' + str(YMIN) + ',' + str(YMAX) + ' ' + os.path.join(OUTDIR, fname) + ' ' + os.path.join(OUTDIR, fname_new)
    os.system(cmd)

    # ##################################### #
    # qrparm.soil_HWSD_cont_cosby2d.nc
    # ##################################### #

    fname = 'qrparm.soil_HWSD_cont_cosby2d.nc'

    # Copy file
    shutil.copy2(os.path.join(DATADIR, fname), OUTDIR)
    
    # Rename x, y to lon, lat
    cmd = 'ncrename -d .x,lon -d .y,lat ' + os.path.join(OUTDIR, fname)
    os.system(cmd)

    # Add lon, lat variables
    nc = netCDF4.Dataset(os.path.join(OUTDIR, fname), 'r+')
    var = nc.createVariable('lat', np.float32, ('lat',))
    var.units = 'degrees North'
    var[:] = lat_vals
    var = nc.createVariable('lon', np.float32, ('lon',))
    var.units = 'degrees East'
    var[:] = lon_vals
    nc.close()

    fname_new = os.path.splitext(fname)[0] + '_igp.nc'
    cmd = 'ncks -O -d lon,' + str(XMIN) + ',' + str(XMAX) + ' -d lat,' + str(YMIN) + ',' + str(YMAX) + ' ' + os.path.join(OUTDIR, fname) + ' ' + os.path.join(OUTDIR, fname_new)
    os.system(cmd)
    
    # ##################################### #
    # Canopy height, leaf area index
    # ##################################### #

    canht_fname = 'jules_5pft_w_crops_prescribed_canopy_height_igp.nc'
    lai_fname = 'jules_5pft_w_crops_prescribed_lai_igp.nc'    
    fname_new = 'jules_5pft_w_crops_veg_func_igp_wfdei.nc'

    # Add lon, lat variables
    canht = netCDF4.Dataset(os.path.join('../data', canht_fname), 'r')
    lai = netCDF4.Dataset(os.path.join('../data', lai_fname), 'r')

    ncout = netCDF4.Dataset(os.path.join(OUTDIR, fname_new), 'w')
    ncout.createDimension('time', None)
    ncout.createDimension('tstep', None)        
    ncout.createDimension('dim1', len(canht['pseudo_level'][:]))
    ncout.createDimension('lat', len(canht['latitude'][:]))
    ncout.createDimension('lon', len(canht['longitude'][:]))
    
    var = ncout.createVariable('tstep', 'i4', ('tstep',))
    var.units = canht['time'].units
    var.calendar = canht['time'].calendar
    var[:] = canht['time'][:]

    var = ncout.createVariable('pseudo_level', 'i4', ('dim1',))
    var.units = canht['pseudo_level'].units
    var.long_name = canht['pseudo_level'].long_name
    var[:] = canht['pseudo_level'][:]
    
    var = ncout.createVariable('lat', np.float32, ('lat',))
    var.units = 'degrees North'
    var[:] = np.flip(canht['latitude'][:])
                          
    var = ncout.createVariable('lon', np.float32, ('lon',))
    var.units = 'degrees East'
    var[:] = canht['longitude'][:]
    
    var = ncout.createVariable('canopy_height', 'f8', ('tstep', 'dim1', 'lat', 'lon'))
    var.standard_name = canht['canopy_height'].standard_name
    var.units = canht['canopy_height'].units
    canht_dims = canht['canopy_height'].dimensions
    lat_index = [i for i in range(len(canht_dims)) if canht_dims[i] == 'latitude'][0]
    var[:] = np.flip(canht['canopy_height'][:], axis=lat_index)

    var = ncout.createVariable('leaf_area_index', 'f8', ('tstep', 'dim1', 'lat', 'lon'))
    var.standard_name = lai['leaf_area_index'].standard_name
    var.units = lai['leaf_area_index'].units
    lai_dims = canht['canopy_height'].dimensions
    lat_index = [i for i in range(len(lai_dims)) if lai_dims[i] == 'latitude'][0]
    var[:] = np.flip(lai['leaf_area_index'][:], axis=lat_index)
    
    ncout.close()

    # Temporal interpolation
    x = xarray.open_dataset(os.path.join(OUTDIR, fname_new))
    x_first = x.isel({'tstep':0})
    x_last = x.isel({'tstep':-1})
    x_first.tstep.values += np.timedelta64(365,'D')
    x_last.tstep.values -= np.timedelta64(365,'D')
    x = xarray.concat([x_last, x, x_first], dim='tstep', data_vars='minimal').resample(tstep='1D').interpolate()
    x = x.sel(tstep=slice("2015-01-01 00:00:00", "2016-01-01 00:00:00"))
    tms = [pd.Timestamp(tstp).to_pydatetime() for tstp in x['tstep'].values]
        
    fname_new = 'jules_5pft_w_crops_veg_func_igp_wfdei_interp.nc'    
    ncout = netCDF4.Dataset(os.path.join(OUTDIR, fname_new), 'w')
    ncout.createDimension('tstep', None)        
    ncout.createDimension('dim1', len(canht['pseudo_level'][:]))
    ncout.createDimension('lat', len(canht['latitude'][:]))
    ncout.createDimension('lon', len(canht['longitude'][:]))
    
    var = ncout.createVariable('tstep', 'i4', ('tstep',))
    var.units = canht['time'].units
    var.calendar = canht['time'].calendar
    var[:] = netCDF4.date2num(tms, units=var.units, calendar=var.calendar)

    var = ncout.createVariable('pseudo_level', 'i4', ('dim1',))
    var.units = canht['pseudo_level'].units
    var.long_name = canht['pseudo_level'].long_name
    var[:] = canht['pseudo_level'][:]
    
    var = ncout.createVariable('lat', np.float32, ('lat',))
    var.units = 'degrees North'
    var[:] = np.flip(canht['latitude'][:])
                          
    var = ncout.createVariable('lon', np.float32, ('lon',))
    var.units = 'degrees East'
    var[:] = canht['longitude'][:]
    
    var = ncout.createVariable('canopy_height', 'f8', ('tstep', 'dim1', 'lat', 'lon'))
    var.standard_name = canht['canopy_height'].standard_name
    var.units = canht['canopy_height'].units
    canht_dims = canht['canopy_height'].dimensions
    lat_index = [i for i in range(len(canht_dims)) if canht_dims[i] == 'latitude'][0]
    var[:] = np.moveaxis(x['canopy_height'].values[:], [0,3], [1,0])
    # var[:] = np.flip(canht['canopy_height'][:], axis=lat_index)

    var = ncout.createVariable('leaf_area_index', 'f8', ('tstep', 'dim1', 'lat', 'lon'))
    var.standard_name = lai['leaf_area_index'].standard_name
    var.units = lai['leaf_area_index'].units
    lai_dims = canht['canopy_height'].dimensions
    lat_index = [i for i in range(len(lai_dims)) if lai_dims[i] == 'latitude'][0]
    var[:] = np.moveaxis(x['leaf_area_index'].values[:], [0,3], [1,0])
    # var[:] = np.flip(lai['leaf_area_index'][:], axis=lat_index)
    
    canht.close()
    lai.close()
    ncout.close()
    
    # ################################# #
    # Land cover fraction
    # ################################# #

    # N.B. previously we didn't vary land frac in time
    # frac_fname = 'jules_frac_5pft_ants_2015_CUSTOM_igp.nc'
    # fname_new = 'jules_5pft_w_crops_veg_frac_igp_wfdei.nc'
    yrs = np.arange(1979, 2015+1)
    for yr in yrs:
        
        frac_fname = 'jules_frac_5pft_ants_' + str(yr) + '_CUSTOM_igp_adjusted.nc'
        fname_new = 'jules_5pft_w_crops_veg_frac_' + str(yr) + '_igp_wfdei.nc'
    
        # Add lon, lat variables
        frac = netCDF4.Dataset(os.path.join('../data', frac_fname), 'r')

        ncout = netCDF4.Dataset(os.path.join(OUTDIR, fname_new), 'w')
        ncout.createDimension('dim0', len(frac['pseudo_level'][:]))
        ncout.createDimension('lat', len(frac['latitude'][:]))
        ncout.createDimension('lon', len(frac['longitude'][:]))

        var = ncout.createVariable('pseudo_level', 'i4', ('dim0',))
        var.units = frac['pseudo_level'].units
        var.long_name = frac['pseudo_level'].long_name
        var[:] = frac['pseudo_level'][:]

        var = ncout.createVariable('lat', np.float32, ('lat',))
        var.units = 'degrees North'
        var[:] = np.flip(frac['latitude'][:])

        var = ncout.createVariable('lon', np.float32, ('lon',))
        var.units = 'degrees East'
        var[:] = frac['longitude'][:]

        var = ncout.createVariable('land_cover_lccs', 'f8', ('dim0', 'lat', 'lon'))
        var.standard_name = frac['land_cover_lccs'].standard_name
        var.units = frac['land_cover_lccs'].units
        frac_dims = frac['land_cover_lccs'].dimensions
        lat_index = [i for i in range(len(frac_dims)) if frac_dims[i] == 'latitude'][0]
        var[:] = np.flip(frac['land_cover_lccs'][:], axis=lat_index)

        ncout.close()
        frac.close()

    # Having created land cover fraction, we need to go back into soil maps
    # and ensure that th_sat is zero on any land ice points
    # N.B. this will now use the data for 2015 which should be fine
    frac = netCDF4.Dataset(os.path.join(OUTDIR, fname_new), 'r')
    vals = frac['land_cover_lccs'][-1, ...]
    ice = vals.data
    ice[vals.mask] = 0
    ice = ice > 0
    frac.close()

    soil1 = netCDF4.Dataset(os.path.join(OUTDIR, 'qrparm.soil_HWSD_class3_van_genuchten2d_igp.nc'), 'r+')
    th_sat = soil1['field332'][:]
    th_sat[ice] = 0
    soil1['field332'][:] = th_sat
    soil1.close()
    
    soil2 = netCDF4.Dataset(os.path.join(OUTDIR, 'qrparm.soil_HWSD_class3_van_genuchtenNew_NewSoilAlbedo-rfu-2D-LatLon-grid_igp.nc'), 'r+')
    th_sat = soil2['sm_sat'][:]
    th_sat[ice] = 0
    soil2['sm_sat'][:] = th_sat
    soil2.close()
    
    soil3 = netCDF4.Dataset(os.path.join(OUTDIR, 'qrparm.soil_HWSD_cont_cosby2d_igp.nc'), 'r+')
    th_sat = soil3['field332'][:]
    th_sat[ice] = 0
    soil3['field332'][:] = th_sat
    soil3.close()
        
if __name__ == '__main__':
    main()
    
