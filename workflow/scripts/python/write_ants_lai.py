#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
# from calendar import monthrange
import datetime
import numpy as np
import rasterio
import netCDF4

DATADIR = os.path.join(os.environ['DATADIR'], 'aux/lai')

def get_region_data(fn):
    """Function to obtain geospatial parameters."""
    ds = rasterio.open(fn)
    # squeeze to remove unit dimension
    land_frac = ds.read(1, masked=False).squeeze()
    land_frac = land_frac > 0
    transform = ds.transform
    extent = ds.bounds
    nlat = land_frac.shape[0]
    nlon = land_frac.shape[1]
    lon_vals = (
        np.arange(nlon) * transform[0] + transform[2] + transform[0]/2
    )    
    lat_vals = (
        np.arange(nlat) * transform[4] + transform[5] + transform[4]/2
    )
    return land_frac, lat_vals, lon_vals, extent

def get_lat_lon_bnds(vals, extent):
    """Calculate lat/lon bounds."""
    bound = np.linspace(
        extent[0], extent[1], endpoint=True, num=len(vals)+1
    )
    bounds = np.array([bound[:-1], bound[1:]]).T
    return bounds

# ##################################### #
# Assign constants:
# ##################################### #

# Land cover types for which LAI is estimated separately:
LCS = [
    "combined",
    "natural",
    "rainfed_cropland",
    "irrigated_cropland_1",
    "irrigated_cropland_2",
    "irrigated_cropland_3",
    "irrigated_cropland_c",
    "fallow_cropland"
]

# Number of LAI points during the year (3 per month)
LYRS = np.arange(36) + 1

# Spatial information
# OFFSET = 180.
OFFSET = 0.
TEMPLATE_FN = os.path.join(
    DATADIR,
    'lai_natural_avg_1_igp_0.041667Deg.tif'
)
_, LAT_VALS, LON_VALS, EXTENT = get_region_data(TEMPLATE_FN)
LON_VALS += OFFSET
NLAT = len(LAT_VALS)
NLON = len(LON_VALS)
LAT_BNDS = get_lat_lon_bnds(
    LAT_VALS, (EXTENT.top, EXTENT.bottom)
)
LON_BNDS = get_lat_lon_bnds(
    LON_VALS, (EXTENT.left+OFFSET, EXTENT.right+OFFSET)
)

# Define time series - because LAI is dealt with as a
# climateology we choose an arbitrary non-leap year
YR = 2015
T0 = datetime.datetime(YR, 1, 10, 0, 0)
TMS = [T0]
for i in np.arange(1, 36):
    TMS.append(T0 + datetime.timedelta(10))
    T0 = TMS[i]
# NB ANTS currently only allows constant delta in integer
# days - for now, apply a constant 10 day time step
# OLD:
# tms = []
# for month in np.arange(12) + 1:
#     tms.append(datetime.datetime(yr, month, 10, 0, 0))
#     tms.append(datetime.datetime(yr, month, 20, 0, 0))
#     tms.append(datetime.datetime(yr, month, , 0, 0))
#     # tms.append(datetime.datetime(yr, month, monthrange(yr, month)[1], 0, 0))

def main():

    for LC in LCS:
        
        # Load LAI data
        lai = []
        for LYR in LYRS:
            lai_ds = rasterio.open(
                os.path.join(
                    DATADIR,
                    'lai_' + LC + '_avg_' + str(LYR) + '_igp_0.041667Deg.tif'
                )
            )
            lai_map = lai_ds.read(1, masked=True).squeeze()
            lai.append(lai_map)

        mask = [x.mask for x in lai]
        lai = np.stack(lai)
        lai.mask = np.stack(mask)

        # Create LAI dataset
        nco = netCDF4.Dataset(
            os.path.join(DATADIR, 'lai_' + LC + '_igp.nc'),
            'w',
            format='NETCDF4'
        )
        nco.createDimension('time', None)
        nco.createDimension('latitude', NLAT)
        nco.createDimension('longitude', NLON)
        nco.createDimension('bnds', 2)
        var = nco.createVariable(
            'longitude', 'f8', ('longitude',)
        )
        var.axis = 'X'
        var.bounds = 'longitude_bnds'
        var.units = 'degrees_east'
        var.standard_name = 'longitude'
        var[:] = LON_VALS

        var = nco.createVariable(
            'longitude_bnds', 'f8', ('longitude', 'bnds')
        )
        var[:] = LON_BNDS        

        var = nco.createVariable(
            'latitude', 'f8', ('latitude',)
        )
        var.axis = 'Y'
        var.bounds = 'latitude_bnds'
        var.units = 'degrees_north'
        var.standard_name = 'latitude'
        var[:] = LAT_VALS        
        var = nco.createVariable(
            'latitude_bnds', 'f8', ('latitude', 'bnds')
        )
        var[:] = LAT_BNDS

        var = nco.createVariable(
            'latitude_longitude', 'i4'
        )
        var.grid_mapping_name = 'latitude_longitude'
        var.longitude_of_prime_meridian = 0.
        var.earth_radius = 6371229.

        var = nco.createVariable(
            'time', 'i4', ('time',)
        )
        var.units = 'hours since 1970-01-01 00:00:00'
        var.calendar = 'gregorian'
        var[:] = netCDF4.date2num(TMS, var.units, var.calendar)

        var = nco.createVariable(
            'leaf_area_index', 'f8', ('time', 'latitude', 'longitude')
        )
        var.standard_name = 'leaf_area_index'
        var.units = '1'
        var.um_stash_source = 'm01s00i217'
        var.cell_methods = 'time: mean within days time: mean over years'
        var.grid_mapping = 'latitude_longitude'
        var[:] = lai 
        nco.close()

if __name__ == '__main__':
    main()
