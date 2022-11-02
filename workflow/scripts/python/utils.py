#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import netCDF4
import rasterio
import numpy as np

# Default fill vals for netCDF 
F8_FILLVAL = netCDF4.default_fillvals['f8']
F4_FILLVAL = netCDF4.default_fillvals['f4']
I4_FILLVAL = netCDF4.default_fillvals['i4']

def get_region_data():
    """Function to obtain geospatial parameters."""
    ds = rasterio.open(os.environ['JULES_LAND_FRAC_FN'])
    land_frac = ds.read(1, masked=False).squeeze()  # squeeze to remove unit dimension
    transform = ds.transform                        # affine
    extent = ds.bounds
    nlat = land_frac.shape[0]                       # nrow
    nlon = land_frac.shape[1]                       # ncol
    lon_vals = np.arange(nlon) * transform[0] + transform[2] + transform[0]/2
    lat_vals = np.arange(nlat) * transform[4] + transform[5] + transform[4]/2
    return land_frac, lat_vals, lon_vals, extent


def get_lat_lon_grids(lat_vals, lon_vals):
    """Expand latitude and longitude values to grid."""
    nlat = len(lat_vals)
    nlon = len(lon_vals)
    lon_vals_2d = lon_vals[None, :] * np.ones(nlat)[:, None]
    lat_vals_2d = lat_vals[:, None] * np.ones(nlon)[None, :]
    return lon_vals_2d, lat_vals_2d


def get_lat_lon_bnds(vals, extent):
    """Calculate lat/lon bounds."""
    bound = np.linspace(extent[0], extent[1], endpoint=True, num=len(vals)+1)
    bounds = np.array([bound[:-1], bound[1:]]).T
    return bounds

# Constants:
LAND_FRAC, LAT_VALS, LON_VALS, EXTENT = get_region_data()
NLAT = len(LAT_VALS)
NLON = len(LON_VALS)
LAT_BNDS = get_lat_lon_bnds(LAT_VALS, (EXTENT.top, EXTENT.bottom))
LON_BNDS = get_lat_lon_bnds(LON_VALS, (EXTENT.left, EXTENT.right))

def add_lat_lon_dims_2d(nco):
    """Add 2d latitude/longitude data to a netCDF object."""
    
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
    return nco
