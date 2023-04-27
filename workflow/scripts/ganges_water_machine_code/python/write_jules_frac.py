#!/usr/bin/env python
# -*- coding: utf-8 -*-

import numpy as np
import netCDF4
from utils import *

def get_jules_frac(year, frac_type_names):
    ntype = len(frac_type_names)
    frac = []
    for lc_name in frac_type_names:
        frac_ds = rasterio.open(
            os.environ['LC_' + lc_name.upper() + '_' + str(year) + '_FN']
        )
        frac_map = frac_ds.read(1, masked=False).squeeze()
        frac.append(frac_map)

    # Divide by sum to ensure the fractions sum to one
    frac = np.stack(frac)
    frac_sum = frac.sum(axis=0)
    frac_sum = np.nan_to_num(frac_sum)  # TEST
    frac = np.divide(frac, frac_sum, out=np.zeros_like(frac), where=frac_sum>0)
    
    # original ice/soil fractions
    ice_orig = frac[-1, ...]
    soil_orig = frac[-2, ...]

    ice = np.zeros_like(ice_orig).astype(np.bool)
    ice[ice_orig > 0.5] = True
    not_ice = np.logical_not(ice)

    # initially set all fractions/heights in ice gridboxes to zero
    frac *= not_ice[None, ...]
    # then set ice fraction to one
    frac[-1][ice] = 1
    frac[-1][not_ice] = 0
    # in non-ice gridboxes, add original ice fraction to bare soil
    frac[-2] = (soil_orig + ice_orig) * not_ice
    frac_sum = frac.sum(axis=0)
    frac = np.divide(frac, frac_sum, out=np.zeros_like(frac), where=frac_sum>0)    
    frac = np.ma.array(
        frac,
        mask=np.broadcast_to(
            np.logical_not(LAND_FRAC),
            (ntype, NLAT, NLON)
        ),
        dtype=np.float64,
        fill_value=F8_FILLVAL
    )
    return frac

# def write_jules_frac_2d(frac_fn, frac, var_name, var_units):
#     nco = netCDF4.Dataset(frac_fn, 'w', format='NETCDF4')
#     ntype = frac.shape[0]
#     nco = add_lat_lon_dims_2d(nco)

#     nco.createDimension('pseudo', ntype)
#     pseu = nco.createVariable('pseudo', 'i4', ('pseudo',))
#     pseu.units = '1'
#     pseu.standard_name = 'pseudo'
#     pseu.long_name = 'pseudo'
#     pseu[:] = np.arange(1, ntype+1)
    
#     var = nco.createVariable(
#         var_name, 'f8', ('pseudo', 'latitude', 'longitude'),
#         fill_value=F8_FILLVAL
#     )
#     var.units = var_units
#     var.standard_name = var_name
#     var.grid_mapping = 'latitude_longitude'
#     var[:] = frac
#     nco.close()

def write_jules_frac_ants(year, lc_names, frac_fn):
    frac = get_jules_frac(year, lc_names)
    ntype = frac.shape[0]
    # extract region characteristics, and move western
    # hemisphere east of east hemisphere.
    _, lat_vals, lon_vals, extent = get_region_data()
    nlat = len(lat_vals)
    nlon = len(lon_vals)
    lat_bnds = get_lat_lon_bnds(lat_vals, (extent.top, extent.bottom))
    lon_bnds = get_lat_lon_bnds(lon_vals, (extent.left, extent.right))

    # REMOVED THIS SECTION:
    # west_hemisphere = lon_vals < 0.
    # east_hemisphere = ~west_hemisphere
    # frac = np.concatenate([frac[:, :, east_hemisphere], frac[:, :, west_hemisphere]], axis=2)
    # lon_vals = np.concatenate([lon_vals[east_hemisphere], lon_vals[west_hemisphere] + 360.], axis=0)
    # lon_bnds = np.concatenate([lon_bnds[east_hemisphere, :], lon_bnds[west_hemisphere, :] + 360.], axis=0)
    
    # create file
    nco = netCDF4.Dataset(frac_fn, 'w', format='NETCDF4')
    # nco.grid_staggering = 6
    
    # add dimensions
    nco.createDimension('dim0', ntype)
    nco.createDimension('latitude', nlat)
    nco.createDimension('longitude', nlon)
    nco.createDimension('bnds', 2)
    # add variables
    var = nco.createVariable(
        'longitude', 'f8', ('longitude',)
    )
    var.axis = 'X'
    var.bounds = 'longitude_bnds'
    var.units = 'degrees_east'
    var.standard_name = 'longitude'
    var[:] = lon_vals
    var = nco.createVariable(
        'longitude_bnds', 'f8', ('longitude', 'bnds')
    )
    var[:] = lon_bnds
    var = nco.createVariable(
        'latitude', 'f8', ('latitude',)
    )
    var.axis = 'Y'
    var.bounds = 'latitude_bnds'
    var.units = 'degrees_north'
    var.standard_name = 'latitude'
    var[:] = lat_vals
    var = nco.createVariable(
        'latitude_bnds', 'f8', ('latitude', 'bnds')
    )
    var[:] = lat_bnds
    var = nco.createVariable(
        'latitude_longitude', 'i4'
    )
    var.grid_mapping_name = 'latitude_longitude'
    var.longitude_of_prime_meridian = 0.
    var.earth_radius = 6371229.    
    # TODO: change variable name
    var = nco.createVariable(
        'land_cover_lccs', 'f8', ('dim0', 'latitude', 'longitude'),
        fill_value=F8_FILLVAL
    )
    var.units = '1'
    var.um_stash_source = 'm01s00i216'
    var.standard_name = 'land_cover_lccs'
    var.grid_mapping = 'latitude_longitude'
    var.coordinates = 'pseudo_level'
    var[:] = frac
        
    pseu = nco.createVariable('pseudo_level', 'i4', ('dim0',))
    pseu.units = '1'
    pseu.long_name = 'pseudo_level'
    pseu[:] = np.arange(1, ntype+1)
    nco.close()
    
# def write_jules_frac_1d(frac_fn, frac, var_name, var_units):
#     nco = netCDF4.Dataset(frac_fn, 'w', format='NETCDF4')
#     ntype, nland = frac.shape[0], frac.shape[1]
#     nco.createDimension('land', nland)
#     nco.createDimension('pseudo', ntype)

#     var = nco.createVariable('pseudo', 'i4', ('pseudo',))
#     var.units = '1'
#     var.standard_name = 'pseudo'
#     var[:] = np.arange(1, ntype+1)

#     var = nco.createVariable(
#         var_name, 'f8', ('pseudo', 'land'),
#         fill_value=F8_FILLVAL
#     )
#     var.units = var_units
#     var.standard_name = var_name
#     var[:] = frac
#     nco.close()

# def write_jules_frac(year, lc_names, frac_fn, one_d=False):
#     # frac, surf_hgt = get_jules_frac(year, lc_names)
#     frac = get_jules_frac(year, lc_names)    
#     ntype = frac.shape[0]
#     if one_d:
#         mask = LAND_FRAC > 0.
#         mask = mask[None, :, :] * np.ones(ntype)[:, None, None]
#         mask = mask.astype(bool)
#         frac = frac.transpose()[mask.transpose()]
#         # surf_hgt = surf_hgt.transpose()[mask.transpose()]        
#         write_jules_frac_1d(frac_fn, frac, 'frac', '1')
#         # write_jules_frac_1d(surf_hgt_fn, surf_hgt, 'elevation', 'm')
#     else:
#         write_jules_frac_2d(frac_fn, frac, 'frac', '1')
#         # write_jules_frac_2d(surf_hgt_fn, surf_hgt, 'elevation', 'm')
