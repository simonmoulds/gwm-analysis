#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import datetime
import numpy as np
import rasterio
import netCDF4
import xarray

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

DATADIR = str(os.environ['DATADIR'])
LAIDATADIR = os.path.join(DATADIR, 'netcdf')
LCDATADIR = os.path.join(DATADIR, 'geotiff')
LAND_FRAC_FN = os.path.join(LCDATADIR, 'jamr_custom_land_frac_igp.tif')

OFFSET = 0.
LAND_FRAC, LAT_VALS, LON_VALS, EXTENT = get_region_data(LAND_FRAC_FN)
NLAT = len(LAT_VALS)
NLON = len(LON_VALS)
LAT_BNDS = get_lat_lon_bnds(
    LAT_VALS, (EXTENT.top, EXTENT.bottom)
)
LON_BNDS = get_lat_lon_bnds(
    LON_VALS, (EXTENT.left + OFFSET, EXTENT.right + OFFSET)
)

LC_DICT = {
    'tree_broadleaf' : {
        'lai' : 'lai_natural_igp_0.500000Deg.nc',
        'canopy_height' : 'canopy_height_natural_igp_0.500000Deg.nc',
        'nc_index' : 0,
        'land_cover' : 'jamr_esa_cci_lc_frac_tree_broadleaf_natural_2015_igp.tif',
        'jules_index' : 0
    },    
    'tree_needleleaf' : {
        'lai' : 'lai_natural_igp_0.500000Deg.nc',
        'canopy_height' : 'canopy_height_natural_igp_0.500000Deg.nc',
        'nc_index' : 1,
        'land_cover' : 'jamr_esa_cci_lc_frac_tree_needleleaf_natural_2015_igp.tif',
        'jules_index' : 1
    },
    'c3_grass' : {
        'lai' : 'lai_natural_igp_0.500000Deg.nc',
        'canopy_height' : 'canopy_height_natural_igp_0.500000Deg.nc',
        'nc_index' : 2,
        'land_cover' : 'jamr_esa_cci_lc_frac_c3_grass_natural_2015_igp.tif',
        'jules_index' : 2
    },
    'c4_grass' : {
        'lai' : 'lai_natural_igp_0.500000Deg.nc',
        'canopy_height' : 'canopy_height_natural_igp_0.500000Deg.nc',
        'nc_index' : 3,
        'land_cover' : 'jamr_esa_cci_lc_frac_c4_grass_natural_2015_igp.tif',
        'jules_index' : 3
    },
    'shrub' : {
        'lai' : 'lai_natural_igp_0.500000Deg.nc',
        'canopy_height' : 'canopy_height_natural_igp_0.500000Deg.nc',
        'nc_index' : 4,
        'land_cover' : 'jamr_esa_cci_lc_frac_shrub_natural_2015_igp.tif',
        'jules_index' : 4
    },
    'fallow' : {
        'lai' : 'lai_fallow_cropland_no_c4_crops_igp_0.500000Deg.nc',
        'canopy_height' : 'canopy_height_fallow_cropland_no_c4_crops_igp_0.500000Deg.nc',
        'nc_index' : 2,
        'land_cover' : 'jamr_esa_cci_lc_frac_fallow_2015_igp.tif',
        'jules_index' : 5
    },
    'rainfed' : {
        'lai' : 'lai_rainfed_cropland_no_c4_crops_igp_0.500000Deg.nc',
        'canopy_height' : 'canopy_height_rainfed_cropland_no_c4_crops_igp_0.500000Deg.nc',
        'nc_index' : 2,
        'land_cover' : 'jamr_esa_cci_lc_frac_rainfed_2015_igp.tif',
        'jules_index' : 6
    },
    'irrigated_single' : {
        'lai' : 'lai_irrigated_cropland_1_no_c4_crops_igp_0.500000Deg.nc',
        'canopy_height' : 'canopy_height_irrigated_cropland_1_no_c4_crops_igp_0.500000Deg.nc',
        'nc_index' : 2,
        'land_cover' : 'jamr_esa_cci_lc_frac_irrigated_single_season_2015_igp.tif',
        'jules_index' : 7
    },
    'irrigated_double' : {
        'lai' : 'lai_irrigated_cropland_2_no_c4_crops_igp_0.500000Deg.nc',
        'canopy_height' : 'canopy_height_irrigated_cropland_2_no_c4_crops_igp_0.500000Deg.nc',
        'nc_index' : 2,
        'land_cover' : 'jamr_esa_cci_lc_frac_irrigated_double_season_2015_igp.tif',
        'jules_index' : 8
    },
    'irrigated_triple' : {
        'lai' : 'lai_irrigated_cropland_3_no_c4_crops_igp_0.500000Deg.nc',
        'canopy_height' : 'canopy_height_irrigated_cropland_3_no_c4_crops_igp_0.500000Deg.nc',
        'nc_index' : 2,
        'land_cover' : 'jamr_esa_cci_lc_frac_irrigated_triple_season_2015_igp.tif',
        'jules_index' : 9
    },
    'irrigated_continuous' : {
        'lai' : 'lai_irrigated_cropland_c_no_c4_crops_igp_0.500000Deg.nc',
        'canopy_height' : 'canopy_height_irrigated_cropland_c_no_c4_crops_igp_0.500000Deg.nc',
        'nc_index' : 2,
        'land_cover' : 'jamr_esa_cci_lc_frac_irrigated_continuous_season_2015_igp.tif',
        'jules_index' : 10
    }# ,
    # 'urban' : {
    #     'lai' : None,
    #     'canopy_height' : None,
    #     'nc_index' : None,
    #     'land_cover' : 'jamr_esa_cci_lc_frac_urban_natural_2015_igp.tif',
    #     'jules_index' : 10
    # },
    # 'water' : {
    #     'lai' : None,
    #     'canopy_height' : None,
    #     'nc_index' : None,
    #     'land_cover' : 'jamr_esa_cci_lc_frac_water_natural_2015_igp.tif',
    #     'jules_index' : 11
    # },
    # 'bare_soil' : {
    #     'lai' : None,
    #     'canopy_height' : None,
    #     'nc_index' : None,
    #     'land_cover' : 'jamr_esa_cci_lc_frac_bare_soil_natural_2015_igp.tif',
    #     'jules_index' : 12
    # },
    # 'snow_ice' : {
    #     'lai' : None,
    #     'canopy_height' : None,
    #     'nc_index' : None,
    #     'land_cover' : 'jamr_esa_cci_lc_frac_snow_ice_natural_2015_igp.tif',
    #     'jules_index' : 13
    # }    
}

# Get number of vegetation types, based on dictionary, above
NVEG = len(LC_DICT)

# Define time series of LAI/canopy height
YR = 2015
T0 = datetime.datetime(YR, 1, 10, 0, 0)
TMS = [T0]
for i in np.arange(1, 36):
    TMS.append(T0 + datetime.timedelta(10))
    T0 = TMS[i]
NT = len(TMS)

def create_template_veg_climatology_netcdf(fpath):
    nco = netCDF4.Dataset(fpath, 'w', format='NETCDF4')
    nco.createDimension('time', None)
    nco.createDimension('dim1', NVEG)
    nco.createDimension('latitude', NLAT)
    nco.createDimension('longitude', NLON)
    nco.createDimension('bnds', 2)
    
    var = nco.createVariable('longitude', 'f8', ('longitude',))
    var.axis = 'X'
    var.bounds = 'longitude_bnds'
    var.units = 'degrees_east'
    var.standard_name = 'longitude'
    var[:] = LON_VALS
    var = nco.createVariable('longitude_bnds', 'f8', ('longitude', 'bnds'))
    var[:] = LON_BNDS        

    var = nco.createVariable('latitude', 'f8', ('latitude',))
    var.axis = 'Y'
    var.bounds = 'latitude_bnds'
    var.units = 'degrees_north'
    var.standard_name = 'latitude'
    var[:] = LAT_VALS        
    var = nco.createVariable('latitude_bnds', 'f8', ('latitude', 'bnds'))
    var[:] = LAT_BNDS

    var = nco.createVariable('latitude_longitude', 'i4')
    var.grid_mapping_name = 'latitude_longitude'
    var.longitude_of_prime_meridian = 0.
    var.earth_radius = 6371229.

    var = nco.createVariable('time', 'i4', ('time',))
    var.axis = 'T'
    var.units = 'hours since 1970-01-01 00:00:00'
    var.calendar = 'gregorian'
    var[:] = netCDF4.date2num(TMS, var.units, var.calendar)

    var = nco.createVariable('pseudo_level', 'i4', ('dim1',))
    var.units = '1'
    var.long_name = 'pseudo_level'
    var[:] = np.arange(1, NVEG + 1)    
    return nco
    
def create_lai_netcdf(fpath):
    nco = create_template_veg_climatology_netcdf(fpath)
    var = nco.createVariable(
        'leaf_area_index', 'f8', ('time', 'dim1', 'latitude', 'longitude')
    )
    var.standard_name = 'leaf_area_index'
    var.units = '1'
    var.um_stash_source = 'm01s00i217'
    var.cell_methods = 'time: mean within days time: mean over years'
    var.grid_mapping = 'latitude_longitude'
    var.coordinates = 'pseudo_level'
    return nco

def create_canopy_height_netcdf(fpath):
    nco = create_template_veg_climatology_netcdf(fpath)
    var = nco.createVariable(
        'canopy_height', 'f8', ('time', 'dim1', 'latitude', 'longitude')
    )
    var.standard_name = 'canopy_height'
    var.units = 'm'
    var.um_stash_source = 'm01s00i218'
    var.cell_methods = 'time: mean within days time: mean over years'
    var.grid_mapping = 'latitude_longitude'
    var.coordinates = 'pseudo_level'
    return nco

LAI_NCFILE = str(os.environ['LAI_NCFILE'])
CANOPY_HEIGHT_NCFILE = str(os.environ['CANOPY_HEIGHT_NCFILE'])

def main():
    lai_nc = create_lai_netcdf(LAI_NCFILE)
    canopy_height_nc = create_canopy_height_netcdf(CANOPY_HEIGHT_NCFILE)
    for time_index in range(len(TMS)):
        for LC in LC_DICT.keys():
            nc_index = LC_DICT[LC]['nc_index']
            jules_index = LC_DICT[LC]['jules_index']
            lai_fn = os.path.join(LAIDATADIR, LC_DICT[LC]['lai'])
            canopy_height_fn = os.path.join(LAIDATADIR, LC_DICT[LC]['canopy_height'])

            if lai_fn is not None:
                lai_nc['leaf_area_index'][time_index, jules_index, :, :] = (
                    xarray.open_dataset(lai_fn)
                )['leaf_area_index'][time_index, nc_index, :, :].values
                
            if canopy_height_fn is not None:
                canopy_height_nc['canopy_height'][time_index, jules_index, :, :] = (
                    xarray.open_dataset(canopy_height_fn)
                )['canopy_height'][time_index, nc_index, :, :].values
                
    lai_nc.close()
    canopy_height_nc.close()

if __name__ == '__main__':
    main()
