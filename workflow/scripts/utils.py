#!/usr/bin/env python
# -*- coding: utf-8 -*-

import numpy as np
import xarray
import pandas as pd
from constants import *

def aggregate_to_month(x):
    # Get the number of days in each month for the current dataset
    days_in_month = xarray.DataArray(
        np.ones(x.time.shape[0]),
        {'time' : x.time.values},
        dims=['time']
    ).groupby('time.month').sum(dim='time')
    x_month = (
        x.groupby('time.month').mean(dim='time')
        * 60 * 60 * 24 * days_in_month
    )
    return x_month

def convert_to_2d(x, variables):

    # Obtain length of dimensions
    if 'time' in x.dims:
        ntime = x.dims['time']
    if 'month' in x.dims:
        ntime = x.dims['month']
    if 'tile' in x.dims:
        ntile = x.dims['tile']
    if 'soil' in x.dims:                
        nsoil = x.dims['soil']
    if 'pft' in x.dims:
        npft = x.dims['pft']
    if 'soilt' in x.dims:
        nsoilt = x.dims['soilt']

    coords = pd.DataFrame(
        {'lat' : x['latitude'].values[:].squeeze(),
         'lon' : x['longitude'].values[:].squeeze()
        }
    )
    coords = coords.merge(LAT_INDEX, 'left').merge(LON_INDEX, 'left')
    indices = np.array([[coords.LAT_INDEX[i], coords.LON_INDEX[i]] for i in range(coords.shape[0])])

    def get_coords(soil=False, soilt=False, tile=False, pft=False):
        coords={}
        dims=[]
        if 'time' in x.dims:
            coords['time'] = x.time.values[:]
            dims.append('time')
        elif 'month' in x.dims:
            coords['month'] = x.month.values[:]
            dims.append('month')            
        if soil:
            coords[SOIL_DIM_NAME] = np.arange(1, nsoil + 1)
            dims.append(SOIL_DIM_NAME)
        if soilt:
            coords['soilt'] = x.soilt.values[:]
            dims.append('soilt')
        if tile:
            coords[TILE_DIM_NAME] = np.arange(1, ntile + 1)
            dims.append(TILE_DIM_NAME)
        if pft:
            coords[PFT_DIM_NAME] = np.arange(1, npft + 1)
            dims.append(PFT_DIM_NAME)
        coords['lat'] = LAT
        coords['lon'] = LON
        dims = dims + ['lat', 'lon']
        return coords, dims
    
    # Define functions to handle outputs with different dimensions
    def convert_gridbox_soilt_var(output_var):
        # (time, soilt, y, x)
        arr = np.zeros((ntime, nsoilt) + (NLAT, NLON)) * np.nan
        for i in range(ntime):
            for j in range(nsoilt):
                output_arr = output_var.values[:][i, j, 0, :]
                arr_ij = arr[i, j, ...]
                arr_ij[tuple(indices.T)] = output_arr
                arr[i, j, ...] = arr_ij
        coords, dims = get_coords(soilt=True)
        xarr = xarray.DataArray(arr, coords, dims, attrs=output_var.attrs)
        return xarr
                    
    def convert_gridbox_var(output_var):
        # (time, y, x)
        arr = np.zeros((ntime,) + (NLAT, NLON)) * np.nan
        for i in range(ntime):
            output_arr = output_var.values[:][i, 0, :]
            arr_ij = arr[i, ...]
            arr_ij[tuple(indices.T)] = output_arr
            arr[i, ...] = arr_ij
        # add mask
        coords, dims = get_coords()
        xarr = xarray.DataArray(arr, coords, dims, attrs=output_var.attrs)
        return xarr

    def convert_tile_var(output_var):
        # (time, tile, y, x)
        arr = np.zeros((ntime, ntile) + (NLAT, NLON)) * np.nan
        for i in range(ntime):
            for j in range(ntile):
                output_arr = output_var.values[:][i, j, 0, :]
                arr_ij = arr[i, j, ...]
                arr_ij[tuple(indices.T)] = output_arr
                arr[i, j, ...] = arr_ij
        coords, dims = get_coords(tile=True)
        xarr = xarray.DataArray(arr, coords, dims, attrs=output_var.attrs)
        return xarr

    def convert_soil_soilt_var(output_var):
        # (time, soil, soilt, y, x)
        arr = np.zeros((ntime, nsoil, nsoilt) + (NLAT, NLON)) * np.nan
        for i in range(ntime):
            for j in range(nsoil):
                for k in range(nsoilt):                            
                    output_arr = output_var.values[:][i, j, k, 0, :]
                    arr_ijk = arr[i, j, k, ...]
                    arr_ijk[tuple(indices.T)] = output_arr
                    arr[i, j, k, ...] = arr_ijk
        coords, dims = get_coords(soil=True, soilt=True)
        xarr = xarray.DataArray(arr, coords, dims, attrs=output_var.attrs)
        return xarr

    def convert_soil_var(output_var):
        # (time, soil, y, x)
        arr = np.zeros((ntime, nsoil) + (NLAT, NLON)) * np.nan
        for i in range(ntime):
            for j in range(nsoil):
                output_arr = output_var.values[:][i, j, 0, :]
                arr_ij = arr[i, j, ...]
                arr_ij[tuple(indices.T)] = output_arr
                arr[i, j, ...] = arr_ij
        coords, dims = get_coords(soil=True)
        xarr = xarray.DataArray(arr, coords, dims, attrs=output_var.attrs)
        return xarr

    def convert_pft_var(output_var):
        # (time, pft, y, x)
        arr = np.zeros((ntime, npft) + (NLAT, NLON)) * np.nan
        for i in range(ntime):
            for j in range(npft):
                output_arr = output_var.values[:][i, j, 0, :]
                arr_ij = arr[i, j, ...]
                arr_ij[tuple(indices.T)] = output_arr
                arr[i, j, ...] = arr_ij
        coords, dims = get_coords(pft=True)
        xarr = xarray.DataArray(arr, coords, dims, attrs=output_var.attrs)
        return xarr

    # Loop through variables and create a list of DataArrays
    xarr_list = []
    for var in variables:
        if var in x.variables:
            if 'tile' in x[var].dims:
                xarr = convert_tile_var(x[var])
            elif 'soil' in x[var].dims:
                if 'soilt' in x[var].dims:
                    xarr = convert_soil_soilt_var(x[var])
                else:
                    xarr = convert_soil_var(x[var])

            elif 'pft' in x[var].dims:
                xarr = convert_pft_var(x[var])
            else:
                if 'soilt' in x[var].dims:
                    xarr = convert_gridbox_soilt_var(x[var])
                else:
                    xarr = convert_gridbox_var(x[var])
            xarr.name = var
            xarr_list.append(xarr)

    # Merge DataArray objects into single dataset
    ds = xarray.merge(xarr_list)
    return ds
