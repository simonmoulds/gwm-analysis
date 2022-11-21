#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import numpy as np
import netCDF4
import xarray
import pandas as pd
import datetime
import calendar
import click
import yaml
from tqdm import tqdm

# The point of this script is to disaggregate JULES
# irrig_water output between the various irrigated
# land cover types.

# TODO:
# * Check whether we ought to account for dynamic land cover
# * Check whether we need to change the year start (i.e. start of kharif/rabi)

F8_FILLVAL = netCDF4.default_fillvals['f8']

@click.command()
@click.option('-i', '--inputfile', default='.', help='Name of output file')
@click.option('-o', '--outputfile', default='.', help='Name of output file')
@click.option('--config', default='config.yml', help='YAML configuration file')
def main(inputfile, outputfile, config):

    with open(config, 'r') as f:
        config = yaml.load(f, Loader=yaml.FullLoader)
    ancil_datadir = config['ancil_datadir']

    land_fn = os.path.join(
        ancil_datadir,
        'WFD-EI-LandFraction2d_igp.nc'
    )
    frac_fn = os.path.join(
        ancil_datadir,
        'jules_5pft_w_crops_veg_frac_2015_igp_wfdei.nc'
    )
    irr_schedule_fn = os.path.join(
        ancil_datadir,
        'jules_5pft_w_crops_irrig_schedule.nc'
    )
    irr_schedule_ref_year = 2015

    # Read land mask to use as mask when creating output netCDF file
    land = xarray.open_dataset(land_fn)['lsmask']
    mask = (1 - land.values).astype(bool)

    # Read land cover fraction data, and extract values for irrigated land covers
    frac = xarray.open_dataset(frac_fn)['land_cover_lccs']
    dim0_values = frac['dim0'].values
    mask_3d = (
        mask[None,...]
        * np.ones(len(dim0_values), dtype=bool)[:,None,None]
    )

    # FIXME be careful here!!!
    irr_frac = frac.values[6:10, ...] * land.values

    # Read JULES files to process
    with open(inputfile, 'r') as f:
        input_filelist = [ln.strip() for ln in f.readlines()]

    # Read irrigation schedule
    irr_schedule = xarray.open_dataset(irr_schedule_fn)['irr_schedule']

    output_filelist = open(outputfile, 'w')
    for filepath in tqdm(input_filelist):
        path = os.path.split(filepath)[0]
        filename = os.path.split(filepath)[1]
        basename = os.path.splitext(filename)[0]
        nc_outputfile = os.path.join(
            path,
            filename.replace('irrig_water', 'rel_irrig_water')
        )

        ds = xarray.open_dataset(filepath)
        try:
            irrig_water = ds['irrig_water']
        except KeyError:
            continue

        ds.close()
        irrig_water_time = [
            pd.Timestamp(tm).to_pydatetime() for tm in irrig_water.time.values
        ]

        # Get time dimension from output file
        with netCDF4.Dataset(filepath) as nc:
            time_units = nc['time'].units
            time_calendar = nc['time'].calendar
            time_values = nc['time'][:]
            lat_values = nc['lat'][:]
            lon_values = nc['lon'][:]
            irr_units = nc['irrig_water'].units
            irr_long_name = nc['irrig_water'].long_name

        # Create new netCDF4 file to store relative irrigation water use
        ncout = netCDF4.Dataset(nc_outputfile, 'w')
        ncout.createDimension('time', None)
        ncout.createDimension('dim0', len(dim0_values))
        ncout.createDimension('lat', len(lat_values))
        ncout.createDimension('lon', len(lon_values))

        var = ncout.createVariable('time', 'i4', ('time',))
        var.units = time_units
        var.calendar = time_calendar
        var[:] = time_values

        var = ncout.createVariable('dim0', 'i4', ('dim0',))
        var[:] = dim0_values

        var = ncout.createVariable('lat', np.float32, ('lat',))
        var.units = 'degrees_north'
        var[:] = lat_values

        var = ncout.createVariable('lon', np.float32, ('lon',))
        var.units = 'degrees_east'
        var[:] = lon_values

        var = ncout.createVariable('irrig_water', 'f8', ('time', 'dim0', 'lat', 'lon'), fill_value=F8_FILLVAL)
        var.standard_name = 'irrig_water'
        var.long_name = irr_long_name
        var.units = irr_units

        for index, tm in enumerate(irrig_water_time):
            # Retrieve the irrigation schedule for today

            if calendar.isleap(tm.year) & (tm.month == 2) & (~calendar.isleap(irr_schedule_ref_year)):
                day = min(tm.day, 28)
            else:
                day = tm.day

            irr_schedule_tm = datetime.datetime(
                irr_schedule_ref_year,
                tm.month, day, tm.hour, tm.minute
            )
            irr_schedule_today = irr_schedule.sel(tstep=irr_schedule_tm)
            # Calculate the relative irrigated area
            # among land covers irrigated today
            irr_schedule_today = irr_schedule_today.values[6:10, ...]
            irr_frac_today = irr_frac * irr_schedule_today
            total_irr_frac_today = irr_frac_today.sum(axis=0)
            rel_irr_frac_today = np.divide(
                irr_frac_today,
                total_irr_frac_today,
                out=np.zeros_like(irr_frac),
                where=total_irr_frac_today > 0
            )
            irrig_water_today = irrig_water.sel(time=tm)
            print(irrig_water_today)
            # irrig_water_today = np.nan_to_num(irrig_water_today.values)
            # irrig_water_today_by_frac = np.zeros(frac.shape)
            # irrig_water_today_by_frac[6:10,...] = (
            #     irrig_water_today * rel_irr_frac_today
            # )
            # # # Rescale to counter any precision errors which have crept in
            # # irrig_water_today_sum = irrig_water_today_by_frac.sum(axis=0)
            # # scale_factor = np.divide(
            # #     irrig_water_today,
            # #     irrig_water_today_sum,
            # #     out=np.zeros_like(irrig_water_today_sum),
            # #     where=irrig_water_today_sum>0
            # # )
            # # irrig_water_today_by_frac *= scale_factor
            # # Do a check
            # irrig_water_today_check = irrig_water_today_by_frac.sum(axis=0)
            # close = np.allclose(
            #     irrig_water_today_check[mask],
            #     irrig_water_today[mask]
            # )
            # if not close:
            #     raise IOError
            # irrig_water_today_by_frac = np.ma.array(
            #     irrig_water_today_by_frac, mask=mask_3d
            # )
            # var[index, ...] = irrig_water_today_by_frac

        ncout.close()
        # land.close()
        # frac.close()
        # irr_schedule.close()
        # # # Now aggregate to month using xarray
        # # x = xarray.open_dataset(fname)
        # # x['irrig_water'] = x['irrig_water'] * 60 * 60 * 24 / 1000
        # # x_month = x.groupby('time.month').sum(dim='time')
        # # fname = os.path.join(
        # #     OUTPUTDIR,
        # #     JULES_ID_STEM + '.jules_' + str(year)
        # #     + '.daily_hydrology.rel_irrig_water.'
        # #     + str(year) + '.2D.month.nc'
        # # )
        # # x_month.to_netcdf(fname)
        # # x.close()

        # Add output file to list
        output_filelist.write(("%s" + os.linesep) % nc_outputfile)

    output_filelist.close()

if __name__ == '__main__':
    main()

