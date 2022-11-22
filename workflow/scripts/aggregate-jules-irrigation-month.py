#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import numpy as np
import xarray as xr
import click
from tqdm import tqdm
# from constants import OUTPUT_VARS


@click.command()
@click.option('-i', '--inputfile', default='.', help='Name of input file')
@click.option('-o', '--outputfile', default='.', help='Name of output file')
@click.option('--config', default='config.yml', help='YAML configuration file')
def main(inputfile, outputfile, config):

    period = period.upper()
    if period not in ['YEAR', 'MONTH']:
        raise ValueError('`period` must be one of month or year')

    with open(inputfile, 'r') as f:
        regrid_filelist = [ln.strip() for ln in f.readlines()]

    output_filelist = open(outputfile, 'w')
    for filepath in tqdm(regrid_filelist):
        path = os.path.split(filepath)[0]
        filename = os.path.split(filepath)[1]
        basename = os.path.splitext(filename)[0]
        x = xr.open_dataset(os.path.join(filepath))
        # Check whether I need to do this again
        # Convert from mass to depth kg m-2 s-1 -> m d-1
        x['irrig_water'] = x['irrig_water'] * 60 * 60 * 24 / 1000
        nc_outputfile = os.path.join(path, basename + '.month.nc')
        x_aggr = x.groupby("time.month").sum(dim="time") # m d-1 -> m month-1
        x_aggr.to_netcdf(nc_outputfile)
        x.close()
        output_filelist.write(("%s" + os.linesep) % nc_outputfile)
    output_filelist.close()

if __name__ == '__main__':
    main()
