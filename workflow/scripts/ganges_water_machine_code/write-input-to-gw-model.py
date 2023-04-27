#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import numpy as np
import xarray
import utils
from constants import *
# for development:
# from importlib import reload

DATADIR = '/home/sm510/JULES_output'
GW_OUTDIR = '../jules-output/u-cg201/gw'
try:
    os.makedirs(GW_OUTDIR)
except FileExistsError:
    pass

SUITE = 'u-cg201'
START = 1979
END = 2014
YEARS = np.arange(START, END + 1)
ID_STEM = 'JULES_vn6.1'
PROFILE_NAMES = ['daily_hydrology']

def main():        
    for profile in PROFILE_NAMES:
        for yr in YEARS:

            print(yr)
            
            # Open JULES 1D output file
            fn = ID_STEM + '.S2.' + profile + '.' + str(yr) + '.nc'
            x = xarray.open_dataset(os.path.join(DATADIR, SUITE, fn))
            # Daily data
            ds = utils.convert_to_2d(x, OUTPUT_VARS[profile])
            # Subtract arbitrary amount so that the date is correct (JULES
            # writes output for the previous day at midnight - e.g. values
            # covering 1979-01-01 are written at 1979-01-02 00:00:00
            ds['time'] = ds.time - np.timedelta64(1, 'h')
            nt = ds.time.size

            # ########################## #
            # recharge [sub_surf_roff]   # 
            # ########################## #

            def get_jules_rate_var(ds, var, time_index):
                x = np.flipud(ds[var][time_index,:,:].values)
                x = np.nan_to_num(x)
                # x[np.isnan(x)] = 0.
                x[x < 0.] = 0.
                x *= (60 * 60 * 24)
                return x
            
            recharge_fn = os.path.join(GW_OUTDIR, 'recharge_actual_' + str(yr) + '.txt')
            with open(recharge_fn, 'wb') as f:
                for i in range(nt):
                    recharge = get_jules_rate_var(ds, 'sub_surf_roff', i)
                    f.write(b"----\n")
                    np.savetxt(f, recharge, '%.1f')                    

            # ########################## #
            # pumping [depends on policy]
            # ########################## #
            
            # assumption is that canals are only viable during jjas
            def is_jjas(month):
                return (month >= 6) & (month <= 9)            

            # read canal area
            actual_canal_area = xarray.open_rasterio("../data/igp_canal_fraction_2010.tif").values.squeeze()
            pumping_fn = os.path.join(GW_OUTDIR, 'pumping_actual_' + str(yr) + '.txt')
            with open(pumping_fn, 'wb') as f:
                for i in range(nt):
                    pumping = get_jules_rate_var(ds, 'irrig_water', i)
                    tm = pd.Timestamp(ds.time.values[i])                    
                    if is_jjas(tm.month):
                        pumping *= (1 - actual_canal_area)
                    f.write(b"----\n")
                    np.savetxt(f, pumping, '%.1f')

            # close dataset
            ds.close()


if __name__ == '__main__':
    main()

