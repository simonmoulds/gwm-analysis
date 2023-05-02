#!/usr/bin/env python3

import os

from grass_session import Session
from grass.script import core as gcore
import grass.script as gscript
import grass.script.setup as gsetup

# Import grass python libraries
from grass.pygrass.modules.shortcuts import general as g
from grass.pygrass.modules.shortcuts import raster as r
from grass.pygrass.modules.shortcuts import vector as v
from grass.pygrass.modules.shortcuts import temporal as t

# Copied from another workflow, as reminder of how to access snakemake constants
# station_file = snakemake.input['stations']
# station = int(snakemake.wildcards['stn'])
# # init_month = int(snakemake.wildcards['init_month'])
# lead_time = int(snakemake.wildcards['lead_time'])
# # season = snakemake.wildcards['season']
# test_period_start = snakemake.config['modelling']['random_forest']['test_start']
# test_period_end = snakemake.config['modelling']['random_forest']['test_end']

# Now we can create a grass session instance
my_gisdb = os.path.join(os.getenv('HOME'), 'grassdata')
my_location = 'latlong'
my_mapset = 'gwm'

user = Session()
user.open(gisdb=my_gisdb, location=my_location, mapset=my_mapset)

r.in_gdal(
    flags="a",
    input="results/WFD-EI-LandFraction2d_IGP.tif",
    output="igp_template",
    overwrite=True
)
g.region(
    raster="igp_template", save="igp", overwrite=True
)
g.region(flags="p")
