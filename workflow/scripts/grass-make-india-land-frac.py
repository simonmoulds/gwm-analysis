#!/usr/bin/env python3

import os
import subprocess

from grass_session import Session
from grass.script import core as gcore
import grass.script as gscript
import grass.script.setup as gsetup

# Import grass python libraries
from grass.pygrass.modules.shortcuts import general as g
from grass.pygrass.modules.shortcuts import raster as r
from grass.pygrass.modules.shortcuts import vector as v
from grass.pygrass.modules.shortcuts import temporal as t

from utils import write_reclass_rules
from constants import (esa_land_covers,
                       jules_land_covers,
                       jules_land_cover_components)

# Now we can create a grass session instance
my_gisdb = os.path.join(os.getenv('HOME'), 'grassdata')
my_location = 'latlong'
my_mapset = 'gwm'

user = Session()
user.open(gisdb=my_gisdb, location=my_location, mapset=my_mapset)

try:
    r.mask(flags="r")
except:
    pass

g.region(
    e='100E', w='60E', n='50N', s='0N', res='0:00:30',
    save='india_0.008333Deg', overwrite=True
)

# Land boundaries
v.in_ogr(
    input='resources/g2015_2010_0/g2015_2010_0.shp',
    output='g2015_2010_0',
    spatial='60,0,100,40',      # TODO correct?
    overwrite=True
)

v.to_rast(
    input='g2015_2010_0',
    output='g2015_2010_0',
    type='area',
    use='attr',
    attr='ADM0_CODE',
    overwrite=True
)

v.in_ogr(
    input='resources/icrisat_polygons.gpkg',
    output='icrisat_polys',
    spatial='60,0,100,40',
    overwrite=True
)

v.to_rast(
    input='icrisat_polys',
    output='icrisat_polys',
    type='area',
    use='attr',
    attr='POLY_ID',
    overwrite=True
)

r.mapcalc('tmp1 = g2015_2010_0 * 0', overwrite=True)
r.mapcalc('tmp2 = if(isnull(icrisat_polys), 0, 1)', overwrite=True)
r.mapcalc('tmp3 = tmp1 + tmp2')
g.region(e='100E', w='60E', n='50N', s='0N', res='0:30')
r.resamp_stats(input='tmp3', output='tmp4', method='average', overwrite=True)
r.out.gdal(
    input='tmp4',
    output='results/india_frac_0.500000Deg.tif',
    overwrite=True
)
