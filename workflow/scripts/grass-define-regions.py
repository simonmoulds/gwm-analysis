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

# Now we can create a grass session instance
my_gisdb = os.path.join(os.getenv('HOME'), 'grassdata')
my_location = 'latlong'
my_mapset = 'gwm'

user = Session()
user.open(gisdb=my_gisdb, location=my_location, mapset=my_mapset)

# ##################################### #
# Define some regions                   #
# ##################################### #

# 0.5 degrees (not currently used but useful to have)
g.region(
    e='180E', w='180W', n='90N', s='90S', res='0:30',
    save='globe_0.500000Deg', overwrite=True
)
# 0.008333 degrees (frac, soil)
g.region(
    e='180E', w='180W', n='90N', s='90S', res='0:00:30',
    save='globe_0.008333Deg', overwrite=True
)
# 0.002778 degrees (ESA CCI)
g.region(
    e='180E', w='180W', n='90N', s='90S', res='0:00:10',
    save='globe_0.002778Deg', overwrite=True
)

# Indo-Gangetic Plain
EAST = '100E'
WEST = '60E'
NORTH = '40N'
SOUTH = '20N'

# 0.008333 degrees
g.region(
    e=EAST, w=WEST, n=NORTH, s=SOUTH, res='0:00:30',
    save='igp_0.008333Deg', overwrite=True
)

# 0.002778 degrees (ESA CCI)
g.region(
    e=EAST, w=WEST, n=NORTH, s=SOUTH, res='0:00:10',
    save='igp_0.002778Deg', overwrite=True
)

# 0.5 degrees
g.region(
    e=EAST, w=WEST, n=NORTH, s=SOUTH, res='0:30',
    save='igp_0.500000Deg', overwrite=True
)

# 0.25 degrees
g.region(
    e=EAST, w=WEST, n=NORTH, s=SOUTH, res='0:15',
    save='igp_0.250000Deg', overwrite=True
)

# 0.1 degrees
g.region(
    e=EAST, w=WEST, n=NORTH, s=SOUTH, res='0:06',
    save='igp_0.100000Deg', overwrite=True
)

# 0.083333 degrees
g.region(
    e=EAST, w=WEST, n=NORTH, s=SOUTH, res='0:05',
    save='igp_0.083333Deg', overwrite=True
)

# 0.041667 degrees
g.region(
    e=EAST, w=WEST, n=NORTH, s=SOUTH, res='0:02:30',
    save='igp_0.041667Deg', overwrite=True
)
