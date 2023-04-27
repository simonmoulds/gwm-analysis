#!/bin/bash

# This script defines some regions which are used during the analysis

# 0.5 degrees (not currently used but useful to have)
g.region \
    e=180E w=180W n=90N s=90S \
    res=0:30 \
    save=globe_0.500000Deg \
    $OVERWRITE

# 0.008333 degrees (frac, soil)
g.region \
    e=180E w=180W n=90N s=90S \
    res=0:00:30 \
    save=globe_0.008333Deg \
    $OVERWRITE

# 0.002778 degrees (ESA CCI)
g.region \
    e=180E w=180W n=90N s=90S \
    res=0:00:10 \
    save=globe_0.002778Deg \
    $OVERWRITE

EAST=100E
WEST=60E
NORTH=40N
SOUTH=20N

# 0.008333 degrees
g.region \
    e=$EAST w=$WEST n=$NORTH s=$SOUTH \
    res=0:00:30 \
    save=igp_0.008333Deg \
    --overwrite

# 0.002778 degrees (ESA CCI)
g.region \
    e=$EAST w=$WEST n=$NORTH s=$SOUTH \
    res=0:00:10 \
    save=igp_0.002778Deg \
    --overwrite

# 0.5 degrees
g.region \
    e=$EAST w=$WEST n=$NORTH s=$SOUTH \
    res=0:30 \
    save=igp_0.500000Deg \
    --overwrite

# 0.25 degrees
g.region \
    e=$EAST w=$WEST n=$NORTH s=$SOUTH \
    res=0:15 \
    save=igp_0.250000Deg \
    --overwrite

# 0.1 degrees
g.region \
    e=$EAST w=$WEST n=$NORTH s=$SOUTH \
    res=0:06 \
    save=igp_0.100000Deg \
    --overwrite

# 0.083333 degrees
g.region \
    e=$EAST w=$WEST n=$NORTH s=$SOUTH \
    res=0:05 \
    save=igp_0.083333Deg \
    --overwrite

# 0.041667 degrees
g.region \
    e=$EAST w=$WEST n=$NORTH s=$SOUTH \
    res=0:02:30 \
    save=igp_0.041667Deg \
    --overwrite

# NOT USED:

# # 0.125 degrees
# g.region \
#     e=180E w=180W n=90N s=90S \
#     res=0:07:30 \
#     save=globe_0.125000Deg \
#     $OVERWRITE

# # 0.0625 degrees
# g.region \
#     e=180E w=180W n=90N s=90S \
#     res=0:03:45 \
#     save=globe_0.062500Deg \
#     $OVERWRITE

# # 0.01 degrees
# g.region \
#     e=180E w=180W n=90N s=90S \
#     res=0:00:36 \
#     save=globe_0.010000Deg \
#     $OVERWRITE
