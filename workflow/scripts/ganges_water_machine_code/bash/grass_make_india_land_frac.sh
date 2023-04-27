#!/bin/bash

r.mask -r

# 0.008333 degrees
g.region \
    e=100E w=60E n=40N s=0N \
    res=0:00:30 # \
    # save=india_0.008333Deg \
    # --overwrite

# unzip -o ../data-raw/g2015_2010_0.zip -d ../data

# # Land boundaries
# v.in.ogr \
#     input=../data/g2015_2010_0/g2015_2010_0.shp \
#     output=g2015_2010_0 \
#     spatial=60,0,100,40 \
#     --overwrite

v.to.rast \
    input=g2015_2010_0 \
    output=g2015_2010_0 \
    type=area \
    use=attr \
    attr=ADM0_CODE \
    --overwrite

# v.in.ogr \
#     input=../../icrisat/data/icrisat_polygons.gpkg \
#     output=icrisat_polys \
#     spatial=60,0,100,40 \
#     --overwrite

v.to.rast \
    input=icrisat_polys \
    output=icrisat_polys \
    type=area \
    use=attr \
    attr=POLY_ID \
    --overwrite

r.mapcalc \
    "tmp1 = g2015_2010_0 * 0" \
    --overwrite

r.mapcalc \
    "tmp2 = if(isnull(icrisat_polys),0,1)" \
    --overwrite

r.mapcalc \
    "tmp3 = tmp1 + tmp2" \
    --overwrite

g.region \
    e=100E w=60E n=40N s=0N \
    res=0:30
    # save=india_0.008333Deg \
    # --overwrite
    
r.resamp.stats \
    input=tmp3 \
    output=tmp4 \
    method=average \
    --overwrite

r.out.gdal \
    input=tmp4 \
    output=../data/india_frac_0.500000Deg.tif \
    --overwrite

# # OLD:

# r.mapcalc \
#     "tmp1 = if((g2015_2010_0==115||g2015_2010_0==40781||g2015_2010_0==2||g2015_2010_0==52||g2015_2010_0==15),1,0)" \
#     --overwrite

# # r.out.gdal \
# #     input=tmp1 \
# #     output=test1.tif \
# #     --overwrite

# g.region \
#     e=100E w=60E n=40N s=0N \
#     res=0:30
#     # save=india_0.008333Deg \
#     # --overwrite
    
# r.resamp.stats \
#     input=tmp1 \
#     output=tmp2 \
#     method=average \
#     --overwrite

# r.out.gdal \
#     input=tmp2 \
#     output=../data/india_frac_0.500000Deg.tif
#     --overwrite
