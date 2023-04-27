## Author : Simon Moulds
## Date   : Oct 2021

library(raster)
library(sf)
library(tidyverse)

## WFDEI land fraction for IGP
## TODO: any way to define extent in environment variables?
## wfdei_land_fraction =
##     raster("../data/wfdei/ancils/WFD-EI-LandFraction2d_igp.nc")
wfdei_land_fraction = raster(xmn=60, xmx=100, ymn=20, ymx=40, nrows=40, ncols=80)
wfdei_land_fraction[] = 1

## Use HydroBASINS to define river basins which need to be included in the analysis
if (!dir.exists("../data/HydroBASINS")) {
    dir.create("../data/HydroBASINS")
}
unzip("../data-raw/as.zip", exdir="../data/HydroBASINS")
unzip("../data/HydroBASINS/hybas_as_lev04_v1c.zip", exdir="../data/HydroBASINS")

basins = st_read("../data/HydroBASINS/hybas_as_lev04_v1c.shp")
basins_sp = as_Spatial(basins)
basins_rast = rasterize(basins, wfdei_land_fraction, field="HYBAS_ID")

ignore_inlet_ids = c(1,2)
inlets = read.table(
    "../data-raw/LPJ_command_inlets_all_replacements_b.txt",
    skip=1,
    sep=","
)
inlets = inlets[!inlets[,4] %in% ignore_inlet_ids,]
inlets =
    inlets[,c(5:6)] %>%
    SpatialPoints(proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
canal_basins_inlets = raster::extract(basins_rast, inlets, df=TRUE)

ignore_area_ids = c(1,2,3) # These are outside the IGP
cmd_area = st_read("../data/command_areas.shp")
cmd_area = cmd_area[!cmd_area$ID %in% ignore_area_ids, ]
cmd_area_sp = as_Spatial(cmd_area)
canal_basins_areas = raster::extract(basins_rast, cmd_area_sp, df=TRUE)

canal_basins =
    c(canal_basins_inlets[,2], canal_basins_areas[,2]) %>%
    `[`(!is.na(.))
uids = unique(canal_basins)

study_region = basins_rast %in% uids
study_region = study_region * wfdei_land_fraction

writeRaster(
    study_region,
    "../data/igp_basins.tif",
    overwrite=TRUE
)

## Now:
## 1 - convert this to netCDF
## 2 - apply to JULES suite
## 3 - use in script to create irrigation calendar

## Also - backup this project!!!!!
