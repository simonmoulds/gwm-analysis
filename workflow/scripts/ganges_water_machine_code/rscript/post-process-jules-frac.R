## Author : Simon Moulds
## Date   : Sep 2021

library(magrittr)
library(tidyr)
library(dplyr)
library(raster)
library(ncdf4)

## 1979-2014

## frac0 = stack(
##     "../../data/netcdf/jules_frac_5pft_ants_2015_CUSTOM_igp.nc",
##     varname="land_cover_lccs"
## )

nc = nc_open("../../data/netcdf/jules_frac_5pft_ants_2015_CUSTOM_igp.nc")
frac0 = ncvar_get(nc, "land_cover_lccs")
nc_close(nc)

template = raster(nrow=40, ncol=80, xmn=60, xmx=100, ymn=20, ymx=40)
## This works because the output is S-N and `raster` fills data
## from the bottom left
landuse_names = c(
    "tree_broadleaf_natural",
    "tree_needleleaf_natural",
    "c3_grass_natural",
    "c4_grass_natural",
    "shrub_natural",
    "rainfed",
    "irrigated_single_season",
    "irrigated_double_season",
    "irrigated_triple_season",
    "fallow",
    "urban_natural",
    "water_natural",
    "bare_soil_natural",
    "snow_ice_natural"
)
n_landuse = length(landuse_names)
maps = vector(mode="list", length=n_landuse)
names(maps) = landuse_names
for (i in 1:n_landuse) {
    nm = landuse_names[i]
    r = template
    r[] = as.numeric(frac0[,,i])
    maps[[nm]] = r
}

df0 = stack(maps) %>% as.data.frame

## /home/sm510/projects/icrisat/data/gross_cropland_area_india_1999.tif
## /home/sm510/projects/icrisat/data/gross_irrigated_area_india_1999.tif
## /home/sm510/projects/icrisat/data/net_cropland_area_india_1999.tif
## /home/sm510/projects/icrisat/data/net_irrigated_area_india_1999.tif
## /home/sm510/projects/icrisat/data/summer_canal_area_india_1999.tif
## /home/sm510/projects/icrisat/data/summer_other_sources_area_india_1999.tif
## /home/sm510/projects/icrisat/data/summer_other_wells_area_india_1999.tif
## /home/sm510/projects/icrisat/data/summer_tanks_area_india_1999.tif
## /home/sm510/projects/icrisat/data/summer_tubewells_area_india_1999.tif
## /home/sm510/projects/icrisat/data/winter_canal_area_india_1999.tif
## /home/sm510/projects/icrisat/data/winter_other_sources_area_india_1999.tif
## /home/sm510/projects/icrisat/data/winter_other_wells_area_india_1999.tif
## /home/sm510/projects/icrisat/data/winter_tanks_area_india_1999.tif
## /home/sm510/projects/icrisat/data/winter_tubewells_area_india_1999.tif

for (yr in 1979:2014) {    
}

