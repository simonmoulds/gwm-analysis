## Author  : Simon Moulds
## Date    : Aug 2015-

library(magrittr)
library(dplyr)
library(tidyr)
library(raster)
library(rgdal)
library(gdalUtils)

options(stringsAsFactors=FALSE)
## devtools::load_all("pkg/r_indagridat/indagridat")

## Input/output directories
icrisat_dir = "/home/sm510/projects/icrisat/data/output_maps"
jules_dir = "/home/sm510/projects/ganges-water-machine/data/wfdei/ancils"

## Pixels belonging to continental India
land_frac = raster(
    file.path(icrisat_dir, "icrisat_india_land_frac.tif")
)
land_area = area(land_frac) * land_frac

template = land_frac
## template_coarse = raster()

## Load polygon data
adm2_2001_ll = readOGR(
    dsn="../data-raw/india_adm2_1990/data",
    layer="g2001_2_India"
)
## adm2_1960_ll = readOGR(
##     dsn=system.file("shapes", package="indagridat"),
##     layer="g1960_2_India")

## adm2_2001_ll$ADM2_CODE %<>% as.numeric
## adm2_1960_ll$ADM2_CODE %<>% as.numeric
## adm2_2001_ll$ADM1_CODE %<>% as.numeric
## adm2_1960_ll$ADM1_CODE %<>% as.numeric
## adm2_2001_ll$ADM0_CODE %<>% as.numeric
## adm2_1960_ll$ADM0_CODE %<>% as.numeric

## Neighbouring countries
system("unzip -o ../data-raw/g2015_2010_0.zip -d ../data")
india_nb_ll = readOGR(
    dsn="data/g2015_2010_0",
    layer="g2015_2010_0"
)
india_nb_ll$ADM0_CODE %<>% as.numeric

## Neighbourhood - this is needed to work out if a grid cell
## borders the ocean or a neighbouring country
nb_coarse = rasterize(
    india_nb_ll,
    template,
    ## template_coarse_ll,
    field="ADM0_CODE"
)

## india_adm0 = 
nb_coarse[(nb_coarse != india_adm0)] = 0


## nb_fine = rasterize(
##     india_nb_ll,
##     template_fine_ll,
##     field="ADM0_CODE"
## )
## writeRaster(
##     nb_coarse,
##     "data/india_nb_coarse.tif",
##     overwrite=TRUE
## )
## writeRaster(
##     nb_fine,
##     "data/india_nb_fine.tif",
##     overwrite=TRUE
## )

## ## Rasterize district vector map
## dist_map_1960 = rasterize(
##     adm2_1960_ll,
##     template_coarse_ll,
##     "ADM2_CODE"
## )
## dist_map_2001 = rasterize(
##     adm2_2001_ll,
##     template_coarse_ll,
##     "ADM2_CODE"
## )
## state_map_2001 = rasterize(
##     adm2_2001_ll,
##     template_coarse_ll,
##     "ADM1_CODE"
## )
## rgn_map = raster(state_map_2001)

## ## NB dist_map_1960 and dist_map_2001 do not have
## ## the same non-NA cells.
## ## na_2001 = is.na(dist_map_2001)
## ## na_1960 = is.na(dist_map_1960)
## ## v2001=getValues(na_2001)
## ## v1960=getValues(na_1960)
## ## which(v2001==0 & v1960==1)

## ## UPDATE 15/09/2021: The difference is only one cell,
## ## so let's disregard for now.

## ## Aquastat irrigation calendar regions (indices are state codes)
## ## NB - Aquastat divides the country into four regions
## ## (n/e/s/w), each with a different irrigation calendar.
## india_north_code = c(
##     1492,1493,40781,1505,70081,70074,1489,70082
## )
## india_east_code = c(
##     15,1487,70073,70078,1504,1511,1500,1501,1502,1503,1507,1509
## )
## india_south_code = c(
##     1485,1494,1495,70080,1508
## )
## india_west_code = c(
##     1491,70079,1498,1506,1490,70077,70076,70075
## )

## rgn_map[state_map_2001 %in% india_north_code] = 1
## rgn_map[state_map_2001 %in% india_east_code]  = 2
## rgn_map[state_map_2001 %in% india_south_code] = 3
## rgn_map[state_map_2001 %in% india_west_code]  = 4

## writeRaster(
##     dist_map_1960,
##     "data/g2008_2_India_1960_rast.tif",
##     format="GTiff",
##     overwrite=TRUE
## )
## writeRaster(
##     dist_map_2001,
##     "data/g2008_2_India_2001_rast.tif",
##     format="GTiff",
##     overwrite=TRUE
## )
## writeRaster(
##     state_map_2001,
##     "data/g2008_1_India_2001_rast.tif",
##     format="GTiff",
##     overwrite=TRUE
## )
## writeRaster(
##     rgn_map,
##     "data/g2008_rgn_India_2001_rast.tif",
##     format="GTiff",
##     overwrite=TRUE
## )

## ## Create directory to store output
## if (!dir.exists("data/district_frac_2005")) {
##     dir.create("data/district_frac_2005")
## }

## if (!dir.exists("data/district_frac_1960")) {
##     dir.create("data/district_frac_1960")
## }

## ## Rasterize district polygons

## ## 2001
## district_coarse_first = rasterize(
##     adm2_2001_ll,
##     template_coarse_ll,
##     field="ADM2_CODE",
##     fun="first"
## )
## district_fine_first = rasterize(
##     adm2_2001_ll,
##     template_fine_ll,
##     field="ADM2_CODE",
##     fun="first"
## )
## writeRaster(
##     district_coarse_first,
##     "data/g2001_adm2_rast_first_5m.tif",
##     overwrite=TRUE
## )
## writeRaster(
##     district_fine_first,
##     "data/g2001_adm2_rast_first_30s.tif",
##     overwrite=TRUE
## )

## ## 1960
## district_coarse_first = rasterize(
##     adm2_1960_ll,
##     template_coarse_ll,
##     field="ADM2_CODE",
##     fun="first"
## )
## district_fine_first = rasterize(
##     adm2_1960_ll,
##     template_fine_ll,
##     field="ADM2_CODE",
##     fun="first"
## )
## writeRaster(
##     district_coarse_first,
##     "data/g1960_adm2_rast_first_5m.tif",
##     overwrite=TRUE
## )
## writeRaster(
##     district_fine_first,
##     "data/g1960_adm2_rast_first_30s.tif",
##     overwrite=TRUE
## )

## district_coarse_2001 = raster(
##     "data/g2001_adm2_rast_5m.tif"
## )
## district_fine_2001 = raster(
##     "data/g2001_adm2_rast_30s.tif"
## )
## district_coarse_1960 = raster(
##     "data/g1960_adm2_rast_5m.tif"
## )
## district_fine_1960 = raster(
##     "data/g1960_adm2_rast_30s.tif"
## )

## Neighbourhood - this is needed to work out if a grid cell
## borders the ocean or a neighbouring country
nb_coarse = rasterize(
    india_nb_ll,
    template_coarse_ll,
    field="ADM0_CODE"
)
nb_fine = rasterize(
    india_nb_ll,
    template_fine_ll,
    field="ADM0_CODE"
)
writeRaster(
    nb_coarse,
    "data/india_nb_coarse.tif",
    overwrite=TRUE
)
writeRaster(
    nb_fine,
    "data/india_nb_fine.tif",
    overwrite=TRUE
)

nb_coarse = raster(
    "data/india_nb_coarse.tif"
)
nb_fine = raster(
    "data/india_nb_fine.tif"
)

myfun = function(district_coarse, district_fine, nb_coarse, nb_fine, path, suffix, ...) {
    ## This function estimates the fraction of each coarse resolution
    ## grid cell belonging to the parent district.
    ## 
    ## Two variants are produced:
    ## * "*_frac1_*.tif" : coast cells set to 1, land border cells
    ##                     indicate fraction belonging to India
    ## * "*_frac2_*.tif" : all border cells (coast, land) set to 1
    ##
    ## The maps are written to file; the function returns nothing
    dists <- sort(unique(getValues(district_fine)))
    
    for (i in 1:length(dists)) {
        dist <- dists[i]
        print(dist)        
        tmp1 <- district_fine 
        tmp1[(!is.na(tmp1) & tmp1 != dist)] <- 0

        ## Zoom region to current district
        tmp2 = district_coarse
        xy = as.data.frame(xyFromCell(object=tmp1, cell=which(getValues(tmp1) %in% dist)))
        ext <- extent(min(xy$x), max(xy$x), min(xy$y), max(xy$y))
        tmp2 <- crop(tmp2, ext, snap="out")
        ext <- extent(extend(tmp2, y=2))

        tmp3 <- tmp1

        ## Crop fine resolution district map to current region
        tmp1[(is.na(tmp1) & (!is.na(nb_fine)))] <- 0
        tmp1[tmp1 == dist] <- 1
        tmp1 <- crop(tmp1, ext)
        tmp1 <- aggregate(tmp1, fact=10, fun=mean, na.rm=TRUE)
        tmp1[tmp1 == 0] <- NA

        fn1 = paste0("dist_", dist, "_frac1_", suffix, ".tif")
        tmp1 <- writeRaster(tmp1, file.path(path, fn1), format="GTiff", overwrite=TRUE)

        tmp3[tmp3 == dist] <- 1
        tmp3 <- crop(tmp3, ext)
        tmp3 <- aggregate(tmp3, fact=10, fun=mean, na.rm=TRUE)
        tmp3[tmp3 == 0] <- NA

        fn2 = paste0("dist_", dist, "_frac2_", suffix, ".tif")
        tmp3 <- writeRaster(tmp3, file.path(path, fn2), format="GTiff", overwrite=TRUE) 
    }
}

## Compute district fraction maps for 2005 and 1960
myfun(
    district_coarse_2001,
    district_fine_2001,
    nb_coarse,
    nb_fine,
    path="data/district_frac_2005",
    suffix="ll"
)

myfun(
    district_coarse_1960,
    district_fine_1960,
    nb_coarse,
    nb_fine,
    path="data/district_frac_1960",
    suffix="ll"
)
