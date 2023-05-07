## Author : Simon Moulds
## Date   : April 2021

library(raster)
library(sf)
library(tidyverse)
library(magrittr)
library(rgdal)
library(maptools)
library(rnaturalearth)

options(stringsAsFactors = FALSE)

if (!dir.exists("../data/output_maps")) {
    dir.create("../data/output_maps")
}

## =================================== ##
## Load spatial data
## =================================== ##

## 1 - Cropland maps
cropland_area_fn = file.path(
    "../data",
    "iiasa_ifpri_cropland_map_0.008333Deg.tif"
)
cropland_area_maps = list("2005"=raster(cropland_area_fn))

## 2 - Irrigated area maps [India only]
## TODO: take maximum value
irrigated_area_maps = list()
for (yr in 2000:2014) {
    irrigated_area_fn = file.path(
        "../data",
        paste0("irrigated_area_", yr, "_", yr+1, "_india_0.008333Deg.tif")
    )
    irrigated_area_maps[[paste0("x", yr)]] = raster(irrigated_area_fn)
}
## max_irrigated_area_map = stackApply(
##     stack(irrigated_area_maps),
##     indices=rep(1, length(irrigated_area_maps)),
##     fun=max
## )

## 3 - Winter cropland area maps [India only]
winter_cropland_area_maps = list()
for (yr in 2001:2016) {
    winter_cropland_area_fn = file.path(
        "../data",
        paste0("winter_cropped_area_", yr, "_india_0.008333Deg.tif")
    )
    winter_cropland_area_maps[[paste0("x", yr)]] = raster(winter_cropland_area_fn)
}

## NOT USED:
## canal_irrigated_area_fn = file.path(
##     "../data-raw",
##     "india_village_census_geotiffs", "igp_canal_govt_sum.tif"
## )
## canal_irrigated_area_maps = list(
##     "2000"=raster(canal_irrigated_area_fn)
## )

## 4 - ICRISAT district IDs
template = raster("../data/icrisat_poly_ids.tif")
land_frac = template
land_frac[!is.na(land_frac)] = 1
writeRaster(
    land_frac,
    "../data/output_maps/icrisat_india_land_frac.tif",
    overwrite=TRUE
)
            
template[!is.na(template)] = 0

## 5 - Canal command areas
cmd_areas_shp = st_read(
    "../data-raw/command_areas.shp"
) %>% as_Spatial
cmd_areas = rasterize(
    cmd_areas_shp, template, field="GRIDCODE"
)

## =================================== ##
## Define some helper functions
## =================================== ##

get_irri_area_map = function(dist_frac, yr) {
    ## Get irrigated area map for a specific district
    ## https://www.nature.com/articles/sdata2016118
    if (yr < 2000) {
        yr = 2000
    } else if (yr > 2014) {
        yr = 2014
    }        
    dist_irri_frac =
        irrigated_area_maps[[paste0("x", yr)]] %>%
        crop(extent(dist_frac)) %>%
        `*`(dist_frac)
    dist_irri_area = area(dist_frac) * dist_irri_frac
    dist_irri_area
}

get_winter_crop_area_map = function(dist_frac, yr) {
    ## Get winter cropped area map for a specific district
    if (yr < 2001) {
        yr = 2001
    } else if (yr > 2016) {
        yr = 2016
    }
    dist_winter_crop_frac =
        winter_cropland_area_maps[[paste0("x", yr)]] %>%
        crop(extent(dist_frac)) %>%
        `*`(dist_frac)
    dist_winter_crop_area = area(dist_frac) * dist_winter_crop_frac
    dist_winter_crop_area
}

get_crop_area_map = function(dist_frac) {
    ## Get cropland area map for a specific district
    dist_crop_frac =
        cropland_area_maps[["2005"]] %>%
        crop(extent(dist_frac)) %>%
        `*`(dist_frac)
    dist_crop_area = area(dist_frac) * dist_crop_frac
    dist_crop_area
}

## get_canal_irri_area_map = function(dist_frac) {
##     dist_canal_irri_area =
##         canal_irrigated_area_maps[["2000"]] %>%
##         crop(extent(dist_frac)) %>%
##         `*`(dist_frac)
##     ## dist_canal_irri_area
##     dist_cell_area = area(dist_frac) * dist_frac
##     dist_canal_irri_area =
##         dist_canal_irri_area %>%
##         `*`(dist_frac) %>%
##         `/`(dist_cell_area * 100)
##     dist_canal_irri_area
## }

get_canal_cmd_area_map = function(dist_frac, cropland_area) {
    ## Get canal cmd area for a specific district
    dist_canal_cmd_area =
        cmd_areas %>% 
        crop(extent(dist_frac))
    dist_canal_cmd_area = !is.na(dist_canal_cmd_area)
    dist_canal_cmd_area = cropland_area * dist_frac * dist_canal_cmd_area
    dist_canal_cmd_area    
}

get_icrisat_data = function(poly_id, icrisat_data, icrisat_data_2005) {
    ## Scale ICRISAT data with values from gridded products
    icrisat_data %<>% filter(POLY_ID %in% poly_id)
    icrisat_data_2005 %<>% filter(POLY_ID %in% poly_id)
    x = list(
        "canal_area" = icrisat_data$canals_area,
        "tanks_area" = icrisat_data$tanks_area,
        "tubewells_area" = icrisat_data$tubewells_area,
        "other_wells_area" = icrisat_data$other_wells_area,
        "total_wells_area" = icrisat_data$total_wells_area,
        "other_sources_area" = icrisat_data$other_sources_area,
        "net_irrigated_area" = icrisat_data$net_area,
        "gross_irrigated_area" = max(
            icrisat_data$gross_area,
            icrisat_data$net_area,
            na.rm=TRUE
        ),
        "net_cropped_area" = icrisat_data$nca,
        "gross_cropped_area" = max(
            icrisat_data$gca,
            icrisat_data$nca,
            na.rm=TRUE
        ),        
        "net_cropped_area_2005" = icrisat_data_2005$nca,
        "gross_cropped_area_2005" = icrisat_data_2005$gca
    )
    x = lapply(x, FUN=function(x) ifelse(is.na(x), 0, x))
    nia_calc = (
        x$canal_area
        + x$tanks_area
        + x$tubewells_area
        + x$other_wells_area
        + x$other_sources_area
    )    
    nia_actual = x$net_irrigated_area
    if (nia_calc > 0) {
        sf = nia_actual / nia_calc
        x$canal_area %<>% `*`(sf)
        x$tanks_area %<>% `*`(sf)
        x$tubewells_area %<>% `*`(sf)
        x$other_wells_area %<>% `*`(sf)
        x$other_sources_area %<>% `*`(sf)
    }
    x
}

get_total_cropland_area = function(dist_crop_area) {
    sum(getValues(dist_crop_area * area(dist_crop_area)), na.rm=TRUE)
}

## Assumptions:
## ############

## TODO: Output cropland area
## TODO: Output irrigated area

## India
icrisat_data_2005 = st_read(
    "irrigated_area_source.gpkg",
    layer="x2005"
)

ids =
    icrisat_data_2005 %>%
    as_tibble %>%
    filter(!is.na(state_code)) %>%
    filter(!is.na(gross_area)) %>%
    `$`(POLY_ID) %>%
    sort

## We ignore some IDs from our analysis
exclude_ids = c(217, 237, 238, 240, 248, 276, 309, 334, 340, 355)
ids = ids[!ids %in% exclude_ids]
src = "irrigated_area_source.gpkg"
dist_frac_suffix="district_frac"

## We need to write a shapefile containing only
## the districts we model
poly = st_read(src, layer=paste0("x", 2005))
poly %<>%
    filter(POLY_ID %in% ids) %>%
    dplyr::select(state_name, dist_name, state_code, dist_code, POLY_ID)
st_write(poly, "../data/icrisat_polygons.gpkg", delete_dsn=TRUE)

## First we loop through all the years for India only
## WARNING: this takes a long time!
yrs = 1979:2016
out_prefix = "india"
## yrs = 2005
## out_prefix = "india"
## yrs = 1979:2016
## out_prefix = "punjab"
## ids = poly$POLY_ID[poly$state_name %in% c("Punjab")]

for (yr in yrs) {
    icrisat_data = st_read(src, layer=paste0("x", yr))
    icrisat_data_2005 = st_read(src, layer="x2005")
    ## Allocate list to hold output
    irri_sources = c(
        "canal", "tubewells",
        "other_wells", "tanks",
        "other_sources"
    )
    gw_irri_sources = c("tubewells", "other_wells")
    sw_irri_sources = irri_sources[!irri_sources %in% gw_irri_sources]
    seasons = c("continuous", "kharif", "rabi", "zaid")
    output_map_nms = c(
        "net_cropland_area",
        ## "net_irrigated_area", "net_cropland_area",
        ## "gross_irrigated_area", "gross_cropland_area",
        apply(
            expand.grid(irri_sources, seasons)[,c(2,1)],
            1, paste, collapse="_"
        )
    )
    output_maps = lapply(
        output_map_nms,
        FUN=function(nm) setNames(template, nm)
    )
    names(output_maps) = output_map_nms

    ## j=234 [Sultanpur]
    ## j=277
    ## j=18 [Ludhiana]
    ## j=102 [Murshidabad]
    for (j in 1:length(ids)) {
        id = ids[j]

        ## Update progress
        print(
            paste0(
                "Progress for year ", yr, ": ", j, " / ", length(ids)
            )
        )

        ## ############################################### ##
        ## ############################################### ##
        ## Get spatial data for current district
        ## ############################################### ##
        ## ############################################### ##
        
        ## District fraction
        dist_frac_fn = file.path(
            "../data/dist_fraction",
            paste0(dist_frac_suffix, "_1km_", id, ".tif")
        )
        dist_frac = raster(dist_frac_fn)
        dist_frac_pts = as(dist_frac, "SpatialPoints")
        ## Irrigated area from Ambika et al. (2016)
        ## ***NOT CURRENTLY USED***
        dist_irri_area = get_irri_area_map(dist_frac, yr)
        ## Winter cropped area from SEDAC
        ## ***NOT CURRENTLY USED***
        dist_winter_crop_area = get_winter_crop_area_map(
            dist_frac, yr
        )
        ## Crop area from IIASA-IFPRI (2005)
        dist_crop_area = get_crop_area_map(dist_frac)
        total_cropland_area_2005 = get_total_cropland_area(
            dist_crop_area
        )
        ## Current net cropped area 
        cell_area_vals = area(dist_frac)[dist_frac_pts]
        crop_frac_vals = dist_crop_area[dist_frac_pts]
        ## TODO: decide whether we actually need to scale cropland area - one option from the point of view of JULES modelling would simply be to increase the rainfed area as irrigated area decreased
        crop_area_vals = (
            cell_area_vals
            * crop_frac_vals
            ## * (dist_icrisat_data$net_cropped_area
            ##     / dist_icrisat_data$net_cropped_area_2005)
        )
        ## TODO: make sure that seasonal values do not exceed this value:
        total_cropland_area = sum(crop_area_vals)
        
        ## TODO: not sure if these two data sources are useful:
        ## ## Canal irrigated area from village-level data
        ## dist_canal_irri_area = get_canal_irri_area_map(dist_frac)
        ## ## total winter cropland area from SEDAC data
        ## total_winter_cropland_area = get_total_cropland_area(
        ##     dist_winter_crop_area
        ## )

        ## Continuous/zaid crop area ***TODO***
        dist_zaid_crop_area = rep(0, length(dist_frac_pts))        
        dist_continuous_crop_area = rep(0, length(dist_frac_pts))
        zaid_crop_area = sum(dist_zaid_crop_area)
        continuous_crop_area = sum(dist_continuous_crop_area)        

        ## Canal command area data
        dist_canal_cmd_area = get_canal_cmd_area_map(
            dist_frac, dist_crop_area
        )

        ## District-level statistics from ICRISAT
        dist_icrisat_data = get_icrisat_data(
            id, icrisat_data, icrisat_data_2005
        )

        ## ############################################### ##
        ## ############################################### ##
        ## Scale net/gross crop/irrigated areas
        ## ############################################### ##
        ## ############################################### ##

        ## Here we attempt to match ICRISAT data with
        ## quantities computed from remote sensing

        ## Compute scale factors to relate inventory data to remote
        ## sensing data. We use 2005 because this is the reference
        ## year for the IIASA-IFPRI map
        net_cropland_scale_factor = (
            total_cropland_area_2005
            / dist_icrisat_data$net_cropped_area_2005
        )
        gross_cropland_scale_factor = (
          dist_icrisat_data$gross_cropped_area
          / dist_icrisat_data$net_cropped_area
        )
        ## Net irrigated scale factor is calculated from the ICRISAT
        ## data alone, which we assume is consistent in fractional
        ## terms (we correct absolute terms using the net cropland
        ## scale factor)

        ## Ratio of net irrigated to net cropland
        net_irrigated_scale_factor = (
            dist_icrisat_data$net_irrigated_area
            / dist_icrisat_data$net_cropped_area
        )
        ## Ratio of gross to net irrigated area
        gross_irrigated_scale_factor = (
            dist_icrisat_data$gross_irrigated_area
            / dist_icrisat_data$net_irrigated_area
        )
        ## Make sure scale factors are finite; otherwise set to zero
        check_finite = function(x) {
            if (!is.finite(x)) {
                x = 0
            }
            x
        }
        net_cropland_scale_factor %<>% check_finite
        gross_cropland_scale_factor %<>% check_finite
        net_irrigated_scale_factor %<>% check_finite
        gross_irrigated_scale_factor %<>% check_finite

        ## Apply scale factors
        net_cropland_area = (
            dist_icrisat_data$net_cropped_area * net_cropland_scale_factor
        )        
        gross_cropland_area = (
            net_cropland_area * gross_cropland_scale_factor
        )
        net_irrigated_area = (
            net_cropland_area * net_irrigated_scale_factor
        )        
        gross_irrigated_area = (
            net_irrigated_area * gross_irrigated_scale_factor
        )
        ## Multiply zaid/continuous area by net cropland scale factor
        zaid_crop_area = (
            zaid_crop_area
            * net_cropland_scale_factor
        )        
        continuous_crop_area = (
            continuous_crop_area
            * net_cropland_scale_factor
        )
        ## Zaid cannot be more than (gross - net)
        zaid_crop_area = min(
            zaid_crop_area,
            gross_cropland_area - net_cropland_area
        )
        ## Continuous cannot be more than net
        continuous_crop_area = min(
            continuous_crop_area,
            net_cropland_area
        )
        ## Ratio of continuous to net crop area
        continuous_crop_fraction = (
            continuous_crop_area
            / net_cropland_area
        ) %>% check_finite
        ## Ratio of zaid to multi-cropped area
        zaid_crop_fraction = (
            zaid_crop_area
            / (gross_cropland_area - net_cropland_area)
        ) %>% check_finite
        
        ## Also save net/gross cropland/irrigated as raster
        net_cropland_area_map = dist_frac * net_cropland_area
        net_irrigated_area_map = dist_frac * net_irrigated_area
        gross_cropland_area_map = dist_frac * gross_cropland_area
        gross_irrigated_area_map = dist_frac * gross_irrigated_area
        
        ## ############################################### ##
        ## ############################################### ##
        ## Scale irrigated areas by source
        ## ############################################### ##
        ## ############################################### ##

        ## NB in ICRISAT the source totals refer to net irrigated area

        net_canal_area = (
            dist_icrisat_data$canal_area
            * (net_irrigated_area
                / dist_icrisat_data$net_irrigated_area)
        )
        net_canal_area %<>% check_finite

        net_tanks_area = (
            dist_icrisat_data$tanks_area
            * (net_irrigated_area
                / dist_icrisat_data$net_irrigated_area)
        )
        net_tanks_area %<>% check_finite

        net_tubewells_area = (
            dist_icrisat_data$tubewells_area
            * (net_irrigated_area
                / dist_icrisat_data$net_irrigated_area)
        )
        net_tubewells_area %<>% check_finite

        net_other_wells_area = (
            dist_icrisat_data$other_wells_area
            * (net_irrigated_area
                / dist_icrisat_data$net_irrigated_area)
        )
        net_other_wells_area %<>% check_finite

        net_other_sources_area = (
            dist_icrisat_data$other_sources_area
            * (net_irrigated_area
                / dist_icrisat_data$net_irrigated_area)
        )
        net_other_sources_area %<>% check_finite
        net_sw_area = max(
            sum(
                c(net_canal_area, net_tanks_area, net_other_sources_area),
                na.rm=TRUE
            ),
            0
        )
        net_gw_area = max(
            sum(
                c(net_tubewells_area, net_other_wells_area),
                na.rm=TRUE
            ),
            0
        )
        ## NB `get_icrisat_data(...)` enforces gca >= nca
        ## and gia >= nia, so we do not need to handle this case

        ## ############################################### ##
        ## ############################################### ##
        ## Estimate cropland area by season
        ## ############################################### ##
        ## ############################################### ##
        
        ## Multi-cropped area is the cultivated area during
        ## the year, with each field counted the number of 
        ## seasons it is used. We assume that this area
        ## can be divided between Rabi and Zaid seasons
        multi_cropped_area = max(
            gross_cropland_area - net_cropland_area,
            0
        )        
        rabi_cropland_area = (
            multi_cropped_area * (1 - zaid_crop_fraction)
        )
        zaid_cropland_area = (
            multi_cropped_area * zaid_crop_fraction
        )
        ## Adjust for the case where rabi_cropland_area exceeds
        ## the available crop area, and add it to zaid
        rabi_cropland_area2 = min(
            rabi_cropland_area,
            net_cropland_area
        )
        zaid_cropland_area2 = min(
            zaid_cropland_area+(rabi_cropland_area-rabi_cropland_area2),
            net_cropland_area
        )
        rabi_cropland_area = rabi_cropland_area2
        zaid_cropland_area = zaid_cropland_area2
        
        kharif_cropland_area = (
            net_cropland_area * (1 - continuous_crop_fraction)
        )
        continuous_cropland_area = (
            net_cropland_area * continuous_crop_fraction
        )
        calc_gross_cropland_area = (
            rabi_cropland_area
            + zaid_cropland_area
            + kharif_cropland_area
            + continuous_cropland_area
        )
        if (!isTRUE(all.equal(calc_gross_cropland_area, gross_cropland_area))) {
            print(paste0("Calculated GCA: ", calc_gross_cropland_area))
            print(paste0("Expected GCA  : ", gross_cropland_area))
            stop()
        }

        ## ############################################### ##
        ## ############################################### ##
        ## Estimate seasonal irrigated area
        ## ############################################### ##
        ## ############################################### ##
        
        ## We could actually estimate the proportion of
        ## rainfed/irrigated zaid/continuous crops from
        ## the crop area dataset. For now, though, we assume
        ## that rabi, zaid and continuous crops are irrigated.
        rabi_irrigated_area = rabi_cropland_area
        zaid_irrigated_area = zaid_cropland_area
        continuous_irrigated_area = continuous_cropland_area

        ## ## Ensure that the total irrigation from these three seasons
        ## ## does not exceed the gross irrigated area
        ## total_irrigated_area = (
        ##     rabi_irrigated_area
        ##     + zaid_irrigated_area
        ##     + continuous_irrigated_area
        ## )
        ## if (total_irrigated_area > gross_irrigated_area) {
        ##     rabi_irrigated_area = (
        ##         gross_irrigated_area
        ##         * (rabi_irrigated_area
        ##             / total_irrigated_area)
        ##     )            
        ##     zaid_irrigated_area = (
        ##         gross_irrigated_area
        ##         * (zaid_irrigated_area
        ##             / total_irrigated_area)
        ##     )            
        ##     continuous_irrigated_area = (
        ##         gross_irrigated_area
        ##         * (continuous_irrigated_area
        ##             / total_irrigated_area)
        ##     )
        ## }                    

        ## FIRST, estimate kharif irrigated area as whatever is
        ## left after allocating rabi, zaid, and continuous, up
        ## to a maximum of the total kharif cropland area
        kharif_irrigated_area = min(
            max(
                gross_irrigated_area - rabi_irrigated_area - zaid_irrigated_area - continuous_irrigated_area,
                0
            ),
            kharif_cropland_area
        )        
        ## NEXT, we assume that kharif irrigated area is the maximum
        ## of rabi_irrigated_area and zaid_irrigated_area (in other
        ## words, we assume that if a farmer has access to irrigation
        ## during rabi/zaid, they will use it during the summer
        ## period if necessary).
        kharif_irrigated_area = min(
            max(
                rabi_irrigated_area,
                zaid_irrigated_area,
                kharif_irrigated_area
            ),
            kharif_cropland_area
        )        
        ## We then have to recompute gross irrigated area
        gross_irrigated_area = (
            rabi_irrigated_area
            + zaid_irrigated_area
            + kharif_irrigated_area
            + continuous_irrigated_area
        )

        ## ############################################### ##        
        ## ############################################### ##
        ## Identify the irrigation source by season
        ## ############################################### ##
        ## ############################################### ##

        ## Strategy is as follows:
        ## kharif -> rabi -> zaid -> continuous
        ## sw -> gw
        
        ## 0 - Preallocate lists to hold results
        irrigation_sw_area = setNames(
            vector(mode="list", length=length(seasons)),
            seasons
        )        
        irrigation_gw_area = irrigation_sw_area

        ## 1 - Kharif
        ## ==========        
        ## 1a - Preferentially allocate sw to Kharif season
        irrigation_sw_area[["kharif"]] = max(
            min(net_sw_area, kharif_irrigated_area),
            0
        )
        ## 1b - Any remaining irrigated area allocated to gw
        irrigation_gw_area[["kharif"]] = max(
            kharif_irrigated_area - irrigation_sw_area[["kharif"]],
            0
        )

        ## 2 - Rabi
        ## ========        
        ## 2a - Any remaining sw is allocated to rabi crops
        ##      up to the entire rabi irrigated area
        irrigation_sw_area[["rabi"]] = max(
            min(
                rabi_irrigated_area,
                net_sw_area - irrigation_sw_area[["kharif"]]
            ),
            0
        )
        ## 2b - Any remaining irrigated area allocated to gw
        irrigation_gw_area[["rabi"]] = max(
            rabi_irrigated_area - irrigation_sw_area[["rabi"]],
            0
        )

        ## 3 - Zaid
        ## ========
        ## 3a - Any remaining sw (after allocating to kharif
        ##      and rabi) is allocated to zaid (almost certainly
        ##      zero)
        irrigation_sw_area[["zaid"]] = max(
            min(
                zaid_irrigated_area,
                net_sw_area - irrigation_sw_area[["kharif"]] - irrigation_sw_area[["rabi"]]
            ),
            0
        )
        ## 3b - Any remaining irrigated area allocated to gw
        irrigation_gw_area[["zaid"]] = max(
            zaid_irrigated_area - irrigation_sw_area[["zaid"]],
            0
        )

        ## 4 - Continuous
        ## ==============
        ## 4a - Assume all continuous irrigation comes from gw
        ##      (TODO: in reality it may come from different
        ##       sources depending on the time of year - is there
        ##       a way to represent this?)
        irrigation_sw_area[["continuous"]] = 0.
        irrigation_gw_area[["continuous"]] = continuous_irrigated_area

        ## Do a check:
        calc_gross_irrigated_area = (
            sum(as.numeric(irrigation_sw_area))
            + sum(as.numeric(irrigation_gw_area))
        )        
        if (!isTRUE(calc_gross_irrigated_area <= gross_irrigated_area + .Machine$double.eps**0.5)) {
            print(paste0("Calculated GIA: ", calc_gross_irrigated_area))
            print(paste0("Expected GIA  : ", gross_irrigated_area))
            stop()
        }        

        ## ############################################### ##
        ## ############################################### ##
        ## Divide sw/gw into the respective sources
        ## ############################################### ##
        ## ############################################### ##
        
        irrigation_source_frac = setNames(
            vector(mode="list", length=5),
            irri_sources
        )        
        irrigation_source_frac[["canal"]] =
            (net_canal_area / net_sw_area) %>%
            check_finite
        irrigation_source_frac[["tanks"]] =
            (net_tanks_area / net_sw_area) %>%
            check_finite
        irrigation_source_frac[["other_sources"]] =
            (1 - (irrigation_source_frac[["canal"]] + irrigation_source_frac[["tanks"]])) %>%
            check_finite
        irrigation_source_frac[["tubewells"]] =
            (net_tubewells_area / net_gw_area) %>%
            check_finite
        irrigation_source_frac[["other_wells"]] =
            (net_other_wells_area / net_gw_area) %>%
            check_finite

        irrigation_source_area = lapply(1:20, FUN=function(x) 0)
        names(irrigation_source_area) = apply(
            expand.grid(irri_sources, seasons)[,c(2,1)], 1, paste, collapse="_"
        )
        for (season in seasons) {
            gw_area = irrigation_gw_area[[season]]
            sw_area = irrigation_sw_area[[season]]
            ## This makes a correction where the specified
            ## gw/sw area > 0, but no gw/sw irrigation source is
            ## specified (we assume irrigated area has been
            ## wrongly allocated to gw/sw)
            if (sum(as.numeric(irrigation_source_frac)) == 0) {
                irrigation_source_frac[["tubewells"]] = 0.5
                irrigation_source_frac[["canal"]] = 0.5
                warning()
            } else if (sum(as.numeric(irrigation_source_frac[gw_irri_sources])) == 0) {
                sw_area = sw_area + gw_area
                gw_area = 0
            } else if (sum(as.numeric(irrigation_source_frac[sw_irri_sources])) == 0) {
                gw_area = gw_area + sw_area
                sw_area = 0
            }            
            for (source in irri_sources) {
                key = paste0(season, "_", source)
                if (source %in% gw_irri_sources) { 
                    ar = gw_area * irrigation_source_frac[[source]]
                } else {
                    ar = sw_area * irrigation_source_frac[[source]]
                }
                irrigation_source_area[[key]] = ar
            }            
        }
        ## Do a check:
        calc_gross_irrigated_area2 = sum(as.numeric(irrigation_source_area))
        if (!isTRUE(all.equal(calc_gross_irrigated_area2, calc_gross_irrigated_area, tolerance=.Machine$double.eps**0.5))) {
            print(paste0("Calculated GIA: ", calc_gross_irrigated_area2))
            print(paste0("Expected GIA  : ", calc_gross_irrigated_area))
            stop()
        }
        
        ## ############################################### ##
        ## ############################################### ##
        ## Spatial allocation
        ## ############################################### ##
        ## ############################################### ##

        ## allocate canal irrigated area first, and update allocated area
        get_total_available_area = function(suit_vals, allocated_area_vals, cell_area_vals) {
            ## Function to estimate the total area
            ## available in the district
            ix = !is.na(suit_vals) & (suit_vals > 0)
            total_area = sum(
                cell_area_vals[ix]
                - allocated_area_vals[ix],
                na.rm=TRUE
            ) 
            total_area
        }
        
        allocate_fun = function(total_area, suit_vals, allocated_area_vals, cell_area_vals) {
            ## Function to allocate an aggregated value to pixels
            total_available_area = get_total_available_area(
                suit_vals,
                allocated_area_vals,
                cell_area_vals
            )
            if (total_available_area < total_area) {
                total_area = total_available_area
            }            
            area = total_area * (suit_vals / sum(suit_vals, na.rm=TRUE))
            ## ********************************************************
            ## TODO: this does not ensure that cell values do not exceed
            ## the total area of the cell
            ## ********************************************************
            area = pmin(area, cell_area_vals) # TEMP FIX
            area
            ## TODO:
            ## frac = pmin(area / cell_area_vals, 1)            
            ## frac
        }
        
        check_suit_vals = function(x, y, reqd_area) {
            xtot = sum(x, na.rm=TRUE)
            if ((xtot > 0.) & (xtot > reqd_area)) {
                suit_vals = x
            } else {
                suit_vals = y
            }
            suit_vals
        }

        ## Put suitability values into a list
        canal_cmd_area_vals = dist_canal_cmd_area[dist_frac_pts]
        winter_crop_area_vals = dist_winter_crop_area[dist_frac_pts]
        suit_vals_list = setNames(
            vector(mode="list", length=length(irri_sources)),
            irri_sources
        )        
        suit_vals_list[["canal"]] = canal_cmd_area_vals
        suit_vals_list[["tubewells"]] = crop_frac_vals
        suit_vals_list[["other_wells"]] = crop_frac_vals
        suit_vals_list[["tanks"]] = crop_frac_vals
        suit_vals_list[["other_sources"]] = crop_frac_vals

        ## Preallocate a list to store results
        allocated_area_vals_list = lapply(1:20, FUN=function(x) 0)
        names(allocated_area_vals_list) = names(irrigation_source_area)
        zeros = rep(0, length(cell_area_vals))
        total_continuous_allocated_area_vals = zeros        
        for (season in seasons) {
            total_allocated_area_vals = total_continuous_allocated_area_vals
            for (source in irri_sources) {
                key = paste0(season, "_", source)
                area = irrigation_source_area[[key]]
                vals = zeros
                if (area > 0) {
                    if (source == "canal") {
                        suit_vals = check_suit_vals(
                            suit_vals_list[["canal"]],
                            crop_frac_vals,
                            area
                        )
                    } else {
                        suit_vals = suit_vals_list[[source]]
                    }                    
                    vals = allocate_fun(
                        area,
                        suit_vals,
                        total_allocated_area_vals,
                        cell_area_vals
                    )
                    ## This forces values to be less than or equal to the
                    ## available cropland area
                    vals = pmin(vals, crop_area_vals)
                }                
                total_allocated_area_vals = rowSums(
                    matrix(c(total_allocated_area_vals, vals), ncol=2)
                )
                allocated_area_vals_list[[key]] = vals
            }
            if (season == "continuous") {
                total_continuous_allocated_area_vals = total_allocated_area_vals
            }
        }

        ## We make a correction to ensure that in each cell the
        ## kharif irrigated area is the maximum of the allocated
        ## kharif irrigated area, and the allocated rabi/zaid
        ## irrigated area. 
        continuous_df = as.data.frame(allocated_area_vals_list[1:5])
        kharif_df = as.data.frame(allocated_area_vals_list[6:10])
        rabi_df = as.data.frame(allocated_area_vals_list[11:15])
        zaid_df = as.data.frame(allocated_area_vals_list[16:20])
        target_kharif_total = pmax(
            rowSums(kharif_df),
            rowSums(rabi_df),
            rowSums(zaid_df)
        )
        kharif_extra = target_kharif_total - rowSums(kharif_df)
        tw_irr = irrigation_source_frac$tubewells
        ow_irr = irrigation_source_frac$other_wells
        gw_irr = tw_irr + ow_irr
        if (gw_irr > 0) {
            tw_frac = tw_irr / gw_irr
            ow_frac = ow_irr / gw_irr
        } else {
            tw_frac = 1
            ow_frac = 0
        }
        kharif_df$kharif_tubewells = (
            kharif_df$kharif_tubewells + kharif_extra * tw_frac
        )
        kharif_df$kharif_other_wells = (
            kharif_df$kharif_other_wells + kharif_extra * ow_frac
        )
        allocated_area_vals_list = cbind(
            kharif_df,
            rabi_df,
            zaid_df,
            continuous_df
        ) %>% as.list
        
        ## ############################################### ##
        ## ############################################### ##
        ## Assign to raster images
        ## ############################################### ##
        ## ############################################### ##

        ## crop_area_vals = crop_area_vals * net_cropland_scale_factor
        output_maps[["net_cropland_area"]][dist_frac_pts] = crop_area_vals
        
        ## First allocate to district maps
        myfun = function(template, vals) {
            r = raster(template)
            r[dist_frac_pts] = vals
            r
        }
        for (season in seasons) {
            for (source in irri_sources) {
                key = paste0(season, "_", source)
                output_maps[[key]][dist_frac_pts] =
                    allocated_area_vals_list[[key]]
            }
        }
    }
    for (i in 1:length(output_maps)) {
        key = names(output_maps)[i]
        fn = file.path(
            "../data/output_maps",
            paste0(key, "_", out_prefix, "_", yr, ".tif")
        )
        writeRaster(
            output_maps[[key]], fn,
            datatype="FLT4S", overwrite=TRUE
        )
    }
}
    
##     print(yr)
##     irri_allocate_fun(
##         src,
##         ids,
##         template,
##         yr,
##         dist_frac_suffix,
##         "india"
##     )
## }

## ## TESTING:
## yr = 2010
## irri_allocate_fun(src, ids, template, yr, dist_frac_suffix, "india")

## ## Sanity checks: make some plots
## r = raster("../data/summer_canal_area_india_2010.tif")
## r = raster("../data/summer_tanks_area_india_2010.tif")
## r = raster("../data/summer_tubewells_area_india_2010.tif")
## r = raster("../data/summer_other_wells_area_india_2010.tif")
## r = raster("../data/winter_canal_area_india_2010.tif")
## r = raster("../data/winter_tanks_area_india_2010.tif")
## r = raster("../data/winter_tubewells_area_india_2010.tif")
## r = raster("../data/winter_other_wells_area_india_2010.tif")

## ## Indus
## indus_data_2005 = st_read(
##     "indus_irrigated_area_source.gpkg",
##     layer="x2005"
## )

## ids = indus_data_2005 %>% as_tibble %>% `$`(POLY_ID) %>% sort
## yr = 2010
## src = "indus_irrigated_area_source.gpkg"
## dist_frac_suffix="command_area_frac"

## irri_allocate_fun(src, ids, template, yr, dist_frac_suffix, "indus")

## ## Combine India and Indus maps
## combine_maps_fun = function(fn1, fn2, mask) {
##     xx = stackApply(stack(c(fn1, fn2)), indices=c(1,1), fun=sum)
##     xx
## }

## summer_canal = combine_maps_fun(
##     "../data/summer_canal_area_india_2010.tif",
##     "../data/summer_canal_area_indus_2010.tif"
## )

## writeRaster(
##     summer_canal,
##     "../data/summer_canal_area_2010.tif",
##     datatype="FLT4S",
##     overwrite=TRUE
## )

## summer_other_sources = combine_maps_fun(
##     "../data/summer_other_sources_area_india_2010.tif",
##     "../data/summer_other_sources_area_indus_2010.tif"
## )

## writeRaster(
##     summer_other_sources,
##     "../data/summer_other_sources_area_2010.tif",
##     datatype="FLT4S",
##     overwrite=TRUE
## )

## summer_other_wells = combine_maps_fun(
##     "../data/summer_other_wells_area_india_2010.tif",
##     "../data/summer_other_wells_area_indus_2010.tif"
## )

## writeRaster(
##     summer_other_wells,
##     "../data/summer_other_wells_area_2010.tif",
##     datatype="FLT4S",
##     overwrite=TRUE
## )

## summer_tanks = combine_maps_fun(
##     "../data/summer_tanks_area_india_2010.tif",
##     "../data/summer_tanks_area_indus_2010.tif"
## )

## writeRaster(
##     summer_tanks,
##     "../data/summer_tanks_area_2010.tif",
##     datatype="FLT4S",
##     overwrite=TRUE
## )

## summer_tubewells = combine_maps_fun(
##     "../data/summer_tubewells_area_india_2010.tif",
##     "../data/summer_tubewells_area_indus_2010.tif"
## )

## writeRaster(
##     summer_tubewells,
##     "../data/summer_tubewells_area_2010.tif",
##     datatype="FLT4S",
##     overwrite=TRUE
## )

## summer_tanks = combine_maps_fun(
##     "../data/summer_tanks_area_india_2010.tif",
##     "../data/summer_tanks_area_indus_2010.tif"
## )

## writeRaster(
##     summer_tanks,
##     "../data/summer_tanks_area_2010.tif",
##     datatype="FLT4S",
##     overwrite=TRUE
## )

## winter_other_wells = combine_maps_fun(
##     "../data/winter_other_wells_area_india_2010.tif",
##     "../data/winter_other_wells_area_indus_2010.tif"
## )

## writeRaster(
##     winter_other_wells,
##     "../data/winter_other_wells_area_2010.tif",
##     datatype="FLT4S",
##     overwrite=TRUE
## )

## winter_tubewells = combine_maps_fun(
##     "../data/winter_tubewells_area_india_2010.tif",
##     "../data/winter_tubewells_area_indus_2010.tif"
## )

## writeRaster(
##     winter_tubewells,
##     "../data/winter_tubewells_area_2010.tif",
##     datatype="FLT4S",
##     overwrite=TRUE
## )



## NOT USED:

## giam = raster("../data/giam_500_m_30classes_south_asia_tif_resamp.tif")

## ## GIAM classes:
## single_crop_classes = c(1, 3, 7, 10, 11, 13, 15, 18, 19, 21, 25, 26, 28, 30)
## double_crop_classes = c(2, 4, 8, 12, 14, 16, 22, 23, 27, 29)
## continuous_crop_classes = c(5, 6, 9, 17, 20, 24)

## ## 1 | Irrigated, surface water, rice, single crop
## ## 2 | Irrigated, surface water, rice, double crop
## ## 3 | Irrigated, surface water, rice-other crops, single crop 
## ## 4 | Irrigated, surface water, rice-other crops, double crop 
## ## 5 | Irrigated, surface water, rice-other crops, continious crop
## ## 6 | Irrigated, conjunctive use, mixed forest, rice-other crops, plantations, continous crop
## ## 7 | Irrigated, surface water, wheat-other crops, single crop
## ## 8 | Irrigated, surface water, wheat-other crops, double crop
## ## 9 | Irrigated, surface water, wheat-other crops, continious crop
## ## 10| Irrigated, surface water, sugarcane-other crops, single crop
## ## 11| Irrigated, surface water, mixed crop, single crop
## ## 12| Irrigated, surface water, mixed crops, double crop
## ## 13| Irrigated, ground water, rice-othercrops, single crop
## ## 14| Irrigated, ground water, rice-othercrops, double crop
## ## 15| Irrigated, ground water, cotton-other crops, single crop
## ## 16| Irrigated, ground water, cotton, wheat-other crops, double crop
## ## 17| Irrigated, ground water, cotton, soyabean-other crops, continious crop
## ## 18| Irrigated, ground water, sugarcane-other crops, single crop
## ## 19| Irrigated, ground water,mixed crops, single crop
## ## 20| Irrigated, ground water,plantations-other crops,continious crop
## ## 21| Irrigated, conjunctive use, rice-other crops, single crop
## ## 22| Irrigated, conjunctive use, rice, wheat-other crops, double crop
## ## 23| Irrigated, conjunctive use, wheat, rice-other crops, double crop
## ## 24| Irrigated, conjuctive use, rice, sugarcane-other crops, continuous crop
## ## 25| Irrigated, conjunctive use, wheat-other crops, single crop
## ## 26| Irrigated, conjuctive use, cotton-other crop, single crop
## ## 27| Irrigated, conjunctive use, cotton, wheat-other crops, double crop
## ## 28| Irrigated, conjunctive use, sugarcane-other crops, single crop
## ## 29| Irrigated, conjunctive use, soyabean, wheat-other crops, double crop
## ## 30| Irrigated, conjunctive use, mixed crops, single crop

## dist_giam = giam %>% crop(extent(dist_frac)) %>% `*`(dist_frac)
