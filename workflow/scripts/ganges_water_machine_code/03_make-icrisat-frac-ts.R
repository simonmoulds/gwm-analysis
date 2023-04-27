## Author : Simon Moulds Date   : Sep 2021
## The purpose of this script is twofold:
## (i)  To standardise crop fractions with data from ICRISAT
## (ii) To make a land cover time series

library(raster)
library(ncdf4)
library(sf)
library(stars)
library(magrittr)

## Years for which a map is available
icrisat_yrs = 1979:2016
## icrisat_yrs = 2005

## Input/output directories
icrisat_dir = "/home/sm510/projects/icrisat/data/output_maps"

gwm_dir = "/home/sm510/projects/ganges-water-machine/"
jules_dir = file.path(gwm_dir, "data/wfdei/ancils")
output_dir = file.path(gwm_dir, "data/irrigated_area_maps")
if (!dir.exists(output_dir)) {
    dir.create(output_dir)
}

## WFDEI land frac
wfdei_land_frac = raster(
    file.path(gwm_dir, "data/WFD-EI-LandFraction2d_IGP.tif")
)

## Pixels belonging to continental India
land_frac = raster(
    file.path(icrisat_dir, "icrisat_india_land_frac.tif")
)

## Create map showing the fraction of each coarse pixel
## which is covered by the ICRISAT data
icrisat_polys = st_read("../../icrisat/data/icrisat_polygons.gpkg")
template = raster(land_frac)
template[] = NA
if (!file.exists("../data/icrisat_polys.tif")) {
    poly_ids = icrisat_polys$POLY_ID
    for (id in poly_ids) {
        fn = paste0("district_frac_1km_", id, ".tif")
        dist_frac = raster(
            file.path("../../icrisat/data/dist_fraction", fn)
        )
        dist_frac_pts = as(dist_frac, "SpatialPoints")
        template[dist_frac_pts] = 1
    }
    writeRaster(template, "../data/icrisat_polys.tif", overwrite=TRUE)
} else {
    template = raster("../data/icrisat_polys.tif")
}

sf_use_s2(FALSE)
neighb_polys = st_read(
    "../data/g2015_2010_0/g2015_2010_0.shp") %>%
    st_crop(c(xmin=60, ymin=0, xmax=100, ymax=40))
stars_template = st_as_stars(template)
neighb_polys$to_rast = rep(0, nrow(neighb_polys))
neighb_rast = st_rasterize(neighb_polys[,"to_rast"], stars_template)
neighb_rast[!is.na(neighb_rast)] = 0
neighb_rast = as(neighb_rast, "Raster")
india_frac = stackApply(
    stack(neighb_rast, template),
    indices=c(1,1),
    fun=sum
)
india_frac[is.na(neighb_rast)] = NA
india_frac = aggregate(india_frac, fact=60, fun=mean)
india_frac = india_frac %>% crop(extent(wfdei_land_frac))
india_frac[is.na(india_frac)] = 0
india_frac[wfdei_land_frac == 0] = NA
writeRaster(
    india_frac,
    file.path(output_dir, "icrisat_india_frac.tif"),
    overwrite=TRUE
)

## template_aggr = aggregate(template, fact=60, fun=sum)
land_area = area(land_frac) * template
## land_area = area(land_frac) * land_frac
## india_frac = raster(
##     file.path(gwm_dir, "data/india_frac_0.500000Deg.tif")
## )

irri_sources = c(
    "canal",
    "tubewells",
    "other_wells",
    "tanks",
    "other_sources"
)
irri_seasons = c(
    "continuous",
    "kharif",
    "rabi",
    "zaid"
)    

get_icrisat_fn = function(season, source, rgn="india", year=2005) {
    valid_sources = c(irri_sources, "")
    valid_seasons = c(irri_seasons, "net_cropland_area")    
    if (!source %in% valid_sources) stop()
    if (!season %in% valid_seasons) stop()
    if (source == "") {
        fn = paste0(season, "_", rgn, "_", year, ".tif")
    } else {
        fn = paste0(season, "_", source, "_", rgn, "_", year, ".tif")
    }    
    return(file.path(icrisat_dir, fn))
}

## load_season_total_irrigation = function(season, rgn, year) {
##     fnames = c(
##         get_icrisat_fn(season, "canal", rgn, year),
##         get_icrisat_fn(season, "tubewells", rgn, year),
##         get_icrisat_fn(season, "other_wells", rgn, year),
##         get_icrisat_fn(season, "tanks", rgn, year),
##         get_icrisat_fn(season, "other_sources", rgn, year)
##     )
##     st = raster::stack(fnames)
##     sm = stackApply(st, indices=rep(1, 5), sum)
##     sm
## }

load_season_irrigation = function(season, rgn, year) {
    maps = list()
    for (source in irri_sources) {
        maps[[source]] = raster(get_icrisat_fn(season, source, rgn, year))
    }
    maps
}

rgn="india"
for (i in 1:length(icrisat_yrs)) {
    yr = icrisat_yrs[i]
    irr_maps = list(
        continuous = load_season_irrigation("continuous", rgn, yr),
        kharif = load_season_irrigation("kharif", rgn, yr),
        rabi = load_season_irrigation("rabi", rgn, yr),
        zaid = load_season_irrigation("zaid", rgn, yr)
    )    
    ## These initial calculations are performed at ~1km resolution    
    irr_continuous = stackApply(
        stack(irr_maps[["continuous"]]),
        indices=rep(1, length(irri_sources)),
        sum
    )
    irr_kharif = stackApply(
        stack(irr_maps[["kharif"]]),
        indices=rep(1, length(irri_sources)),
        sum
    )
    irr_rabi = stackApply(
        stack(irr_maps[["rabi"]]),
        indices=rep(1, length(irri_sources)),
        sum
    )
    irr_zaid = stackApply(
        stack(irr_maps[["zaid"]]),
        indices=rep(1, length(irri_sources)),
        sum
    )
              
    ## TODO: in the allocation routine, check these against
    ## (adjusted) district total net/gros irr area
    net_irr_area = stackApply(
        stack(
            irr_continuous + irr_kharif,
            irr_continuous + irr_rabi,
            irr_continuous + irr_zaid
        ),
        indices=rep(1, 3),
        fun=max
    )
    gross_irr_area = stackApply(
        stack(
            irr_continuous,
            irr_kharif,
            irr_rabi,
            irr_zaid
        ),
        indices=rep(1, 4),
        fun=sum
    )
    
    net_crop_area = raster(
        get_icrisat_fn("net_cropland_area", "", rgn, yr)
    )
    
    ## Rainfed area: assume this is the difference between
    ## net_crop_area and net_irr_area
    rainfed = net_crop_area - net_irr_area
    rainfed[rainfed < 0] = 0 # TEMPORARY FIX

    ## Assume that triple irrigated regions correspond to
    ## the season with the smallest irrigated area
    irr_triple = stackApply(
        stack(
            irr_kharif,
            irr_rabi,
            irr_zaid
        ),
        indices=rep(1, 3),
        fun=min
    )
    kharif_rabi = stackApply(
        stack(
            irr_kharif - irr_triple,
            irr_rabi - irr_triple
        ),
        indices=rep(1, 2),
        fun=min
    )    
    kharif_zaid = stackApply(
        stack(
            irr_kharif - irr_triple,
            irr_zaid - irr_triple
        ),
        indices=rep(1, 2),
        fun=min
    )    
    rabi_zaid = stackApply(
        stack(
            irr_rabi - irr_triple,
            irr_zaid - irr_triple
        ),
        indices=rep(1, 2),
        fun=min
    )    
    irr_double = stackApply(
        stack(kharif_rabi, kharif_zaid, rabi_zaid),
        indices=rep(1, 3),
        fun=max
    )
    irr_continuous = irr_continuous
    irr_single = (
        net_irr_area
        - irr_double
        - irr_triple - irr_continuous
    )

    ## We now have six raster images, with areas in km2:
    ##   (i) rainfed
    ##  (ii) irrigated_single
    ## (iii) irrigated_double
    ##  (iv) irrigated_triple
    ##   (v) irrigated_continuous    
    ## Convert the maps to cell fractions:
    area_aggr = aggregate(
        land_area, fact=60, fun=sum, na.rm=TRUE
    )
    rainfed_aggr = aggregate(
        rainfed, fact=60, fun=sum, na.rm=TRUE
    )
    irrigated_single_aggr = aggregate(
        irr_single, fact=60, fun=sum, na.rm=TRUE
    )
    irrigated_double_aggr = aggregate(
        irr_double, fact=60, fun=sum, na.rm=TRUE
    )
    irrigated_triple_aggr = aggregate(
        irr_triple, fact=60, fun=sum, na.rm=TRUE
    )
    irrigated_continuous_aggr = aggregate(
        irr_continuous, fact=60, fun=sum, na.rm=TRUE
    )    
    ## These values represent the various irrigation
    ## fractions of Indian territory **included in the
    ## ICRISAT dataset**
    irr_fraction_maps = list(
        rainfed_frac = rainfed_aggr,
        irrigated_single_frac = irrigated_single_aggr,
        irrigated_double_frac = irrigated_double_aggr,
        irrigated_triple_frac = irrigated_triple_aggr,
        irrigated_continuous_frac = irrigated_continuous_aggr
    )
    for (j in 1:length(irr_fraction_maps)) {
        nm = names(irr_fraction_maps)[j]
        fn = paste0(
            "icrisat_",
            nm, "_",
            yr, "_", rgn, "_0.500000Deg.tif"
        )
        print(paste0("Writing map ", fn, "..."))
        map = irr_fraction_maps[[nm]] / area_aggr
        map = map %>% crop(extent(wfdei_land_frac))
        map[is.na(map)] = 0
        map[wfdei_land_frac == 0] = NA
        writeRaster(map, file.path(output_dir, fn), overwrite=TRUE)
    }
    for (j in 1:length(irri_seasons)) {
        season = irri_seasons[j]
        for (k in 1:length(irri_sources)) {
            src = irri_sources[k]
            fn = paste0(
                "icrisat_",
                season, "_",
                src, "_",
                yr, "_", rgn, "_0.500000Deg.tif"
            )
            map = irr_maps[[j]][[k]]
            map_aggr = aggregate(map, fact=60, fun=sum, na.rm=TRUE)
            map_aggr = map_aggr / area_aggr
            map_aggr = map_aggr %>% crop(extent(wfdei_land_frac))
            map_aggr[is.na(map_aggr)] = 0
            map_aggr[wfdei_land_frac == 0] = NA
            print(paste0("Writing map ", fn, "..."))
            writeRaster(map_aggr, file.path(output_dir, fn), overwrite=TRUE)
        }
    }
}

stop()
## Below this point is some code to evaluate the output

sw_total = rep(0, length(icrisat_yrs))
gw_total = rep(0, length(icrisat_yrs))
area = area_aggr %>% crop(extent(wfdei_land_frac))
rgn = "india"
for (i in 1:length(icrisat_yrs)) {
    yr = icrisat_yrs[i]
    for (src in irri_sources) {
        ## for (season in irri_seasons) {
        for (season in "kharif") {
            fn = file.path(
                output_dir,
                paste0("icrisat_", season, "_", src, "_", yr, "_", rgn, "_0.500000Deg.tif")
            )
            r = raster(fn)
            ar = getValues(r * area) %>% sum(na.rm=T)
            if (src %in% c("tubewells", "other_wells")) {
                gw_total[i] = gw_total[i] + ar
            } else {
                sw_total[i] = sw_total[i] + ar
            }            
        }
    }
}

## Has gw_total and sw_total changed over time?
## Yes - this aspect of the code is working fine

## Continuous maps should currently be zero
r = raster("../data/irrigated_area_maps/icrisat_continuous_canal_2005_india_0.500000Deg.tif") %>% plot
r = raster("../data/irrigated_area_maps/icrisat_continuous_other_sources_2005_india_0.500000Deg.tif") %>% plot
r = raster("../data/irrigated_area_maps/icrisat_continuous_other_wells_2005_india_0.500000Deg.tif") %>% plot
r = raster("../data/irrigated_area_maps/icrisat_continuous_tanks_2005_india_0.500000Deg.tif") %>% plot
r = raster("../data/irrigated_area_maps/icrisat_continuous_tubewells_2005_india_0.500000Deg.tif") %>% plot

## These are the maps which are supplied directly to JULES:
r1 = raster("../data/irrigated_area_maps/icrisat_rainfed_frac_2005_india_0.500000Deg.tif") %>% plot # TODO: some of these values are less than zero - precision error?
r2 = raster("../data/irrigated_area_maps/icrisat_irrigated_double_frac_2005_india_0.500000Deg.tif") %>% plot
r3 = raster("../data/irrigated_area_maps/icrisat_irrigated_single_frac_2005_india_0.500000Deg.tif") %>% plot
r4 = raster("../data/irrigated_area_maps/icrisat_irrigated_triple_frac_2005_india_0.500000Deg.tif") %>% plot
r5 = raster("../data/irrigated_area_maps/icrisat_irrigated_continuous_frac_2005_india_0.500000Deg.tif") %>% plot

st = stack(r1,r2,r3,r4,r5) %>% stackApply(indices=rep(1,5), fun=sum)

## Kharif
r1 = raster("../data/irrigated_area_maps/icrisat_kharif_canal_2005_india_0.500000Deg.tif")
r2 = raster("../data/irrigated_area_maps/icrisat_kharif_other_sources_2005_india_0.500000Deg.tif")
r3 = raster("../data/irrigated_area_maps/icrisat_kharif_other_wells_2005_india_0.500000Deg.tif")
r4 = raster("../data/irrigated_area_maps/icrisat_kharif_tanks_2005_india_0.500000Deg.tif")
r5 = raster("../data/irrigated_area_maps/icrisat_kharif_tubewells_2005_india_0.500000Deg.tif")
kharif_tot = stackApply(stack(r1,r2,r3,r4,r5), indices=rep(1,5), sum)

## Rabi
r1 = raster("../data/irrigated_area_maps/icrisat_rabi_canal_2005_india_0.500000Deg.tif")
r2 = raster("../data/irrigated_area_maps/icrisat_rabi_other_sources_2005_india_0.500000Deg.tif")
r3 = raster("../data/irrigated_area_maps/icrisat_rabi_other_wells_2005_india_0.500000Deg.tif")
r4 = raster("../data/irrigated_area_maps/icrisat_rabi_tanks_2005_india_0.500000Deg.tif")
r5 = raster("../data/irrigated_area_maps/icrisat_rabi_tubewells_2005_india_0.500000Deg.tif")
rabi_tot = stackApply(stack(r1,r2,r3,r4,r5), indices=rep(1,5), sum)

## Zaid
r1 = raster("../data/irrigated_area_maps/icrisat_zaid_canal_2005_india_0.500000Deg.tif")
r2 = raster("../data/irrigated_area_maps/icrisat_zaid_other_sources_2005_india_0.500000Deg.tif")
r3 = raster("../data/irrigated_area_maps/icrisat_zaid_other_wells_2005_india_0.500000Deg.tif")
r4 = raster("../data/irrigated_area_maps/icrisat_zaid_tanks_2005_india_0.500000Deg.tif")
r5 = raster("../data/irrigated_area_maps/icrisat_zaid_tubewells_2005_india_0.500000Deg.tif")
zaid_tot = stackApply(stack(r1,r2,r3,r4,r5), indices=rep(1,5), sum)








## NOT USED:

## nca = raster(
##     file.path(
##         icrisat_dir,
##         paste0("net_cropland_area_india_", yr, ".tif")
##     )
## )
## gca = raster(
##     file.path(
##         icrisat_dir,
##         paste0("gross_cropland_area_india_", yr, ".tif")
##     )
## )
## nia = raster(
##     file.path(
##         icrisat_dir,
##         paste0("net_irr_area_india_", yr, ".tif")
##     )
## )
## gia = raster(
##     file.path(
##         icrisat_dir,
##         paste0("gross_irr_area_india_", yr, ".tif")
##     )
## )
