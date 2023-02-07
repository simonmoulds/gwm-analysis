## Author : Simon Moulds
## Date   : April 2021

## library(ncdf4)
## library(ncdf4.helpers)
library(raster)
## library(rasterVis)
## library(RColorBrewer)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgdal)
## library(spatialEco)
library(tidyverse)
## library(patchwork)
## library(cowplot)
## Use `zyp` to get estimate of intercept as well as slope
## library(zyp)

options(stringsAsFactors = FALSE)

## Extract configuration info
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly=TRUE)
  inputdir <- args[1]
  outputdir <- args[2]
  args <- commandArgs()
  m <- regexpr("(?<=^--file=).+", args, perl=TRUE)
  cwd <- dirname(regmatches(args, m))
}
## Load custom utilities
source(file.path(cwd, "utils.R"))

if (!dir.exists(outputdir))
  dir.create(outputdir, recursive = TRUE)

## ####################################################### ##
## ####################################################### ##
##
## Preamble [TODO - put in separate script]
##
## ####################################################### ##
## ####################################################### ##

ganges_basin <- raster("resources/land.nc")
ganges_basin[ganges_basin == 0] <- NA

## Identify canal command area
cmd_area <- st_read("resources/irrigation/command_areas.shp")
pakistan_ids <- c(1:5, 8:14, 16, 20, 37, 41)
west_bengal_ids <- c(31, 34, 42, 43, 44)
india_cmd_area <-
  cmd_area %>%
  filter(!ID %in% c(pakistan_ids, west_bengal_ids))

## Convert to sp for plotting with raster
india_cmd_area_sp <- india_cmd_area %>% as_Spatial()
india_cmd_area <- rasterize(india_cmd_area_sp, ganges_basin, field = "ID")
india_cmd_area[india_cmd_area > 0] <- 1
india_cmd_area_poly <- rasterToPolygons(
  india_cmd_area, dissolve = TRUE
) %>% st_as_sf()

## Create raster images of the east and west portion of the basin
## TODO check this is consistent with the literature
ew_divide <- colFromX(india_cmd_area, 79.125)
india_cmd_area_west <- india_cmd_area
india_cmd_area_west[,ew_divide:160] <- NA
india_cmd_area_east <- india_cmd_area
india_cmd_area_east[,1:(ew_divide - 1)] <- NA

## Create a list of basin regions which we can loop through
basin_regions <- list(
  igp=india_cmd_area,
  igp_east = india_cmd_area_east,
  igp_west = india_cmd_area_west
)
grid_cell_area <- raster::area(india_cmd_area) # km2

## ####################################################### ##
## ####################################################### ##
##
## Historical time series
##
## ####################################################### ##
## ####################################################### ##

overwrite <- TRUE
years <- 1979:2013
seasons <- c("kharif", "rabi", "zaid", "continuous")
types <- c("gw", "sw", "total")
basins <- names(basin_regions)

compute_basin_total <- function(fn, basin) {
  r <- raster(fn)
  r <- resample(r, ganges_basin, method="ngb")
  r <- r / 1000 # m -> km
  basin_area <- basin * grid_cell_area
  r <- r * basin_area
  basin_sum <- sum(getValues(r), na.rm=TRUE) # km3
  basin_area <- sum(getValues(basin_area), na.rm=TRUE)
  basin_avg <- (basin_sum / (basin_area)) * 1000
  basin_avg
}

output_list <- list()
for (m in 1:length(basins)) {
  basin = basins[m]
  pb <- txtProgressBar(min = 0, max = length(years), initial = 0)
  for (i in 1:length(years)) {
    year <- years[i]
    ptn <- paste0("annual_precip_historical_", year, "_(.*).tif")
    fn <- list.files(inputdir, ptn, full.names = TRUE)
    ## fn <- file.path(
    ##   inputdir,
    ##   paste0("annual_precip_historical_", year, "_noirrig.tif")
    ## )
    precip_basin_sum <- compute_basin_total(fn, basin_regions[[basin]])
    et_basin_sum <- compute_basin_total(
      fn %>% gsub("precip", "et", .),
      basin_regions[[basin]]
    )
    surf_roff_basin_sum <- compute_basin_total(
      fn %>% gsub("precip", "surf_roff", .),
      basin_regions[[basin]]
    )
    sub_surf_roff_basin_sum <- compute_basin_total(
      fn %>% gsub("precip", "sub_surf_roff", .),
      basin_regions[[basin]]
    )
    for (j in 1:length(seasons)) {
      season <- seasons[j]
      for (k in 1:length(types)) {
        type <- types[k]
        ptn <- paste0(season, "_", type, "_irrigation_historical_", year, "_(.*).tif")
        fn <- list.files(inputdir, ptn, full.names = TRUE)
        ## fn <- file.path(
        ##   inputdir,
        ##   paste0(
        ##     season, "_", type,
        ##     "_irrigation_historical_",
        ##     year, "_irrig.tif"
        ##   )
        ## )
        irrigation_basin_sum <- compute_basin_total(fn, basin_regions[[basin]])
        output_list[[length(output_list) + 1]] <- data.frame(
          year = year,
          season = season,
          types = type,
          basin = basin,
          irrigation = irrigation_basin_sum,
          precip = precip_basin_sum,
          et = et_basin_sum,
          surf_roff = surf_roff_basin_sum,
          sub_surf_roff = sub_surf_roff_basin_sum
        )
      }
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
}
historical_ts <- do.call("rbind", output_list) %>% as_tibble()
saveRDS(
  object = historical_ts,
  file = file.path(outputdir, "historical_irrigation_demand_ts.rds")
)

## FIXME this only needs to be done for JULES_vn6.1_irrig
## output_list = list()
## pb = txtProgressBar(min = 0, max = length(years), initial = 0)
## for (i in 1:(length(years)-1)) {
##   year = years[i]
##   ## TODO precipitation output
##   recharge_fn = file.path(
##     historical_analysis_dir,
##     sprintf("recharge_historical_%d_irrig.tif", year)
##   )
##   recharge_map = raster(recharge_fn)
##   abstraction_fn = file.path(
##     historical_analysis_dir,
##     sprintf("abstraction_historical_%d_irrig.tif", year)
##   )
##   abstraction_map = raster(abstraction_fn)
##   dS_map = recharge_map - abstraction_map
##   dS_map = resample(dS_map, india_cmd_area)
##   ## output_map_list[[length(output_map_list) + 1]] = dS_map
##   for (j in 1:length(basins)) {
##     basin = basins[j]
##     recharge_sum = compute_basin_total(recharge_fn, basin_regions[[basin]])
##     abstraction_sum = compute_basin_total(abstraction_fn, basin_regions[[basin]])
##     dS = recharge_sum - abstraction_sum
##     output_list[[length(output_list) + 1]] = data.frame(
##       year=year,
##       basin = basin,
##       volume=dS
##     )
##   }
##   setTxtProgressBar(pb, i)
## }
## close(pb)

## output = do.call("rbind", output_list) %>% as_tibble()
## xx = output %>% arrange(year)
## saveRDS(
##   object = xx,
##   file = file.path(outputdir, "historical_water_balance_ts.rds")
## )
