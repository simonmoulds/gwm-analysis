## Author : Simon Moulds
## Date   : April 2021

# library(ncdf4)
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

## TESTING
cwd <- 'workflow/scripts'
source(file.path(cwd, "utils.R"))

## if (!dir.exists(outputdir))
##   dir.create(outputdir, recursive = TRUE)


## ####################################################### ##
## ####################################################### ##
##
## Preamble [TODO - put in separate script]
##
## ####################################################### ##
## ####################################################### ##


# Years in simulation
years <- 1979:2013

# Growing seasons
seasons <- c("kharif", "rabi", "zaid", "continuous")

# Irrigation sources
types <- c("gw", "sw", "total")

# JULES landmask
ganges_basin <- raster("resources/land.nc", varname = "mask")
ganges_basin[ganges_basin == 0] <- NA

# Canal command areas
india_cmd_area <- raster("results/india_command_area.tif")
india_cmd_area_west <- raster("results/india_command_area_west.tif")
india_cmd_area_east <- raster("results/india_command_area_east.tif")
india_cmd_area_poly <- rasterToPolygons(
  india_cmd_area, dissolve = TRUE
) %>% st_as_sf()
st_write(india_cmd_area_poly, "results/plotting/india_command_area.gpkg", delete_dsn = TRUE)

# Create a list of basin regions which we can loop through
basin_regions <- list(
  igp = india_cmd_area,
  igp_east = india_cmd_area_east,
  igp_west = india_cmd_area_west
)
basins <- names(basin_regions)

# Grid cell area is needed to convert depths to volumes
grid_cell_area <- raster::area(india_cmd_area) # km2


## ####################################################### ##
## ####################################################### ##
##
## Historical time series
##
## ####################################################### ##
## ####################################################### ##


output_list <- list()
for (m in 1:length(basins)) {
  basin <- basins[m]
  pb <- txtProgressBar(min = 0, max = length(years), initial = 0)
  for (i in 1:length(years)) {
    year <- years[i]
    fn <- file.path(
      "results/JULES_vn6.1_irrig",
      paste0("annual_precip_historical_", year, "_irrig.tif")
    )
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
        fn <- file.path(
          "results/JULES_vn6.1_irrig",
          paste0(
            season, "_", type,
            "_irrigation_historical_",
            year, "_irrig.tif"
          )
        )
        irrigation_basin_sum <- compute_basin_total(
          fn, basin_regions[[basin]]
        )
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
  file = "results/plotting/historical_irrigation_demand_ts.rds"
)


# Historical water balance
output_list <- list()
output_map_list <- list()
pb <- txtProgressBar(min = 0, max = length(years), initial = 0)
for (i in 1:(length(years)-1)) {
  year <- years[i]
  recharge_fn <- file.path(
    "results/JULES_vn6.1_irrig",
    sprintf("recharge_historical_%d_irrig.tif", year)
  )
  recharge_map <- raster(recharge_fn)
  abstraction_fn <- file.path(
    "results/JULES_vn6.1_irrig",
    sprintf("abstraction_historical_%d_irrig.tif", year)
  )
  abstraction_map <- raster(abstraction_fn)
  dS_map <- recharge_map - abstraction_map
  dS_map <- resample(dS_map, india_cmd_area)
  for (j in 1:length(basins)) {
    basin <- basins[j]
    recharge_sum <- compute_basin_total(recharge_fn, basin_regions[[basin]])
    abstraction_sum <- compute_basin_total(abstraction_fn, basin_regions[[basin]])
    dS <- recharge_sum - abstraction_sum
    output_list[[length(output_list) + 1]] <- data.frame(
      year = year,
      basin = basin,
      volume = dS
    )
  }
  dS_map <- dS_map * india_cmd_area
  output_map_list[[length(output_map_list) + 1]] <- dS_map
  setTxtProgressBar(pb, i)
}
close(pb)

# Save water balance time series data
output <- do.call("rbind", output_list) %>% as_tibble()
xx <- output %>% arrange(year)
saveRDS(
  object = xx,
  file = "results/plotting/historical_water_balance_ts.rds"
)

# Save water balance maps
historical_water_balance_maps <- stack(output_map_list)
historical_water_balance_maps <- resample(historical_water_balance_maps, india_cmd_area)
historical_water_balance_maps <- historical_water_balance_maps * india_cmd_area
writeRaster(historical_water_balance_maps, "results/plotting/historical_water_balance_ts.tif", options = "INTERLEAVE=BAND", overwrite = TRUE)

dS_historical_mean <- stackApply(
  historical_water_balance_maps, 
  indices = rep(1, nlayers(historical_water_balance_maps)), 
  fun = mean
)
writeRaster(dS_historical_mean, "results/plotting/dS_historical_mean.tif", overwrite = TRUE)

# Historical irrigation 
sw_kharif_maps <- list()
gw_kharif_maps <- list()
sw_rabi_maps <- list()
gw_rabi_maps <- list()
precip_maps <- list()
pet_maps <- list()
aridity_maps <- list()

for (i in 1:length(years)) {

  sw_kharif_total <- list.files(
    "resources/irrigated_area_maps/",
    pattern = paste0("icrisat_kharif_(canal|other_sources|tanks)_", years[i], "_india_0.500000Deg.tif"),
    full.names = TRUE
  ) %>%
    raster::stack() %>%
    stackApply(indices = c(1, 1, 1), fun = sum) %>%
    resample(india_cmd_area) %>%
    `*`(india_cmd_area)

  gw_kharif_total <- list.files(
    "resources/irrigated_area_maps/",
    pattern = paste0("icrisat_kharif_(other_wells|tubewells)_", years[i], "_india_0.500000Deg.tif"),
    full.names = TRUE
  ) %>%
    raster::stack() %>%
    stackApply(indices = c(1, 1), fun = sum) %>%
    resample(india_cmd_area) %>%
    `*`(india_cmd_area)

  sw_rabi_total <- list.files(
    "resources/irrigated_area_maps/",
    pattern = paste0("icrisat_rabi_(canal|other_sources|tanks)_", years[i], "_india_0.500000Deg.tif"),
    full.names = TRUE
  ) %>%
    raster::stack() %>%
    stackApply(indices = c(1, 1), fun = sum) %>%
    resample(india_cmd_area) %>%
    `*`(india_cmd_area)

  gw_rabi_total <- list.files(
    "resources/irrigated_area_maps/",
    pattern = paste0("icrisat_rabi_(other_wells|tubewells)_", years[i], "_india_0.500000Deg.tif"),
    full.names = TRUE
  ) %>%
    raster::stack() %>%
    stackApply(indices = c(1, 1), fun = sum) %>%
    resample(india_cmd_area) %>%
    `*`(india_cmd_area)

  precip_total <- list.files(
    "results/JULES_vn6.1_irrig",
    pattern = paste0("annual_precip_historical_", years[i], "_irrig.tif"),
    full.names = TRUE
  ) %>%
    raster::raster() %>%
    resample(india_cmd_area) %>%
    `*`(india_cmd_area)

  pet_total <- list.files(
    "results/JULES_vn6.1_irrig",
    pattern = paste0("annual_pet_historical_", years[i], "_irrig.tif"),
    full.names = TRUE
  ) %>%
    raster::raster() %>%
    resample(india_cmd_area) %>%
    `*`(india_cmd_area)

  sw_kharif_maps[[i]] <- sw_kharif_total * raster::area(sw_kharif_total)
  gw_kharif_maps[[i]] <- gw_kharif_total * raster::area(gw_kharif_total)
  sw_rabi_maps[[i]] <- sw_rabi_total * raster::area(sw_rabi_total)
  gw_rabi_maps[[i]] <- gw_rabi_total * raster::area(gw_rabi_total)
  precip_maps[[i]] <- precip_total
  pet_maps[[i]] <- pet_total
  aridity_index <- precip_total / pet_total
  aridity_maps[[i]] <- aridity_index
}

## Promote to RasterStack
sw_kharif_maps <- stack(sw_kharif_maps)
gw_kharif_maps <- stack(gw_kharif_maps)
sw_rabi_maps <- stack(sw_rabi_maps)
gw_rabi_maps <- stack(gw_rabi_maps)
precip_maps <- stack(precip_maps)
pet_maps <- stack(pet_maps)
aridity_maps <- stack(aridity_maps)

writeRaster(sw_kharif_maps, "results/plotting/historical_sw_kharif_ts.tif", options = "INTERLEAVE=BAND", overwrite = TRUE)
writeRaster(gw_kharif_maps, "results/plotting/historical_gw_kharif_ts.tif", options = "INTERLEAVE=BAND", overwrite = TRUE)
writeRaster(sw_rabi_maps, "results/plotting/historical_sw_rabi_ts.tif", options = "INTERLEAVE=BAND", overwrite = TRUE)
writeRaster(gw_rabi_maps, "results/plotting/historical_gw_rabi_ts.tif", options = "INTERLEAVE=BAND", overwrite = TRUE)
writeRaster(precip_maps, "results/plotting/historical_precip_ts.tif", options = "INTERLEAVE=BAND", overwrite = TRUE)
writeRaster(pet_maps, "results/plotting/historical_pet_ts.tif", options = "INTERLEAVE=BAND", overwrite = TRUE)
writeRaster(aridity_maps, "results/plotting/historical_aridity_ts.tif", options = "INTERLEAVE=BAND", overwrite = TRUE)

# Maps for plotting activated GWM
current_gw_area <- stack(
  list.files(
    "results/irrigated_area_maps",
    pattern = "icrisat_kharif_(other_wells|tubewells)_2010_india_0.500000Deg_current_canal.tif",
    full.names = TRUE
  )
) %>% stackApply(indices = c(1, 1), fun = sum)
current_gw_area <- resample(current_gw_area, india, method = "ngb")
current_gw_area <- current_gw_area * india_cmd_area
writeRaster(current_gw_area, "results/plotting/current_gw_area.tif", overwrite = TRUE) 

current_canal_area <- raster(
  file.path(
    "results/irrigated_area_maps",
    "icrisat_kharif_canal_2010_india_0.500000Deg_current_canal.tif"
  )
)
current_canal_area <- resample(current_canal_area, india, method = "ngb")
current_canal_area <- current_canal_area * india_cmd_area
writeRaster(current_canal_area, "results/plotting/current_canal_area.tif", overwrite = TRUE)

restored_canal_area <- raster(
  file.path(
    "results/irrigated_area_maps",
    "icrisat_kharif_canal_2010_india_0.500000Deg_restored_canal.tif"
  )
)
writeRaster(restored_canal_area, "results/plotting/restored_canal_area.tif", overwrite = TRUE)

output_list <- list()
current_output_map_list <- list()
restored_output_map_list <- list()
pb = txtProgressBar(min = 0, max = length(years) - 1, initial = 0)
for (i in 1:(length(years)-1)) {
  year <- years[i]
  
  ## Change in storage under current canal area
  current_abstraction_fn <- file.path(
    "results/JULES_vn6.1_irrig_current",
    sprintf("abstraction_current_canal_%s_current.tif", year)
  )

  current_recharge_fn <- file.path(
    "results/JULES_vn6.1_irrig_current",
    sprintf("recharge_current_canal_%s_current.tif", year)
  )
  current_abstraction_map <- raster(current_abstraction_fn)
  current_recharge_map <- raster(current_recharge_fn)
  dS_current_map <- current_recharge_map - current_abstraction_map
  dS_current_map <- resample(dS_current_map, india_cmd_area)
  dS_current_map <- dS_current_map * india_cmd_area

  ## Change in storage under restored canal area
  restored_abstraction_fn <- file.path(
    "results/JULES_vn6.1_irrig_current",
    sprintf("abstraction_restored_canal_%s_current.tif", year)
  )
  restored_recharge_fn <- file.path(
    "results/JULES_vn6.1_irrig_current",
    sprintf("recharge_restored_canal_%s_current.tif", year)
  )
  restored_abstraction_map <- raster(restored_abstraction_fn)
  restored_recharge_map <- raster(restored_recharge_fn)
  dS_restored_map <- restored_recharge_map - restored_abstraction_map
  dS_restored_map <- resample(dS_restored_map, india_cmd_area)
  dS_restored_map <- dS_restored_map * india_cmd_area

  current_output_map_list[[length(current_output_map_list) + 1]] <- dS_current_map
  restored_output_map_list[[length(restored_output_map_list) + 1]] <- dS_restored_map

  for (m in 1:length(basins)) {
    basin <- basins[m]
    current_abstraction_sum <- compute_basin_total(current_abstraction_fn, basin_regions[[basin]])
    current_recharge_sum <- compute_basin_total(current_recharge_fn, basin_regions[[basin]])
    dS_current <- current_recharge_sum - current_abstraction_sum
    restored_recharge_sum <- compute_basin_total(restored_recharge_fn, basin_regions[[basin]])
    restored_abstraction_sum <- compute_basin_total(restored_abstraction_fn, basin_regions[[basin]])
    dS_restored <- restored_recharge_sum - restored_abstraction_sum
    ## Add to data frame
    output_list[[length(output_list) + 1]] <- data.frame(
      year = year,
      basin = basin,
      policy = c("current", "restored"),
      dS = c(dS_current, dS_restored),
      abstraction = c(current_abstraction_sum, restored_abstraction_sum),
      recharge = c(current_recharge_sum, restored_recharge_sum)
    )
  }
  setTxtProgressBar(pb, i)
}
close(pb)

output <-
  do.call("rbind", output_list) %>%
  as_tibble() %>%
  mutate(volume = recharge - abstraction)

saveRDS( 
  object = output,
  file = "results/plotting/scenario_water_balance_ts.rds"
)

dS_current_mean <-
  raster::stack(current_output_map_list) %>%
  stackApply(rep(1, length(current_output_map_list)), mean)

dS_restored_mean <-
  raster::stack(restored_output_map_list) %>%
  stackApply(rep(1, length(restored_output_map_list)), mean)

writeRaster(dS_current_mean, "results/plotting/dS_current_canal_ts.tif", overwrite = TRUE)
writeRaster(dS_restored_mean, "results/plotting/dS_restored_canal_ts.tif", overwrite = TRUE)

# Irrigated area in current and restored scenarios (same for both)
# NB the wildcard in the file pattern is irrigation source
irr_area_fs <- list.files(
  "results/irrigated_area_maps",
  pattern = "icrisat_kharif_(.*)_2010_india_0.500000Deg_current_canal.tif",
  full.names = TRUE
)
irr_area <- stack(irr_area_fs) %>% stackApply(rep(1, 5), sum)
irr_area <- resample(irr_area, india_cmd_area)
irr_area <- irr_area * india_cmd_area
writeRaster(irr_area, "results/plotting/current_irrigated_area.tif", overwrite = TRUE)
