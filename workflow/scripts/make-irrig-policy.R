## Author : Simon Moulds
## Date   : October 2021

## The aim is to show the extent to which increased sw irrigation
## during the rainy season can enhance groundwater recharge and
## offset groundwater abstraction for irrigation during the rabi
## season.
## In this script we produce the maps showing the amount by which
## canal irrigation can be increased.

## TODO:
## * Total volumetric irrigation demand [historical mean]
## * Current canal supply
## * Inflow at command area inlets
## * Decide on the maximum mean canal coverage within command area [e.g. 0.9]
## ## Get volumetric irrigation demand [from JULES]
## dmd = dmd[pts]
## ## Current canal supply [from JULES]
## canal_supply = supply[pts]
## ## Get inflow to the command area [from JULES-MOSART]
## inflow = inflow[pts]
## ## Calculate deficit
## deficit = dmd - canal_supply
## ## Scale factor [limit by maximum mean canal coverage]
## sf = min(1, inflow / deficit)
## * checks
## * one more policy, with increased irrigated area

library(raster)
library(rgdal)
library(magrittr)
library(sf)
library(dplyr)

## Extract configuration info
if (sys.nframe() == 0L) {
  args = commandArgs(trailingOnly=TRUE)
  outputdir = args[1]
  ## args = commandArgs()
  ## m <- regexpr("(?<=^--file=).+", args, perl=TRUE)
  ## cwd <- dirname(regmatches(args, m))
}

if (!dir.exists(outputdir))
  dir.create(outputdir, recursive = TRUE)

resourcedir = 'resources'

## TODO increase
## For now use a scale factor of 0.75
sf = 0.75

## ################################### ##
## Load irrigated area maps
## ################################### ##

reference_year = 2010
irrigation_sources = c(
  "canal", "other_sources", "other_wells", "tanks", "tubewells"
)
irrigation_source_maps = list()
for (i in 1:length(irrigation_sources)) {
  source = irrigation_sources[i]
  fn = file.path(
    resourcedir, 'irrigated_area_maps',
    paste0("icrisat_kharif_", source, "_", reference_year, "_india_0.500000Deg.tif")
  )
  irrigation_source_maps[[source]] = raster(fn)
}
irrigation_source_maps = stack(irrigation_source_maps)

## ################################### ##
## India command areas
## ################################### ##

command_areas = st_read(
  file.path(resourcedir, "command_areas.shp")
)
pakistan_ids = c(1,2,3,4,5,8,9,10,11,12,13,14,16,20,37,41)
india_command_areas = command_areas %>% filter(!ID %in% pakistan_ids)

india_frac = raster(file.path(resourcedir, "india_frac_0.500000Deg.tif"))
wfdei_frac = raster(file.path(resourcedir, "WFD-EI-LandFraction2d_IGP.tif"))
india_canal_frac = raster(wfdei_frac)
india_canal_frac[wfdei_frac] = 0

command_area_ids = india_command_areas$ID
command_area_maps = lapply(command_area_ids, FUN=function(x) india_canal_frac)
for (i in 1:length(command_area_ids)) {
  id = command_area_ids[i]
  ar = india_command_areas %>% filter(ID %in% id)
  ext = extent(ar)
  xmn = floor(ext@xmin)
  xmx = ceiling(ext@xmax)
  ymn = floor(ext@ymin)
  ymx = ceiling(ext@ymax)
  fine_template = raster(
    nrows = (ymx - ymn) * 120,
    ncols = (xmx - xmn) * 120,
    xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx
  )
  r = rasterize(ar, fine_template)
  r[is.na(r)] = 0
  frac = raster::aggregate(r, fact=60, fun=mean)
  pts = as(frac, "SpatialPoints")
  india_canal_frac[pts] = india_canal_frac[pts] + frac[pts]
  command_area_maps[[i]][pts] = command_area_maps[[i]][pts] + frac[pts]
}

india_canal_frac[india_canal_frac == 0] = NA
pts = as(india_canal_frac, "SpatialPoints")
india_canal_frac_adj = india_canal_frac
india_canal_frac_adj[pts] = pmin(india_canal_frac[pts], india_frac[pts])

## Scale accordingly to ensure that in any given grid cell
## the canal fraction does not exceed the fraction
## belonging to India
for (i in 1:length(command_area_ids)) {
  command_area_maps[[i]] = (command_area_maps[[i]] / india_canal_frac) * india_canal_frac_adj
}
india_canal_frac = stackApply(stack(command_area_maps), indices=rep(1, length(command_area_maps)), fun=sum)

## Write output - fraction of grid squares belonging to each command area
writeRaster(
  india_canal_frac,
  file.path(outputdir, "india_command_area_fraction.tif"),
  overwrite=TRUE
)

for (i in 1:length(command_area_ids)) {
  id = command_area_ids[i]
  writeRaster(
    command_area_maps[[i]],
    file.path(
      outputdir,
      paste0("india_command_area_", id, "_fraction.tif")
    ),
    overwrite=TRUE
  )
}

## Allow for the fact that some command areas may be able
## to increase capacity by more than others
reference_map = raster(
  file.path(
    resourcedir, "irrigated_area_maps",
    paste0("icrisat_kharif_canal_2010_india_0.500000Deg.tif")
  )
)

## Write current policy map
writeRaster(
  reference_map,
  file.path(
    outputdir,
    paste0("icrisat_kharif_canal_2010_india_0.500000Deg_current.tif")
  ),
  overwrite=TRUE
)

## Write restored policy map
## restored_map = reference_map
## restored_map = restored_map - (india_canal_frac * restored_map)
restored_irrigation_source_maps =
    irrigation_source_maps %>%
    unstack %>%
    setNames(irrigation_sources)

for (i in 1:length(command_area_maps)) {
  ## Fraction of each cell belonging to current grid square
  ar = command_area_maps[[i]]
  ar[ar==0] = NA
  pts = as(ar, "SpatialPoints") # this removes NA cells
  frac = ar[pts]
  ## Area irrigated by canals in current command area
  current_area =
    irrigation_source_maps[pts] %>%
    as_tibble %>%
    mutate(across(all_of(names(.)), ~.* frac))
  total_canal_area = current_area[["canal"]]
  total_irrigated_area =
    current_area %>%
    apply(1, sum)
  total_not_canal_area =
    current_area %>%
    dplyr::select(-canal) %>% apply(1, sum)
  max_increase = total_irrigated_area - total_canal_area

  ## Multiply max_increase by scale factor
  increase_area = max_increase * sf
  restored_area =
    current_area %>%
    mutate(canal=canal+increase_area)
  remaining_area = total_irrigated_area - restored_area$canal
  ## Convert other sources to relative values
  other_sources = c("other_sources", "other_wells", "tanks", "tubewells")
  restored_area =
    restored_area %>%
    mutate(across(!canal, ~./total_not_canal_area)) %>%
    mutate(across(!canal, ~.*remaining_area))
  new_total_irrigated_area = apply(restored_area, 1, sum)
  new_total_irrigated_area %<>% `[<-`(!is.finite(.), 0)
  if (!all.equal(total_irrigated_area, new_total_irrigated_area)) {
    stop()
  }
  for (source in irrigation_sources) {
    map = restored_irrigation_source_maps[[source]]
    map[pts] = (map[pts] * (1 - frac)) + restored_area[[source]]
    restored_irrigation_source_maps[[source]] = map
  }
}

for (i in 1:length(irrigation_sources)) {
  source = irrigation_sources[i]
  writeRaster(
    irrigation_source_maps[[source]],
    file.path(
      outputdir,
      paste0(
        "icrisat_kharif_",
        source,
        "_", reference_year,
        "_india_0.500000Deg_current_canal.tif"
      )
    ),
    overwrite=TRUE
  )
  writeRaster(
    restored_irrigation_source_maps[[source]],
    file.path(
      outputdir,
      paste0(
        "icrisat_kharif_",
        source,
        "_", reference_year,
        "_india_0.500000Deg_restored_canal.tif"
      )
    ),
    overwrite=TRUE
  )

  ## Copy other maps
  for (policy in c("current_canal", "restored_canal")) {
    for (season in c("rabi", "zaid", "continuous")) {
      fn0 = file.path(
        resourcedir, "irrigated_area_maps",
        paste0("icrisat_", season, "_", source, "_", reference_year, "_india_0.500000Deg.tif")
      )
      fn1 = file.path(
        outputdir,
        paste0("icrisat_", season, "_", source, "_", reference_year, "_india_0.500000Deg_", policy, ".tif")
      )
      cmd = paste0("cp ", fn0, " ", fn1)
      system(cmd)
    }
  }
}

