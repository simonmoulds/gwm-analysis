## Author : Simon Moulds
## Date   : March 2021

library(tidyverse)
library(raster)
library(sf)
library(yaml)

## Extract configuration info
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly=TRUE)
  config <- read_yaml(args[1])
  args <- commandArgs()
  m <- regexpr("(?<=^--file=).+", args, perl=TRUE)
  cwd <- dirname(regmatches(args, m))
}

## Use JULES land mask as template
template <- raster("resources/land.nc", varname = "mask")
template[template == 0] <- NA

## Command areas
cmd_areas <- st_read("resources/irrigation/command_areas.shp")

## Identify canal command area
## Pakistan and West Bengal are outside of the study region
pakistan_ids <- c(1:5, 8:14, 16, 20, 37, 41)
west_bengal_ids <- c(31, 34, 42, 43, 44)
india_cmd_area <-
  cmd_areas %>%
  filter(!ID %in% c(pakistan_ids, west_bengal_ids))

## Convert to sp for plotting with raster
india_cmd_area_sp <- india_cmd_area %>% as_Spatial()
india_cmd_area <- rasterize(
  india_cmd_area_sp, template, field = "ID"
)
india_cmd_area[india_cmd_area > 0] <- 1
india_cmd_area_poly <- rasterToPolygons(
  india_cmd_area, dissolve = TRUE
) %>% st_as_sf()

## Create raster images of the east and west portion of the basin
## TODO check this is consistent with the literature
## TODO maybe divide by median rainfall in the study region?
ew_divide <- colFromX(india_cmd_area, 79.125)
india_cmd_area_west <- india_cmd_area
india_cmd_area_west[, ew_divide:160] <- NA
india_cmd_area_east <- india_cmd_area
india_cmd_area_east[, 1:(ew_divide - 1)] <- NA

## Write command area regions to file 
writeRaster(
  india_cmd_area, "results/india_command_area.tif", overwrite = TRUE
) 
writeRaster(
  india_cmd_area_west, "results/india_command_area_west.tif", overwrite = TRUE
) 
writeRaster(
  india_cmd_area_east, "results/india_command_area_east.tif", overwrite = TRUE
)

# unzip(file.path(datadir, "lpj_com_b4_fullextent.zip"), exdir = datadir)
# r <- raster(file.path(datadir, "lpj_com_b4_fullextent.asc")) %>% 
#   crop(extent(template))
# writeRaster(
#   r,
#   "results/command_areas.tif",
#   overwrite = TRUE,
#   datatype = 'INT2U'
# )

## Reservoir points
pts <- read.table(
  file.path("resources/irrigation/LPJ_command_inlets_all_replacements_b.txt"),
  sep = ",", 
  header = TRUE
)
st_write(
  st_as_sf(pts, coords = c("POINT_X", "POINT_Y"), crs = 4326),
  "results/command_inlets_0.083333Deg.gpkg",
  append = FALSE
)

## @ 0.083333 degree resolution:
## LPJID| X       | Y
## -------------------------
## 108  | 80.1250 | 29.04163

## @ 0.100000 degree resolution:
## LPJID| X       | Y
## -------------------------
## 2    | 72.65   | 34.05
## 13   | 68.85   | 27.65
## 14   | 68.35   | 25.45
## 103  | 76.55   | 31.15
## 107  | 78.15   | 29.95
## 108  | 80.05   | 28.95
## 109  | 81.05   | 28.25
## 110  | 82.15   | 25.35
## 112  | 82.65   | 26.55
## 120  | 79.95   | 24.65
## 123  | 75.85   | 25.15
## 124  | 76.65   | 25.85
## 127  | 81.35   | 24.25
## 201  | 89.05   | 26.15

replace_xy <- function(pts, id, new_x, new_y) {
    if (all(id %in% pts$LPJID)) {
      ix <- match(id, pts$LPJID)
    } else {
      stop()
    }
    pts$POINT_X[ix] <- new_x
    pts$POINT_Y[ix] <- new_y
    pts
}

pts <- replace_xy(
    pts,
    id = c(2, 13, 14, 103, 107, 108, 109, 110, 112, 120, 123, 124, 127, 201),
    new_x = c(72.65, 68.85, 68.35, 76.55, 78.15, 80.05, 81.05, 82.15, 82.65, 79.95, 75.85, 76.65, 81.35, 89.05),
    new_y = c(34.05, 27.65, 25.45, 31.15, 29.95, 28.95, 28.25, 25.35, 26.55, 24.65, 25.15, 25.85, 24.25, 26.15)
)

pts$POINT_X <- floor(pts$POINT_X * 10) / 10 + 0.05
pts$POINT_Y <- floor(pts$POINT_Y * 10) / 10 + 0.05
pts <- pts %>% 
  dplyr::select(LPJID, POINT_X, POINT_Y) %>% 
  arrange(LPJID)

pts <- st_as_sf(pts, coords = c('POINT_X','POINT_Y'), crs = 4326)
st_write(
  pts,
  "results/command_inlets_0.100000Deg.gpkg",
  append = FALSE
)

## ## Rivers
## riv = raster("../data/merit_accum_cell_igp_0.100000Deg.tif")
## v = getValues(riv)
## v[v<25] = NA
## riv[] = v
## writeRaster(riv, "../data/merit_accum_cell_igp_filtered_0.100000Deg.tif", overwrite=TRUE, datatype='INT4U')
