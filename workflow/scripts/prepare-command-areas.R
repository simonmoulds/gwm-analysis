## Author : Simon Moulds
## Date   : March 2021

library(tidyverse)
library(raster)
library(sf)
library(yaml)

sessionInfo()
print(.libPaths())

## Extract configuration info
if (sys.nframe() == 0L) {
  args = commandArgs(trailingOnly=TRUE)
  config = read_yaml(args[1])
  args = commandArgs()
  m <- regexpr("(?<=^--file=).+", args, perl=TRUE)
  cwd <- dirname(regmatches(args, m))
}

datadir = config$irrigation_datadir

## Command areas
unzip(file.path(datadir, 'command_areas.zip'), exdir=datadir)
cmd_areas = st_read(file.path(datadir, 'command_areas.shp'))

## Command areas, raster
template = raster(xmn=60, xmx=100, ymn=20, ymx=35, nrow=30, ncol=80)
unzip(file.path(datadir, 'lpj_com_b4_fullextent.zip'), exdir=datadir)
r = raster(file.path(datadir, 'lpj_com_b4_fullextent.asc')) %>% crop(extent(template))
writeRaster(
  r,
  "results/command_areas.tif",
  overwrite=TRUE,
  datatype='INT2U'
)

## Reservoir points
pts = read.table(file.path(datadir, 'LPJ_command_inlets_all_replacements_b.txt'), sep=",", header=TRUE)
st_write(
  st_as_sf(pts, coords=c("POINT_X", "POINT_Y"), crs=4326),
  "results/command_inlets_0.083333Deg.gpkg",
  append=FALSE
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

replace_xy = function(pts, id, new_x, new_y) {
    if (all(id %in% pts$LPJID)) {        
        ix = match(id, pts$LPJID)
    } else {
        stop()
    }
    pts$POINT_X[ix] = new_x
    pts$POINT_Y[ix] = new_y
    pts
}

pts = replace_xy(
    pts,
    id=c(2, 13, 14, 103, 107, 108, 109, 110, 112, 120, 123, 124, 127, 201),
    new_x = c(72.65, 68.85, 68.35, 76.55, 78.15, 80.05, 81.05, 82.15, 82.65, 79.95, 75.85, 76.65, 81.35, 89.05),
    new_y = c(34.05, 27.65, 25.45, 31.15, 29.95, 28.95, 28.25, 25.35, 26.55, 24.65, 25.15, 25.85, 24.25, 26.15)
)

pts$POINT_X = floor(pts$POINT_X * 10) / 10 + 0.05
pts$POINT_Y = floor(pts$POINT_Y * 10) / 10 + 0.05
pts %<>% dplyr::select(LPJID, POINT_X, POINT_Y) %>% arrange(LPJID)

## write.table(
##   pts,
##   "results/command_inlets_0.100000Deg.txt",
##   sep=' ', row.names=FALSE
## )

pts = st_as_sf(pts, coords=c('POINT_X','POINT_Y'), crs=4326)
st_write(
  pts,
  'results/command_inlets_0.100000Deg.gpkg',
  append=FALSE
)

## ## Rivers
## riv = raster("../data/merit_accum_cell_igp_0.100000Deg.tif")
## v = getValues(riv)
## v[v<25] = NA
## riv[] = v
## writeRaster(riv, "../data/merit_accum_cell_igp_filtered_0.100000Deg.tif", overwrite=TRUE, datatype='INT4U')
