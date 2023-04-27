## Author : Simon Moulds
## Date   : Oct 2021

library(raster)
library(sf)

## Study region
mask = raster('../data/wfdei/ancils/WFD-EI-LandFraction2d_igp.nc')

## Monsoon onset
onset = raster("../data-raw/median.onset.wet.season.ap.igp.nc") * mask
onset[mask==0] = 0

## 1 July
onset[is.na(onset)] = 182

writeRaster(
    onset,
    "../data/igp_wet_season_onset.tif",
    overwrite=TRUE
)

