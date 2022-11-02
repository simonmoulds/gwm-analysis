## Author : Simon Moulds
## Date   : Feb 2020

library(raster)
library(magrittr)

datadir = Sys.getenv("DATADIR")

## Load WFDEI data
frac = raster(file.path(datadir, "WFD-EI-LandFraction2d.nc"))

## Create template map @ 0.5 degree resolution
globe = raster(nrow=360, ncol=720)

## Create extent for IGP
igp_ext = extent(x=60, xmax=100, ymin=20, ymax=40)

## Assign values, write output
globe[] = getValues(frac)
igp = globe %>% crop(igp_ext)
writeRaster(
    igp,
    file.path(datadir, "WFD-EI-LandFraction2d_IGP.tif"),
    overwrite=TRUE
)
