## Author : Simon Moulds
## Date   : Feb 2020

library(tidyverse)
library(terra)

## Load WFDEI data
frac <- rast("resources/WFD-EI-LandFraction2d.nc")

## Create template map @ 0.5 degree resolution
globe <- rast(nrow = 360, ncol = 720)

## Create extent for IGP
igp_ext <- extent(x = 60, xmax = 100, ymin = 20, ymax = 40)

## Assign values, write output
globe[] <- values(frac)
igp <- globe %>% crop(igp_ext)
writeRaster(
    igp, "results/WFD-EI-LandFraction2d_IGP.tif",
overwrite = TRUE
)
