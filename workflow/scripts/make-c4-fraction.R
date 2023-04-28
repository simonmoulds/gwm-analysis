#!/usr/bin/env Rscript

## Author : Simon Moulds
## Date   : Oct 2019

library(terra)
library(magrittr)

## To divide vegetation classes between C3/C4 types we follow the method
## outlined in Zhang et al (2016):
## https://www.nature.com/articles/sdata2017165#data-records

## ####################################################### ##
## 1. C4 fraction: natural vegetation
## ####################################################### ##

unzip(
  "resources/ISLSCP_C4_1DEG_932.zip",
  exdir = "results/intermediate"
)
x <- rast("results/intermediate/ISLSCP_C4_1DEG_932/data/c4_percent_1d.asc")
x[x == -999] <- 0
x <- x / 100
rgns <- c(0.5, 0.25, 0.125, 0.1, 1 / 12, 0.0625, 0.05, 1 / 60, 0.01, 1 / 120)
rgns_str <- formatC(rgns, digits = 6, format = "f", flag = 0)
for (i in 1:length(rgns)) {
  y <- rast(extent = ext(r), resolution = rgns[i])
  fact <- 1 / rgns[i]
  ## The native resolution is coarser than all the resolutions
  ## we are interested in, so we can use disagg
  r <- disagg(x, fact, method = 'bilinear') # method = 'near')
  writeRaster(
    r,
    file.path('results/intermediate', paste0('c4_nat_veg_frac_globe_', rgns_str[i], 'Deg.tif')),
    overwrite = TRUE
  )
}


## ####################################################### ##
## 2. C4 fraction: cropland
## ####################################################### ##

unzip(
  "resources/MapSPAM/spam2005V3r1_global_harv_area.geotiff.zip",
  exdir = "results/intermediate/mapspam_data"
)

fs <- list.files(
    "results/intermediate/mapspam_data",
    pattern = "^SPAM2005V3r1_global_H_T(A|H|I|L|R|S)_[A-Z]+_(A|H|I|L|R|S).tif$",
    full.names = TRUE
)
st <- rast(fs)
total_cropland_area <- app(st, sum, na.rm = TRUE)
total_cropland_area[total_cropland_area == 0] <- NA

## C4 crops: sugarcane, maize, millet (small, pearl), sorghum
c4_fs <- list.files(
    "results/intermediate/mapspam_data",
    pattern = "^SPAM2005V3r1_global_H_T(A|H|I|L|R|S)_(SUGC|MAIZ|SMIL|PMIL|SORG)_(A|H|I|L|R|S).tif$",
    full.names = TRUE
)
st <- rast(c4_fs)
total_c4_cropland_area <- app(st, sum, na.rm = TRUE)
total_c4_cropland_area[total_c4_cropland_area == 0] <- NA

global_ext <- ext(-180, 180, -90, 90)
total_cropland_area <- extend(total_cropland_area, global_ext)
total_c4_cropland_area <- extend(total_c4_cropland_area, global_ext)

c4_crop_5arc <- total_c4_cropland_area / total_cropland_area
c4_crop_5arc[is.na(c4_crop_5arc)] <- 0

writeRaster(
  c4_crop_5arc,
  file.path("results/intermediate/c4_crop_frac_0.083333Deg.tif"),
  overwrite = TRUE
)

for (i in 1:length(rgns)) {
  y <- rast(extent = ext(r), resolution = rgns[i])
  r <- resample(c4_crop_5arc, y, method = 'bilinear')
  writeRaster(
    r,
    file.path('results/intermediate', paste0('c4_crop_frac_globe_', rgns_str[i], 'Deg.tif')),
    overwrite = TRUE
  )

}
