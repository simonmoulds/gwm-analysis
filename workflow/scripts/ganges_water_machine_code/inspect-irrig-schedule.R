
library(tidyverse)
library(ncdf4)
library(raster)

## ## Pixels belonging to Indo-Gangetic Plain (Indus + Ganges)
## land_frac = raster(
##     file.path("../data/wfdei/ancils/WFD-EI-LandFraction2d_igp.nc")
## )
## template = raster(land_frac)

nc = nc_open("../data/wfdei/ancils/WFD-EI-LandFraction2d_igp.nc")
mask = ncvar_get(nc, "lsmask")
nc_close(nc)

nc = nc_open("../data/wfdei/ancils/jules_5pft_w_crops_irrig_schedule.nc")
sch = ncvar_get(nc, "irr_schedule")
lat = ncvar_get(nc, "lat")
lon = ncvar_get(nc, "lon")
nc_close(nc)

lat_ix = which(lat %in% 27.75)
lon_ix = which(lon %in% 76.25)

sch_pt = t(sch[lon_ix,lat_ix,,]) %>% as.data.frame

## 6: rainfed [should sum to zero] *OK*
sum(sch_pt[,6])
## 7: irrigated single *OK*
sum(sch_pt[,7])
## 8: irrigated double *OK*
sum(sch_pt[,8])
## 9: irrigated triple [should sum to zero] *OK*
sum(sch_pt[,9])
## 10: irrigated continuous [should sum to 366] *OK*
sum(sch_pt[,10])

nc = nc_open("../data/wfdei/ancils/jules_5pft_w_crops_veg_frac_1979_igp_wfdei.nc")
frac = ncvar_get(nc, "land_cover_lccs")
lat = ncvar_get(nc, "lat")
lon = ncvar_get(nc, "lon")
nc_close(nc)

lat_ix = which(lat %in% 27.75)
lon_ix = which(lon %in% 76.25)

for (i in 1:15) {
    frac[,,i] = frac[,,i] * mask
}
res <- matrix(frac, prod(dim(frac)[1:2]), dim(frac)[3])
frac_pt = t(frac[lon_ix,lat_ix,])

## r = template
## r[] = t(sch[,40:1,7,100]); plot(r)

## Now let's have a look at some output
nc = nc_open("~/JULES_output/u-ci496/JULES_vn6.1_irrig_pt.jules_1980.daily_hydrology.1980.nc")
irrig_water_pt = ncvar_get(nc, "irrig_water")
nc_close(nc)
plot(irrig_water_pt)

nc = nc_open("~/JULES_output/u-ci496/JULES_vn6.1_irrig.jules_1980.daily_hydrology.1980.nc")
lat = ncvar_get(nc, "latitude")
lon = ncvar_get(nc, "longitude")
irrig_water = ncvar_get(nc, "irrig_water")
nc_close(nc)
latlon_ix = which(lat %in% 27.75 & lon %in% 76.25)
plot(irrig_water[latlon_ix,])
points(irrig_water_pt, col="magenta")
