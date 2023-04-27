## Author : Simon Moulds
## Date   : August 2020

library(ncdf4)
library(magrittr)
library(dplyr)
library(raster)

datadir = "/home/sm510/JULES_India/data"

## grdnc = nc_open(file.path(datadir, "ancil", "India_0.250000Deg", "jules_land_frac_merit_india_0.250000Deg.nc"))
## lat = ncvar_get(grdnc, "lat")
## lon = ncvar_get(grdnc, "lon")
## lfrac = ncvar_get(grdnc, "land_frac")
## nc_close(grdnc)

## #################################### #
## Extract output data
## #################################### #

## N.B. JULES output stores space arrays as (y, x) and
## space-time arrays as (time, y, x). `ncdf4` appears
## to read data in reverse order - e.g. (x, y, time)

outnc = nc_open(file.path(datadir, "output", "jules.Base.nc"))
lat = ncvar_get(outnc, "latitude") %>% t 
lon = ncvar_get(outnc, "longitude") %>% t
time = ncvar_get(outnc, "time")
runoff = ncvar_get(outnc, "runoff") %>% aperm(c(2, 1, 3))
et = ncvar_get(outnc, "esoil_gb") %>% aperm(c(2, 1, 3))
prec = ncvar_get(outnc, "precip") %>% aperm(c(2, 1, 3))
nc_close(outnc)

## create raster template
pts = data.frame(x=as.numeric(lon), y=as.numeric(lat), z=1)
r = rasterFromXYZ(pts)
projection(r) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
area = area(r) %>% as.matrix

## Extract some time series for a random point:
xcoord = 78.875
ycoord = 29.125

pt = SpatialPoints(
    data.frame(x=xcoord, y=ycoord),
    CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
)

xidx = which(lat[,1] %in% ycoord)
yidx = which(lon[1,] %in% xcoord)

et_ts = et[yidx, xidx, ] * 86400
ro_ts = runoff[yidx, xidx, ] * 86400
pr_ts = prec[yidx, xidx, ] * 86400

## #################################### #
## Runoff ratio in every grid cell
## #################################### #

rr = apply(runoff, c(1,2), sum) / apply(prec, c(1,2), sum)
rr = sum(rr * area) / sum(area) # area weighted average

## #################################### #
## Comparison with MODIS ET data
## #################################### #

## Read MODIS ET data
fs = list.files(
    "~/data/earthengine/",
    "MODIS_NTSG_MOD16A2_105_A2012[0-9]{3}_ET.tif",
    full.names=TRUE
)
st = stack(fs)
st[st == 32767] = NA # Fill value, out of the Earth
st[st == 32766] = NA # Water body
st[st == 32765] = NA # Barren or sparsely vegetated
st[st == 32764] = NA # Permanent snow/ice
st[st == 32763] = NA # Permanent wetland
st[st == 32762] = NA # Urban or built-up
st[st == 32761] = NA # Unclassified

is_leapyear=function(year){
  #http://en.wikipedia.org/wiki/Leap_year
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

## Period over which ET values are aggregated in MODIS product
div = rep(8, length(fs))
if (is_leapyear(2012)) {
    div[length(fs)] = 6
} else {
    div[length(fs)] = 5
}

## Get mean daily ET by dividing by aggregation period and scaling factor
for (i in 1:nlayers(st)) {
    st[[i]] = st[[i]] / div[i] / 10
}

## Resample to coarser resolution
st = crop(st, extent(r))
fact = floor(res(r)[1] / res(st)[1])
st = aggregate(st, fact=fact, fun=mean, na.rm=TRUE)

## Extract ET data for a single point
ts = st[pt] %>% unname %>%  as.numeric
