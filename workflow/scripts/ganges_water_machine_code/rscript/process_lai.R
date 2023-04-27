## Author : Simon Moulds
## Date   : Dec 2020

library(raster)
library(magrittr)
library(tidyverse)

datadir = Sys.getenv("DATADIR")

lai_datadir = file.path(datadir, "aux/lai")

yrs = 2014:2019
nyrs = length(yrs)

get_lai_maps = function(lc) {
    lai = list()
    for (i in 1:nyrs) {
        yr = yrs[i]
        ptn = paste0("lai_", lc, "_", yr, "_[0-9]{2}_[0-9]{2}_igp_0.041667Deg.tif")
        fs = list.files(
            lai_datadir,
            pattern=ptn,
            full.names=TRUE
        )
        st = stack(fs)
        lai[[paste0("x", yr)]] = st
    }
    lai
}

get_avg_lai_maps = function(lai_list) {
    avg_lai = list()
    for (i in 1:36) {
        maps = list()
        for (j in 1:length(lai_list)) {
            maps[[j]] = lai_list[[j]][[i]]
        }
        avg = stackApply(stack(maps), indices=rep(1, nyrs), fun=mean)
        avg_lai[[i]] = avg
    }
    avg_lai    
}

write_lai_maps = function(x, nm, land, fill) {
    missing_ix = is.na(x) & !is.na(land)
    for (i in 1:nlayers(x)) {    
        x[[i]][missing_ix[[i]]] = fill[missing_ix[[i]]]
        writeRaster(
            x[[i]],
            file.path(lai_datadir, paste0("lai_", nm, "_avg_", i, "_igp_0.041667Deg.tif")),
            overwrite=TRUE
        )
    }    
}

## Create a map showing the minimum LAI across all
## cells - use for filling gaps
print("Creating minimum LAI map...")
mn_list = list()
lus = c("natural", "rainfed_cropland", "irrigated_cropland_1", "irrigated_cropland_2", "irrigated_cropland_3", "irrigated_cropland_c", "fallow_cropland")
for (lu in lus) {
    st = stack(
        list.files(
            lai_datadir,
            pattern=paste0("lai_", lu, "_[0-9]{4}_[0-9]{2}_[0-9]{2}_igp_0.041667Deg.tif"),
            full.name=TRUE
        )
    )
    mn = stackApply(st, indices=rep(1, nlayers(st)), fun=min)
    mn_list[[lu]] = mn
}
st = stack(mn_list)
mn = stackApply(st, indices=rep(1, nlayers(st)), fun=min)
land = !is.na(mn)
land[is.na(mn)] = NA

## Combined LAI:
print("Creating (natural + agri) LAI maps...")
combined_lai = get_lai_maps("combined")
avg_combined_lai = get_avg_lai_maps(combined_lai) %>% stack
write_lai_maps(avg_combined_lai, "combined", land, mn)

## Natural LAI:
print("Creating average natural LAI maps...")
natural_lai = get_lai_maps("natural")
avg_natural_lai = get_avg_lai_maps(natural_lai) %>% stack
write_lai_maps(avg_natural_lai, "natural", land, mn)

## Rainfed cropland
print("Creating average rainfed cropland LAI maps...")
rainfed_lai = get_lai_maps("rainfed_cropland")
avg_rainfed_lai = get_avg_lai_maps(rainfed_lai) %>% stack
write_lai_maps(avg_rainfed_lai, "rainfed_cropland", land, mn)

## Irrigated-single
print("Creating average irrigated cropland (single) LAI maps...")
irrigated_single_lai = get_lai_maps("irrigated_cropland_1")
avg_irrigated_single_lai = get_avg_lai_maps(irrigated_single_lai) %>% stack
write_lai_maps(avg_irrigated_single_lai, "irrigated_cropland_1", land, mn)

## Irrigated-double
print("Creating average irrigated cropland (double) LAI maps...")
irrigated_double_lai = get_lai_maps("irrigated_cropland_2")
avg_irrigated_double_lai = get_avg_lai_maps(irrigated_double_lai) %>% stack
write_lai_maps(avg_irrigated_double_lai, "irrigated_cropland_2", land, mn)

## Irrigated-triple
print("Creating average irrigated cropland (triple) LAI maps...")
irrigated_triple_lai = get_lai_maps("irrigated_cropland_3")
avg_irrigated_triple_lai = get_avg_lai_maps(irrigated_triple_lai) %>% stack
write_lai_maps(avg_irrigated_triple_lai, "irrigated_cropland_3", land, mn)

## Irrigated-continuous
print("Creating average irrigated cropland (continuous) LAI maps...")
irrigated_continuous_lai = get_lai_maps("irrigated_cropland_c")
avg_irrigated_continuous_lai = get_avg_lai_maps(irrigated_continuous_lai) %>% stack
write_lai_maps(avg_irrigated_continuous_lai, "irrigated_cropland_c", land, mn)

## Fallow
print("Creating average fallow cropland LAI maps...")
fallow_lai = get_lai_maps("fallow_cropland")
avg_fallow_lai = get_avg_lai_maps(fallow_lai) %>% stack
write_lai_maps(avg_fallow_lai, "fallow_cropland", land, mn)


## pt = cellFromXY(st[[1]], c(79,29))
## plot(avg_rainfed_lai[pt][1,], ylim=c(0,5))
## lines(avg_irrigated_single_lai[pt][1,])
## lines(avg_irrigated_double_lai[pt][1,])
## lines(avg_irrigated_triple_lai[pt][1,])

## meanr = sapply(unstack(avg_rainfed_lai), FUN=function(x) mean(getValues(x), na.rm=TRUE))
## mean3 = sapply(unstack(avg_irrigated_triple_lai), FUN=function(x) mean(getValues(x), na.rm=TRUE))
## mean2 = sapply(unstack(avg_irrigated_double_lai), FUN=function(x) mean(getValues(x), na.rm=TRUE))
## mean1 = sapply(unstack(avg_irrigated_single_lai), FUN=function(x) mean(getValues(x), na.rm=TRUE))
## plot(meanr, ylim=c(0,5))
## lines(mean1, col="red")
## lines(mean2, col="blue")
## lines(mean3, col="green")
