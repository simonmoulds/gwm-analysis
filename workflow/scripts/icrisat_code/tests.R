## Author : Simon Moulds
## Date   : Sep 2021

library(tidyverse)
library(raster)

## continuous maps should currently be zero 
r = raster(
    "../data/output_maps/continuous_canal_india_2005.tif"
) %>% plot
r = raster(
    "../data/output_maps/continuous_other_sources_india_2005.tif"
) %>% plot
r = raster(
    "../data/output_maps/continuous_other_wells_india_2005.tif"
) %>% plot
r = raster(
    "../data/output_maps/continuous_tanks_india_2005.tif"
) %>% plot
r = raster(
    "../data/output_maps/continuous_tubewells_india_2005.tif"
) %>% plot

## Net/gross cropland/irrigated area
r = raster(
    "../data/output_maps/gross_cropland_area_india_2005.tif"
) %>% plot
r = raster(
    "../data/output_maps/gross_irrigated_area_india_2005.tif"
) %>% plot
r = raster(
    "../data/output_maps/net_cropland_area_india_2005.tif"
) %>% plot
r = raster(
    "../data/output_maps/net_irrigated_area_india_2005.tif"
) %>% plot

## Kharif
r = raster(
    "../data/output_maps/kharif_canal_india_2005.tif"
) %>% plot
r = raster(
    "../data/output_maps/kharif_other_sources_india_2005.tif
") %>% plot
r = raster(
    "../data/output_maps/kharif_other_wells_india_2005.tif"
) %>% plot
r = raster(
    "../data/output_maps/kharif_tanks_india_2005.tif"
) %>% plot
r = raster(
    "../data/output_maps/kharif_tubewells_india_2005.tif
") %>% plot
r = raster(
    "../data/output_maps/kharif_tubewells_india_2005.tif
") %>% plot

## Rabi
r = raster(
    "../data/output_maps/rabi_canal_india_2005.tif"
) %>% plot
r = raster(
    "../data/output_maps/rabi_other_sources_india_2005.tif"
) %>% plot
r = raster(
    "../data/output_maps/rabi_other_wells_india_2005.tif"
) %>% plot
r = raster(
    "../data/output_maps/rabi_tanks_india_2005.tif"
) %>% plot
r = raster(
    "../data/output_maps/rabi_tubewells_india_2005.tif"
) %>% plot

r = raster(
    "../data/output_maps/zaid_canal_india_2005.tif"
) %>% plot
r = raster(
    "../data/output_maps/zaid_other_sources_india_2005.tif"
) %>% plot
r = raster(
    "../data/output_maps/zaid_other_wells_india_2005.tif"
) %>% plot
r = raster(
    "../data/output_maps/zaid_tanks_india_2005.tif"
) %>% plot
r = raster(
    "../data/output_maps/zaid_tubewells_india_2005.tif"
) %>% plot
