## Author : Simon Moulds
## Date   : April 2021

## library(ncdf4)
## library(ncdf4.helpers)
library(raster)
## library(rasterVis)
library(RColorBrewer)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgdal)
## library(spatialEco)
library(tidyverse)
library(patchwork)
## library(cowplot)
## ## Use `zyp` to get estimate of intercept as well as slope
## library(zyp)

options(stringsAsFactors = FALSE)

## Extract configuration info
if (sys.nframe() == 0L) {
  args = commandArgs(trailingOnly=TRUE)
  ## outputdir = args[1]
  args = commandArgs()
  m <- regexpr("(?<=^--file=).+", args, perl=TRUE)
  cwd <- dirname(regmatches(args, m))
}
## Load custom utilities
source(file.path(cwd, "utils.R"))

## if (!dir.exists(outputdir))
##   dir.create(outputdir, recursive = TRUE)

## resourcedir = 'resources'
## overwrite = FALSE

## TODO sketch out plot ideas using R, but ultimately implement them in Python

## TODO (additional plot ideas)
## * Mean water demand in Kharif and Rabi
## * Scatter plot of Kharif/rabi precip against mean dS
## * Scatter plot of precipitation anomaly against annual dS in east/west basin regions
## * [spatial] Mean dS under current and restored scenarios
## * [spatial] dS trend under current and restored scenarios
## * Barplot showing irrigation volume under current and restored scenarios

## TODO (JULES modelling)
## * Decide whether current and restored need different JULES configs (more summertime irrigation during kharif)

## ####################################################### ##
## ####################################################### ##
##
## Preamble
##
## ####################################################### ##
## ####################################################### ##

ganges_basin = raster("resources/land.nc")
ganges_basin[ganges_basin==0] = NA

## Identify canal command area
cmd_area = st_read("resources/irrigation/command_areas.shp")
## Pakistan and West Bengal are outside of the study region
pakistan_ids = c(1:5, 8:14, 16, 20, 37, 41)
west_bengal_ids = c(31, 34, 42, 43, 44)
india_cmd_area =
  cmd_area %>%
  filter(!ID %in% c(pakistan_ids, west_bengal_ids))
## Convert to sp for plotting with raster
india_cmd_area_sp = india_cmd_area %>% as_Spatial()
india_cmd_area = rasterize(india_cmd_area_sp, ganges_basin, field = "ID")
india_cmd_area[india_cmd_area > 0] = 1
india_cmd_area_poly = rasterToPolygons(
  india_cmd_area, dissolve = TRUE
) %>% st_as_sf()

## Create raster images of the east and west portion of the basin
## TODO check this is consistent with the literature
ew_divide = colFromX(india_cmd_area, 79.125)
india_cmd_area_west = india_cmd_area
india_cmd_area_west[,ew_divide:160] = NA
india_cmd_area_east = india_cmd_area
india_cmd_area_east[,1:(ew_divide - 1)] = NA

## Create a list of basin regions which we can loop through
basin_regions <- list(
  igp=india_cmd_area,
  igp_east = india_cmd_area_east,
  igp_west = india_cmd_area_west
)
## grid_cell_area <- raster::area(india_cmd_area) # km2

## ## Create directories for output
## historical_analysis_dir <- file.path(outputdir, "historical")
## current_canal_analysis_dir <- file.path(outputdir, "current_canal")
## restored_canal_analysis_dir <- file.path(outputdir, "restored_canal")

## For spatial plots
world <- ne_countries(scale="small", continent="asia", returnclass="sf")
india <-
  st_read(file.path("resources", "GIS-shapefiles-1966base", "india70again.shp")) %>%
  as_Spatial()

india$ID = 1
india = rasterize(india, ganges_basin, field = "ID")
india[india_cmd_area == 1] = 1

## Variables for plotting
axis_title_size = 6
axis_label_size = 6
axis_label_size_small = 6
legend_label_size = 6
legend_title_size = 6
tag_label_size = 6
strip_label_size = 6

## ####################################################### ##
## ####################################################### ##
##
## Historical time series
##
## ####################################################### ##
## ####################################################### ##

overwrite = TRUE
years = 1979:2013
seasons = c("kharif", "rabi", "zaid", "continuous")
types = c("gw", "sw", "total")
basins = names(basin_regions)

## compute_basin_total = function(fn, basin) {
##   r = raster(fn)
##   r = resample(r, ganges_basin, method="ngb")
##   r = r / 1000 # m -> km
##   basin_area = basin * grid_cell_area
##   r = r * basin_area
##   basin_sum = sum(getValues(r), na.rm=TRUE) # km3
##   basin_area = sum(getValues(basin_area), na.rm=TRUE)
##   basin_avg = (basin_sum / (basin_area)) * 1000
##   basin_avg
## }

## ## TODO make this its own script because it takes so long
## ## overwrite = FALSE
## ## if (overwrite | !file.exists("historical_irrigation_demand_ts.rds")) {
## output_list = list()
## for (m in 1:length(basins)) {
##   ## basin = basins[m]; print(basin)
##   print(sprintf("Extracting data for basin %s", basin))
##   pb = txtProgressBar(min = 0, max = length(years), initial = 0)
##   for (i in 1:length(years)) {
##     year <- years[i]
##     fn <- file.path(
##       historical_analysis_dir,
##       paste0("annual_precip_historical_", year, "_noirrig.tif")
##     )
##     precip_basin_sum <- compute_basin_total(fn, basin_regions[[basin]])
##     et_basin_sum <- compute_basin_total(
##       fn %>% gsub("precip", "et", .),
##       basin_regions[[basin]]
##     )
##     surf_roff_basin_sum <- compute_basin_total(
##       fn %>% gsub("precip", "surf_roff", .),
##       basin_regions[[basin]]
##     )
##     sub_surf_roff_basin_sum <- compute_basin_total(
##       fn %>% gsub("precip", "sub_surf_roff", .),
##       basin_regions[[basin]]
##     )
##     for (j in 1:length(seasons)) {
##       season <- seasons[j]
##       for (k in 1:length(types)) {
##         type <- types[k]
##         fn <- file.path(
##           historical_analysis_dir,
##           paste0(
##             season, "_", type,
##             "_irrigation_historical_",
##             year, "_irrig.tif"
##           )
##         )
##         irrigation_basin_sum <- compute_basin_total(fn, basin_regions[[basin]])
##         output_list[[length(output_list) + 1]] <- data.frame(
##           year = year,
##           season = season,
##           types = type,
##           basin = basin,
##           irrigation = irrigation_basin_sum,
##           precip = precip_basin_sum,
##           et = et_basin_sum,
##           surf_roff = surf_roff_basin_sum,
##           sub_surf_roff = sub_surf_roff_basin_sum
##         )
##       }
##     }
##     setTxtProgressBar(pb, i)
##   }
##   close(pb)
## }
## historical_ts = do.call("rbind", output_list) %>% as_tibble()
## saveRDS(output, "historical_irrigation_demand_ts.rds")
## } else {
##   historical_ts = readRDS("historical_irrigation_demand_ts.rds")
## }

## ####################################################### ##
## ####################################################### ##
##
## Historical spatial trends
##
## ####################################################### ##
## ####################################################### ##

## Load some maps for plotting
sw_kharif_maps = list()
gw_kharif_maps = list()
sw_rabi_maps = list()
gw_rabi_maps = list()
precip_maps = list()
pet_maps = list()
aridity_maps = list()

for (i in 1:length(years)) {

  sw_kharif_total <- list.files(
    "resources/irrigated_area_maps/",
    pattern = paste0("icrisat_kharif_(canal|other_sources|tanks)_", years[i], "_india_0.500000Deg.tif"),
    full.names = TRUE
  ) %>%
    raster::stack() %>%
    stackApply(indices = c(1, 1, 1), fun = sum) %>%
    resample(india_cmd_area) %>%
    `*`(india_cmd_area)

  gw_kharif_total <- list.files(
    "resources/irrigated_area_maps/",
    pattern = paste0("icrisat_kharif_(other_wells|tubewells)_", years[i], "_india_0.500000Deg.tif"),
    full.names = TRUE
  ) %>%
    raster::stack() %>%
    stackApply(indices = c(1, 1), fun = sum) %>%
    resample(india_cmd_area) %>%
    `*`(india_cmd_area)

  sw_rabi_total <- list.files(
    "resources/irrigated_area_maps/",
    pattern = paste0("icrisat_rabi_(canal|other_sources|tanks)_", years[i], "_india_0.500000Deg.tif"),
    full.names = TRUE
  ) %>%
    raster::stack() %>%
    stackApply(indices = c(1, 1), fun = sum) %>%
    resample(india_cmd_area) %>%
    `*`(india_cmd_area)

  gw_rabi_total <- list.files(
    "resources/irrigated_area_maps/",
    pattern = paste0("icrisat_rabi_(other_wells|tubewells)_", years[i], "_india_0.500000Deg.tif"),
    full.names = TRUE
  ) %>%
    raster::stack() %>%
    stackApply(indices = c(1, 1), fun = sum) %>%
    resample(india_cmd_area) %>%
    `*`(india_cmd_area)

  ## precip_total = list.files(
  ##   "results/JULES_vn6.1_irrig",
  ##   pattern = paste0("annual_precip_historical_", years[i], "_current.tif"),
  ##   full.names = TRUE
  ## ) %>%
  ##   raster::raster() %>%
  ##   resample(india_cmd_area) %>%
  ##   `*`(india_cmd_area)

  ## pet_total = list.files(
  ##   "results/JULES_vn6.1_irrig_current",
  ##   pattern = paste0("annual_et_historical_", years[i], "_current.tif"),
  ##   full.names = TRUE
  ## ) %>%
  ##   raster::raster() %>%
  ##   resample(india_cmd_area) %>%
  ##   `*`(india_cmd_area)

  ## aridity_index = precip_total / pet_total

  sw_kharif_maps[[i]] = sw_kharif_total * raster::area(sw_kharif_total)
  gw_kharif_maps[[i]] = gw_kharif_total * raster::area(gw_kharif_total)
  sw_rabi_maps[[i]] = sw_rabi_total * raster::area(sw_rabi_total)
  gw_rabi_maps[[i]] = gw_rabi_total * raster::area(gw_rabi_total)
  ## precip_maps[[i]] = precip_total
  ## pet_maps[[i]] = pet_total
  ## aridity_maps[[i]] = aridity_index
}

## Promote to RasterStack
sw_kharif_maps = stack(sw_kharif_maps)
gw_kharif_maps = stack(gw_kharif_maps)
sw_rabi_maps = stack(sw_rabi_maps)
gw_rabi_maps = stack(gw_rabi_maps)
## precip_maps = stack(precip_maps)
## pet_maps = stack(pet_maps)
## aridity_maps = stack(aridity_maps)

## TODO move above to a separate script and save input datasets as RData file

## ####################################################### ##
## ####################################################### ##
##
## Figure 2: Historical demand-side changes
##
## ####################################################### ##
## ####################################################### ##

current_canal_area = raster(
  file.path(
    "results/irrigated_area_maps",
    "icrisat_kharif_canal_2010_india_0.500000Deg_current_canal.tif"
  )
)
current_canal_area = resample(current_canal_area, india, method = "ngb")
current_canal_area = current_canal_area * india_cmd_area
p1 <- myplotfun1(current_canal_area)
labelfun <- function(x) { ifelse((x %% 0.1) == 0, x, "") }
breaks <- seq(0, 0.6, by=0.01)
nbreaks <- length(breaks)
p1 <- p1 +
  scale_fill_stepsn(
    colours = RColorBrewer::brewer.pal(9, "Blues"),
    breaks = breaks,
    limits = c(min(breaks), max(breaks)),
    labels = labelfun,
    na.value = "grey"
  ) +
  theme(
    legend.position = "bottom"
  ) +
  guides(
    fill = guide_bins(
      title = expression(Fractional~area), #Depth~(m)),
      ## title = expression(Depth~(m)),
      title.position = "top",
      axis = FALSE,
      keywidth = grid::unit(fig1_keywidth / nbreaks, "npc"),
      keyheight = grid::unit(3, "mm")
    )
  )

current_gw_area = stack(
  list.files(
    "results/irrigated_area_maps",
    pattern = "icrisat_kharif_(other_wells|tubewells)_2010_india_0.500000Deg_current_canal.tif",
    full.names = TRUE
  )
) %>% stackApply(indices = c(1,1), fun = sum)
current_gw_area = resample(current_gw_area, india, method = "ngb")
current_gw_area = current_gw_area * india_cmd_area
p2 <- myplotfun1(current_gw_area)
labelfun <- function(x) { ifelse((x %% 0.1) == 0, x, "") }
breaks <- seq(0, 0.6, by=0.01)
nbreaks <- length(breaks)
p2 <- p2 +
  scale_fill_stepsn(
    colours = RColorBrewer::brewer.pal(9, "Blues"),
    breaks = breaks,
    limits = c(min(breaks), max(breaks)),
    labels = labelfun,
    na.value = "grey"
  ) +
  theme(
    legend.position = "bottom"
  ) +
  guides(
    fill = guide_bins(
      title = expression(Fractional~area), #Depth~(m)),
      ## title = expression(Depth~(m)),
      title.position = "top",
      axis = FALSE,
      keywidth = grid::unit(fig1_keywidth / nbreaks, "npc"),
      keyheight = grid::unit(3, "mm")
    )
  )

trend <- raster.kendall(sw_kharif_maps, p.value = TRUE)
slope <- trend$slope
pval <- trend$p.value
signif = pval <= 0.05
signif_pts = as(signif, "SpatialPointsDataFrame")
signif_pts = signif_pts[signif_pts$layer > 0,]
signif_pts = st_as_sf(signif_pts)
## slope = slope * 1000 # meter/year -> mm/year
p3 <- myplotfun1(slope)
labelfun <- function(x) { ifelse((x %% 1) == 0, x, "") }
breaks <- seq(-5, 3.5, 0.05)
nbreaks <- length(breaks)
rdbu_pal = RColorBrewer::brewer.pal(9, "RdBu")
p3 <- p3 +
  geom_sf(data = signif_pts, size = 0.01, shape = 20) +
  scale_fill_stepsn(
    colours = rdbu_pal,
    breaks = breaks,
    values = scales::rescale(c(-5, 0, 3.5)),
    limits = c(min(breaks), max(breaks)),
    labels = labelfun,
    na.value = "grey"
  ) +
  theme(
    legend.position = "bottom"
  ) +
  guides(
    fill = guide_bins(
      title = expression(Area~trend~(km^2~y^-1)),
      title.position = "top",
      axis = FALSE,
      keywidth = grid::unit(fig1_keywidth / nbreaks, "npc"),
      keyheight = grid::unit(3, "mm")
    )
  )

trend <- raster.kendall(gw_kharif_maps, p.value = TRUE)
## st <- st * india_cmd_area
slope <- trend$slope
pval <- trend$p.value
signif = pval <= 0.05
signif_pts = as(signif, "SpatialPointsDataFrame")
signif_pts = signif_pts[signif_pts$layer > 0,]
signif_pts = st_as_sf(signif_pts)
## slope = slope * 1000 # meter/year -> mm/year
p4 <- myplotfun1(slope)
labelfun <- function(x) { ifelse((x %% 1) == 0, x, "") }
breaks <- seq(0, 7.5, 0.05)
nbreaks <- length(breaks)
## rdbu_pal = RColorBrewer::brewer.pal(9, "Blues")
rdbu_pal = RColorBrewer::brewer.pal(9, "RdBu")[5:9]
p4 <- p4 +
  geom_sf(data = signif_pts, size = 0.01, shape = 20) +
  scale_fill_stepsn(
    colours = rdbu_pal,
    breaks = breaks,
    values = scales::rescale(c(0, 0, 7.5)),
    limits = c(min(breaks), max(breaks)),
    labels = labelfun,
    na.value = "grey"
  ) +
  theme(
    legend.position = "bottom"
  ) +
  guides(
    fill = guide_bins(
      title = expression(Area~trend~(km^2~y^-1)),
      title.position = "top",
      axis = FALSE,
      keywidth = grid::unit(fig1_keywidth / nbreaks, "npc"),
      keyheight = grid::unit(3, "mm")
    )
  )

## TODO kharif gw/sw stack plot
historical_ts <- readRDS("results/historical_irrigation_demand_ts.rds")
x <-
  historical_ts %>%
  filter(season %in% "kharif" & types %in% c("gw", "sw")) %>%
  dplyr::select(-(precip:sub_surf_roff)) %>%
  rename(volume = irrigation)

p5 <- myplotfun4(x %>% filter(basin %in% "igp"))
p6 <- myplotfun4(x %>% filter(basin %in% "igp_east"))
p6 <- p6 + theme(axis.title.y = element_blank(),
                 axis.title.x = element_blank())
p7 <- myplotfun4(x %>% filter(basin %in% "igp_west"))
p7 <- p7 + theme(axis.title.y = element_blank())

p12 <-
  p1 + p2 + plot_layout(ncol = 2, guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, 0, -5, 0)
  )

## ggsave("results/figure_test.png", width = 6, height = 6, units = "in")

## p3 <- p3 + theme(legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(-10, 0, -5, 0))
## p4 <- p4 + theme(legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(-10, 0, -5, 0))
p34 <-
  p3 + p4 + plot_layout(ncol = 2, guides = "keep") &
  theme(
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, 0, -5, 0)
  )

p67 <- p6 + p7 + plot_layout(nrow = 2)

p12$patches$plots[[1]] =
  p12$patches$plots[[1]] +
  labs(tag = "a") +
  theme(plot.tag.position = c(0.14, 1.035),
        plot.tag = element_text(size = tag_label_size, face="bold"))
p12 = p12 +
  labs(tag = "b") +
  theme(plot.tag.position = c(0.14, 1),
        plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
p34$patches$plots[[1]] =
  p34$patches$plots[[1]] +
  labs(tag = "c") +
  theme(plot.tag.position = c(0.14, 1.035),
        plot.tag = element_text(size = tag_label_size, face="bold"))
p34 = p34 +
  labs(tag = "d") +
  theme(plot.tag.position = c(0.14, 1),
        plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

p67$patches$plots[[1]] =
  p67$patches$plots[[1]] +
  labs(tag = "f") +
  theme(plot.tag.position = c(0.14, 1.06),
        plot.tag = element_text(size = tag_label_size, face="bold"))

p67 = p67 +
  labs(tag = "g") +
  theme(plot.tag.position = c(0.14, 1.02),
        plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

p567 <-
  p5 + p67 + plot_layout(ncol = 2, guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, 0, -5, 0)
  )

p567$patches$plots[[1]] =
  p567$patches$plots[[1]] +
  labs(tag = "e") +
  theme(plot.tag.position = c(0.14, 1.025),
        plot.tag = element_text(size = tag_label_size, face="bold"))

## plot_grid(p12, p34, p567, nrow=3)
## aligned = align_plots(p34, p567, align = "v")
fig2 <- plot_grid(p12, p34, p567, nrow=3, align = "v")
ggsave("results/fig/figure2.png", width = 6, height = 7.5, units = "in")

## Rabi - same as above plot, but put in supplementary material

## ## ####################################################### ##
## ## ####################################################### ##
## ##
## ## Historical change in storage
## ##
## ## ####################################################### ##
## ## ####################################################### ##

## years = 1979:2013
## output_map_list = list()
## pb = txtProgressBar(min = 0, max = length(years), initial = 0)
## for (i in 1:(length(years)-1)) {
##   year = years[i]
##   recharge_fn = file.path(
##     historical_analysis_dir,
##     sprintf("recharge_historical_%d_irrig.tif", year)
##   )
##   recharge_map = raster(recharge_fn)
##   abstraction_fn = file.path(
##     historical_analysis_dir,
##     sprintf("abstraction_historical_%d_irrig.tif", year)
##   )
##   abstraction_map = raster(abstraction_fn)
##   dS_map = recharge_map - abstraction_map
##   dS_map = resample(dS_map, india_cmd_area)
##   dS_map = dS_map * india_cmd_area
##   output_map_list[[length(output_map_list) + 1]] = dS_map
##   setTxtProgressBar(pb, i)
## }
## close(pb)

## ## 2a Mean annual net storage change (recharge - abstraction)
## st = stack(output_map_list)
## dS_mean = stackApply(st, indices=rep(1, nlayers(st)), fun=mean)
## p1 <- myplotfun1(dS_mean)
## ## p1 <- p1 +
## ##   scale_fill_stepsn(
## ##     colours = RColorBrewer::brewer.pal(9, "Blues"), # TODO change color scheme
## ##     ## breaks = seq(0, 0.5, by=0.025),
## ##     ## limits = c(0, 0.5),
## ##     ## labels = labelfun,
## ##     na.value = "grey"
## ##   ) +
## ##   theme(legend.key.height = unit(0.2, "cm"), legend.key.width = unit(0.4, "cm"))
## labelfun <- function(x) { ifelse((x %% 0.1) == 0, x, "") }
## breaks <- seq(-0.4, 0.6, 0.01)
## nbreaks <- length(breaks)
## rdbu_pal = RColorBrewer::brewer.pal(9, "RdBu")
## p1 <- p1 +
##   ## geom_sf(data = signif_pts, size = 0.01, shape = 20) +
##   scale_fill_stepsn(
##     colours = rdbu_pal,
##     breaks = breaks,
##     values = scales::rescale(c(-0.4, 0, 0.6)),
##     limits = c(min(breaks), max(breaks)),
##     labels = labelfun,
##     na.value = "grey"
##   ) +
##   theme(
##     legend.position = "bottom"
##   ) +
##   guides(
##     fill = guide_bins(
##       ## title = expression(Mean~Delta~S(m)),
##       title = "Mean annual change in storage (m)",
##       title.position = "top",
##       axis = FALSE,
##       keywidth = grid::unit(fig1_keywidth / nbreaks, "npc"),
##       keyheight = grid::unit(3, "mm")
##     )
##   )

## ## 2b Trend in annual net storage change (recharge - abstraction)
## st <- stack(output_map_list)
## st <- resample(st, india_cmd_area)
## st <- st * india_cmd_area
## trend <- raster.kendall(st, p.value = TRUE)
## slope <- trend$slope
## pval <- trend$p.value
## signif = pval <= 0.05
## signif_pts = as(signif, "SpatialPointsDataFrame")
## signif_pts = signif_pts[signif_pts$layer > 0,]
## signif_pts = st_as_sf(signif_pts)
## slope = slope * 1000 # meter/year -> mm/year
## p2 <- myplotfun1(slope)
## labelfun <- function(x) { ifelse((x %% 5) == 0, x, "") }
## breaks <- seq(-18, 8, 0.1)
## nbreaks <- length(breaks)
## rdbu_pal = RColorBrewer::brewer.pal(9, "RdBu")
## p2 <- p2 +
##   geom_sf(data = signif_pts, size = 0.01, shape = 20) +
##   scale_fill_stepsn(
##     colours = rdbu_pal,
##     breaks = breaks,
##     values = scales::rescale(c(-18, 0, 8)),
##     limits = c(min(breaks), max(breaks)),
##     labels = labelfun,
##     na.value = "grey"
##   ) +
##   theme(
##     legend.position = "bottom"
##   ) +
##   guides(
##     fill = guide_bins(
##       title = expression(Trend~"in"~annual~storage~change~(mm~y^-1)),
##       title.position = "top",
##       axis = FALSE,
##       keywidth = grid::unit(fig1_keywidth / nbreaks, "npc"),
##       keyheight = grid::unit(3, "mm")
##     )
##   )

## ## FIXME comment out once working in extract-time-series.R
## ## 2c/d/e
## ## output_map_list = list()
## output_list = list()
## pb = txtProgressBar(min = 0, max = length(years), initial = 0)
## for (i in 1:(length(years)-1)) {
##   year = years[i]
##   ## TODO precipitation output
##   recharge_fn = file.path(
##     historical_analysis_dir,
##     sprintf("recharge_historical_%d_irrig.tif", year)
##   )
##   recharge_map = raster(recharge_fn)
##   abstraction_fn = file.path(
##     historical_analysis_dir,
##     sprintf("abstraction_historical_%d_irrig.tif", year)
##   )
##   abstraction_map = raster(abstraction_fn)
##   dS_map = recharge_map - abstraction_map
##   dS_map = resample(dS_map, india_cmd_area)
##   ## output_map_list[[length(output_map_list) + 1]] = dS_map
##   for (j in 1:length(basins)) {
##     basin = basins[j]
##     recharge_sum = compute_basin_total(recharge_fn, basin_regions[[basin]])
##     abstraction_sum = compute_basin_total(abstraction_fn, basin_regions[[basin]])
##     dS = recharge_sum - abstraction_sum
##     output_list[[length(output_list) + 1]] = data.frame(
##       year=year,
##       basin = basin,
##       volume=dS
##     )
##   }
##   setTxtProgressBar(pb, i)
## }
## close(pb)

## output = do.call("rbind", output_list) %>% as_tibble()
## xx = output %>% arrange(year)

## xx_igp = xx %>% filter(basin %in% "igp")
## p3 <- myplotfun2(xx_igp)
## p3 <-
##   p3 +
##   scale_y_continuous(
##     name = "\u0394 Storage (m)",
##     limits = c(-0.3, 0.3)
##   )

## xx_igp_west = xx %>% filter(basin %in% "igp_east")
## p4 <- myplotfun2(xx_igp_west)
## p4 <-
##   p4 +
##   scale_y_continuous(
##     name = "\u0394 Storage (m)",
##     limits = c(-0.3, 0.3)
##   )

## xx_igp_east = xx %>% filter(basin %in% "igp_west")
## p5 <- myplotfun2(xx_igp_east)
## p5 <-
##   p5 +
##   scale_y_continuous(
##     name = "\u0394 Storage (m)",
##     limits = c(-0.3, 0.3)
##   )

## p4 <- p4 + theme(axis.text.y = element_blank())
## p5 <- p5 + theme(axis.text.y = element_blank())

## ## Combine panels
## p12 <-
##   p1 + p2 + plot_layout(ncol = 2, guides = "keep") &
##   theme(
##     legend.position = "bottom",
##     legend.margin = margin(0, 0, 0, 0),
##     legend.box.margin = margin(-10, 0, -5, 0)
##   )

## p45 <- p4 + p5 + plot_layout(nrow = 2)

## p12$patches$plots[[1]] =
##   p12$patches$plots[[1]] +
##   labs(tag = "a") +
##   theme(plot.tag.position = c(0.14, 1.035),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p12 = p12 +
##   labs(tag = "b") +
##   theme(plot.tag.position = c(0.14, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

## p45$patches$plots[[1]] =
##   p45$patches$plots[[1]] +
##   labs(tag = "d") +
##   theme(plot.tag.position = c(0.14, 1.06),
##         plot.tag = element_text(size = tag_label_size, face="bold"))

## p45 = p45 +
##   labs(tag = "e") +
##   theme(plot.tag.position = c(0.14, 1.02),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

## p345 <-
##   p3 + p45 + plot_layout(ncol = 2, guides = "collect") &
##   theme(
##     legend.position = "bottom",
##     legend.title = element_blank(),
##     legend.margin = margin(0, 0, 0, 0),
##     legend.box.margin = margin(-10, 0, -5, 0)
##   )

## p345$patches$plots[[1]] =
##   p345$patches$plots[[1]] +
##   labs(tag = "c") +
##   theme(plot.tag.position = c(0.14, 1.025),
##         plot.tag = element_text(size = tag_label_size, face="bold"))

## fig3 <- plot_grid(p12, p345, nrow=2, align = "v")
## ggsave("results/fig/figure3.png", width = 6, height = 5, units = "in")

## ## ####################################################### ##
## ## ####################################################### ##
## ##
## ## Activated Ganges water machine
## ##
## ## ####################################################### ##
## ## ####################################################### ##

## ## This is a counterfactual simulation to evaluate the potential
## ## impact of canal restoration on the historical water balace
## ## Use results from "JULES_vn6.1_irrig_current" to compare
## ## policies "current_canal" AND "restored_canal"

## current_canal_area = raster(
##   file.path(
##     "results/irrigated_area_maps",
##     "icrisat_kharif_canal_2010_india_0.500000Deg_current_canal.tif"
##   )
## )
## current_canal_area = resample(current_canal_area, india, method = "ngb")
## current_canal_area = current_canal_area * india_cmd_area

## restored_canal_area = raster(
##   file.path(
##     "results/irrigated_area_maps",
##     "icrisat_kharif_canal_2010_india_0.500000Deg_restored_canal.tif"
##   )
## )
## restored_canal_area = resample(restored_canal_area, india, method = "ngb")
## restored_canal_area = restored_canal_area * india_cmd_area

## max_canal_area <- max(cellStats(current_canal_area, max), cellStats(restored_canal_area, max))

## p1 <- myplotfun1(current_canal_area)
## labelfun <- function(x) { ifelse((x %% 0.1) == 0, x, "") }
## breaks <- seq(0, 0.75, by=0.01)
## nbreaks <- length(breaks)
## p1 <- p1 +
##   scale_fill_stepsn(
##     colours = RColorBrewer::brewer.pal(9, "Blues"),
##     breaks = breaks,
##     limits = c(min(breaks), max(breaks)),
##     labels = labelfun,
##     na.value = "grey"
##   ) +
##   theme(
##     legend.position = "bottom"
##   ) +
##   guides(
##     fill = guide_bins(
##       title = expression(Fractional~area), #Depth~(m)),
##       ## title = expression(Depth~(m)),
##       title.position = "top",
##       axis = FALSE,
##       keywidth = grid::unit(fig1_keywidth / nbreaks, "npc"),
##       keyheight = grid::unit(3, "mm")
##     )
##   )

## p2 <- myplotfun1(restored_canal_area)
## labelfun <- function(x) { ifelse((x %% 0.1) == 0, x, "") }
## breaks <- seq(0, 0.6, by=0.01)
## nbreaks <- length(breaks)
## p2 <- p2 +
##   scale_fill_stepsn(
##     colours = RColorBrewer::brewer.pal(9, "Blues"),
##     breaks = breaks,
##     limits = c(min(breaks), max(breaks)),
##     labels = labelfun,
##     na.value = "grey"
##   ) +
##   theme(
##     legend.position = "bottom"
##   ) +
##   guides(
##     fill = guide_bins(
##       title = expression(Fractional~canal~irrigated~area), #Depth~(m)),
##       ## title = expression(Depth~(m)),
##       title.position = "top",
##       axis = FALSE,
##       keywidth = grid::unit(fig1_keywidth / nbreaks, "npc"),
##       keyheight = grid::unit(3, "mm")
##     )
##   )
## ## plot(stack(r1,r2,r3))
## ## compareRaster(r2,r3,values=TRUE)
## ## r1 <- resample(r1, india_cmd_area)
## ## r1 <- r1 * india_cmd_area
## ## plot(r1)
## ## r2 <- resample(r2, india_cmd_area)
## ## r2 <- r2 * india_cmd_area
## ## plot(r2)
## ## r3 <- resample(r3, india_cmd_area)
## ## r3 <- r3 * india_cmd_area
## ## plot(r3)
## ## r4 <- resample(r4, india_cmd_area)
## ## r4 <- r4 * india_cmd_area
## ## plot(r4)

## ## current_canal_analysis_dir = "../data/analysis_old/current_canal"
## ## restored_canal_analysis_dir = "../data/analysis_old/restored_canal"

## output_list = list()
## current_output_map_list = list()
## restored_output_map_list = list()
## pb = txtProgressBar(min = 0, max = length(years) - 1, initial = 0)
## for (i in 1:(length(years)-1)) {
##   year = years[i]
##   ## Change in storage under current canal area
##   current_abstraction_fn = file.path(
##     "results/JULES_vn6.1_irrig_current",
##     sprintf("abstraction_current_canal_%s_irrig.tif", year)
##   )

##   current_recharge_fn = file.path(
##     "results/JULES_vn6.1_irrig_current",
##     sprintf("recharge_current_canal_%s_irrig.tif", year)
##   )
##   current_abstraction_map = raster(current_abstraction_fn)
##   current_recharge_map = raster(current_recharge_fn)
##   dS_current_map = current_recharge_map - current_abstraction_map
##   dS_current_map = resample(dS_current_map, india_cmd_area)
##   dS_current_map = dS_current_map * india_cmd_area

##   ## Change in storage under restored canal area
##   restored_abstraction_fn = file.path(
##     "results/JULES_vn6.1_irrig_current",
##     sprintf("abstraction_restored_canal_%s_irrig.tif", year)
##   )
##   restored_recharge_fn = file.path(
##     "results/JULES_vn6.1_irrig_current",
##     sprintf("recharge_restored_canal_%s_irrig.tif", year)
##   )
##   restored_abstraction_map = raster(restored_abstraction_fn)
##   restored_recharge_map = raster(restored_recharge_fn)
##   dS_restored_map = restored_recharge_map - restored_abstraction_map
##   dS_restored_map = resample(dS_restored_map, india_cmd_area)
##   dS_restored_map = dS_restored_map * india_cmd_area

##   current_output_map_list[[length(current_output_map_list) + 1]] = dS_current_map
##   restored_output_map_list[[length(restored_output_map_list) + 1]] = dS_restored_map

##   for (m in 1:length(basins)) {
##     basin = basins[m]
##     current_abstraction_sum = compute_basin_total(current_abstraction_fn, basin_regions[[basin]])
##     current_recharge_sum = compute_basin_total(current_recharge_fn, basin_regions[[basin]])
##     dS_current = current_recharge_sum - current_abstraction_sum
##     restored_recharge_sum = compute_basin_total(restored_recharge_fn, basin_regions[[basin]])
##     restored_abstraction_sum = compute_basin_total(restored_abstraction_fn, basin_regions[[basin]])
##     dS_restored = restored_recharge_sum - restored_abstraction_sum
##     ## Add to data frame
##     output_list[[length(output_list) + 1]] = data.frame(
##       year = year,
##       basin = basin,
##       policy = c("current", "restored"),
##       dS = c(dS_current, dS_restored),
##       abstraction = c(current_abstraction_sum, restored_abstraction_sum),
##       recharge = c(current_recharge_sum, restored_recharge_sum)
##     )
##   }
##   setTxtProgressBar(pb, i)
## }
## close(pb)

## output =
##   do.call("rbind", output_list) %>%
##   as_tibble() %>%
##   mutate(volume = recharge - abstraction)

## precip_mean = stackApply(precip_maps, rep(1, nlayers(precip_maps)), mean)
## aridity_mean = stackApply(aridity_maps, rep(1, nlayers(precip_maps)), mean)

## dS_current_mean =
##   raster::stack(current_output_map_list) %>%
##   stackApply(rep(1, length(current_output_map_list)), mean)

## dS_restored_mean =
##   raster::stack(restored_output_map_list) %>%
##   stackApply(rep(1, length(restored_output_map_list)), mean)

## ## irrigated area [same for restored/current]
## irr_area_fs = list.files(
##   "results/irrigated_area_maps",
##   pattern = "icrisat_kharif_(.*)_2010_india_0.500000Deg_current_canal.tif",
##   full.names = TRUE)
## irr_area = stack(irr_area_fs) %>% stackApply(rep(1, 5), sum)
## irr_area = resample(irr_area, india_cmd_area)
## irr_area = irr_area * india_cmd_area

## df = stack(list(aridity = aridity_mean,
##                 precip = precip_mean,
##                 dS_current = dS_current_mean,
##                 dS_restored = dS_restored_mean,
##                 irr_area = irr_area)) %>%
##   as.data.frame(xy = TRUE) %>%
##   na.omit() %>%
##   as.tibble() %>%
##   filter(irr_area >= 0.25)


## ## Size of the point could also be related to the increase in canal area in the grid cell
## sf <- 0.5
## df_sampled <- df[sample(1:nrow(df), floor(nrow(df) * sf)) %>% sort,]
## p3 <- myplotfun5(df_sampled)
## p3 <- p3 +
##   theme(
##     legend.position = "bottom"
##   ) +
##   scale_x_continuous(
##     name = "\u0394 Storage (m)",
##   ) +
##   scale_y_continuous(
##     name = "Aridity (-)"#,
##     ## limits = c(0.2, 1.4)
##   ) +
##   guides(
##     size = guide_legend(
##       title = expression(Fractional~irrigated~area),
##       title.position = "top"#,
##       ## axis = FALSE#,
##       ## keywidth = grid::unit(fig1_keywidth / nbreaks, "npc"),
##       ## keyheight = grid::unit(3, "mm")
##     )
##   )

## ## xx = output %>% pivot_longer(all_of(c("dS", "abstraction", "recharge"))) %>% arrange(year)
## xx =
##   scale_x_continuous(
##   scale_x_continuous(
##     name = "\u0394 Storage (m)",
##   )
##     name = "\u0394 Storage (m)",
##   )
##   output %>%
##   arrange(year) #%>%
##   ## mutate(policy = factor(policy, levels = c("current", "restored"), labels = c("Current", "Restored")))

## xx_igp = xx %>% filter(basin %in% "igp")
## p4 <- myplotfun3(xx_igp)
## p4 <- p4 +
##   scale_y_continuous(
##     name = "\u0394 Storage (m)",
##     limits = c(-0.3, 0.3)
##   )

## xx_igp_east = xx %>% filter(basin %in% "igp_east")
## p5 <- myplotfun3(xx_igp_east)
## p5 <- p5 +
##   scale_y_continuous(
##     name = "\u0394 Storage (m)",
##     limits = c(-0.3, 0.3)
##   )

## xx_igp_west = xx %>% filter(basin %in% "igp_west")
## p6 <- myplotfun3(xx_igp_west)
## p6 <- p6 +
##   scale_y_continuous(
##     name = "\u0394 Storage (m)",
##     limits = c(-0.3, 0.3)
##   )

## p1 <- p1 + theme(legend.position = "none", axis.text.x = element_blank())
## p12 <- p1 + p2 + plot_layout(nrow = 2)

## p56 <- p5 + p6 + plot_layout(nrow = 2)

## p12$patches$plots[[1]] =
##   p12$patches$plots[[1]] +
##   labs(tag = "a") +
##   theme(plot.tag.position = c(0.14, 1.035),
##         plot.tag = element_text(size = tag_label_size, face="bold"))
## p12 = p12 +
##   labs(tag = "b") +
##   theme(plot.tag.position = c(0.14, 1),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

## p56$patches$plots[[1]] =
##   p56$patches$plots[[1]] +
##   labs(tag = "f") +
##   theme(plot.tag.position = c(0.14, 1.06),
##         plot.tag = element_text(size = tag_label_size, face="bold"))

## p56 = p56 +
##   labs(tag = "g") +
##   theme(plot.tag.position = c(0.14, 1.02),
##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

## p123 <- p12 | p3 #+ #plot_layout(guides = "keep")

## p456 <-
##   p4 + p56 + plot_layout(ncol = 2, guides = "collect") &
##   theme(
##     legend.position = "bottom",
##     legend.title = element_blank(),
##     legend.margin = margin(0, 0, 0, 0),
##     legend.box.margin = margin(-10, 0, -5, 0)
##   )

## p123 = p123 +
##   labs(tag = "c") +
##   theme(plot.tag.position = c(0.14, 1.025),
##         plot.tag = element_text(size = tag_label_size, face="bold"))

## p456$patches$plots[[1]] =
##   p456$patches$plots[[1]] +
##   labs(tag = "d") +
##   theme(plot.tag.position = c(0.14, 1.025),
##         plot.tag = element_text(size = tag_label_size, face="bold"))

## fig4 <- plot_grid(p123, p456, nrow=2, align = "v", rel_heights = c(1, 0.75))
## ggsave("results/fig/figure4.png", width = 6, height = 7, units = "in")

## ## ####################################################### ##
## ## ####################################################### ##
## ##
## ## Figure X: Water balance under current and potential
## ## (climate change)
## ##
## ## ####################################################### ##
## ## ####################################################### ##

## ## The same as above, except using future climate change
## ## scenarios from Pradeep

## ## TODO, as a very last step once JULES model is finalized





## ## NOT USED:
## ##
## ## wb_vars = c("precip", "et", "surf_roff", "sub_surf_roff")
## ## xx_wb =
## ##   output %>%
## ##   group_by(year) %>%
## ##   summarize(across(all_of(wb_vars), mean)) %>%
## ##   mutate(precip = precip - mean(precip)) %>%

## ## xx_irr_source =
## ##   output %>%
## ##   group_by(year, types, basin) %>%
## ##   summarize(
## ##     across(all_of("irrigation"), sum)
## ##   ) %>%
## ##   mutate(types = factor(types, levels = c("total", "gw", "sw"), labels = c("Total", "GW", "SW")))

## ## xx_irr_season =
## ##   output %>%
## ##   filter(types %in% "total") %>%
## ##   group_by(year, season, basin) %>%
## ##   summarize(
## ##     across(all_of("irrigation"), sum)
## ##   ) %>%
## ##   filter(season %in% c("kharif", "rabi")) %>% # FIXME
## ##   mutate(season = factor(season, levels = c("kharif", "rabi"), labels = c("Kharif", "Rabi")))

## ## TODO not sure if these plots are useful:

## ## ## Precipitation anomaly
## ## p1 = ggplot(xx_wb, aes(x=year, y=precip)) +
## ##   geom_bar(stat = "identity") +
## ##   ## geom_point() +
## ##   scale_x_continuous(
## ##     name = "",
## ##     breaks = seq(1980, 2010, 10),
## ##     labels = c("1980", "1990", "2000", "2010"),
## ##     limits = c(1979, 2011)
## ##   ) +
## ##   scale_y_continuous(
## ##     name = "Precipitation anomaly (m)",
## ##     breaks = seq(-0.2, 0.2, 0.1),
## ##     labels = c("-0.2", "-0.1", "0", "0.1", "0.2"),
## ##     limits = c(-0.225, 0.275)
## ##   ) +
## ##   geom_hline(yintercept = 0, size = 0.25) +
## ##   theme_bw() +
## ##   theme(panel.grid = element_blank(),
## ##         axis.text.x = element_text(size = axis_label_size),
## ##         axis.text.y = element_text(size = axis_label_size),
## ##         axis.title.x = element_text(size = axis_title_size),
## ##         axis.title.y = element_text(size = axis_title_size),
## ##         legend.title = element_text(size = legend_title_size),
## ##         legend.text = element_text(size = legend_label_size))

## ## p2 = ggplot(xx_irr_source, aes(x=year, y=irrigation, color=types)) +
## ##   geom_line() +
## ##   scale_x_continuous(
## ##     name = "",
## ##     breaks = seq(1980, 2010, 10),
## ##     labels = c("1980", "1990", "2000", "2010"),
## ##     limits = c(1979, 2011)
## ##   ) +
## ##   scale_y_continuous(
## ##     name = "Irrigation (m)",
## ##     breaks = seq(0, 0.4, 0.1),
## ##     labels = c("0.0", "0.1", "0.2", "0.3", "0.4"),
## ##     limits = c(0, 0.4)
## ##   ) +
## ##   theme_bw() +
## ##   theme(panel.grid = element_blank(),
## ##         axis.text.x = element_text(size = axis_label_size),
## ##         axis.text.y = element_text(size = axis_label_size),
## ##         axis.title.x = element_text(size = axis_title_size),
## ##         axis.title.y = element_text(size = axis_title_size),
## ##         legend.title = element_text(size = legend_title_size),
## ##         legend.text = element_text(size = legend_label_size)) +
## ##   guides(color = guide_legend(title = "Irrigation source"))

## ## p3 = ggplot(xx_irr_season %>% filter(basin %in% "igp"), aes(x=year, y=irrigation, color=season)) +
## ##   geom_line() +
## ##   scale_x_continuous(
## ##     name = "",
## ##     breaks = seq(1980, 2010, 10),
## ##     labels = c("1980", "1990", "2000", "2010"),
## ##     limits = c(1979, 2011)
## ##   ) +
## ##   scale_y_continuous(
## ##     name = "Irrigation (m)",
## ##     breaks = seq(0, 0.4, 0.1),
## ##     labels = c("0.0", "0.1", "0.2", "0.3", "0.4"),
## ##     limits = c(0, 0.225)
## ##   ) +
## ##   theme_bw() +
## ##   theme(panel.grid = element_blank(),
## ##         axis.text.x = element_text(size = axis_label_size),
## ##         axis.text.y = element_text(size = axis_label_size),
## ##         axis.title.x = element_text(size = axis_title_size),
## ##         axis.title.y = element_text(size = axis_title_size),
## ##         legend.title = element_text(size = legend_title_size),
## ##         legend.text = element_text(size = legend_label_size)) +
## ##   guides(color = guide_legend(title= "Growing season"))

## ## ## Arrange plots
## ## p = p1 + p2 + p3 + plot_layout(ncol = 1, heights = c(1, 2, 2))
## ## p

## ## ggsave("fig1.png", plot = p, width = 6, height = 6, units = "in")

## ## current_canal_area = raster("../data/irrigated_area_maps/icrisat_kharif_canal_2010_india_0.500000Deg_current_canal.tif")
## ## restored_canal_area = raster("../data/irrigated_area_maps/icrisat_kharif_canal_2010_india_0.500000Deg_restored_canal.tif")

## ## st = stack(current_canal_area, restored_canal_area)
## ## st = resample(st, ganges_basin, method="bilinear")
## ## st = st * india_cmd_area
## ## st = st %>% setNames(c("Current", "Restored"))
## ## st_data = gplot_data(st)

## ## p2 = ggplot() +
## ##   geom_tile(data = st_data, aes(x = x, y = y, fill = value)) +
## ##   facet_wrap(~variable) +
## ##   ## geom_sf(data = india_cmd_area_poly, color = "black", fill = NA) +
## ##   xlim(69, 88.75) +
## ##   ylim(23.25, 33.25) +
## ##   scale_fill_stepsn(
## ##     colours = RColorBrewer::brewer.pal(9, "Blues"),
## ##     breaks = seq(0, 0.6, by=0.1),
## ##     limits = c(0, 0.6)
## ##   ) +
## ##   coord_sf() +
## ##   theme_bw() +
## ##   theme(panel.grid = element_blank(),
## ##         axis.title = element_blank(),
## ##         strip.background = element_blank(),
## ##         strip.text = element_text(size = strip_label_size),
## ##         axis.text.x = element_text(size = axis_label_size),
## ##         axis.text.y = element_text(size = axis_label_size),
## ##         axis.title.x = element_blank(),
## ##         axis.title.y = element_blank(),
## ##         legend.title = element_text(size = legend_title_size),
## ##         legend.text = element_text(size = legend_label_size)) +
## ##   guides(fill = guide_legend(title= "Fractional area"))
## ## p2

## ## p = p2 + p1 + plot_layout(ncol=1, nrow=2)
## ## p

## ## ggsave("fig2.png", plot = p, width = 6, height = 6, units = "in")

## ## stop()

## ## ## ####################################################### ##
## ## ## ####################################################### ##
## ## ##
## ## ## Figure 2: Historical irrigation demand - maps
## ## ##
## ## ## ####################################################### ##
## ## ## ####################################################### ##

## ## get_map_data <- function(season, source, scenario) {
## ##   ptn = paste0(season, "_", source, "_irrigation_", scenario, "_[0-9]{4}_irrig.tif")
## ##   maps = list.files(file.path("../data/analysis", scenario), pattern = ptn, full.names = TRUE)
## ##   maps = maps[3:32] # 1981-2010
## ##   st = stack(maps)
## ##   st = stackApply(st, indices = rep(1:6, each=5), fun=mean)
## ##   ## st = stackApply(st, indices = rep(1:3, each=10), fun=mean)
## ##   st = resample(st, ganges_basin, method="bilinear")
## ##   st = st * india_cmd_area
## ##   ## st = st * india
## ##   st_data = gplot_data(st)
## ## }

## ## rabi_gw_data = get_map_data("rabi", "gw", "historical")
## ## annual_total_data = get_map_data("annual", "total", "historical")

## ## p = ggplot() +
## ##   geom_tile(data = annual_total_data, aes(x = x, y = y, fill = value)) +
## ##   facet_wrap(~variable) +
## ##   ## geom_sf(data = india_cmd_area_poly, color = "black", fill = NA) +
## ##   xlim(69, 88.75) +
## ##   ylim(23.25, 33.25) +
## ##   scale_fill_gradient(low="white", high="blue", na.value = "lightgrey") +
## ##   coord_sf() +
## ##   theme_bw() +
## ##   theme(panel.grid = element_blank(),
## ##         axis.title = element_blank(),
## ##         strip.background = element_blank()#,
## ##         ## strip. = element_blank(),
## ##         )
## ## p

## ## ## Plot 1c
## ## maps = list.files(
## ##     historical_analysis_dir,
## ##     pattern = "dS_historical_[0-9]{4}_irrig.tif",
## ##     full.names = TRUE
## ## )

## ## maps = maps[3:32] # 1981-2010
## ## st = stack(maps)
## ## st = stackApply(st, indices = rep(1:6, each=5), fun=mean)
## ## ## st = stackApply(st, indices = rep(1:3, each=10), fun=mean)
## ## st = resample(st, ganges_basin, method="bilinear")
## ## st = st * india_cmd_area
## ## ## st = st * india
## ## plot(st)

## ## st_data = gplot_data(st)

## ## p = ggplot() +
## ##   geom_tile(data = st_data, aes(x = x, y = y, fill = value)) +
## ##   facet_wrap(~variable) +
## ##   ## geom_sf(data = india_cmd_area_poly, color = "black", fill = NA) +
## ##   ## scale_fill_gradient(low="white", high="blue") +
## ##   coord_sf()
## ## p

## ## NOT USED:
## ##
## ## ####################################################### ##
## ## ####################################################### ##
## ##
## ## Figure 1: Historical supply-side changes
## ##
## ## ####################################################### ##
## ## ####################################################### ##

## ## fig1_keywidth = 0.3
## ## ## fig1_keywidth =
## ## ##   grid::unit(fig1_keywidth / nbreaks, "npc")

## ## ## Figure 1: Hydroclimatic trends
## ## mean_precip = stackApply(precip_maps, indices = rep(1, nlayers(precip_maps)), mean)
## ## p1 <- myplotfun1(mean_precip)
## ## labelfun <- function(x) { ifelse((x %% 0.2) == 0, x, "") }
## ## breaks <- seq(0, 1.8, by=0.05)
## ## nbreaks <- length(breaks)
## ## p1 <- p1 +
## ##   scale_fill_stepsn(
## ##     colours = RColorBrewer::brewer.pal(9, "Blues"),
## ##     breaks = breaks,
## ##     limits = c(min(breaks), max(breaks)),
## ##     labels = labelfun,
## ##     na.value = "grey"
## ##   ) +
## ##   theme(
## ##     legend.position = "bottom"
## ##   ) +
## ##   guides(
## ##     fill = guide_bins(
## ##       title = expression(Depth~(m)),
## ##       title.position = "top",
## ##       axis = FALSE,
## ##       keywidth = grid::unit(fig1_keywidth / nbreaks, "npc"),
## ##       keyheight = grid::unit(3, "mm")
## ##     )
## ##   )
## ## p1

## ## mean_pet = stackApply(aridity_maps, indices = rep(1, nlayers(pet_maps)), mean)
## ## p2 <- myplotfun1(mean_pet)
## ## labelfun <- function(x) { ifelse((x %% 0.2) == 0, x, "") }
## ## breaks <- seq(0, 1.6, by = 0.05)
## ## nbreaks <- length(breaks)
## ## p2 <- p2 +
## ##   scale_fill_stepsn(
## ##     colours = RColorBrewer::brewer.pal(9, "YlOrRd"), #Br"),
## ##     breaks = breaks,
## ##     limits = c(min(breaks), max(breaks)),
## ##     labels = labelfun,
## ##     na.value = "grey"
## ##   ) +
## ##   theme(
## ##     legend.position = "bottom"
## ##   ) +
## ##   guides(
## ##     fill = guide_bins(
## ##       title = expression(Aridity~index~(`-`)),
## ##       title.position = "top",
## ##       axis = FALSE,
## ##       keywidth = grid::unit(fig1_keywidth / nbreaks, "npc"),
## ##       keyheight = grid::unit(3, "mm")
## ##     )
## ##   )
## ## p2

## ## ## Precipitation trend [not sure how to fit this in paper yet, but an important part of the story]
## ## trend <- raster.kendall(precip_maps, p.value = TRUE)
## ## slope <- trend$slope
## ## pval <- trend$p.value
## ## signif = pval <= 0.05
## ## signif_pts = as(signif, "SpatialPointsDataFrame")
## ## signif_pts = signif_pts[signif_pts$layer > 0,]
## ## signif_pts = st_as_sf(signif_pts)
## ## slope = slope * 1000 # meter/year -> mm/year
## ## p3 <- myplotfun1(slope)
## ## labelfun <- function(x) { ifelse((x %% 5) == 0, x, "") }
## ## breaks <- seq(-20, 10, 0.5)
## ## nbreaks <- length(breaks)
## ## rdbu_pal = RColorBrewer::brewer.pal(9, "RdBu")
## ## p3 <- p3 +
## ##   geom_sf(data = signif_pts, size = 0.01, shape = 20) +
## ##   scale_fill_stepsn(
## ##     colours = rdbu_pal,
## ##     breaks = breaks,
## ##     values = scales::rescale(c(-20, 0, 10)),
## ##     limits = c(min(breaks), max(breaks)),
## ##     labels = labelfun,
## ##     na.value = "grey"
## ##   ) +
## ##   theme(
## ##     legend.position = "bottom"
## ##   ) +
## ##   guides(
## ##     fill = guide_bins(
## ##       title = expression(Precipitation~trend~(mm~y^-1)),
## ##       title.position = "top",
## ##       axis = FALSE,
## ##       keywidth = grid::unit(fig1_keywidth / nbreaks, "npc"),
## ##       keyheight = grid::unit(3, "mm")
## ##     )
## ##   )
## ## p3

## ## trend <- raster.kendall(aridity_maps, p.value = TRUE)
## ## slope <- trend$slope
## ## pval <- trend$p.value
## ## signif = pval <= 0.05
## ## signif_pts = as(signif, "SpatialPointsDataFrame")
## ## signif_pts = signif_pts[signif_pts$layer > 0,]
## ## signif_pts = st_as_sf(signif_pts)
## ## ## slope = slope * 1000 # meter/year -> mm/year
## ## p4 <- myplotfun1(slope)
## ## labelfun <- function(x) { sapply(x, FUN=function(x) ifelse(isTRUE(all.equal(x %% 0.005, 0)), x, "")) }
## ## breaks <- seq(-0.015, 0.01, 0.0005)
## ## nbreaks <- length(breaks)
## ## rdbu_pal = RColorBrewer::brewer.pal(9, "BrBG")
## ## p4 <- p4 +
## ##   geom_sf(data = signif_pts, size = 0.01, shape = 20) +
## ##   scale_fill_stepsn(
## ##     colours = rdbu_pal,
## ##     breaks = breaks,
## ##     values = scales::rescale(c(-0.015, 0, 0.01)),
## ##     limits = c(min(breaks), max(breaks)),
## ##     labels = labelfun,
## ##     na.value = "grey"
## ##   ) +
## ##   theme(
## ##     legend.position = "bottom"
## ##   ) +
## ##   guides(
## ##     fill = guide_bins(
## ##       title = expression(Aridity~index~trend~(y^-1)),
## ##       title.position = "top",
## ##       axis = FALSE,
## ##       keywidth = grid::unit(fig1_keywidth / nbreaks, "npc"),
## ##       keyheight = grid::unit(3, "mm")
## ##   ))
## ## p4

## ## p1 <-
## ##   p1 +
## ##   theme(
## ##     legend.margin = margin(0, 0, 0, 0),
## ##     legend.box.margin = margin(-10, 0, -5, 0),
## ##     ## plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))
## ##   )

## ## p2 <-
## ##   p2 +
## ##   theme(
## ##     legend.margin = margin(0, 0, 0, 0),
## ##     legend.box.margin = margin(-10, 0, -5, 0),
## ##     ## plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
## ##     axis.text.y = element_blank()
## ##   )
## ## p3 <-
## ##   p3 +
## ##   theme(
## ##     legend.margin = margin(0, 0, 0, 0),
## ##     legend.box.margin = margin(-10, 0, -5, 0)
## ##     )

## ##     ## plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

## ## p4 <-
## ##   p4 +
## ##   theme(
## ##     legend.margin = margin(0, 0, 0, 0),
## ##     legend.box.margin = margin(-10, 0, -5, 0),
## ##     ## plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
## ##     axis.text.y = element_blank()
## ##   )

## ## fig1 <- p1 + p2 + p3 + p4 + plot_layout(nrow = 2, ncol = 2)
## ## ## fig1

## ## fig1$patches$plots[[1]] =
## ##   fig1$patches$plots[[1]] +
## ##   labs(tag = "a") +
## ##   theme(plot.tag.position = c(0.095, 1.035),
## ##         plot.tag = element_text(size = tag_label_size, face="bold"))
## ## fig1$patches$plots[[2]] =
## ##   fig1$patches$plots[[2]] +
## ##   labs(tag = "b") +
## ##   theme(plot.tag.position = c(0.03, 1.035),
## ##         plot.tag = element_text(size = tag_label_size, face="bold"))
## ## fig1$patches$plots[[3]] =
## ##   fig1$patches$plots[[3]] +
## ##   labs(tag = "c") +
## ##   theme(plot.tag.position = c(0.095, 1.035),
## ##         plot.tag = element_text(size = tag_label_size, face="bold"))
## ## fig1 =
## ##   fig1 +
## ##   labs(tag = "d") +
## ##   theme(plot.tag.position = c(0.03, 1),
## ##         plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
## ## fig1

## ## ggsave("../doc/figure1.png", width = 6, height = 5, units = "in")

## ## TODO
## ## * Fix misalignment of legends in p3/p4, which happens when title.position = "top"
## ## * Make legend title smaller
## ## * Inset plot to orient reader
