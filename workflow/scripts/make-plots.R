## Author : Simon Moulds
## Date   : April 2021

library(raster)
library(terra)
library(RColorBrewer)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgdal)
library(spatialEco)
library(tidyverse)
library(patchwork)
library(cowplot)
library(zyp)

options(stringsAsFactors = FALSE)

## Extract configuration info
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly=TRUE)
  args <- commandArgs()
  m <- regexpr("(?<=^--file=).+", args, perl=TRUE)
  cwd <- dirname(regmatches(args, m))
}

## Load custom utilities
home <- Sys.getenv("HOME")
cwd <- file.path(home, "projects/gwm-analysis/workflow/scripts")
source(file.path(cwd, "utils.R"))
source(file.path(cwd, "plotting.R"))

## TODO (additional plot ideas)
## * Fig 2 but for Rabi - put in supplementary material
## * Mean water demand in Kharif and Rabi
## * Scatter plot of Kharif/rabi precip against mean dS
## * Scatter plot of precipitation anomaly against annual dS in east/west basin regions
## * [spatial] Mean dS under current and restored scenarios
## * [spatial] dS trend under current and restored scenarios
## * Barplot showing irrigation volume under current and restored scenarios
## * Decide whether current and restored need different JULES 
##   configs (more summertime irrigation during kharif)

## ####################################################### ##
## ####################################################### ##
##
## Preamble
##
## ####################################################### ##
## ####################################################### ##

# Years in simulation
years <- 1979:2013

# Irrigation sources
types <- c("gw", "sw", "total")

# India comand areas as polygon, for plotting
india_cmd_area_poly <- st_read("results/plotting/india_command_area.gpkg")

# Global map for plotting
world <- ne_countries(
  scale = "small", 
  continent = "asia", 
  returnclass = "sf"
)

# All data used in plots
sw_kharif_maps <- stack("results/plotting/historical_sw_kharif_ts.tif")
gw_kharif_maps <- stack("results/plotting/historical_gw_kharif_ts.tif")
sw_rabi_maps <- stack("results/plotting/historical_sw_rabi_ts.tif")
gw_rabi_maps <- stack("results/plotting/historical_gw_rabi_ts.tif")
precip_maps <- stack("results/plotting/historical_precip_ts.tif")
pet_maps <- stack("results/plotting/historical_pet_ts.tif")
aridity_maps <- stack("results/plotting/historical_aridity_ts.tif")

irr_area <- raster("results/plotting/current_irrigated_area.tif")
current_canal_area <- raster("results/plotting/current_canal_area.tif")
restored_canal_area <- raster("results/plotting/restored_canal_area.tif")
current_gw_area <- raster("results/plotting/current_gw_area.tif") 

historical_water_balance_maps <- stack("results/plotting/historical_water_balance_ts.tif")
dS_historical_mean <- raster("results/plotting/dS_historical_mean.tif")
dS_current_mean <- raster("results/plotting/dS_current_canal_ts.tif")
dS_restored_mean <- raster("results/plotting/dS_restored_canal_ts.tif")

historical_irrigation_demand_ts <- readRDS("results/plotting/historical_irrigation_demand_ts.rds")
historical_water_balance_ts <- readRDS("results/plotting/historical_water_balance_ts.rds")
scenario_water_balance_ts <- readRDS("results/plotting/scenario_water_balance_ts.rds")

## Variables for plotting
axis_title_size <- 6
axis_label_size <- 6
axis_label_size_small <- 6
legend_label_size <- 6
legend_title_size <- 6
tag_label_size <- 6
strip_label_size <- 6
fig1_keywidth <- 0.3

## ####################################################### ##
## ####################################################### ##
##
## Historical demand-side changes
##
## ####################################################### ##
## ####################################################### ##


labelfun <- function(x) { ifelse((x %% 0.1) == 0, x, "") }
breaks <- seq(0, 0.6, by = 0.01)
nbreaks <- length(breaks)


# Panel a: Current canal area
p1 <- myplotfun1(current_canal_area)
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

# Panel b: Current groundwater area
p2 <- myplotfun1(current_gw_area)
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

# Panel c: Surface water irrigation trend
trend <- raster.kendall(terra::rast(sw_kharif_maps), p.value = TRUE)
slope <- raster(trend$slope)
pval <- raster(trend$p.value)
signif <- pval <= 0.05
signif_pts <- as(signif, "SpatialPointsDataFrame")
signif_pts <- signif_pts[signif_pts$layer > 0,]
signif_pts <- st_as_sf(signif_pts)

## slope = slope * 1000 # meter/year -> mm/year
labelfun <- function(x) { ifelse((x %% 1) == 0, x, "") }
breaks <- seq(-5, 3.5, 0.05)
nbreaks <- length(breaks)
rdbu_pal <- RColorBrewer::brewer.pal(9, "RdBu")

p3 <- myplotfun1(slope)
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


# Panel d: Trend in groundwater irrigation
trend <- raster.kendall(terra::rast(gw_kharif_maps), p.value = TRUE)
slope <- raster(trend$slope)
pval <- raster(trend$p.value)
signif <- pval <= 0.05
signif_pts <- as(signif, "SpatialPointsDataFrame")
signif_pts <- signif_pts[signif_pts$layer > 0,]
signif_pts <- st_as_sf(signif_pts)

## slope = slope * 1000 # meter/year -> mm/year
labelfun <- function(x) { ifelse((x %% 1) == 0, x, "") }
breaks <- seq(0, 7.5, 0.05)
nbreaks <- length(breaks)
p4 <- myplotfun1(slope)
rdbu_pal <- RColorBrewer::brewer.pal(9, "RdBu")[5:9]
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

x <-
  historical_irrigation_demand_ts %>%
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
p12 <- p12 +
  labs(tag = "b") +
  theme(plot.tag.position = c(0.14, 1),
        plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))
p34$patches$plots[[1]] =
  p34$patches$plots[[1]] +
  labs(tag = "c") +
  theme(plot.tag.position = c(0.14, 1.035),
        plot.tag = element_text(size = tag_label_size, face="bold"))
p34 <- p34 +
  labs(tag = "d") +
  theme(plot.tag.position = c(0.14, 1),
        plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

p67$patches$plots[[1]] =
  p67$patches$plots[[1]] +
  labs(tag = "f") +
  theme(plot.tag.position = c(0.14, 1.06),
        plot.tag = element_text(size = tag_label_size, face="bold"))

p67 <- p67 +
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

fig2 <- plot_grid(p12, p34, p567, nrow = 3, align = "v")
ggsave("results/fig/figure2.png", width = 6, height = 7.5, units = "in")


## ####################################################### ##
## ####################################################### ##
##
## Historical change in storage
##
## ####################################################### ##
## ####################################################### ##

# Panel a: Mean annual net storage change (recharge - abstraction)
labelfun <- function(x) { ifelse((x %% 0.1) == 0, x, "") }
breaks <- seq(-0.4, 0.6, 0.01)
nbreaks <- length(breaks)
rdbu_pal <- RColorBrewer::brewer.pal(9, "RdBu")

p1 <- myplotfun1(dS_historical_mean)
p1 <- p1 +
  ## geom_sf(data = signif_pts, size = 0.01, shape = 20) +
  scale_fill_stepsn(
    colours = rdbu_pal,
    breaks = breaks,
    values = scales::rescale(c(-0.4, 0, 0.6)),
    limits = c(min(breaks), max(breaks)),
    labels = labelfun,
    na.value = "grey"
  ) +
  theme(
    legend.position = "bottom"
  ) +
  guides(
    fill = guide_bins(
      ## title = expression(Mean~Delta~S(m)),
      title = "Mean annual change in storage (m)",
      title.position = "top",
      axis = FALSE,
      keywidth = grid::unit(fig1_keywidth / nbreaks, "npc"),
      keyheight = grid::unit(3, "mm")
    )
  )

# Panel b Trend in annual net storage change (recharge - abstraction)
# st <- resample(st, india_cmd_area)
# st <- st * india_cmd_area
trend <- raster.kendall(terra::rast(historical_water_balance_maps), p.value = TRUE)
slope <- raster(trend$slope)
pval <- raster(trend$p.value)
signif <- pval <= 0.05
signif_pts <- as(signif, "SpatialPointsDataFrame")
signif_pts <- signif_pts[signif_pts$layer > 0,]
signif_pts <- st_as_sf(signif_pts)
slope <- slope * 1000 # meter/year -> mm/year

labelfun <- function(x) { ifelse((x %% 5) == 0, x, "") }
breaks <- seq(-18, 8, 0.1)
nbreaks <- length(breaks)
rdbu_pal = RColorBrewer::brewer.pal(9, "RdBu")

p2 <- myplotfun1(slope)
p2 <- p2 +
  geom_sf(data = signif_pts, size = 0.01, shape = 20) +
  scale_fill_stepsn(
    colours = rdbu_pal,
    breaks = breaks,
    values = scales::rescale(c(-18, 0, 8)),
    limits = c(min(breaks), max(breaks)),
    labels = labelfun,
    na.value = "grey"
  ) +
  theme(
    legend.position = "bottom"
  ) +
  guides(
    fill = guide_bins(
      title = expression(Trend~"in"~annual~storage~change~(mm~y^-1)),
      title.position = "top",
      axis = FALSE,
      keywidth = grid::unit(fig1_keywidth / nbreaks, "npc"),
      keyheight = grid::unit(3, "mm")
    )
  )

igp_historical_water_balance_ts <- historical_water_balance_ts %>% filter(basin %in% "igp")
p3 <- myplotfun2(igp_historical_water_balance_ts)
p3 <-
  p3 +
  scale_y_continuous(
    name = "\u0394 Storage (m)",
    limits = c(-0.3, 0.3)
  )

igp_west_historical_water_balance_ts <- historical_water_balance_ts %>% filter(basin %in% "igp_east")
p4 <- myplotfun2(igp_west_historical_water_balance_ts)
p4 <-
  p4 +
  scale_y_continuous(
    name = "\u0394 Storage (m)",
    limits = c(-0.3, 0.3)
  )

igp_east_historical_water_balance_ts <- historical_water_balance_ts %>% filter(basin %in% "igp_west")
p5 <- myplotfun2(igp_east_historical_water_balance_ts)
p5 <-
  p5 +
  scale_y_continuous(
    name = "\u0394 Storage (m)",
    limits = c(-0.3, 0.3)
  )

p4 <- p4 + theme(axis.text.y = element_blank())
p5 <- p5 + theme(axis.text.y = element_blank())

## Combine panels
p12 <-
  p1 + p2 + plot_layout(ncol = 2, guides = "keep") &
  theme(
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, 0, -5, 0)
  )

p45 <- p4 + p5 + plot_layout(nrow = 2)

p12$patches$plots[[1]] <-
  p12$patches$plots[[1]] +
  labs(tag = "a") +
  theme(plot.tag.position = c(0.14, 1.035),
        plot.tag = element_text(size = tag_label_size, face="bold"))
p12 <- p12 +
  labs(tag = "b") +
  theme(plot.tag.position = c(0.14, 1),
        plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

p45$patches$plots[[1]] <-
  p45$patches$plots[[1]] +
  labs(tag = "d") +
  theme(plot.tag.position = c(0.14, 1.06),
        plot.tag = element_text(size = tag_label_size, face="bold"))

p45 <- p45 +
  labs(tag = "e") +
  theme(plot.tag.position = c(0.14, 1.02),
        plot.tag = element_text(vjust = -0.7, size = tag_label_size, face="bold"))

p345 <-
  p3 + p45 + plot_layout(ncol = 2, guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, 0, -5, 0)
  )

p345$patches$plots[[1]] =
  p345$patches$plots[[1]] +
  labs(tag = "c") +
  theme(plot.tag.position = c(0.14, 1.025),
        plot.tag = element_text(size = tag_label_size, face="bold"))

fig3 <- plot_grid(p12, p345, nrow=2, align = "v")
ggsave("results/fig/figure3.png", width = 6, height = 5, units = "in")


## ####################################################### ##
## ####################################################### ##
##
## Activated Ganges water machine
##
## ####################################################### ##
## ####################################################### ##

## This is a counterfactual simulation to evaluate the potential
## impact of canal restoration on the historical water balace
## Use results from "JULES_vn6.1_irrig_current" to compare
## policies "current_canal" AND "restored_canal"

max_canal_area <- max(
  cellStats(current_canal_area, max), 
  cellStats(restored_canal_area, max)
)

p1 <- myplotfun1(current_canal_area)
labelfun <- function(x) { ifelse((x %% 0.1) == 0, x, "") }
breaks <- seq(0, 0.75, by=0.01)
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

## FIXME put this in extract_time_series.R
india_cmd_area <- raster("results/india_command_area.tif")
restored_canal_area <- resample(restored_canal_area, india_cmd_area)
restored_canal_area <- restored_canal_area * india_cmd_area

p2 <- myplotfun1(restored_canal_area)
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
      title = expression(Fractional~canal~irrigated~area), #Depth~(m)),
      ## title = expression(Depth~(m)),
      title.position = "top",
      axis = FALSE,
      keywidth = grid::unit(fig1_keywidth / nbreaks, "npc"),
      keyheight = grid::unit(3, "mm")
    )
  )

precip_mean <- stackApply(precip_maps, rep(1, nlayers(precip_maps)), mean)
aridity_mean <- stackApply(aridity_maps, rep(1, nlayers(precip_maps)), mean)

df <- stack(list(aridity = aridity_mean,
                 precip = precip_mean,
                 dS_current = dS_current_mean,
                 dS_restored = dS_restored_mean,
                 irr_area = irr_area)) %>%
  as.data.frame(xy = TRUE) %>%
  na.omit() %>%
  as.tibble() %>%
  filter(irr_area >= 0.25)


## Size of the point could also be related to the increase in canal area in the grid cell
sf <- 0.5
df_sampled <- df[sample(1:nrow(df), floor(nrow(df) * sf)) %>% sort,]
p3 <- myplotfun5(df_sampled)
p3 <- p3 +
  theme(
    legend.position = "bottom"
  ) +
  scale_x_continuous(
    name = "\u0394 Storage (m)",
  ) +
  scale_y_continuous(
    name = "Aridity (-)"#,
    ## limits = c(0.2, 1.4)
  ) +
  guides(
    colour = "none",
    size = guide_legend(
      title = expression(Fractional~irrigated~area),
      title.position = "top"#,
      ## axis = FALSE#,
      ## keywidth = grid::unit(fig1_keywidth / nbreaks, "npc"),
      ## keyheight = grid::unit(3, "mm")
    )
  )

## xx = output %>% pivot_longer(all_of(c("dS", "abstraction", "recharge"))) %>% arrange(year)
xx <- scenario_water_balance_ts %>% arrange(year)

xx_igp <- xx %>% filter(basin %in% "igp")
p4 <- myplotfun3(xx_igp)
p4 <- p4 +
  scale_y_continuous(
    name = "\u0394 Storage (m)",
    limits = c(-0.3, 0.3)
  )

xx_igp_east <- xx %>% filter(basin %in% "igp_east")
p5 <- myplotfun3(xx_igp_east)
p5 <- p5 +
  scale_y_continuous(
    name = "\u0394 Storage (m)",
    limits = c(-0.3, 0.3)
  )

xx_igp_west <- xx %>% filter(basin %in% "igp_west")
p6 <- myplotfun3(xx_igp_west)
p6 <- p6 +
  scale_y_continuous(
    name = "\u0394 Storage (m)",
    limits = c(-0.3, 0.3)
  )

p1 <- p1 + theme(legend.position = "none", axis.text.x = element_blank())

p1 <- p1 +
  labs(title = "a") +
  theme(plot.title = element_text(size = tag_label_size, face="bold"))

p2 <- p2 +
  labs(title = "b") +
  theme(plot.title = element_text(size = tag_label_size, face="bold"))

p3 <- p3 +
  labs(title = "c") +
  theme(plot.title = element_text(size = tag_label_size, face="bold"))

p12 <- p1 + p2 + plot_layout(nrow = 2)

p4 <- p4 +
  labs(title = "d") +
  theme(plot.title = element_text(size = tag_label_size, face="bold"))

p5 <- p5 +
  labs(title = "e") +
  theme(plot.title = element_text(size = tag_label_size, face="bold"))

p6 <- p6 +
  labs(title = "f") +
  theme(plot.title = element_text(size = tag_label_size, face="bold"))

p56 <- p5 + p6 + plot_layout(nrow = 2)

p123 <- p12 | p3 #+ #plot_layout(guides = "keep")

p456 <-
  p4 + p56 + plot_layout(ncol = 2, guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, 0, -5, 0)
  )

fig4 <- plot_grid(p123, p456, nrow=2, align = "v", rel_heights = c(1, 0.75))
ggsave("results/fig/figure4.png", width = 6, height = 7, units = "in", dpi = 300)

## ggsave("results/fig/figure4_pres.png", width = 4, height = 5, units = "in", dpi = 600)

## ####################################################### ##
## ####################################################### ##
##
## Figure X: Water balance under current and potential
## (climate change)
##
## ####################################################### ##
## ####################################################### ##

## The same as above, except using future climate change
## scenarios from Pradeep

## TODO, as a very last step once JULES model is finalized
