## Author : Simon Moulds
## Date   : April 2021

library(tidyverse)
library(sf)
library(raster)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)

# world <- ne_countries(scale="small", continent="asia", returnclass="sf")
# india <-
#   st_read(file.path("resources", "GIS-shapefiles-1966base", "india70again.shp")) %>%
#   as_Spatial()
#
# india$ID = 1
# india = rasterize(india, ganges_basin, field = "ID")
# india[india_cmd_area == 0] = 1


gplot_data <- function(x, maxpixels = 50000)  {
  ## https://stackoverflow.com/a/47190738
  x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
  coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  ## Extract values
  dat <- utils::stack(as.data.frame(raster::getValues(x)))
  names(dat) <- c('value', 'variable')
  dat <- dplyr::as_tibble(data.frame(coords, dat))
  if (!is.null(levels(x))) {
    dat <- dplyr::left_join(dat, levels(x)[[1]],
                            by = c("value" = "ID"))
  }
  dat
}

sf_use_s2(FALSE) # Otherwise coord_sf(...) does not work correctly
myplotfun1 <- function(x, ...) {
  st_data <- gplot_data(x, ...)
  p <- ggplot() +
    geom_raster(data = st_data, aes(x = x, y = y, fill = value)) +
    geom_sf(data = india_cmd_area_poly, color = "black", fill = NA, size = 0.2) +
    geom_sf(data = world, color = "black", fill = NA, size = 0.4) +
    scale_x_continuous(limits = c(69, 88.75), expand = expansion(0)) +
    scale_y_continuous(limits = c(23.25, 33.25), expand = expansion(0)) +
    ## scale_fill_stepsn(
    ##   colours = RColorBrewer::brewer.pal(9, "Blues"),
    ##   breaks = seq(0, 0.5, by=0.025),
    ##   limits = c(0, 0.5),
    ##   labels = labelfun,
    ##   na.value = "grey",
    ##   guide = "legend",
    ##   expand = c(0, 0)
    ## ) +
    coord_sf() +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = strip_label_size),
          axis.text.x = element_text(size = axis_label_size),
          axis.text.y = element_text(size = axis_label_size),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_text(size = legend_title_size),
          legend.text = element_text(size = legend_label_size))
  p
}

labelfun <- function(x) { ifelse((x %% 0.1) == 0, x, "") }

get_trend <- function(x, lhs = "volume", rhs = "year", ...) {
  form <- as.formula(paste0(lhs, "~", rhs))
  trend = zyp.sen(form, x)
  intercept = trend$coefficients[1]
  slope = trend$coefficients[2]
  trend_conf = confint.zyp(trend)
  ## slope_025 = trend_conf[2,1]
  ## slope_975 = trend_conf[2,2]
  x_trend =
    data.frame(x[[rhs]], intercept + slope * x[[rhs]]) %>%
    setNames(c(rhs, "trend"))
    ## year = x$year,
    ## trend = intercept + slope * x$year
  ## )
  x_trend
}

myplotfun2 <- function(x, ...) {
  x_trend <- get_trend(x)
  p = ggplot(x, aes(x=year, y=volume)) +
    geom_line() +
    scale_x_continuous(
      name = "",
      breaks = seq(1980, 2010, 10),
      labels = c("1980", "1990", "2000", "2010"),
      limits = c(1979, 2011)
    ) +
    geom_line(aes(x=year, y=trend), data = x_trend, size=0.2, linetype=2) +
    ## scale_y_continuous(
    ##   name = "\u0394 Storage (m)"#,
    ##   ## breaks = seq(-0.2, 0.2, 0.1),
    ##   ## labels = c("-0.2", "-0.1", "0", "0.1", "0.2"),
    ##   ## limits = c(-0.23, 0.11)
    ## ) +
    geom_hline(yintercept = 0, size = 0.25) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(size = axis_label_size),
          axis.text.y = element_text(size = axis_label_size),
          axis.title.x = element_text(size = axis_title_size),
          axis.title.y = element_text(size = axis_title_size),
          legend.title = element_text(size = legend_title_size),
          legend.text = element_text(size = legend_label_size))
  p
}

myplotfun3 <- function(x, ...) {
  x_current <- x %>% filter(policy %in% "current")
  x_restored <- x %>% filter(policy %in% "restored")
  x_current_trend <- get_trend(x_current) %>% mutate(policy = "current")
  x_restored_trend <- get_trend(x_restored) %>% mutate(policy = "restored")
  x_trend <- rbind(x_current_trend, x_restored_trend) %>% arrange(year)
  p = ggplot(x, aes(x=year, y=volume, color=policy)) +
    geom_line() +
    scale_x_continuous(
      name = "",
      breaks = seq(1980, 2010, 10),
      labels = c("1980", "1990", "2000", "2010"),
      limits = c(1979, 2011)
    ) +
    geom_line(aes(x=year, y=trend, color=policy), data = x_trend, size=0.2, linetype=2) +
    scale_y_continuous(
      name = "\u0394 Storage (m)"#,
      ## breaks = seq(-0.2, 0.2, 0.1),
      ## labels = c("-0.2", "-0.1", "0", "0.1", "0.2"),
      ## limits = c(-0.23, 0.11)
    ) +
    geom_hline(yintercept = 0, size = 0.25) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(size = axis_label_size),
          axis.text.y = element_text(size = axis_label_size),
          axis.title.x = element_text(size = axis_title_size),
          axis.title.y = element_text(size = axis_title_size),
          legend.title = element_text(size = legend_title_size),
          legend.text = element_text(size = legend_label_size))
  p
}

myplotfun4 <- function(x, ...) {
  x_sw <- x %>% filter(types %in% "sw")
  x_gw <- x %>% filter(types %in% "gw")
  x_sw_trend <- get_trend(x_sw) %>% mutate(types = "sw")
  x_gw_trend <- get_trend(x_gw) %>% mutate(types = "gw")
  x_trend <- rbind(x_sw_trend, x_gw_trend) %>% arrange(year)
  x <-
    x %>%
    mutate(types = factor(types, levels = c("sw", "gw"), labels = c("Surface water", "Groundwater"))) %>%
    arrange(year, types)
  x_trend <-
    x_trend %>%
    mutate(types = factor(types, levels = c("sw", "gw"), labels = c("Surface water", "Groundwater"))) %>%
    arrange(year, types)
  p = ggplot(x, aes(x=year, y=volume, color=types)) +
    geom_line() +
    scale_x_continuous(
      name = "",
      breaks = seq(1980, 2010, 10),
      labels = c("1980", "1990", "2000", "2010"),
      limits = c(1979, 2011)
    ) +
    geom_line(
      aes(x=year, y=trend, color=types),
      data = x_trend, size=0.2, linetype=2
    ) +
    scale_y_continuous(
      name = "Irrigation (m)"#,
      ## breaks = seq(-0.2, 0.2, 0.1),
      ## labels = c("-0.2", "-0.1", "0", "0.1", "0.2"),
      ## limits = c(-0.23, 0.11)
    ) +
    ## geom_hline(yintercept = 0, size = 0.25) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(size = axis_label_size),
          axis.text.y = element_text(size = axis_label_size),
          axis.title.x = element_text(size = axis_title_size),
          axis.title.y = element_text(size = axis_title_size),
          legend.title = element_text(size = legend_title_size),
          legend.text = element_text(size = legend_label_size))
  p
}

library(ggnewscale)
myplotfun5 <- function(x, ...) {
  ## p = ggplot(x, aes(x=dS_current, y=precip, size=irr_area, color=x)) +
  blues_pal = colorRampPalette(brewer.pal(9, "Blues"))
  reds_pal = colorRampPalette(brewer.pal(9, "Reds"))
  p = ggplot(data = x, aes(x=dS_current, y=aridity, size=irr_area)) +
    geom_point(
      shape = 1,
      color = "grey60"
    ) +
    geom_point(
      data = x,
      aes(
        x=dS_restored,
        y=aridity,
        size=irr_area#,
        ## color=x
      ),
      color = "grey10",
      shape = 1,
      stroke = 0.1
    ) +
    xlim(c(-0.5, 0.5)) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(size = axis_label_size),
          axis.text.y = element_text(size = axis_label_size),
          axis.title.x = element_text(size = axis_title_size),
          axis.title.y = element_text(size = axis_title_size),
          legend.title = element_text(size = legend_title_size),
          legend.text = element_text(size = legend_label_size))
  p
}
