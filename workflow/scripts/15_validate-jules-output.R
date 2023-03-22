## Author : Simon Moulds
## Date   : Oct 2021

library(tidyverse)
library(sf)
library(ncdf4)
## library(ncdf4.helpers)
library(raster)
library(terra)
library(exactextractr)
library(magrittr)
library(zoo)
library(lubridate)

suite <- "u-ci496"

get_jules_data <- function(id_stem, year, month, varnames) {
  ## In these files, variables have units of m month-1
  fn <- paste0(id_stem, ".jules_", year, ".daily_hydrology.", year, ".2D.month.nc")
  r <- terra::rast(file.path("results/JULES_output", fn))
  maps <- list()
  for (i in 1:length(varnames)) {
    varname <- varnames[i]
    maps[[i]] <- r[varname][[month]]
  }
  st <- terra::rast(maps)
  sm <- app(st, sum, na.rm = TRUE)
  return(sm)
}

## ################################################################# ##
## ################################################################# ##
##
## Preamble
##
## ################################################################# ##
## ################################################################# ##

## Define the various JULES simulations (scenarios)
irrig_id_stem <- "JULES_vn6.1_irrig"
irrig_current_id_stem <- "JULES_vn6.1_irrig_current"
noirrig_id_stem <- "JULES_vn6.1_noirrig"
stems <- list(
  irrig = irrig_id_stem,
  irrig_current = irrig_current_id_stem,
  no_irrig = noirrig_id_stem
)
profile_name <- c("daily_hydrology")
start_year <- 1980
end_year <- 2010
jules_years <- seq(start_year, end_year)
npoints <- 861  # This is the number of points modelled by JULES

## Auxillary maps
india_cmd_area <- terra::rast("results/india_command_area.tif")
# india_cmd_area_west <- terra::rast("results/india_command_area_west.tif")
# india_cmd_area_east <- terra::rast("results/india_command_area_east.tif")
study_rgn_ext <- terra::ext(india_cmd_area)
cmd_areas <- st_read("resources/irrigation/command_areas.shp")


## ## ################################################################# ##
## ## ################################################################# ##
## ##
## ## 1 - Water balance
## ##
## ## ################################################################# ##
## ## ################################################################# ##

## wb_vars <- c(
##   "rainfall", "precip", "surf_roff", "sub_surf_roff", "elake",
##   "esoil_gb", "ecan_gb", "irrig_water", "fqw_gb", "runoff",
##   "fao_et0", "snowfall"
## )

## wb_output <-
##   lapply(wb_vars, FUN = function(x) rep(0, npoints)) %>%
##   setNames(wb_vars)

## for (i in 1:length(jules_years)) {
##   year <- jules_years[i]
##   for (j in 1:length(profile_name)) {
##     fn <- paste0(irrig_id_stem, ".jules_", year, ".daily_hydrology.", year, ".nc")
##     nc <- nc_open(file.path("~/JULES_output", suite, fn))
##     for (var in wb_vars) {
##       if (var %in% nc.get.variable.list(nc)) {
##         arr <- ncvar_get(nc, var) * 60 * 60 * 24 # kg m-2 s-1 -> mm/d
##         arr <- apply(arr, 1, sum)
##         wb_output[[var]] = wb_output[[var]] + arr
##       }
##     }
##     nc_close(nc)
##   }
## }

## ## NB elake is implicitly included in runoff
## precip <- wb_output[["precip"]]
## irr <- wb_output[["irrig_water"]]
## et <- wb_output[["esoil_gb"]] + wb_output[["ecan_gb"]]
## runoff <- wb_output[["surf_roff"]] + wb_output[["sub_surf_roff"]]
## wb <- precip + irr - et - runoff
## wb_err <- (wb / (precip + irr)) * 100

## snow_ix <- wb_output[["snowfall"]] > 1
## # There is relatively high error in snowy
## # grid cells due to snow accumulation (?)
## mean_wb_err_snow <- mean(wb_err[snow_ix])
## mean_wb_err_nosnow <- mean(wb_err[!snow_ix]) # relatively low error
## mean_wb_err <- mean(wb_err)


## ################################################################# ##
## ################################################################# ##
##
## 2 - Comparison with GLEAM
##
## ################################################################# ##
## ################################################################# ##

## Create output directory
dir.create("results/validation/gleam_comparison", recursive = TRUE, showWarnings = FALSE)

gleam_datadir <- "/var/data/scratch/data/GLEAM/data/v3.5b/monthly/"
gleam_components <- c("E", "Eb", "Ei", "Ep", "Es", "Et", "Ew")

## Get time from one of the GLEAM files
fn <- paste0("E_2003-2020_GLEAM_v3.5b_MO.nc")
gleam <- terra::rast(file.path(gleam_datadir, fn))
time_decoded <- terra::time(gleam)
time_yearmon <- as.yearmon(time_decoded)

## Get the times for which JULES also has data
years <- time_yearmon %>% format("%Y")
jules_time_yearmon <- time_yearmon[years %in% jules_years]
jules_et_components <- c("esoil_gb", "ecan_gb", "elake")

## Loop through each component
pb <- txtProgressBar(min = 0, max = length(time_yearmon) - 1, initial = 0)
for (j in 1:length(time_yearmon)) {
  year <- year(time_yearmon[i])
  month <- month(time_yearmon[i])
  lst <- vector(mode = "list", length = length(gleam_components))
  names(lst) <- gleam_components
  for (i in 1:length(gleam_components)) {
    component <- gleam_components[i]
    fn <- paste0(component, "_", "2003-2020_GLEAM_v3.5b_MO.nc")
    ## Note that opening netcdf with terra::rast(...) doesn't work on
    ## these files (latitude read as longitude and vice versa)
    gleam <- nc_open(file.path(gleam_datadir, fn))
    et <- ncvar_get(gleam, component, start = c(1,1,j), count = c(720, 1440, 1))
    r <- terra::rast(et)
    ext(r) <- ext(-180, 180, -90, 90)
    crs(r) <- "epsg:4326"
    # GLEAM data have units of mm/month [convert to m/month?]
    r <- r / 1000
    r <- r %>%
      terra::crop(study_rgn_ext) %>%
      aggregate(fact = 2, FUN = mean)
    lst[[i]] <- r
    nc_close(gleam)
  }
  st <- terra::rast(lst)
  area_tot_list <- exact_extract(
    st, cmd_areas, coverage_area = TRUE, progress = FALSE
  )
  for (i in 1:length(area_tot_list)) {
    area_tot <- area_tot_list[[i]]
    area_tot <- colSums(area_tot) #%>% tibble() %>% mutate(ID=id)
  }

  ## Write E (= Eb + Ei + Es + Et + Ew)
  writeRaster(
    lst[["E"]],
    file.path(
      "results/validation/gleam_comparison",
      paste0("gleam_et_", year, "_", month, ".tif")
    ),
    overwrite = TRUE
  )
  if (time_yearmon[j] %in% jules_time_yearmon) {
    for (k in 1:length(stems)) { 
      id_stem <- stems[[k]]
      id_stem_label <- names(stems)[k]
      jules_data <- get_jules_data(id_stem, year, month, jules_et_components)
      jules_data <- jules_data %>% crop(study_rgn_ext)
      writeRaster(
        jules_data,
        file.path(
          "results/validation/gleam_comparison",
          paste0("jules_", id_stem_label, "_et_", year, "_", month, ".tif")
        ),
        overwrite = TRUE
      )
    }
  }
  setTxtProgressBar(pb, j)
}
close(pb)

# TODO: find example plot comparing ET
# TODO: make sure study period length is the same

## # Plot multiyear biases
## fs <- list.files("results/validation/gleam_comparison", pattern = "jules_irrig_et_[0-9]{4}_[0-9]+.tif", full.names = TRUE)
## st <- terra::rast(fs)
## jules_irrig_et_mean <- app(st, mean)

## fs <- list.files("results/validation/gleam_comparison", pattern = "jules_noirrig_et_[0-9]{4}_[0-9]+.tif", full.names = TRUE)
## st <- terra::rast(fs)
## jules_noirrig_et_mean <- app(st, mean)

## fs <- list.files("results/validation/gleam_comparison", pattern = "gleam_et_[0-9]{4}_[0-9]+.tif", full.names = TRUE)
## st <- terra::rast(fs)
## gleam_et_mean <- app(st, mean)


## ################################################################# ##
## ################################################################# ##
##
## ISIMIP2a comparison
##
## ################################################################# ##
## ################################################################# ##

## https://esg.pik-potsdam.de/search/isimip/?project=ISIMIP2a&product=output&sector=Water+Global

## Compare irrigation demand with pirrww from ISIMIP2a experiments
# (model)_watch-wfdei_nobc_hist_(nosoc|pressoc|varsoc)_co2_(variable)_global_daily_(YYYY-YYYY)_IGP.month.nc4

## Create directory for output
dir.create("results/validation/isimip_comparison", recursive = TRUE, showWarnings = FALSE)

## Map JULES variable names to ISIMIP equivalent
isimip2jules <- list(
  qtot = "runoff",
  evap = c("esoil_gb", "ecan_gb", "elake"),
  # qr = "sub_surf_roff",
  # qs = "surf_roff",
  pirruse = "irrig_water",
  airruse = "irrig_water"
)

## Which ISIMIP experiment?
society <- "varsoc"

# Calculate weights for averaging
w <- terra::cellSize(india_cmd_area)
w <- w / 1000 / 1000
w <- w * india_cmd_area 
wsum <- global(w, mean, na.rm = TRUE)
wsum <- as.numeric(wsum)

models <- c(
  "watergap2-2c", 
  "vic", "pcr-globwb", "orchidee",
  "mpi-hm", "matsiro", "lpjml", "jules-w1", "h08",
  "dbh", "clm40"
)
variables <- names(isimip2jules)

## Various periods are used - loop is written to
## test all and skip if file doesn't exist
periods <- c("1981_1990", "1991_2000", "2001_2010", "2011_2016", "1971_2010", "1901_2016")

## df_list <- list()
for (i in 1:length(models)) {
  model <- models[i]

  for (j in 1:length(variables)) {
    variable <- variables[j]
    
    for (k in 1:length(periods)) {
      period <- periods[k]
      fn <- sprintf(
        "%s_watch-wfdei_nobc_hist_%s_co2_%s_global_daily_%s_IGP.month.nc4", 
        model, society, variable, period
      )
      if (!file.exists(file.path("resources/ISIMIP2a", fn))) {
        # print(fn)
        next
      }
      x <- terra::rast(file.path("resources/ISIMIP2a", fn))
      x <- resample(x, india_cmd_area, method = "near")
      x <- x * india_cmd_area

      ## Unit conversion (kg m-2 s-1 -> mm/day)
      tm <- time(x)
      num_days <- lubridate::days_in_month(tm) 
      x <- x * 24 * 60 * 60
      x <- x * num_days # Convert from mm/day to mm/month
      x <- x / 1000     # mm/month -> m/month 

      ## Write output
      years <- lubridate::year(tm)
      months <- lubridate::month(tm)
      for (ii in 1:length(tm)) {
        year <- years[ii]
        month <- months[ii]
        ## Skip if year does not correspond to a year in the JULES simulation
        if (!year %in% jules_years) {
          next
        }
        map <- x[[ii]]
        fn <- paste0(model, "_", society, "_", variable, "_", year, "_", month, ".tif")
        writeRaster(map, file.path("results/validation/isimip_comparison", fn), overwrite = TRUE)
      }
    }
  }
}

## Monthly time series for whole JULES simulation period
jules_time_yearmon <- seq(
  ym(paste0(start_year, "01")),
  ym(paste0(end_year, "12")), by = "1 month"
)
jules_time_yearmon <- as.yearmon(jules_time_yearmon)

## Now write the JULES output
for (i in 1:length(jules_time_yearmon)) {
  year <- year(jules_time_yearmon[i])
  month <- month(jules_time_yearmon[i])
  for (j in 1:length(variables)) {
    variable <- variables[[j]]
    jules_vars <- isimip2jules[[variable]]

    for (k in 1:length(stems)) {
      id_stem <- stems[[k]]
      id_stem_label <- names(stems)[k]
      jules_data <- get_jules_data(id_stem, year, month, jules_vars)
      jules_data <- jules_data %>% crop(study_rgn_ext)
      fn <- paste0("jules_", id_stem_label, "_", variable, "_", year, "_", month, ".tif")
      writeRaster(
        jules_data,
        file.path("results/validation/isimip_comparison", fn),
        overwrite = TRUE
      )
    }
  }
}


## ################################################################# ##
## ################################################################# ##
##
## Recharge
##
## ################################################################# ##
## ################################################################# ##

dir.create("results/validation/recharge_comparison", recursive = TRUE, showWarnings = FALSE)

global_recharge <- terra::rast("resources/global_recharge/RechargeTotal.nc")
global_recharge <- crop(global_recharge, study_rgn_ext)
global_recharge <- aggregate(global_recharge, fact = 30, fun = mean)
global_recharge <- global_recharge / 1000
writeRaster(
  global_recharge, 
  file.path("results/validation/recharge_comparison/average_annual_recharge.tif"), 
  overwrite = TRUE
)

global_recharge_frac <- terra::rast("resources/global_recharge/RechargeFraction.nc")
global_recharge_frac <- crop(global_recharge_frac, study_rgn_ext)
global_recharge_frac <- aggregate(global_recharge_frac, fact = 30, fun = mean)
writeRaster(
  global_recharge_frac, 
  file.path("results/validation/recharge_comparison/average_annual_recharge_frac.tif"), 
  overwrite = TRUE
)

stems <- list(no_irrig = noirrig_id_stem)
for (i in 1:length(stems)) {
  id_stem <- stems[[i]]
  id_stem_label <- names(stems)[i]

  total_recharge_maps <- list() 
  recharge_frac_maps <- list() 

  for (j in 1:length(jules_years)) {
    year <- jules_years[j]
    precip_maps <- list()
    recharge_maps <- list()

    ## We want annual totals, so sum up over Jan-Dec
    for (k in 1:12) {
      recharge_maps[[k]] <- get_jules_data(id_stem, year, k, "sub_surf_roff")
      precip_maps[[k]] <- get_jules_data(id_stem, year, k, "precip")
    }
    ## Add up to get units of mm / year
    jules_recharge <- app(terra::rast(recharge_maps), sum)
    jules_precip <- app(terra::rast(precip_maps), sum)
    jules_recharge_frac <- jules_recharge / jules_precip

    total_recharge_maps[[j]] <- jules_recharge 
    recharge_frac_maps[[j]] <- jules_recharge_frac 

  }

  total_recharge <- app(terra::rast(total_recharge_maps), mean) 
  recharge_frac <- app(terra::rast(recharge_frac_maps), mean) 

  ## Write output
  fn <- paste0("jules_", id_stem_label, "_total_recharge_", year, "_", k, ".tif")
  writeRaster(
    total_recharge,
    file.path("results/validation/recharge_comparison", fn),
    overwrite = TRUE
  )
  fn <- paste0("jules_", id_stem_label, "_recharge_frac_", year, "_", k, ".tif")
  writeRaster(
    recharge_frac,
    file.path("results/validation/recharge_comparison", fn),
    overwrite = TRUE
  )
}


## OLD:

## ## ################################################################# ##
## ## ################################################################# ##
## ##
## ## Recharge
## ##
## ## ################################################################# ##
## ## ################################################################# ##

## ## Compare with Qsb from earth2observe models
## e2o_models <- c("cnrs", "csiro", "ecmwf", "jrc", "jrchbv", "metfr", "nerc", "univk", "univu")
## for (i in 1:length(e2o_models)) {
##   model <- e2o_models[i]
##   ncfile <- file.path("../data/earth2observe", paste0("e2o_", model, "_wrr1_glob30_mon_Qsb_1979-2012.nc"))
##   br <- brick(ncfile) %>% crop(extent(india_cmd_area))
##   tms <- seq(as.Date(as.yearmon("1979-01")), as.Date(as.yearmon("2012-12")), by = "1 month")
##   for (j in 1:length(tms)) {
##     nd <- lubridate::days_in_month(tms[j]) %>% unname
##     br[[j]] <- br[[j]] * 60 * 60 * 24 * nd / 1000 # kg/m2/s -> m
##   }
##   br <- stackApply(br, indices = rep(1:34, each = 12), fun=sum)
##   br <- stackApply(br, indices = rep(1, nlayers(br)), fun=mean)
##   br <- resample(br, india_cmd_area, method = "ngb")
##   br <- br * india_cmd_area
##   br <- br * -1 # convert recharge to positive
## }

## ## Compare recharge with qr from ISIMIP2a experiments

## ## ################################################################# ##
## ## ################################################################# ##
## ##
## ## River-routing [MOSARTWM-Py]
## ##
## ## ################################################################# ##
## ## ################################################################# ##

## ## TODO

## ## Compare river routing with dis from ISIMIP2a experiments



## NOT USED:

## ## ################################################################# ##
## ## ################################################################# ##
## ##
## ## 0 - Recharge estimation from observed data
## ##
## ## ################################################################# ##
## ## ################################################################# ##

## ## Raw data - see Macdonald et al. (2016) "Groundwater quality and depletion in the Indo-Gangetic Basin mapped from in situ observations"

## india_data_1 =
##   read_csv("../data/IGP_GW_DATA/FirstclassDATAFORSTATS.csv", show_col_types = FALSE) %>%
##   mutate(flag = 1)

## india_data_2 =
##   read_csv("../data/IGP_GW_DATA/SecondclassDATAFORSTATS.csv", show_col_types = FALSE) %>%
##   mutate(flag = 2)

## ## Combine datasets and format
## india_data =
##   rbind(india_data_1, india_data_2) %>%
##   filter(!is.na(WaterLevel)) %>%
##   dplyr::select(State, District, X, Y, RecordingType, DateOfRecording, WaterLevel, flag) %>%
##   distinct(X, Y, RecordingType, DateOfRecording, .keep_all = TRUE)

## ## Format date of recording
## india_data =
##   india_data %>%
##   mutate(DateOfRecording = as.POSIXct(DateOfRecording, tz="GMT", format="%m/%d/%y")) %>%
##   mutate(Year = format(DateOfRecording, "%Y") %>% as.numeric) %>%
##   mutate(Month = format(DateOfRecording, "%m") %>% as.numeric)

## ## Remove outliers using Tukey's fences [as per Macdonald et al]
## isnt_out_tukey <- function(x, k = 1.5, na.rm = TRUE) {
##   quar <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
##   iqr <- diff(quar)
##   (quar[1] - k * iqr <= x) & (x <= quar[2] + k * iqr)
## }
## india_data =
##   india_data %>%
##   group_by(X, Y, RecordingType) %>%
##   mutate(is_outlier = !isnt_out_tukey(WaterLevel)) %>%
##   filter(!is_outlier) %>%
##   dplyr::select(-is_outlier)

## india_data =
##   india_data %>%
##   group_by(X, Y, RecordingType, Year) %>%
##   mutate(n = n()) %>%
##   filter(n >= 4) %>%
##   ## filter(n >= 2) %>%
##   ungroup()

## india_data =
##   india_data %>%
##   group_by(X, Y, RecordingType) %>%
##   mutate(n_year = n_distinct(Year))  %>%
##   filter(n_year >= 7)

## india_data =
##   india_data %>%
##   group_by(State, District, X, Y, RecordingType, Year) %>%
##   dplyr::summarise(
##     min_WaterLevel = min(WaterLevel, na.rm = TRUE),
##     max_WaterLevel = max(WaterLevel, na.rm = TRUE)) %>%
##   ungroup()

## wells =
##   india_data %>%
##   distinct(State, District, X, Y) %>%
##   na.omit() %>%
##   filter(X > 74 & X < 88) %>%
##   ## filter(X > 60) %>%
##   st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

## wells_sp = as_Spatial(wells)

## ## ## Prepare Sy map - these commands not working from R, do manually for now
## ## system('gdal_translate -a_srs "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs" ../data/specstor01.asc ../data/specstor01.tif')
## ## system('gdalwarp -t_srs EPSG:4326 ../data/specstor01.tif ../data/specstor01_ll.tif')

## r = raster("../data/specstor01_ll.tif")
## r = r * 100
## vals = r[wells_sp]
## Sy = wells %>% mutate(Sy = vals) #%>% filter(!is.na(Sy_vals))

## india_data =
##   india_data %>%
##   left_join(st_drop_geometry(Sy), by = c("State", "District", "X", "Y")) %>%
##   filter(!is.na(Sy))

## ## Calculate recharge
## india_data =
##   india_data %>%
##   mutate(recharge = (min_WaterLevel - max_WaterLevel) * Sy * -1)

## ## Exclude years with zero recharge, then restrict to wells with more
## ## than seven years
## india_data =
##   india_data %>%
##   filter(abs(recharge) > 0) %>%
##   group_by(X, Y, RecordingType) %>%
##   mutate(n_year = n_distinct(Year))  %>%
##   filter(n_year >= 7)

## recharge_list = list()
## scenarios = c("irrig", "noirrig")
## for (i in 1:(length(years)-1)) {
##   year = years[i]
##   for (j in 1:length(scenarios)) {
##     scenario = scenarios[j]
##     fn = file.path(
##       historical_analysis_dir,
##       sprintf("recharge_historical_%d_%s.tif", year, scenario)
##     )
##     r = raster(fn)
##     vals = r[wells_sp]
##     recharge =
##       st_drop_geometry(wells) %>%
##       mutate(
##         Year = year,
##         scenario = paste0("recharge_", scenario),
##         value = vals
##       )
##     idx = length(recharge_list) + 1
##     recharge_list[[idx]] = recharge
##   }
## }
## jules_recharge = do.call("rbind", recharge_list)
## jules_recharge =
##   jules_recharge %>%
##   pivot_wider(names_from = scenario, values_from = value)

## india_data =
##   india_data %>%
##   left_join(jules_recharge, by = c("State", "District", "X", "Y", "Year"))

## r = raster(nrows = 40, ncols = 80, xmn = 60, xmx = 100, ymn = 20, ymx = 40)
## r[] = 1:ncell(r)
## vals = r[wells_sp]
## cell_id = st_drop_geometry(wells) %>% mutate(JULES_cell_ID = vals)

## india_data =
##   india_data %>%
##   left_join(cell_id, by = c("State", "District", "X", "Y"))

## anomalyfun = function(x) {
##   x - mean(x)
## }
## recharge_data =
##   india_data %>%
##   filter(Year %in% 2000:2012) %>%
##   group_by(Year, JULES_cell_ID) %>%
##   dplyr::summarize(across(contains("recharge"), mean, na.rm =TRUE), n_wells = n()) %>%
##   filter(n_wells >= 5) %>%
##   ungroup() %>%
##   group_by(JULES_cell_ID) %>%
##   mutate(n_year = n()) %>%
##   arrange(JULES_cell_ID, Year) %>%
##   na.omit()

## ## Compare mean recharge across study region
## mean_recharge =
##   recharge_data %>%
##   filter(n_year >= 7) %>%
##   group_by(JULES_cell_ID) %>%
##   dplyr::summarize(across(contains("recharge"), mean))
## cor = cor.test(mean_recharge$recharge, mean_recharge$recharge_irrig)$estimate %>% unname

## recharge_acc =
##   recharge_data %>%
##   filter(n_year >= 7) %>%
##   group_by(JULES_cell_ID) %>%
##   mutate(across(contains("recharge"), anomalyfun)) %>%
##   dplyr::summarize(
##     acc = cor.test(recharge, recharge_irrig)$estimate,
##     n_year = mean(n_year))

## ## TODO comparisons:
## ## PCR-GLOBWB: https://opendap.4tu.nl/thredds/catalog/data2/pcrglobwb/catalog.html
## ## JULES_noirrig
## ## [...] any other models we can obtain
## ## Unfortunately ISIMIP only goes up to 2005
## ## recharge_acc$acc %>% boxplot

## ## Plot map of ACC for recharge 2000-2012
## ids = recharge_acc$JULES_cell_ID
## r = raster(nrows = 40, ncols = 80, xmn = 60, xmx = 100, ymn = 20, ymx = 40)
## r[] = NA
## r[ids] = recharge_acc$acc

## st_data = gplot_data(r)
## p = ggplot() +
##   geom_tile(data = st_data, aes(x = x, y = y, fill = value)) +
##   ## facet_wrap(~variable) +
##   geom_sf(data = india_cmd_area_poly, color = "black", fill = NA) +
##   xlim(69, 88.75) +
##   ylim(23.25, 33.25) +
##   scale_fill_stepsn(
##     colours = RColorBrewer::brewer.pal(9, "RdBu"),
##     breaks = seq(-0.6, 0.6, by=0.2) %>% round(digits=1),
##     limits = c(-0.6, 0.6)
##   ) +
##   coord_sf() +
##   theme_bw() +
##   theme(panel.grid = element_blank(),
##         axis.title = element_blank(),
##         strip.background = element_blank(),
##         strip.text = element_text(size = strip_label_size),
##         axis.text.x = element_text(size = axis_label_size),
##         axis.text.y = element_text(size = axis_label_size),
##         axis.title.x = element_blank(),
##         axis.title.y = element_blank(),
##         legend.title = element_text(size = legend_title_size),
##         legend.text = element_text(size = legend_label_size)) +
##   guides(fill = guide_legend(title= "ACC"))
## p

## ## TODO save plot

## stop("all OK")

## ## FIXME do we need to include abstraction in the above calculation? It is not obvious how to do this.

## ## http://cgwb.gov.in/District_Profile/UP_districtprofile.html
