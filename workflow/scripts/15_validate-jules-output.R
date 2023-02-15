## Author : Simon Moulds
## Date   : Oct 2021

library(tidyverse)
library(sf)
library(ncdf4)
library(ncdf4.helpers)
library(raster)
library(magrittr)
library(zoo)

## validation_output_dir = "../data/validation"
## if (!dir.exists(validation_output_dir)) {
##     dir.create(validation_output_dir)
## }
## jules_output_dir = "../jules-output"
## id_stem = "JULES_vn6.1_upper_ganga"
## profile_name = "daily_hydrology"
## start_year = 2000
## end_year = 2010
## ## start_year = 1979
## ## end_year = 2014
## years = seq(start_year, end_year)

years = 1979:2013
## analysis_dir = "../data/analysis"
## historical_analysis_dir = file.path(
##     analysis_dir, "historical"
## )
## current_canal_analysis_dir = file.path(
##     analysis_dir, "current_canal"
## )
## restored_canal_analysis_dir = file.path(
##     analysis_dir, "restored_canal"
## )

## ################################################################# ##
## ################################################################# ##
##
## 1 - Water balance
##
## ################################################################# ##
## ################################################################# ##

template_fn = paste0(id_stem, ".S2.", profile_name, ".2010.2D.year.nc")
template = nc_open(file.path(jules_output_dir, template_fn))
lats = ncvar_get(template, "lat")
lons = ncvar_get(template, "lon")
nlat = length(lats)
nlon = length(lons)
template_raster = raster(
    ncol=nlon, nrow=nlat, xmn=60, xmx=100, ymn=20, ymx=40
)
template_raster[] = 0

reorder_array = function(arr) {
    arr %<>% aperm(c(2,1)) %>% `[`(seq(nlat, 1),)
    arr
}
add_to_raster = function(r, arr) {
    r[] = as.matrix(r) + (arr %>% reorder_array)
    r
}

wb_vars = c(
    "rainfall", "precip", "surf_roff", "sub_surf_roff", "elake",
    "esoil_gb", "ecan_gb", "irrig_water", "fqw_gb", "runoff", "fao_et0",
    "snowfall"
)

## GWM:
suite <- "u-ci496"
id_stem <- "JULES_vn6.1_noirrig"
profile_name <- c("daily_hydrology")
start_year <- 1980
end_year <- 2010
years <- seq(start_year, end_year)
npoints = 861

wb_output =
    lapply(wb_vars, FUN=function(x) rep(0, npoints)) %>%
    setNames(wb_vars)

for (i in 1:length(years)) {
    year = years[i]
    for (j in 1:length(profile_name)) {
        fn = paste0(id_stem, ".S2.", profile_name[j], ".", year, ".nc")
        nc = nc_open(file.path("~/JULES_output", suite, fn))
        for (var in wb_vars) {
            if (var %in% nc.get.variable.list(nc)) {
                arr = ncvar_get(nc, var) * 60 * 60 * 24 # kg m-2 s-1 -> mm/d
                arr = apply(arr, 1, sum)
                wb_output[[var]] = wb_output[[var]] + arr
            }
        }
        nc_close(nc)
    }
}

## NB Do not include elake because this is implicitly included in runoff
precip = wb_output[["precip"]]
irr = wb_output[["irrig_water"]]
et = (
    wb_output[["esoil_gb"]]
    + wb_output[["ecan_gb"]]
)
runoff = (
    wb_output[["surf_roff"]]
    + wb_output[["sub_surf_roff"]]
)
wb = precip + irr - et - runoff
wb_err = (wb / (precip + irr)) * 100

snow_ix = wb_output[["snowfall"]] > 0
mean(wb_err[ix])  # high error - accumulation of snow (more than 3m in some places?)
mean(wb_err[!ix]) # low error

## Mean annual irrigation
mean_annual_irr = irr / length(years)

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


## ## ################################################################# ##
## ## ################################################################# ##
## ##
## ## 2 - Comparison with GLEAM + MODIS ET
## ##
## ## ################################################################# ##
## ## ################################################################# ##

## ## TODO download GLEAM data to this PC

## ## gleam_datadir = "/mnt/scratch/scratch/data/GLEAM/data/v3.5b/monthly/"
## ## gleam_components = c("E","Eb","Ei","Ep","Es","Et","Ew")
## ## ## study_rgn_ext = extent(60, 100, 20, 40)
## ## study_rgn_ext = extent(77, 81, 26, 32)

## ## get_jules_et_data = function(year, month) {
## ##     fn = paste0(id_stem, ".S2.daily_hydrology.", year, ".2D.month.nc")
## ##     jules = nc_open(file.path(jules_output_dir, fn))
## ##     ## lat = ncvar_get(jules, "lat")
## ##     ## lon = ncvar_get(jules, "lon")
## ##     ## JULES ET components: esoil_gb, ecan_gb, elake
## ##     esoil = ncvar_get(jules, "esoil_gb", start=c(1,1,month), count=c(80, 40, 1))
## ##     ecan = ncvar_get(jules, "ecan_gb", start=c(1,1,month), count=c(80, 40, 1))
## ##     elake = ncvar_get(jules, "elake", start=c(1,1,month), count=c(80, 40, 1))
## ##     et = array(data=NA, dim=c(3, 80, 40))
## ##     et[1,,] = esoil
## ##     et[2,,] = ecan
## ##     et[3,,] = elake
## ##     et = apply(et, MARGIN=c(2,3), FUN=sum, na.rm=T)
## ##     et = aperm(et, c(2,1))
## ##     et = et[rev(seq_len(nrow(et))),]
## ##     r = raster(nrows=40, ncols=80, xmn=60, xmx=100, ymn=20, ymx=40)
## ##     r[] = et
## ##     r
## ## }

## ## ## Get time from one of the GLEAM files
## ## fn = paste0("E_2003-2020_GLEAM_v3.5b_MO.nc")
## ## gleam = nc_open(file.path(gleam_datadir, fn))
## ## time_raw = ncvar_get(gleam, "time")
## ## time_decoded =
## ##     nc.get.time.series(gleam, time.dim.name="time") %>%
## ##     as.character
## ## time_yearmon = as.yearmon(time_decoded)
## ## nc_close(gleam)

## ## ## Get the times for which JULES also has data
## ## start_year = 2000
## ## end_year = 2010
## ## time_years = time_yearmon %>% format("%Y")
## ## jules_time_yearmon = time_yearmon[time_years %in% seq(start_year, end_year)]

## ## gleam_comparison_output_dir = file.path(validation_output_dir, "gleam_comparison")
## ## if (!dir.exists(gleam_comparison_output_dir)) {
## ##     dir.create(gleam_comparison_output_dir)
## ## }

## ## ## Loop through each component
## ## for (j in 1:length(time_yearmon)) {
## ##     print(time_yearmon[j])
## ##     year = time_yearmon[j] %>% format("%Y") %>% as.integer
## ##     month = time_yearmon[j] %>% format("%m") %>% as.integer
## ##     lst = vector(mode="list", length=length(gleam_components))
## ##     names(lst) = gleam_components
## ##     for (i in 1:length(gleam_components)) {
## ##         component = gleam_components[i]
## ##         fn = paste0(component, "_", "2003-2020_GLEAM_v3.5b_MO.nc")
## ##         gleam = nc_open(file.path(gleam_datadir, fn))
## ##         et = ncvar_get(gleam, component, start=c(1,1,j), count=c(720, 1440, 1))
## ##         r = raster(nrows=720, ncols=1440)
## ##         r[] = et / 1000
## ##         r %<>% crop(study_rgn_ext) %>% aggregate(fact=2, FUN=mean)
## ##         lst[[i]] = r
## ##         nc_close(gleam)
## ##     }
## ##     ## Write E (= Eb + Ei + Es + Et + Ew)
## ##     writeRaster(
## ##         lst$E,
## ##         file.path(
## ##             validation_output_dir,
## ##             paste0("gleam_et_", year, "_", month, ".tif")
## ##         ),
## ##         overwrite=TRUE
## ##     )
## ##     if (time_yearmon[j] %in% jules_time_yearmon) {
## ##         jules_et = get_jules_et_data(year, month) %>% crop(study_rgn_ext)
## ##         writeRaster(
## ##             jules_et,
## ##             file.path(
## ##                 validation_output_dir,
## ##                 paste0("jules_et_", year, "_", month, ".tif")
## ##             ),
## ##             overwrite=TRUE
## ##         )
## ##     }
## ## }

## ## ## TODO: do something with the plots, e.g.:
## ## jules_et_jul_2010 = raster(file.path(validation_output_dir, "jules_et_2010_7.tif"))
## ## gleam_et_jul_2010 = raster(file.path(validation_output_dir, "gleam_et_2010_7.tif"))
## ## et_jul_2010 = stack(jules_et_jul_2010, gleam_et_jul_2010)
## ## plot(et_jun_2010)

## ## ################################################################# ##
## ## ################################################################# ##
## ##
## ## Irrigation demand comparison
## ##
## ## ################################################################# ##
## ## ################################################################# ##

## ## https://esg.pik-potsdam.de/search/isimip/?project=ISIMIP2a&product=output&sector=Water+Global

## ## Compare irrigation demand with pirrww from ISIMIP2a experiments

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
