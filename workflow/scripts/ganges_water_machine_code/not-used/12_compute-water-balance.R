## Author : Simon Moulds
## Date   : April 2021

library(ncdf4)
library(ncdf4.helpers)
library(raster)
library(magrittr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgdal)
library(dplyr)

## Load custom utilities
source("utils.R")

## Output for analysis
analysis_output_dir = "../data/analysis"
if (!dir.exists(analysis_output_dir)) {
    dir.create(analysis_output_dir)
}

## Irrgation sources
irrigation_sources = c("canal", "other_sources", "other_wells", "tanks", "tubewells")

## Policies, reference year for policies
policies = c("historical", "current_canal", "restored_canal")
for (policy in policies) {
  dir.create(file.path(analysis_output_dir, policy), showWarnings = FALSE)
}
reference_year = 2010

## JULES simulation details
jules_output_dir = "../jules-output/u-ci496"
## *_irrig         : historical irrigated area
## *_irrig_current : current irrigated area
## *_noirrig       : no irrigation
id_stems = c("JULES_vn6.1_irrig", "JULES_vn6.1_irrig_current", "JULES_vn6.1_noirrig")
job_name = "jules_%s"
profile_name = "daily_hydrology"
start_year = 1979
end_year = 2014
years = seq(start_year, end_year)

## Create monthly seasonal data (rather simple for the time being)
## [The below values are taken from 07_create-irrig-schedule.py]
## Irrigation single [Jun-Oct (end)]
## 153-304
## Irrigation double [Nov-Mar (end)]
## 305-90
## Irrigation triple [Apr-May (end)]
## 91-151
seasons = c("continuous", "kharif", "rabi", "zaid")
season_maps = vector("list", length(seasons)) %>% setNames(seasons)

## Take agricultural season Nov-Oct [start of Rabi to end of Kharif]
## TODO check for consistency with land use change [run JULES Nov-Oct?]
season_months = list(
    continuous = c(11:22),
    kharif = c(18:22),
    rabi = c(11:15),
    zaid = c(16:17)
)
year_months = 11:22 # Nov-Oct (year+1)

load_jules_output <- function(id_stem, policy, yr, ...) {

  irri_yr <- ifelse(policy == "historical", yr, reference_year)
  ## `get_sw_gw_irr_frac()` returns a list with elements:
  ## gw    : Fraction of grid cell irrigated with groundwater
  ## sw    : Fraction of grid cell irrigated with surface water
  ## total : Total irrigation
  ## TODO: ***ensure these are obtained directly from JULES input maps***
  kharif_irr = get_sw_gw_irr_frac(irri_yr, "kharif", policy)
  rabi_irr = get_sw_gw_irr_frac(irri_yr, "rabi", policy)
  zaid_irr = get_sw_gw_irr_frac(irri_yr, "zaid", policy)
  continuous_irr = get_sw_gw_irr_frac(irri_yr, "continuous", policy)

  ## id_stem = id_stems[i]
  ## suffix = sub("(JULES)_(vn.*)_([a-z]+)", "\\3", id_stem)

  ## Preallocate lists to store values
  precip = vector("list", 12)
  ## irrig_water = vector("list", 12)
  surf_roff = vector("list", 12)
  sub_surf_roff = vector("list", 12)
  et = vector("list", 12)
  gw_irrig_water = vector("list", 12)
  sw_irrig_water = vector("list", 12)
  total_irrig_water = vector("list", 12)

  ## Season irrigation - each season divided between gw/sw/total
  season_irrig_maps = vector("list", 4) %>% setNames(seasons)
  for (j in 1:length(seasons)) {
    season = seasons[j]
    season_irrig_maps[[season]] =
      lapply(1:3, FUN=function(x) vector("list", 12)) %>%
      setNames(c("total", "sw", "gw"))
  }

  ## Loop through months and calculate each variable
  for (j in 1:12) {
    month = year_months[j]
    ## Precipitation
    precip[[j]] = get_jules_month_data(
        yr, month, "precip", id_stem, job_name, profile_name
    )
    ## Runoff
    sub_surf_roff[[j]] = get_jules_month_data(
        yr, month, "sub_surf_roff", id_stem, job_name, profile_name
    )
    surf_roff[[j]] = get_jules_month_data(
        yr, month, "surf_roff", id_stem, job_name, profile_name
    )
    ## Evaporation
    esoil = get_jules_month_data(
        yr, month, "esoil_gb", id_stem, job_name, profile_name
    )
    ecan = get_jules_month_data(
        yr, month, "ecan_gb", id_stem, job_name, profile_name
    )
    elake = get_jules_month_data(
        yr, month, "elake", id_stem, job_name, profile_name
    )
    et[[j]] = esoil + ecan + elake
    ## Irrigation
    irrig_water = get_jules_month_irrig_rel_data(
        yr, month, "irrig_water", id_stem, job_name, profile_name
    )
    irrig_water_reference = get_jules_month_data(
        yr, month, "irrig_water", id_stem, job_name, profile_name
    )
    irrig_water_total = stackApply(
        irrig_water,
        indices=rep(1, nlayers(irrig_water)),
        fun=sum
    )
    ## 7/8/9/10 are the indices of the respective land fraction in JULES
    kharif_index = (irrig_water[[7]] > 0)
    rabi_index = (irrig_water[[8]] > 0) & (!kharif_index)
    zaid_index = (irrig_water[[9]] > 0) & (!(kharif_index | rabi_index))
    continuous_index = (irrig_water[[10]] > 0)
    ## Compute kharif irrigation among each irrigated land cover
    kharif_irrig_water = stackApply(
        irrig_water[[7:9]],
        indices=rep(1, 3),
        fun=sum
    ) * kharif_index
    season_irrig_maps[["kharif"]][["gw"]][[j]] =
        kharif_irrig_water * kharif_irr$gw
    season_irrig_maps[["kharif"]][["sw"]][[j]] =
        kharif_irrig_water * kharif_irr$sw
    season_irrig_maps[["kharif"]][["total"]][[j]] =
        kharif_irrig_water
    rabi_irrig_water = stackApply(
        irrig_water[[8:9]],
        indices=rep(1, 2),
        fun=sum
    ) * rabi_index
    season_irrig_maps[["rabi"]][["gw"]][[j]] =
        rabi_irrig_water * rabi_irr$gw
    season_irrig_maps[["rabi"]][["sw"]][[j]] =
        rabi_irrig_water * rabi_irr$sw
    season_irrig_maps[["rabi"]][["total"]][[j]] =
        rabi_irrig_water
    zaid_irrig_water = irrig_water[[9]] * zaid_index
    season_irrig_maps[["zaid"]][["gw"]][[j]] =
        zaid_irrig_water * zaid_irr$gw
    season_irrig_maps[["zaid"]][["sw"]][[j]] =
        zaid_irrig_water * zaid_irr$sw
    season_irrig_maps[["zaid"]][["total"]][[j]] =
        zaid_irrig_water
    continuous_irrig_water = irrig_water[[10]] * continuous_index
    season_irrig_maps[["continuous"]][["gw"]][[j]] =
        continuous_irrig_water * continuous_irr$gw
    season_irrig_maps[["continuous"]][["sw"]][[j]] =
        continuous_irrig_water * continuous_irr$sw
    season_irrig_maps[["continuous"]][["total"]][[j]] =
        continuous_irrig_water
    ## Source totals
    gw_irrig_water[[j]] =
        season_irrig_maps[["kharif"]][["gw"]][[j]] +
        season_irrig_maps[["rabi"]][["gw"]][[j]] +
        season_irrig_maps[["zaid"]][["gw"]][[j]] +
        season_irrig_maps[["continuous"]][["gw"]][[j]]
    sw_irrig_water[[j]] =
        season_irrig_maps[["kharif"]][["sw"]][[j]] +
        season_irrig_maps[["rabi"]][["sw"]][[j]] +
        season_irrig_maps[["zaid"]][["sw"]][[j]] +
        season_irrig_maps[["continuous"]][["sw"]][[j]]
    total_irrig_water[[j]] = gw_irrig_water[[j]] + sw_irrig_water[[j]]
    ## ## Do some checks to make sure the irrigation depths are
    ## ## preserved after decomposition into sw/gw etc.
    ## checkfun = function(maps, season, index) {
    ##     sw = maps[[season]][["sw"]][[index]]
    ##     gw = maps[[season]][["gw"]][[index]]
    ##     calc_total = sw + gw
    ##     total = maps[[season]][["total"]][[index]]
    ##     v1 = getValues(calc_total)
    ##     v2 = getValues(total)
    ##     idx = v1 > 0
    ##     res = all(near(v1[idx], v2[idx], tol=1e-5))
    ##     ## res = all.equal(v1[idx], v2[idx])
    ##     res
    ## }
    ## ## We check the decomposed irrigation values in
    ## ## 11_process-jules-output.py at the daily level,
    ## ## although minor discrepancies may creep in when
    ## ## we convert from kg m-2 s-1 to m/month. Set
    ## ## tolerance at 1e-3 (i.e. 1mm/month), which seems
    ## ## reasonable.
    ## c1 = isTRUE(checkfun(season_irrig_maps, "kharif", j))
    ## c2 = isTRUE(checkfun(season_irrig_maps, "rabi", j))
    ## c3 = isTRUE(checkfun(season_irrig_maps, "zaid", j))
    ## c4 = isTRUE(checkfun(season_irrig_maps, "continuous", j))
    ## irrig_water_check =
    ##     season_irrig_maps[["kharif"]][["total"]][[j]] +
    ##     season_irrig_maps[["rabi"]][["total"]][[j]] +
    ##     season_irrig_maps[["zaid"]][["total"]][[j]] +
    ##     season_irrig_maps[["continuous"]][["total"]][[j]]
    ## v1 = getValues(irrig_water_reference)
    ## v2 = getValues(irrig_water_check)
    ## c5 = all(near(v1, v2, tol=1e-1))
    ## if (!all(c1, c2, c3, c4, c5)) {
    ##     stop("Irrigation depths not preserved!")
    ## }
  }
  list(
    precip = precip,
    surf_roff = surf_roff,
    sub_surf_roff = sub_surf_roff,
    et = et,
    gw_irrig_water = gw_irrig_water,
    sw_irrig_water = sw_irrig_water,
    total_irrig_water = total_irrig_water,
    season_irrig_maps = season_irrig_maps
  )
}

compute_target_canal_area <- function(year_str = "[0-9]{4}",
                                      f_leakage = 0.15,
                                      ...) {
  ## Recharge
  fs <- list.files(
    "../data/analysis/current_canal",
    pattern = paste0("recharge_current_canal_", year_str, "_current.tif"),
    full.names = TRUE
  )
  if (length(fs) > 1) {
    recharge <- stackApply(stack(fs), indices = rep(1, length(fs)), fun = mean)
  } else if (length(fs) == 1) {
    recharge <- raster(fs[1])
  }
  ## Abstraction
  fs <- list.files(
    "../data/analysis/current_canal",
    pattern = paste0("abstraction_current_canal_", year_str, "_current.tif"),
    full.names = TRUE
  )
  ## abstraction <- stackApply(stack(fs), indices = rep(1, length(fs)), fun = mean)
  if (length(fs) > 1) {
    abstraction <- stackApply(stack(fs), indices = rep(1, length(fs)), fun = mean)
  } else if (length(fs) == 1) {
    abstraction <- raster(fs[1])
  }
  ## GW irrigation
  fs <- list.files(
    "../data/analysis/current_canal",
    pattern = paste0("rabi_gw_irrigation_current_canal_", year_str, "_current.tif"),
    full.names = TRUE
  )
  ## rabi_gw_irrigation <- stackApply(stack(fs), indices = rep(1, length(fs)), fun = mean)
  if (length(fs) > 1) {
    rabi_gw_irrigation <- stackApply(stack(fs), indices = rep(1, length(fs)), fun = mean)
  } else if (length(fs) == 1) {
    rabi_gw_irrigation <- raster(fs[1])
  }
  fs <- list.files(
    "../data/analysis/current_canal",
    pattern = paste0("zaid_gw_irrigation_current_canal_", year_str, "_current.tif"),
    full.names = TRUE
  )
  ## zaid_gw_irrigation <- stackApply(stack(fs), indices = rep(1, length(fs)), fun = mean)
  if (length(fs) > 1) {
    zaid_gw_irrigation <- stackApply(stack(fs), indices = rep(1, length(fs)), fun = mean)
  } else if (length(fs) == 1) {
    zaid_gw_irrigation <- raster(fs[1])
  }
  fs <- list.files(
    "../data/analysis/current_canal",
    pattern = paste0("continuous_gw_irrigation_current_canal_", year_str, "_current.tif"),
    full.names = TRUE
  )
  ## continuous_gw_irrigation <- stackApply(stack(fs), indices = rep(1, length(fs)), fun = mean)
  if (length(fs) > 1) {
    continuous_gw_irrigation <- stackApply(stack(fs), indices = rep(1, length(fs)), fun = mean)
  } else if (length(fs) == 1) {
    continuous_gw_irrigation <- raster(fs[1])
  }
  fs <- list.files(
    "../data/analysis/current_canal",
    pattern = paste0("kharif_total_irrigation_current_canal_", year_str, "_current.tif"),
    full.names = TRUE
  )
  ## kharif_total_irrigation <- stackApply(stack(fs), indices = rep(1, length(fs)), fun = mean)
  if (length(fs) > 1) {
    kharif_total_irrigation <- stackApply(stack(fs), indices = rep(1, length(fs)), fun = mean)
  } else if (length(fs) == 1) {
    kharif_total_irrigation <- raster(fs[1])
  }
  f_kharif_gw <- (
    (recharge - rabi_gw_irrigation - zaid_gw_irrigation - continuous_gw_irrigation + f_leakage * kharif_total_irrigation)
    / (kharif_total_irrigation + f_leakage * kharif_total_irrigation)
  )
  f_kharif_gw[f_kharif_gw < 0] <- 0
  f_kharif_gw[f_kharif_gw > 1] <- 1
  f_kharif_sw <- 1 - f_kharif_gw
  list(sw=f_kharif_sw, gw=f_kharif_gw)
}

## ## Precipitation
## fs <- list.files(
##   "../data/analysis/current_canal",
##   pattern = "annual_precip_current_canal_[0-9]{4}_current.tif",
##   full.names = TRUE
## ) %>% sort()
## ## fs <- fs[1:32] # 1979-2010
## yrs <- sub(".*/annual_precip_current_canal_([0-9]{4})_current.tif", "\\1", fs) %>% as.numeric()
## precip <- stack(fs)
## precip <- resample(precip, india_cmd_area)
## precip <- precip * india_cmd_area
## total_precip <- sapply(unstack(precip), FUN=function(x) mean(getValues(x), na.rm = TRUE))
## q10 <- quantile(total_precip, probs = 0.10) # Drier
## q50 <- quantile(total_precip, probs = 0.50) # Median
## q75 <- quantile(total_precip, probs = 0.75) # Median
## q90 <- quantile(total_precip, probs = 0.90) # Wetter
## q10_year <- yrs[which.min(abs(total_precip - q10))]
## q50_year <- yrs[which.min(abs(total_precip - q50))]
## q75_year <- yrs[which.min(abs(total_precip - q75))]
## q90_year <- yrs[which.min(abs(total_precip - q90))]
## ## TODO decide how to use the above information

compute_restored_canal_policy <- function(...) {
  ## Load current canal [to get total irrigated area]
  fs <- list.files(
    path = "../data/irrigated_area_maps",
    pattern = "icrisat_kharif_.*_2010_india_0.500000Deg.tif",
    full.names = TRUE
  )
  total_irrigated_area <- stack(fs) %>% stackApply(indices = rep(1, length(fs)), fun = sum)
  f_kharif <- compute_target_canal_area(f_leakage = 0.15)
  f_kharif_sw <- f_kharif$sw
  f_kharif_gw <- f_kharif$gw

  ## f_kharif_sw <- resample(f_kharif_sw, india_cmd_area)
  ## f_kharif_sw <- f_kharif_sw * india_cmd_area
  ## plot(f_kharif_sw)
  ## writeRaster(f_kharif_sw, "../data/restored_canal_mean.tif", overwrite = TRUE)

  ## Divide between the five irrigation sources considered
  canal_irrigated_area <- total_irrigated_area * f_kharif_sw
  other_sources_irrigated_area <- total_irrigated_area * 0.
  other_wells_irrigated_area <- total_irrigated_area * 0.
  tanks_irrigated_area <- total_irrigated_area * 0.
  tubewell_irrigated_area <- total_irrigated_area * f_kharif_gw

  get_filename <- function(source) {
    sprintf("../data/irrigated_area_maps/icrisat_kharif_%s_2010_india_0.500000Deg_restored_canal.tif", source)
  }
  writeRaster(canal_irrigated_area, get_filename("canal"), overwrite = TRUE)
  writeRaster(other_sources_irrigated_area, get_filename("other_sources"), overwrite = TRUE)
  writeRaster(other_wells_irrigated_area, get_filename("other_wells"), overwrite = TRUE)
  writeRaster(tanks_irrigated_area, get_filename("tanks"), overwrite = TRUE)
  writeRaster(tubewell_irrigated_area, get_filename("tubewells"), overwrite = TRUE)

  ## FIXME - Rabi/zaid/continuous are as for current canal
  ##
  ## Rabi - min canal irrigated area
  fs <- list.files(
    path = "../data/irrigated_area_maps",
    pattern = "icrisat_rabi_.*_2010_india_0.500000Deg.tif",
    full.names = TRUE
  )
  total_irrigated_area <- stack(fs) %>% stackApply(indices = rep(1, length(fs)), fun = sum)
  fs <- list.files(
    path = "../data/irrigated_area_maps",
    pattern = "icrisat_rabi_canal_[0-9]{4}_india_0.500000Deg.tif",
    full.names = TRUE
  )
  min_canal_area <- stack(fs) %>% stackApply(indices = rep(1, length(fs)), fun = min)
  canal_irrigated_area <- min_canal_area
  other_sources_irrigated_area <- total_irrigated_area * 0.
  other_wells_irrigated_area <- total_irrigated_area * 0.
  tanks_irrigated_area <- total_irrigated_area * 0.
  tubewell_irrigated_area <- total_irrigated_area - canal_irrigated_area
  get_filename <- function(source) {
    sprintf("../data/irrigated_area_maps/icrisat_rabi_%s_2010_india_0.500000Deg_restored_canal.tif", source)
  }
  writeRaster(canal_irrigated_area, get_filename("canal"), overwrite = TRUE)
  writeRaster(other_sources_irrigated_area, get_filename("other_sources"), overwrite = TRUE)
  writeRaster(other_wells_irrigated_area, get_filename("other_wells"), overwrite = TRUE)
  writeRaster(tanks_irrigated_area, get_filename("tanks"), overwrite = TRUE)
  writeRaster(tubewell_irrigated_area, get_filename("tubewells"), overwrite = TRUE)

  ## Zaid - no canal irrigated area
  fs <- list.files(
    path = "../data/irrigated_area_maps",
    pattern = "icrisat_zaid_.*_2010_india_0.500000Deg.tif",
    full.names = TRUE
  )
  total_irrigated_area <- stack(fs) %>% stackApply(indices = rep(1, length(fs)), fun = sum)
  canal_irrigated_area <- total_irrigated_area * 0.
  other_sources_irrigated_area <- total_irrigated_area * 0.
  other_wells_irrigated_area <- total_irrigated_area * 0.
  tanks_irrigated_area <- total_irrigated_area * 0.
  tubewell_irrigated_area <- total_irrigated_area - canal_irrigated_area
  get_filename <- function(source) {
    sprintf("../data/irrigated_area_maps/icrisat_zaid_%s_2010_india_0.500000Deg_restored_canal.tif", source)
  }
  writeRaster(canal_irrigated_area, get_filename("canal"), overwrite = TRUE)
  writeRaster(other_sources_irrigated_area, get_filename("other_sources"), overwrite = TRUE)
  writeRaster(other_wells_irrigated_area, get_filename("other_wells"), overwrite = TRUE)
  writeRaster(tanks_irrigated_area, get_filename("tanks"), overwrite = TRUE)
  writeRaster(tubewell_irrigated_area, get_filename("tubewells"), overwrite = TRUE)

  ## Continuous - no canal irrigated area
  fs <- list.files(
    path = "../data/irrigated_area_maps",
    pattern = "icrisat_continuous_.*_2010_india_0.500000Deg.tif",
    full.names = TRUE
  )
  total_irrigated_area <- stack(fs) %>% stackApply(indices = rep(1, length(fs)), fun = sum)
  canal_irrigated_area <- total_irrigated_area * 0.
  other_sources_irrigated_area <- total_irrigated_area * 0.
  other_wells_irrigated_area <- total_irrigated_area * 0.
  tanks_irrigated_area <- total_irrigated_area * 0.
  tubewell_irrigated_area <- total_irrigated_area - canal_irrigated_area
  get_filename <- function(source) {
    sprintf("../data/irrigated_area_maps/icrisat_continuous_%s_2010_india_0.500000Deg_restored_canal.tif", source)
  }
  writeRaster(canal_irrigated_area, get_filename("canal"), overwrite = TRUE)
  writeRaster(other_sources_irrigated_area, get_filename("other_sources"), overwrite = TRUE)
  writeRaster(other_wells_irrigated_area, get_filename("other_wells"), overwrite = TRUE)
  writeRaster(tanks_irrigated_area, get_filename("tanks"), overwrite = TRUE)
  writeRaster(tubewell_irrigated_area, get_filename("tubewells"), overwrite = TRUE)

}

compute_current_canal_policy <- function(...) {
  ## Max kharif canal irrigated area
  fs <- list.files(
    path = "../data/irrigated_area_maps",
    pattern = "icrisat_kharif_.*_2010_india_0.500000Deg.tif",
    full.names = TRUE
  )
  total_irrigated_area <- stack(fs) %>% stackApply(indices = rep(1, length(fs)), fun = sum)
  fs <- list.files(
    path = "../data/irrigated_area_maps",
    pattern = "icrisat_kharif_canal_[0-9]{4}_india_0.500000Deg.tif",
    full.names = TRUE
  )
  max_canal_area <- stack(fs) %>% stackApply(indices = rep(1, length(fs)), fun = max)
  canal_irrigated_area <- stackApply(stack(total_irrigated_area, max_canal_area), indices = rep(1, 1), fun = min)
  other_sources_irrigated_area <- total_irrigated_area * 0.
  other_wells_irrigated_area <- total_irrigated_area * 0.
  tanks_irrigated_area <- total_irrigated_area * 0.
  tubewell_irrigated_area <- total_irrigated_area - canal_irrigated_area
  get_filename <- function(source) {
    sprintf("../data/irrigated_area_maps/icrisat_kharif_%s_2010_india_0.500000Deg_current_canal.tif", source)
  }
  writeRaster(canal_irrigated_area, get_filename("canal"), overwrite = TRUE)
  writeRaster(other_sources_irrigated_area, get_filename("other_sources"), overwrite = TRUE)
  writeRaster(other_wells_irrigated_area, get_filename("other_wells"), overwrite = TRUE)
  writeRaster(tanks_irrigated_area, get_filename("tanks"), overwrite = TRUE)
  writeRaster(tubewell_irrigated_area, get_filename("tubewells"), overwrite = TRUE)

  ## Rabi - min canal irrigated area
  fs <- list.files(
    path = "../data/irrigated_area_maps",
    pattern = "icrisat_rabi_.*_2010_india_0.500000Deg.tif",
    full.names = TRUE
  )
  total_irrigated_area <- stack(fs) %>% stackApply(indices = rep(1, length(fs)), fun = sum)
  fs <- list.files(
    path = "../data/irrigated_area_maps",
    pattern = "icrisat_rabi_canal_[0-9]{4}_india_0.500000Deg.tif",
    full.names = TRUE
  )
  min_canal_area <- stack(fs) %>% stackApply(indices = rep(1, length(fs)), fun = min)
  canal_irrigated_area <- min_canal_area
  other_sources_irrigated_area <- total_irrigated_area * 0.
  other_wells_irrigated_area <- total_irrigated_area * 0.
  tanks_irrigated_area <- total_irrigated_area * 0.
  tubewell_irrigated_area <- total_irrigated_area - canal_irrigated_area
  get_filename <- function(source) {
    sprintf("../data/irrigated_area_maps/icrisat_rabi_%s_2010_india_0.500000Deg_current_canal.tif", source)
  }
  writeRaster(canal_irrigated_area, get_filename("canal"), overwrite = TRUE)
  writeRaster(other_sources_irrigated_area, get_filename("other_sources"), overwrite = TRUE)
  writeRaster(other_wells_irrigated_area, get_filename("other_wells"), overwrite = TRUE)
  writeRaster(tanks_irrigated_area, get_filename("tanks"), overwrite = TRUE)
  writeRaster(tubewell_irrigated_area, get_filename("tubewells"), overwrite = TRUE)

  ## Zaid - no canal irrigated area
  fs <- list.files(
    path = "../data/irrigated_area_maps",
    pattern = "icrisat_zaid_.*_2010_india_0.500000Deg.tif",
    full.names = TRUE
  )
  total_irrigated_area <- stack(fs) %>% stackApply(indices = rep(1, length(fs)), fun = sum)
  canal_irrigated_area <- total_irrigated_area * 0.
  other_sources_irrigated_area <- total_irrigated_area * 0.
  other_wells_irrigated_area <- total_irrigated_area * 0.
  tanks_irrigated_area <- total_irrigated_area * 0.
  tubewell_irrigated_area <- total_irrigated_area - canal_irrigated_area
  get_filename <- function(source) {
    sprintf("../data/irrigated_area_maps/icrisat_zaid_%s_2010_india_0.500000Deg_current_canal.tif", source)
  }
  writeRaster(canal_irrigated_area, get_filename("canal"), overwrite = TRUE)
  writeRaster(other_sources_irrigated_area, get_filename("other_sources"), overwrite = TRUE)
  writeRaster(other_wells_irrigated_area, get_filename("other_wells"), overwrite = TRUE)
  writeRaster(tanks_irrigated_area, get_filename("tanks"), overwrite = TRUE)
  writeRaster(tubewell_irrigated_area, get_filename("tubewells"), overwrite = TRUE)

  ## Continuous - no canal irrigated area
  fs <- list.files(
    path = "../data/irrigated_area_maps",
    pattern = "icrisat_continuous_.*_2010_india_0.500000Deg.tif",
    full.names = TRUE
  )
  total_irrigated_area <- stack(fs) %>% stackApply(indices = rep(1, length(fs)), fun = sum)
  canal_irrigated_area <- total_irrigated_area * 0.
  other_sources_irrigated_area <- total_irrigated_area * 0.
  other_wells_irrigated_area <- total_irrigated_area * 0.
  tanks_irrigated_area <- total_irrigated_area * 0.
  tubewell_irrigated_area <- total_irrigated_area - canal_irrigated_area
  get_filename <- function(source) {
    sprintf("../data/irrigated_area_maps/icrisat_continuous_%s_2010_india_0.500000Deg_current_canal.tif", source)
  }
  writeRaster(canal_irrigated_area, get_filename("canal"), overwrite = TRUE)
  writeRaster(other_sources_irrigated_area, get_filename("other_sources"), overwrite = TRUE)
  writeRaster(other_wells_irrigated_area, get_filename("other_wells"), overwrite = TRUE)
  writeRaster(tanks_irrigated_area, get_filename("tanks"), overwrite = TRUE)
  writeRaster(tubewell_irrigated_area, get_filename("tubewells"), overwrite = TRUE)
}

## ## Next, compute canal area for each year separately
## f_kharif <- compute_target_canal_area(year_str = "1982", f_leakage = 0.15)
## f_kharif_sw <- f_kharif$sw
## f_kharif_gw <- f_kharif$gw

## canal_irrigated_area <- total_irrigated_area * f_kharif_sw
## other_sources_irrigated_area <- total_irrigated_area * 0.
## other_wells_irrigated_area <- total_irrigated_area * 0.
## tanks_irrigated_area <- total_irrigated_area * 0.
## tubewell_irrigated_area <- total_irrigated_area * f_kharif_gw

## get_filename <- function(source) {
##   sprintf("../data/irrigated_area_maps/icrisat_kharif_%s_2010_india_0.500000Deg_restored_canal_q75.tif", source)
## }
## writeRaster(canal_irrigated_area, get_filename("canal"), overwrite = TRUE)
## writeRaster(other_sources_irrigated_area, get_filename("other_sources"), overwrite = TRUE)
## writeRaster(other_wells_irrigated_area, get_filename("other_wells"), overwrite = TRUE)
## writeRaster(tanks_irrigated_area, get_filename("tanks"), overwrite = TRUE)
## writeRaster(tubewell_irrigated_area, get_filename("tubewells"), overwrite = TRUE)

## #################################### ##
## Load data
## #################################### ##

## Pixels belonging to Indo-Gangetic Plain (Indus + Ganges)
land_frac = raster(
    file.path("../data/wfdei/ancils/WFD-EI-LandFraction2d_igp.nc")
)

for (i in 1:length(seasons)) {
    season = seasons[i]
    season_maps[[season]] = stack(lapply(1:12, FUN=function(x) land_frac))
    for (j in 1:12) {
        month = year_months[j]
        if (!month %in% season_months[[season]]) {
            season_maps[[season]][[j]] = season_maps[[season]][[j]] * 0
        }
    }
}

## ## TESTING:
## policies = c("historical")
## years = 1998
## id_stems = c("JULES_vn6.1_irrig")
## Start analysis by policy/year
## for (m in 1:length(policies)) {
for (m in 1:length(policies)) {
  policy = policies[m]
  sprintf("Computing values for %s policy...", policy)
  pb = txtProgressBar(min = 0, max = length(years) - 1, initial = 0)
  for (k in 1:(length(years) - 1)) {
    yr = years[k]
    for (i in 1:length(id_stems)) {
      jules_output <- load_jules_output(id_stems[i], policy, yr)
      precip <- jules_output$precip
      surf_roff <- jules_output$surf_roff
      sub_surf_roff <- jules_output$sub_surf_roff
      et <- jules_output$et
      gw_irrig_water <- jules_output$gw_irrig_water
      sw_irrig_water <- jules_output$sw_irrig_water
      total_irrig_water <- jules_output$total_irrig_water
      season_irrig_maps <- jules_output$season_irrig_maps

      ## Annual total irrigation
      suffix = sub("(JULES)_(vn.*)_([a-z]+)", "\\3", id_stems[i])
      annual_total_irrigation = stackApply(stack(total_irrig_water), indices=rep(1, 12), fun=sum)
      fn <- paste0("annual_total_irrigation_", policy, "_", yr, "_", suffix, ".tif")
      writeRaster(annual_total_irrigation, file.path(analysis_output_dir, policy, fn), overwrite = TRUE)
      ## Annual gw irrigation
      annual_gw_irrigation = stackApply(stack(gw_irrig_water), indices=rep(1, 12), fun=sum)
      fn <- paste0("annual_gw_irrigation_", policy, "_", yr, "_", suffix, ".tif")
      writeRaster(annual_gw_irrigation, file.path(analysis_output_dir, policy, fn), overwrite = TRUE)
      ## Annual sw irrigation
      annual_sw_irrigation = stackApply(stack(sw_irrig_water), indices=rep(1, 12), fun=sum)
      fn <- paste0("annual_sw_irrigation_", policy, "_", yr, "_", suffix, ".tif")
      writeRaster(annual_sw_irrigation, file.path(analysis_output_dir, policy, fn), overwrite = TRUE)
      ## Monthly and season-wise total/gw/sw irrigation
      for (j in 1:length(seasons)) {
        season = seasons[j]
        for (type in c("total", "gw", "sw")) {
          season_irrigation = stackApply(
            stack(season_irrig_maps[[season]][[type]]),
            indices=rep(1, 12),
            fun=sum
          )
          fn <- paste0(season, "_", type, "_irrigation_", policy, "_", yr, "_", suffix, ".tif")
          writeRaster(season_irrigation, file.path(analysis_output_dir, policy, fn), overwrite=TRUE)
        }
      }
      for (j in 1:12) {
        month = year_months[j]
        for (type in c("total", "gw", "sw")) {
          maps = stack(lapply(seasons, FUN=function(season) season_irrig_maps[[season]][[type]][[j]]))
          month_irrigation = stackApply(maps, indices=rep(1, length(seasons)), fun=sum)
          fn <- paste0(type, "_irrigation_", policy, "_", yr, "_", formatC(month, width=2, flag=0), "_", suffix, ".tif")
          writeRaster(month_irrigation, file.path(analysis_output_dir, policy, fn), overwrite = TRUE)
        }
      }
      ## Annual ET
      annual_et = stackApply(stack(et), indices=rep(1, 12), fun=sum)
      fn <- paste0("annual_et_", policy, "_", yr, "_", suffix, ".tif")
      writeRaster(annual_et, file.path(analysis_output_dir, policy, fn), overwrite = TRUE)
      ## Annual precip
      annual_precip = stackApply(stack(precip), indices=rep(1, 12), fun=sum)
      fn <- paste0("annual_precip_", policy, "_", yr, "_", suffix, ".tif")
      writeRaster(annual_precip, file.path(analysis_output_dir, policy, fn), overwrite = TRUE)
      ## Annual surface runoff
      annual_surf_roff = stackApply(stack(surf_roff), indices=rep(1, 12), fun=sum)
      fn <- paste0("annual_surf_roff_", policy, "_", yr, "_", suffix, ".tif")
      writeRaster(annual_surf_roff, file.path(analysis_output_dir, policy, fn), overwrite = TRUE)
      ## Annual subsurface runoff
      annual_sub_surf_roff = stackApply(stack(sub_surf_roff), indices=rep(1, 12), fun=sum)
      fn <- paste0("annual_sub_surf_roff_", policy, "_", yr, "_", suffix, ".tif")
      writeRaster(annual_sub_surf_roff, file.path(analysis_output_dir, policy, fn), overwrite = TRUE)
      ## Recharge
      Qin = stackApply(stack(sub_surf_roff), indices=rep(1, 12), fun=sum)
      fn <- paste0("recharge_", policy, "_", yr, "_", suffix, ".tif")
      writeRaster(Qin, file.path(analysis_output_dir, policy, fn), overwrite = TRUE)
      ## Abstraction
      Qout = stackApply(stack(gw_irrig_water), indices=rep(1, 12), fun=sum)
      fn <- paste0("abstraction_", policy, "_", yr, "_", suffix, ".tif")
      writeRaster(Qout, file.path(analysis_output_dir, policy, fn), overwrite = TRUE)
      ## Change in storage
      dS = Qin - Qout
      fn <- paste0("dS_", policy, "_", yr, "_", suffix, ".tif")
      writeRaster(dS, file.path(analysis_output_dir, policy, fn), overwrite = TRUE)
    }
    setTxtProgressBar(pb, k)
  }
  close(pb)
  ## `current_canal` and `restored_canal` policies are computed on the basis of the previous output
  if (policies[m] == "historical") {
    compute_current_canal_policy()
  } else if (policies[m] == "current_canal") {
    compute_restored_canal_policy()
  }
}

## Now compute the target sw fraction
## f_kharif,gw = (R - I_gw,rabi - I_gw,zaid - I_gw,continuous) / I_total,kharif
## Note these maps still represent depths, but this is OK so long as we are consistent

