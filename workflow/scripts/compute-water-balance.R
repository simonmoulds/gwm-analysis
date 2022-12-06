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

## Extract configuration info
if (sys.nframe() == 0L) {
  args = commandArgs(trailingOnly=TRUE)
  inputdir = args[1]
  stem = args[2]
  outputdir = args[3]
  args = commandArgs()
  m <- regexpr("(?<=^--file=).+", args, perl=TRUE)
  cwd <- dirname(regmatches(args, m))
}
## Load custom utilities
source(file.path(cwd, "utils.R"))

## Irrgation sources
irrigation_sources = c(
  "canal", "other_sources", "other_wells", "tanks", "tubewells"
)

## Policies, reference year for policies
policies = c("historical", "current_canal", "restored_canal")
for (policy in policies) {
  dir.create(file.path(outputdir, policy), recursive=TRUE, showWarnings = FALSE)
}

## TODO this has been referred to before - put in config
reference_year = 2010

## JULES simulation details
## *_irrig         : historical irrigated area
## *_irrig_current : current irrigated area
## *_noirrig       : no irrigation
## TODO put these in config
## job_name = "jules_%s"
## profile_name = "daily_hydrology"
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

## #################################### ##
## Load data
## #################################### ##

## Pixels belonging to Indo-Gangetic Plain (Indus + Ganges)
land_frac = raster(
  file.path("resources/wfdei/ancils/WFD-EI-LandFraction2d_igp.nc")
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
for (m in 1:length(policies)) { # historical, current_canal, restored_canal
  policy = policies[m]
  if (!dir.exists(file.path(outputdir, policy))) {

  }
  sprintf("Computing values for %s policy...", policy)
  pb = txtProgressBar(min = 0, max = length(years) - 1, initial = 0)
  for (k in 1:(length(years) - 1)) {
    yr = years[k]
    ## for (i in 1:length(id_stems)) {
    ## jules_output <- load_jules_output(id_stems[i], policy, yr)
    jules_output <- load_jules_output(stem, policy, yr)
    precip <- jules_output$precip
    surf_roff <- jules_output$surf_roff
    sub_surf_roff <- jules_output$sub_surf_roff
    et <- jules_output$et
    gw_irrig_water <- jules_output$gw_irrig_water
    sw_irrig_water <- jules_output$sw_irrig_water
    total_irrig_water <- jules_output$total_irrig_water
    season_irrig_maps <- jules_output$season_irrig_maps

    ## ############################# ##
    ## Annual total irrigation       ##
    ## ############################# ##
    suffix = sub("(JULES)_(vn.*)_([a-z]+)", "\\3", id_stems[i])
    annual_total_irrigation = stackApply(
      stack(total_irrig_water),
      indices=rep(1, 12),
      fun=sum
    )
    fn <- paste0(
      "annual_total_irrigation_", policy, "_", yr, "_", suffix, ".tif"
    )
    writeRaster(
      annual_total_irrigation,
      file.path(outputdir, policy, fn),
      overwrite = TRUE
    )

    ## ############################# ##
    ## Annual gw irrigation
    ## ############################# ##
    annual_gw_irrigation = stackApply(
      stack(gw_irrig_water),
      indices=rep(1, 12),
      fun=sum
    )
    fn <- paste0(
      "annual_gw_irrigation_", policy, "_", yr, "_", suffix, ".tif"
    )
    writeRaster(
      annual_gw_irrigation,
      file.path(outputdir, policy, fn),
      overwrite = TRUE
    )

    ## ############################# ##
    ## Annual sw irrigation
    ## ############################# ##
    annual_sw_irrigation = stackApply(
      stack(sw_irrig_water),
      indices=rep(1, 12),
      fun=sum
    )
    fn <- paste0(
      "annual_sw_irrigation_", policy, "_", yr, "_", suffix, ".tif"
    )
    writeRaster(
      annual_sw_irrigation,
      file.path(outputdir, policy, fn),
      overwrite = TRUE
    )

    ## ############################# ##
    ## Monthly and season-wise totals
    ## ############################# ##
    for (j in 1:length(seasons)) {
      season = seasons[j]
      for (type in c("total", "gw", "sw")) {
        season_irrigation = stackApply(
          stack(season_irrig_maps[[season]][[type]]),
          indices=rep(1, 12),
          fun=sum
        )
        fn <- paste0(
          season, "_", type, "_irrigation_",
          policy, "_", yr, "_", suffix, ".tif"
        )
        writeRaster(
          season_irrigation,
          file.path(outputdir, policy, fn),
          overwrite=TRUE
        )
      }
    }
    for (j in 1:12) {
      month = year_months[j]
      for (type in c("total", "gw", "sw")) {
        maps = stack(
          lapply(seasons, FUN=function(season)
            season_irrig_maps[[season]][[type]][[j]])
        )
        month_irrigation = stackApply(
          maps,
          indices=rep(1, length(seasons)),
          fun=sum
        )
        fn <- paste0(
          type, "_irrigation_", policy, "_", yr, "_",
          formatC(month, width=2, flag=0), "_", suffix, ".tif"
        )
        writeRaster(
          month_irrigation,
          file.path(outputdir, policy, fn),
          overwrite = TRUE
        )
      }
    }

    ## ############################# ##
    ## Annual ET
    ## ############################# ##
    annual_et = stackApply(stack(et), indices=rep(1, 12), fun=sum)
    fn <- paste0("annual_et_", policy, "_", yr, "_", suffix, ".tif")
    writeRaster(
      annual_et,
      file.path(outputdir, policy, fn),
      overwrite = TRUE
    )

    ## ############################# ##
    ## Annual precip
    ## ############################# ##
    annual_precip = stackApply(stack(precip), indices=rep(1, 12), fun=sum)
    fn <- paste0("annual_precip_", policy, "_", yr, "_", suffix, ".tif")
    writeRaster(
      annual_precip,
      file.path(outputdir, policy, fn),
      overwrite = TRUE
    )

    ## ############################# ##
    ## Annual surface runoff
    ## ############################# ##
    annual_surf_roff = stackApply(stack(surf_roff), indices=rep(1, 12), fun=sum)
    fn <- paste0("annual_surf_roff_", policy, "_", yr, "_", suffix, ".tif")
    writeRaster(
      annual_surf_roff,
      file.path(outputdir, policy, fn),
      overwrite = TRUE
    )

    ## ############################# ##
    ## Annual subsurface runoff
    ## ############################# ##
    annual_sub_surf_roff = stackApply(stack(sub_surf_roff), indices=rep(1, 12), fun=sum)
    fn <- paste0("annual_sub_surf_roff_", policy, "_", yr, "_", suffix, ".tif")
    writeRaster(
      annual_sub_surf_roff,
      file.path(outputdir, policy, fn),
      overwrite = TRUE
    )

    ## ############################# ##
    ## Recharge
    ## ############################# ##
    Qin = stackApply(stack(sub_surf_roff), indices=rep(1, 12), fun=sum)
    fn <- paste0("recharge_", policy, "_", yr, "_", suffix, ".tif")
    writeRaster(
      Qin,
      file.path(outputdir, policy, fn),
      overwrite = TRUE
    )

    ## ############################# ##
    ## Abstraction
    ## ############################# ##
    Qout = stackApply(stack(gw_irrig_water), indices=rep(1, 12), fun=sum)
    fn <- paste0("abstraction_", policy, "_", yr, "_", suffix, ".tif")
    writeRaster(
      Qout,
      file.path(outputdir, policy, fn),
      overwrite = TRUE
    )

    ## ############################# ##
    ## Change in storage
    ## ############################# ##
    dS = Qin - Qout
    fn <- paste0("dS", policy, "_", yr, "_", suffix, ".tif")
    writeRaster(
      dS,
      file.path(outputdir, policy, fn),
      overwrite = TRUE
    )
    setTxtProgressBar(pb, k)
  }
  close(pb)
  ## `current_canal` and `restored_canal` policies are
  ## computed on the basis of the previous output
  if (policies[m] == "historical") {
    compute_current_canal_policy(inputdir)
  } else if (policies[m] == "current_canal") {
    compute_restored_canal_policy(inputdir)
  }
}





## NOT USED:
##
## Now compute the target sw fraction
## f_kharif,gw = (R - I_gw,rabi - I_gw,zaid - I_gw,continuous) / I_total,kharif
## Note these maps still represent depths, but this is OK so long as we are consistent

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
