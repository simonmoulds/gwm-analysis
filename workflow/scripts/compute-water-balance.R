## Author : Simon Moulds
## Date   : April 2021

## Extract configuration info
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  stem <- args[1]
  outputdir <- args[2]
  args <- commandArgs()
  m <- regexpr("(?<=^--file=).+", args, perl = TRUE)
  cwd <- dirname(regmatches(args, m))
}
## Load custom utilities
source(file.path(cwd, "utils.R"))

## TODO this has been referred to before - put in config
reference_year <- 2010
start_year <- 1979
end_year <- 2014
years <- seq(start_year, end_year)

if (stem %in% c("JULES_vn6.1_irrig", "JULES_vn6.1_noirrig")) {
  summarise_water_balance(stem, "historical", years, outputdir)
}

## Irrigation policy maps:
## icrisat_{season}_{source}_{reference_year}_india_0.500000Deg_current_canal.tif
## icrisat_{season}_{source}_{reference_year}_india_0.500000Deg_restored_canal.tif

if (stem %in% c("JULES_vn6.1_irrig_current")) {

  ## First we compute the water balance components based
  ## on the current canal irrigation capacity
  dir.create(
    "results/irrigated_area_maps",
    recursive = TRUE,
    showWarnings = FALSE
  )
  ## This takes the current canal area and a
  ## best guess estimate of canal leakage
  compute_current_canal_policy("results/irrigated_area_maps")
  summarise_water_balance(stem, "current_canal", years, outputdir)

  ## Next we compute by how much canal irrigation
  ## needs to be expanded to meet demand. This has
  ## two steps. First we compute some policies of
  ## expanding canal area. Then we estimate the
  ## amount of leakage that would be required.
  compute_restored_canal_policy(
    inputdir = file.path("results", stem),
    outputdir = "results/irrigated_area_maps",
    f_leakage = 0.15
  )
  summarise_water_balance(stem, "restored_canal", years, outputdir)
}

file.create(sprintf("results/compute_water_balance_%s.txt", stem))
