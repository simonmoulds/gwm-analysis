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

if (stem %in% c("JULES_vn6.1_irrig_current")) {
  ## First we compute the water balance components based
  ## on the current canal irrigation capacity
  dir.create(
    "results/irrigated_area_maps",
    recursive = TRUE,
    showWarnings = FALSE
  )
  compute_current_canal_policy("results/irrigated_area_maps")
  summarise_water_balance(stem, "current_canal", years, outputdir)

  ## Compute how much canal irrigation needs to be expanded
  compute_restored_canal_policy(
    inputdir = file.path("results", stem),
    outputdir = "results/irrigated_area_maps"
  )
  summarise_water_balance(stem, "restored_canal", years, outputdir)
}

file.create(sprintf("results/compute_water_balance_%s.txt", stem))
