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
} else { 
  stem <- "JULES_vn6.1_irrig_current"
  outputdir <- paste0("results/", stem)
  cwd <- "workflow/scripts"
}

## Load custom utilities
source(file.path(cwd, "utils.R"))

## TODO this has been referred to before - put in config
reference_year <- 2010
start_year <- 1979
end_year <- 2014
years <- seq(start_year, end_year)

## These are historical scenarios
if (stem %in% c("JULES_vn6.1_irrig", "JULES_vn6.1_noirrig")) {
  summarise_water_balance(
    outputdir, stem, "historical", years,
    fixed_leakage = TRUE,
    f_leakage = 0.
  )
}

## Policy scenarios
if (stem %in% c("JULES_vn6.1_irrig_current")) {

  ## Ensure output directory exists
  dir.create(
    "results/irrigated_area_maps",
    recursive = TRUE,
    showWarnings = FALSE
  )

  ## First we compute the water balance components based
  ## on the current canal irrigation capacity

  ## This takes the current canal area and a best guess
  ## estimate of canal leakage. It outputs maps with the
  ## following filename structure:
  ## icrisat_{season}_{source}_2010_india_0.500000Deg_current_canal.tif
  ## TODO this currently assumes canal irrigated area is the
  ## maximum canal irrigated area during the study period,
  ## which seems overly generous
  compute_current_canal_policy("results/irrigated_area_maps")

  ## Then we compute the water balance over the study period
  ## considering the above irrigation sources
  summarise_water_balance(
    outputdir, stem, "current_canal", years,
    fixed_leakage = TRUE,
    f_leakage = 0.
  )

  ## Next we compute by how much canal irrigation
  ## needs to be expanded to meet demand. This has
  ## two steps. First we compute some policies of
  ## expanding canal area. Then we estimate the
  ## amount of leakage that would be required.
  compute_restored_canal_policy("results/irrigated_area_maps", sf = 0.5)

  ## As above, we now compute the water balance
  ## considering the above computed irrigation sources.
  ## This time, we allow the leakage to vary to the extent
  ## needed to achieve a water balance in a given year.
  summarise_water_balance(
    outputdir, stem, "restored_canal", years,
    fixed_leakage = FALSE,
    f_leakage_max = Inf
  )
}

file.create(sprintf("results/compute_water_balance_%s.txt", stem))
