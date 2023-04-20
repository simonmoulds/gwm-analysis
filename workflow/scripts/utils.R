## Author : Simon Moulds
## Date   : October 2021

library(ncdf4)
library(ncdf4.helpers)
library(raster)
library(magrittr)
library(sf)
# library(rnaturalearth)
# library(rnaturalearthdata)
library(rgdal)
library(dplyr)


## Global variables
irrigation_sources <- c(
  "canal", "other_sources", "other_wells", 
  "tanks", "tubewells"
)
seasons <- c("continuous", "kharif", "rabi", "zaid")
year_months <- 11:22 # Nov-Oct (year+1)
reference_year <- 2010

## Function definitions 
compute_basin_total <- function(fn, basin) {
  r = raster(fn)
  r = resample(r, ganges_basin, method="ngb")
  r = r / 1000 # m -> km
  basin_area = basin * grid_cell_area
  r = r * basin_area
  basin_sum = sum(getValues(r), na.rm=TRUE) # km3
  basin_area = sum(getValues(basin_area), na.rm=TRUE)
  basin_avg = (basin_sum / (basin_area)) * 1000
  basin_avg
}



get_jules_month_data <- function(datadir,
                                 year,
                                 month,
                                 varname,
                                 id_stem,
                                 job_name,
                                 profile_name) {

    # This function loads monthly JULES output to a raster
    if (month > 12) {
        month <- month - 12
        year_offset <- 1
    } else {
        year_offset <- 0
    }
    fn <- paste0(
        id_stem,
        ".", sprintf(job_name, year + year_offset),
        ".", profile_name,
        ".", year + year_offset,
        ".2D.month.nc"
    )
    jules <- nc_open(file.path(datadir, fn))
    var <- ncvar_get(jules, varname, start=c(1, 1, month), count=c(80, 40, 1))
    var <- aperm(var, c(2, 1))
    var <- var[rev(seq_len(nrow(var))), ]
    r <- raster(
        nrows = 40, ncols = 80, 
        xmn = 60, xmx = 100, ymn = 20, ymx = 40
    )
    r[] <- var
    r
}

get_jules_month_irrig_rel_data <- function(datadir,
                                           year,
                                           month,
                                           varname,
                                           id_stem,
                                           job_name,
                                           profile_name) {

    if (month > 12) {
        month <- month - 12
        year_offset <- 1
    } else {
        year_offset <- 0
    }
    fn <- paste0(
        id_stem,
        ".", sprintf(job_name, year + year_offset),
        ".rel_irrig_water",
        ".", year + year_offset,
        ".2D.month.nc"
    )
    if (file.exists(file.path("results", id_stem, fn))) {
        jules <- nc_open(file.path("results", id_stem, fn))
        var <- ncvar_get(
            jules,
            varname,
            start = c(1, 1, 1, month),
            count = c(80, 40, 15, 1)
        )
        var <- aperm(var, c(3, 2, 1))
        var <- var[, rev(seq_len(dim(var)[2])), ]
    } else {
        var <- array(data = 0, dim = c(15, 40, 80))
    }
    template <- raster(
        nrows = 40, ncols = 80, 
        xmn = 60, xmx = 100, ymn = 20, ymx = 40
    )
    rlist <- list()
    for (i in 1:15) {
        r <- template
        r[] <- var[i, , ]
        rlist[[i]] <- r
    }
    st <- stack(rlist)
    st
}

get_irr_frac <- function(year,
                         season,
                         policy = "current_canal") {

  if (policy == "historical") {
    suffix <- ".tif"
    mapdir <- "resources/irrigated_area_maps"
  } else {
    suffix <- paste0("_", policy, ".tif")
    mapdir <- "results/irrigated_area_maps"
  }
  maps = list()
  for (i in 1:length(irrigation_sources)) {
    source <- irrigation_sources[i]
    fn <- file.path(
      mapdir,
      paste0(
        "icrisat_", season, "_", source, "_",
        year, "_india_0.500000Deg", suffix
      )
    )
    maps[[source]] <- raster(fn)
  }
  maps <- stack(maps)
  maps
}

get_sw_gw_irr_frac <- function(year, season, policy) {
    maps <- get_irr_frac(year, season, policy)
    sw_maps <- maps[[c("canal","other_sources","tanks")]]
    gw_maps <- maps[[c("other_wells","tubewells")]]
    sw_total <- stackApply(sw_maps, indices=rep(1, 3), fun=sum)
    gw_total <- stackApply(gw_maps, indices=rep(1, 2), fun=sum)
    total <- stackApply(maps, indices=rep(1, 5), fun=sum)
    sw_total <- sw_total / total
    sw_total[!is.finite(sw_total)] = 0
    gw_total <- gw_total / total
    gw_total[!is.finite(gw_total)] = 0    
    list(sw = sw_total, gw = gw_total, total = total)
}

get_nia_gia <- function(year, policy) {
    maps <- vector("list", 4) %>% setNames(seasons)
    for (i in 1:length(seasons)) {
        season <- seasons[i]
        maps[[season]] <- get_sw_gw_irr_frac(year, season, policy)
    }
    types <- c("gw", "sw", "total")
    nia <- vector("list", 3) %>% setNames(types)
    gia <- vector("list", 3) %>% setNames(types)
    for (i in 1:length(types)) {
        type <- types[i]
        st <- stack(lapply(maps, FUN=function(x) x[[type]]))
        nia[[type]] <- stackApply(st, indices=rep(1, 4), fun=max)
        gia[[type]] <- stackApply(st, indices=rep(1, 4), fun=sum)
    }
    list(gia = gia, nia = nia)
}

load_jules_output <- function(datadir,
                              id_stem,
                              policy,
                              yr, ...) {

  ## FIXME - make these arguments to function
  job_name <- "jules_%s"
  profile_name <- "daily_hydrology"

  irri_yr <- ifelse(policy == "historical", yr, reference_year)
  ## `get_sw_gw_irr_frac()` returns a list with elements:
  ## gw    : Fraction of grid cell irrigated with groundwater
  ## sw    : Fraction of grid cell irrigated with surface water
  ## total : Total irrigation
  kharif_irr <- get_sw_gw_irr_frac(irri_yr, "kharif", policy)
  rabi_irr <- get_sw_gw_irr_frac(irri_yr, "rabi", policy)
  zaid_irr <- get_sw_gw_irr_frac(irri_yr, "zaid", policy)
  continuous_irr <- get_sw_gw_irr_frac(irri_yr, "continuous", policy)

  ## Preallocate lists to store values
  precip <- vector("list", 12)
  surf_roff <- vector("list", 12)
  sub_surf_roff <- vector("list", 12)
  et <- vector("list", 12)
  pet <- vector("list", 12)
  gw_irrig_water <- vector("list", 12)
  sw_irrig_water <- vector("list", 12)
  total_irrig_water <- vector("list", 12)

  ## Season irrigation - each season divided between gw/sw/total
  season_irrig_maps <- vector("list", 4) %>% setNames(seasons)
  for (j in 1:length(seasons)) {
    season <- seasons[j]
    season_irrig_maps[[season]] <-
      lapply(1:3, FUN = function(x) vector("list", 12)) %>%
      setNames(c("total", "sw", "gw"))
  }

  ## Loop through months and calculate each variable
  for (j in 1:12) {
    month <- year_months[j]
    precip[[j]] <- get_jules_month_data(
      datadir, yr, month, "precip", id_stem, job_name, profile_name
    )
    sub_surf_roff[[j]] <- get_jules_month_data(
        datadir, yr, month, "sub_surf_roff", id_stem, job_name, profile_name
    )
    surf_roff[[j]] <- get_jules_month_data(
        datadir, yr, month, "surf_roff", id_stem, job_name, profile_name
    )
    esoil <- get_jules_month_data(
        datadir, yr, month, "esoil_gb", id_stem, job_name, profile_name
    )
    ecan <- get_jules_month_data(
        datadir, yr, month, "ecan_gb", id_stem, job_name, profile_name
    )
    elake <- get_jules_month_data(
        datadir, yr, month, "elake", id_stem, job_name, profile_name
    )
    et[[j]] <- esoil + ecan + elake
    pet[[j]] <- get_jules_month_data(
        datadir, yr, month, "fao_et0", id_stem, job_name, profile_name
    )
    irrig_water <- get_jules_month_irrig_rel_data(
        datadir, yr, month, "irrig_water", id_stem, job_name, profile_name
    )
    # irrig_water_reference <- get_jules_month_data(
    #     datadir, yr, month, "irrig_water", id_stem, job_name, profile_name
    # )
    irrig_water_total <- stackApply(
        irrig_water,
        indices = rep(1, nlayers(irrig_water)),
        fun = sum
    )
    ## 7/8/9/10 are the indices of the respective land fraction in JULES
    kharif_index <- (irrig_water[[7]] > 0)
    rabi_index <- (irrig_water[[8]] > 0) & (!kharif_index)
    zaid_index <- (irrig_water[[9]] > 0) & (!(kharif_index | rabi_index))
    continuous_index <- (irrig_water[[10]] > 0)
    ## Compute kharif irrigation among each irrigated land cover
    kharif_irrig_water <- stackApply(
        irrig_water[[7:9]],
        indices = rep(1, 3),
        fun = sum
    ) * kharif_index
    season_irrig_maps[["kharif"]][["gw"]][[j]] <- (
        kharif_irrig_water * kharif_irr[["gw"]]
    )
    season_irrig_maps[["kharif"]][["sw"]][[j]] <- (
        kharif_irrig_water * kharif_irr[["sw"]]
    )
    season_irrig_maps[["kharif"]][["total"]][[j]] <- kharif_irrig_water
    rabi_irrig_water <- stackApply(
        irrig_water[[8:9]],
        indices=rep(1, 2),
        fun=sum
    ) * rabi_index
    season_irrig_maps[["rabi"]][["gw"]][[j]] <- (
        rabi_irrig_water * rabi_irr[["gw"]]
    )
    season_irrig_maps[["rabi"]][["sw"]][[j]] <- (
        rabi_irrig_water * rabi_irr[["sw"]]
    )
    season_irrig_maps[["rabi"]][["total"]][[j]] <- rabi_irrig_water
    zaid_irrig_water <- irrig_water[[9]] * zaid_index
    season_irrig_maps[["zaid"]][["gw"]][[j]] <- (
        zaid_irrig_water * zaid_irr[["gw"]]
    )
    season_irrig_maps[["zaid"]][["sw"]][[j]] <- (
        zaid_irrig_water * zaid_irr[["sw"]]
    )
    season_irrig_maps[["zaid"]][["total"]][[j]] <- zaid_irrig_water
    continuous_irrig_water <- irrig_water[[10]] * continuous_index
    season_irrig_maps[["continuous"]][["gw"]][[j]] <- (
        continuous_irrig_water * continuous_irr$gw
    )
    season_irrig_maps[["continuous"]][["sw"]][[j]] <- (
        continuous_irrig_water * continuous_irr$sw
    )
    season_irrig_maps[["continuous"]][["total"]][[j]] <- continuous_irrig_water
    ## Source totals
    gw_irrig_water[[j]] <-
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
    pet = pet,
    gw_irrig_water = gw_irrig_water,
    sw_irrig_water = sw_irrig_water,
    total_irrig_water = total_irrig_water,
    season_irrig_maps = season_irrig_maps
  )
}

compute_restored_canal_policy <- function(datadir, sf = 0.5) {

  ## ##################################################### ##
  ## Step 1: Load irrigated areas under current policy
  ## ##################################################### ##

  irrigation_sources <- c(
    "canal", "other_sources", "other_wells", "tanks", "tubewells"
  )
  irrigation_source_maps <- list()
  file_ptn <- "icrisat_kharif_%s_2010_india_0.500000Deg_current_canal.tif"
  for (i in 1:length(irrigation_sources)) {
    source <- irrigation_sources[i]
    fn <- file.path(
      "results/irrigated_area_maps", sprintf(file_ptn, source)
    )
    irrigation_source_maps[[source]] <- raster(fn)
  }
  irrigation_source_maps <- stack(irrigation_source_maps)

  ## ##################################################### ##
  ## Step 2: Load canal command areas
  ## ##################################################### ##

  command_areas <- st_read("resources/irrigation/command_areas.shp")
  pakistan_ids = c(
    1, 2, 3, 4, 5, 8, 9, 10, 11,
    12, 13, 14, 16, 20, 37, 41
  )
  india_command_areas <-
    command_areas %>%
    filter(!ID %in% pakistan_ids)

  india_frac <- raster("resources/india_frac_0.500000Deg.tif")
  wfdei_frac <- raster("resources/WFD-EI-LandFraction2d_IGP.tif")
  india_canal_frac <- raster(wfdei_frac)
  india_canal_frac[wfdei_frac] <- 0

  ## Compile list of maps showing the fractional grid cell
  ## area belonging to each canal command area
  command_area_ids <- india_command_areas$ID
  command_area_maps <- lapply(command_area_ids, FUN=function(x) india_canal_frac)
  for (i in 1:length(command_area_ids)) {
    id <- command_area_ids[i]
    ar <- india_command_areas %>% filter(ID %in% id)
    ext <- extent(ar)
    xmn <- floor(ext@xmin)
    xmx <- ceiling(ext@xmax)
    ymn <- floor(ext@ymin)
    ymx <- ceiling(ext@ymax)
    fine_template <- raster(
      nrows = (ymx - ymn) * 120,
      ncols = (xmx - xmn) * 120,
      xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx
    )
    r <- rasterize(ar, fine_template)
    r[is.na(r)] <- 0
    frac <- raster::aggregate(r, fact=60, fun=mean)
    pts <- as(frac, "SpatialPoints")
    india_canal_frac[pts] <- india_canal_frac[pts] + frac[pts]
    command_area_maps[[i]][pts] <- command_area_maps[[i]][pts] + frac[pts]
  }

  india_canal_frac[india_canal_frac == 0] <- NA
  pts <- as(india_canal_frac, "SpatialPoints")
  india_canal_frac_adj <- india_canal_frac
  india_canal_frac_adj[pts] <- pmin(india_canal_frac[pts], india_frac[pts])

  ## Scale accordingly to ensure that in any given grid cell
  ## the canal fraction does not exceed the fraction
  ## belonging to India
  for (i in 1:length(command_area_ids)) {
    command_area_maps[[i]] <- (
      (command_area_maps[[i]] / india_canal_frac) * india_canal_frac_adj
    )
  }
  india_canal_frac <- stackApply(
    stack(command_area_maps),
    indices=rep(1, length(command_area_maps)), fun=sum
  )

  ## ##################################################### ##
  ## Step 3: Compute restored area
  ## ##################################################### ##

  restored_irrigation_source_maps <-
    irrigation_source_maps %>%
    unstack %>%
    setNames(irrigation_sources)

  for (i in 1:length(command_area_maps)) {
    ## Fraction of each cell belonging to current grid square
    ar <- command_area_maps[[i]]
    ar[ar == 0] <- NA
    pts <- as(ar, "SpatialPoints") # this removes NA cells
    frac <- ar[pts]
    ## Area irrigated by canals in current command area
    current_area <-
      irrigation_source_maps[pts] %>%
      as_tibble %>%
      mutate(across(all_of(names(.)), ~.* frac))
    total_canal_area <- current_area[["canal"]]
    total_irrigated_area <-
      current_area %>%
      apply(1, sum)
    total_not_canal_area <-
      current_area %>%
      dplyr::select(-canal) %>% apply(1, sum)
    max_increase <- total_irrigated_area - total_canal_area

    ## There are various ways of increasing the canal irrigated area.
    ## Here we impose a minimum fraction of the total irrigated area,
    ## unless the current irrigated area is zero, in which case we
    ## assume the area will remain at zero.

    ## ## Method 1: impose minimum fraction
    ## ## Current fraction
    ## cf <- total_canal_area / total_irrigated_area
    ## zero_ix <- cf == 0.
    ## nf <- pmax(cf, 0.5)
    ## nf[zero_ix] <- 0
    ## restored_area <- total_irrigated_area * nf

    ## Method 2: reduce deficit by a specified fraction
    ## Multiply max_increase by scale factor
    increase_area <- max_increase * sf
    restored_area <-
      current_area %>%
      mutate(canal = canal + increase_area)

    remaining_area <- total_irrigated_area - restored_area$canal
    ## Convert other sources to relative values
    other_sources <- c("other_sources", "other_wells", "tanks", "tubewells")
    restored_area <-
      restored_area %>%
      mutate(across(!canal, ~./total_not_canal_area)) %>%
      mutate(across(!canal, ~.*remaining_area))
    new_total_irrigated_area <- apply(restored_area, 1, sum, na.rm = TRUE)
    new_total_irrigated_area <-
      new_total_irrigated_area %>%
      `[<-`(!is.finite(.), 0)

    if (!isTRUE(all.equal(total_irrigated_area, new_total_irrigated_area))) {
      stop("New irrigated area does not equal old irrigated area")
    }
    for (source in irrigation_sources) {
      map <- restored_irrigation_source_maps[[source]]
      map[pts] <- (map[pts] * (1 - frac)) + restored_area[[source]]
      restored_irrigation_source_maps[[source]] <- map
    }
  }

  file_ptn <- "icrisat_%s_%s_2010_india_0.500000Deg_restored_canal.tif"
  for (i in 1:length(irrigation_sources)) {
    source <- irrigation_sources[i]
    writeRaster(
      restored_irrigation_source_maps[[source]],
      file.path(datadir, sprintf(file_ptn, "kharif", source)),
      overwrite=TRUE
    )

    ## Copy maps for other seasons from current canal area
    for (season in c("rabi", "zaid", "continuous")) {
      fn0 <- file.path(
        "resources/irrigated_area_maps",
        sprintf(
          "icrisat_%s_%s_2010_india_0.500000Deg_current_canal.tif",
          season, source
        )
      )
      fn1 <- file.path(
        datadir,
        sprintf(file_ptn, season, source)
      )
      cmd <- paste0("cp ", fn0, " ", fn1)
      system(cmd)
    }
  }
}

## compute_target_canal_area <- function(datadir,
##                                       year_str = "[0-9]{4}",
##                                       f_leakage = 0.15,
##                                       ...) {

##   load_wb_component <- function(component, ...) {
##     ptn <- paste0("^", component, "_current_canal_", year_str, "_.*.tif$")
##     fs <- list.files(datadir, ptn, full.names = TRUE)
##     if (length(fs) > 1) {
##       x <- stackApply(stack(fs), indices = rep(1, length(fs)), fun = mean)
##     } else if (length(fs) == 1) {
##       x <- raster(fs[1])
##     }
##     x
##   }
##   recharge <- load_wb_component("recharge")
##   abstraction <- load_wb_component("abstraction")
##   rabi_gw_irrigation <- load_wb_component("rabi_gw_irrigation")
##   zaid_gw_irrigation <- load_wb_component("zaid_gw_irrigation")
##   continuous_gw_irrigation <- load_wb_component("continuous_gw_irrigation")
##   kharif_total_irrigation <- load_wb_component("kharif_total_irrigation")
##   NULL
##   ## ## This estimates area given an estimate of leakage
##   ## f_kharif_gw <- (
##   ##   (recharge - rabi_gw_irrigation - zaid_gw_irrigation - continuous_gw_irrigation + f_leakage * kharif_total_irrigation)
##   ##   / (kharif_total_irrigation + f_leakage * kharif_total_irrigation)
##   ## )
##   ## f_kharif_gw[f_kharif_gw < 0] <- 0
##   ## f_kharif_gw[f_kharif_gw > 1] <- 1
##   ## f_kharif_sw <- 1 - f_kharif_gw
##   ## list(sw=f_kharif_sw, gw=f_kharif_gw)
## }

## ## compute_restored_canal_policy <- function(inputdir, outputdir, f_leakage, ...) {
## compute_restored_canal_policy <- function(inputdir, outputdir, ...) {

##   ## ## Load current canal [to get total irrigated area]
##   ## fs <- list.files(
##   ##   path = "resources/irrigated_area_maps",
##   ##   pattern = "icrisat_kharif_.*_2010_india_0.500000Deg.tif",
##   ##   full.names = TRUE
##   ## )
##   ## total_irrigated_area <- stack(fs) %>%
##   ##   stackApply(indices = rep(1, length(fs)), fun = sum)

##   ## TODO `summarise_water_balance` doesn't seem to account for canal leakage???
##   ## TODO adapt `compute_target_canal_area` to compute optimum leakage
##   f_kharif <- compute_target_canal_area(
##     inputdir,
##     f_leakage = f_leakage
##   )
##   f_kharif_sw <- f_kharif$sw
##   f_kharif_gw <- f_kharif$gw

##   ## ## f_kharif_sw <- resample(f_kharif_sw, india_cmd_area)
##   ## ## f_kharif_sw <- f_kharif_sw * india_cmd_area
##   ## ## plot(f_kharif_sw)
##   ## ## writeRaster(f_kharif_sw, "../data/restored_canal_mean.tif", overwrite = TRUE)

##   ## ## Divide between the five irrigation sources considered
##   ## canal_irrigated_area <- total_irrigated_area * f_kharif_sw
##   ## other_sources_irrigated_area <- total_irrigated_area * 0.
##   ## other_wells_irrigated_area <- total_irrigated_area * 0.
##   ## tanks_irrigated_area <- total_irrigated_area * 0.
##   ## tubewell_irrigated_area <- total_irrigated_area * f_kharif_gw

##   ## ## FIXME shouldn't be writing to irrigated_area_maps
##   ## get_filename <- function(source) {
##   ##   fn <- sprintf("icrisat_kharif_%s_2010_india_0.500000Deg_restored_canal.tif", source)
##   ##   ## return(file.path("resources/irrigated_area_maps", fn))
##   ##   return(file.path(outputdir, fn))
##   ## }
##   ## writeRaster(
##   ##   canal_irrigated_area,
##   ##   get_filename("canal"),
##   ##   overwrite = TRUE
##   ## )
##   ## writeRaster(
##   ##   other_sources_irrigated_area,
##   ##   get_filename("other_sources"),
##   ##   overwrite = TRUE
##   ## )
##   ## writeRaster(
##   ##   other_wells_irrigated_area,
##   ##   get_filename("other_wells"),
##   ##   overwrite = TRUE
##   ## )
##   ## writeRaster(
##   ##   tanks_irrigated_area,
##   ##   get_filename("tanks"),
##   ##   overwrite = TRUE
##   ## )
##   ## writeRaster(
##   ##   tubewell_irrigated_area,
##   ##   get_filename("tubewells"),
##   ##   overwrite = TRUE
##   ## )

##   ## ## FIXME - Rabi/zaid/continuous are as for current canal
##   ## ##
##   ## ## Rabi - min canal irrigated area
##   ## fs <- list.files(
##   ##   path = "resources/irrigated_area_maps",
##   ##   pattern = "icrisat_rabi_.*_2010_india_0.500000Deg.tif",
##   ##   full.names = TRUE
##   ## )
##   ## total_irrigated_area <- stack(fs) %>%
##   ##   stackApply(indices = rep(1, length(fs)), fun = sum)
##   ## fs <- list.files(
##   ##   path = "resources/irrigated_area_maps",
##   ##   pattern = "icrisat_rabi_canal_[0-9]{4}_india_0.500000Deg.tif",
##   ##   full.names = TRUE
##   ## )
##   ## min_canal_area <- stack(fs) %>%
##   ##   stackApply(indices = rep(1, length(fs)), fun = min)
##   ## canal_irrigated_area <- min_canal_area
##   ## other_sources_irrigated_area <- total_irrigated_area * 0.
##   ## other_wells_irrigated_area <- total_irrigated_area * 0.
##   ## tanks_irrigated_area <- total_irrigated_area * 0.
##   ## tubewell_irrigated_area <- total_irrigated_area - canal_irrigated_area
##   ## get_filename <- function(source) {
##   ##   fn <- sprintf("icrisat_rabi_%s_2010_india_0.500000Deg_restored_canal.tif", source)
##   ##   ## return(file.path("resources/irrigated_area_maps", fn))
##   ##   return(file.path(outputdir, fn))
##   ## }
##   ## writeRaster(
##   ##   canal_irrigated_area,
##   ##   get_filename("canal"),
##   ##   overwrite = TRUE
##   ## )
##   ## writeRaster(
##   ##   other_sources_irrigated_area,
##   ##   get_filename("other_sources"),
##   ##   overwrite = TRUE
##   ## )
##   ## writeRaster(
##   ##   other_wells_irrigated_area,
##   ##   get_filename("other_wells"),
##   ##   overwrite = TRUE
##   ## )
##   ## writeRaster(
##   ##   tanks_irrigated_area,
##   ##   get_filename("tanks"),
##   ##   overwrite = TRUE
##   ## )
##   ## writeRaster(
##   ##   tubewell_irrigated_area,
##   ##   get_filename("tubewells"),
##   ##   overwrite = TRUE
##   ## )

##   ## ## Zaid - no canal irrigated area
##   ## fs <- list.files(
##   ##   path = "resources/irrigated_area_maps",
##   ##   pattern = "icrisat_zaid_.*_2010_india_0.500000Deg.tif",
##   ##   full.names = TRUE
##   ## )
##   ## total_irrigated_area <- stack(fs) %>%
##   ##   stackApply(indices = rep(1, length(fs)), fun = sum)
##   ## canal_irrigated_area <- total_irrigated_area * 0.
##   ## other_sources_irrigated_area <- total_irrigated_area * 0.
##   ## other_wells_irrigated_area <- total_irrigated_area * 0.
##   ## tanks_irrigated_area <- total_irrigated_area * 0.
##   ## tubewell_irrigated_area <- total_irrigated_area - canal_irrigated_area
##   ## get_filename <- function(source) {
##   ##   fn <- sprintf("icrisat_zaid_%s_2010_india_0.500000Deg_restored_canal.tif", source)
##   ##   ## return(file.path("resources/irrigated_area_maps", fn))
##   ##   return(file.path(outputdir, fn))
##   ## }
##   ## writeRaster(
##   ##   canal_irrigated_area,
##   ##   get_filename("canal"),
##   ##   overwrite = TRUE
##   ## )
##   ## writeRaster(
##   ##   other_sources_irrigated_area,
##   ##   get_filename("other_sources"),
##   ##   overwrite = TRUE
##   ## )
##   ## writeRaster(
##   ##   other_wells_irrigated_area,
##   ##   get_filename("other_wells"),
##   ##   overwrite = TRUE
##   ## )
##   ## writeRaster(
##   ##   tanks_irrigated_area,
##   ##   get_filename("tanks"),
##   ##   overwrite = TRUE
##   ## )
##   ## writeRaster(
##   ##   tubewell_irrigated_area,
##   ##   get_filename("tubewells"),
##   ##   overwrite = TRUE
##   ## )

##   ## ## Continuous - no canal irrigated area
##   ## fs <- list.files(
##   ##   path = "resources/irrigated_area_maps",
##   ##   pattern = "icrisat_continuous_.*_2010_india_0.500000Deg.tif",
##   ##   full.names = TRUE
##   ## )
##   ## total_irrigated_area <- stack(fs) %>% stackApply(indices = rep(1, length(fs)), fun = sum)
##   ## canal_irrigated_area <- total_irrigated_area * 0.
##   ## other_sources_irrigated_area <- total_irrigated_area * 0.
##   ## other_wells_irrigated_area <- total_irrigated_area * 0.
##   ## tanks_irrigated_area <- total_irrigated_area * 0.
##   ## tubewell_irrigated_area <- total_irrigated_area - canal_irrigated_area
##   ## get_filename <- function(source) {
##   ##   fn <- sprintf("icrisat_continuous_%s_2010_india_0.500000Deg_restored_canal.tif", source)
##   ##   ## return(file.path("resources/irrigated_area_maps", fn))
##   ##   return(file.path(outputdir, fn))
##   ## }
##   ## writeRaster(
##   ##   canal_irrigated_area,
##   ##   get_filename("canal"),
##   ##   overwrite = TRUE
##   ## )
##   ## writeRaster(
##   ##   other_sources_irrigated_area,
##   ##   get_filename("other_sources"),
##   ##   overwrite = TRUE
##   ## )
##   ## writeRaster(
##   ##   other_wells_irrigated_area,
##   ##   get_filename("other_wells"),
##   ##   overwrite = TRUE
##   ## )
##   ## writeRaster(
##   ##   tanks_irrigated_area,
##   ##   get_filename("tanks"),
##   ##   overwrite = TRUE
##   ## )
##   ## writeRaster(
##   ##   tubewell_irrigated_area,
##   ##   get_filename("tubewells"),
##   ##   overwrite = TRUE
##   ## )
## }

compute_current_canal_policy <- function(datadir, ...) {

  ## Assume the current canal irrigated area is the
  ## maximum canal irrigated area over the study period.
  ## TODO this seems overly generous - perhaps the maximum
  ## over the last 10 years?
  fs <- list.files(
    path = "resources/irrigated_area_maps",
    pattern = "icrisat_kharif_.*_2010_india_0.500000Deg.tif",
    full.names = TRUE
  )
  total_irrigated_area <-
    stack(fs) %>%
    stackApply(indices = rep(1, length(fs)), fun = sum)
  fs <- list.files(
    path = "resources/irrigated_area_maps",
    pattern = "icrisat_kharif_canal_[0-9]{4}_india_0.500000Deg.tif",
    full.names = TRUE
  )
  max_canal_area <-
    stack(fs) %>%
    stackApply(indices = rep(1, length(fs)), fun = max)
  canal_irrigated_area <- stackApply(
    stack(total_irrigated_area, max_canal_area),
    indices = rep(1, 1),
    fun = min
  )
  other_sources_irrigated_area <- total_irrigated_area * 0.
  other_wells_irrigated_area <- total_irrigated_area * 0.
  tanks_irrigated_area <- total_irrigated_area * 0.
  tubewell_irrigated_area <- total_irrigated_area - canal_irrigated_area
  get_filename <- function(source) {
    fn <- sprintf("icrisat_kharif_%s_2010_india_0.500000Deg_current_canal.tif", source)
    return(file.path(datadir, fn))
  }
  writeRaster(
    canal_irrigated_area,
    get_filename("canal"),
    overwrite = TRUE
  )
  writeRaster(
    other_sources_irrigated_area,
    get_filename("other_sources"),
    overwrite = TRUE
  )
  writeRaster(
    other_wells_irrigated_area,
    get_filename("other_wells"),
    overwrite = TRUE
  )
  writeRaster(
    tanks_irrigated_area,
    get_filename("tanks"),
    overwrite = TRUE
  )
  writeRaster(
    tubewell_irrigated_area,
    get_filename("tubewells"),
    overwrite = TRUE
  )

  ## Rabi - min canal irrigated area
  fs <- list.files(
    path = "resources/irrigated_area_maps",
    pattern = "icrisat_rabi_.*_2010_india_0.500000Deg.tif",
    full.names = TRUE
  )
  total_irrigated_area <- stack(fs) %>%
    stackApply(indices = rep(1, length(fs)), fun = sum)
  fs <- list.files(
    path = "resources/irrigated_area_maps",
    pattern = "icrisat_rabi_canal_[0-9]{4}_india_0.500000Deg.tif",
    full.names = TRUE
  )
  min_canal_area <- stack(fs) %>%
    stackApply(indices = rep(1, length(fs)), fun = min)
  canal_irrigated_area <- min_canal_area
  other_sources_irrigated_area <- total_irrigated_area * 0.
  other_wells_irrigated_area <- total_irrigated_area * 0.
  tanks_irrigated_area <- total_irrigated_area * 0.
  tubewell_irrigated_area <- total_irrigated_area - canal_irrigated_area
  get_filename <- function(source) {
    fn <- sprintf("icrisat_rabi_%s_2010_india_0.500000Deg_current_canal.tif", source)
    ## return(file.path("resources/irrigated_area_maps", fn))
    return(file.path(datadir, fn))
  }
  writeRaster(
    canal_irrigated_area,
    get_filename("canal"),
    overwrite = TRUE
  )
  writeRaster(
    other_sources_irrigated_area,
    get_filename("other_sources"),
    overwrite = TRUE
  )
  writeRaster(
    other_wells_irrigated_area,
    get_filename("other_wells"),
    overwrite = TRUE
  )
  writeRaster(
    tanks_irrigated_area,
    get_filename("tanks"),
    overwrite = TRUE
  )
  writeRaster(
    tubewell_irrigated_area,
    get_filename("tubewells"),
    overwrite = TRUE
  )

  ## Zaid - no canal irrigated area
  fs <- list.files(
    path = "resources/irrigated_area_maps",
    pattern = "icrisat_zaid_.*_2010_india_0.500000Deg.tif",
    full.names = TRUE
  )
  total_irrigated_area <- stack(fs) %>%
    stackApply(indices = rep(1, length(fs)), fun = sum)
  canal_irrigated_area <- total_irrigated_area * 0.
  other_sources_irrigated_area <- total_irrigated_area * 0.
  other_wells_irrigated_area <- total_irrigated_area * 0.
  tanks_irrigated_area <- total_irrigated_area * 0.
  tubewell_irrigated_area <- total_irrigated_area - canal_irrigated_area
  get_filename <- function(source) {
    fn <- sprintf("icrisat_zaid_%s_2010_india_0.500000Deg_current_canal.tif", source)
    ## return(file.path("resources/irrigated_area_maps", fn))
    return(file.path(datadir, fn))
  }
  writeRaster(
    canal_irrigated_area,
    get_filename("canal"),
    overwrite = TRUE
  )
  writeRaster(
    other_sources_irrigated_area,
    get_filename("other_sources"),
    overwrite = TRUE
  )
  writeRaster(
    other_wells_irrigated_area,
    get_filename("other_wells"),
    overwrite = TRUE
  )
  writeRaster(
    tanks_irrigated_area,
    get_filename("tanks"),
    overwrite = TRUE
  )
  writeRaster(
    tubewell_irrigated_area,
    get_filename("tubewells"),
    overwrite = TRUE
  )

  ## Continuous - no canal irrigated area
  fs <- list.files(
    path = "resources/irrigated_area_maps",
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
    fn <- sprintf("icrisat_continuous_%s_2010_india_0.500000Deg_current_canal.tif", source)
    ## return(file.path("resources/irrigated_area_maps", fn))
    return(file.path(datadir, fn))
  }
  writeRaster(
    canal_irrigated_area,
    get_filename("canal"),
    overwrite = TRUE
  )
  writeRaster(
    other_sources_irrigated_area,
    get_filename("other_sources"),
    overwrite = TRUE
  )
  writeRaster(
    other_wells_irrigated_area,
    get_filename("other_wells"),
    overwrite = TRUE
  )
  writeRaster(
    tanks_irrigated_area,
    get_filename("tanks"),
    overwrite = TRUE
  )
  writeRaster(
    tubewell_irrigated_area,
    get_filename("tubewells"),
    overwrite = TRUE
  )
}

summarise_water_balance <- function(datadir,
                                    stem,
                                    policy,
                                    years,
                                    fixed_leakage = TRUE,
                                    f_leakage = 0.,
                                    f_leakage_max = Inf) {

  ## Loop through years
  pb = txtProgressBar(min = 0, max = length(years) - 1, initial = 0)
  for (k in 1:(length(years) - 1)) {
    yr <- years[k]

    ## Load JULES output
    jules_output <- load_jules_output("results/JULES_output", stem, policy, yr)
    precip <- jules_output[["precip"]]
    surf_roff <- jules_output[["surf_roff"]]
    sub_surf_roff <- jules_output[["sub_surf_roff"]]
    et <- jules_output[["et"]]
    pet <- jules_output[["pet"]]
    gw_irrig_water <- jules_output[["gw_irrig_water"]]
    sw_irrig_water <- jules_output[["sw_irrig_water"]]
    total_irrig_water <- jules_output[["total_irrig_water"]]
    season_irrig_maps <- jules_output[["season_irrig_maps"]]

    ## Annual total irrigation
    suffix <- sub("(JULES)_(vn.*)_([a-z]+)", "\\3", stem) #id_stems[i])
    annual_total_irrigation <- stackApply(
      stack(total_irrig_water),
      indices = rep(1, 12),
      fun = sum
    )
    fn <- paste0(
      "annual_total_irrigation_", policy, "_", yr, "_", suffix, ".tif"
    )
    writeRaster(
      annual_total_irrigation,
      file.path(datadir, fn),
      overwrite = TRUE
    )

    ## Annual groundwater irrigation
    annual_gw_irrigation <- stackApply(
      stack(gw_irrig_water),
      indices = rep(1, 12),
      fun = sum
    )
    fn <- paste0(
      "annual_gw_irrigation_", policy, "_", yr, "_", suffix, ".tif"
    )
    writeRaster(
      annual_gw_irrigation,
      file.path(datadir, fn),
      overwrite = TRUE
    )

    ## Annual sw irrigation
    annual_sw_irrigation <- stackApply(
      stack(sw_irrig_water),
      indices = rep(1, 12),
      fun = sum
    )
    fn <- paste0(
      "annual_sw_irrigation_", policy, "_", yr, "_", suffix, ".tif"
    )
    writeRaster(
      annual_sw_irrigation,
      file.path(datadir, fn),
      overwrite = TRUE
    )

    ## Monthly and season-wise totals
    for (j in 1:length(seasons)) {
      season <- seasons[j]
      for (type in c("total", "gw", "sw")) {
        season_irrigation <- stackApply(
          stack(season_irrig_maps[[season]][[type]]),
          indices = rep(1, 12),
          fun = sum
        )
        fn <- paste0(
          season, "_", type, "_irrigation_",
          policy, "_", yr, "_", suffix, ".tif"
        )
        writeRaster(
          season_irrigation,
          file.path(datadir, fn),
          overwrite = TRUE
        )
      }
    }
    for (j in 1:12) {
      month <- year_months[j]
      for (type in c("total", "gw", "sw")) {
        maps <- stack(
          lapply(seasons, FUN = function(season)
            season_irrig_maps[[season]][[type]][[j]])
        )
        month_irrigation <- stackApply(
          maps,
          indices = rep(1, length(seasons)),
          fun = sum
        )
        fn <- paste0(
          type, "_irrigation_", policy, "_", yr, "_",
          formatC(month, width=2, flag=0), "_", suffix, ".tif"
        )
        writeRaster(
          month_irrigation,
          file.path(datadir, fn),
          overwrite = TRUE
        )
      }
    }

    ## Annual ET
    annual_et <- stackApply(
        stack(et), 
        indices = rep(1, 12), 
        fun = sum
    )
    fn <- paste0("annual_et_", policy, "_", yr, "_", suffix, ".tif")
    writeRaster(
      annual_et,
      file.path(datadir, fn),
      overwrite = TRUE
    )

    ## Annual PET
    annual_pet <- stackApply(
        stack(pet), 
        indices = rep(1, 12), 
        fun = sum
    )
    fn <- paste0("annual_pet_", policy, "_", yr, "_", suffix, ".tif")
    writeRaster(
      annual_pet,
      file.path(datadir, fn),
      overwrite = TRUE
    )

    ## Annual precipitation
    annual_precip = stackApply(
        stack(precip), 
        indices = rep(1, 12), 
        fun = sum
    )
    fn <- paste0("annual_precip_", policy, "_", yr, "_", suffix, ".tif")
    writeRaster(
      annual_precip,
      file.path(datadir, fn),
      overwrite = TRUE
    )

    ## Annual surface runoff
    annual_surf_roff <- stackApply(
        stack(surf_roff), 
        indices = rep(1, 12), 
        fun = sum
    )
    fn <- paste0("annual_surf_roff_", policy, "_", yr, "_", suffix, ".tif")
    writeRaster(
      annual_surf_roff,
      file.path(datadir, fn),
      overwrite = TRUE
    )

    ## Annual subsurface runoff
    annual_sub_surf_roff <- stackApply(
        stack(sub_surf_roff), 
        indices = rep(1, 12), 
        fun = sum
    )
    fn <- paste0("annual_sub_surf_roff_", policy, "_", yr, "_", suffix, ".tif")
    writeRaster(
      annual_sub_surf_roff,
      file.path(datadir, fn),
      overwrite = TRUE
    )

    ## ################################################### ##
    ## Calculate water balance
    ## ################################################### ##

    ## Initial recharge before accounting for canal leakage
    Qin <- stackApply(
        stack(sub_surf_roff),
        indices = rep(1, 12),
        fun = sum
    )

    ## Abstraction
    Qout <- stackApply(
        stack(gw_irrig_water), 
        indices = rep(1, 12), 
        fun = sum
    )
    fn <- paste0("abstraction_", policy, "_", yr, "_", suffix, ".tif")
    writeRaster(Qout, file.path(datadir, fn), overwrite = TRUE)

    ## Compute canal leakage
    deficit <- Qout - Qin # +ve indicates storage loss / -ve indicates storage gain
    if (fixed_leakage) {
      annual_canal_leakage <- annual_sw_irrigation * f_leakage
      ## Convert to raster map so that we can write to file
      f_leakage <- annual_canal_leakage / annual_sw_irrigation
    } else {
      ## Else we calculate what is needed to ensure water balance
      f_leakage <- deficit / annual_sw_irrigation
      f_leakage[f_leakage < 0] <- 0
      f_leakage[f_leakage > f_leakage_max] <- f_leakage_max
      annual_canal_leakage <- annual_sw_irrigation * f_leakage
    }

    ## Add canal leakage to recharge and write to file
    Qin <- Qin + annual_canal_leakage
    fn <- paste0("recharge_", policy, "_", yr, "_", suffix, ".tif")
    writeRaster(Qin, file.path(datadir, fn), overwrite = TRUE)

    ## Write canal leakage factor to file
    fn <- paste0("f_leakage_", policy, "_", yr, "_", suffix, ".tif")
    writeRaster(f_leakage, file.path(datadir, fn), overwrite = TRUE)

    ## Change in storage
    dS <- Qin - Qout
    fn <- paste0("dS", policy, "_", yr, "_", suffix, ".tif")
    writeRaster(dS, file.path(datadir, fn), overwrite = TRUE)
    setTxtProgressBar(pb, k)
  }
  close(pb)
}
