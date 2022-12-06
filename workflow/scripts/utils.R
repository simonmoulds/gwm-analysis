## Author : Simon Moulds
## Date   : October 2021

get_jules_month_data <- function(year, month, varname, id_stem, job_name, profile_name) {
    if (month > 12) {
        month = month - 12
        year_offset = 1
    } else {
        year_offset = 0
    }    
    fn = paste0(
        id_stem,
        ".", sprintf(job_name, year + year_offset), 
        ".", profile_name,
        ".", year + year_offset,
        ".2D.month.nc"
    )
    jules = nc_open(file.path(jules_output_dir, fn))
    var = ncvar_get(jules, varname, start=c(1,1,month), count=c(80,40,1))
    var = aperm(var, c(2,1))
    var = var[rev(seq_len(nrow(var))),]
    r = raster(nrows=40, ncols=80, xmn=60, xmx=100, ymn=20, ymx=40)
    r[] = var
    r    
}

get_jules_month_irrig_rel_data <- function(year, month, varname, id_stem, job_name, profile_name) {
    if (month > 12) {
        month = month - 12
        year_offset = 1
    } else {
        year_offset = 0
    }    
    fn = paste0(
        id_stem,
        ".", sprintf(job_name, year + year_offset), 
        ".", profile_name,
        ".rel_irrig_water",
        ".", year + year_offset,
        ".2D.month.nc"
    )
    if (file.exists(file.path(jules_output_dir, fn))) {
        jules = nc_open(file.path(jules_output_dir, fn))
        var = ncvar_get(jules, varname, start=c(1,1,1,month), count=c(80,40,15,1))
        var = aperm(var, c(3,2,1))
        var = var[,rev(seq_len(dim(var)[2])),]
    } else {
        var = array(data=0, dim=c(15, 40, 80))
    }
    template = raster(nrows=40, ncols=80, xmn=60, xmx=100, ymn=20, ymx=40)
    rlist = list()
    for (i in 1:15) {
        r = template
        r[] = var[i,,]
        rlist[[i]] = r
    }
    st = stack(rlist)
    st
}

get_jules_jjas_data <- function(year, varname) {
    maps = list()    
    for (i in 1:4) {
        ## +5 so we start at June
        maps[[i]] = get_jules_month_data(year, i+5, varname)
    }
    sm = stackApply(stack(maps), indices=rep(1, 4), fun=sum)
    sm
}

get_jules_year_data <- function(year, month, varname) {
    maps = list()
    for (i in 1:12) {
        maps[[i]] = get_jules_month_data(year, i, varname)
    }
    sm = stackApply(stack(maps), indices=rep(1, 12), fun=sum)
    sm
}

get_irr_frac <- function(year, season, policy="current_canal") {
    if (policy %in% policies) {
        if (policy == "historical") {
            suffix = ".tif"
        } else {            
            suffix = paste0("_", policy, ".tif")
        }        
    } else {
        msg = paste0("Policy ", policy, " not recognised!")
        stop(msg)
    }    
    maps = list()
    for (i in 1:length(irrigation_sources)) {
        source = irrigation_sources[i]
        fn = file.path(
            "resources/irrigated_area_maps/",
            paste0(
                "icrisat_", season, "_", source, "_",
                year, "_india_0.500000Deg", suffix
            )
        )
        maps[[source]] = raster(fn)
    }
    maps = stack(maps)
    maps
}

## get_irr_frac2 = function(year, season, policy) {
##     frac_fn = file.path(
##         "../data/wfdei/ancils/",
##         paste0("jules_5pft_w_crops_veg_frac_", year, "_igp_wfdei.nc")
##     )
##     template = raster(xmn=60, xmx=100, ymn=20, ymx=40, nrow=40, ncol=80)
##     ds = nc_open(frac_fn)
##     frac = ncvar_get(ds, "land_cover_lccs")
##     if (season == "kharif") {
##         ## irrigated_single + irrigated_double + irrigated_triple
##         ## i.e. assume that if land is irrigated at all, it is
##         ## always irrigated during kharif
##         tot_frac = apply(frac[,,7:9], 1:2, sum)
##     } else if (season == "rabi") {
##         ## irrigated_double + irrigated_triple
##         ## i.e. assume that if land is irrigated twice, it is
##         ## always irrigated during rabi
##         tot_frac = apply(frac[,,8:9], 1:2, sum)
##     } else if (season == "zaid") {
##         ## irrigated_triple
##         ## i.e. assume that if land is irrigated three times, it is
##         ## always irrigated during zaid
##         tot_frac = frac[,,9]
##     } else if (season == "continuous") {
##         tot_frac = frac[,,10]
##     }
##     template[] = apply(t(tot_frac), 2, rev)
##     template[template >= 1e+30] = NA # FIXME
##     template
## }

get_sw_gw_irr_frac <- function(year, season, policy) {
    maps = get_irr_frac(year, season, policy)
    sw_maps = maps[[c("canal","other_sources","tanks")]]
    gw_maps = maps[[c("other_wells","tubewells")]]
    sw_total = stackApply(sw_maps, indices=rep(1, 3), fun=sum)
    gw_total = stackApply(gw_maps, indices=rep(1, 2), fun=sum)
    total = stackApply(maps, indices=rep(1, 5), fun=sum)
    sw_total = sw_total / total
    sw_total[!is.finite(sw_total)] = 0
    gw_total = gw_total / total
    gw_total[!is.finite(gw_total)] = 0    
    list(sw=sw_total, gw=gw_total, total=total)
}

seasons = c("continuous", "kharif", "rabi", "zaid")
get_nia_gia <- function(year, policy) {
    maps = vector("list", 4) %>% setNames(seasons)
    for (i in 1:length(seasons)) {
        season = seasons[i]
        maps[[season]] = get_sw_gw_irr_frac(year, season, policy)
    }
    types = c("gw", "sw", "total")
    nia = vector("list", 3) %>% setNames(types)
    gia = vector("list", 3) %>% setNames(types)
    for (i in 1:length(types)) {
        type = types[i]
        st = stack(lapply(maps, FUN=function(x) x[[type]]))
        nia[[type]] = stackApply(st, indices=rep(1, 4), fun=max)
        gia[[type]] = stackApply(st, indices=rep(1, 4), fun=sum)
    }
    list(gia=gia, nia=nia)
}

## TODO put these functions in utils.R
load_jules_output <- function(id_stem, policy, yr, ...) {

  ## FIXME - make these arguments to function
  job_name = "jules_%s"
  profile_name = "daily_hydrology"

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

## ## NOT CURRENTLY USED:
## compute_target_canal_area <- function(year_str = "[0-9]{4}",
##                                       f_leakage = 0.15,
##                                       ...) {
##   ## Recharge
##   fs <- list.files(
##     "../data/analysis/current_canal",
##     pattern = paste0("recharge_current_canal_", year_str, "_current.tif"),
##     full.names = TRUE
##   )
##   if (length(fs) > 1) {
##     recharge <- stackApply(stack(fs), indices = rep(1, length(fs)), fun = mean)
##   } else if (length(fs) == 1) {
##     recharge <- raster(fs[1])
##   }
##   ## Abstraction
##   fs <- list.files(
##     "../data/analysis/current_canal",
##     pattern = paste0("abstraction_current_canal_", year_str, "_current.tif"),
##     full.names = TRUE
##   )
##   ## abstraction <- stackApply(stack(fs), indices = rep(1, length(fs)), fun = mean)
##   if (length(fs) > 1) {
##     abstraction <- stackApply(stack(fs), indices = rep(1, length(fs)), fun = mean)
##   } else if (length(fs) == 1) {
##     abstraction <- raster(fs[1])
##   }
##   ## GW irrigation
##   fs <- list.files(
##     "../data/analysis/current_canal",
##     pattern = paste0("rabi_gw_irrigation_current_canal_", year_str, "_current.tif"),
##     full.names = TRUE
##   )
##   ## rabi_gw_irrigation <- stackApply(stack(fs), indices = rep(1, length(fs)), fun = mean)
##   if (length(fs) > 1) {
##     rabi_gw_irrigation <- stackApply(stack(fs), indices = rep(1, length(fs)), fun = mean)
##   } else if (length(fs) == 1) {
##     rabi_gw_irrigation <- raster(fs[1])
##   }
##   fs <- list.files(
##     "../data/analysis/current_canal",
##     pattern = paste0("zaid_gw_irrigation_current_canal_", year_str, "_current.tif"),
##     full.names = TRUE
##   )
##   ## zaid_gw_irrigation <- stackApply(stack(fs), indices = rep(1, length(fs)), fun = mean)
##   if (length(fs) > 1) {
##     zaid_gw_irrigation <- stackApply(stack(fs), indices = rep(1, length(fs)), fun = mean)
##   } else if (length(fs) == 1) {
##     zaid_gw_irrigation <- raster(fs[1])
##   }
##   fs <- list.files(
##     "../data/analysis/current_canal",
##     pattern = paste0("continuous_gw_irrigation_current_canal_", year_str, "_current.tif"),
##     full.names = TRUE
##   )
##   ## continuous_gw_irrigation <- stackApply(stack(fs), indices = rep(1, length(fs)), fun = mean)
##   if (length(fs) > 1) {
##     continuous_gw_irrigation <- stackApply(stack(fs), indices = rep(1, length(fs)), fun = mean)
##   } else if (length(fs) == 1) {
##     continuous_gw_irrigation <- raster(fs[1])
##   }
##   fs <- list.files(
##     "../data/analysis/current_canal",
##     pattern = paste0("kharif_total_irrigation_current_canal_", year_str, "_current.tif"),
##     full.names = TRUE
##   )
##   ## kharif_total_irrigation <- stackApply(stack(fs), indices = rep(1, length(fs)), fun = mean)
##   if (length(fs) > 1) {
##     kharif_total_irrigation <- stackApply(stack(fs), indices = rep(1, length(fs)), fun = mean)
##   } else if (length(fs) == 1) {
##     kharif_total_irrigation <- raster(fs[1])
##   }
##   f_kharif_gw <- (
##     (recharge - rabi_gw_irrigation - zaid_gw_irrigation - continuous_gw_irrigation + f_leakage * kharif_total_irrigation)
##     / (kharif_total_irrigation + f_leakage * kharif_total_irrigation)
##   )
##   f_kharif_gw[f_kharif_gw < 0] <- 0
##   f_kharif_gw[f_kharif_gw > 1] <- 1
##   f_kharif_sw <- 1 - f_kharif_gw
##   list(sw=f_kharif_sw, gw=f_kharif_gw)
## }

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

compute_restored_canal_policy <- function(datadir, ...) {
  ## Load current canal [to get total irrigated area]
  fs <- list.files(
    path = "resources/irrigated_area_maps",
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
    fn <- sprintf("icrisat_kharif_%s_2010_india_0.500000Deg_restored_canal.tif", source)
    return(file.path("resources/irrigated_area_maps", fn))
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
    path = "resources/irrigated_area_maps",
    pattern = "icrisat_rabi_.*_2010_india_0.500000Deg.tif",
    full.names = TRUE
  )
  total_irrigated_area <- stack(fs) %>% stackApply(indices = rep(1, length(fs)), fun = sum)
  fs <- list.files(
    path = "resources/irrigated_area_maps",
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
    fn <- sprintf("icrisat_rabi_%s_2010_india_0.500000Deg_restored_canal.tif", source)
    return(file.path("resources/irrigated_area_maps", fn))
  }
  writeRaster(canal_irrigated_area, get_filename("canal"), overwrite = TRUE)
  writeRaster(other_sources_irrigated_area, get_filename("other_sources"), overwrite = TRUE)
  writeRaster(other_wells_irrigated_area, get_filename("other_wells"), overwrite = TRUE)
  writeRaster(tanks_irrigated_area, get_filename("tanks"), overwrite = TRUE)
  writeRaster(tubewell_irrigated_area, get_filename("tubewells"), overwrite = TRUE)

  ## Zaid - no canal irrigated area
  fs <- list.files(
    path = "resources/irrigated_area_maps",
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
    fn <- sprintf("icrisat_zaid_%s_2010_india_0.500000Deg_restored_canal.tif", source)
    return(file.path("resources/irrigated_area_maps", fn))
  }
  writeRaster(canal_irrigated_area, get_filename("canal"), overwrite = TRUE)
  writeRaster(other_sources_irrigated_area, get_filename("other_sources"), overwrite = TRUE)
  writeRaster(other_wells_irrigated_area, get_filename("other_wells"), overwrite = TRUE)
  writeRaster(tanks_irrigated_area, get_filename("tanks"), overwrite = TRUE)
  writeRaster(tubewell_irrigated_area, get_filename("tubewells"), overwrite = TRUE)

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
    fn <- sprintf("icrisat_continuous_%s_2010_india_0.500000Deg_restored_canal.tif", source)
    return(file.path("resources/irrigated_area_maps", fn))
  }
  writeRaster(canal_irrigated_area, get_filename("canal"), overwrite = TRUE)
  writeRaster(other_sources_irrigated_area, get_filename("other_sources"), overwrite = TRUE)
  writeRaster(other_wells_irrigated_area, get_filename("other_wells"), overwrite = TRUE)
  writeRaster(tanks_irrigated_area, get_filename("tanks"), overwrite = TRUE)
  writeRaster(tubewell_irrigated_area, get_filename("tubewells"), overwrite = TRUE)

}

compute_current_canal_policy <- function(datadir, ...) {
  ## Max kharif canal irrigated area
  fs <- list.files(
    path = "resources/irrigated_area_maps",
    pattern = "icrisat_kharif_.*_2010_india_0.500000Deg.tif",
    full.names = TRUE
  )
  total_irrigated_area <- stack(fs) %>% stackApply(indices = rep(1, length(fs)), fun = sum)
  fs <- list.files(
    path = "resources/irrigated_area_maps",
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
    fn <- sprintf("icrisat_kharif_%s_2010_india_0.500000Deg_current_canal.tif", source)
    return(file.path("resources/irrigated_area_maps", fn))
  }
  writeRaster(canal_irrigated_area, get_filename("canal"), overwrite = TRUE)
  writeRaster(other_sources_irrigated_area, get_filename("other_sources"), overwrite = TRUE)
  writeRaster(other_wells_irrigated_area, get_filename("other_wells"), overwrite = TRUE)
  writeRaster(tanks_irrigated_area, get_filename("tanks"), overwrite = TRUE)
  writeRaster(tubewell_irrigated_area, get_filename("tubewells"), overwrite = TRUE)

  ## Rabi - min canal irrigated area
  fs <- list.files(
    path = "resources/irrigated_area_maps",
    pattern = "icrisat_rabi_.*_2010_india_0.500000Deg.tif",
    full.names = TRUE
  )
  total_irrigated_area <- stack(fs) %>% stackApply(indices = rep(1, length(fs)), fun = sum)
  fs <- list.files(
    path = "resources/irrigated_area_maps",
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
    fn <- sprintf("icrisat_rabi_%s_2010_india_0.500000Deg_current_canal.tif", source)
    return(file.path("resources/irrigated_area_maps", fn))
  }
  writeRaster(canal_irrigated_area, get_filename("canal"), overwrite = TRUE)
  writeRaster(other_sources_irrigated_area, get_filename("other_sources"), overwrite = TRUE)
  writeRaster(other_wells_irrigated_area, get_filename("other_wells"), overwrite = TRUE)
  writeRaster(tanks_irrigated_area, get_filename("tanks"), overwrite = TRUE)
  writeRaster(tubewell_irrigated_area, get_filename("tubewells"), overwrite = TRUE)

  ## Zaid - no canal irrigated area
  fs <- list.files(
    path = "resources/irrigated_area_maps",
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
    fn <- sprintf("icrisat_zaid_%s_2010_india_0.500000Deg_current_canal.tif", source)
    return(file.path("resources/irrigated_area_maps", fn))
  }
  writeRaster(canal_irrigated_area, get_filename("canal"), overwrite = TRUE)
  writeRaster(other_sources_irrigated_area, get_filename("other_sources"), overwrite = TRUE)
  writeRaster(other_wells_irrigated_area, get_filename("other_wells"), overwrite = TRUE)
  writeRaster(tanks_irrigated_area, get_filename("tanks"), overwrite = TRUE)
  writeRaster(tubewell_irrigated_area, get_filename("tubewells"), overwrite = TRUE)

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
    return(file.path("resources/irrigated_area_maps", fn))
  }
  writeRaster(canal_irrigated_area, get_filename("canal"), overwrite = TRUE)
  writeRaster(other_sources_irrigated_area, get_filename("other_sources"), overwrite = TRUE)
  writeRaster(other_wells_irrigated_area, get_filename("other_wells"), overwrite = TRUE)
  writeRaster(tanks_irrigated_area, get_filename("tanks"), overwrite = TRUE)
  writeRaster(tubewell_irrigated_area, get_filename("tubewells"), overwrite = TRUE)
}
