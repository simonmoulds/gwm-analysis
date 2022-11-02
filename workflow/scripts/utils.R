## Author : Simon Moulds
## Date   : October 2021

get_jules_month_data = function(year, month, varname, id_stem, job_name, profile_name) {
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

get_jules_month_irrig_rel_data = function(year, month, varname, id_stem, job_name, profile_name) {
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

get_jules_jjas_data = function(year, varname) {
    maps = list()    
    for (i in 1:4) {
        ## +5 so we start at June
        maps[[i]] = get_jules_month_data(year, i+5, varname)
    }
    sm = stackApply(stack(maps), indices=rep(1, 4), fun=sum)
    sm
}

get_jules_year_data = function(year, month, varname) {
    maps = list()
    for (i in 1:12) {
        maps[[i]] = get_jules_month_data(year, i, varname)
    }
    sm = stackApply(stack(maps), indices=rep(1, 12), fun=sum)
    sm
}

get_irr_frac = function(year, season, policy="current_canal") {
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
            "../data/irrigated_area_maps/",
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

get_sw_gw_irr_frac = function(year, season, policy) {
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
get_nia_gia = function(year, policy) {
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

