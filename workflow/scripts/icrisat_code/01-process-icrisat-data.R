## Author : Simon Moulds
## Date   : April 2021

library(raster)
library(sf)
library(tidyverse)
library(magrittr)
library(rgdal)
library(maptools)
options(stringsAsFactors = FALSE)

## This script generates the file `irrigated_area_source.gpkg`, which
## contains quality-controlled irrigation source data for the period
## 1970-2010

unzip("../data/GIS-shapefiles-1966base.zip", exdir="../data")
shp = st_read("../data/GIS-shapefiles-1966base/india70again.shp")
st_crs(shp) = "EPSG:4326"
sf_use_s2(FALSE)
area = st_area(shp)
shp$AREA = area
sf_use_s2(TRUE)
shp %<>% as_Spatial

## apy = read.csv("../data/ICRISAT-District Level Data_apy.csv")
src = read.csv("../data/ICRISAT-District Level Data_source_irrigated_area.csv")
lu = read.csv("../data/ICRISAT-District Level Data_land_use.csv") %>% dplyr::select(-State.Name, -Dist.Name)
src %<>% left_join(lu) ## %>% left_join(apy)
## src %<>%
##     dplyr::select(-contains("PRODUCTION")) %>%
##     dplyr::select(-contains("YIELD"))

src_states = src$State.Name
src$State.Name[src$State.Name %in% "Chhattisgarh"] = "Madhya Pradesh"
src$State.Name[src$State.Name %in% "Jharkhand"] = "Bihar"
src$State.Name[src$State.Name %in% "Telangana"] = "Andhra Pradesh"
src$State.Name[src$State.Name %in% "Uttarakhand"] = "Uttar Pradesh"

x =
    shp %>%
    as.data.frame %>%
    dplyr::select(c("NAME1", "NAME2")) %>%
    setNames(c("STATE","DIST")) %>%
    arrange(STATE, DIST)
y =
    src %>%
    dplyr::select(c("State.Name","Dist.Name")) %>%
    unique %>% setNames(c("STATE","DIST")) %>%
    arrange(STATE, DIST)

## UNCOMMENT IF NEEDED
## x_STATE = unique(x$STATE)
## y_STATE = unique(y$STATE)
## y_STATE[!y_STATE %in% x_STATE]

## ## limit to ICRISAT states
## x %<>% filter(x$STATE %in% y$STATE)
## combined = left_join(x, y, keep=T)
## write.csv(combined, "merged_dists.csv")

## create a set of unique ids for each state/district combination
ids = seq(1, nrow(shp))
## make a copy to modify
union_ids = ids
union_dists = shp$NAME2
## assign to district polygons
shp$POLY_ID = ids

## Andhra Pradesh
## ##############

replace_dist_name = function(x, state, district, replacement) {
    ix = which((x[["Dist.Name"]] %in% district) & (x[["State.Name"]] %in% state))
    if (length(ix) == 0) {
        stop()
    }    
    x[["Dist.Name"]][ix] = replacement
    x
}

## 2     Andhra Pradesh                 Anantapur
## 4     Andhra Pradesh                  Cuddapah
## 13    Andhra Pradesh               Mahbubnagar
## 16    Andhra Pradesh                   Nellore
## 19    Andhra Pradesh            Vishakhapatnam
src %<>% replace_dist_name("Andhra Pradesh", "Ananthapur", "Anantapur")
src %<>% replace_dist_name("Andhra Pradesh", "Kadapa YSR", "Cuddapah")
src %<>% replace_dist_name("Andhra Pradesh", "Mahabubnagar", "Mahbubnagar")
src %<>% replace_dist_name("Andhra Pradesh", "S.P.S. Nellore", "Nellore")
src %<>% replace_dist_name("Andhra Pradesh", "Visakhapatnam", "Vishakhapatnam")

## 27             Assam     Karbi Anglong (Diphu)
## 29             Assam                 N.C Hills
src %<>% replace_dist_name("Assam", "Karbi Anglong", "Karbi Anglong (Diphu)")
src %<>% replace_dist_name("Assam", "North Cachar Hil / Dima hasao", "N.C Hills")

## 36             Bihar   Dumka(Santhal Parganas)
## 38             Bihar                 Hazaribag
## 39             Bihar                    Munger
## 40             Bihar              Muzzaffarpur
## 41             Bihar       Palamu (Daltenganj)
## 43             Bihar                    Purnia
## 45             Bihar                   Sahabad
src %<>% replace_dist_name("Bihar", "Santhal Paragana / Dumka", "Dumka(Santhal Parganas)")
src %<>% replace_dist_name("Bihar", "Hazaribagh", "Hazaribag")
src %<>% replace_dist_name("Bihar", "Mungair", "Munger")
src %<>% replace_dist_name("Bihar", "Muzaffarpur", "Muzzaffarpur")
src %<>% replace_dist_name("Bihar", "Palamau", "Palamu (Daltenganj)")
src %<>% replace_dist_name("Bihar", "Purnea", "Purnia")
src %<>% replace_dist_name("Bihar", "Shahabad (now part of Bhojpur district)", "Sahabad")

## 49           Gujarat                 Ahmadabad
## 51           Gujarat              Banas Kantha
## 69           Gujarat            Kachchh (Bhuj)
## 75           Gujarat                  Mahesana
## 76           Gujarat      Panchmahals (Godhra)
## 78           Gujarat  Sabarkantha (Himatnagar)
## 81           Gujarat          The Dangs (Ahwa)
## 82           Gujarat                  Vadodara
src %<>% replace_dist_name("Gujarat", "Ahmedabad", "Ahmadabad")
src %<>% replace_dist_name("Gujarat", "Banaskantha", "Banas Kantha")
src %<>% replace_dist_name("Gujarat", "Kutch", "Kachchh (Bhuj)")
src %<>% replace_dist_name("Gujarat", "Mehsana", "Mahesana")
src %<>% replace_dist_name("Gujarat", "Panchmahal", "Panchmahals (Godhra)")
src %<>% replace_dist_name("Gujarat", "Sabarkantha", "Sabarkantha (Himatnagar)")
src %<>% replace_dist_name("Gujarat", "Dangs", "The Dangs (Ahwa)")
src %<>% replace_dist_name("Gujarat", "Vadodara / Baroda", "Vadodara")

## 90           Haryana              Mahendragarh
src %<>% replace_dist_name("Haryana", "Mahendragarh / Narnaul", "Mahendragarh")

## 92  Himachal Pradesh                  Bilaspur
## 98  Himachal Pradesh           Lahul and Spiti
## 101 Himachal Pradesh           Sirmaur (Nahan)
src %<>% replace_dist_name("Himachal Pradesh", "Bilashpur", "Bilaspur")
src %<>% replace_dist_name("Himachal Pradesh", "Lahul & Spiti", "Lahul and Spiti")
src %<>% replace_dist_name("Himachal Pradesh", "Sirmaur", "Sirmaur (Nahan)")

## 107        Karnataka                   Bijapur
## 108        Karnataka               Chikmagalur
## 110        Karnataka            Dakshin Kannad
## 112        Karnataka                  Gulbarga
## 114        Karnataka                    Kodagu
## 119        Karnataka                   Shimoga
## 121        Karnataka              Uttar Kannad
src %<>% replace_dist_name("Karnataka", "Bijapur / Vijayapura", "Bijapur")
src %<>% replace_dist_name("Karnataka", "Chickmagalur", "Chikmagalur")
src %<>% replace_dist_name("Karnataka", "Dakshina Kannada", "Dakshin Kannad")
src %<>% replace_dist_name("Karnataka", "Gulbarga / Kalaburagi", "Gulbarga")
src %<>% replace_dist_name("Karnataka", "Kodagu / Coorg", "Kodagu")
src %<>% replace_dist_name("Karnataka", "Shimoge", "Shimoga")
src %<>% replace_dist_name("Karnataka", "Uttara Kannada", "Uttar Kannad")

## 122           Kerala                  Alleppey
## 123           Kerala                 Cannanore
## 124           Kerala                 Ernakulam
## 125           Kerala           Kollam (Quilon)
## 130           Kerala        Palakkad (Palghat)
## 131           Kerala Thiruvananthapuram (Triv)
## 132           Kerala         Trissur (Trichur)
src %<>% replace_dist_name("Kerala", "Alappuzha", "Alleppey")
src %<>% replace_dist_name("Kerala", "Kannur", "Cannanore")
src %<>% replace_dist_name("Kerala", "Eranakulam", "Ernakulam")
src %<>% replace_dist_name("Kerala", "Kollam", "Kollam (Quilon)")
src %<>% replace_dist_name("Kerala", "Palakkad", "Palakkad (Palghat)")
src %<>% replace_dist_name("Kerala", "Thiruvananthapuram", "Thiruvananthapuram (Triv)")
src %<>% replace_dist_name("Kerala", "Thrissur", "Trissur (Trichur)")

## 134   Madhya Pradesh        Bastar (Jagdalpur)
## 138   Madhya Pradesh                Chhatrapur
## 145   Madhya Pradesh      East Nimar (Khandwa)
## 155   Madhya Pradesh               Narsimhapur
## 166   Madhya Pradesh                     Seoni
## 171   Madhya Pradesh       Surguja (Ambikapur)
## 175   Madhya Pradesh      West Nimar (Khargon)
src %<>% replace_dist_name("Madhya Pradesh", "Bastar", "Bastar (Jagdalpur)")
src %<>% replace_dist_name("Madhya Pradesh", "Chhatarpur", "Chhatrapur")
src %<>% replace_dist_name("Madhya Pradesh", "Khandwa / East Nimar", "East Nimar (Khandwa)")
src %<>% replace_dist_name("Madhya Pradesh", "Narsinghpur", "Narsimhapur")
src %<>% replace_dist_name("Madhya Pradesh", "Seoni / Shivani", "Seoni")
src %<>% replace_dist_name("Madhya Pradesh", "Surguja", "Surguja (Ambikapur)")
src %<>% replace_dist_name("Madhya Pradesh", "Khargone / West Nimar", "West Nimar (Khargon)")

## 176      Maharashtra                Ahmadnagar
## 178      Maharashtra                  Amravati
## 182      Maharashtra                   Buldana
## 185      Maharashtra            Greater Bombay
## 190      Maharashtra                    Nashik
## 194      Maharashtra          Raigarh (Alibag)
## 198      Maharashtra                Sindhudurg
## 202      Maharashtra                  Yavatmal
src %<>% replace_dist_name("Maharashtra", "Ahmednagar", "Ahmadnagar")
src %<>% replace_dist_name("Maharashtra", "Amarawati", "Amravati")
src %<>% replace_dist_name("Maharashtra", "Buldhana", "Buldana")
src %<>% replace_dist_name("Maharashtra", "Bombay", "Greater Bombay")
src %<>% replace_dist_name("Maharashtra", "Nasik", "Nashik")
src %<>% replace_dist_name("Maharashtra", "Raigad", "Raigarh (Alibag)")
src %<>% replace_dist_name("Maharashtra", "Yeotmal", "Yavatmal")

ix = match(c("Ratnagiri", "Sindhudurg"), shp$NAME2)
union_ids[ix] = ix[1]
union_dists[ix] = "Ratnagiri"

## 203           Orissa                  Balangir
## 204           Orissa                 Baleshwar
## 211           Orissa                 Kendujhar
## 214           Orissa                Mayurbhanj
## 215           Orissa                  Phulbani
src %<>% replace_dist_name("Orissa", "Bolangir", "Balangir")
src %<>% replace_dist_name("Orissa", "Balasore", "Baleshwar")
src %<>% replace_dist_name("Orissa", "Keonjhar", "Kendujhar")
src %<>% replace_dist_name("Orissa", "Mayurbhanja", "Mayurbhanj")
src %<>% replace_dist_name("Orissa", "Phulbani ( Kandhamal )", "Phulbani")

## 220           Punjab                  Bathinda
## 221           Punjab                  Firozpur
## 229           Punjab                  Rupnagar
src %<>% replace_dist_name("Punjab", "Bhatinda", "Bathinda")
src %<>% replace_dist_name("Punjab", "Ferozpur", "Firozpur")
src %<>% replace_dist_name("Punjab", "Roopnagar / Ropar", "Rupnagar")

## 240        Rajasthan              Chittaurgarh
## 241        Rajasthan              Chittaurgarh
## 244        Rajasthan                 Gangangar
## 247        Rajasthan                     Jalor
## 249        Rajasthan                Jhunjhunun
## 254        Rajasthan            Sawai Madhopur
src %<>% replace_dist_name("Rajasthan", "Chittorgarh", "Chittaurgarh")
src %<>% replace_dist_name("Rajasthan", "Ganganagar", "Gangangar")
src %<>% replace_dist_name("Rajasthan", "Jalore", "Jalor")
src %<>% replace_dist_name("Rajasthan", "Jhunjhunu", "Jhunjhunun")
src %<>% replace_dist_name("Rajasthan", "Swami Madhopur", "Sawai Madhopur")

## 259       Tamil Nadu               Chengaianna
## 261       Tamil Nadu             Kanniyakumari
## 262       Tamil Nadu                    Madras
## 264       Tamil Nadu                  Nilgiris
## 265       Tamil Nadu    North Arcot (Ambedkar)
## 266       Tamil Nadu            Ramanathapuram
## 268       Tamil Nadu               South Arcot
## 270       Tamil Nadu         Tiruchchirappalli
## 271       Tamil Nadu   Tirunelveli Kattabomman
src %<>% replace_dist_name("Tamil Nadu", "Chengalpattu MGR / Kanchipuram", "Chengaianna")
src %<>% replace_dist_name("Tamil Nadu", "Kanyakumari", "Kanniyakumari")
src %<>% replace_dist_name("Tamil Nadu", "The Nilgiris", "Nilgiris")
src %<>% replace_dist_name("Tamil Nadu", "North Arcot / Vellore", "North Arcot (Ambedkar)")
src %<>% replace_dist_name("Tamil Nadu", "Ramananthapuram", "Ramanathapuram")
src %<>% replace_dist_name("Tamil Nadu", "South Arcot / Cuddalore", "South Arcot")
src %<>% replace_dist_name("Tamil Nadu", "Tiruchirapalli / Trichy", "Tiruchchirappalli")
src %<>% replace_dist_name("Tamil Nadu", "Thirunelveli", "Tirunelveli Kattabomman")

## No data for Madras

## 275    Uttar Pradesh                    Almora
## 285    Uttar Pradesh               Bulandshahr
## 287    Uttar Pradesh                 Dehra Dun
## 295    Uttar Pradesh                   Gazipur
## 309    Uttar Pradesh                  Mirzapur
## 313    Uttar Pradesh                Partapgarh
## 315    Uttar Pradesh               Pithoragarh
## 316    Uttar Pradesh               Pithoragarh
## 317    Uttar Pradesh                Rae Bareli
## 323    Uttar Pradesh             Tehri-Garhwal
## 325    Uttar Pradesh                Uttarkashi
## 326    Uttar Pradesh                Uttarkashi
src %<>% replace_dist_name("Uttar Pradesh", "Almorah", "Almora")
src %<>% replace_dist_name("Uttar Pradesh", "Buland Shahar", "Bulandshahr")
src %<>% replace_dist_name("Uttar Pradesh", "Dehradun", "Dehra Dun")
src %<>% replace_dist_name("Uttar Pradesh", "Ghazipur", "Gazipur")
src %<>% replace_dist_name("Uttar Pradesh", "Mirzpur", "Mirzapur")
src %<>% replace_dist_name("Uttar Pradesh", "Pratapgarh", "Partapgarh")
src %<>% replace_dist_name("Uttar Pradesh", "Pithorgarh", "Pithoragarh")
src %<>% replace_dist_name("Uttar Pradesh", "Rae-Bareily", "Rae Bareli")
src %<>% replace_dist_name("Uttar Pradesh", "Tehri Garhwal", "Tehri-Garhwal")
src %<>% replace_dist_name("Uttar Pradesh", "Uttar Kashi", "Uttarkashi")

## 329      West Bengal                Barddhaman
## 330      West Bengal          Birbhum (Situri)
## 331      West Bengal                  Calcutta
## 332      West Bengal                 Darjiling
## 333      West Bengal                     Haora
## 334      West Bengal         Hugli (Chunchura)
## 336      West Bengal                Koch Bihar
## 337      West Bengal                    Maldah
## 338      West Bengal                 Medinipur
## 340      West Bengal      Nadia (Krishnanagar)
## 341      West Bengal                  Puruliya
## 342      West Bengal         South 24 Panganas
src %<>% replace_dist_name("West Bengal", "Burdwan", "Barddhaman")
src %<>% replace_dist_name("West Bengal", "Birbhum", "Birbhum (Situri)")
src %<>% replace_dist_name("West Bengal", "Darjeeling", "Darjiling")
src %<>% replace_dist_name("West Bengal", "Howrah", "Haora")
src %<>% replace_dist_name("West Bengal", "Hooghly", "Hugli (Chunchura)")
src %<>% replace_dist_name("West Bengal", "Cooch Behar", "Koch Bihar")
src %<>% replace_dist_name("West Bengal", "Malda", "Maldah")
src %<>% replace_dist_name("West Bengal", "Midnapur", "Medinipur")
src %<>% replace_dist_name("West Bengal", "Nadia", "Nadia (Krishnanagar)")
src %<>% replace_dist_name("West Bengal", "Purulia", "Puruliya")
src %<>% replace_dist_name("West Bengal", "24 Parganas", "South 24 Panganas")

## No data for Calcutta

## Now merge polygons:
## ###################

new_shp = unionSpatialPolygons(shp, union_ids)
poly_ids = sapply(new_shp@polygons, FUN=function(x) x@ID)
new_data = data.frame(
    NAME1=shp$NAME1,
    NAME2=union_dists,
    POLY_ID=shp$POLY_ID,
    AREA=shp$AREA,
    union_ids=union_ids
)

new_data = new_data[!duplicated(new_data$union_ids),]
new_data = new_data[match(poly_ids, new_data$union_ids),]
row.names(new_data) = poly_ids
new_shp =
    SpatialPolygonsDataFrame(new_shp, data=new_data) %>%
    as("sf") %>%
    rename(state_name=NAME1, dist_name=NAME2)

## ## CHECK
## src_dists = src$Dist.Name %>% unique
## shp_dists = new_shp$dist_name %>% unique
## src_dists[!src_dists %in% shp_dists]

## Quality control
irr_columns = c(
  "canals_area", "tanks_area", "tubewells_area",
  "other_wells_area", "other_sources_area",
  "net_area", "gross_area"
)
lu_columns = c(
  "forest", "barren", "nonagri", "cultivable",
  "permanent_pasture", "other_fallow", "current_fallow",
  "nca", "gca"
)
## apy_columns =
## "RICE.AREA..1000.ha."                           
## "WHEAT.AREA..1000.ha."                          
## "KHARIF.SORGHUM.AREA..1000.ha."                 
## "RABI.SORGHUM.AREA..1000.ha."                   
## "SORGHUM.AREA..1000.ha."                        
## "PEARL.MILLET.AREA..1000.ha."                   
## "MAIZE.AREA..1000.ha."                          
## "FINGER.MILLET.AREA..1000.ha."                  
## "BARLEY.AREA..1000.ha."                         
## "CHICKPEA.AREA..1000.ha."                       
## "PIGEONPEA.AREA..1000.ha."                      
## "MINOR.PULSES.AREA..1000.ha."                   
## "GROUNDNUT.AREA..1000.ha."                      
## "SESAMUM.AREA..1000.ha."                        
## "RAPESEED.AND.MUSTARD.AREA..1000.ha."           
## "SAFFLOWER.AREA..1000.ha."                      
## "CASTOR.AREA..1000.ha."                         
## "LINSEED.AREA..1000.ha."                        
## "SUNFLOWER.AREA..1000.ha."                      
## "SOYABEAN.AREA..1000.ha."                       
## "OILSEEDS.AREA..1000.ha."                       
## "SUGARCANE.AREA..1000.ha."                      
## "COTTON.AREA..1000.ha."                         
## "FRUITS.AREA..1000.ha."                         
## "VEGETABLES.AREA..1000.ha."                     
## "FRUITS.AND.VEGETABLES.AREA..1000.ha."          
## "POTATOES.AREA..1000.ha."                       
## "ONION.AREA..1000.ha."                          
## "FODDER.AREA..1000.ha."                         
    
id_columns = c(
  "state_name", "state_code", "dist_name",
  "dist_code", "POLY_ID", "AREA"
)
x =
    src %>%
    setNames(
        c("dist_code","year","state_code","state_name","dist_name",
          "canals_area","tanks_area","tubewells_area","other_wells_area",
          "total_wells_area","other_sources_area","net_area","gross_area",
          "total_area","forest","barren","nonagri","cultivable",
          "permanent_pasture","other_fallow","current_fallow","nca",
          "gca","cropping_intensity")
    ) %>%
    dplyr::select(state_name, state_code, dist_name, dist_code, year, canals_area:cropping_intensity) %>%
    as_tibble %>%
    arrange(state_name, dist_name)

uids =
    x %>%
    dplyr::select(state_name, dist_name) %>%
    unique %>%
    mutate(ID=1:nrow(.))
x %<>% left_join(uids)
x %<>% mutate(across(where(is.numeric), ~na_if(., -1)))
x %<>%
    left_join(
        new_shp %>%
        dplyr::select(state_name:AREA) %>%
        st_set_geometry(NULL)
    )

x %<>% mutate(AREA=as.numeric(AREA)/1000/1000/10)

complete_years = c(1966:2017)
ids = unique(x$ID)
lst = vector(mode="list", length=length(ids))

for (i in 1:length(ids)) {
    xx = x %>% filter(ID %in% ids[i])
    ## Arrange area in descending area, so that if duplicates
    ## arise the largest area will be retained (we do this because
    ## duplicates may represent small inconsistencies in the polygon
    ## dataset)
    xx %<>% arrange(year, desc(AREA))
    xx = xx[!duplicated(xx$year),]
    ref_df = data.frame(ID=ids[i], year=complete_years)
    xx = left_join(ref_df, xx)
    for (col in id_columns) {
        xx[[col]] %<>% zoo::na.locf(na.rm=FALSE)
    }
    lst[[i]] = xx
}
x = do.call(rbind, lst)

select_dist = function(x, state, dist) {
    x %>% filter((state_name %in% state) & (dist_name %in% dist))
}

update_dist = function(x, y, state, dist) {
    index = (state_name %in% state) & (dist_name %in% dist)
    x[index,] = y
    x
}

## Treat each district in turn, plotting the irrigation sources

plotfun = function(x) {
  irr_sources = c(
    "canals_area", "tanks_area", "tubewells_area",
    "other_wells_area", "total_wells_area",
    "other_sources_area"
  )
  d = x %>%
    dplyr::select(year, all_of(irr_sources)) %>%
    gather(variable, value, -year)
  p = ggplot(d, aes(x=year, y=value, colour=variable, shape=variable)) + ## , size=variable, group=variable)) +
    geom_line(size=0.5) +
    geom_point() +
    scale_x_continuous(breaks=c(1960,2020,10))
  p
}

plotfun2 = function(x) {
    d = x %>%
        dplyr::select(
                   year,
                   all_of(c("net_area", "gross_area", "total_area"))
               ) %>%
        gather(variable, value, -year)
    p = ggplot(d, aes(x=year, y=value, colour=variable, shape=variable)) + ## , size=variable, group=variable)) +
        geom_line(size=0.5) +
        geom_point() +
        scale_x_continuous(breaks=c(1960,2020,10))
    p
}

plotfun3 = function(x) {
    d = x %>%
        dplyr::select(year, all_of(c("nca","gca"))) %>%
        gather(variable, value, -year)
    p = ggplot(d, aes(x=year, y=value, colour=variable, shape=variable)) + ## , size=variable, group=variable)) +
        geom_line(size=0.5) +
        geom_point() +
        scale_x_continuous(breaks=c(1960,2020,10))
    p
}

rescale_irr_fun = function(x) {
    net_area = x$net_area
    irr_cols = c(
      "canals_area","tanks_area","tubewells_area",
      "other_wells_area","other_sources_area"
    )
    calc_net_area = apply(x[,irr_cols], 1, sum)
    sf = (net_area / calc_net_area) %>% unname
    sf %<>% `[<-`(!is.finite(.), 0)
    ## x %<>% mutate_each(funs(.*sf), all_of(irr_cols))
    x %<>% mutate_each(~.*sf, all_of(irr_cols))
    x$total_wells_area = apply(x[,c("tubewells_area","other_wells_area")], 1, sum)
    x
}

rescale_lu_fun = function(x) {
    ## total_area = x$total_area
    total_area = x$AREA
    lu_cols = c(
      "forest","barren","nonagri","cultivable",
      "permanent_pasture","other_fallow",
      "current_fallow","nca"
    )
    calc_lu_area = apply(x[,lu_cols], 1, sum)
    sf = (total_area / calc_lu_area) %>% unname
    sf %<>% `[<-`(!is.finite(.), 0)
    ## x %<>% mutate_each(funs(.*sf), all_of(c(lu_cols, "gca")))
    x %<>% mutate_each(~.*sf, all_of(c(lu_cols, "gca")))
    x$total_area = apply(x[,lu_cols], 1, sum)
    x
}

rescale_fun = function(x) {
    irrigated_fraction = x$net_area / x$nca
    irrigation_intensity = x$gross_area / x$net_area
    cropping_intensity = x$gca / x$nca
    irrigated_fraction %<>% `[<-`(!is.finite(.), 0)
    irrigation_intensity %<>% `[<-`(!is.finite(.), 0)
    cropping_intensity %<>% `[<-`(!is.finite(.), 0)
    ## Ensure total land use sums to polygon area
    x %<>% rescale_lu_fun()
    ## Assume irrigated fraction remains the same
    x$net_area = x$nca * irrigated_fraction
    x$gross_area = x$net_area * irrigation_intensity
    ## Ensure constituent parts match net_area
    x %<>% rescale_irr_fun()
    x
}

linear_interp = function(x, na_ix, ...) {
    x[na_ix] = NA
    x = zoo::na.approx(x, na.rm=FALSE)
    x
}

locf_interp = function(x, na_ix, ...) {
    x[na_ix] = NA
    x = zoo::na.locf(x, na.rm=FALSE, ...)
    x
}

replace_fun = function(x, y) {
    dist_code = y$dist_code %>% unique
    x_ix = x$dist_code %in% dist_code
    x = x[!x_ix,]
    x = rbind(x, y) %>% arrange(state_name, dist_name)
    x
}

## ################################### ##
## Uttar Pradesh
## ################################### ##

state = "Uttar Pradesh"
dists = uids %>% filter(state_name %in% state) %>% `$`(dist_name)

## Agra - CHECKED
y = select_dist(x, state, "Agra")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Aligarh - CHECKED
y = select_dist(x, state, "Aligarh")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Allahabad - CHECKED
y = select_dist(x, state, "Allahabad")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Almora [Uttarakhand] - CHECKED
y = select_dist(x, state, "Almora")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Azamgarh - CHECKED
y = select_dist(x, state, "Azamgarh")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Bahraich - CHECKED
y = select_dist(x, state, "Bahraich")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Ballia
y = select_dist(x, state, "Ballia")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Banda
y = select_dist(x, state, "Banda")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Barabanki
y = select_dist(x, state, "Barabanki")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Bareilly
y = select_dist(x, state, "Bareilly")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Basti
y = select_dist(x, state, "Basti")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Bijnor
y = select_dist(x, state, "Bijnor")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Budaun
y = select_dist(x, state, "Budaun")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Bulandshahr
y = select_dist(x, state, "Bulandshahr")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Chamoli [Uttarakhand]
y = select_dist(x, state, "Chamoli")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Dehra Dun [Uttarakhand]
y = select_dist(x, state, "Dehra Dun")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Deoria
y = select_dist(x, state, "Deoria")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Etah
y = select_dist(x, state, "Etah")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Etawah
y = select_dist(x, state, "Etawah")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Faizabad
y = select_dist(x, state, "Faizabad")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Farrukhabad
y = select_dist(x, state, "Farrukhabad")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Fatehpur
y = select_dist(x, state, "Fatehpur")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Garhwal
y = select_dist(x, state, "Garhwal")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Gazipur
y = select_dist(x, state, "Gazipur")
y %>% plotfun()
y$gca[18] = NA
y[["gca"]] %<>% zoo::na.approx(na.rm=FALSE)
y[42,irr_columns] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Gonda
y = select_dist(x, state, "Gonda")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Gorakhpur
y = select_dist(x, state, "Gorakhpur")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Hamirpur
y = select_dist(x, state, "Hamirpur")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Hardoi
y = select_dist(x, state, "Hardoi")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Jalaun
y = select_dist(x, state, "Jalaun")
y %>% plotfun()
y[42,irr_columns] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Jaunpur
y = select_dist(x, state, "Jaunpur")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Jhansi
y = select_dist(x, state, "Jhansi")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Kanpur
y = select_dist(x, state, "Kanpur")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Kheri
y = select_dist(x, state, "Kheri")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Lucknow
y = select_dist(x, state, "Lucknow")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Mainpuri
y = select_dist(x, state, "Mainpuri")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Mathura
y = select_dist(x, state, "Mathura")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Meerut
y = select_dist(x, state, "Meerut")
y %>% plotfun()
y[23,irr_columns] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Mirzapur
y = select_dist(x, state, "Mirzapur")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Moradabad
y = select_dist(x, state, "Moradabad")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Muzaffarnagar
y = select_dist(x, state, "Muzaffarnagar")
y %>% plotfun()
y[49,irr_columns] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Nainital
y = select_dist(x, state, "Nainital")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Partapgarh
y = select_dist(x, state, "Partapgarh")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Pilibhit
y = select_dist(x, state, "Pilibhit")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Pithoragarh [Uttarakhand]
y = select_dist(x, state, "Pithoragarh")
y = y[!duplicated(y$year),]
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Rae Bareli
y = select_dist(x, state, "Rae Bareli")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Rampur
y = select_dist(x, state, "Rampur")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Saharanpur
y = select_dist(x, state, "Saharanpur")
y %>% plotfun()
y[52,lu_columns] = NA
y[52,irr_columns] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Shahjahanpur
y = select_dist(x, state, "Shahjahanpur")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Sitapur
y = select_dist(x, state, "Sitapur")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Sultanpur
y = select_dist(x, state, "Sultanpur")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Tehri-Garhwal [Uttarakhand]
y = select_dist(x, state, "Tehri-Garhwal")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Unnao
y = select_dist(x, state, "Unnao")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Uttarkashi
y = select_dist(x, state, "Uttarkashi")
y = y[!duplicated(y$year),]
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Varanasi
y = select_dist(x, state, "Varanasi")
y %>% plotfun()
y[29:30,irr_columns] = NA
y[42,irr_columns] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

xx =
  x %>%
  filter(state_name %in% "Uttar Pradesh") %>%
  dplyr::select(-state_code, -dist_code, canals_area:gca) %>%
  gather(name, value, -state_name, -dist_name, -year, -ID) %>%
  group_by(state_name, year, name) %>%
  summarise(total = sum(value, na.rm=TRUE)) %>%
  spread(name, total) %>%
  ungroup()

xx %>% plotfun()
xx %>% plotfun2()
xx %>% plotfun3()

## ################################### ##
## Bihar
## ################################### ##

state = "Bihar"
dists = uids %>% filter(state_name %in% state) %>% `$`(dist_name)

## Bhagalpur
y = select_dist(x, state, "Bhagalpur")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Champaran
y = select_dist(x, state, "Champaran")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Darbhanga
y = select_dist(x, state, "Darbhanga")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Dhanbad
y = select_dist(x, state, "Dhanbad")
y %>% plotfun()
y[30:32,c(irr_columns)] = NA
y[32,c(lu_columns)] = NA
y[46,c(irr_columns,lu_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Dumka(Santhal Parganas)
y = select_dist(x, state, "Dumka(Santhal Parganas)")
y %>% plotfun()
y[30:32,c(irr_columns)] = NA
y[32,c(lu_columns)] = NA
y[46,c(irr_columns,lu_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Gaya
y = select_dist(x, state, "Gaya")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Hazaribag
y = select_dist(x, state, "Hazaribag")
y %>% plotfun()
y[30:32,c(irr_columns)] = NA
y[32,c(lu_columns)] = NA
y[46,c(irr_columns,lu_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Munger
y = select_dist(x, state, "Munger")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Muzzaffarpur
y = select_dist(x, state, "Muzzaffarpur")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Palamu (Daltenganj)
y = select_dist(x, state, "Palamu (Daltenganj)")
y %>% plotfun()
y[30:32,c(irr_columns)] = NA
y[32,c(lu_columns)] = NA
y[46,c(irr_columns,lu_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Patna
y = select_dist(x, state, "Patna")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Purnia
y = select_dist(x, state, "Purnia")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Ranchi
y = select_dist(x, state, "Ranchi")
y %>% plotfun()
y[30:32,c(irr_columns)] = NA
y[32,c(lu_columns)] = NA
y[46,c(irr_columns,lu_columns)] = NA
y[50,c(irr_columns,lu_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Sahabad
y = select_dist(x, state, "Sahabad")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Saharsa
y = select_dist(x, state, "Saharsa")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Saran
y = select_dist(x, state, "Saran")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Singhbhum
y = select_dist(x, state, "Singhbhum")
y %>% plotfun()
y[30:32,c(irr_columns)] = NA
y[32,c(lu_columns)] = NA
y[46,c(irr_columns,lu_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

xx =
  x %>%
  filter(state_name %in% "Bihar") %>%
  dplyr::select(-state_code, -dist_code, canals_area:gca) %>%
  gather(name, value, -state_name, -dist_name, -year, -ID) %>%
  group_by(state_name, year, name) %>%
  summarise(total = sum(value, na.rm=TRUE)) %>%
  spread(name, total) %>%
  ungroup()

xx %>% plotfun()
xx %>% plotfun2()
xx %>% plotfun3()

## ################################### ##
## West Bengal
## ################################### ##

state = "West Bengal"
dists = uids %>% filter(state_name %in% state) %>% `$`(dist_name)

## ## Agricultural statistics?
## agcensus =
##   read.csv("agcensus/state_combined_data.csv") %>%
##   filter(State %in% "West Bengal") %>%
##   dplyr::select(
##            State, Year,
##            Forests:Total.Cropped.Area,
##            Canal.Government.NIA:Total.NIA
##          ) %>%
##   dplyr::select(-Total, -Total.1, -Total.2) %>%
##   setNames(
##     c("state_name","year","forest","nonagri",
##       "barren","permanent_pasture","tree_crops",
##       "cultivable","other_fallow","current_fallow",
##       "nca","gca","canals_govt_area","canals_private_area",
##       "canals_area","tanks_area","tubewells_area",
##       "other_wells_area","other_sources_area"))

## Bankura
y = select_dist(x, state, "Bankura")
y %>% plotfun()
y[1:47,c(lu_columns,irr_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Barddhaman
y = select_dist(x, state, "Barddhaman")
y %>% plotfun()
y[1:47,c(lu_columns,irr_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Birbhum (Situri)
y = select_dist(x, state, "Birbhum (Situri)")
y %>% plotfun()
y[1:47,c(lu_columns,irr_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Darjiling
y = select_dist(x, state, "Darjiling")
y %>% plotfun()
y[1:47,c(lu_columns,irr_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Haora
y = select_dist(x, state, "Haora")
y %>% plotfun()
y[1:47,c(lu_columns,irr_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Hugli (Chunchura)
y = select_dist(x, state, "Hugli (Chunchura)")
y %>% plotfun()
y[1:47,c(lu_columns,irr_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Jalpaiguri
y = select_dist(x, state, "Jalpaiguri")
y %>% plotfun()
y[1:47,c(lu_columns,irr_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Koch Bihar
y = select_dist(x, state, "Koch Bihar")
y %>% plotfun()
y[1:47,c(lu_columns,irr_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Maldah
y = select_dist(x, state, "Maldah")
y %>% plotfun()
y[1:47,c(lu_columns,irr_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Medinipur
y = select_dist(x, state, "Medinipur")
y %>% plotfun()
y[1:47,c(lu_columns,irr_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Murshidabad
y = select_dist(x, state, "Murshidabad")
y %>% plotfun()
y[1:47,c(lu_columns,irr_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Nadia (Krishnanagar)
y = select_dist(x, state, "Nadia (Krishnanagar)")
y %>% plotfun()
y[1:47,c(lu_columns,irr_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Puruliya
y = select_dist(x, state, "Puruliya")
y %>% plotfun()
y[1:47,c(lu_columns,irr_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## South 24 Panganas
y = select_dist(x, state, "South 24 Panganas")
y %>% plotfun()
y[1:47,c(lu_columns,irr_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## West Dinajpur
y = select_dist(x, state, "West Dinajpur")
y %>% plotfun()
y[1:47,c(lu_columns,irr_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

xx =
  x %>%
  filter(state_name %in% "West Bengal") %>%
  dplyr::select(-state_code, -dist_code, canals_area:gca) %>%
  gather(name, value, -state_name, -dist_name, -year, -ID) %>%
  group_by(state_name, year, name) %>%
  summarise(total = sum(value, na.rm=TRUE)) %>%
  spread(name, total) %>%
  ungroup()

xx %>% plotfun()
xx %>% plotfun2()
xx %>% plotfun3()

## ################################### ##
## Haryana
## ################################### ##

state = "Haryana"
dists = uids %>% filter(state_name %in% state) %>% `$`(dist_name)

## Ambala
y = select_dist(x, state, "Ambala")
y %>% plotfun()
y[45,"gross_area"] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Gurgaon
y = select_dist(x, state, "Gurgaon")
y %>% plotfun()
y[45,"gross_area"] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Hissar
y = select_dist(x, state, "Hissar")
y %>% plotfun()
y[45,"gross_area"] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Jind
y = select_dist(x, state, "Jind")
y %>% plotfun()
y[c(39,45),"gross_area"] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Karnal
y = select_dist(x, state, "Karnal")
y %>% plotfun()
y[45,"gross_area"] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Mahendragarh
y = select_dist(x, state, "Mahendragarh")
y %>% plotfun()
y[45,"gross_area"] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Rohtak
y = select_dist(x, state, "Rohtak")
y %>% plotfun()
y[45,"gross_area"] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

xx =
  x %>%
  filter(state_name %in% "Haryana") %>%
  dplyr::select(-state_code, -dist_code, canals_area:gca) %>%
  gather(name, value, -state_name, -dist_name, -year, -ID) %>%
  group_by(state_name, year, name) %>%
  summarise(total = sum(value, na.rm=TRUE)) %>%
  spread(name, total) %>%
  ungroup()

xx %>% plotfun()
xx %>% plotfun2()
xx %>% plotfun3()

## ################################### ##
## Himachal Pradesh
## ################################### ##

state = "Himachal Pradesh"
dists = uids %>% filter(state_name %in% state) %>% `$`(dist_name)

## Bilaspur
y = select_dist(x, state, "Bilaspur")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Chamba
y = select_dist(x, state, "Chamba")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Kangra
y = select_dist(x, state, "Kangra")
y %>% plotfun()
y[c(26,27,30,35,37),irr_columns] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Kinnaur
y = select_dist(x, state, "Kinnaur")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Kullu
y = select_dist(x, state, "Kullu")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Lahul and Spiti
y = select_dist(x, state, "Lahul and Spiti")
y %>% plotfun()
y[c(2:8),c(lu_columns,irr_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Mandi
y = select_dist(x, state, "Mandi")
y %>% plotfun()
y[c(29,30), "gross_area"] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Shimla
y = select_dist(x, state, "Shimla")
y %>% plotfun()
y[c(1:6), c(lu_columns, irr_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Sirmaur (Nahan)
y = select_dist(x, state, "Sirmaur (Nahan)")
y %>% plotfun()
y[c(28:30), "gross_area"] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Solan
y = select_dist(x, state, "Solan")
y %>% plotfun()
y[c(29:30), "gross_area"] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

xx =
  x %>%
  filter(state_name %in% "Himachal Pradesh") %>%
  dplyr::select(-state_code, -dist_code, canals_area:gca) %>%
  gather(name, value, -state_name, -dist_name, -year, -ID) %>%
  group_by(state_name, year, name) %>%
  summarise(total = sum(value, na.rm=TRUE)) %>%
  spread(name, total) %>%
  ungroup()

xx %>% plotfun()
xx %>% plotfun2()
xx %>% plotfun3()

## ################################### ##
## Rajasthan
## ################################### ##

state = "Rajasthan"
dists = uids %>% filter(state_name %in% state) %>% `$`(dist_name)

## Ajmer
y = select_dist(x, state, "Ajmer")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Alwar
y = select_dist(x, state, "Alwar")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Banswara
y = select_dist(x, state, "Banswara")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Barmer
y = select_dist(x, state, "Barmer")
y %>% plotfun()
y[52,"canals_area"] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Bharatpur
y = select_dist(x, state, "Bharatpur")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Bhilwara
y = select_dist(x, state, "Bhilwara")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Bikaner
y = select_dist(x, state, "Bikaner")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Bundi
y = select_dist(x, state, "Bundi")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Chittaurgarh
y = select_dist(x, state, "Chittaurgarh")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Churu
y = select_dist(x, state, "Churu")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Dungarpur
y = select_dist(x, state, "Dungarpur")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Gangangar
y = select_dist(x, state, "Gangangar")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Jaipur
y = select_dist(x, state, "Jaipur")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Jaisalmer
y = select_dist(x, state, "Jaisalmer")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Jalor
y = select_dist(x, state, "Jalor")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Jhalawar
y = select_dist(x, state, "Jhalawar")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Jhunjhunun
y = select_dist(x, state, "Jhunjhunun")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Jodhpur
y = select_dist(x, state, "Jodhpur")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Kota
y = select_dist(x, state, "Kota")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Nagaur
y = select_dist(x, state, "Nagaur")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Pali
y = select_dist(x, state, "Pali")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Sawai Madhopur
y = select_dist(x, state, "Sawai Madhopur")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Sikar
y = select_dist(x, state, "Sikar")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Sirohi
y = select_dist(x, state, "Sirohi")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Tonk
y = select_dist(x, state, "Tonk")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Udaipur
y = select_dist(x, state, "Udaipur")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

xx =
  x %>%
  filter(state_name %in% "Rajasthan") %>%
  dplyr::select(-state_code, -dist_code, canals_area:gca) %>%
  gather(name, value, -state_name, -dist_name, -year, -ID) %>%
  group_by(state_name, year, name) %>%
  summarise(total = sum(value, na.rm=TRUE)) %>%
  spread(name, total) %>%
  ungroup()

xx %>% plotfun()
xx %>% plotfun2()
xx %>% plotfun3()

## ################################### ##
## Madhya Pradesh
## ################################### ##

state = "Madhya Pradesh"
dists = uids %>% filter(state_name %in% state) %>% `$`(dist_name)

## Balaghat
y = select_dist(x, state, "Balaghat")
y %>% plotfun()
y$forest[c(25,41:46)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Bastar (Jagdalpur)
y = select_dist(x, state, "Bastar (Jagdalpur)")
y %>% plotfun()
y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25,28:32)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Betul
y = select_dist(x, state, "Betul")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25,41:46)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Bhind
y = select_dist(x, state, "Bhind")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
## y$forest[c(25,41:46)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Bilaspur
y = select_dist(x, state, "Bilaspur")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25,28:32)] = NA
y[48:50,lu_columns] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Chhatrapur
y = select_dist(x, state, "Chhatrapur")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
## y$forest[c(25,41:46)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Chhindwara
y = select_dist(x, state, "Chhindwara")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Damoh
y = select_dist(x, state, "Damoh")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Datia
y = select_dist(x, state, "Datia")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
## y$forest[c(25,41:46)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Dewas
y = select_dist(x, state, "Dewas")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Dhar
y = select_dist(x, state, "Dhar")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25)] = NA
y$nonagri[c(8)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Durg
y = select_dist(x, state, "Durg")
y %>% plotfun()
y[49:52,irr_columns] = NA
## y$tubewells_area[49:40] = NA
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25,48:50)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## East Nimar (Khandwa)
y = select_dist(x, state, "East Nimar (Khandwa)")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Guna
y = select_dist(x, state, "Guna")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Gwalior
y = select_dist(x, state, "Gwalior")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
## y$forest[c(25,41:46)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Hoshangabad
y = select_dist(x, state, "Hoshangabad")
y %>% plotfun()
y[c(48,51),c(irr_columns)] = NA
y$forest[c(25,41:46)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Indore
y = select_dist(x, state, "Indore")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25,41:46)] = NA
y$cultivable[c(36:43)] = 0
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Jabalpur
y = select_dist(x, state, "Jabalpur")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
## y$forest[c(25,41:46)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Jhabua
y = select_dist(x, state, "Jhabua")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Mandla
y = select_dist(x, state, "Mandla")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Mandsaur
y = select_dist(x, state, "Mandsaur")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(41:46)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Morena
y = select_dist(x, state, "Morena")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25)] = NA
y[37,lu_columns] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Narsimhapur
y = select_dist(x, state, "Narsimhapur")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Panna
y = select_dist(x, state, "Panna")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Raigarh
y = select_dist(x, state, "Raigarh")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
## y$forest[c(25,41:46)] = NA
y[c(25,48:50),lu_columns] = NA
y$forest[c(28:32,34)] = NA
y$permanent_pasture[28:34] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Raipur
y = select_dist(x, state, "Raipur")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
## y$forest[c(25,41:46)] = NA
y[c(25,48:50),lu_columns] = NA
y$forest[c(28:32)] = NA
## y$permanent_pasture[28:34] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Raisen
y = select_dist(x, state, "Raisen")
y %>% plotfun()
y$forest[c(8:10,25,41:46)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Rajgarh
y = select_dist(x, state, "Rajgarh")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(1:4)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Ratlam
y = select_dist(x, state, "Ratlam")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Rewa
y = select_dist(x, state, "Rewa")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Sagar
y = select_dist(x, state, "Sagar")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Satna
y = select_dist(x, state, "Satna")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Sehore
y = select_dist(x, state, "Sehore")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25,41:46)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Seoni
y = select_dist(x, state, "Seoni")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Shahdol
y = select_dist(x, state, "Shahdol")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Shajapur
y = select_dist(x, state, "Shajapur")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
## y$forest[c(25,41:46)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Shivpuri
y = select_dist(x, state, "Shivpuri")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Sidhi
y = select_dist(x, state, "Sidhi")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25)] = NA
y[50,irr_columns] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Surguja (Ambikapur)
y = select_dist(x, state, "Surguja (Ambikapur)")
y %>% plotfun()
y$forest[c(25,28:32,48:50)] = NA
y[50,irr_columns] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Tikamgarh
y = select_dist(x, state, "Tikamgarh")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
## y$forest[c(25,41:46)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Ujjain
y = select_dist(x, state, "Ujjain")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
## y$forest[c(25,41:46)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Vidisha
y = select_dist(x, state, "Vidisha")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## West Nimar (Khargon)
y = select_dist(x, state, "West Nimar (Khargon)")
y %>% plotfun()
## y[47:50,c(lu_columns,irr_columns)] = NA
y$forest[c(25,28:32,47:50)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

xx =
  x %>%
  filter(state_name %in% "Madhya Pradesh") %>%
  dplyr::select(-state_code, -dist_code, canals_area:gca) %>%
  gather(name, value, -state_name, -dist_name, -year, -ID) %>%
  group_by(state_name, year, name) %>%
  summarise(total = sum(value, na.rm=TRUE)) %>%
  spread(name, total) %>%
  ungroup()

xx %>% plotfun()
xx %>% plotfun2()
xx %>% plotfun3()

## ################################### ##
## Punjab
## ################################### ##

state = "Punjab"
dists = uids %>% filter(state_name %in% state) %>% `$`(dist_name)

## Amritsar
y = select_dist(x, state, "Amritsar")
y %>% plotfun()
y[50,c(irr_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Bathinda
y = select_dist(x, state, "Bathinda")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Firozpur
y = select_dist(x, state, "Firozpur")
y %>% plotfun()
y[c(46,50),irr_columns] = NA
y[c(46),lu_columns] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Gurdaspur
y = select_dist(x, state, "Gurdaspur")
y %>% plotfun()
y[c(22,46),lu_columns] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Hoshiarpur
y = select_dist(x, state, "Hoshiarpur")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Jalandhar
y = select_dist(x, state, "Jalandhar")
y %>% plotfun()
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Kapurthala
y = select_dist(x, state, "Kapurthala")
y %>% plotfun()
y[c(30:31),lu_columns] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Ludhiana
y = select_dist(x, state, "Ludhiana")
y %>% plotfun()
y[c(48),lu_columns] = NA
for (col in c(lu_columns, irr_columns)) {
    y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
    y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
    y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Patiala
y = select_dist(x, state, "Patiala")
y %>% plotfun()
y[50, irr_columns] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Rupnagar
y = select_dist(x, state, "Rupnagar")
y %>% plotfun()
y[50, irr_columns] = NA
for (col in c(irr_columns)) { # LOCF first
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

## Sangrur
y = select_dist(x, state, "Sangrur")
y %>% plotfun()
y[c(31:32,50), c(lu_columns, irr_columns)] = NA
for (col in c(lu_columns, irr_columns)) { # LOCF first
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()
## return updated data frame to main data frame
x %<>% replace_fun(y)

xx =
  x %>%
  filter(state_name %in% "Punjab") %>%
  dplyr::select(-state_code, -dist_code, canals_area:gca) %>%
  gather(name, value, -state_name, -dist_name, -year, -ID) %>%
  group_by(state_name, year, name) %>%
  summarise(total = sum(value, na.rm=TRUE)) %>%
  spread(name, total) %>%
  ungroup()

xx %>% plotfun()
xx %>% plotfun2()
xx %>% plotfun3()

## Delhi

## load("~/dev/r_indagridat/indagridat/data/indiastat1960.rda")
load("~/projects/india_crop_area_2005/pkg/r_indagridat/indagridat/data/indiastat1960.rda")
y =
  indiastat1960 %>%
  filter(State %in% "Delhi") %>%
  dplyr::select(State, District, Year, Forest:Total_GIA) %>%
  dplyr::select(
           -Total_Unavailable,
           -Total_Other_Uncultivated,
           -Total_Fallow,
           -Multiple_Cropped_Area,
           -(Canal_Gov_GIA:Other_Sources_GIA)
         ) %>%
  setNames(c(
    "state_name", "dist_name", "year", "forest",
    "nonagri", "barren", "permanent_pasture", "tree_crops",
    "cultivable", "other_fallow", "current_fallow",
    "nca", "gca", "canals_gov_area", "canals_pvt_area",
    "canals_area", "tanks_area", "tubewells_area",
    "other_wells_area", "other_sources_area",
    "net_area", "gross_area"))

y$tree_crops[1] = NA
y$nca = apply(y[,c("nca","tree_crops")], 1, sum, na.rm=TRUE)
y$nca[y$nca==0] = NA
y$canals_area = apply(y[,c("canals_gov_area","canals_pvt_area")], 1, sum, na.rm=TRUE)
y %<>% dplyr::select(-tree_crops, -canals_gov_area, -canals_pvt_area)

## Add some columns
y %<>% cbind(
         data.frame(
           ID=NA,
           state_code=NA,
           dist_code=NA,
           total_wells_area=NA,
           AREA=NA,
           total_area=NA,
           POLY_ID=NA,
           cropping_intensity=NA
         )
       )

## Reorder columns
y = y[,match(names(x), names(y))]
y$year %<>% strsplit("-") %>% sapply(FUN=function(x) x[1]) %>% as.numeric
y = left_join(data.frame(year=complete_years), y)

y$ID = 999
y$state_code = 999
y$dist_code = 2701
y$total_wells_area = apply(y[,c("tubewells_area","other_wells_area")], 1, sum, na.rm=TRUE)
y$AREA = 1398432237/1000/1000/10
y$total_area = y$AREA
y$POLY_ID = 71
y$cropping_intensity = y$gca / y$nca

y %>% plotfun()
y[c(1:4,28:34),irr_columns] = NA
y[c(48:52),c(lu_columns,irr_columns)] = NA
## y[32,c(lu_columns,irr_columns)] = NA
for (col in c(lu_columns, irr_columns)) {
  y[[col]] %<>% zoo::na.approx(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE)
  y[[col]] %<>% zoo::na.locf(na.rm=FALSE, fromLast=TRUE)
}
y %>% plotfun()
## y %<>% rescale_fun()
y %>% plotfun()
y %>% plotfun2()
y %>% plotfun3()

## Add to main dataframe
x = rbind(x, y) %>% arrange(state_name, dist_name)

## Convert *1000 Ha to km2 by multiplying by 10

x$canals_area %<>% `*`(10)
x$tanks_area %<>% `*`(10)
x$tubewells_area %<>% `*`(10)
x$other_wells_area %<>% `*`(10)
x$total_wells_area %<>% `*`(10)
x$other_sources_area %<>% `*`(10)
x$net_area %<>% `*`(10)
x$gross_area %<>% `*`(10)
x$total_area %<>% `*`(10)
x$forest %<>% `*`(10)
x$barren %<>% `*`(10)
x$nonagri %<>% `*`(10)
x$cultivable %<>% `*`(10)
x$permanent_pasture %<>% `*`(10)
x$other_fallow %<>% `*`(10)
x$current_fallow %<>% `*`(10)
x$nca %<>% `*`(10)
x$gca %<>% `*`(10)

yrs = x$year %>% unique %>% sort
for (i in 1:length(yrs)) {
    yr = yrs[i]
    x_yr = x %>% filter(year %in% yr)    
    shp_yr = new_shp %>% dplyr::select(-AREA) %>% left_join(x_yr)
    ## https://gis.stackexchange.com/a/251051
    if (i == 1) {
        st_write(
            shp_yr,
            dsn="irrigated_area_source.gpkg",
            layer=paste0("x", yr),
            append=FALSE,
            delete_dsn=TRUE
        )
    } else {
        st_write(
            shp_yr,
            dsn="irrigated_area_source.gpkg",
            layer=paste0("x", yr),
            append=TRUE
        )
    }    
}

st_layers("irrigated_area_source.gpkg")

