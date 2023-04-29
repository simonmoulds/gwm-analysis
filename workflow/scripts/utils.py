#!/usr/bin/env python3


def write_reclass_rules():
    # Implement Table 2 in:
    # Poulter et al. (2015) Plant functional type
    # classification for earth system models.
    # Geosci. Model Dev. 8, 2315--2328
    write_tree_broadleaf_deciduous_reclass_rules()
    write_tree_broadleaf_evergreen_reclass_rules()
    write_tree_needleleaf_deciduous_reclass_rules()
    write_tree_needleleaf_evergreen_reclass_rules()
    write_shrub_broadleaf_deciduous_reclass_rules()
    write_shrub_broadleaf_evergreen_reclass_rules()
    write_shrub_needleleaf_deciduous_reclass_rules()
    write_shrub_needleleaf_evergreen_reclass_rules()
    write_natural_grass_reclass_rules()
    write_managed_grass_reclass_rules()
    write_urban_reclass_rules()
    write_bare_soil_reclass_rules()
    write_water_reclass_rules()
    write_snow_ice_reclass_rules()
    write_nodata_reclass_rules()


def write_tree_broadleaf_evergreen_reclass_rules():
    tree_broadleaf_evergreen_rules = [
        "30  = 50",
        "40  = 50",
        "50  = 900",
        "100 = 100",
        "110 = 50",
        "150 = 10",
        "160 = 300",
        "170 = 600",
        "*   = 0"
    ]
    with open('results/intermediate/tree_broadleaf_evergreen_reclass.txt', 'w') as f:
        f.writelines([s + '\n' for s in tree_broadleaf_evergreen_rules])


def write_tree_broadleaf_deciduous_reclass_rules():
    tree_broadleaf_deciduous_rules = [
        "30  = 50",
        "40  = 50",
        "60  = 700",
        "61  = 700",
        "62  = 300",
        "90  = 300",
        "100 = 200",
        "110 = 100",
        "150 = 30",
        "151 = 20",
        "160 = 300",
        "180 = 50",
        "190 = 25",
        "*   = 0"
    ]
    with open('results/intermediate/tree_broadleaf_deciduous_reclass.txt', 'w') as f:
        f.writelines([s + '\n' for s in tree_broadleaf_deciduous_rules])


def write_tree_needleleaf_evergreen_reclass_rules():
    tree_needleleaf_evergreen_rules = [
        "70  = 700",
        "71  = 700",
        "72  = 300",
        "90  = 200",
        "100 = 50",
        "110 = 50",
        "150 = 10",
        "151 = 60",
        "180 = 100",
        "190 = 25",
        "*  = 0"
    ]
    with open('results/intermediate/tree_needleleaf_evergreen_reclass.txt', 'w') as f:
        f.writelines([s + '\n' for s in tree_needleleaf_evergreen_rules])


def write_tree_needleleaf_deciduous_reclass_rules():
    tree_needleleaf_deciduous_rules = [
        "80  = 700",
        "81  = 700",
        "82  = 300",
        "90  = 100",
        "100 = 50",
        "151 = 20",
        "*   = 0"
    ]
    with open('results/intermediate/tree_needleleaf_deciduous_reclass.txt', 'w') as f:
        f.writelines([s + '\n' for s in tree_needleleaf_deciduous_rules])


def write_shrub_broadleaf_evergreen_reclass_rules():
    shrub_broadleaf_evergreen_rules = [
        "30  = 50",
        "40  = 75",
        "50  = 50",
        "70  = 50",
        "71  = 50",
        "80  = 50",
        "81  = 50",
        "90  = 50",
        "100 = 50",
        "110 = 50",
        "120 = 200",
        "121 = 300",
        "150 = 10",
        "152 = 20",
        "170 = 200",
        "*   = 0"
    ]
    with open('results/intermediate/shrub_broadleaf_evergreen_reclass.txt', 'w') as f:
        f.writelines([s + '\n' for s in shrub_broadleaf_evergreen_rules])


def write_shrub_broadleaf_deciduous_reclass_rules():
    shrub_broadleaf_deciduous_rules = [
        "12  = 500",
        "30  = 50",
        "40  = 100",
        "50  = 50",
        "60  = 150",
        "61  = 150",
        "62  = 250",
        "70  = 50",
        "71  = 50",
        "72  = 50",
        "80  = 50",
        "81  = 50",
        "82  = 50",
        "90  = 50",
        "100 = 100",
        "110 = 100",
        "120 = 200",
        "122 = 600",
        "150 = 30",
        "152 = 60",
        "180 = 100",
        "*   = 0"
    ]
    with open('results/intermediate/shrub_broadleaf_deciduous_reclass.txt', 'w') as f:
        f.writelines([s + '\n' for s in shrub_broadleaf_deciduous_rules])


def write_shrub_needleleaf_evergreen_reclass_rules():
    shrub_needleleaf_evergreen_rules = [
        "30  = 50",
        "40  = 75",
        "70  = 50",
        "71  = 50",
        "72  = 50",
        "80  = 50",
        "81  = 50",
        "82  = 50",
        "90  = 50",
        "100 = 50",
        "110 = 50",
        "120 = 200",
        "121 = 300",
        "150 = 10",
        "152 = 20",
        "180 = 50",
        "*   = 0"
    ]
    with open('results/intermediate/shrub_needleleaf_evergreen_reclass.txt', 'w') as f:
        f.writelines([s + '\n' for s in shrub_needleleaf_evergreen_rules])


def write_shrub_needleleaf_deciduous_reclass_rules():
    shrub_needleleaf_deciduous_rules = ["*  = 0"]
    with open('results/intermediate/shrub_needleleaf_deciduous_reclass.txt', 'w') as f:
        f.writelines([s + '\n' for s in shrub_needleleaf_deciduous_rules])

def write_natural_grass_reclass_rules():
    natural_grass_rules = [
        "30  = 150",
        "40  = 250",
        "60  = 150",
        "61  = 150",
        "62  = 350",
        "70  = 150",
        "71  = 150",
        "72  = 300",
        "80  = 150",
        "81  = 150",
        "82  = 300",
        "90  = 150",
        "100 = 400",
        "110 = 600",
        "120 = 200",
        "121 = 200",
        "122 = 200",
        "130 = 600",
        "140 = 600",
        "150 = 50",
        "151 = 50",
        "152 = 50",
        "153 = 150",
        "160 = 200",
        "180 = 400",
        "190 = 150",
        "*   = 0"
    ]
    with open('results/intermediate/natural_grass_reclass.txt', 'w') as f:
        f.writelines([s + '\n' for s in natural_grass_rules])


def write_managed_grass_reclass_rules():
    managed_grass_rules = [
        "10 = 1000",
        "11 = 1000",
        "12 = 500",
        "20 = 1000",
        "30 = 600",
        "40 = 400",
        "*  = 0"
    ]
    with open('results/intermediate/managed_grass_reclass.txt', 'w') as f:
        f.writelines([s + '\n' for s in managed_grass_rules])


def write_urban_reclass_rules():
    urban_rules = ["190 = 750", "*   = 0"]
    with open('results/intermediate/urban_reclass.txt', 'w') as f:
        f.writelines([s + '\n' for s in urban_rules])


def write_bare_soil_reclass_rules():
    bare_soil_rules = [
        "62  = 100",
        "72  = 300",
        "82  = 300",
        "90  = 100",
        "120 = 200",
        "121 = 200",
        "122 = 200",
        "130 = 400",
        "140 = 400",
        "150 = 850",
        "151 = 850",
        "152 = 850",
        "153 = 850",
        "200 = 1000",
        "201 = 1000",
        "202 = 1000",
        "*   = 0"
    ]
    with open('results/intermediate/bare_soil_reclass.txt', 'w') as f:
        f.writelines([s + '\n' for s in bare_soil_rules])


def write_water_reclass_rules():
    water_rules = [
        "160 = 200",
        "170 = 200",
        "180 = 300",
        "190 = 50",
        "210 = 1000",
        "*   = 0"
    ]
    with open('results/intermediate/water_reclass.txt', 'w') as f:
        f.writelines([s + '\n' for s in water_rules])


def write_snow_ice_reclass_rules():
    snow_ice_rules = ["220 = 1000", "*   = 0"]
    with open('results/intermediate/snow_ice_reclass.txt', 'w') as f:
        f.writelines([s + '\n' for s in snow_ice_rules])


def write_nodata_reclass_rules():
    nodata_rules = ["0 = 1000", "* = 0"]
    with open('results/intermediate/nodata_reclass.txt', 'w') as f:
        f.writelines([s + '\n' for s in nodata_rules])


