#!/usr/bin/env python3

esa_land_covers = [
    'tree_broadleaf_evergreen', 'tree_broadleaf_deciduous',
    'tree_needleleaf_evergreen', 'tree_needleleaf_deciduous',
    'shrub_broadleaf_evergreen', 'shrub_broadleaf_deciduous',
    'shrub_needleleaf_evergreen', 'shrub_neefleleaf_deciduous',
    'natural_grass', 'managed_grass', 'urban', 'bare_soil',
    'water', 'snow_ice', 'no_data'
]

jules_land_covers = [
    'tree_broadleaf',
    'tree_needleleaf',
    'shrub',
    'c3_grass',
    'c4_grass',
    'tree_broadleaf_evergreen_tropical',
    'tree_broadleaf_evergreen_temperate',
    'tree_broadleaf_deciduous',
    'tree_needleleaf_evergreen',
    'tree_needleleaf_deciduous',
    'shrub_evergreen',
    'shrub_deciduous'
    'urban',
    'water',
    'bare_soil',
    'snow_ice'
]

jules_land_cover_components = {
    'tree_broadleaf': [
        'tree_broadleaf_evergreen',
        'tree_broadleaf_deciduous'
    ],
    'tree_needleleaf': [
        'tree_needleleaf_evergreen',
        'tree_needleleaf_deciduous'
    ],
    'shrub': [
        'shrub_broadleaf_evergreen',
        'shrub_broadleaf_deciduous',
        'shrub_needleleaf_evergreen',
        'shrub_needleleaf_deciduous'
    ],
    'tree_broadleaf_evergreen_tropical': [
        'tree_broadleaf_evergreen'
    ],
    'tree_broadleaf_evergreen_temperate': [
        'tree_broadleaf_evergreen'
    ],
    'tree_broadleaf_deciduous': [
        'tree_broadleaf_deciduous'
    ],
    'tree_needleleaf_evergreen': [
        'tree_needleleaf_evergreen'
    ],
    'tree_needleleaf_deciduous': [
        'tree_needleleaf_deciduous'
    ],
    'shrub_evergreen': [
        'shrub_evergreen'
    ],
    'shrub_deciduous': [
        'shrub_deciduous'
    ],
    'urban': ['urban'],
    'water': ['water'],
    'bare_soil': ['bare_soil'],
    'snow_ice': ['snow_ice']
}
