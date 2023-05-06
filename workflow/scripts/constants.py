#!/usr/bin/env python3

esa_land_covers = [
    'tree_broadleaf_evergreen', 'tree_broadleaf_deciduous',
    'tree_needleleaf_evergreen', 'tree_needleleaf_deciduous',
    'shrub_broadleaf_evergreen', 'shrub_broadleaf_deciduous',
    'shrub_needleleaf_evergreen', 'shrub_needleleaf_deciduous',
    'natural_grass', 'managed_grass', 'urban', 'bare_soil',
    'water', 'snow_ice', 'nodata'
]

jules_5pft = [
    'tree_broadleaf',
    'tree_needleleaf',
    'shrub',
    'c3_grass',
    'c4_grass'
]

jules_9pft = [
    'tree_broadleaf_evergreen_tropical',
    'tree_broadleaf_evergreen_temperate',
    'tree_broadleaf_deciduous',
    'tree_needleleaf_evergreen',
    'tree_needleleaf_deciduous',
    'c3_grass',
    'c4_grass',
    'shrub_evergreen',
    'shrub_deciduous'
]
jules_common_pft = list(set(jules_5pft).intersection(jules_9pft))
jules_9pft_only = [lc for lc in jules_9pft if lc not in jules_common_pft]
jules_5pft_only = [lc for lc in jules_5pft if lc not in jules_common_pft]

jules_land_covers = (
    jules_5pft_only +
    jules_9pft_only +
    jules_common_pft +
    ['urban', 'water', 'bare_soil', 'snow_ice']
)

jules_land_uses = ['combined', 'natural', 'rainfed', 'irrigated']

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
        'shrub_broadleaf_evergreen',
        'shrub_needleleaf_evergreen'
    ],
    'shrub_deciduous': [
        'shrub_broadleaf_deciduous',
        'shrub_needleleaf_deciduous'
    ],
    'urban': ['urban'],
    'water': ['water'],
    'bare_soil': ['bare_soil'],
    'snow_ice': ['snow_ice']
}
