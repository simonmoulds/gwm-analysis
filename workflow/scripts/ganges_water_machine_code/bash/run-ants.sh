#!/bin/bash

# This script runs the ANTS LAI algorithm for the various vegetation types

# Activate the ants anaconda environment
CONDA_BASE=$(conda info --base)
source $CONDA_BASE/etc/profile.d/conda.sh
conda activate ants

# DATADIR=../data
CONTRIB_DIR=$HOME/packages/ants/contrib/trunk/
LAI_WEIGHTS=$DATADIR/lai_weights_gl9.json
VEG_FRAC_MASTER=$(pwd)/netcdf/jules_frac_natural_5pft_ants_2015_CUSTOM_igp.nc
CANOPY_HEIGHT_FACTORS=$DATADIR/canopy_height_factors_gl9.json
TREES_DATASET=$DATADIR/Simard_Pinto_3DGlobalVeg_JGR.nc

# Update LD_LIBRARY_PATH
LD_LIBRARY_PATH=$HOME/packages/shumlib/build/vm-x86-gfortran-gcc/lib:$LD_LIBRARY_PATH

# declare -a VEG_TYPES=(combined natural rainfed irrigated irrigated irrigated)
# declare -a LAI_TYPES=(combined natural rainfed_cropland irrigated_cropland_1 irrigated_cropland_2 irrigated_cropland_3)
declare -a VEG_TYPES=(combined
		      natural
		      rainfed
		      irrigated
		      irrigated
		      irrigated
		      irrigated
		      fallow
		      rainfed_no_c4_crops
		      irrigated_no_c4_crops
		      irrigated_no_c4_crops
		      irrigated_no_c4_crops
		      irrigated_no_c4_crops
		      fallow_no_c4_crops)
declare -a LAI_TYPES=(combined
		      natural
		      rainfed_cropland
		      irrigated_cropland_1
		      irrigated_cropland_2
		      irrigated_cropland_3
		      irrigated_cropland_c
		      fallow_cropland
		      rainfed_cropland
		      irrigated_cropland_1
		      irrigated_cropland_2
		      irrigated_cropland_3
		      irrigated_cropland_c
		      fallow_cropland)
declare -a LAI_TARGET_NAMES=(combined
			     natural
			     rainfed_cropland
			     irrigated_cropland_1
			     irrigated_cropland_2
			     irrigated_cropland_3
			     irrigated_cropland_c
			     fallow_cropland
			     rainfed_cropland_no_c4_crops
			     irrigated_cropland_1_no_c4_crops
			     irrigated_cropland_2_no_c4_crops
			     irrigated_cropland_3_no_c4_crops
			     irrigated_cropland_c_no_c4_crops
			     fallow_cropland_no_c4_crops)
for i in "${!VEG_TYPES[@]}"
do
    VEG_FRAC_MASTER=$DATADIR/netcdf/jules_frac_${VEG_TYPES[$i]}_5pft_ants_2015_CUSTOM_igp.nc
    LAI_MASTER=$DATADIR/aux/lai/lai_${LAI_TYPES[$i]}_igp.nc
    LAI_TARGET=$DATADIR/netcdf/lai_${LAI_TARGET_NAMES[$i]}_igp_0.500000Deg
    CANOPY_HEIGHT_TARGET=$DATADIR/netcdf/canopy_height_${LAI_TARGET_NAMES[$i]}_igp_0.500000Deg
    # Leaf area index
    python3 $CONTRIB_DIR/Lai/ancil_lai.py --output $LAI_TARGET $LAI_MASTER $VEG_FRAC_MASTER --relative-weights $LAI_WEIGHTS
    # Canopy cover
    python3 $CONTRIB_DIR/CanopyHeights/ancil_canopy_heights.py $LAI_TARGET.nc --canopy-height-factors $CANOPY_HEIGHT_FACTORS --trees-dataset $TREES_DATASET --output $CANOPY_HEIGHT_TARGET
done
