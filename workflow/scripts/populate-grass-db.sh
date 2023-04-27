#!/bin/bash

# ========================================================= #
# Write Help
# ========================================================= #

Help()
{
    # Display help
    echo "Main script to create JULES input data."
    echo
    echo "Syntax: populate-grass-db.sh [-h|o]"
    echo "options:"
    echo "-h | --help       Print this help message."
    echo "-o | --overwrite  Overwrite existing database (WARNING: could take a long time)."
    echo
}

# ========================================================= #
# Get options
# ========================================================= #

# Based on advice from:
# https://www.codebyamir.com/blog/parse-command-line-arguments-using-getopt
# https://stackoverflow.com/a/7948533 

# Adapted from:
# https://stackoverflow.com/a/14203146

POSITIONAL=()
while [[ $# -gt 0 ]]
do
    key="$1"
    case $key in
	-h|--help)
	    Help
	    shift
	    ;;
	-o|--overwrite)
	    OVERWRITE='--overwrite'
	    shift
	    ;;
	*)  # unknown option
	    POSITIONAL+=("$1") # save it in an array for later
	    shift # past argument
	    ;;
    esac
done

# ========================================================= #
# Declare global variables
# ========================================================= #

export WD=$(pwd)
export SRCDIR=$(pwd)
export DATADIR=$(pwd)/../data/
export OUTDIR=${DATADIR}
export AUXDIR=$(pwd)/../data/aux
export LAIDIR=/var/data/scratch/data/LAI/LAI_300m_V1
export FRACDIR=$(pwd)/../data/aux/frac
export ESACCIDIR=/var/data/scratch/data/ESA_CCI_LC
export CAMADIR=/var/data/scratch/data/MERIT/ToECMWF
export OVERWRITE=$OVERWRITE

# ========================================================= #
# Make output directories
# ========================================================= #

if [ ! -d $AUXDIR ]
then
    mkdir $AUXDIR
fi

if [ ! -d $AUXDIR/lai ]
then
    mkdir $AUXDIR/lai
fi

if [ ! -d $AUXDIR/frac ]
then
    mkdir $AUXDIR/frac
fi

if [ ! -d $AUXDIR/land_frac ]
then
    mkdir $AUXDIR/land_frac
fi

# ========================================================= #
# Set-up GRASS
# ========================================================= #

MAPSET=$HOME/grassdata/latlong/gwm
if [ ! -d $MAPSET ]
then    
   grass -e -c $MAPSET
fi

# Deactivate Anaconda environment (this can mess up GRASS)
# See - https://github.com/conda/conda/issues/7980#issuecomment-441358406
# https://stackoverflow.com/a/45817972
conda --version > /dev/null 2>&1
if [ $? == 0 ]
then
    ANACONDA_INSTALLED=1
else
    ANACONDA_INSTALLED=0
fi

if [ $ANACONDA_INSTALLED == 1 ]
then    
    CONDA_BASE=$(conda info --base)
    source $CONDA_BASE/etc/profile.d/conda.sh
    conda deactivate
    # conda activate base
fi

# ========================================================= #
# Run GRASS scripts to populate GRASS DB and write aux maps
# ==========================================================#

# Define some global regions which are used during data processing
chmod u+x $SRCDIR/bash/grass_define_regions.sh
export GRASS_BATCH_JOB=$SRCDIR/bash/grass_define_regions.sh
grass $MAPSET
unset GRASS_BATCH_JOB

# chmod u+x $SRCDIR/bash/grass_make_lai.sh
# export GRASS_BATCH_JOB=$SRCDIR/bash/grass_make_lai.sh
# grass76 $MAPSET
# unset GRASS_BATCH_JOB

# TODO: currently untested
chmod u+x $SRCDIR/bash/grass_make_jules_land_frac.sh
export GRASS_BATCH_JOB=$SRCDIR/bash/grass_make_jules_land_frac.sh
grass $MAPSET
unset GRASS_BATCH_JOB

# The main outputs of this script are land fraction maps for
# 5PFT/9PFT configurations [we currently use 5PFT]. For each PFT
# we provide a map for rainfed/irrigated/natural components. This
# is only really used for C3/C4 grass, which include crops and
# natural vegetation. Nevertheless it makes the LAI processing
# more straightforward. 
# TODO: check that mask is working as intended
chmod u+x $SRCDIR/bash/grass_make_jules_frac.sh
export GRASS_BATCH_JOB=$SRCDIR/bash/grass_make_jules_frac.sh
grass $MAPSET
unset GRASS_BATCH_JOB

# The main outputs of this script are the following maps at
# 0.002778 degree resolution:
# * permanent_cropland [2014-2018]
# * permanent_rainfed_cropland [2014-2018]
# * permanent_irrigated_cropland [2014-2018]
# * permanent_natural_vegetation [2014-2018]
# These are used to extract LAI pixels belonging to the
# respective cropland classes in order to derive the phenology
chmod u+x $SRCDIR/bash/grass_make_permanent_crop_maps.sh
export GRASS_BATCH_JOB=$SRCDIR/bash/grass_make_permanent_crop_maps.sh
grass $MAPSET
unset GRASS_BATCH_JOB

# The main outputs of this script are the following maps at
# 0.002777 degree resolution, also aggregated to 0.008333 degrees
# * irrigated_continuous_${YEAR}
# * irrigated_single_season_${YEAR}
# * irrigated_double_season_${YEAR}
# * irrigated_triple_season_${YEAR}
# * rainfed_${YEAR}
# * fallow_${YEAR}
chmod u+x $SRCDIR/bash/grass_detect_growing_seasons.sh
export GRASS_BATCH_JOB=$SRCDIR/bash/grass_detect_growing_seasons.sh
grass $MAPSET
unset GRASS_BATCH_JOB

# # This script produces lai maps for the above cropland types, as
# # well as natural and combined (natural + cropland)
# chmod u+x $SRCDIR/bash/grass_make_jules_lai.sh
# export GRASS_BATCH_JOB=$SRCDIR/bash/grass_make_jules_lai.sh
# grass76 $MAPSET
# unset GRASS_BATCH_JOB

# Make India land frac
chmod u+x $SRCDIR/bash/grass_make_india_land_frac.sh
export GRASS_BATCH_JOB=$SRCDIR/bash/grass_make_india_land_frac.sh
grass $MAPSET
unset GRASS_BATCH_JOB

# # Process ICRISAT maps (no - do this in R)
# chmod u+x $SRCDIR/bash/grass_make_irrigated_area_maps.sh
# export GRASS_BATCH_JOB=$SRCDIR/bash/grass_make_irrigated_area_maps.sh
# grass76 $MAPSET
# unset GRASS_BATCH_JOB
