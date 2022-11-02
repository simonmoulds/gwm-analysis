#!/bin/bash

# ========================================================= #
# Write Help
# ========================================================= #

Help()
{
    echo "Program to create JULES land cover fraction maps in netCDF format."
    echo
    echo "Syntax: create-app.sh [-h|o]"
    echo "options:"
    echo "-h | --help          Print this help message."
    echo "-o | --overwrite     Overwrite existing database (WARNING: could take a long time)."
    echo "--nine-pft           Write 9 PFT fraction maps."
    echo "--five-pft           Write 5 PFT fraction maps."
    echo "--region             Name for model domain, e.g. 'globe'."
    echo "--file               Name of geocoded raster file to use to specify region."
    echo "--use-file-land-frac Use file indicated by `--file` to define land frac"
    echo "-d | --destdir    Output directory."
    echo
}

# ========================================================= #
# Get options
# ========================================================= #

# Based on advice from:
# https://www.codebyamir.com/blog/parse-command-line-arguments-using-getopt
# https://stackoverflow.com/a/7948533 

# Adapted from:
# https://stackoverflow.com/a/14203146 ***

# Set defaults
ONE_D=0
NINEPFT=0
FIVEPFT=0
FILE_LAND_FRAC=0
OUTDIR=.
POSITIONAL=()
while [[ $# -gt 0 ]]
do
    key="$1"
    case $key in
	-h|--help)
	    Help
	    exit
	    ;;
	-o|--overwrite)
	    OVERWRITE='--overwrite'
	    shift
	    ;;
	--nine-pft)
	    NINEPFT=1
	    shift
	    ;;
	--five-pft)
	    FIVEPFT=1
	    shift
	    ;;
	--region)
	    REGION="$2"
	    shift
	    shift
	    ;;	
	--file)
	    FILE="$2"
	    shift
	    shift
	    ;;
	--use-file-land-frac)
	    FILE_LAND_FRAC=1
	    shift
	    ;;
	-d|--destdir)
	    OUTDIR="$2"
	    shift
	    shift
	    ;;
	*)  # unknown option
	    POSITIONAL+=("$1") # save it in an array for later
	    shift # past argument
	    ;;
    esac
done

# If a filename is supplied as the region, check the file exists
if [[ -f $FILE && $FILE != '' ]]
then    
    USEFILE=1
elif [[ ! -f $FILE && $FILE != '' ]]
then    
    echo "Filename supplied, but does not exist"
    echo "Exiting..."
    exit
else
    USEFILE=0
fi

if [[ $YRES == '' ]]
then
    YRES=$XRES
fi

# Export the variables required to set GRASS region
export WD=$(pwd)
export REGION=$REGION
export FILE=$FILE
export FILE_LAND_FRAC=$FILE_LAND_FRAC
export OUTDIR=$OUTDIR
export OVERWRITE=$OVERWRITE
if [ ! -d $OUTDIR ]
then
    mkdir $OUTDIR
fi
if [ ! -d $OUTDIR/geotiff ]
then
    mkdir $OUTDIR/geotiff
fi
if [ ! -d $OUTDIR/netcdf ]
then
    mkdir $OUTDIR/netcdf
fi

# ##################################### #
# Create GRASS region
# ##################################### #

# Check whether supplied region is the name of a protected region:
# TODO: could add a string to make it less likely that a similar name would be used
# e.g. globe_0.250000Deg_PROTECTED
echo "globe_0.500000Deg
globe_0.250000Deg
globe_0.125000Deg
globe_0.100000Deg
globe_0.083333Deg
globe_0.062500Deg
globe_0.050000Deg
globe_0.016667Deg
globe_0.010000Deg
globe_0.008333Deg
globe_0.004167Deg
globe_0.002083Deg
globe_0.000833Deg" > /tmp/current_regions.txt

if [ `grep -x $REGION /tmp/current_regions.txt` ]
then
    echo "The supplied region name is the name of a protected region: "
    echo "Please choose another region name"
    exit
fi

SRCDIR=$(pwd)

# Define GRASS mapset in which to run
MAPSET=$HOME/grassdata/latlong/gwm

# Run GRASS script to create user-specified region
chmod u+x $SRCDIR/bash/grass_define_custom_region.sh
export GRASS_BATCH_JOB=$SRCDIR/bash/grass_define_custom_region.sh
grass76 $MAPSET
unset GRASS_BATCH_JOB

# # The above script prints the region; here we check that
# # the user is happy to go ahead
# while true;
# do
#     read -p "The specified region is summarised above. Do you wish to proceed? " yn
#     case $yn in
# 	[Yy]* ) break;;
# 	[Nn]* ) exit;;
# 	* ) echo "Please answer yes or no.";;
#     esac
# done

# Export options which define which ancillary maps to write
export NINEPFT=$NINEPFT
export FIVEPFT=$FIVEPFT

# Deactivate Anaconda environment
CONDA_BASE=$(conda info --base)
source $CONDA_BASE/etc/profile.d/conda.sh
conda deactivate

# Export maps from GRASS database to geotiff image files
export AUXDIR=$DATADIR/aux
export FRAC_BASENM=$DATADIR/aux/frac/lc
export FRAC_VARNM=esa_cci_lc_frac
chmod u+x $SRCDIR/bash/grass_write_jules_ancil_maps.sh
export GRASS_BATCH_JOB=$SRCDIR/bash/grass_write_jules_ancil_maps.sh
grass76 $MAPSET
unset GRASS_BATCH_JOB

set -a
. $OUTDIR/geotiff/filenames.txt
set +a

# Convert geotiffs to netCDFs with Python script
conda activate jules-data

# First, we create a set of files which manipulates the land
# cover fractions so that when we run ants the correct
# proportions in relation to the LAI data are used.

# In each case the LAI file has 5 (or 9) PFTs, corresponding
# to the standard JULES PFTs. However, because the LAI dataset
# has already been separated into natural, irrigated (1/2/3),
# rainfed and fallow components we need to create the
# corresponding land cover files. This produces the following
# set of input files:
# * jules_frac_combined_5pft_ants_YYYY_SUFFIX.nc
#   -> PFTs combine natural/managed land
# * jules_frac_natural_5pft_ants_YYYY_SUFFIX.nc
#   -> PFTs represent only natural land
# * jules_frac_rainfed_5pft_ants_YYYY_SUFFIX.nc
#   -> PFTs represent only rainfed components [C3/C4 only]
# * jules_frac_irrigated_5pft_ants_YYYY_SUFFIX.nc
#   -> PFTs represent only irrigated land [C3/C4 only]
# In each file the relative fractions are adjusted so that
# they sum to one. 
python3 python/make-ants-frac-input.py -d $OUTDIR/netcdf

# Next we create a land cover fraction file which includes
# the additional crop classes (rainfed/irrigated-1/
# irrigated-2/irrigated-3/fallow as separate PFTs.
# * jules_frac_5pft_ants_YYYY_SUFFIX.nc
python3 python/make-frac-input.py -d $OUTDIR/netcdf

conda deactivate

