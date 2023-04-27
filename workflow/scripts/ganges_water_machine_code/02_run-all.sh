#!/bin/bash

# **************************
# **************************

# This script requires that
# ./populate-grass-db.sh has
# already been run

# **************************
# **************************

export DATADIR=$(pwd)/../data

# ######################################################### #
# Process land fraction data
# ######################################################### #

# Land fraction is currently based on the WFDEI grid
# It wouldn't be too much work to change this

echo ""
echo ""
echo "Creating land fraction data"
echo ""
echo ""
Rscript rscript/create_land_frac.R

# ######################################################### #
# Create land cover fraction data
# ######################################################### #

# Two parts:
# (i)  create inputs for ANTS LAI algorithm
# (ii) create inputs for JULES itself

echo ""
echo ""
echo "Creating land cover fraction input files"
echo ""
echo ""
bash bash/create-app.sh --five-pft --region igp --file $DATADIR/WFD-EI-LandFraction2d_IGP.tif --use-file-land-frac --overwrite -d $DATADIR

# TODO: post-process the resulting land fraction file to match irrigated areas from ICRISAT analysis [do this in R?]

# # ######################################################### #
# # Create average LAI maps
# # ######################################################### #

# # This script averages LAI maps for each time point in a
# # given year, resulting in an average LAI climatology

# echo ""
# echo ""
# echo "Creating average LAI data"
# echo ""
# echo ""
# Rscript rscript/process_lai.R

# # ######################################################### #
# # Format the LAI maps
# # ######################################################### #

# # Convert LAI maps created in the previous step to netCDF
# # format, compatible with ANTS

# echo ""
# echo ""
# echo "Writing LAI input files to supply to ANTS"
# echo ""
# echo ""
# python3 python/write_ants_lai.py

# # ######################################################### #
# # Run ANTS to create balanced LAI maps
# # ######################################################### #

# echo ""
# echo ""
# echo "Running ANTS to create balanced LAI and canopy height data"
# echo ""
# echo ""
# bash bash/run-ants.sh

# # ######################################################### #
# # Format ANTS output to create climatologies
# # ######################################################### #

# echo ""
# echo ""
# echo "Creating LAI and canopy height climatology data"
# echo ""
# echo ""

# export LAI_NCFILE=$DATADIR/netcdf/jules_5pft_w_crops_prescribed_lai_igp.nc
# export CANOPY_HEIGHT_NCFILE=$DATADIR/netcdf/jules_5pft_w_crops_prescribed_canopy_height_igp.nc
# export DATADIR=$DATADIR
# python3 python/write_jules_lai.py

# ######################################################### #
# Copy the principle output files to the root directory
# ######################################################### #

if [ ! -d $DATADIR/wfdei/ancils ]
then
    mkdir $DATADIR/wfdei/ancils
fi

# # 1-LAI
# cp $LAI_NCFILE $DATADIR/wfdei/ancils
#
# # 2-Canopy height
# cp $CANOPY_HEIGHT_NCFILE $DATADIR/wfdei/ancils

# 3-Land cover
cp $DATADIR/netcdf/jules_frac_5pft_ants_2015_CUSTOM_igp.nc $DATADIR/wfdei/ancils
