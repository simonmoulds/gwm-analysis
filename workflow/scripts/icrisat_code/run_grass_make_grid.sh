#!/bin/bash

export WD=$HOME/projects/icrisat
export DATADIR=$WD/data-raw
export OUTDIR=$WD/data

# Make directory for intermediate outputs, used for testing
if [ ! -d $OUTDIR/testing ]
then
    mkdir $OUTDIR/testing
fi

# Make directory for district fraction maps
if [ ! -d $OUTDIR/dist_fraction ]
then
    mkdir $OUTDIR/dist_fraction
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

# ##################################### #
# Process ICRISAT data (R)
# ##################################### #
   
Rscript $WD/code/process-icrisat-data.R

# ##################################### #
# Create locations/mapset
# ##################################### #

# if [ ! -d $HOME/grassdata/latlong ]
# then
#     grass -c -e "${DATADIR}"/india-village-census-2001-BR_ll.gpkg $HOME/grassdata/latlong
# fi

# if [ ! -d $HOME/grassdata/utm44n ]
# then
#     grass -d -e EPSG:32644 $HOME/grassdata/utm44n
# fi

if [ ! -d $HOME/grassdata/latlong/icrisat ]
then    
   grass -e -c $HOME/grassdata/latlong/icrisat
fi

# ##################################### #
# run jobs
# ##################################### #

JOB=$WD/code/grass_make_grid.sh
chmod u+x $JOB
export GRASS_BATCH_JOB=$JOB
grass $HOME/grassdata/latlong/icrisat
unset GRASS_BATCH_JOB

# JOB=$WD/code/grass_process_village_census.sh
# chmod u+x $JOB
# export GRASS_BATCH_JOB=$JOB
# grass $HOME/grassdata/latlong/icrisat
# unset GRASS_BATCH_JOB

# JOB=$WD/code/grass_reproject_maps.sh
# chmod u+x $jobfile
# export GRASS_BATCH_JOB=$JOB
# grass $HOME/grassdata/latlong/icrisat
# unset GRASS_BATCH_JOB
