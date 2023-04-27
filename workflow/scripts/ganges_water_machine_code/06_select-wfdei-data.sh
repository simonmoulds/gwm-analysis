#!/bin/bash

DATADIR=/mnt/scratch/scratch/data/WFDEI/WFDEI_3h
OUTDIR=$HOME/projects/ganges-water-machine/data/wfdei/WFDEI_3h_IGP

YEAR0=1979
YEAR1=2016

XMIN=60.0
XMAX=100.0
YMIN=20.0
YMAX=40.0

declare -a VARS=(LWdown_WFDEI Rainf_WFDEI_CRU Rainf_WFDEI_GPCC SWdown_WFDEI PSurf_WFDEI Wind_WFDEI Snowf_WFDEI_CRU Snowf_WFDEI_GPCC Qair_WFDEI Tair_WFDEI)

if [[ ! -d $OUTDIR ]]
then
    mkdir $OUTDIR
fi

if [[ ! -d $OUTDIR/WFDEI_3h_IGP ]]
then
    mkdir $OUTDIR/WFDEI_3h_IGP
fi

for VAR in "${VARS[@]}"
do    
    # Make equivalent directory in output location
    if [[ ! -d ${OUTDIR}/WFDEI_3h_IGP/${VAR} ]]
    then
	mkdir ${OUTDIR}/WFDEI_3h_IGP/${VAR}
    fi    
    # Loop through time
    for ((YEAR=YEAR0; YEAR<=YEAR1; YEAR++))
    do
	echo $VAR $YEAR
	for MONTH in {01..12}
	do	    
	    ifile=${DATADIR}/${VAR}/${VAR}_${YEAR}${MONTH}.nc
	    ofile=${OUTDIR}/WFDEI_3h_IGP/${VAR}/${VAR}_${YEAR}${MONTH}_IGP.nc
	    ncks -O -d lon,${XMIN},${XMAX} -d lat,${YMIN},${YMAX} ${ifile} ${ofile}
	done	
    done
done

