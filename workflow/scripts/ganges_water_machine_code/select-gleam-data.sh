#!/bin/bash

DATADIR=/mnt/scratch/scratch/data/GLEAM/data
OUTDIR=$HOME/projects/ganges-water-machine/data/gleam

XMIN=60.0
XMAX=100.0
YMIN=20.0
YMAX=40.0

echo "gridtype=lonlat
xsize=80
ysize=40
xfirst=60.25
xinc=0.5
yfirst=39.75
yinc=-0.5" > /tmp/grid_igp_0.500000Deg.txt

declare -a PRODUCTS=(v3.5a v3.5b)
declare -a TIMES=(daily monthly yearly)
declare -a VARIABLES=(E Eb Ei Ep Es Et Ew S SMroot SMsurf)

if [[ ! -d $OUTDIR ]]
then
    mkdir $OUTDIR
fi

for PRODUCT in "${PRODUCTS[@]}"
do
    if [ $PRODUCT == v3.5a ]
    then
	YEAR0=1980
    elif [ $PRODUCT == v3.5b ]
    then
	YEAR0=2003
    fi
    YEAR1=2020
    
    # Make equivalent directory in output location
    if [[ ! -d ${OUTDIR}/${PRODUCT} ]]
    then
	mkdir ${OUTDIR}/${PRODUCT}
    fi    
    for TIME in "${TIMES[@]}"
    do
	if [[ ! -d ${OUTDIR}/${PRODUCT}/${TIME} ]]
	then
	    mkdir ${OUTDIR}/${PRODUCT}/${TIME}
	fi
	for VAR in "${VARIABLES[@]}"
	do
	    if [ $TIME == daily ]
	    then
	    	for ((YEAR=YEAR0; YEAR<=YEAR1; YEAR++))
	    	do
	    	    if [[ ! -d ${OUTDIR}/${PRODUCT}/${TIME}/${YEAR} ]]
	    	    then
	    		mkdir ${OUTDIR}/${PRODUCT}/${TIME}/${YEAR}
	    	    fi		    
	    	    # First simply crop to study region, preserving the
	    	    # original resolution
	    	    ifile=${DATADIR}/${PRODUCT}/${TIME}/${YEAR}/${VAR}_${YEAR}_GLEAM_${PRODUCT}.nc
	    	    ofile=${OUTDIR}/${PRODUCT}/${TIME}/${YEAR}/${VAR}_${YEAR}_GLEAM_${PRODUCT}_IGP.nc
	    	    ncks -O -d lon,${XMIN},${XMAX} -d lat,${YMIN},${YMAX} ${ifile} ${ofile}
	    	    # Next, resample to 0.5 degree resolution, to match
	    	    # JULES output
	    	    # For info on correct remapping algorithm, see
	    	    # https://stackoverflow.com/a/55748114)
	    	    ofile=${OUTDIR}/${PRODUCT}/${TIME}/${YEAR}/${VAR}_${YEAR}_GLEAM_${PRODUCT}_IGP_halfdeg.nc
	    	    cdo remapcon,/tmp/grid_igp_0.500000Deg.txt $ifile $ofile
		    
	    	done
	    fi
	    
	    if [ $TIME == monthly ]
	    then
		if [[ ! -d ${OUTDIR}/${PRODUCT}/${TIME}/ ]]
		then
		    mkdir ${OUTDIR}/${PRODUCT}/${TIME}/
		fi
	    	ifile=${DATADIR}/${PRODUCT}/${TIME}/${VAR}_${YEAR0}-${YEAR1}_GLEAM_${PRODUCT}_MO.nc
	    	ofile=${OUTDIR}/${PRODUCT}/${TIME}/${VAR}_${YEAR0}-${YEAR1}_GLEAM_${PRODUCT}_MO_IGP.nc
	    	ncks -O -d lon,${XMIN},${XMAX} -d lat,${YMIN},${YMAX} ${ifile} ${ofile}
		
		ifile=$ofile		
	    	ofile=${OUTDIR}/${PRODUCT}/${TIME}/${YEAR}/${VAR}_${YEAR0}-${YEAR1}_GLEAM_${PRODUCT}_MO_IGP_halfdeg.nc
	    	cdo remapcon,/tmp/grid_igp_0.500000Deg.txt $ifile $ofile
	    fi
	    
	    if [ $TIME == yearly ]
	    then
		if [[ ! -d ${OUTDIR}/${PRODUCT}/${TIME}/ ]]
		then
		    mkdir ${OUTDIR}/${PRODUCT}/${TIME}/
		fi		    
	    	ifile=${DATADIR}/${PRODUCT}/${TIME}/${VAR}_${YEAR0}-${YEAR1}_GLEAM_${PRODUCT}_YR.nc
	    	ofile=${OUTDIR}/${PRODUCT}/${TIME}/${VAR}_${YEAR0}-${YEAR1}_GLEAM_${PRODUCT}_YR_IGP.nc
	    	ncks -O -d lon,${XMIN},${XMAX} -d lat,${YMIN},${YMAX} ${ifile} ${ofile}
		ifile=$ofile
	    	ofile=${OUTDIR}/${PRODUCT}/${TIME}/${YEAR}/${VAR}_${YEAR0}-${YEAR1}_GLEAM_${PRODUCT}_YR_IGP_halfdeg.nc
	    	cdo remapcon,/tmp/grid_igp_0.500000Deg.txt $ifile $ofile
	    fi
	done	
    done    
done

