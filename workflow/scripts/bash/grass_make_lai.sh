#!/bin/bash

# This script process ESA LAI data, writing lai maps to $AUXDIR

r.mask -r
r.external.out -r

r.external.out \
    directory=${AUXDIR}/lai \
    format="GTiff" \
    options="COMPRESS=DEFLATE"    

for YEAR in {2014..2019}
do    
    g.region region=igp_0.002778Deg
    g.region -p
    eval `g.region -g`	
    
    # Loop through LAI maps (three images per month, 2014--2019)
    for MONTH in {01..12}
    do
	NDAY=`cal $MONTH $YEAR | awk 'NF {DAYS = $NF}; END {print DAYS}'`
	for DAY in 10 20 $NDAY
	do	    
	    echo "Processing ESA LAI map for ${DAY}/${MONTH}/${YEAR}"
	    g.region region=igp_0.002778Deg
	    eval `g.region -g`
	    
	    # import LAI data
	    VN=$(find "${LAIDIR}"/"${YEAR}"/"${MONTH}"/"${DAY}" -mindepth 1 -maxdepth 1 -regextype posix-extended -regex '.*/LAI300_[0-9]{4}[0-9]{2}[0-9]{2}0000_GLOBE_PROBAV_V[0-9].[0-9].[0-9]$' | rev | cut -d'/' -f-1 | rev | sed 's/\(.*\)_V\([0-9].[0-9].[0-9]\)/\2/g')
	    
	    BASENAME=c_gls_LAI300_${YEAR}${MONTH}${DAY}0000_GLOBE_PROBAV_V${VN}
	    TIF_FN=$AUXDIR/lai/${BASENAME}_India.tif
	    NC_FN="${LAIDIR}"/"${YEAR}"/"${MONTH}"/"${DAY}"/LAI300_${YEAR}${MONTH}${DAY}0000_GLOBE_PROBAV_V${VN}/"${BASENAME}".nc

	    # Resample to current region using bilinear interpolation
	    gdalwarp \
	    	-te $w $s $e $n \
	    	-tr $ewres $nsres \
	    	-r bilinear \
	    	-co "COMPRESS=lzw" \
	    	-overwrite \
	    	NETCDF:"${NC_FN}":LAI \
	    	"${TIF_FN}"

	    # Import LAI data to grass
	    r.external -a input="${TIF_FN}" output=lai_unscaled --overwrite
	    
	    # Apply scale factor (=0.033333)
	    r.mapcalc \
	    	"lai_${YEAR}_${MONTH}_${DAY}_igp_0.002778Deg.tif = lai_unscaled * 0.033333" \
	    	--overwrite	    
	    g.remove -f type=raster name=lai_unscaled
	done
    done
done

r.external.out -r    
