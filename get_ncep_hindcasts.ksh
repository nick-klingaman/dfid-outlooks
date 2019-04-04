#!/bin/ksh

start_year=1982
stop_year=2011

# Month to download
month=03
# Dates to download in this month
set -A dates '07' 
set -A hours '00' '06' '12' '18'

nmonths=10
get=1

set -A out_vars 'precip' 't2m' 'soilmc'
set -A nc_vars 'prate' 't_1' 'soilw'
set -A hyperslab ' ' ' ' '-d depth,0,0'

basedir=/gws/nopw/j04/klingaman/datasets/S2S/NCEP/ENSO

for this_date in ${dates[@]} ; do
    outdir=${basedir}/hindcasts_${month}${this_date}
    mkdir -p ${basedir}/hindcasts_${month}${this_date}
    this_year=${start_year}
    
    if [ ${get} -eq 1 ] ; then
	/bin/rm ${basedir}/wget.list
	touch ${basedir}/wget.list
	while [ ${this_year} -le ${stop_year} ] ; do
	    for this_hour in ${hours[@]} ; do
		url=http://nomads.ncdc.noaa.gov/modeldata/cmd_mm_9mon/${this_year}/${this_year}${month}/${this_year}${month}${this_date}/flxf${this_year}${month}${this_date}${this_hour}.01.
		j=0		
		while [ ${j} -le ${nmonths} ] ; do
		    fcst_month=$((month+j))
		    if [ ${fcst_month} -gt 12 ] ; then
			fcst_year=$((this_year+1))
			fcst_month=$((fcst_month-12))
		    else
			fcst_year=$((this_year))
		    fi
		    if [ ${fcst_month} -lt 10 ] ; then
			fcst_month=0${fcst_month} ; fi
		    this_url=${url}${fcst_year}${fcst_month}.avrg.grb2		
		    if [ ! -e ${outdir}/flxf${this_year}${month}${this_date}${this_hour}.01.${fcst_year}${fcst_month}.avrg.nc ] ; then
		       echo ${this_url} >> ${basedir}/wget.list
		    fi
		    j=$((j+1))
		done
	    done
	    this_year=$((this_year+1))
	done
	wget -i ${basedir}/wget.list -P ${outdir}
    fi
    
    # Convert files
    for this_file in `ls ${outdir}/*.grb2` ; do
	if [ ! -e ${this_file%%.grb2}.nc ] ; then
	    $HOME/src/convsh/conv2nc_linux.tcl -i ${this_file} -o ${this_file%%.grb2}.nc 
	fi
    done

    # Make climatologies for desired variables
    
    k=0
    for this_var in ${out_vars[@]} ; do
	this_ncvar=${nc_vars[$k]}
	j=0
	while [ ${j} -lt ${nmonths} ] ; do
	    fcst_month=$((month+j))
	    if [ ${fcst_month} -gt 12 ] ; then	    
		fcst_month=$((fcst_month-12)) ; fi
	    if [ ${fcst_month} -lt 10 ] ; then
		fcst_month=0${fcst_month} ; fi	
	    echo ${fcst_month}
	    set -A infiles `ls ${outdir}/flxf*.01.*${fcst_month}.avrg.nc`
	    outfile=${outdir}/${this_var}_clim_month${fcst_month}.nc
	    ncea -O -o ${outfile} -v ${this_ncvar} ${hyperslab[$k]} ${infiles[@]}
	    j=$((j+1))
	done
	k=$((k+1))
    done
done

