#!/bin/ksh

# Current year
baseyear=2019
# Month of the first forecast in the set
month=3
# Years for each forecast start date
set -A years '2019' '2019' '2019' '2019' '2019' '2019'
# Forecast start dates to obtain (MMDD)
set -A dates '0330' '0331' '0401' '0402' '0403' '0404'
# Hours (NCEP use a lagged ensemble - no need to edit)
set -A hours '00' '06' '12' '18'
# Number of ensemble members for each hour above (do not edit)
set -A numbers 4    1    1    1
# Use bias correction? (1=yes, 0=no)
bias_correct=1
# Reforecast start date to use for bias correction (MMDD) - must have already been downloaded and processed
hcast_date=0401

# Number of months in forecast
nmonths=9
# Download forecast data before processing? (1=yes, 0=no)
get=1

set -A out_vars 'precip' 't2m' 'soilmc'
set -A nc_vars 'prate' 't_1' 'soilw'
set -A hyperslab ' ' ' ' '-d depth,0,0'

basedir=/gws/nopw/j04/klingaman/datasets/S2S/NCEP/ENSO
outdir=${basedir}/operational_${years[0]}${dates[0]}-${years[5]}${dates[5]}
mkdir -p ${outdir}

baseurl=http://nomads.ncep.noaa.gov/pub/data/nccf/com/cfs/prod/cfs

if [ ${get} -eq 1 ] ; then
    if [ -e ${basedir}/wget_op.list ] ; then
	/bin/rm ${basedir}/wget_op.list
	touch ${basedir}/wget_op.list
    fi
    i=0
    for this_date in ${dates[@]} ; do	
	j=0
	year=${years[i]}
	dateurl=${baseurl}/cfs.${year}${this_date}
	for this_hour in ${hours[@]} ; do
	    max_number=${numbers[$j]}
	    k=1
	    echo $max_number
	    hoururl=${dateurl}/${this_hour}
	    while [ ${k} -le ${max_number} ] ; do
		this_number=0${k}
		ensurl=${hoururl}/monthly_grib_${this_number}
		fileurl=${ensurl}/flxf.${this_number}.${year}${this_date}${this_hour}
		m=1
		while [ ${m} -lt ${nmonths} ] ; do
		    fcst_month=$((month+m))
		    if [ ${fcst_month} -gt 12 ] ; then
			fcst_year=$((baseyear+1))
			fcst_month=$((fcst_month-12))
		    else
			fcst_year=${baseyear}
		    fi
		    if [ ${fcst_month} -lt 10 ] ; then
			fcst_month=0${fcst_month} ; fi
		    fcsturl=${fileurl}.${fcst_year}${fcst_month}.avrg.grib.grb2
		    localfile=${outdir}/flxf.${this_number}.${year}${this_date}${this_hour}.${fcst_year}${fcst_month}.avrg.grib.grb2
		    if [ ! -e ${outdir}/flxf.${this_number}.${year}${this_date}${this_hour}.${fcst_year}${fcst_month}.avrg.grib.grb2 ] ; then
			echo ${fcsturl} >> ${basedir}/wget_op.list
			echo ${localfile}
		    	fi
		    m=$((m+1))
		done
		k=$((k+1))
	    done
	    j=$((j+1))
	done
	i=$((i+1))
    done
    wget -i ${basedir}/wget_op.list -P ${outdir}    
fi
# Convert to netCDF
for this_file in `ls ${outdir}/*.grb2` ; do
    if [ ! -e ${this_file%%.grib.grb2}.nc ] ; then
	$HOME/src/convsh/conv2nc_linux.tcl -i ${this_file} -o ${this_file%%.grib.grb2}.nc ; fi
done

# Make climatologies for desired variables
k=0
for this_var in ${out_vars[@]} ; do
    this_ncvar=${nc_vars[$k]}
    j=1
    set -A fcst_months
    set -A ensmean_anom_outfiles
    set -A ensmean_outfiles
    set -A ensmem_outfiles
    set -A ensmem_anom_outfiles
    while [ ${j} -lt ${nmonths} ] ; do
	fcst_month=$((month+j))
	if [ ${fcst_month} -gt 12 ] ; then	    
	    fcst_month=$((fcst_month-12))
	    fcst_year=$((baseyear+1))
	else
	    fcst_year=${baseyear}
	fi
	if [ ${fcst_month} -lt 10 ] ; then
	    fcst_month=0${fcst_month} ; fi	
	echo ${fcst_month}
	set -A infiles `ls ${outdir}/flxf*.01.*${fcst_month}.avrg.nc`
	ensmean_outfile=${outdir}/${this_var}_${fcst_year}${fcst_month}.nc
	ensmem_outfile=${outdir}/${this_var}_${fcst_year}${fcst_month}_ensmem.nc
	command='ncea -O -o '${ensmean_outfile}' -v '${this_ncvar}' '${hyperslab[$k]}' '${infiles[@]} ; echo $command ; $command
	command='ncecat -O -o '${ensmem_outfile}' -v '${this_ncvar}' '${hyperslab[$k]}' '${infiles[@]} ; echo $command ; $command
	set -A ensmem_outfiles ${ensmem_outfiles[@]} ${ensmem_outfile}
	set -A ensmean_outfiles ${ensmean_outfiles[@]} ${ensmean_outfile}
# Bias correct
	command='ncpdq -O -a t,record '${ensmem_outfile}' '${ensmem_outfile} ; echo $command ; $command	
	if [ ${bias_correct} -eq 1 ] ; then
	    hcast_file=${basedir}/hindcasts_${hcast_date}/${this_var}_clim_month${fcst_month}.nc
	    ensmean_anom_outfile=${outdir}/${this_var}_${fcst_year}${fcst_month}.anom_hcast_${hcast_date}.nc
	    command='ncdiff -O '${ensmean_outfile}' '${hcast_file}' '${ensmean_anom_outfile} ; echo $command ; $command
 	    ensmem_anom_outfile=${outdir}/${this_var}_${fcst_year}${fcst_month}.anom_hcast_${hcast_date}_ensmem.nc
	    command='ncdiff -O '${ensmem_outfile}' '${hcast_file}' '${ensmem_anom_outfile} ; echo $command ; $command
	    set -A ensmean_anom_outfiles ${ensmean_anom_outfiles[@]} ${ensmean_anom_outfile}	    
	    set -A ensmem_anom_outfiles ${ensmem_anom_outfiles[@]} ${ensmem_anom_outfile}
	    ncpdq -O -a t,record ${ensmem_anom_outfile} ${ensmem_anom_outfile}
	fi
	set -A fcst_months ${fcst_months[@]} ${fcst_year}${fcst_month}
	j=$((j+1))
    done
    concat_outfile=${outdir}/${this_var}_${fcst_months[0]}-${fcst_months[$((nmonths-2))]}.nc
    ncrcat -O -o ${concat_outfile} ${ensmean_outfiles[@]}    
    concat_outfile=${outdir}/${this_var}_${fcst_months[0]}-${fcst_months[$((nmonths-2))]}_ensmem.nc
    ncrcat -O -o ${concat_outfile} ${ensmem_outfiles[@]}
	ncks -3 ${concat_outfile} -O ${concat_outfile}
	ncrename -d record,realization ${concat_outfile}
	cat << EOF > fix_netcdf_variable_monotonic.namelist
&ctl 
 infile='${concat_outfile}'
 var_name='realization'
 start_val=1
 interval=1
&end
EOF
	fix_netcdf_variable_monotonic < ./fix_netcdf_variable_monotonic.namelist
	ncwa -a surface -O ${concat_outfile} ${concat_outfile}
    if [ ${bias_correct} -eq 1 ] ; then
		concat_anom_outfile=${outdir}/${this_var}_${fcst_months[0]}-${fcst_months[$((nmonths-2))]}.anom_hcast_${hcast_date}.nc
		ncrcat -O -o ${concat_anom_outfile} ${ensmean_anom_outfiles[@]} ; fi
	concat_anom_outfile=${outdir}/${this_var}_${fcst_months[0]}-${fcst_months[$((nmonths-2))]}.anom_hcast_${hcast_date}_ensmem.nc
	ncrcat -O -o ${concat_anom_outfile} ${ensmem_anom_outfiles[@]}
    k=$((k+1))
done

