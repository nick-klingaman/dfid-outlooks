#!/bin/ksh
set -x
set -A fields 'total_precipitation_daily' '2m_temperature_6hr'
set -A grib_names 'tp' 't2m'

#set -A fields 't2m'
#set -A grib_names 't2m'

days_fcst=215
sixhr_fcst=`expr ${days_fcst} \* 4 - 1`
this_date=$1
run_year=$2
this_year=$3

i=0
cd hindcasts_${this_date}_${run_year}
for this_field in ${fields[@]} ; do
    set -A infiles `ls hindcast_${this_year}????_${this_field}.grib`
    set -A outfiles
    for infile in ${infiles[@]} ; do
	outfile=${infile%%.grib}.nc
#	grib_to_netcdf -T ${infile} -o ${outfile}	    
#	ncpdq -U -O -o ${outfile} ${outfile}
	ncks -d system,5.,5. -O -o ${outfile} ${outfile} 
	ncwa -a system -O -o ${outfile} ${outfile}
 	ncks --mk_rec_dmn step ${outfile} -O -o ${outfile} 
	if [[ ${this_field} == 'total_precipitation_daily' ]] ; then
	    j=0	
	    typeset -Z4 this_time=${j}
	    this_longtime=${this_year}${this_time}
	    ncks -d step,0,0 ${outfile} -O temp_${this_longtime}.nc
	    cp temp_${this_longtime}.nc last_${this_year}.nc
	    j=$((j+1))
	    while [ ${j} -lt ${days_fcst} ] ; do
		typeset -Z4 this_time=${j}
		this_longtime=${this_year}${this_time}
		echo ${this_longtime}
		command='ncks -O -o this_'${this_year}'.nc -d step,'${j}','${j}' '${outfile} ; $command
		command='ncdiff -O -o temp_'${this_longtime}'.nc  this_'${this_year}'.nc last_'${this_year}'.nc' ; $command
		cp this_${this_year}.nc last_${this_year}.nc
		j=$((j+1))
	    done
	    set -A temp_files `ls temp_${this_year}????.nc`
	    command='ncrcat -O -o '${outfile}' '${temp_files[@]} ; echo $command ; $command
	    rm -v temp_${this_year}????.nc this_${this_year}.nc last_${this_year}.nc
	fi
	set -A outfiles ${outfiles[@]} ${outfile}
	command='ncrename -d number,realization -v number,realisation -d step,time -v step,time '${outfile} ; echo $command ; $command
#	command='ncpdq -a realization,time '${outfile}' -O '${outfile} ; echo $command ; $command
	command='ncks --mk_rec_dmn realization '${outfile}' -O '${outfile} ; echo $command ; $command
	done
    ensmean_outfile=hindcast_${this_year}${this_date}_${this_field}_ensmean.nc
    command='ncea -O -o '${ensmean_outfile}' '${outfiles[@]} ; echo $command ; $command
    command='ncwa -a realization -O '${ensmean_outfile}' '${ensmean_outfile} ; echo $command ; $command
done


