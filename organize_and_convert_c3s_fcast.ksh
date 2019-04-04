#!/bin/ksh

set -x
set -A fields '2m_temperature_6hr' #'total_precipitation_daily'
set -A grib_names 't2m' #'tp'
#set -A fields 'total_precipitation_daily' 
#set -A grib_names 'tp' 't2m'

#set -A fields 't2m'
#set -A grib_names 't2m'

days_fcst=215
sixhr_fcst=`expr ${days_fcst} \* 4 - 1`
this_date=$1
run_year=$2

i=0
cd operational_${this_date}
for this_field in ${fields[@]} ; do
	set -A infiles `ls operational_????????_${this_field}.grib`
	set -A outfiles
	for infile in ${infiles[@]} ; do
	    outfile=${infile%%.grib}.nc
	    grib_to_netcdf -T -u step ${infile} -o ${outfile}	    
	    ncpdq -U -O -o ${outfile} ${outfile}
		# Deaccumulate daily precipitation
	    if [[ ${this_field} == 'total_precipitation_daily' ]] ; then
			j=0	
			typeset -Z4 this_time=${j}
			ncks -d step,0,0 ${outfile} -O temp${this_time}.nc
			cp temp${this_time}.nc last.nc
			j=$((j+1))
			while [ ${j} -lt ${days_fcst} ] ; do
			    typeset -Z4 this_time=${j}
			    command='ncks -O -o this.nc -d step,'${j}','${j}' '${outfile} ; $command
			    command='ncdiff -O -o temp'${this_time}.nc' this.nc last.nc' ; $command
		    	cp this.nc last.nc
		    	j=$((j+1))
			done
		    set -A temp_files `ls temp????.nc`
		    command='ncrcat -O -o '${outfile}' '${temp_files[@]} ; echo $command ; $command
			rm -v temp????.nc this.nc last.nc
		fi
		set -A outfiles ${outfiles[@]} ${outfile}
		command='ncrename -d number,realization -v number,realisation -d step,time -v step,time '${outfile} ; echo $command ; $command
		command='ncpdq -a realization,time '${outfile}' -O '${outfile} ; echo $command ; $command
		command='ncks --mk_rec_dmn realization '${outfile}' -O '${outfile} ; echo $command ; $command
	done
	ensmem_outfile=operational_${this_date}_${this_field}_ensmem.nc
	ensmean_outfile=operational_${this_date}_${this_field}_ensmean.nc
	command='ncrcat -O -o '${ensmem_outfile}' '${outfiles[@]} ; echo $command ; $command
	command='ncea -O -o '${ensmean_outfile}' '${outfiles[@]} ; echo $command ; $command
	command='ncwa -a realization -O '${ensmean_outfile}' '${ensmean_outfile} ; echo $command ; $command
	cat << EOF > fix_netcdf_variable_monotonic.namelist
&ctl
  infile='${ensmem_outfile}'
  var_name='realization'
  start_val=1
  interval=1
&end
EOF
	    fix_netcdf_variable_monotonic < fix_netcdf_variable_monotonic.namelist
done


