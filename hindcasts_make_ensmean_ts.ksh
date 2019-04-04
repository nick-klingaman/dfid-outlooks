#!/bin/ksh

date=$1
cd hindcasts_${date}
set -A vars 'prate' 't_1'
start_month=`echo ${date} | cut -c1-2`
stop_month=$((start_month+9))
if [ ${stop_month} -gt 12 ] ; then
    stop_month=$((stop_month-12))
fi
if [ ${stop_month} -lt 10 ] ; then
	stop_month=0${stop_month} ; fi
	
for this_var in ${vars[@]} ; do
    ensmean_outfile=${this_var}.month${start_month}-month${stop_month}_mmeans_ensmean.1982-2011.nc   
    if [ ! -e ${ensmean_outfile} ] ; then
        set -A temp_files
        for this_file in `ls flxf????${date}00.*.avrg.nc` ; do
            set -A infiles ${this_file}
            yr=`echo ${this_file} | cut -c5-8`
            for hr in 06 12 18 ; do
                set -A infiles ${infiles[@]} flxf${yr}${date}${hr}${this_file##flxf$yr${date}00}
            done 
            temp_file=ensmean_${this_var}_${yr}${date}${this_file##flxf$yr${date}00}
            if [ ! -e ${temp_file} ] ; then
		command='ncea -O -o '${temp_file}' -v '${this_var}' '${infiles[@]} ; echo $command ; $command
	    fi
            set -A temp_files ${temp_files[@]} ${temp_file}
        done
        command='ncrcat -O '${temp_files[@]}' '${ensmean_outfile} ; echo $command ; $command
    fi
    ensmem_outfile=${this_var}.month${start_month}-month${stop_month}_mmeans_ensmem.1982-2011.nc
    if [ ! -e ${ensmem_outfile} ] ; then
		set -A temp_files
		m=0
		while [ ${m} -lt 10 ] ; do
	   		this_month=$((m+start_month))
	    	if [ ${this_month} -gt 12 ] ; then
				this_month=$((this_month-12)) ; fi
	    	if [ ${this_month} -lt 10 ] ; then
				this_month=0${this_month} ; fi
	    	set -A infiles `ls flxf*.01.????${this_month}.avrg.nc`	
	    	temp_file=ensmem_${this_month}.nc
	    	command='ncecat -v '${this_var}' -O -o '${temp_file}' '${infiles[@]} ; echo $command ; $command
	    	command='ncwa -a surface -O -o '${temp_file}' '${temp_file} ; echo $command ; $command
	    	command='ncpdq -a t,record -O -o '${temp_file}' '${temp_file} ; echo $command ; $command
	    	command='ncks --mk_rec_dmn t -O -o '${temp_file}' '${temp_file} ; echo $command ; $command
	    	m=$((m+1))
	    	set -A temp_files ${temp_files[@]} ${temp_file}
		done
		command='ncrcat -O -o '${ensmem_outfile}' '${temp_files[@]} ; echo $command ; $command
		command='ncks -3 -O -o '${ensmem_outfile}' '${ensmem_outfile} ; echo $command ; $command
		command='ncrename -d record,realization '${ensmem_outfile} ; echo $command ; $command
		cat << EOF > fix_netcdf_variable_monotonic.namelist
&ctl
 infile='${ensmem_outfile}'
 var_name='realization'
 start_val=1
 interval=1
&end
EOF
		fix_netcdf_variable_monotonic < ./fix_netcdf_variable_monotonic.namelist
    fi
done
