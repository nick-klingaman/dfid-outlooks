#!/bin/ksh

fcast_date=$1
fcast_year=`echo ${fcast_date} | cut -c1-4`
hcast_date=$2
hcast_runyear=$3
start_month=$4
offset_day=$5
nmem=25

hcast_proc=0
fcast_proc=1

ndays=215
set -A months        '01' '02' '03' '04' '05' '06' '07' '08' '09' '10' '11' '12' 
set -A ndays_noleap  '31' '28' '31' '31' '30' '30' '31' '31' '30' '31' '30' '31'
set -A ndays_leap    '31' '29' '31' '31' '30' '30' '31' '31' '30' '31' '30' '31'

set -A fields '2m_temperature_6hr' 'total_precipitation_daily' #'2m_temperature_6hr'

fcast_dir=operational_${fcast_date}
hcast_dir=hindcasts_${hcast_date}_${hcast_runyear}
    
for this_field in ${fields[@]} ; do
    if [ ${this_field} == 'total_precipitation_daily' ] ; then
        time_mult=1
    elif [ ${this_field} == '2m_temperature_6hr' ] ; then
        time_mult=4
    fi
    ## Hindcast processing
    if [ ${hcast_proc} -eq 1 ] ; then
        hcast_ensmean_infile=${hcast_dir}/hindcast_${hcast_date}_${this_field}_ensmean.nc
        command='ncks -4 --mk_rec_dmn time -O -o '${hcast_ensmean_infile}' '${hcast_ensmean_infile} ; echo $command ; $command
        set -A hcast_ensmem_mmean_files
        day=0
        this_month=$((start_month-1))
        ndays_month=${ndays_noleap[$this_month]}
        start_time=`expr $((day+offset_day)) \* ${time_mult}`
        stop_time=`expr ${start_time} + ${ndays_month} \* ${time_mult} - 1`
        this_year=${fcast_year}
        while [ ${stop_time} -lt $((ndays*time_mult)) ] ; do
            echo ${start_time} ${stop_time}
            hcast_ensmean_mmean=${hcast_dir}/hindcast_${hcast_date}_${this_field}_ensmean_${this_year}${months[$this_month]}.nc
            if [ ! -e ${hcast_ensmean_mmean} ] ; then
                command='ncra -O -o '${hcast_ensmean_mmean}' -d time,'${start_time}','${stop_time}' '${hcast_ensmean_infile} ; echo $command ; $command 
	            command='ncwa -a realization -O -o '${hcast_ensmean_mmean}' '${hcast_ensmean_mmean} ; echo $command ; $command ; fi
            # Process re-forecasts
            set -A hcast_files `ls ${hcast_dir}/hindcast_????${hcast_date}_${this_field}.nc`
	        set -A hcast_mmeans
            for this_file in ${hcast_files[@]} ; do
                hcast_mmean=${this_file%%.nc}_${this_year}${months[${this_month}]}.nc
                if [ ! -e ${hcast_mmean} ] ; then
                    command='ncks -O -o '${hcast_mmean}' -d time,'${start_time}','${stop_time}' '${this_file} ; echo $command ; $command
        	        command='ncwa -a time '${hcast_mmean}' -O '${hcast_mmean} ; echo $command ; $command
                fi
	            set -A hcast_mmeans ${hcast_mmeans[@]} ${hcast_mmean}
            done
            ensmem_mmean=${hcast_dir}/${this_field}_ensmem_${months[${this_month}]}.nc
	        if [ ! -e ${ensmem_mmean} ] ; then
	            command='ncrcat -O -o '${ensmem_mmean}' '${hcast_mmeans[@]} ; echo $command ; $command
	        fi
	        set -A hcast_ensmem_mmean_files ${hcast_ensmem_mmean_files[@]} ${ensmem_mmean}

            this_month=$((this_month+1))
            if [ ${this_month} -ge 12 ] ; then
                this_month=$((this_month-12))
                this_year=$((this_year+1))
            fi
            ndays_month=${ndays_noleap[$this_month]}
            start_time=$((stop_time+1))
            stop_time=`expr $start_time + $ndays_month \* ${time_mult} - 1`
            if [ ${stop_time} -ge $((ndays*time_mult)) ] ; then
                this_month=$((this_month-1))
                if [ ${this_month} -lt 1 ] ; then
                    this_month=$((this_month+12))
                    this_year=$((this_year-1))
                fi
            fi
            echo ${this_year} ${this_month} ${start_time} ${stop_time}
        done
        set -A hcast_mmeans `ls ${hcast_dir}/hindcast_????${hcast_date}_${this_field}_??????.nc`
    hcast_ensmem=${hcast_dir}/hindcast_${hcast_date}_${this_field}_mmeans_ensmem.nc
    hcast_ensmean=${hcast_dir}/hindcast_${hcast_date}_${this_field}_mmeans_ensmeans.nc
    m=0
    set -A mem_files
    while [ $m -lt ${nmem} ] ; do
        mem=`printf %02i $((m+1))`
        mem_file=${hcast_dir}/temp_mem${mem}.nc
        command='ncecat -O -o '${mem_file}' -d realization,'$m','$m' '${hcast_mmeans[@]} ; echo $command ; $command
    	command='ncwa -a realization -O '${mem_file}' '${mem_file} ; echo $command ; $command
    	command='ncrename -d record,time '${mem_file} ; echo $command ; $command
    	set -A mem_files ${mem_files[@]} ${mem_file}
        m=$((m+1))
    done
    if [ ! -e ${hcast_ensmean} ] ; then
        command='ncea -O -o '${hcast_ensmean}' '${mem_files[@]} ; echo $command ; $command
        command='ncks -3 -O -o '${hcast_ensmean}' '${hcast_ensmean} ; echo $command ; $command
        command='ncrename -d .record,time -v .record,time '${hcast_ensmean} ; echo $command ; $command
    fi
    if [ ! -e ${hcast_ensmem} ] ; then
        command='ncecat -O -o '${hcast_ensmem}' '${hcast_ensmem_mmean_files[@]} ; echo $command ; $command
        command='ncks -3 -O -o '${hcast_ensmem}' '${hcast_ensmem} ; echo $command ; $command
        command='ncrename -d .record,time -v .record,time '${hcast_ensmem} ; echo $command ; $command
	command='ncks --mk_rec_dmn time -O -o '${hcast_ensmem}' '${hcast_ensmem} ; echo $command ; $command
    fi
    cat << EOF > fix_netcdf_variable_monotonic.namelist
        &ctl
        infile='${hcast_ensmean}'
        var_name='time'
        start_val=0.5
        interval=1
        &end
EOF
    fix_netcdf_variable_monotonic < ./fix_netcdf_variable_monotonic.namelist
    cat << EOF > fix_netcdf_variable_monotonic.namelist
        &ctl
        infile='${hcast_ensmem}'
        var_name='time'
        start_val=0.5
        interval=1
        &end
EOF
    fix_netcdf_variable_monotonic < ./fix_netcdf_variable_monotonic.namelist
    cat << EOF > fix_netcdf_variable_monotonic.namelist
    &ctl
     infile='${hcast_ensmem}'
     var_name='realization'
     start_val=1
     interval=1
    &end
EOF
    fix_netcdf_variable_monotonic < ./fix_netcdf_variable_monotonic.namelist
    rm -v ${mem_files[@]}        
    fi
    
    # Forecast processing
    if [ ${fcast_proc} -eq 1 ] ; then
    
        fcast_ensmean_infile=${fcast_dir}/operational_${fcast_date}_${this_field}_ensmean.nc
        fcast_ensmem_infile=${fcast_dir}/operational_${fcast_date}_${this_field}_ensmem.nc
        command='ncks -4 --mk_rec_dmn time -O -o '${fcast_ensmean_infile}' '${fcast_ensmean_infile} ; echo $command ; $command
        command='ncks -4 --mk_rec_dmn time -O -o '${fcast_ensmem_infile}' '${fcast_ensmem_infile} ; echo $command ; $command
        command='ncks --fix_rec_dmn realization -O -o '${fcast_ensmem_infile}' '${fcast_ensmem_infile} ; echo $command ; $command
        day=0
        this_month=$((start_month-1))
        ndays_month=${ndays_noleap[$this_month]}
        start_time=`expr $((day+offset_day)) \* ${time_mult}`
        stop_time=`expr ${start_time} + ${ndays_month} \* ${time_mult} - 1`
        set -A fcast_ensmean_mmean_files
        set -A fcast_ensmem_mmean_files
        set -A fcast_ensmean_anom_files
        set -A fcast_ensmem_anom_files

        this_year=${fcast_year}
        while [ ${stop_time} -lt $((ndays*time_mult)) ] ; do
            echo ${start_time} ${stop_time}
            fcast_ensmean_mmean=${fcast_dir}/operational_${fcast_date}_${this_field}_ensmean_${this_year}${months[$this_month]}.nc
            fcast_ensmem_mmean=${fcast_dir}/operational_${fcast_date}_${this_field}_ensmem_${this_year}${months[$this_month]}.nc
            fcast_ensmean_anom=${fcast_dir}/operational_${fcast_date}_${this_field}_ensmean_${this_year}${months[$this_month]}_anom.nc
            fcast_ensmem_anom=${fcast_dir}/operational_${fcast_date}_${this_field}_ensmem_${this_year}${months[$this_month]}_anom.nc
            hcast_ensmean_mmean=${hcast_dir}/hindcast_${hcast_date}_${this_field}_ensmean_${this_year}${months[$this_month]}.nc

            if [ ! -e ${fcast_ensmean_mmean} ] ; then
                command='ncra -O -o '${fcast_ensmean_mmean}' -d time,'${start_time}','${stop_time}' '${fcast_ensmean_infile} ; echo $command ; $command ; fi
            if [ ! -e ${fcast_ensmem_mmean} ] ; then
                command='ncks -O -o '${fcast_ensmem_mmean}' -d time,'${start_time}','${stop_time}' '${fcast_ensmem_infile} ; echo $command ; $command
                command='ncpdq -a time,realization -O -o '${fcast_ensmem_mmean}' '${fcast_ensmem_mmean} ; echo $command ; $command
                command='ncks -3 -O -o '${fcast_ensmem_mmean}' '${fcast_ensmem_mmean}' ' ; echo $command ; $command
                command='ncra -O -o '${fcast_ensmem_mmean}' '${fcast_ensmem_mmean} ; echo $command ; $command
            fi
            if [ ! -e ${fcast_ensmean_anom} ] ; then
                command='ncdiff -O -o '${fcast_ensmean_anom}' '${fcast_ensmean_mmean}' '${hcast_ensmean_mmean} ; echo $command ; $command ; fi
            if [ ! -e ${fcast_ensmem_anom} ] ; then
                command='ncdiff -O -o '${fcast_ensmem_anom}' '${fcast_ensmem_mmean}' '${hcast_ensmean_mmean} ; echo $command ; $command ; fi
            set -A fcast_ensmean_mmean_files ${fcast_ensmean_mmean_files[@]} ${fcast_ensmean_mmean}
            set -A fcast_ensmem_mmean_files ${fcast_ensmem_mmean_files[@]} ${fcast_ensmem_mmean}
            set -A fcast_ensmean_anom_files ${fcast_ensmean_anom_files[@]} ${fcast_ensmean_anom}
            set -A fcast_ensmem_anom_files ${fcast_ensmem_anom_files[@]} ${fcast_ensmem_anom}

            this_month=$((this_month+1))
            if [ ${this_month} -ge 12 ] ; then
                this_month=$((this_month-12))
                this_year=$((this_year+1))
            fi
            ndays_month=${ndays_noleap[$this_month]}
            start_time=$((stop_time+1))
            stop_time=`expr $start_time + $ndays_month \* ${time_mult} - 1`
            if [ ${stop_time} -ge $((ndays*time_mult)) ] ; then
                this_month=$((this_month-1))
                if [ ${this_month} -lt 1 ] ; then
                    this_month=$((this_month+12))
                    this_year=$((this_year-1))
                fi
            fi
            echo ${this_year} ${this_month} ${start_time} ${stop_time}
        done
        time_range=${fcast_year}${months[$((start_month-1))]}-${this_year}${months[$this_month]}
        fcast_ensmean_mmeans=${fcast_dir}/operational_${fcast_date}_${this_field}_ensmean_${time_range}.nc
        fcast_ensmem_mmeans=${fcast_dir}/operational_${fcast_date}_${this_field}_ensmem_${time_range}.nc
        fcast_ensmean_anoms=${fcast_dir}/operational_${fcast_date}_${this_field}_ensmean_anom_${time_range}.nc
        fcast_ensmem_anoms=${fcast_dir}/operational_${fcast_date}_${this_field}_ensmem_anom_${time_range}.nc
        command='ncrcat -O -o '${fcast_ensmean_mmeans}' '${fcast_ensmean_mmean_files[@]} ; echo $command ; $command
        command='ncrcat -O -o '${fcast_ensmem_mmeans}' '${fcast_ensmem_mmean_files[@]} ; echo $command ; $command
        command='ncrcat -O -o '${fcast_ensmean_anoms}' '${fcast_ensmean_anom_files[@]} ; echo $command ; $command
        command='ncrcat -O -o '${fcast_ensmem_anoms}' '${fcast_ensmem_anom_files[@]} ; echo $command ; $command
    fi
done
