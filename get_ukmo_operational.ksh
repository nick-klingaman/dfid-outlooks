#!/bin/ksh

baseyear=2019

# Start year of each date in lagged ensemble
set -A start_years '2019' '2019' '2019' '2019' '2019' '2019' '2019' '2019' '2019' '2019' 
# Stop year of each date in lagged ensemble
set -A stop_years  '2019' '2019' '2019' '2019' '2019' '2019' '2019' '2019' '2019' '2019'
# Start date (MMDD) of each date in lagged ensemble
set -A start_dates '0312' '0313' '0314' '0315' '0316' '0317' '0318' '0319' '0320' '0321'
# Stop date (MMDD) of each date in lagged ensemble
set -A stop_dates  '1014' '1015' '1016' '1017' '1018' '1019' '1020' '1021' '1022' '1023'
# Offset to the start of the first month to process
set -A offsets        20     19     18     17     16     15     14     13     12     11
# Does the forecast contain a leap day (0=no, 1=yes)
set -A leap            0      0      0      0      0      0      0      0      0      0

#set -A start_dates '0310' '0311' '0312' '0313' '0313' '0315' '0316' '0317' '0318' '0319'
#set -A stop_dates  '1012' '1013' '1014' '1015' '1015' '1017' '1018' '1019' '1020' '1021'
#set -A offsets        22     21     20     19     19     17     16     15     14     13

# Forecast months to process
set -A fcast_months '201904' '201905' '201906' '201907' '201908' '201909'
# Number of days in each month
set -A month_ends        30       31       30       31       31       30
# Number of ensemble members per forecast day
ensmem=2

# Reforecast start date (MMDD)
hcast_start_date=0317
# Offset to start of the first month to process
hcast_offset=15
# Reforecast stop date (MMDD)
hcast_stop_date=1019
# Does the reforecast period contain a leap day (0=no, 1=yes)?
hcast_leap=0
# First reforecast year
hcast_start_year=1993
# Last reforecast year
hcast_stop_year=2015
# Number of reforecast ensemble members
hcast_ensmem=7

# Do you want to download reforecast data?
hcast_get=1
# Do you want to process reforecast data?
hcast_proc=1

# Do you want to download forecast data?
fcast_get=1
# Do you want to process forecast data?
fcast_proc=1

set -A in_vars 'pr' 'tas' 'mrsos'
set -A nc_vars 'precipitation_flux' 'air_temperature' 'moisture_content_of_soil_layer'
set -A out_vars 'precip' 't2m' 'soilmc'
set -A hyperslab ' ' ' ' ' '

# Base path for downloading and processing data (directories will be created here for each forecast and reforecast).
basedir=/gws/nopw/j04/klingaman/datasets/S2S/UKMO/ENSO
fcast_outdir=${basedir}/operational_${start_years[0]}${start_dates[0]}-${start_years[9]}${start_dates[9]}
mkdir -p ${fcast_outdir}

baseurl=http://gws-access.ceda.ac.uk/public/ukmo-cr/GloSea-PSI-data/enfo/

if [ ${fcast_get} -eq 1 ] ; then
    if [ -e ${basedir}/wget_op.list ] ; then
	/bin/rm ${basedir}/wget_op.list
	touch ${basedir}/wget_op.list
    fi
    i=0
    for this_date in ${start_dates[@]} ; do	
	j=0
	year=${start_years[i]}
	dateurl=${baseurl}/${year}${this_date}/day
	for this_field in ${in_vars[@]} ; do
	    k=1	
	    while [ ${k} -le ${ensmem} ] ; do
		this_number=00${k}
		fileurl=${dateurl}/${this_field}/ukmo_enfo_atmos_day_sfc_GloSea5-GC2_${start_years[i]}${start_dates[i]}00-${stop_years[i]}${stop_dates[i]}00_${this_field}_r${this_number}.nc
		localfile=${fcast_outdir}/ukmo_enfo_atmos_day_sfc_GloSea5-GC2_${start_years[i]}${start_dates[i]}00-${stop_years[i]}${stop_dates[i]}00_${this_field}_r${this_number}.nc
		if [ ! -e ${localfile} ] ; then
		    echo ${fileurl} >> ${basedir}/wget_op.list
		    echo ${localfile}
		fi	
		k=$((k+1))
	    done	    
	    j=$((j+1))
	done
	i=$((i+1))
    done
    wget --http-user reading-dfid --http-password @vyM4SH.4 -i ${basedir}/wget_op.list -P ${fcast_outdir}    
fi

if [ ${hcast_get} -eq 1 ] ; then
    if [ -e ${basedir}/wget_op.list ] ; then
		/bin/rm ${basedir}/wget_op.list
		touch ${basedir}/wget_op.list
    fi
    j=0
    baseurl=http://gws-access.ceda.ac.uk/public/ukmo-cr/GloSea-PSI-data/enfh
    hcast_outdir=${basedir}/hindcasts_${hcast_start_date}_${baseyear}
    mkdir -p ${hcast_outdir}
    dateurl=${baseurl}/${baseyear}${hcast_start_date}/day
    for this_field in ${in_vars[@]} ; do
		k=1
		while [ ${k} -le ${hcast_ensmem} ] ; do
	    	this_number=30${k}
	  	y=${hcast_start_year}
	    	while [ ${y} -le ${hcast_stop_year} ] ; do
				if [ $(((y)%4)) -eq 0 ] ; then
		    		this_stop_date=$((hcast_stop_date-hcast_leap))
				else
			    	this_stop_date=${hcast_stop_date}
				fi
				typeset -Z4 this_stop_date=${this_stop_date} 
				fileurl=${dateurl}/${this_field}/ukmo_enfh_atmos_day_sfc_GloSea5-GC2_${y}${hcast_start_date}00-$((y))${this_stop_date}00_${this_field}_r${this_number}.nc
				localfile=${hcast_outdir}/ukmo_enfh_atmos_day_sfc_GloSea5-GC2_${y}${hcast_start_date}00-$((y))${this_stop_date}00_${this_field}_r${this_number}.nc
				if [ ! -e ${localfile} ] ; then
			    	echo ${fileurl} >> ${basedir}/wget_op.list
			    	echo ${localfile}
				fi	
				y=$((y+1))
	    	done
	    	k=$((k+1))
		done
	done
    wget --http-user reading-dfid --http-password @vyM4SH.4 -i ${basedir}/wget_op.list -P ${hcast_outdir}
fi
if [ ${hcast_proc} -eq 1 ] ; then
    hcast_outdir=${basedir}/hindcasts_${hcast_start_date}_${baseyear}
    i=0
    for this_field in ${in_vars[@]} ; do
		set -A infiles `ls ${hcast_outdir}/ukmo_enfh_atmos_day_sfc_GloSea5-GC2_*${this_field}_r???.nc`
		set -A ensmem_mmeans
		this_start=${hcast_offset}
		k=0
		for this_month in ${fcast_months[@]} ; do
	 	   	this_stop=$((this_start+${month_ends[$k]}-1))
			echo ${this_start} ${this_stop}
	 	   	temp_outfile=${hcast_outdir}/${this_field}_${hcast_start_date}_${this_month}.nc
			if [ ! -e ${temp_outfile} ] ; then
		 	   	command='ncea -O -o '${temp_outfile}' -v '${nc_vars[$i]}' -d time,'${this_start}','${this_stop}' '${infiles[@]} ; echo $command ; $command
		 	   	command='ncra -O -o '${temp_outfile}' '${temp_outfile} ; echo $command ; $command
			fi
			for this_file in ${infiles[@]} ; do
				outfile=${this_file%%.nc}_${this_month}.nc
				if [ ! -e ${outfile} ] ; then
					command='ncra -O -o '${outfile}' -d time,'${this_start}','${this_stop}' '${this_file} ; echo $command ; $command
				fi
			done
			ensmem_mmean=${hcast_outdir}/${this_field}_ensmem_${this_month}.nc
			if [ ! -e ${ensmem_mmean} ] ; then
				command='ncecat -O -o '${ensmem_mmean}' -v '${nc_vars[$i]}' -d time,'${this_start}','${this_stop}' '${infiles[@]} ; echo $command ; $command 
				command='ncpdq -a time,record -O -o '${ensmem_mmean}' '${ensmem_mmean}' ' ; echo $command ; $command
				command='ncks --mk_rec_dmn time -O -o '${ensmem_mmean}' '${ensmem_mmean}' ' ; echo $command ; $command
				command='ncra -O -o '${ensmem_mmean}' '${ensmem_mmean}' ' ; echo $command ; $command
			fi
			set -A ensmem_mmeans ${ensmem_mmeans[@]} ${ensmem_mmean}
			k=$((k+1))
	 	   	this_start=$((this_stop+1))
		done

		hcast_ensmean=${hcast_outdir}/${this_field}_${hcast_start_date}_${fcast_months[0]}-${fcast_months[-1]}_ensmeans.nc
		set -A hcast_ensmeans 
		for this_file in `ls ${hcast_outdir}/ukmo_enfh_atmos_day_sfc_GloSea5-GC2_*${this_field}_r301_??????.nc` ; do	
			m=2
			set -A mmean_files ${this_file}
			while [ $m -le 7 ] ; do
				memfile=`echo ${this_file} | sed -e s/r301/r30${m}/g`				
				set -A mmean_files ${mmean_files[@]} ${memfile}
				m=$((m+1))
			done
			ensmean_file=`echo ${this_file} | sed -e s/r301/ensmean/g`
			set -A hcast_ensmeans ${hcast_ensmeans[@]} ${ensmean_file}
			if [ ! -e ${ensmean_file} ] ; then
				command='ncea -O -o '${ensmean_file}' '${mmean_files[@]} ; echo $command ; $command ; fi
		done
		command='ncrcat -O -o '${hcast_ensmean}' '${hcast_ensmeans[@]} ; echo $command ; $command
		command='ncks -3 -O -o '${hcast_ensmean}' '${hcast_ensmean} ; echo $command ; $command
		command=${HOME}'/src/convsh/interpolate_to_n768e_linux.tcl -i '${hcast_ensmean}' -o '${hcast_ensmean%%.nc}.n768e.nc' -f 4 -b' ; echo $command ; $command

		hcast_ensmem=${hcast_outdir}/${this_field}_${hcast_start_date}_${fcast_months[0]}-${fcast_months[-1]}_ensmem.nc
		if [ ! -e ${hcast_ensmem} ] ; then 
			command='ncrcat -O -o '${hcast_ensmem}' '${ensmem_mmeans[@]} ; echo $command ; $command
			command='ncrename -d .record,realization '${hcast_ensmem} ; echo $command ; $command
			command='ncks -3 -O -o '${hcast_ensmem}' '${hcast_ensmem} ; echo $command ; $command
			cat << EOF > fix_netcdf_variable_monotonic.namelist
&ctl
 infile='${hcast_ensmem}'
 var_name='realization'
 start_val=1
 interval=1
&end
EOF
			command=${HOME}'/src/convsh/interpolate_to_n768e_linux.tcl -i '${hcast_ensmem}' -o '${hcast_ensmem%%.nc}.n768e.nc' -f 4 -b' ; echo $command ; $command
		fi
		i=$((i+1))
    done
fi

if [ ${fcast_proc} -eq 1 ] ; then
    echo 'Processing forecasts ...'
    hcast_outdir=${basedir}/hindcasts_${hcast_start_date}_${baseyear}
    i=0
    for this_field in ${in_vars[@]} ; do
		echo 'Processing '${this_field}' ...'
		j=0
		for this_date in ${start_dates[@]} ; do
	   		echo 'Processing start date '${this_date}' ...'
	    		m=1
	   		while [ ${m} -le ${ensmem} ] ; do
				set -A infiles `ls ${fcast_outdir}/ukmo_enfo_atmos_day_sfc_GloSea5-GC2_${start_years[j]}${this_date}*${this_field}_r00${m}.nc`
				this_start=${offsets[$j]}
				k=0	    
				for this_month in ${fcast_months[@]} ; do	   
			    		this_stop=$((this_start+${month_ends[$k]}-1))
			   		echo ${this_start} ${this_stop}	
			    		temp_outfile=${fcast_outdir}/${this_field}_${this_date}_mem${m}_${this_month}.nc
			    		if [ ! -e ${temp_outfile} ] ; then
						command='ncra -O -o '${temp_outfile}' -v '${nc_vars[$i]}' -d time,'${this_start}','${this_stop}' '${infiles[@]} ; echo $command ; $command
					fi		    
					k=$((k+1))
					this_start=$((this_stop+1))
				done
				m=$((m+1))
		    	done
		    	j=$((j+1))
		done
		set -A ensmean_outfiles
		set -A ensmem_outfiles
		set -A ensmean_anom_outfiles
		set -A ensmem_anom_outfiles
		for this_month in ${fcast_months[@]} ; do
	    		set -A infiles `ls ${fcast_outdir}/${this_field}_*_${this_month}.nc`
	    		ensmean_outfile=${fcast_outdir}/${this_field}_${this_month}_ensmean.nc
		    	if [ ! -e ${ensmean_outfile} ] ; then
				command='ncea -O -o '${ensmean_outfile}' '${infiles[@]} ; echo $command ; $command ; fi	   
	    			ensmem_outfile=${fcast_outdir}/${this_field}_${this_month}_ensmem.nc
	    	if [ ! -e ${ensmem_outfile} ] ; then
				command='ncecat -O -o '${ensmem_outfile}' '${infiles[@]} ; echo $command ; $command ; fi
	    	hcast_file=`ls ${hcast_outdir}/${this_field}_${hcast_start_date}_${this_month}.nc`
	    	ensmean_anom_outfile=${fcast_outdir}/${this_field}_${this_month}_ensmean_anom.nc
	    	ensmem_anom_outfile=${fcast_outdir}/${this_field}_${this_month}_ensmem_anom.nc
	    	if [ ! -e ${ensmean_anom_outfile} ] ; then
				command='ncdiff -O -o '${ensmean_anom_outfile}' '${ensmean_outfile}' '${hcast_file} ; echo $command ; $command ; fi
	    	if [ ! -e ${ensmem_anom_outfile} ] ; then
				command='ncdiff -O -o '${ensmem_anom_outfile}' '${ensmem_outfile}' '${hcast_file} ; echo $command ; $command
				command='ncpdq -a time,record '${ensmem_anom_outfile}' -O '${ensmem_anom_outfile} ; echo $command ; $command ; fi
	    		command='ncpdq -a time,record '${ensmem_outfile}' -O '${ensmem_outfile} ; echo $command ; $command
	  		set -A ensmean_outfiles ${ensmean_outfiles[@]} ${ensmean_outfile}
	    	set -A ensmean_anom_outfiles ${ensmean_anom_outfiles[@]} ${ensmean_anom_outfile}
	    	set -A ensmem_outfiles ${ensmem_outfiles[@]} ${ensmem_outfile}
			set -A ensmem_anom_outfiles ${ensmem_anom_outfiles[@]} ${ensmem_anom_outfile}
			ensmem_outfile=${fcast_outdir}/${this_field}_${fcast_months[0]}-${fcast_months[-1]}_ensmem.nc
			command='ncrcat -O -o '${fcast_outdir}/${this_field}_${fcast_months[0]}-${fcast_months[-1]}_ensmean.nc' '${ensmean_outfiles[@]} ; echo $command ; $command
			command='ncrcat -O -o '${fcast_outdir}/${this_field}_${fcast_months[0]}-${fcast_months[-1]}_ensmean_anom.nc' '${ensmean_anom_outfiles[@]} ; echo $command ; $command
			command='ncrcat -O -o '${ensmem_outfile}' '${ensmem_outfiles[@]} ; echo $command ; $command
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
			command='ncrcat -O -o '${fcast_outdir}/${this_field}_${fcast_months[0]}-${fcast_months[-1]}_ensmem_anom.nc' '${ensmem_anom_outfiles[@]} ; echo $command ; $command
	    	done
		i=$((i+1))
	done
fi
