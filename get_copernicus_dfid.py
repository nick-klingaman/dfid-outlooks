from get_copernicus_c3s import get_fcst
import datetime
import subprocess

# Basedates for forecasts and hindcasts in ECMWF and Meteo-France
fcast_basedate=datetime.datetime(year=2019,month=3,day=1)
hcast_basedate=datetime.datetime(year=2019,month=3,day=1)

# Operational forecasts
centres_systems_basedirs[['meteo_france','6','/mnt/e/METEOFRANCE/ENSO/operational_'+fcast_basedate.strftime('%Y%m%d')],
                         ['ecmwf','5','/mnt/e/ECMWF/ENSO/operational_'+fcast_basedate.strftime('%Y%m%d')]]
vars_freqs=[['total_precipitation','daily'],['2m_temperature','6hr']]   
for centre,system,basedir in centres_systems_basedirs:
    subprocess.call(['mkdir','-p',basedir])
    for var,freq in vars_freqs:
        get_fcst(dates,centre,system,var,freq,basedir)

# Associated hindcasts
hindcast_start_year=1993
hindcast_stop_year=2015
hcast_dates=[]
ukmo_hcast_dates=[]
for year in xrange(hindcast_start_year,hindcast_stop_year+1):
    hcast_dates.append(hcast_basedate.replace(year=year))
    ukmo_hcast_dates.append(ukmo_hcast_basedate.replace(year=year))
centres_systems_basedirs=[
                   ['meteo_france',`6`,'/mnt/e/METEOFRANCE/ENSO/hindcasts_'+hcast_basedate.strftime('%m%d')+'_'+hcast_basedate.strftime('%Y')]]
                   ['ecmwf','5','/mnt/e/ECMWF/ENSO/hindcasts_'+hcast_basedate.strftime('%m%d')+'_'+hcast_basedate.strftime('%Y')]]
for centre,system,basedir in centres_systems_basedirs:
    subprocess.call(['mkdir','-p',basedir])
    for var,freq in vars_freqs:
        get_fcst(dates,centre,system,var,freq,basedir)
