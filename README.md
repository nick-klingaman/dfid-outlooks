# dfid-outlooks
## Code to process data for DFID Climate Outlooks

Reforecasts and operational forecasts are obtained from four models:

| Model | Reforecast ensemble	| Operational ensemble | Method |
| --- | --- | --- | --- |
| Met Office (UKMO)	| 7 members, 9th of month, 1993-2015, downloaded each time | 20 members: 2 members per day, lagged ensemble of last 10  days | From public sector catalogue on JASMIN |
| National Centers for Environmental Prediction	| 4 members, every 5 days, 1982-2011, fixed hindcast set | 24 members: 4 members per day, lagged ensemble of last 6 days | From NCEP HTTP site |
| European Centre for Medium-range Weather Forecasts | 25 members, 1st of month, 1993-2015, downloaded each time | 51 members, 1st of month (but only publicly available on 13th at 12Z) | From Copernicus Data Service (CDS) |
| Meteo-France | 25 members, 1st of month, 1993-2015, downloaded each time | 51 members, 1st of month (but only publicly available on 13th at 12Z) | From Copernicus Data Service (CDS) |

### UKMO
1.	Use “get_ukmo_operational.ksh” to download and process Met Office operational forecasts and re-forecasts from the JASMIN catalogue.
2.	You can download and process the re-forecasts in advance by setting “fcast_get” and “fcast_proc” to zero and “hcast_get” and “hcast_proc” to one.

### NCEP
1.	Use “get_ncep_operational.ksh” to download and process NCEP operational forecasts from the NCEP HTTP site.
2.	Use “get_ncep_hindcasts.ksh” to download and process NCEP re-forecasts from the NCEP HTTP site.  NCEP use a fixed reforecast set, so you need to do this only once per re-forecast set.

### ECMWF and Meteo-France
1.	Use “get_copernicus_dfid.py” to download ECMWF and Meteo-France data from the CDS.
2.	You can download re-forecast data in advance by commenting out the lines that download forecast data, or by setting an “old” forecast date you have already downloaded, as the script will not overwrite files.
3.	Use “organise_and_convert_c3s_hcast.ksh” to process re-forecast data from either model.  The script is designed to convert one re-forecast, so it can be run in parallel as the re-forecast data are downloaded as one GRIB file per year.
4.	Use “organise_and_convert_c3s_fcast.ksh” to process forecast data from either model.
5.	Use “compute_mmeans_anoms_c3.ksh” to process forecast and re-forecast data into monthly means and ensemble means.

NB: The KSH scripts above all take various arguments, which are commented in the code.

## Dependencies

* Python packages:
  * salem (for country averaging): https://salem.readthedocs.io/en/v0.2.3/
  * cf-python (for plotting maps): https://cfpython.bitbucket.io/
  * cf-plot (for plotting maps): http://ajheaps.github.io/cf-plot
* Fortran:
  * fix_netcdf_dimension_monotonic (for overwriting netCDF coordinate variables) - included in repository but needs to be compiled on your system.
