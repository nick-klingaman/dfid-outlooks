import dfid_functions as dfid
import salem
import numpy as np
import json
import datetime

if __name__ == '__main__':

    file_desc='dfid_country_forecast_mar2019'

    gpcp_file='/gws/nopw/j04/klingaman/datasets/GPCP/monthly/GPCP_vn2p3.jan-dec_mmeans.1979-2015.n768e.nc'
    eraint_file='/gws/nopw/j04/klingaman/datasets/ERA-INTERIM/T2/ERA_interim.jan1979-sep2017_mmeans.t2m.n768e.nc'
    
    ncep_fcst_dir='/gws/nopw/j04/klingaman/datasets/S2S/NCEP/ENSO/operational_20190306-20190311'
    ncep_hcst_dir='/gws/nopw/j04/klingaman/datasets/S2S/NCEP/ENSO/hindcasts_0307'
    ncep_precip_fcst_file=ncep_fcst_dir+'/precip_201903-201910.anom_hcast_0307.n768e.nc'
    ncep_temp_fcst_file=ncep_fcst_dir+'/t2m_201903-201910.anom_hcast_0307.n768e.nc'
    ncep_precip_hcst_file=ncep_hcst_dir+'/prate.month03-month12_mmeans_ensmean.1982-2011.n768e.nc'
    ncep_temp_hcst_file=ncep_hcst_dir+'/t_1.month03-month12_mmeans_ensmean.1982-2011.n768e.nc'
    ncep_start_month=2 # As offset
    
    ecmwf_fcst_dir='/gws/nopw/j04/klingaman/datasets/S2S/ECMWF/ENSO/operational_20190301'
    ecmwf_hcst_dir='/gws/nopw/j04/klingaman/datasets/S2S/ECMWF/ENSO/hindcasts_0301_2019'
    ecmwf_precip_fcst_file=ecmwf_fcst_dir+'/operational_20190301_total_precipitation_daily_ensmean_anom_201904-201909.n768e.nc'
    ecmwf_temp_fcst_file=ecmwf_fcst_dir+'/operational_20190301_2m_temperature_6hr_ensmean_anom_201904-201909.n768e.nc'
    ecmwf_precip_hcst_file=ecmwf_hcst_dir+'/hindcast_0301_total_precipitation_daily_mmeans_ensmeans.n768e.nc'
    ecmwf_temp_hcst_file=ecmwf_hcst_dir+'/hindcast_0301_2m_temperature_6hr_mmeans_ensmeans.n768e.nc'
    ecmwf_start_month=3
    
    metfr_fcst_dir='/gws/nopw/j04/klingaman/datasets/S2S/METEOFRANCE/ENSO/operational_20190301'
    metfr_hcst_dir='/gws/nopw/j04/klingaman/datasets/S2S/METEOFRANCE/ENSO/hindcasts_0301_2019'
    metfr_precip_fcst_file=metfr_fcst_dir+'/operational_20190301_total_precipitation_daily_ensmean_anom_201904-201909.n768e.nc'
    metfr_temp_fcst_file=metfr_fcst_dir+'/operational_20190301_2m_temperature_6hr_ensmean_anom_201904-201909.n768e.nc'
    metfr_precip_hcst_file=metfr_hcst_dir+'/hindcast_0301_total_precipitation_daily_mmeans_ensmeans.n768e.nc'
    metfr_temp_hcst_file=metfr_hcst_dir+'/hindcast_0301_2m_temperature_6hr_mmeans_ensmeans.n768e.nc'
    metfr_start_month=3

    ukmo_fcst_dir='/gws/nopw/j04/klingaman/datasets/S2S/UKMO/ENSO/operational_20190302-20190311'
    ukmo_hcst_dir='/gws/nopw/j04/klingaman/datasets/S2S/UKMO/ENSO/hindcasts_0309_2019'
    ukmo_precip_fcst_file=ukmo_fcst_dir+'/pr_201904-201909_ensmean_anom.n768e.nc'
    ukmo_temp_fcst_file=ukmo_fcst_dir+'/tas_201904-201909_ensmean_anom.n768e.nc'
    ukmo_precip_hcst_file=ukmo_hcst_dir+'/pr_0309_201904-201909_ensmeans.n768e.nc'
    ukmo_temp_hcst_file=ukmo_hcst_dir+'/tas_0309_201904-201909_ensmeans.n768e.nc'
    ukmo_start_month=3
    
    nov = dfid.build_season_dictionary('nov','November',[10],[31])
    dec = dfid.build_season_dictionary('dec','December',[11],[31])
    jan = dfid.build_season_dictionary('jan','January',[0],[31])
    feb = dfid.build_season_dictionary('feb','February',[1],[28])
    mar = dfid.build_season_dictionary('mar','March',[2],[31])
    apr = dfid.build_season_dictionary('apr','April',[3],[30])
    may = dfid.build_season_dictionary('may','May',[4],[31])
    jun = dfid.build_season_dictionary('jun','June',[5],[30])
    jul = dfid.build_season_dictionary('jul','July',[6],[31])
    aug = dfid.build_season_dictionary('aug','August',[7],[31])
    sep = dfid.build_season_dictionary('sep','September',[8],[30])
    otb = dfid.build_season_dictionary('oct','October',[9],[31])
    ndj = dfid.build_season_dictionary('ndj','November-January',[10,11,12],[30,31,31])
    djf = dfid.build_season_dictionary('djf','December-February',[11,12,13],[31,31,31])
    jfm = dfid.build_season_dictionary('jfm','January-March',[0,1,2],[31,28,31])
    fma = dfid.build_season_dictionary('fma','February-April',[1,2,3],[28,31,30])
    mam = dfid.build_season_dictionary('mam','March-May',[2,3,4],[31,30,31])
    amj = dfid.build_season_dictionary('amj','April-June',[3,4,5],[30,31,30])
    mjj = dfid.build_season_dictionary('mjj','May-July',[4,5,6],[31,30,31])
    jja = dfid.build_season_dictionary('jja','June-August',[5,6,7],[30,31,31])
    jas = dfid.build_season_dictionary('jas','July-September',[6,7,8],[31,31,30])
    aso = dfid.build_season_dictionary('aso','August-October',[7,8,9],[31,30,31])
    son = dfid.build_season_dictionary('son','September-November',[8,9,10],[30,31,30])
    ond = dfid.build_season_dictionary('ond','October-December',[9,10,11],[31,30,31])
 
    #list_of_seasons = [apr,amj,jas]
    list_of_seasons = [jas]

    gpcp_ds = salem.open_xr_dataset(gpcp_file)
    eraint_ds = salem.open_xr_dataset(eraint_file)
    
    precip_cat={}
    precip_cat['lowest']='Very Dry'
    precip_cat['low']='Dry'
    precip_cat['avg']='Average'
    precip_cat['high']='Wet'
    precip_cat['highest']='Very Wet'
    
    temp_cat={}
    temp_cat['lowest']='Cold'
    temp_cat['low']='Cool'
    temp_cat['avg']='Average'
    temp_cat['high']='Warm'
    temp_cat['highest']='Hot'        

    soil_cat=precip_cat

    list_of_regions = dfid.region_list()

    for season in list_of_seasons:
        print '>',season['long']
        list_of_models=['NCEP','UKMO','ECMWF','METFR']
        nmodels=len(list_of_models)
        fpj=open(file_desc+'_precip.'+season['short']+'.json','wb')
        ftj=open(file_desc+'_temp.'+season['short']+'.json','wb')
        fpa=open(file_desc+'_precip.'+season['short']+'.txt','wb')
        fta=open(file_desc+'_temp.'+season['short']+'.txt','wb')
        fpa.write('Forecast issued on 13 March 2019 \n\n')
        fpa.write('Forecast precipitation for '+season['long']+' in mm \n\n')
        fta.write('Forecast issued on 13 March 2019 \n\n')
        fta.write('Forecast temperature for '+season['long']+' in degC \n\n')
        for region in list_of_regions:
            list_of_countries = dfid.country_list(region=region)            
            region_dict = dfid.build_region_dictionary(region,list_of_countries)
            print '-->',region_dict['region_name']
            print '----> Precipitation'
            format_string = dfid.forecast_print_header(fpa,region_dict,list_of_models)
            for this_country in list_of_countries:
                print '------>',this_country
                raw_vals=np.zeros(nmodels)
                anoms=np.zeros(nmodels)
                mmm_percentiles=np.zeros(4)
                i=0
                output_string=(this_country,)
                for m in xrange(nmodels):
                    print '-------->',list_of_models[m]
                    if list_of_models[m] == 'NCEP':
                        precip_fcst_ds=salem.open_xr_dataset(ncep_precip_fcst_file)
                        precip_fcst_var=precip_fcst_ds.prate
                        precip_hcst_ds=salem.open_xr_dataset(ncep_precip_hcst_file)
                        precip_hcst_var=precip_hcst_ds.prate
                        start_month=ncep_start_month
                        hcst_nmonths=10
                        scale_factor=86400.0 # mm/sec -> mm/day
                    if list_of_models[m] == 'ECMWF':
                        precip_fcst_ds=salem.open_xr_dataset(ecmwf_precip_fcst_file)
                        precip_fcst_var=precip_fcst_ds.tp
                        precip_hcst_ds=salem.open_xr_dataset(ecmwf_precip_hcst_file)
                        precip_hcst_var=precip_hcst_ds.tp
                        start_month=ecmwf_start_month
                        scale_factor=1000.0 # m/day -> mm/day
                        hcst_nmonths=6
                    if list_of_models[m] == 'METFR':
                        precip_fcst_ds=salem.open_xr_dataset(metfr_precip_fcst_file)
                        precip_fcst_var=precip_fcst_ds.tp
                        precip_hcst_ds=salem.open_xr_dataset(metfr_precip_hcst_file)
                        precip_hcst_var=precip_hcst_ds.tp
                        start_month=metfr_start_month
                        hcst_nmonths=6
                        scale_factor=1000.0 # m/day -> mm/day
                    if list_of_models[m] == 'UKMO':
                        precip_fcst_ds=salem.open_xr_dataset(ukmo_precip_fcst_file)
                        precip_fcst_var=precip_fcst_ds.precipitation_flux
                        precip_hcst_ds=salem.open_xr_dataset(ukmo_precip_hcst_file)
                        precip_hcst_var=precip_hcst_ds.precipitation_flux
                        start_month=ukmo_start_month
                        hcst_months=6
                        scale_factor=86400.0 # mm/sec -> mm/day
                    category,raw_val,anom,hcst_percentiles = dfid.forecast_process_country(precip_fcst_var,precip_hcst_var,gpcp_ds.precip,
                                                                                           this_country,season,start_month,precip_cat,
                                                                                           accum=True,scale_factor=scale_factor,hcst_nmonths=hcst_nmonths)
                    mmm_percentiles = mmm_percentiles + hcst_percentiles/nmodels
                    if raw_val < 0:
                        anom=anom-raw_val
                        raw_val=0.0
                    raw_vals[m]=raw_val
                    anoms[m]=anom
                    output_string=output_string+(category,raw_val,anom)
                mmm_cat_str,mmm_cat_num = dfid.determine_category(np.mean(anoms),mmm_percentiles,precip_cat)
                print mmm_percentiles
                # Place an asterisk next to countries with less than 10 mm/month climatological rainfall
                if raw_vals[0]-anoms[0] < 10 * len(season['months']):
                    mmm_cat_str = mmm_cat_str+'*'
                print mmm_cat_str
                output_string=output_string+(mmm_cat_str,np.mean(raw_vals),np.mean(anoms))                
                print output_string
                fpa.write(format_string % output_string)
                fpa.write('\n')
            print '----> Temperature'
            format_string = dfid.forecast_print_header(fta,region_dict,list_of_models)
            for this_country in list_of_countries:
                print '------>',this_country
                raw_vals=np.zeros(nmodels)
                anoms=np.zeros(nmodels)
                i=0
                output_string=(this_country,)
                mmm_percentiles=np.zeros(4)
                for m in xrange(nmodels):
                    if list_of_models[m] == 'NCEP':
                        temp_fcst_ds=salem.open_xr_dataset(ncep_temp_fcst_file)
                        temp_fcst_var=temp_fcst_ds.t_1
                        temp_hcst_ds=salem.open_xr_dataset(ncep_temp_hcst_file)
                        temp_hcst_var=temp_hcst_ds.t_1
                        start_month=ncep_start_month
                        scale_factor=1
                        hcst_nmonths=10
                    if list_of_models[m] == 'ECMWF':
                        temp_fcst_ds=salem.open_xr_dataset(ecmwf_temp_fcst_file)
                        temp_fcst_var=temp_fcst_ds.t2m
                        temp_hcst_ds=salem.open_xr_dataset(ecmwf_temp_hcst_file)
                        temp_hcst_var=temp_hcst_ds.t2m
                        start_month=ecmwf_start_month
                        scale_factor=1
                        hcst_nmonts=6
                    if list_of_models[m] == 'METFR':
                        temp_fcst_ds=salem.open_xr_dataset(metfr_temp_fcst_file)
                        temp_fcst_var=temp_fcst_ds.t2m
                        temp_hcst_ds=salem.open_xr_dataset(metfr_temp_hcst_file)
                        temp_hcst_var=temp_hcst_ds.p
                        start_month=metfr_start_month
                        scale_factor=1
                        hcst_nmonths=6
                    if list_of_models[m] == 'UKMO':
                        temp_fcst_ds=salem.open_xr_dataset(ukmo_temp_fcst_file)
                        temp_fcst_var=temp_fcst_ds.air_temperature
                        temp_hcst_ds=salem.open_xr_dataset(ukmo_temp_hcst_file)
                        temp_hcst_var=temp_hcst_ds.air_temperature
                        start_month=ukmo_start_month
                        scale_factor=1
                        hcst_nmonths=6
                    category,raw_val,anom,hcst_percentiles = dfid.forecast_process_country(temp_fcst_var,temp_hcst_var,eraint_ds.T2,
                                                                                          this_country,season,start_month,temp_cat,
                                                                                          obs_offset=273.15,hcst_nmonths=hcst_nmonths)
                    mmm_percentiles = mmm_percentiles + hcst_percentiles/nmodels
                    raw_vals[m]=raw_val
                    anoms[m]=anom
                    output_string=output_string+(category,raw_val,anom)
                mmm_cat_str,mmm_cat_num = dfid.determine_category(np.mean(anoms),mmm_percentiles,temp_cat)
                print mmm_percentiles
                output_string=output_string+(mmm_cat_str,np.mean(raw_vals),np.mean(anoms))                
               	print output_string
                fta.write(format_string % output_string)
                fta.write('\n')
            fpa.write('\n\n')
            fta.write('\n\n')
        fta.close()
        fpa.close()
