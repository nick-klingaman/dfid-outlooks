import salem
import numpy as np
import json

def forecast_print_header(f,region_dict,models):
    f.write('Region: '+region_dict['region_name']+' \n')
    format_string='%-40s'
    data_string='%-40s'
    label_string=()
    model_string='%-41s'
    model_list=(' ',)
    models_extend = models+['Multi-model mean']
    label_string=label_string+('Country',)
    for x in xrange(len(models_extend)):
        format_string=format_string+' %8s %8s %8s '
        data_string=data_string+' %8s %8.2f %9.2f '
        label_string=label_string+(' Category', 'Raw Val',' Anomaly')
        model_string=model_string+' %-28s'
        model_list=model_list+(models_extend[x],)
    f.write(model_string % model_list)
    f.write('\n')
    f.write(format_string % label_string)
    f.write('\n -------------- \n')
    return data_string

def determine_category(fcst_aavg,obs_percentiles,cat_dict):
    if fcst_aavg < obs_percentiles[3]:
        fcst_cat=cat_dict['lowest']
        number=0
    elif fcst_aavg < obs_percentiles[2]:
        fcst_cat=cat_dict['low']
        number=1
    elif fcst_aavg > obs_percentiles[0]:
        fcst_cat=cat_dict['highest']
        number=4
    elif fcst_aavg > obs_percentiles[1]:
        fcst_cat=cat_dict['high']
        number=3
    else:
        fcst_cat=cat_dict['avg']
        number=2
    return fcst_cat,number

def forecast_process_country(fcst_var,hcast_var,obs_var,country,season_dict,start_month,cat_dict,accum=False,scale_factor=1,obs_offset=0,hcst_nmonths=0):
    fcst_aavg = country_avg(fcst_var,country,season_dict['months'],season_dict['ndays'],1,forecast=True,offset=start_month)*scale_factor
    hcst_aavg = country_avg(hcast_var,country,season_dict['months'],season_dict['ndays'],1,forecast=False,offset=start_month,nmonths_per_year=hcst_nmonths)*scale_factor
    obs_aavg = country_avg(obs_var,country,season_dict['months'],season_dict['ndays'],1,forecast=False,offset=0,nmonths_per_year=12)
    if accum:
        fcst_aavg = fcst_aavg*np.sum(season_dict['ndays'])
        hcst_aavg = hcst_aavg*np.sum(season_dict['ndays'])
        obs_aavg = obs_aavg*np.sum(season_dict['ndays'])-obs_offset
    else:
        obs_aavg = obs_aavg-obs_offset
    hcst_percentiles, hcst_min_mean_med_max = compute_percentiles(hcst_aavg)
    obs_percentiles, obs_min_mean_med_max = compute_percentiles(obs_aavg)
    print hcst_percentiles,obs_percentiles
    # Convert percentiles to anomalies from mean, to agree with forecast data (anomalies from reforecast climatology)
    hcst_percentiles = hcst_percentiles[0][:]-hcst_min_mean_med_max[0][1]
    print hcst_percentiles,fcst_aavg
    fcst_cat,fcst_num = determine_category(fcst_aavg,hcst_percentiles,cat_dict)
    return fcst_cat,obs_min_mean_med_max[0][1]+fcst_aavg,fcst_aavg,hcst_percentiles

def country_avg(dataset,country,months,ndays_per_month,margin,forecast=False,offset=0,nmonths_per_year=12):
    # Compute a country-wide area average of input dataset for specified months, using Natural Earth shapefile
    shapefile='/gws/nopw/j04/klingaman/datasets/NATURAL_EARTH/ne_110m_admin_0_countries.shp'
    shdf = salem.read_shapefile(shapefile)
    shdf = shdf.loc[shdf['admin'].isin([country])]
    
    try:
        nx = len(dataset.coords['lon'])
        lon_name='lon'
    except:
        nx = len(dataset.coords['longitude'])
        lon_name='longitude'
    try:
        ny = len(dataset.coords['lat'])
        lat_name='lat'
    except:
        ny = len(dataset.coords['latitude'])
        lat_name='latitude'
    try:
        nt = len(dataset.coords['time'])
    except:
        try:
            nt = len(dataset.coords['t'])
        except:
            nt = 1
    if forecast:
        nyr = 1
    else:
        nyr = nt//nmonths_per_year
    
    if np.amax(months) >= nmonths_per_year and nyr > 1:
	    my_nyr=nyr-1
    else:
	    my_nyr=nyr
    #print my_nyr
    nmonths = np.size(months)
    data_sub_aavg = np.zeros(my_nyr)
    for month in xrange(nmonths):
	if forecast or my_nyr==0:
            if nmonths > 1: 
                my_t = months[month] - offset
            else:
                my_t = months[0] - offset
            if my_t < 0:
                my_t = my_t + nmonths_per_year
        else:
            if nmonths > 1:
                my_t = np.arange(my_nyr)*nmonths_per_year+months[month]-offset
            else:
                my_t = np.arange(my_nyr)*nmonths_per_year+months-offset
        #print my_t
        try:
            month_sub = dataset.isel(time=my_t)
            threed_flag = True
        except:
            try:
                month_sub = dataset.isel(t=my_t)
                threed_flag = True
            except:
                month_sub = dataset
                threed_flag = False
        try:
            month_sub = month_sub.isel(surface=0)
        except:
            month_sub = month_sub
        try:
            month_sub = month_sub.salem.subset(shape=shdf)
            month_sub = month_sub.salem.roi(shape=shdf)
        except ValueError:
            month_sub = month_sub.salem.subset(corners=((shdf.min_x,shdf.min_y),(shdf.max_x,shdf.max_y)))
        if forecast or nyr == 0:
            threed_flag=False

        cntry_nx = len(month_sub.coords[lon_name])
        cntry_ny = len(month_sub.coords[lat_name])
        weights = np.empty((cntry_ny,cntry_nx))
	for x in xrange(cntry_nx):
            for y in xrange(cntry_ny):
                if threed_flag:
                    if not np.isnan(month_sub.values[0,y,x]):
                        weights[y,x] = np.cos(month_sub.coords[lat_name].values[y]*np.pi/180.)
                    else:
                        weights[y,x] = np.nan
                else:
                    if not np.isnan(month_sub.values[y,x]):
                        weights[y,x] = np.cos(month_sub.coords[lat_name].values[y]*np.pi/180.)
                    else:
                        weights[y,x] = np.nan
        weights = weights/np.nansum(weights)
        if threed_flag: # or nmonths > 1:
            axes=(1,2)
        else:
            axes=(0,1)
        if np.size(months) > 1:
	    if threed_flag:
		data_sub_aavg = data_sub_aavg + np.nansum(month_sub.values*weights,axis=axes)*ndays_per_month[month]
	    else:
            	data_sub_aavg = data_sub_aavg + np.nansum(month_sub.values*weights,axis=axes)*ndays_per_month[month]
        else:
            data_sub_aavg = np.nansum(month_sub.values*weights,axis=axes)*ndays_per_month[0]
    
    data_sub_aavg = data_sub_aavg/float(np.sum(ndays_per_month))
    return data_sub_aavg

def compute_percentiles(ts):
    ts_sorted = np.sort(ts)
    nt = len(ts_sorted)
    percentiles = [[ts_sorted[np.int(np.ceil(nt*9//10))],ts_sorted[np.int(np.ceil(nt*3//4))],ts_sorted[np.int(np.floor(nt//4))],ts_sorted[np.int(np.floor(nt//10))]]]
    min_mean_med_max = [[np.min(ts),np.mean(ts),np.median(ts),np.max(ts)]]
    return percentiles,min_mean_med_max

def process_region(region_dict,season_dict,fpj,ftj,fpa,fta):
    fpa.write('Region: '+region_dict['region_name']+' \n')
    fpa.write('%-40s %7s %7s %7s %7s %7s %7s %7s %7s \n' % ('Country','90th ','75th ','25th ','10th ','Min ','Mean ','Median ','Max '))
    fpa.write('-------------- \n')
    fta.write('Region: '+region_dict['region_name']+' \n')
    fta.write('%-40s %7s %7s %7s %7s %7s %7s %7s %7s \n' % ('Country','90th ','75th ','25th ','10th ','Min ','Mean ','Median ','Max '))
    fta.write('-------------- \n')
    for country in sorted(region_dict['countries']):
        print '---->',country
        precip_aavg = country_avg(gpcp_ds.precip,gpcp_ds,country,season_dict['months'],season_dict['ndays'],1)*np.sum(season_dict['ndays'])
        temp_aavg = country_avg(eraint_ds.T2,eraint_ds,country,season_dict['months'],season_dict['ndays'],1)-273.15
        precip_percentiles,precip_4m = compute_percentiles(precip_aavg)
        temp_percentiles,temp_4m = compute_percentiles(temp_aavg)
        json.dump(country,fpj)
        json.dump(precip_percentiles,fpj)
        json.dump(precip_4m,fpj)
        json.dump(country,ftj)
        json.dump(temp_percentiles,ftj)
        json.dump(temp_4m,fpj)
        fpa.write('%-40s' % country)
        fta.write('%-40s' % country)
        np.savetxt(fpa,np.column_stack((precip_percentiles,precip_4m)),fmt=['%7.2f','%7.2f','%7.2f','%7.2f','%7.2f','%7.2f','%7.2f','%7.2f'])
        np.savetxt(fta,np.column_stack((temp_percentiles,temp_4m)),fmt=['%7.2f','%7.2f','%7.2f','%7.2f','%7.2f','%7.2f','%7.2f','%7.2f'])

def build_category_dictionary(variable):
    if variable == 'precipitation':
        category_dict={}
        category_dict['lowest']='Very Dry'
        category_dict['low']='Dry'
        category_dict['avg']='Average'
        category_dict['high']='Wet'
        category_dict['highest']='Very Wet'
    elif variable == 'temperature':
        category_dict={}
        category_dict['lowest']='Cold'
        category_dict['low']='Cool'
        category_dict['avg']='Average'
        category_dict['high']='Warm'
        category_dict['highest']='Very Warm'
    return(category_dict)

def build_season_dictionary(short_name,long_name,months,ndays):
    season_dict={}
    season_dict['short']=short_name
    season_dict['long']=long_name
    season_dict['months']=months
    season_dict['ndays']=ndays
    return season_dict

def build_region_dictionary(region_name,countries):
    region_dict={}
    region_dict['region_name']=region_name
    region_dict['countries']=countries
    return region_dict

def region_list():
    list_of_regions=['Southern Africa',
                     'Western Africa',
                     'Eastern Africa',
                     'Central Africa',
                     'Southern Asia',
                     'Southeastern Asia',
                     'Maritime Continent',
                     'Middle East and North Africa',
                     'Caribbean',
                     'Central Asia']
                     
    return list_of_regions

def country_list(region=None):
    if region == 'Southern Africa':
        countries=['South Africa',
                   'Mozambique',
                   'Malawi',
                   'Zambia',
                   'Zimbabwe',
		   'Madagascar']
    elif region == 'Western Africa':
        countries=['Nigeria',
                   'Ghana',
                   'Sierra Leone',
                   'Liberia',
                   'Cameroon',
                   'Mali']
    elif region == 'Eastern Africa':
        countries=['Ethiopia',
                   'South Sudan',
                   'Kenya',
                   'Uganda',
                   'Somalia',
                   'Sudan',
                   'United Republic of Tanzania',
                   'Rwanda']
    elif region == 'Central Africa':
        countries=['Democratic Republic of the Congo',
                   'Chad',
                   'Niger']
    elif region == 'Southern Asia':
        countries=['India',
                   'Pakistan',
                   'Bangladesh',
                   'Nepal']
    elif region == 'Southeastern Asia':
        countries=['China',
                   'Vietnam',
                   'Myanmar']
    elif region == 'Maritime Continent':
        countries=['Indonesia','Papua New Guinea']
    elif region == 'Middle East and North Africa':
        countries=['Libya',
                   'Egypt',
                   'Algeria',
                   'Lebanon',
                   'Jordan',
                   'Palestine',
                   'Syria',
                   'Iraq',
                   'Yemen',
                   'Morocco',
                   'Tunisia',
                   'Turkey',
                   'Eritrea',
                   'Mauritania']
    elif region == 'Caribbean':
        countries=['Guyana','Haiti']
    elif region == 'Central Asia':
        countries=['Afghanistan','Tajikistan','Kyrgyzstan']
                   
                   
    if region is None:
        # Load full list of countries
        countries=['Ethiopia',
                   'Pakistan',
                   'India',
                   'Nigeria',
                   'Bangladesh',
                   'Afghanistan',
                   'United Republic of Tanzania',
                   'Democratic Republic of the Congo',
                   'Malawi',
                   'South Sudan',
                   'Kenya',
                   'Palestine',
                   'Uganda',
                   'Zimbabwe',
                   'Somalia',
                   'Mozambique',
                   'South Africa',
                   'Rwanda',
                   'Yemen',
                   'Nepal',
                   'Sierra Leone',
                   'Sudan',
                   'Zambia',
                   'Ghana',
                   'Tajikistan',
                   'Liberia',
                   'Kyrgyzstan',
                   'Vietnam',
                   'Indonesia',
                   'China',
                   'Guyana',
                   'Morocco',
                   'Algeria',
                   'Tunisia',
                   'Libya',
                   'Egypt',
                   'Syria',
                   'Yemen',
                   'Iraq',
                   'Turkey',
                   'Lebanon',
                   'Jordan',
                   'Eritrea',
                   'Mauritania',
                   'Chad',
                   'Niger',
                   'Cameroon',
                   'Mali',
                   'Haiti',
                   'Myanmar',
                   'Papua New Guinea']
    return countries
