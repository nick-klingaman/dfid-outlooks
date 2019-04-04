import dfid_functions as dfid
import cf,cfplot as cfp
import numpy as np
import iris
import cartopy.feature as cfeature
import cartopy.crs as ccrs
import matplotlib.pyplot as plt

def season_anoms(ts,start_month,season_length,accum=True,ens=True):
    ndpm = [31,28,31,30,31,30,31,31,30,31,30,31]
    clim_month=[]
    nlon = len(ts.coord('longitude').points)
    nlat = len(ts.coord('latitude').points)
    try:
        nyears = len(ts.coord('time').points)//12
        time_name='time'
    except:
        nyears = len(ts.coord('t').points)//1
        time_name='t'
    if nyears == 0:
        nyears=1
        end_time=len(ts.coord(time_name).points)+1
    else:
        end_time=nyears*12
    ndpm_total=0
    print ts
    if ens == True:
        nmem = len(ts.coord('realization').points)
    for month in xrange(start_month,start_month+season_length,1):
        if month >= 12:
            clim_month = month-12
        else:
            clim_month = month
        print month,clim_month
        if month == start_month:
            if ens == True:
                anom = ts[0:nyears,:,:,:].copy(data=np.zeros((nyears,nmem,nlat,nlon)))
            else: 
                anom = ts[0:nyears,:,:].copy(data=np.zeros((nyears,nlat,nlon)))
        if accum == True:
            if ens == True:
                anom.data = anom.data + ts[clim_month:end_time:12,:,:,:].data*ndpm[clim_month]
            else:
                anom.data = anom.data + ts[clim_month:end_time:12,:,:].data*ndpm[clim_month]
        else:
            if ens == True:
                anom.data = anom.data + ts[clim_month:end_time:12,:,:,:].data
            else:
                anom.data = anom.data + ts[clim_month:end_time:12,:,:].data
               
    return(anom)

def add_bounds(cube):
    cube.coord('longitude').guess_bounds()
    cube.coord('latitude').guess_bounds()

def interp_n48(cube,has_bounds=False):
    if has_bounds == False:
        add_bounds(cube)
    latitude = iris.coords.DimCoord(np.linspace(-90,90,49), standard_name='latitude', units='degrees')
    longitude = iris.coords.DimCoord(np.linspace(1.875,358.125,96),standard_name='longitude',units='degrees')
    target_cube = iris.cube.Cube(np.zeros((49,96),np.float32),dim_coords_and_dims=[(latitude,0),(longitude,1)])
    target_cube.coord('longitude').circular=True
    add_bounds(target_cube)
    cube_interp = cube.regrid(target_cube,iris.analysis.AreaWeighted(mdtol=1))
    return(cube_interp)

def compute_probabilities(fcst_anom,hcst_anom,fcst_start_month,hcst_start_month,season_length,var_name,accum=True):
    fcst_seas_anom = season_anoms(fcst_anom,fcst_start_month,season_length,accum=accum,ens=True)
    hcst_seas_anom = season_anoms(hcst_anom,hcst_start_month,season_length,accum=accum,ens=True)
    
    nlon=len(fcst_seas_anom.coord('longitude').points)
    nlat=len(fcst_seas_anom.coord('latitude').points)
    nmem=len(fcst_seas_anom.coord('realization').points)
    category_probs = np.zeros((5,nlat,nlon))
    
    print hcst_seas_anom
    category_dict=dfid.build_category_dictionary(var_name)
    for lat in xrange(nlat):
        for lon in xrange(nlon):
            percentiles,min_mean_med_max = dfid.compute_percentiles(hcst_seas_anom.data[0,:,lat,lon])
            for mem in xrange(nmem):
                category,number = dfid.determine_category(fcst_seas_anom.data[0,mem,lat,lon],percentiles[0],category_dict)
                #print category,number
                category_probs[number,lat,lon]=category_probs[number,lat,lon]+1
                
    category_probs=category_probs/float(nmem)
    return(category_probs)

def plot_region_map(category_prob,lon,lat,prob_levs,clim_prob,var_name,category_name,category_desc,region_name,fcst_start_name,fcst_season_name,fcst_season_desc,first):
    if region_name == 'Africa':
        map_bounds=[-35,-20,45,65]
    if region_name == 'Asia':
        map_bounds=[-15,40,60,180]
    if region_name == 'Global':
        map_bounds=[-90,-180,90,180]

    cfp.setvars(file='dfid_maps_forecast_'+fcst_start_name+'.'+region_name+'_'+var_name+'_'+fcst_season_name+'_'+category_name+'.png',axis_label_fontsize=18,text_fontsize=18)
    cfp.gopen()
    cfp.mapset(lonmin=map_bounds[1],lonmax=map_bounds[3],latmin=map_bounds[0],latmax=map_bounds[2])
    #cfp.plotvars.plot.axis.add_feature(cartopy.feature.BORDERS)
    cfp.plotvars.mymap.readshapefile('/gws/nopw/j04/klingaman/datasets/GADM/ISO3Code_2014','countries')
    cfp.levs(manual=np.array(prob_levs))
    cfp.cscale(cmap='nrl_sirkes',ncols=len(prob_levs)+1)
    cfp.con(f=category_prob,y=lat,x=lon,ptype=1,title='Probability of '+category_desc+' for '+fcst_season_desc,colorbar_title='Forecast probability (climatological probability = '+clim_prob+' )',lines=False,line_labels=False,colorbar=True,blockfill=1)
    cfp.gclose(view=True)
    plt.show()

if __name__ == "__main__":

    forecast_start='mar2019'

    ncep_fcst_dir='/gws/nopw/j04/klingaman/datasets/S2S/NCEP/ENSO/operational_20190306-20190311'
    ncep_precip_fcst_file=ncep_fcst_dir+'/precip_201903-201910_ensmem.nc'
    ncep_temp_fcst_file=ncep_fcst_dir+'/t2m_201903-201910_ensmem.nc'
    ncep_hcst_dir='/gws/nopw/j04/klingaman/datasets/S2S/NCEP/ENSO/hindcasts_0307'
    ncep_precip_hcst_file=ncep_hcst_dir+'/prate.month03-month12_mmeans_ensmem.1982-2011.nc'
    ncep_temp_hcst_file=ncep_hcst_dir+'/t_1.month03-month12_mmeans_ensmem.1982-2011.nc'
    ncep_start_month=2 # Zero-based
    
    ukmo_fcst_dir='/gws/nopw/j04/klingaman/datasets/S2S/UKMO/ENSO/operational_20190302-20190311'
    ukmo_precip_fcst_file=ukmo_fcst_dir+'/pr_201904-201909_ensmem.nc'
    ukmo_temp_fcst_file=ukmo_fcst_dir+'/tas_201904-201909_ensmem.nc'
    ukmo_hcst_dir='/gws/nopw/j04/klingaman/datasets/S2S/UKMO/ENSO/hindcasts_0309_2019'
    ukmo_precip_hcst_file=ukmo_hcst_dir+'/pr_0309_201904-201909_ensmem.nc'
    ukmo_temp_hcst_file=ukmo_hcst_dir+'/tas_0309_201904-201909_ensmem.nc'
    ukmo_start_month=3 # Zero-based

    ecmwf_fcst_dir='/gws/nopw/j04/klingaman/datasets/S2S/ECMWF/ENSO/operational_20190301'
    ecmwf_hcst_dir='/gws/nopw/j04/klingaman/datasets/S2S/ECMWF/ENSO/hindcasts_0301_2019'
    ecmwf_precip_fcst_file=ecmwf_fcst_dir+'/operational_20190301_total_precipitation_daily_ensmem_201904-201909.nc'
    ecmwf_temp_fcst_file=ecmwf_fcst_dir+'/operational_20190301_2m_temperature_6hr_ensmem_201904-201909.nc'
    ecmwf_precip_hcst_file=ecmwf_hcst_dir+'/hindcast_0301_total_precipitation_daily_mmeans_ensmem.nc'
    ecmwf_temp_hcst_file=ecmwf_hcst_dir+'/hindcast_0301_2m_temperature_6hr_mmeans_ensmem.nc'
    ecmwf_start_month=3
    
    metfr_fcst_dir='/gws/nopw/j04/klingaman/datasets/S2S/METEOFRANCE/ENSO/operational_20190301'
    metfr_hcst_dir='/gws/nopw/j04/klingaman/datasets/S2S/METEOFRANCE/ENSO/hindcasts_0301_2019'
    metfr_precip_fcst_file=metfr_fcst_dir+'/operational_20190301_total_precipitation_daily_ensmem_201904-201909.nc'
    metfr_temp_fcst_file=metfr_fcst_dir+'/operational_20190301_2m_temperature_6hr_ensmem_201904-201909.nc'
    metfr_precip_hcst_file=metfr_hcst_dir+'/hindcast_0301_total_precipitation_daily_mmeans_ensmem.nc'
    metfr_temp_hcst_file=metfr_hcst_dir+'/hindcast_0301_2m_temperature_6hr_mmeans_ensmem.nc'
    metfr_start_month=3

    ncep_precip_fcst=iris.load_cube(ncep_precip_fcst_file,'Precipitation rate')*86400.0
    ncep_temp_fcst=iris.load_cube(ncep_temp_fcst_file,'air_temperature')  
    ncep_precip_hcst=iris.load_cube(ncep_precip_hcst_file,'Precipitation rate')*86400.0
    ncep_temp_hcst=iris.load_cube(ncep_temp_hcst_file,'Temperature')

    ukmo_precip_fcst=iris.load_cube(ukmo_precip_fcst_file,'precipitation_flux')*86400.0
    ukmo_temp_fcst=iris.load_cube(ukmo_temp_fcst_file,'air_temperature')
    ukmo_precip_hcst=iris.load_cube(ukmo_precip_hcst_file,'precipitation_flux')*86400.0
    ukmo_temp_hcst=iris.load_cube(ukmo_temp_hcst_file,'air_temperature')

    ecmwf_precip_fcst=iris.load_cube(ecmwf_precip_fcst_file,'Total precipitation')
    ecmwf_temp_fcst=iris.load_cube(ecmwf_temp_fcst_file,'2 metre temperature')  
    ecmwf_precip_hcst=iris.load_cube(ecmwf_precip_hcst_file,'Total precipitation')
    ecmwf_temp_hcst=iris.load_cube(ecmwf_temp_hcst_file,'2 metre temperature')

    metfr_precip_fcst=iris.load_cube(metfr_precip_fcst_file,'Total precipitation')
    metfr_temp_fcst=iris.load_cube(metfr_temp_fcst_file,'2 metre temperature')
    metfr_precip_hcst=iris.load_cube(metfr_precip_hcst_file,'Total precipitation')
    metfr_temp_hcst=iris.load_cube(metfr_temp_hcst_file,'2 metre temperature')

    gpcp_ts_file='/gws/nopw/j04/klingaman/datasets/GPCP/monthly/GPCP_vn2p3.jan-dec_mmeans.1979-2015.nc'
    gpcp_clim_file='/gws/nopw/j04/klingaman/datasets/GPCP/monthly/GPCP_vn2p3.jan-dec_mmean_clim.1981-2010.nc'
    eraint_ts_file='/gws/nopw/j04/klingaman/datasets/ERA-INTERIM/T2/ERA_interim.jan1979-sep2017_mmeans.t2m.nc'
    eraint_clim_file='/gws/nopw/j04/klingaman/datasets/ERA-INTERIM/T2/ERA_interim.jan1979-sep2017_mmean_clim.t2m.nc'
    
#    gpcp_ts=iris.load_cube(gpcp_ts_file,'Average Monthly Rate of Precipitation')
#    gpcp_clim=iris.load_cube(gpcp_clim_file,'Long Term Mean Average Monthly Rate of Precipitation')
#    gpcp_anoms = gpcp_ts.copy()
#    for month in xrange(12):
#        gpcp_anoms.data[month::12,:,:]=gpcp_ts.data[month::12,:,:]-gpcp_clim.data[month,:,:]

#    eraint_ts=iris.load_cube(eraint_ts_file,'air_temperature')
#    eraint_clim=iris.load_cube(eraint_clim_file,'air_temperature')
#    eraint_anoms = eraint_ts.copy()
#    for month in xrange(12):
#        eraint_anoms.data[month::12,:,:]=eraint_ts.data[month::12,:,:]-eraint_clim.data[month,:,:]

    print '--> Interpolating to N48'
    ncep_precip_fcst = interp_n48(ncep_precip_fcst)
    print '----> NCEP precip fcst'
    ncep_temp_fcst = interp_n48(ncep_temp_fcst)
    print '----> NCEP temp fcst'
    ncep_precip_hcst = interp_n48(ncep_precip_hcst)
    print '----> NCEP precip hcst'
    ncep_temp_hcst = interp_n48(ncep_temp_hcst)
    print '----> NCEP temp hcst'

    ukmo_precip_fcst = interp_n48(ukmo_precip_fcst)
    print '----> UKMO precip fcst'
    ukmo_temp_fcst = interp_n48(ukmo_temp_fcst)
    print '----> UKMO temp fcst'
    ukmo_precip_hcst = interp_n48(ukmo_precip_hcst)
    print '----> UKMO precip hcst'
    ukmo_temp_hcst = interp_n48(ukmo_temp_hcst)
    print '----> UKMO temp hcst'

    ecmwf_precip_fcst = interp_n48(ecmwf_precip_fcst)
    print '----> ECMWF precip fcst'
    ecmwf_temp_fcst = interp_n48(ecmwf_temp_fcst)
    print '----> ECMWF temp fcst'
    ecmwf_precip_hcst = interp_n48(ecmwf_precip_hcst)
    print '----> ECMWF precip hcst'
    ecmwf_temp_hcst = interp_n48(ecmwf_temp_hcst)
    print '----> ECMWF temp hcst'

    metfr_precip_fcst = interp_n48(metfr_precip_fcst)
    print '----> METFR precip fcst'
    metfr_temp_fcst = interp_n48(metfr_temp_fcst)
    print '----> METFR temp fcst'
    metfr_precip_hcst = interp_n48(metfr_precip_hcst)
    print '----> METFR precip hcst'
    metfr_temp_hcst = interp_n48(metfr_temp_hcst)
    print '----> METFR temp hcst'

#    seasons=['jas2018','ond2018']
#    season_descs=['July-Sep 2018','Oct-Dec 2018']
#    seasons=['aso2018','ndj2018']
#    season_descs=['Aug-Oct 2018','Nov-Jan 2018-19']
#    seasons=['feb2019','fma2019','mjj2019']
#    season_descs=['Feb 2019','Feb-Apr 2019','May-Jul 2019']
#    seasons=['mar2019','mam2019','jja2019']
#    season_descs=['Mar 2019','Mar-May 2019','Jun-Aug 2019']
    seasons=['apr2019','amj2019','jas2019']
    season_descs=['Apr 2019','Apr-Jun 2019','Jul-Sep 2019']
    ukmo_season_starts=[0,0,3]
    ecmwf_season_starts=[0,0,3]
    metfr_season_starts=[0,0,3]
    ncep_season_starts=[1,1,4]
    season_lengths=[1,3,3]
    nseasons=len(seasons)
    first=True
    for season in xrange(nseasons):
        ncep_temp_cat_probs = compute_probabilities(ncep_temp_fcst,ncep_temp_hcst,ncep_season_starts[season],ncep_season_starts[season],season_lengths[season],'temperature',accum=False)
        ukmo_temp_cat_probs = compute_probabilities(ukmo_temp_fcst,ukmo_temp_hcst,ukmo_season_starts[season],ukmo_season_starts[season],season_lengths[season],'temperature',accum=False)
        ecmwf_temp_cat_probs = compute_probabilities(ecmwf_temp_fcst,ecmwf_temp_hcst,ecmwf_season_starts[season],ecmwf_season_starts[season],season_lengths[season],'temperature',accum=False)
        metfr_temp_cat_probs = compute_probabilities(metfr_temp_fcst,metfr_temp_hcst,metfr_season_starts[season],metfr_season_starts[season],season_lengths[season],'temperature',accum=False)
        #metfr_temp_cat_probs = ecmwf_temp_cat_probs
        ensmean_temp_cat_probs = (ncep_temp_cat_probs+ukmo_temp_cat_probs+metfr_temp_cat_probs+ecmwf_temp_cat_probs)/4.0
#        ensmean_temp_cat_probs = (ncep_temp_cat_probs+ukmo_temp_cat_probs+ecmwf_temp_cat_probs)/3.0
        print np.mean(ncep_temp_cat_probs,axis=(1,2)),np.mean(ukmo_temp_cat_probs,axis=(1,2)),np.mean(ecmwf_temp_cat_probs,axis=(1,2)) #,np.mean(metfr_temp_cat_probs,axis=(1,2))

        ncep_precip_cat_probs = compute_probabilities(ncep_precip_fcst,ncep_precip_hcst,ncep_season_starts[season],ncep_season_starts[season],season_lengths[season],'precipitation',accum=True)
        ukmo_precip_cat_probs = compute_probabilities(ukmo_precip_fcst,ukmo_precip_hcst,ukmo_season_starts[season],ukmo_season_starts[season],season_lengths[season],'precipitation',accum=True)
        ecmwf_precip_cat_probs = compute_probabilities(ecmwf_precip_fcst,ecmwf_precip_hcst,ukmo_season_starts[season],ecmwf_season_starts[season],season_lengths[season],'precipitation',accum=True)
        metfr_precip_cat_probs = compute_probabilities(metfr_precip_fcst,metfr_precip_hcst,metfr_season_starts[season],metfr_season_starts[season],season_lengths[season],'precipitation',accum=True)
        #metfr_precip_cat_probs = ecmwf_precip_cat_probs
        ensmean_precip_cat_probs = (ncep_precip_cat_probs+ukmo_precip_cat_probs+ecmwf_precip_cat_probs+metfr_precip_cat_probs)/4.0
        #ensmean_precip_cat_probs = (ncep_precip_cat_probs+ukmo_precip_cat_probs+ecmwf_precip_cat_probs)/3.0
        print np.mean(ncep_precip_cat_probs,axis=(1,2)),np.mean(ukmo_precip_cat_probs,axis=(1,2)),np.mean(ecmwf_precip_cat_probs,axis=(1,2)) #,np.mean(metfr_precip_cat_probs,axis=(1,2))


        quartile_prob_levs=[0.05,0.1,0.2,0.3,0.5,0.8]
        half_prob_levs=[0.1,0.2,0.4,0.6,0.8,0.9]

        regions=['Global','Africa','Asia']
        for region in regions:
            plot_region_map(ensmean_precip_cat_probs[0,:,:]+ensmean_precip_cat_probs[1,:,:],ncep_precip_fcst.coord('longitude').points,ncep_precip_fcst.coord('latitude').points,quartile_prob_levs,
                            '0.25','precipitation','dry-or-verydry','Dry or Very Dry',region,forecast_start,seasons[season],season_descs[season],first)
            plot_region_map(ensmean_precip_cat_probs[3,:,:]+ensmean_precip_cat_probs[4,:,:],ncep_precip_fcst.coord('longitude').points,ncep_precip_fcst.coord('latitude').points,quartile_prob_levs,
                            '0.25','precipitation','wet-or-verywet','Wet or Very Wet',region,forecast_start,seasons[season],season_descs[season],first)
            plot_region_map(ensmean_precip_cat_probs[2,:,:],ncep_precip_fcst.coord('longitude').points,ncep_precip_fcst.coord('latitude').points,half_prob_levs,
                            '0.5','precipitation','average','Average',region,forecast_start,seasons[season],season_descs[season],first)
            
            plot_region_map(ensmean_temp_cat_probs[0,:,:]+ensmean_temp_cat_probs[1,:,:],ncep_precip_fcst.coord('longitude').points,ncep_precip_fcst.coord('latitude').points,quartile_prob_levs,
                            '0.25','temperature','cool-or-cold','Cool or Cold',region,forecast_start,seasons[season],season_descs[season],first)
            plot_region_map(ensmean_temp_cat_probs[3,:,:]+ensmean_temp_cat_probs[4,:,:],ncep_precip_fcst.coord('longitude').points,ncep_precip_fcst.coord('latitude').points,quartile_prob_levs,
                            '0.25','temperature','warm-or-hot','Warm or Hot',region,forecast_start,seasons[season],season_descs[season],first)
            plot_region_map(ensmean_temp_cat_probs[2,:,:],ncep_precip_fcst.coord('longitude').points,ncep_precip_fcst.coord('latitude').points,half_prob_levs,
                            '0.5','temperature','average','Average',region,forecast_start,seasons[season],season_descs[season],first)
