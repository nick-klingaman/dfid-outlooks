def download_c3s(date,centre,system,variable,frequency,outdir):
    import cdsapi
    import os
    c = cdsapi.Client()
    year = date.strftime('%Y')
    month = date.strftime('%m')
    day = date.strftime('%d')
    leadtime_hour=[]
    if frequency == 'daily':
        for i in xrange(216):
            leadtime_hour.append(str(i*24))
    elif frequency == '6hr':
        for i in xrange(215*4+1):
            leadtime_hour.append(str(i*6))
    if date.year < 2018:
        outfile=outdir+'/hindcast_'+year+month+day+'_'+variable+'_'+frequency+'.grib'
    else:
        outfile=outdir+'/operational_'+year+month+day+'_'+variable+'_'+frequency+'.grib'
    if os.path.isfile(outfile) == False:
        print outfile,os.path.isfile(outfile),'Download'
        r = c.retrieve(
        'seasonal-original-single-levels',
        {
            'originating_centre': centre,
            'system': system,
            'variable': variable,
            'year' : year,
            'month' : month,
            'day' : day,       
            'leadtime_hour': leadtime_hour,
            'format': 'grib'
        }, outdir+'/hindcast_'+year+month+day+'_'+variable+'_'+frequency+'.grib')
    else:
        pass
    return

def get_fcst(dates,centre,system,variable,frequency,outdir):
    try:
        for date in dates:
            download_c3s(date,centre,system,variable,frequency,outdir)
    #except:
    #    print 'Fail'
    except:
        download_c3s(dates,centre,system,variable,frequency,outdir)

