#!/usr/bin/env python
# coding: utf-8
# Downloads HRRR and produces inputs for SNOWPACK model
#
# Will acquire real-time data if nothing is passed to script on command line
# Use YYYY-MM-DD HH:00:00 after script name to acquire specific model run
#
# Requires python 3.9
# Jim Steenburgh 16 March 2024
#
import numpy as np
import pandas as pd
import xarray as xr
import boto3
from botocore import UNSIGNED
from botocore.client import Config
import datetime
import os
import requests
import sys
import glob
from metpy.units import units
from metpy.calc import wind_direction
import cfgrib
from scipy import spatial
from sklearn.neighbors import KDTree
from multiprocessing import Pool
from platform import python_version
import warnings
warnings.filterwarnings('ignore')

# ------------------ USER SPECIFIED VARIABLES ----------------

# serial or parallel?
processing = 'parallel'

# Maximum number of parallel processes if being run parallel
maxprocesses = 25

# Scratch directory to temporarily store HRRR grib files
scratchdir = '/scratch/general/nfs1/u0028395/snowpack-hrrr/'

# Station coordinates (currently Atwater based on google maps)
sitelat = 40.591230
sitelon = -111.637711

# ---------------- END USER SPECIFIED VARIABLES ----------------

# Downloads HRRR from AWS, identifies or calculates needed variables, and finds values for closest grid point to station coordinates
# Use grib_ls <gribfilename> on the commandline on the linux system for complete list of shortName, typeOfLevel, etc.
def processhrrr (yr, mn, dy, hr, fhr):

    # File names and URLs on AWS, Nomads, and local disk
    serverfile = 'hrrr.t'+str(hr)+'z.wrfprsf'+str(fhr).zfill(2)+'.grib2'
    localfile = scratchdir+str(yr)+str(mn)+str(dy)+str(hr)+'F'+str(fhr).zfill(2)+'hrrr.grib2'
    awsbucket_name = 'noaa-hrrr-bdp-pds'
    awsobject_key = 'hrrr.'+str(yr)+str(mn)+str(dy)+'/conus/'+serverfile

    # boto3 settings
    s3 = boto3.client('s3',
                      config=Config(                                                                                                                            
                          signature_version=UNSIGNED,  # Specify your signature version if required                                                             
                          connect_timeout=5,           # Set connection timeout to 5 seconds                                                                             
                          read_timeout=30,             # Set read timeout to 30 seconds                                                                                 
                          retries={                                                                                                                             
                              'max_attempts': 3,       # Set the maximum number of retry attempts                                                                    
                              'mode': 'standard'       # Use the standard retry mode                                                                                
                          }                                                                                                                                     
                      ))


    # Try to retrieve from AWS
    awsraise = 0
    try:                                                                                                                                                        
        s3.head_object(Bucket=awsbucket_name, Key=awsobject_key)                                                                                                
    except:
        awsraise = 1

    if awsraise == 0:
        s3.download_file(awsbucket_name,awsobject_key,localfile)
    else:
        print(serverfile+' not available')
        raise

    # Load needed variables and rename to something less obtuse if needed
    # Surface elevation (m)
    orog_in = xr.open_dataset(localfile, decode_coords='all', engine='cfgrib', filter_by_keys={'stepType': 'instant','typeOfLevel': 'surface', 'shortName':'orog'}) 

    # Grid-relative 10-m u wind (m/s)
    u10m_in = xr.open_dataset(localfile, engine='cfgrib', filter_by_keys={'typeOfLevel': 'heightAboveGround',
                                        'stepType':'instant', 'shortName':'10u'}).rename({'u10':'u10m'})
    
    # Grid-relative 10-m v wind (m/s)
    v10m_in = xr.open_dataset(localfile, engine='cfgrib', filter_by_keys={'typeOfLevel': 'heightAboveGround',
                                        'stepType':'instant', 'shortName':'10v'}).rename({'v10':'v10m'}) 
    
    # 2-m temperature (K)
    t2m_in = xr.open_dataset(localfile, engine='cfgrib', filter_by_keys={'typeOfLevel': 'heightAboveGround',
                                        'stepType':'instant', 'shortName':'2t'})   # Variable name is t2m in dataset

    # 2-m rh (%)                                                                                                                                                                                                                                               
    rh2m_in = xr.open_dataset(localfile, engine='cfgrib', filter_by_keys={'typeOfLevel': 'heightAboveGround',
                                        'stepType':'instant', 'shortName':'2r'}).rename({'r2':'rh2m'})
    
    # Surface temperature (K)
    tsfc_in = xr.open_dataset(localfile, engine='cfgrib', filter_by_keys={'typeOfLevel': 'surface',
                                        'stepType':'instant', 'shortName':'t'}).rename({'t':'tsfc'}) 
    
    # Surface pressure (Pa)
    psfc_in = xr.open_dataset(localfile, engine='cfgrib', filter_by_keys={'typeOfLevel': 'surface',
                                        'stepType':'instant', 'shortName':'sp'}).rename({'sp':'psfc'}) 
    
    # Surface downward shortwave radiation (W/m**2)
    dswrf_in = xr.open_dataset(localfile, engine='cfgrib', filter_by_keys={'typeOfLevel': 'surface',
                                        'stepType':'instant', 'shortName':'dswrf'})

    # Surface upward shortwave radiation (W/m**2)
    uswrf_in = xr.open_dataset(localfile, engine='cfgrib', filter_by_keys={'typeOfLevel': 'surface',
                                        'stepType':'instant', 'shortName':'uswrf'})

    # Surface downward longwave radiation (W/m**2)
    dlwrf_in = xr.open_dataset(localfile, engine='cfgrib', filter_by_keys={'typeOfLevel': 'surface',
                                        'stepType':'instant', 'shortName':'dlwrf'})
    
    # Surface upward longwave radiation (W/m**2)
    ulwrf_in = xr.open_dataset(localfile, engine='cfgrib', filter_by_keys={'typeOfLevel': 'surface',
                                        'stepType':'instant', 'shortName':'ulwrf'})
    
    # Grid-relative pressure-level u wind (m/s) 
    upress_in = xr.open_dataset(localfile, decode_coords='all', engine='cfgrib', filter_by_keys={'typeOfLevel': 'isobaricInhPa',
                                        'stepType':'instant', 'shortName':'u'}).rename({'u':'upress'}) 

    # Grid-relative pressure-level v wind (m/s)
    vpress_in = xr.open_dataset(localfile, decode_coords='all', engine='cfgrib', filter_by_keys={'typeOfLevel': 'isobaricInhPa',
                                        'stepType':'instant', 'shortName':'v'}).rename({'v':'vpress'}) 
    
    # Pressure-level geopotential height (m)
    hpress_in = xr.open_dataset(localfile, decode_coords='all', engine='cfgrib', filter_by_keys={'typeOfLevel': 'isobaricInhPa',
                                        'stepType':'instant', 'shortName':'gh'}).rename({'gh':'hpress'}) 

    # Pressure level temperature 
    tpress_in = xr.open_dataset(localfile, decode_coords='all', engine='cfgrib', filter_by_keys={'typeOfLevel': 'isobaricInhPa',
                                        'stepType':'instant', 'shortName':'t'}).rename({'t':'tpress'}) 

    # Hourly water-equivalent precipitation (kg/m**2 which is same as mm)
    if fhr == 0:
        hourly_precip_in = xr.open_dataset(localfile, engine='cfgrib', filter_by_keys={
                                        'stepRange': str(fhr), 'shortName':'tp'}).rename({'tp' : 'hourlyprecip'})
    else:
        prev_fhr = fhr - 1
        hourly_precip_in = xr.open_dataset(localfile, engine='cfgrib', filter_by_keys={
                                        'stepRange': str(prev_fhr)+'-'+str(fhr), 'shortName':'tp'}).rename({'tp' : 'hourlyprecip'})

    # Merge into single dataset
    hrrrdata = xr.merge([orog_in,u10m_in,v10m_in,t2m_in,rh2m_in, tsfc_in, psfc_in, dswrf_in,uswrf_in,dlwrf_in,ulwrf_in,upress_in,vpress_in,hpress_in,tpress_in,hourly_precip_in], compat='override') 

    # Calculate 10-m wind speed
    hrrrdata = hrrrdata.assign(wspd10m=(hrrrdata['u10m']**2 + hrrrdata['v10m']**2)**0.5)

    # Calculate pressure level wind speeds
    hrrrdata = hrrrdata.assign(wspdpress=(hrrrdata['upress']**2 + hrrrdata['vpress']**2)**0.5)
    
    # Grid rotate winds and store as u10mearth and v10mearth (see https://rapidrefresh.noaa.gov/faq/HRRR.faq.html)
    rotcon, lonp, latp =.0622515, -97.5, 38.5
    angle2 = rotcon*(hrrrdata['latitude'] - lonp)*0.017453
    sinx2, cosx2 = np.sin(angle2), np.cos(angle2)
    hrrrdata = hrrrdata.assign(u10m_er=cosx2*hrrrdata['u10m']+sinx2*hrrrdata['v10m'])
    hrrrdata = hrrrdata.assign(v10m_er=-sinx2*hrrrdata['u10m']+cosx2*hrrrdata['v10m'])
    hrrrdata.u10m_er.attrs['units'] = 'm s**-1'
    hrrrdata.v10m_er.attrs['units'] = 'm s**-1'

    # Calculate wind direction
    hrrrdata = hrrrdata.assign(wdir10m=wind_direction(hrrrdata['u10m_er'],hrrrdata['v10m_er']))
    
    # Get data for gridpoint closest to station coordinates and save in dataframe
    # See https://github.com/blaylockbk/pyBKB_v3/blob/master/demo/KDTree_nearest_neighbor.ipynb
    lons = hrrrdata.longitude.values
    lats = hrrrdata.latitude.values
    tree = spatial.KDTree(np.column_stack([lons.ravel(), lats.ravel()]))
    point = np.array([sitelon+360., sitelat])  # Add 360 to match HRRR lons 
    dist, idx = tree.query(point)
    y,x = np.unravel_index(idx,lons.shape)
    yyyymmddhh = str(yr)+str(mn)+str(dy)+str(hr)
    ifhr = int(fhr)
    nearest_lat = np.round(hrrrdata.latitude.isel(x=x,y=y).values,3)   
    nearest_lon = np.round(hrrrdata.longitude.isel(x=x,y=y).values - 360.,3)  # Subtract 360 for negative wes lon
    nearest_elev = np.round(hrrrdata.orog.isel(x=x,y=y).values,3)
    nearest_psfc = np.round(hrrrdata.psfc.isel(x=x,y=y).values,3)
    nearest_tsfc = np.round(hrrrdata.tsfc.isel(x=x,y=y).values,3)
    nearest_t2m = np.round(hrrrdata.t2m.isel(x=x,y=y).values,3)
    nearest_rh2m = np.round(hrrrdata.rh2m.isel(x=x,y=y).values,3)
    nearest_wspd10m = np.round(hrrrdata.wspd10m.isel(x=x,y=y).values,3)
    nearest_wdir10m = np.round(hrrrdata.wdir10m.isel(x=x,y=y).values,3)
    nearest_dswrf = np.round(hrrrdata.dswrf.isel(x=x,y=y).values,3)
    nearest_uswrf = np.round(hrrrdata.uswrf.isel(x=x,y=y).values,3)
    nearest_dlwrf = np.round(hrrrdata.dlwrf.isel(x=x,y=y).values,3)
    nearest_ulwrf = np.round(hrrrdata.ulwrf.isel(x=x,y=y).values,3)
    nearest_hourlyprecip = np.round(hrrrdata.hourlyprecip.isel(x=x,y=y).values,3)

    # SLR and snow coming soon...for now NaNs
    nearest_slr = np.nan
    nearest_snow = np.nan 
##    T05K, SPD05K, T1K, SPD1K, T2K, SPD2K = calcTSPD(hrrrdata.tpress.isel(x=x,y=y).values, hrrrdata.hpress.isel(x=x,y=y).values,
##                                                    hrrrdata.orog.isel(x=x,y=y).values, hrrrdata.wspdpress.isel(x=x,y=y).values )  
##    nearest_slr = calc_slr(T05K, SPD05K, T1K, SPD1K, T2K, SPD2K, wbz, elev2)
    return (
        yyyymmddhh, ifhr, nearest_lat, nearest_lon, nearest_elev, nearest_psfc, nearest_tsfc, nearest_t2m, nearest_rh2m,
        nearest_wspd10m, nearest_wdir10m, nearest_dswrf, nearest_uswrf, nearest_dlwrf,
        nearest_ulwrf, nearest_hourlyprecip, nearest_slr, nearest_snow
        )

# Calculates SLR using random forest described and verified by Pletcher et al. (20124)
# Available at https://github.com/mdpletcher/SLR_random_forest_pletcher
# Snow level/ptype based on wet-bulb zero method
def calc_slr(T05K, SPD05K, T1K, SPD1K, T2K, SPD2K, wbz, elev):

    # Flatten 2D xarray Dataset arrays for SLR model 
    T05K   = T05K.to_numpy().flatten()
    T1K    = T1K.to_numpy().flatten()
    T2K    = T2K.to_numpy().flatten()
    SPD05K = SPD05K.to_numpy().flatten()
    SPD1K  = SPD1K.to_numpy().flatten()
    SPD2K  = SPD2K.to_numpy().flatten()

    # Empty DataFrame
    df_slr = pd.DataFrame()

    # Create columns with keys for model
    df_slr['T05K']   = pd.Series(T05K)
    df_slr['T1K']    = pd.Series(T1K)
    df_slr['T2K']    = pd.Series(T2K)
    df_slr['SPD05K'] = pd.Series(SPD05K)
    df_slr['SPD1K']  = pd.Series(SPD1K)
    df_slr['SPD2K']  = pd.Series(SPD2K)

    # Load model components
    model  = np.load(slrmodeldir+'All7Sites_RF_slr_modelAGL38b.pickle', 
        allow_pickle = True)
    keys   = np.load(slrmodeldir+'All7Sites_slr_model_keysAGL38b.npy', 
        allow_pickle = True)

    # Predict SLR for all HRRR grid points
    data_trim = df_slr.loc[:, keys]
    df_slr['slr'] = model.predict(data_trim)

    # Initial SLR, reshape back to HRRR grid shape (1059, 1799). I checked and made sure
    # the values go back to the same place as before
    initslr   = df_slr['slr']
    initslr   = initslr.to_numpy().reshape(1059, 1799)

    # Snowlevel calculation
    snowlevel = wbz - allsnow
    snowlevel = xr.where(snowlevel < 0., 0., snowlevel)

    # SLR QC
    slr = xr.where(elev >= snowlevel, initslr, 0.)

    slr = xr.where(
        ((elev < snowlevel) & (elev > (snowlevel - melt))),
        (initslr * (elev - (snowlevel - melt)) / melt), slr)

    return slr

def calcTSPD(_t, _gh, topo, _spd):

    AGL_levs = [2000,1000,500]; 
    for i in range(0,len(AGL_levs)):
        AGLm = AGL_levs[i];
    
        # Geo Height - Surface Elev + a distance AGL. Gives Geo Heights ABOVE GROUND LEVEL + a buffer defined by 'AGLm' (this is either 500, 1000, 2000m)
        gh_agl = (_gh - (topo + AGLm)).compute()
    
        # Where this is zero, set to 1.0
        gh_agl = xr.where(gh_agl == 0.0, 1.0, gh_agl)
        
        # If the 1000mb height is > 0, use the 1000 mb temperature to start. Otherwise assign t=0 ans s=0
        tvals = xr.where(gh_agl.sel(isobaricInhPa=1000) > 0, _t.sel(isobaricInhPa=1000), 0) # - 273.15, 0)
        svals = xr.where(gh_agl.sel(isobaricInhPa=1000) > 0, _spd.sel(isobaricInhPa=1000), 0)  
        for i in range(_t.isobaricInhPa.size)[:0:-1]:

            # current level
            lc = _t.isobaricInhPa.isel(isobaricInhPa=i).values; zc = gh_agl.isel(isobaricInhPa=i); tc = _t.isel(isobaricInhPa=i); sc = _spd.isel(isobaricInhPa=i);
        
            # level above (correct for 'wraparound')
            up = i+1 if i+1 < _t.isobaricInhPa.size else 0
            lup = _t.isobaricInhPa.isel(isobaricInhPa=up).values; zup = gh_agl.isel(isobaricInhPa=up); tup = _t.isel(isobaricInhPa=up); sup = _spd.isel(isobaricInhPa=up)
        
            # level below
            ldn = _t.isobaricInhPa.isel(isobaricInhPa=i-1).values; zdn = gh_agl.isel(isobaricInhPa=i-1); tdn = _t.isel(isobaricInhPa=i-1); sdn = _spd.isel(isobaricInhPa=i-1)
        
            # Where the geo height AGL > 0 at this level and geo height AGL < 0 at level below...
            tvals = xr.where(((zc > 0.0) & (zdn < 0.0)),
            # Do this
            ( ( zc / ( zc - zdn ) ) * ( tdn - tc ) + tc ),
            # Else use tvals already determined
            tvals )

            # Same for svals
            svals = xr.where(((zc > 0.0) & (zdn < 0.0)), ( ( zc / ( zc - zdn ) ) * ( sdn - sc ) + sc ), svals )
        
        if AGLm==500:
            T05K = tvals; SPD05K = svals; 
        if AGLm==1000:
            T1K = tvals; SPD1K = svals; 
        if AGLm==2000:
            T2K = tvals; SPD2K = svals; 

    return T05K, SPD05K, T1K, SPD1K, T2K, SPD2K

# Delete grib2 and idx files if they exist
def delhrrrfiles():
    suffixes = ['*grib2*','*.idx']
    for suffix in suffixes:
        filelist = glob.glob(suffix)
        for file in filelist:
            os.remove(file)

# ------------------ MAIN PROGRAM ----------------

# Make sure we are running python 3.9
version = python_version().split('.')[0]+'.'+python_version().split('.')[1]
if version != '3.9':
    print('You are using python version '+python_version()[:3])
    print('Needs to be 3.9')
    sys.exit

# Delete any existing grib2 or idx files
delhrrrfiles()

# Make scratch directory if needed
if not os.path.exists(scratchdir):
    os.makedirs(scratchdir)

# Determine time to process if not entered on command line
if len(sys.argv) < 2 or not sys.argv[1]:
    run_date = datetime.datetime.utcnow() - datetime.timedelta(hours=1) 

# Otherwise use user specified time entered on command line
# Format sys.argv[1]: YYYY-MM-DY sys.argv[2]: HH:00:00
else:
    run_date = pd.Timestamp(sys.argv[1]+' '+sys.argv[2])

# Get rid of mm ss and extract yr mn dy hr
run_date = run_date.strftime('%Y-%m-%d-%H')
yr,mn,dy,hr = str(run_date).split('-')
if (hr == '00' or hr == '06' or hr == '12' or hr == '18'):
    maxfhr = 48
else:
    maxfhr = 18 
fhrs = tuple(range(maxfhr+1))
items = [(yr,mn,dy,hr,fhr) for fhr in fhrs] 
if processing == 'parallel':

    # parallel process this mess
    fhrs = tuple(range(maxfhr+1))
    items = [(yr,mn,dy,hr,fhr) for fhr in fhrs]
    processes = min(maxfhr+1, maxprocesses)
    print('Running with '+str(processes)+' processes')
    with Pool(processes=processes) as p:
        output = p.starmap(processhrrr, items)

    columns = ['INIT (YYYYMMDDHH UTC)','FHR','Grid Point Lat','Grid Point Lon','Grid Point Elev (m)','PSFC (PA)','TSFC (K)','T2m (K)','RH2m (%)',
               'Wind Speed 10m (m/s)','Wind Direction 10 m (deg)','Downward Short Wave (W/m2)', 'Upward Short Wave (W/m2)',
               'Downward Long Wave (W/m2)','Upward Long Wave (W/m2)','Water Equiv Precip (mm)',
               'Snow-to-Liquid Ratio','Snowfall (cm)']
    stationdf = pd.DataFrame(output, columns=columns)
    print(stationdf)
    stationdf.to_csv('./data.csv', index=False)
    
elif processing == 'serial':

    # serial (helpful for debugging)
    for item in items:   
        output = processhrrr(item[0],item[1],item[2],item[3],item[4])
        print(output)
        
# Delete any existing grib2 or idx files
delhrrrfiles()

print('Processing complete')

# -------------- END MAIN PROGRAM ----------------
