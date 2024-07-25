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
processing = 'serial'

# Maximum number of parallel processes if being run parallel
maxprocesses = 20

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
    print('hrrr data obtained and merged into single dataset')
    print('do radiation and precip variables have data at hour 0?')

    # Calculate 10-m wind speed
    hrrrdata = hrrrdata.assign(wspd10m=(hrrrdata['u10m']**2 + hrrrdata['v10m']**2)**0.5)
    
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
    
    # Get data for gridpoint closest to station coordinates
    # See https://github.com/blaylockbk/pyBKB_v3/blob/master/demo/KDTree_nearest_neighbor.ipynb
    lons = hrrrdata.longitude.values
    lats = hrrrdata.latitude.values
    tree = spatial.KDTree(np.column_stack([lons.ravel(), lats.ravel()]))
    point = np.array([sitelon+360., sitelat])  # Add 360 to match HRRR lons 
    dist, idx = tree.query(point)
    y,x = np.unravel_index(idx,lons.shape)
    nearest_lon = hrrrdata.longitude.isel(x=x,y=y).values
    nearest_lat = hrrrdata.latitude.isel(x=x,y=y).values
    nearest_tsfc = hrrrdata.tsfc.isel(x=x,y=y).values
    nearest_t2m = hrrrdata.t2m.isel(x=x,y=y).values
    nearest_rh2m = hrrrdata.rh2m.isel(x=x,y=y).values
    nearest_wspd10m = hrrrdata.wspd10m.isel(x=x,y=y).values
    nearest_wdir10m = hrrrdata.wdir10m.isel(x=x,y=y).values
    nearest_dswrf = hrrrdata.dswrf.isel(x=x,y=y).values
    nearest_uswrf = hrrrdata.uswrf.isel(x=x,y=y).values
    nearest_dlwrf = hrrrdata.dlwrf.isel(x=x,y=y).values
    nearest_ulwrf = hrrrdata.ulwrf.isel(x=x,y=y).values
    nearest_hourlyprecip = hrrrdata.hourlyprecip.isel(x=x,y=y).values
    print(nearest_lat, nearest_lon, nearest_t2m, nearest_rh2m, nearest_tsfc, nearest_wspd10m, nearest_wdir10m, nearest_dswrf, nearest_uswrf, nearest_dlwrf, nearest_ulwrf, nearest_hourlyprecip)
    return

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
    print('Results for processing of HRRR initialized '+time+':')
    print(output)

elif processing == 'serial':

    # serial (helpful for debugging)
    for item in items:   
        processhrrr(item[0],item[1],item[2],item[3],item[4])

    
# Delete any existing grib2 or idx files
delhrrrfiles()

print('Processing complete')

# -------------- END MAIN PROGRAM ----------------
