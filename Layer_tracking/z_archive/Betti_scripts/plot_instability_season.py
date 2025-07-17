# -*- coding: utf-8 -*-
"""
Created on Tue May 21 18:57:17 2024

@author: betti
"""
#%%
import joblib
import datetime
import matplotlib.pyplot as plt
from dateutil.rrule import rrule, DAILY
import pandas as pd

import sys 
# Add the path to the scripts directory
sys.path.append('Layer_tracking/z_archive/Betti_scripts')
import readProfile #function to read .pro file
import plot_profile2 #plot snowpack evolution and daily profiles of graintype, density, etc
import get_RF



#%% load RF model Instability

feature_names = ['viscdefrate', 'rcflat', 'sphericity', 'grainsize', 'penetrationdepth','slab_rhogs']  
# This model was developed using Python 3.7.4 and scikit.learn version 0.22.1
model  = joblib.load('/Users/travismorrison/Documents/GitHub/U-CAAST/Layer_tracking/rf_models/RF_instability_model.sav')

#%%

input_folder = 'Layer_tracking/z_archive/Betti_scripts/ATH20'
fn_start = input_folder + '/ATH20'
fn_end = '_10_05_2023_05_15_2024.pro'

slopes = ['flat','north','east','south','west']

#%%
#Plot daily profile including instability
output_folder = '/Users/betti/OneDrive/Dokumente/snowpack/data_uac/plots/slopes_daily_new/'

timestamp = datetime.datetime(2024,2,14,12,0)  #(year, month, day, hour, minute)

for ia, aspect in enumerate(slopes):
    if ia == 0:
        filename = fn_start+fn_end
        slopeangle=0
    else:
        filename = fn_start+str(ia)+fn_end
        slopeangle=35
    figname = '{}/ath_instability_{}_{}.png'.format( output_folder, aspect, datetime.datetime.strftime(timestamp,'%Y_%m_%d'))
    prof = readProfile.read_profile(filename,timestamp,remove_soil=True)
    df_prof = get_RF.create_RFprof(prof, slopeangle, model)
    
    try: plt.cla(); plt.clf(); plt.close()
    except: pass
    fig=plt.figure(figsize=(4,5))
    ax=fig.add_axes([0.18,0.1,0.62,0.72])
    axcolor=fig.add_axes([0.85,0.1,0.05,0.72])
    plot_profile2.plot_single_profile(fig, ax, df_prof, var='P_unstable',ax_colorbar=axcolor,
                                        ylim=300)
    ax.set_title('Atwater '+aspect+ datetime.datetime.strftime(timestamp,' %m/%d')+ ' - Instability')
    plt.savefig( figname  )

#%%
#Plot seasonal evolution of instability
a = datetime.datetime(2023,11,1,12,0)
b = datetime.datetime(2024,5,15,0,0)
ylim=(0,370) #Define snow depth limits

output_folder = '/Users/betti/OneDrive/Dokumente/snowpack/data_uac/plots/slopes_season_new/'
for ia, aspect in enumerate(slopes):
    if ia == 0:
        slopeangle=0
        filename = fn_start+fn_end
    else:
        slopeangle=35
        filename = fn_start+str(ia)+fn_end
    figname = '{}/ath_{}.png'.format( output_folder, aspect)
    profiles = readProfile.read_profile(filename,remove_soil=True)
    
    # Compute Instability
    df_list = []
    dates = pd.date_range(a,b, freq='D')
    for ts in dates: 
        if ts in profiles['data'].keys():
            prof = profiles['data'][ts]        
            try:
                df0 = get_RF.create_RFprof(prof, slopeangle, model) 
                df0['HS'] = df0['layer_top'].iloc[-1]
            except:
                # no snow on the ground (empty array when soil is not computed)
                df0 = pd.DataFrame(columns = ['P_unstable', 'layer_top', 'density',
                                              'hardness', 'graintype', 'viscdefrate', 'rcflat', 'sphericity', 'grainsize',
                                              'penetrationdepth', 'slab_rhogs', 'HS'],
                                  index = [0]) # 1 row filled with nan
                df0['HS'] = 0.
            df0.insert(0,'datetime',ts)
            df_list.append(df0)
        else:
            print('date {}  not found'.format(ts))
    df_evo = pd.concat(df_list, ignore_index = True)

    #Plot instability evolution
    try: plt.cla(); plt.clf(); plt.close()
    except: pass
    fig, ax = plt.subplots(1,1, figsize = (12,6))

    cb=plot_profile2.plot_seasonal_evolution(ax, df_evo, var='P_unstable',a=a,b=b,rule=DAILY, vmax=1, vmin=0)
    ax.set_xlim(a,b)
    ax.set_ylim(ylim)
    ax.set_title('Atwater '+aspect + ': grain type')

    plt.tight_layout()
    figname = output_folder+'/instability_ath_'+aspect+'.png'
    plt.savefig(figname)
