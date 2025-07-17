#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct 28 08:41:47 2022

@author: Bettina Richter
"""

import datetime
from dateutil.rrule import rrule, DAILY
import joblib
import pandas as pd
import numpy as np
import scipy.signal

import readProfile
import get_RF as rf_params

#%%
################
#Get synthezised stratigraphy variables

def get_extremas(punst,height,min_distance=10):
    ii_max = scipy.signal.argrelextrema( np.array(punst.iloc[:-1]), order= 2, comparator=np.greater_equal)[0]  #
    indexsort = np.argsort(punst[ii_max])[::-1]
    mini_ind = ii_max[indexsort]
    try:
        ii_inst = mini_ind[np.where((punst[mini_ind] >= 0.77))[0]]
        i_deepest = ii_inst[np.nanargmin(height[ii_inst])]
    except:
        try:
            ii_inst = mini_ind[np.where((punst[mini_ind] >= 0.5))[0]]
            i_deepest = ii_inst[np.nanargmin(height[ii_inst])]
        except:    
            #i_deepest = mini_ind[0]
            i_deepest = 0
            
    #make dure, there at least min_distance apart
    depths = height[mini_ind]
    mini_ind_new = []
    depths_new = []
    for i, d in enumerate(depths):
        if all( abs(d-prev) >= min_distance for prev in depths_new ):
            depths_new.append(d)
            mini_ind_new.append( mini_ind[i] )
    
    return(mini_ind_new, i_deepest)

#%%
def get_percentage_graintype(thickness, gt, gtype='persistent'):
    """gtype can be either 'persistent' or 'crust'
    
    gt: array
        graintype of each layer
    thickness: array
        thickness of each layer.
        same shape as gt
    """
    
    gt=divmod(gt,100)[0].astype(int)    
    if gtype == 'persistent':
        gt_depth = sum(thickness[np.where((gt==4) |(gt==5) | (gt==6) |(gt == 9) )])
    elif gtype == 'crust':
        gt_depth = sum(thickness[np.where((gt==7))])
    else:
        print('Unknown gain type: ', gt)
        gt_depth = 0
    return(gt_depth/sum(thickness))
    
def get_weighted_average(var, thick):
    wa = np.sum( var*thick ) / np.sum(thick)
    return(wa)

def compute_variables(var, df, upper_xcm = False, iwl=False):
    thick = np.diff(np.concatenate((np.array([0]), df['height'])))
    if upper_xcm: 
        isl = np.where(( df['height'] >= df['height'].iloc[-1] - upper_xcm ))
    elif iwl: 
        isl = np.where(( df['height'] > df['height'].iloc[iwl] ))
    else:
        isl = np.arange(len(thick))
    
    if var=='perc_pers':
        wa = get_percentage_graintype(thick[isl], df['grain_type'].iloc[isl], gtype='persistent')
    elif var=='perc_crust':
        wa = get_percentage_graintype(thick[isl], df['grain_type'].iloc[isl], gtype='crust')
    elif var == 'swe':
        wa = np.sum(thick[isl]/100.*df['density'].iloc[isl])
    else: 
        wa = get_weighted_average(df[var].iloc[isl], thick[isl])
    
    return( wa )

def init_stab_params(syn_var,iwl):
    wl='wl'+str(iwl)
    syn_var[wl+'_st_pinst'] = 0 #= totally stable, 1 = totally unstable
    syn_var[wl+'_st_sn38'] = 6 #Max stability value from SNOWPACK 6=stable or undefined
    syn_var[wl+'_st_sk38'] = 6
    syn_var[wl+'_st_ssi'] = 6
    syn_var[wl+'_rc'] = 3 #2m seems to be super long 
    sl='sl'+str(iwl)
    syn_var[sl+'_depth'] = 0
    return(syn_var)
    
def init_additional_stabparams(syn_var,iwl):
    wl='wl'+str(iwl)
    syn_var[wl+'_density'] = 600 #kg/m^3
    syn_var[wl+'_grainsize'] = 0.1 #mm
    syn_var[wl+'_grain_type'] = 3 #rounded grains
    syn_var[wl+'_viscdefrate'] = 0
    syn_var[wl+'_sphericity'] = 1 #round
    syn_var[wl+'_hand_hardness'] = 6
    syn_var[wl+'_lwc'] = 0
    sl='sl'+str(iwl)
    syn_var[sl+'_hand_hardness'] = 6
    syn_var[sl+'_density'] = 600
    syn_var[sl+'_grainsize'] = 0.1
    syn_var[sl+'_lwc'] = 0
    syn_var[sl+'_temperature'] = 0
    return(syn_var)
    
def getSynthesizedVariables(df, 
                            structure_depths = [10,20], structure_variables = ['lwc','temperature'],
                            nwl=1, deepestwl=False, addallwlinfo = False, penetration_depth_properties = True):
    '''
    Variables, which are computed are:
    ----------------------------------
    hs: snow depth [cm]
    lwc_all: weighted average of liquid water content by volume of whole snowpack [%]
    temperature_all: weighted average of snow temperature of whole snowpack [°C]
    swe_all: snow water equivalent [kg/m^2]
    density_all: average density of snowpack [kg/m^3]
    hardness_all: weigthed average of hand hardness of snowpack
    perc_pers_all: Percentage of persistant layers in profile (faceted crystals, depth hoar, surface hoar, rounded facets) 
        computed as: sum(thickness of persistent layers) / sum(thickness)
    perc_crust_all: Percentage of crusts in profile
    
    Additional default variables are:
    ---------------------------------
    temperature_10, temperature_20 [in °C], lwc_10, lwc_20 [in %], 
    other structure variables: perc_pers, pers_crust, density, swe, hand_hardness of certain depths
    These variables are computed as the weighted average of the upper x cm of the snowpack ( sum(variable*thickness) / sum(thickness) )
    e.g. _10 are the upper 10 cm, _20 are the upper 20 cm
    
    Add other additional variable:
    ------------------------------
    structure_variables: list
        A list containing variables of which the upper x cm of the snowpack should be averaged
        Add more variables to list if needed.
        Default: structure_variables = ['lwc','temperature']
        If no other information is needed, pass empty list: structure_variables = []
    structure_depths: list
        A list containing the depths of which the upper x cm in list should be averaged for each of the variables above.
        Add more depths to list if needed.
        Default: structure_depths = [10,20]
        Insteresting variables could also be: structure_depths = [10,20,50,100]
        If no other information is needed, pass empty list: structure_depths = []
    nwl: int
        Number of weak layers, which will be written out.
        All weak and slab layer variables are calculated for the weak layer, which was detected by Stephie's RF model
        All weak layer and slab layer properties will be named wl1_, wl_2, wl..._ , sl1_, sl2_, sl..._
        Weak layers are found by searching for extremas in P_unstable.
        Weak layer are sorted by stability, most unstable (highest value of P_unstable - Probability of being unstable) will be wl1
        If no extrema (or second, third extrema,...) is found, weak and slab layer properties will be assigned to the values of the deepest layer in the profile
        Default: nwl=1
        Properties of weak layer x, which will be written are:
            wlx_st_pinst: weak layer probability of being unstable (in stephie's paper for pmax>0.77 the snowpack is unstable) [0-1]
            wlx_st_sn38: SN38 weak layer natural stability index projected on 38° slope [0-1]
            wlx_st_ssi: SSI weak layer strucural stability index [0-1] ?
            wlx_st_sk38: SK38 weak layer skier stability index projected on 38° slope [0-1]
            wlx_rc: weak layer critical cut length [cm] (one of stephie's features, flat field parameterization works better than slope one)
            slx_depth: slab thickness [cm]
    deepestwl: Bool
        Default: False
        If true, properties of the deepest weak layer with pinst>0.77 will be extracted.
        If no value >0.77 then same layer as wl1 will be used, otherwise deepest layer
    addallwlinfo: Bool
        Default: False
        If true following additional weak and slab layer properties will be extracted for all weak layers:
            wlx_density: weak layer density [kg/m^3]
            wlx_viscdefrate: weak layer viscous deformation rate (one of stephie's features)
            wlx_sphericity: weak layer sphericity [0-1, with 1 being fully spherical and 0 being fully faceted]  (one of stephie's features)
            wlx_grain_type: first grain type of weak layer
            wlx_grainsize: weak layer graon syze [mm] (one of stephie's features)
            wlx_hand_hardness: weak layer hand hardness [1-5, 1: fist; 2: 4fingers; 3: 1finger; 4: pencil; 5: knife]
            wlx_lwc: weak layer liquid water content by volume [%]
            slx_hand_hardness: weighted average of slab hand hardness [1-5]
            slx_density: weighted average of slab density [kg/m^3]
            slx_grainsize: weighted average of slab grain size [mm?]
            slx_lwc: weighted average of slab liquid water content by volume [%]
            slx_temperature: weighted average of slab temperature [°C]
    penetration_depth_properties: Bool
        Default: True
        Following parameters will be extracted:
            penetrationdepth: Penetration depth in cm! Attention: SNOWPACK computes penetration depth in meters! (one of Stephie's features) [cm] 
            pd_rc: Critical cut length for the last layer which will be penetrated (penetration depth wl) HS - penetration depth) [cm]
            pd_st_pinst: probability of being unstable for the penetration depth wl [0-1, > 0.77 is unstable]
            pd_st_sn38: SN38 for penetration depth wl
            pd_st_ssi: SSI for penetration depth wl
            pd_st_sk38: SK38 for penetration depth wl
    '''

    keys=['hs', 
          'lwc_all', 'temperature_all', 'swe_all', 'density_all', 'hardness_all',
          'perc_pers_all', 'perc_crust_all']
    
    #Error handling
    for var in structure_variables:
        if (var not in ['perc_pers','perc_crust','swe']) and (var not in df.keys()): 
            print('Variable not found in profile, will be skipped: ',var)
            structure_variables.remove(var)
    
    #Add mean structure variables for a list of upper x cm of the profile
    for var in structure_variables:
        for upper_cm in structure_depths:
            key = var+'_'+str(int(upper_cm))
            keys.append(key)
            
    #Fill variabes empty: 
    syn_var = {}
    for key in keys:
        syn_var[key] = np.nan
    
    thick = np.diff(np.concatenate((np.array([0]), df['height'])))
    snow_depth = df['height'].iloc[-1]
    syn_var['hs'] = snow_depth
    
    #Return Zeros if no snow
    if snow_depth == 0: return(syn_var)
    
    #########################################
    ######Compute default variables##########
    syn_var['lwc_all'] = compute_variables( 'lwc', df)
    syn_var['temperature_all'] = compute_variables( 'temperature', df)
    syn_var['swe_all'] = compute_variables( 'swe', df)
    syn_var['density_all'] = compute_variables( 'density', df)
    syn_var['hardness_all'] = compute_variables( 'hand_hardness', df)
    syn_var['perc_pers_all'] = compute_variables( 'perc_pers', df)
    syn_var['perc_crust_all'] = compute_variables( 'perc_crust', df)
    
    ###########  Get variables for certain snow depths ###############
    for var in structure_variables:
        for upper_cm in structure_depths:
            key = var+'_'+str(int(upper_cm))
            syn_var[key] = compute_variables( var, df, upper_xcm = upper_cm )
    
    ####################################################
    ########## Get penetration depth pd  ###############
    if penetration_depth_properties:
        
        pd = df['penetrationdepth'].iloc[-1]*100
        syn_var['penetrationdepth'] = pd
        ipd = np.where(( df['height'] >= df['height'].iloc[-1] - pd ))[0][0]
    
        ########## Get stability parameters for layer in the depth of the pd ###################
        syn_var['pd_rc'] = df['rcflat'][ipd]
        syn_var['pd_st_pinst'] = df['P_unstable'][ipd]
        syn_var['pd_st_sn38'] = df['sn38'][ipd]
        syn_var['pd_st_ssi'] = df['ssi'][ipd]
        syn_var['pd_st_sk38'] = df['sk38'][ipd]
    
    #########################################
    ########### Rf instability ##############
    ########### Get weak layer params ###############
    ########### Initialize wl_params ################
    
    if nwl>0:
        for iwl in np.arange(1,nwl+1,1):
            syn_var = init_stab_params(syn_var,iwl)
            if addallwlinfo:
                syn_var = init_additional_stabparams(syn_var,iwl)
    
    if deepestwl:
        syn_var = init_stab_params(syn_var,'deep')
        if addallwlinfo:
            syn_var = init_additional_stabparams(syn_var,'deep')
            
    try:
        ii_maxima, i_deepest = get_extremas(df['P_unstable'],df['height'])
    except: 
        #Stop if no Weak Layer found
        return(syn_var)
    
    ### If not enough extremas were found, fill with 
    for iwl in np.arange(nwl):
        wl='wl'+str(iwl+1) #Count start with 0, call first WL: wl1
        sl='sl'+str(iwl+1)
        try:
            i_pmax = ii_maxima[iwl]
        except: continue
        #Add default weak and slab layer properties
        pmax = df['P_unstable'][i_pmax]
        syn_var[wl+'_st_pinst'] = pmax
        syn_var[wl+'_st_sn38'] = df['sn38'][i_pmax]
        syn_var[wl+'_st_ssi'] = df['ssi'][i_pmax]
        syn_var[wl+'_st_sk38'] = df['sk38'][i_pmax]
        syn_var[wl+'_rc'] = df['rcflat'][i_pmax]    
        syn_var[sl+'_depth'] = np.sum(thick[i_pmax+1:])
        
        if addallwlinfo:
            ######### Add additional Weak layer properties ###########
            gt=divmod(df['grain_type'],100)[0].astype(int)
            syn_var[wl+'_grain_type'] = gt[i_pmax]
            for key in ['density', 'viscdefrate', 'sphericity', 'grainsize', 'hand_hardness', 'lwc']:
                wlkey = wl+'_'+key
                syn_var[wlkey] = df[key][i_pmax]
            ########## Get slab parameters #####################
            for key in ['hand_hardness', 'density', 'grainsize', 'lwc', 'temperature']:
                slkey = sl+'_'+key
                syn_var[slkey] = compute_variables( key, df, iwl = i_pmax )
                
    if deepestwl:
        wl='wldeep'
        sl='sldeep'
        i_pmax = i_deepest
        
        #Add default weak and slab layer properties
        pmax = df['P_unstable'][i_pmax]
        syn_var[wl+'_st_pinst'] = pmax
        syn_var[wl+'_st_sn38'] = df['sn38'][i_pmax]
        syn_var[wl+'_st_ssi'] = df['ssi'][i_pmax]
        syn_var[wl+'_st_sk38'] = df['sk38'][i_pmax]
        syn_var[wl+'_rc'] = df['rcflat'][i_pmax]    
        syn_var[sl+'_depth'] = np.sum(thick[i_pmax+1:])
        
        if addallwlinfo:
            ######### Add additional Weak layer properties ###########
            gt=divmod(df['grain_type'],100)[0].astype(int)
            syn_var[wl+'_grain_type'] = gt[i_pmax]
            for key in ['density', 'viscdefrate', 'sphericity', 'grainsize', 'hand_hardness', 'lwc']:
                wlkey = wl+'_'+key
                syn_var[wlkey] = df[key][i_pmax]
            ########## Get slab parameters #####################
            for key in ['hand_hardness', 'density', 'grainsize', 'lwc', 'temperature']:
                slkey = sl+'_'+key
                syn_var[slkey] = compute_variables( key, df, iwl = i_pmax )
        
    return(syn_var)

#%%
def get_all_profile(pro, df):
    data = {}
    for key in df.keys():
        data[key] = df[key]
    for key in pro.keys():
        data[key] = pro[key]
    return( pd.DataFrame(data = data) )

#%%

def loadProfile(filename, 
                a = datetime.datetime(2021, 10, 1, 12), b = datetime.datetime(2022, 6, 30, 12), 
		model = './models/RF_instability_model.sav',
                daily=False,
                write_df=False, 
                filename_output = './test.csv',
                structure_depths = [10,20], structure_variables = ['lwc','temperature'],
                nwl=1, deepestwl=False, addallwlinfo = False, penetration_depth_properties = True):
    
    '''
    filename: str
        filename of *.pro file to sythesize and extract computed variables.
    a: datetime object
        profiles will be sythesized between two dates a and b
        a is starting date
        Default: a = datetime.datetime(2021, 10, 1, 12)
    b: datetime object
        profiles will be sythesized between two dates a and b
        b is end date
        Default: b = datetime.datetime(2022, 6, 30, 12)
    model: str
	filename/path to RF model instability of Stephie Mayer
    daily: boolean
        if False: each timestamp, which is available in profile will be synthesized.
        if True: Only one profile perday will be extracted and synthesized.
        Default: False
    write_df: boolean
        if False: No csv-file will be witten.
        if True: csv file will be written, recommended
        Default: True
    filename_output: str
        Name of csv file in which synthesized variables should be written
        Default: filename_output = './test.csv'
    
    For each timestamp, the profile is synthesized and following variables are extracted:
    ----------------------------------
    hs: snow depth [cm]
    lwc_all: weighted average of liquid water content by volume of whole snowpack [%]
    temperature_all: weighted average of snow temperature of whole snowpack [°C]
    swe_all: snow water equivalent [kg/m^2]
    density_all: average density of snowpack [kg/m^3]
    hardness_all: weigthed average of hand hardness of snowpack
    perc_pers_all: Percentage of persistant layers in profile (faceted crystals, depth hoar, surface hoar, rounded facets) 
        computed as: sum(thickness of persistent layers) / sum(thickness)
    perc_crust_all: Percentage of crusts in profile
    
    Additional default variables are:
    ---------------------------------
    temperature_10, temperature_20 [in °C], lwc_10, lwc_20 [in %], 
    other structure variables: perc_pers, pers_crust, density, swe, hand_hardness of certain depths
    These variables are computed as the weighted average of the upper x cm of the snowpack ( sum(variable*thickness) / sum(thickness) )
    e.g. _10 are the upper 10 cm, _20 are the upper 20 cm
    
    Add other additional variable:
    ------------------------------
    structure_variables: list
        A list containing variables of which the upper x cm of the snowpack should be averaged
        Add more variables to list if needed.
        Default: structure_variables = ['lwc','temperature']
        If no other information is needed, pass empty list: structure_variables = []
    structure_depths: list
        A list containing the depths of which the upper x cm in list should be averaged for each of the variables above.
        Add more depths to list if needed.
        Default: structure_depths = [10,20]
        Insteresting variables could also be: structure_depths = [10,20,50,100]
        If no other information is needed, pass empty list: structure_depths = []
    nwl: int
        Number of weak layers, which will be written out.
        All weak and slab layer variables are calculated for the weak layer, which was detected by Stephie's RF model
        All weak layer and slab layer properties will be named wl1_, wl_2, wl..._ , sl1_, sl2_, sl..._
        Weak layers are found by searching for extremas in P_unstable.
        Weak layer are sorted by stability, most unstable (highest value of P_unstable - Probability of being unstable) will be wl1
        If no extrema (or second, third extrema,...) is found, weak and slab layer properties will be assigned to the values of the deepest layer in the profile
        Default: nwl=1
        Properties of weak layer x, which will be written are:
            wlx_st_pinst: weak layer probability of being unstable (in stephie's paper for pmax>0.77 the snowpack is unstable) [0-1]
            wlx_st_sn38: SN38 weak layer natural stability index projected on 38° slope [0-1]
            wlx_st_ssi: SSI weak layer strucural stability index [0-1] ?
            wlx_st_sk38: SK38 weak layer skier stability index projected on 38° slope [0-1]
            wlx_rc: weak layer critical cut length [cm] (one of stephie's features, flat field parameterization works better than slope one)
            slx_depth: slab thickness [cm]
    deepestwl: Bool
        Default: False
        If true, properties of the deepest weak layer with pinst>0.77 will be extracted.
        If no value >0.77 then same layer as wl1 will be used, otherwise deepest layer
    addallwlinfo: Bool
        Default: False
        If true following additional weak and slab layer properties will be extracted for all weak layers:
            wlx_density: weak layer density [kg/m^3]
            wlx_viscdefrate: weak layer viscous deformation rate (one of stephie's features)
            wlx_sphericity: weak layer sphericity [0-1, with 1 being fully spherical and 0 being fully faceted]  (one of stephie's features)
            wlx_grain_type: first grain type of weak layer
            wlx_grainsize: weak layer graon syze [mm] (one of stephie's features)
            wlx_hand_hardness: weak layer hand hardness [1-5, 1: fist; 2: 4fingers; 3: 1finger; 4: pencil; 5: knife]
            wlx_lwc: weak layer liquid water content by volume [%]
            slx_hand_hardness: weighted average of slab hand hardness [1-5]
            slx_density: weighted average of slab density [kg/m^3]
            slx_grainsize: weighted average of slab grain size [mm?]
            slx_lwc: weighted average of slab liquid water content by volume [%]
            slx_temperature: weighted average of slab temperature [°C]
    penetration_depth_properties: Bool
        Default: True
        Following parameters will be extracted:
            penetrationdepth: Penetration depth in cm! Attention: SNOWPACK computes penetration depth in meters! (one of Stephie's features) [cm] 
            pd_rc: Critical cut length for the last layer which will be penetrated (penetration depth wl) HS - penetration depth) [cm]
            pd_st_pinst: probability of being unstable for the penetration depth wl [0-1, > 0.77 is unstable]
            pd_st_sn38: SN38 for penetration depth wl
            pd_st_ssi: SSI for penetration depth wl
            pd_st_sk38: SK38 for penetration depth wl
    '''

    
    
    
    model  = joblib.load(model)

    prof = readProfile.read_profile(filename,remove_soil=True)
    ts=[]
    synthesized_variables = {}
    
    if daily:
        tslist = rrule(DAILY, dtstart=a, until=b)
    else:
        tslist= sorted(prof['data'].keys())
    for dt in tslist:
        #Clear memory if needed
        #Define dates
        try: 
            #Run RF model instability
            df = rf_params.create_RFprof(prof['data'][dt],prof['info']['slopeAngle'],model)
            #Synthesize snow stratigraphy
        except: 
            continue
        ts.append(dt)
        dfall = get_all_profile(prof['data'][dt], df)
        syn = getSynthesizedVariables(dfall, 
                                      structure_depths = structure_depths, structure_variables = structure_variables,
                                      nwl=nwl, deepestwl=deepestwl, addallwlinfo = addallwlinfo, 
                                      penetration_depth_properties = penetration_depth_properties)
        
        for key in syn.keys():
            if key not in synthesized_variables.keys(): 
                synthesized_variables[key] = []
            synthesized_variables[key].append(syn[key])
        ############ Free memory #########
        del df, syn, dfall
    
    #Generate Dataframe
    data = pd.DataFrame( synthesized_variables, index=ts )
    #Wirte Dataframe to csv
    if write_df:
        data.to_csv(filename_output,index_label='timestamp',float_format='%.2f')
 
    return(data)

