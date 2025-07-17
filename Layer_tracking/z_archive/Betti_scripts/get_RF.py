# -*- coding: utf-8 -*-
"""
Created on Tue Apr 13 16:30:42 2021

@author: Stephanie Mayer
"""

import pandas as pd
import numpy as np


###########################################################################################################
#####
#####   Name:       rc_flat
#####
#####   Purpose:    calculate critical crack length [m] (Richter 2019) for each layer of a snow profile
#####               given arrays of upper boundaries of layers (layer_top, [cm]), density (rho, [kg m-3]),   
#####               grain size (gs, [mm]), shear strength (strength_s [kPa]). 
#####   Remarks:    The first entry of each array corresponds to the snow layer closest to the ground, the last entry corresponds to the uppermost layer.
#####               Critical crack length is calculated for the flat, even if input profile is given at slope angle. 
###########################################################################################################    
    
def rc_flat(layer_top, rho, gs, strength_s):
    rho_ice = 917. #kg m-3
    gs_0 = 0.00125 #m
    thick = np.diff(np.concatenate((np.array([0]), layer_top)))
    rho_wl = rho
    gs_wl = gs*0.001 #[m]
    rho_sl = np.append(np.flip(np.cumsum(rho[::-1]*thick[::-1])/np.cumsum(thick[::-1]))[1:len(rho)],np.nan)
    tau_p = strength_s*1000. #[Pa]
    eprime = 5.07e9*(rho_sl/rho_ice)**5.13 / (1-0.2**2) #Eprime = E' = E/(1-nu**2) ; poisson ratio nu=0.2
    dsl_over_sigman = 1. / (9.81 * rho_sl) #D_sl/sigma_n = D_sl / (rho_sl*9.81*D_sl) = 1/(9.81*rho_sl)
    a = 4.6e-9
    b = -2.
    rc_flat = np.sqrt(a*( rho_wl/rho_ice * gs_wl/gs_0 )**b)*np.sqrt(2*tau_p*eprime*dsl_over_sigman)
    return rc_flat


###########################################################################################################
#####
#####   Name:       comp_features
#####
#####   Purpose:     given SNOWPACK profile in readProfile format, calculate all necessary features for every single layer (from bottom to top of profile)
#####               
#####   Remarks:  don't change order of features in output (the RF model needs exactly this order)
#####
###########################################################################################################
    
def comp_features(prof, slopeangle):
    
    strength_s = prof['shear_strength']  
    gs = prof['grain_size']
    rho = prof['density']
    layer_top = prof['height']
    nlayers = len(rho)
    
    # 1. viscous deformation rate
    viscdefrate = prof['viscous_deformation_rate']
    
    # 2. critical crack length
    rcflat = rc_flat(layer_top, rho, gs, strength_s)
    
    # 3. sphericity

    # 4. grainsize
  
    
    # 5. penetration depth
    pen_depth = comp_pendepth(prof,slopeangle)
    pen_depth_rep = np.repeat(pen_depth, nlayers)
    
    # 6. mean of slab density divided by slab grain size <rho/gs>_{slab}
    thick = np.diff(np.concatenate((np.array([0]), layer_top)))
    rhogs = rho*thick/gs
    slab_rhogs = np.append(np.flip(np.cumsum(rhogs[::-1])/np.cumsum(thick[::-1]))[1:len(rho)],np.nan)

    # put all features together into one dataframe. 
    d = {'viscdefrate': viscdefrate,
         'rcflat': rcflat,
         'sphericity': prof['sphericity'],
         'grainsize': gs,
         'penetrationdepth': pen_depth_rep,
         'slab_rhogs': slab_rhogs}  #
    
    features = pd.DataFrame(data = d)
    
    return features 


###########################################################################################################
#####
#####   Name:       comp_rf_probability
#####
#####   Purpose:     given SNOWPACK profile in readProfile format and RF model, calculate RF probability for each layer
#####               
#####   Remarks:  
#####
###########################################################################################################
    
def comp_rf_probability(features, model):
    # make sure features are in right order for model
    features = features[['viscdefrate', 'rcflat', 'sphericity', 'grainsize', 'penetrationdepth','slab_rhogs']]
    P_unstable = np.repeat(np.nan,len(features))    
    
    # for i, row in features.iterrows():
        # if i < len(features)-1: #all layers except the upper one
            # P_unstable[i] = model.predict_proba(features.loc[[i]])[:,0]
    if len(P_unstable)> 1:# more than one element is needed to compute P_unstable, wl + slab
        P_unstable[:-1] = model.predict_proba(features.iloc[:-1])[:,0] 
    else:
        if len(P_unstable) ==1:
            P_unstable = [np.nan]
        else:
            P_unstable = []
        

    return P_unstable

###########################################################################################################
#####
#####   Name:       comp_prof_dataframe
#####
#####   Purpose:     given SNOWPACK profile in readProfile format and RF model, calculate RF probability for each layer
#####               and save relevant properties in dataframe
#####   Remarks:  
#####
###########################################################################################################

def create_RFprof(prof, slopeangle, model):
    #get features for RF model
    features = comp_features(prof, slopeangle)
    #get P_unstable
    df = features
    df['P_unstable'] = comp_rf_probability(features, model)
    #get some additional features for plotting
    df['layer_top'] = prof['height']
    df['density'] = prof['density']
    df['hardness'] = prof['hand_hardness']
    df['graintype'] = prof['grain_type']    
    return df
        



###########################################################################################################
#####
#####   Name:       comp_pendepth
#####
#####   Purpose:     given SNOWPACK profile in readProfile format, calculate skier penetration depth
#####               
#####   Remarks: might slightly differ from SNOWPACK source code, but is correct with regard to publications Jamieson Johnston (1998)
#####               and Bellaire (2006)
###########################################################################################################
def comp_pendepth(prof, slopeangle):
    top_crust = 0
    thick_crust = 0
    rho_Pk = 0
    dz_Pk = 1.e-12
    crust = False
    e_crust = -999
    
    layer_top = prof['height']
    ee = len(layer_top)-1
    thick = np.diff(np.concatenate((np.array([0]), layer_top)))
    rho = prof['density']
    HS = layer_top[-1]
    graintype = prof['grain_type']  
    min_thick_crust = 3 #cm
    
    while (ee >= 0) & ((HS-layer_top[ee])<30):
        
        rho_Pk = rho_Pk + rho[ee]*thick[ee]
        dz_Pk = dz_Pk + thick[ee]
        
        if crust == False:
        ##Test for strong mf-crusts MFcr.
        ## Look for the first (from top) with thickness perp to slope > 3cm
            if (graintype[ee] == 772) & (rho[ee] >500.): ## very high density threshold, but implemented as this in SP
                if e_crust == -999:
                   e_crust = ee
                   top_crust = layer_top[ee]
                   thick_crust = thick_crust + thick[ee]
                elif (e_crust - ee) <2:
                   thick_crust = thick_crust + thick[ee]
                   e_crust = ee
            elif e_crust > 0:
               if thick_crust*np.cos(np.deg2rad(slopeangle)) > min_thick_crust:
                   crust = True
               else:
                   e_crust = -999
                   top_crust = 0
                   thick_crust = 0

        ee = ee-1
                         

    
    rho_Pk = rho_Pk/dz_Pk        #average density of the upper 30 cm slab
    return np.min([0.8*43.3/rho_Pk, (HS-top_crust)/100.]) #NOTE  Pre-factor 0.8 introduced May 2006 by S. Bellaire , Pk = 34.6/rho_30
#original regression by Jamieson Johnston (1998)

