#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on 01.01.2016

@author: richter bettina

Edited on 01.01.2021

@author: mayer stephanie

Edited on 01.11.2022

@author: richter bettina

"""

import datetime
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
from dateutil.rrule import rrule, DAILY


#%%

###########################################################################################################
#####
#####   Name:       plot_single_profile
#####
#####   Purpose:    plot single profile for a given timestamp
#####               x-axis: Hand hardness
#####               y-axis: snow depth
#####               colors: grain type colorcode
#####               
#####   Optional:   additional variable can be a layer properity which is plotted as a line (seperate x-axis)
#####               
###########################################################################################################

def plot_single_profile(fig,ax, pro, var=None, ax_colorbar=None,ylim=None):
    
    try: hh = -pro['hand_hardness'] #get postive values for hand hardness 
    except: hh= -pro['hardness']
    try: height = pro['height']
    except: height = pro['layer_top']
    try: graintype = pro['grain_type']
    except: graintype = pro['graintype']
    
    if len(height) == 0: return
      
    #Define colorcode for grain types
    cgt=['greenyellow','darkgreen','pink','lightblue','blue','magenta','red','cyan','lightblue']#'red','grey']
    gt = divmod(graintype,100)[0].astype(int)
    
    ########### contours ##########################
    try: hs = height[-1]
    except: hs = height.max()
    ax.plot([0, 0], [0, hs], c='black', linewidth=1)  #y-axis line
    ax.plot([0, hh[0]], [0, 0], c='black', linewidth=1)  #x-axis line

    ################ Plot profile against hand hardness ########################
    
    ybottom=0
    for iy, y in enumerate(height):

        """
        ######################################################        
        #Plot layer contours - horizontal line top of layer
        if iy == len(height)-1:
            ax.plot([0, hh[iy]], [y, y], c='black', linewidth=0.5)
        else:
            ax.plot([0, np.max([hh[iy], hh[iy + 1]])], [y, y], c='black', linewidth=0.5)        
        #Plot layer contours - vertical line right of layer
        ax.plot([hh[iy], hh[iy]], [ybottom, y], c='black', linewidth=0.5)
        """
        ######################################################
        #Fill layer with color of grain type
        ax.fill_betweenx([ybottom, y], 0, hh[iy], color=cgt[int(gt[iy]) - 1] , alpha=0.9)
        
        #cb = ax.pcolormesh([0, hh[iy]],[ybottom, y],np.ones((2,2))*gt[iy],cmap=cmap,vmin=vmin,vmax=vmax,alpha=0.9)
        ybottom = y
        
    ################ colorbar ########################################################
    if ax_colorbar:
        cmapcolorbar = ['greenyellow', 'darkgreen', 'pink', 'lightblue', 'blue', 'magenta', 'red', 'cyan']
        ticklabels = ['PP', 'DF', 'RG', 'FC', 'DH', 'SH', 'MF', 'IF']
        cmapc = mpl.colors.ListedColormap(cmapcolorbar)
        bounds = np.arange(len(cmapcolorbar) + 1)
        norm = mpl.colors.BoundaryNorm(bounds, cmapc.N)
        ticks = np.arange(len(cmapcolorbar)) +0.5
        cb1 = mpl.colorbar.ColorbarBase(ax_colorbar, cmap=cmapc, norm=norm, 
                                        ticks=ticks, 
                                        orientation='vertical')
        cb1.set_ticklabels(ticklabels)
        cb1.minorticks_off()

    ########## Plot SH again, since layers are very thin not alost not visible ########################
    try:
        ish = int(np.where(( gt == 6 ))[0])
        ax.fill_betweenx([height[ish-1],height[ish]] , 0, hh[ish], color='magenta' )
        print('++++++++++++++ SHFOUND  in profile +++++++++')
    except: pass

    ax.set_xlim(0, 5.5)
    ax.set_xticks(np.arange(0, 5.5, 1))
    ax.set_xticklabels(['', 'F ', '4F', '1F', 'P', 'K'])
    ax.set_ylabel('Snow depth [cm]')
    ax.set_xlabel('Hand hardness')
    
    if ylim:
        if hs > ylim: 
            ax.set_ylim(0,hs+20)
        else: 
            ax.set_ylim(0,ylim)
    elif hs < 200: 
        ax.set_ylim(0,200)
    else: 
        ax.set_ylim(0, hs+20)

    ########## Plot Variable as line, seperate x-axis ########################
    if var:
        variable = pro[var]
        ax11 = ax.twiny()
        height_var= np.repeat(np.concatenate((np.array([0]), height)), 2)[1:-1]
        var_repeat = np.repeat(variable, 2)
        c='black'
        label=var
        xlabel=var
        xlim = (np.nanmin(variable),np.nanmax(variable))
        if var == 'temperature':
            c='red'
            label = 'Snow temperature'
            xlabel = 'Snow temperature [$^\circ$C]'
            xlim = (-21,0)
        elif var == 'P_unstable':
            xlabel = 'Probability of being unstable [0-1]'
            xlim = (0,1)
            ax11.plot([0.77,0.77],[ax11.get_ylim()[0],ax11.get_ylim()[1]],color='black',linestyle='--')
        ax11.plot(var_repeat, height_var, c=c, linewidth=1.5, label=label)
        ax11.legend(loc=1)
        ax11.set_xlabel(xlabel)
        ax11.set_xlim(xlim)
        return ax11  

#%%
###########################################################################################################
#####
#####   Name:       plot_seasonal_evolution
#####
#####   Purpose:    plot seasonal evolution of snowpack
#####               x-axis: Time
#####               y-axis: snow depth
#####               colors: grain type by colorcode or colorbar for layer properties, e.g. density
#####               
#####               
###########################################################################################################

def plot_seasonal_evolution(ax, profile, 
                      a = datetime.datetime(2021, 10, 1, 12),b = datetime.datetime(2022, 6, 30, 12), 
                      rule=None, #To make it faster: rule can be e.g. DAILY, then only one profile per day is plotted. Change to: #rule=DAILY,
                      cmap=None, 
                      var='grain_type', colorbar = True,
                      vmin=0,vmax=2):
    
    prof,ts = get_1prof(profile)
    
    #Get timestamps
    if not a: a=ts[0]
    if not b: b=ts[-1]
    
    if rule:
        timestamps = rrule(rule, dtstart=a, until=b)
        deltat=datetime.timedelta(days=1)
    else: 
        try: timestamps = np.array(ts)[ts.index(a):ts.index(b)]
        except: timestamps = ts
        deltat = timestamps[1] - timestamps[0]
     
    for dt in timestamps:
        depth,variable = get_2prof_datastructure(prof,dt,var=var)
        if len(depth)==0: continue
        depth_edges = np.concatenate((np.array([0]),depth))
        #Get time delta to plot
        x=[dt, dt+deltat]
        #Get grain type
        if var=='grain_type':
            cgt=['greenyellow','darkgreen','pink','lightblue','blue','magenta','red','cyan','lightblue']
            cmap=mcolors.ListedColormap(cgt)
            vmin=0.5
            vmax=len(cmap.colors)+0.5
            try:
                cb=ax.pcolormesh(x,depth_edges,np.array([variable]).transpose(),cmap=cmap,vmin=vmin,vmax=vmax,alpha=0.9,shading='flat')
            except: continue
        else:
            colors = [(0.9, 0.9, 0.9),(0.1, 1, 1), (0.2, 0.7, 1 ), (0.1,0.4,1), (0, 0, 0.5)]
            cm = mcolors.LinearSegmentedColormap.from_list('test', colors, N=100)
            try:
                cb=ax.pcolormesh(x,depth_edges,np.array([variable]).transpose(),vmin=vmin,vmax=vmax,cmap=cm,shading='flat')
            except:
                continue
            
    if colorbar:
        try: cb1=plt.colorbar(cb,ax=ax)
        except: return()
        if var=='grain_type':
            ticklabels = ['PP', 'DF', 'RG', 'FC', 'DH', 'SH', 'MF', 'IF','rFC']
            cb1.set_ticks(np.arange(1,vmax+0.5,1))
            cb1.set_ticklabels(ticklabels,)
            cb1.set_label('Grain type')
        elif var=='lwc':
            cb1.set_label('LWC [%]')
        else: cb1.set_label(var)
    
    ax.set_ylabel('Snow depth [cm]')
    
    return(cb)


#%%
###########################################################################################################
#####
#####   Name:       load from pandas DataFrame for plotting
#####
#####   Purpose:    Load data from SNOWPACK *.pro file or pandas DataFrom for plotting seasonal evolution of snowpack
#####               x-axis: Time
#####               y-axis: snow depth
#####               colors: grain type by colorcode or colorbar for layer properties, e.g. density
#####               
#####               
###########################################################################################################

def get_1prof(profile):
    try: #From SNOWPACK *.pro-files 
        prof = profile['data']
        ts = sorted( prof.keys() )
    except: #From pandas dataframes
        prof=profile
        ts = sorted(set(prof['datetime']))
    return(prof,ts)
    
def get_2prof_datastructure(prof,dt, var ='grain_type'):
    return_empty = ([],[])
    try:
        pro=prof[dt]
    except: 
        try: 
            pro = prof[prof['datetime'] == dt]
        except: 
            print('Date not found in profile: ', dt)
            return return_empty
    #get layer height
    if 'height' in pro.keys(): depth = np.array(pro['height'])
    elif 'layer_top' in pro.keys(): depth = np.array(pro['layer_top'])
    else:
        return return_empty
    if len(depth)==0: 
        return return_empty
    
    #get_variables
    if var=='grain_type':
        if 'grain_type' in pro.keys(): graintype = np.array(pro['grain_type'])
        elif 'graintype' in pro.keys(): graintype = np.array(pro['graintype'])
        else: 
            print('Grain types not found for timestamp: ', dt)
            return return_empty
        gt = divmod(graintype,100)[0]
        gt=gt.astype(int)
        if len(gt)==0:
            return return_empty
        variable = gt
    else:
        try: 
            variable = np.array(pro[var])
        except: 
            print('No variable named '+var)
            return return_empty
    
    return(depth,variable)
        
        
    

