"""
@author: Bettina Richter  & Stephanie Mayer
"""
import numpy as np
import datetime


##########################################################################################################
####
####   Name: read_profile
####
####   Purpose: read SNOWPACK .pro file and return dictionary with properties of profile for given timestamp
#####   
####    Remarks: if no timestamp is given, all profiles for all timestamps in the .pro file are returned
###########################################################################################################


def read_profile(filename,timestamp=None,is3d=False,remove_soil=False):
    datafile=open(filename); content= datafile.readlines(); datafile.close()

    prof={}; is_data=False
    prof['info']={};prof['data']={}

    print('Reading file: ',filename)
    for line in content:
        
        if line.startswith('Altitude='): prof['info']['altitude']=float(line.strip().split('=')[1])
        elif line.startswith('Latitude='): prof['info']['latitude']=float(line.strip().split('=')[1])
        elif line.startswith('Longitude='): prof['info']['longitude']=float(line.strip().split('=')[1])
        elif line.startswith('SlopeAngle='): prof['info']['slopeAngle']=float(line.strip().split('=')[1])
        elif line.startswith('SlopeAzi='): prof['info']['slopeAzi']=float(line.strip().split('=')[1])
        elif line.startswith('StationName='): prof['info']['stationName']=line.strip().split('=')[1]

        elif line.startswith('[DATA]'): 
                is_data=True        

        elif line.startswith('0500') and is_data: 
            ts = datetime.datetime.strptime( line.strip().split(',')[1], '%d.%m.%Y %H:%M:%S' )
            prof['data'][ts]={}
        elif line.startswith('0501') and is_data:
            height = np.array([ float(x) for x in line.strip().split(',') ] )[2:]
            if len(height) == 1 and height.item() == 0: continue
            else: prof['data'][ts]['height'] = height
#        elif line.startswith('0504') and is_data:
#            prof['data'][ts]['element_ID']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
        elif line.startswith('0502') and is_data:
            prof['data'][ts]['density']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
#        elif line.startswith('0503') and is_data:
#            prof['data'][ts]['temperature']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
#        elif line.startswith('0506') and is_data:
#            prof['data'][ts]['lwc']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
#        elif line.startswith('0508') and is_data:
#            prof['data'][ts]['dendricity']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
        elif line.startswith('0509') and is_data:
            prof['data'][ts]['sphericity']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
#        elif line.startswith('0510') and is_data:
#            prof['data'][ts]['coord_number']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
#        elif line.startswith('0511') and is_data:
#            prof['data'][ts]['bond_size']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
        elif line.startswith('0512') and is_data:
            prof['data'][ts]['grain_size']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
        elif line.startswith('0513') and is_data:
            prof['data'][ts]['grain_type']= np.array([ float(x) for x in line.strip().split(',') ])[2:-1] #last entry is () and 772 is crust
#        elif line.startswith('0514') and is_data:        
#            prof['data'][ts]['sh_at_surface']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
#        elif line.startswith('0515') and is_data:
#            prof['data'][ts]['ice_content']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
#        elif line.startswith('0516') and is_data:
#            prof['data'][ts]['air_content']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
        elif line.startswith('0517') and is_data:
            prof['data'][ts]['stress']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
#        elif line.startswith('0518') and is_data:
#            prof['data'][ts]['viscosity']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
#        elif line.startswith('0520') and is_data:
#            prof['data'][ts]['temperature_gradient']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
        elif line.startswith('0523') and is_data:
            prof['data'][ts]['viscous_deformation_rate']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
#        elif line.startswith('0530') and is_data:
#            prof['data'][ts]['stab_indices']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
#        elif line.startswith('0531') and is_data:
#            prof['data'][ts]['stab_deformation_rate']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
#        elif line.startswith('0532') and is_data:
#            prof['data'][ts]['sn38']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
#        elif line.startswith('0533') and is_data:
#            prof['data'][ts]['sk38']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
        elif line.startswith('0534') and is_data:
            prof['data'][ts]['hand_hardness']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
#        elif line.startswith('0535') and is_data:
#            prof['data'][ts]['opt_equ_grain_size']= array([ float(x) for x in line.strip().split(',') ])[2:]
        elif line.startswith('0601') and is_data:
            prof['data'][ts]['shear_strength']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
#        elif line.startswith('0602') and is_data:
#            prof['data'][ts]['gs_difference']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
#        elif line.startswith('0603') and is_data:
#            prof['data'][ts]['hardness_difference']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
#        elif line.startswith('0604') and is_data:
#            prof['data'][ts]['ssi']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
#        elif line.startswith('0606') and is_data:
#            prof['data'][ts]['crit_cut_length']= np.array([ float(x) for x in line.strip().split(',') ])[2:]
#        elif line.startswith('0607') and is_data:
#            prof['data'][ts]['depth_pen']= np.array([ float(x) for x in line.strip().split(',') ])[2:]        
#        elif line.startswith('1501') and is_data:
#            prof['data'][ts]['height_nodes']= array([ float(x) for x in line.strip().split(',') ])[2:]
#        elif line.startswith('1532') and is_data:
#            prof['data'][ts]['sn38_nodes']= array([ float(x) for x in line.strip().split(',') ])[2:]
#        elif line.startswith('1533') and is_data:
#            prof['data'][ts]['sk38_nodes']= array([ float(x) for x in line.strip().split(',') ])[2:]
#        elif line.startswith('0607') and is_data:
#            prof['data'][ts]['accumulated_temperature_gradient']= array([ float(x) for x in line.strip().split(',') ])[2:]

    if is3d: getCoordinates(prof)
    if remove_soil:
        for profile in prof['data'].values():
            try:
                i_gr = np.where((profile['height']==0))[0].item()
                profile['height'] = profile['height'][i_gr+1:]
                profile['density'] = profile['density'][i_gr:]
#                profile['temperature'] = profile['temperature'][i_gr:]
#                profile['temperature_gradient'] = profile['temperature_gradient'][i_gr:]
                profile['stress'] = profile['stress'][i_gr:]
#                profile['ice_content'] = profile['ice_content'][i_gr:]
#                profile['air_content'] = profile['air_content'][i_gr:]
#                profile['viscosity'] = profile['viscosity'][i_gr:]
#                profile['element_ID'] = profile['element_ID'][i_gr:]
#                profile['lwc'] = profile['lwc'][i_gr:]
            except: continue

    for ts in prof['data'].keys():
        for var in prof['data'][ts].keys():
            data = prof['data'][ts][var]
            try: prof['data'][ts][var] = np.where((data==-999),np.nan,data)
            except: pass

    if timestamp: return prof['data'][timestamp]

    return prof


def getCoordinates(prof,yllcorner=186500,xllcorner=779000,gridsize=1,nx=600,ny=600,dem=None):
    ix,iy,name=prof['info']['stationName'].strip().split('_')
    ix=int(ix);iy=int(iy)
    prof['info']['ind_x'] = ix
    prof['info']['ind_y'] = ny-1-iy
    prof['info']['coord_x'] = xllcorner+ix*gridsize
    prof['info']['coord_y'] = yllcorner+iy*gridsize
    if dem:
        if dem[prof['info']['ind_y'],prof['info']['ind_x']] != prof['info']['altitude']: print( 'ACHTUNG')
        
