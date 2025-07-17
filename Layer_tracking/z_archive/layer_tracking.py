


# Import libraries
import snowpat.snowpackreader as spr
import logging as log
import os
import pandas as pd
import numpy as np

# Configure logging for handling validation and verification
log.basicConfig(
    filename='./Layer_tracking/lib-layer_tracking.log',  # File to write logs to
    level=log.DEBUG,  # Minimum level of messages to log
    filemode='a',  # 'w' to overwrite, 'a' to append
    format='%(asctime)s - %(levelname)s - %(message)s'
)

# Functions

def parse_gtype(grain_code):
    try:
        return grain_code // 100, (grain_code % 100) // 10, grain_code % 10
    except Exception:
        print("Failed to parse grain code")
        return None, None, None

def identify_surface_hoar(profile):
    """_summary_

    Args:
        profile (dataframe): snowpack profile 

    Returns:
       sh_likelihood (boolean): was surface hoar detected at the surface of the profile? 0 not 
    """
    #Swiss Code F1F2F3 Surface Hoar Code is 6, for more information on data formats see : https://snowpack.slf.ch/doc-release/html/snowpackio.html
    #initialize boolean
    #code 514 is for surface hoar, 0514,3,grain type, grain size (mm), and density (kg m-3) of SH at surface - if -999 no surface hoar
    #appears that 514 is not being read by the current library - will leave as is for now. 
    sh_bool= [0,0,0] #[primary bool, secondary bool, tertiary bool]
    try:
        primary_gtype,secondary_gtype, tertiary_gtype = parse_gtype(profile['0513'].iloc[-1])

        #type 4 = SH, type 5 is Depth hoar, type 6 is facets
        if primary_gtype == 4 or primary_gtype == 5 or primary_gtype == 6:
            sh_bool[0] = 1
        if secondary_gtype == 4 or secondary_gtype == 5 or secondary_gtype == 6:
            sh_bool[1] = 1
        if tertiary_gtype == 4 or tertiary_gtype == 5 or tertiary_gtype == 6:
            sh_bool[2] = 1

    except:
        print("surface hoar detection failed")

    return sh_bool

def identify_near_surface_facets(profile,surface_depth = 50):
    """_summary_

    Args:
        profile (dataframe): snowpack profile 

    Returns:
       var (boolean): was surface hoar detected at the surface of the profile? 0 not 
    """
    #Swiss Code F1F2F3 Surface Hoar Code is 6, for more information on data formats see : https://snowpack.slf.ch/doc-release/html/snowpackio.html
    #initialize boolean
    try:
        total_depth = profile['layer thickness'].sum()
        #ensure there is enough snow to deem looking for near surface facets
        nsf_bool= [0,0,0] #[primary bool, secondary bool, tertiary bool]
        if (total_depth>surface_depth):
            #start search from top of snowpack
            search_index = len(profile) - 2 #start from second layer since first layer will be caught by SH detector
            search_depth = profile['layer thickness'].iloc[search_index+1] #initiate layer depths including first layer thickness
            nsf_bool= [0] #[primary bool, secondary bool, tertiary bool]
            while(search_depth<surface_depth):

                primary_gtype,secondary_gtype, tertiary_gtype = parse_gtype(profile['0513'].iloc[search_index]) #grab the variable at search index
                #type 4 = SH, type 5 is Depth hoar, type 6 is facets
                if primary_gtype == 4 or primary_gtype == 5 or primary_gtype == 6:
                    nsf_bool[0] = 1
                    break
                
                #increase depth and index
                search_depth = search_depth + profile['layer thickness'].iloc[search_index]
                search_index = search_index - 1

    except:
        print("near surface facet search fail")

    return nsf_bool, search_depth


# Log some messages
#log.debug('This is a debug message')
#log.info('This is an info message')
#log.warning('This is a warning message')
#log.error('This is an error message')
#log.critical('This is a critical message')
# Main
# loop through all test cases and write results to log file
if __name__ == "__main__":
    log.info('Entering main')

    # Set dev flag for testing functions
    dev_flag = True   
    log.info('Dev flag: ' + str(dev_flag))
    
    unit_pro_path = './Unit_profiles'
    validaiton_pro_path = './Validation_profiles/'

    if dev_flag == True:
        log.info('Entering verification test cases')
        # Loop through all files in the unit_pro_path folder
        for filename in os.listdir(unit_pro_path):
            if filename.endswith(".pro"):  #only load *.pro files
                file_path = os.path.join(unit_pro_path, filename)
                log.info(f'Processing unit test: {file_path}')
            try:
                # Read the profile data
                pro = spr.readPRO(file_path)
                dates = pro.get_all_dates()

                # Get the last profile
                profil_end = pro.get_profile_on(dates[-1]) #corrsponds to Jan 27 2025, a period with surface hoar formation 
                profile_df = profil_end.toDf()

                # Call the identify_surface_hoar function
                sh_likelihood = identify_surface_hoar(profile_df)
                log.info(f'identify_surface_hoar result for {filename}: {sh_likelihood}')
                
                # Call the identify_surface_hoar function
                nsf_bool = identify_near_surface_facets(profile_df)
                log.info(f'identify_surface_hoar result for {filename}: {nsf_bool}')

            except Exception as e:
                log.error(f'Error processing file {filename}: {e}')

        log.info('Entering validation test cases')
        # Loop through all files in the validation_pro_path folder
        for filename in os.listdir(validaiton_pro_path):
            if filename.endswith(".pro"):  #only load *.pro files
                file_path = os.path.join(validaiton_pro_path, filename)
                log.info(f'Processing unit test: {file_path}')
            try:
                # Read the profile data
                pro = spr.readPRO(file_path)
                dates = pro.get_all_dates()

                # Get the last profile
                profil_end = pro.get_profile_on(dates[10990]) #corrsponds to Jan 27 2025, a period with surface hoar formation
                profile_df = profil_end.toDf()

                # Call the identify_surface_hoar function
                sh_bool = identify_surface_hoar(profile_df)
                log.info(f'identify_surface_hoar result for {filename}: {sh_bool}')
                
                # Call the identify_surface_hoar function
                nsf_bool = identify_near_surface_facets(profile_df)
                log.info(f'identify_surface_hoar result for {filename}: {nsf_bool}')

            except Exception as e:
                log.error(f'Error processing file {filename}: {e}')