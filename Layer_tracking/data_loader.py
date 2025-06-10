# data_loader.py

import snowpat.snowpackreader as spr

def load_profile(path):
    pro = spr.readPRO(path)
    pro.discard_below_ground(True)
    return pro

def extract_variables(pro):
    return {
        'ht': pro.get_var('0501'),
        'id': pro.get_var('0504'),
        'rcl': pro.get_var('0606'),
        'gt': pro.get_var('0513'),
        'rho': pro.get_var('0502'),
        'sn38': pro.get_var('0532'),
        'dates': pro.get_all_dates()
    }
