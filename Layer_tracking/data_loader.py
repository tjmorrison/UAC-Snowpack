# data_loader.py

import snowpat.snowpackreader as spr
import contextlib
import io

def load_profile(path):
    pro = spr.readPRO(path)
    pro.discard_below_ground(True)
    return pro

def extract_variables(pro):
    """
    Extract variables from the snow profile.

    Args:
        pro (snowpat.snowpackreader.SnowProfile): The snow profile object.

    Returns:
        dict: A dictionary containing the extracted variables.
    """
    with contextlib.redirect_stdout(io.StringIO()):
        ht = pro.get_var('0501')
        temp = pro.get_var('0503')
        id_ = pro.get_var('0504')
        hardness = pro.get_var('0534')
        rcl = pro.get_var('0606')
        gt = pro.get_var('0513')
        rho = pro.get_var('0502')
        sn38 = pro.get_var('0532')
        shear_strength = pro.get_var('0601')
        dates = pro.get_all_dates()

    return {
        'ht': ht,
        'temp': temp,
        'id': id_,
        'hardness': hardness,
        'rcl': rcl,
        'gt': gt,
        'rho': rho,
        'sn38': sn38,
        'shear_strength': shear_strength,
        'dates': dates
    }




