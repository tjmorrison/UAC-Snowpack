# main.py
# Main script to load profile, compute HST, identify accumulation periods, and plot weak layers

from config import PRO_FILE
from data_loader import load_profile, extract_variables
from utils import compute_hst, extract_weak_layers_by_event, identify_accumulation_periods, identify_accumulation_periods_old, track_layers,get_number_events, track_all_layers_xr
from plotting import plot_hst_and_periods, plot_seasonal_sn38, plot_weak_layers_by_event, plot_grain_type_with_weak_layers
import matplotlib.pyplot as plt
import numpy as np
import pickle

#load profile data
pro = load_profile(PRO_FILE)

# Extract variables from the profile
# This function should return a dictionary with keys like 'ht', 'dates', 'id',
# 'rcl', 'gt', 'rho', 'sn38', etc.
vars = extract_variables(pro)

# Compute the historical snow height (HST)
hst = compute_hst(vars['ht'])


# Identify accumulation and dry periods
# Note: You can use either the new or old method for identifying accumulation periods.
dry_indices, accum_indices = identify_accumulation_periods_old(hst)
#labels = identify_accumulation_periods(hst)
#accum_indices = np.where(labels == 1)[0]
#dry_indices = np.where(labels == 0)[0]

# Plot the historical snow height and accumulation/dry periods
plot_hst_and_periods(vars['dates'], hst, dry_indices, accum_indices)

# Extract the dates for dry and accumulation periods
num_events, accum_end_index =get_number_events(accum_indices)
print(f"Number of events detected: {num_events}")

#Track all layers from one event
#DEPRECATED: This function is now replaced by track_all_layers_xr
#ds = track_layers(accum_indices, accum_end_index, 0, vars['id'], vars['rcl'], vars['gt'], vars['rho'], vars['sn38'], vars['ht'], vars['dates'])

#Track all layers for accumulation events periods
ds_accum = track_all_layers_xr(accum_indices, accum_end_index, vars['id'], vars['rcl'], vars['gt'], vars['rho'], vars['sn38'],vars['shear_strength'], vars['ht'], vars['dates'])

# Save ds_accum to a file (e.g., using pickle)
#with open('ds_accum.pkl', 'wb') as f:
#    pickle.dump(ds_accum, f)

#Extract the weak layers by event
#weak_layers_dry = extract_weak_layers_by_event(ds_dry)
weak_layers_accum  = extract_weak_layers_by_event(ds_accum)

# Save weak_layers_accum to a file (e.g., using pickle)
with open('weak_layers_accum.pkl', 'wb') as f:
    pickle.dump(weak_layers_accum, f)

#Plot the weak layers for the entire season
plot_weak_layers_by_event(weak_layers_accum, ds_accum, fig_name='accum_Layers_CCL_layer.png')

#plot the seasonal weak layers 
#plot_grain_type_with_weak_layers(vars, weak_layers_accum)
# Focused zoom example from January 15 to February 10
plot_grain_type_with_weak_layers(
    vars,
    weak_layers_accum,
    start_date='2025-01-15',
    end_date='2025-02-10',
    fig_name='weak_layers_jan_feb.png'
)




