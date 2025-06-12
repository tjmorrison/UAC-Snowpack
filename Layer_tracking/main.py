# main.py

from config import PRO_FILE
from data_loader import load_profile, extract_variables
from utils import compute_hst, identify_accumulation_periods, track_layers
from plotting import plot_hst_and_periods, plot_seasonal_sn38


pro = load_profile(PRO_FILE)
vars = extract_variables(pro)

hst = compute_hst(vars['ht'])
dry_indices, accum_indices = identify_accumulation_periods(hst)

plot_hst_and_periods(vars['dates'], hst, dry_indices, accum_indices)
new_layer_sn38, new_layer_ht, new_layer_dt, new_layer_id, new_layer_rcl, new_layer_gt, new_layer_rho, new_ids = track_layers(
    accum_indices,
    vars['id'],
    vars['rcl'],
    vars['gt'],
    vars['rho'],
    vars['sn38'],
    vars['ht'],
    vars['dates']
)
# Note: The track_layers function is currently not fully implemented and will need further development.

plot_seasonal_sn38(new_layer_dt, new_layer_ht, new_layer_sn38, vars['dates'], hst)