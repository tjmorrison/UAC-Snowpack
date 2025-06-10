# main.py

from config import PRO_FILE, FIG_DIR
from data_loader import load_profile, extract_variables
from layer_tracking import compute_hst, identify_accumulation_periods
from plotting import plot_hst_and_periods


pro = load_profile(PRO_FILE)
vars = extract_variables(pro)

hst = compute_hst(vars['ht'])
dry_indices, accum_indices = identify_accumulation_periods(hst)

plot_hst_and_periods(vars['dates'], hst, dry_indices, accum_indices)
