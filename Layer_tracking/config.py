# config.py

import os

# Base path to project directory (optional)
BASE_DIR = os.path.dirname(os.path.abspath(__file__))


# Snowpack .pro file path
PRO_FILE = os.path.join(BASE_DIR, 'Validation_profiles', 'ATH20_10_05_2024_03_27_2025.pro')

# Output directory for figures
FIG_DIR = os.path.join(BASE_DIR, 'figures')

# Model sampling interval in minutes (used in accumulation logic)
MODEL_INTERVAL_MINUTES = 15

# Threshold for determining "dry" period (in cm)
DRY_THRESHOLD_CM = 10

# Accumulation window in hours
ACCUM_PERIOD_HOURS = 24

# Grain type codes for surface hoar, facets, etc.
GRAIN_CODES = {
    'surface_hoar': 4,
    'depth_hoar': 5,
    'facets': 6
}

# Plotting settings
PLOT_STYLE = 'seaborn-darkgrid'
