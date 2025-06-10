# plotting.py

import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import os
from config import FIG_DIR

def plot_hst_and_periods(dates, hst, dry_indices, accum_indices):
    plt.figure(figsize=(12, 6))
    plt.plot(dates, hst, 'k--', linewidth=2)
    plt.scatter([dates[i] for i in dry_indices], hst[dry_indices], 5, "r", label='Dry')
    plt.scatter([dates[i] for i in accum_indices], hst[accum_indices], 5, "b", label='Accumulation')
    plt.legend()
    plt.xlabel("Date")
    plt.ylabel("Snow Height (cm)")
    plt.title("Snow Accumulation and Dry Periods")
    plt.grid(True, linestyle='--', alpha=0.5)
    plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))
    plt.gcf().autofmt_xdate()
    plt.tight_layout()
    plt.savefig(os.path.join(FIG_DIR, 'hst_and_periods.png'), dpi=300, bbox_inches='tight')
    plt.show()
    
