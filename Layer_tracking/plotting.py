# plotting.py

import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import os
import numpy as np
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
    

def plot_seasonal_sn38(new_layer_dt, new_layer_ht, new_layer_sn38, dates, hst):
    """
    Plot Sn38 for a new layer over time.
    
    Parameters:
    - new_layer_dt: Dates for the new layer.
    - new_layer_ht: Heights for the new layer.
    - new_layer_sn38: Sn38 values for the new layer.
    - dates: All dates in the dataset.
    - hst: Historical snow height data.
    """
    
    # Create figure and plot space
    plt.figure(figsize=(12, 6))
    plt.plot(dates, hst, 'k--', linewidth=2)
    scatter = None
    for ii in range(new_layer_sn38.shape[1]):
        # Set alpha to 0.2 where sn38 > 1, else 0.8
        alphas = np.where(new_layer_sn38[:, ii] > 1, 0.05, 1)
        scatter = plt.scatter(
            new_layer_dt,
            new_layer_ht[:, ii],
            c=new_layer_sn38[:, ii],
            s=50,
            cmap='viridis',
            marker='o',
            alpha=alphas,
            edgecolors='none'
        )
        plt.clim([0, 2])
    plt.colorbar(scatter, label='Sn38')
    plt.clim([0, 2])
    #plt.ylim([0, 300])
    plt.xlabel("Date", fontsize=12)
    plt.ylabel("Height (cm)", fontsize=12)
    plt.title("Sn38 for New Layer ID Over Time", fontsize=14)
    plt.grid(True, linestyle='--', alpha=0.5)
    plt.tight_layout()

    # Annotate the start of the scatter
    plt.annotate(
        "Layer: {}".format(new_layer_dt[0].strftime('%Y-%m-%d')), 
        (new_layer_dt[0], new_layer_ht[0, 0]), 
        textcoords="offset points", 
        xytext=(10, 10), 
        ha='left', 
        fontsize=12, 
        color='black',
        arrowprops=dict(arrowstyle="->", color='black')
    )

    # Format x-axis for dates
    plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))
    plt.gcf().autofmt_xdate()
    plt.savefig(os.path.join(FIG_DIR, 'sn38_new_layer.png'), dpi=300, bbox_inches='tight')