# plotting.py

import matplotlib.pyplot as plt
import matplotlib.dates as mdates, matplotlib.colors as mcolors
import os
import numpy as np
from utils import parse_gtype
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
    #plt.show()
    
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
        "Layer: {}".format(new_layer_dt[0]), #.strftime('%Y-%m-%d')
        (new_layer_dt[0], new_layer_ht[0, 0]), 
        textcoords="offset points", 
        xytext=(10, 10), 
        ha='left', 
        fontsize=12, 
        color='black',
        arrowprops=dict(arrowstyle="->", color='black')
    )

    # Format x-axis for dates
    #plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))
    plt.gcf().autofmt_xdate()
    plt.savefig(os.path.join(FIG_DIR, 'sn38_new_layer.png'), dpi=300, bbox_inches='tight')


def plot_weak_layers_by_event(weak_layers_by_event, hst=None, dates=None, fig_name='Layers_CCL_layer.png'):
    """
    Plot tracked weakest layers by RCL for each event.

    Parameters:
    - weak_layers_by_event: dict, keys are event names, values are dicts with keys:
        'time', 'rcl', 'ht', 'layer_idx'
    - hst: optional, array-like, historical snow height data to overlay
    - dates: optional, array-like, dates corresponding to hst
    """
    plt.figure(figsize=(12, 6))

    for event, data in weak_layers_by_event.items():
        time = data["time"]                     # shape: (time,)
        rcl = data["rcl"]                       # shape: (time,)
        ht = data["ht"]                         # shape: (time,)
        layer_idx = data["layer_idx"]           # scalar

        # Mask invalid RCL values
        rcl_clean = np.where(rcl >= 0, rcl, np.nan)
        ht_clean = np.where(np.isnan(rcl_clean), np.nan, ht)

        # Scatter plot: time vs height colored by RCL
        sc = plt.scatter(time, ht_clean, c=rcl_clean, cmap='viridis', alpha=0.6, linewidth=0.3)

        # Annotate start date of weak layer
        valid_idx = np.where(~np.isnan(rcl_clean))[0]
        if valid_idx.size > 0:
            start_idx = valid_idx[0]
            start_time = time[start_idx]
            start_ht = ht[start_idx]
            # Map layer_idx to G-type label
            gtype_map = {4: "Fx", 5: "DH", 6: "SH"}
            gtype_label = gtype_map.get(layer_idx, f"L{layer_idx}")
            plt.annotate(
                f"{np.datetime_as_string(start_time, unit='D')} ({gtype_label})",
                (start_time, start_ht),
                textcoords="offset points",
                xytext=(-70, 20),
                ha='center',
                fontsize=12,
                color='black',
                arrowprops=dict(arrowstyle='->', color='black', lw=0.5)
            )

    # Optional: overlay HST line (if available)
    if hst is not None and dates is not None:
        try:
            plt.plot(dates, hst, 'k--', label='HST', linewidth=1.5)
        except Exception as e:
            print("Warning: Could not plot HST line —", e)

    # Final formatting
    plt.title("Tracked Weakest Layers by RCL")
    plt.xlabel("Time")
    plt.ylabel("Height (m)")
    plt.xticks(rotation=45)
    plt.colorbar(sc, label="RCL (m)")
    plt.grid(True)
    plt.tight_layout()
    plt.savefig(os.path.join(FIG_DIR, fig_name), dpi=300, bbox_inches='tight')



def plot_grain_type_with_weak_layers(
    vars,
    weak_layers_by_event,
    fig_name='grain_and_weak_layers.png',
    start_date=None,
    end_date=None
):
    """
    Plot grain type pcolormesh with overlaid weak layers shaded by RCL.

    Parameters:
    - vars: dict with keys 'dates', 'ht', 'gt'
    - weak_layers_by_event: dict of weak layer tracking info
    - fig_name: filename to save the figure
    - start_date, end_date: optional datetime64-like bounds
    """

    # --- Normalize dates ---
    all_dates = np.array(vars['dates'], dtype='datetime64[ns]')
    hst = np.array(vars['ht'], dtype=object)
    Z = np.array(vars['gt'], dtype=object)

    # Convert start/end dates to datetime64
    if start_date is not None:
        start_date = np.datetime64(start_date, 'ns')
    else:
        start_date = all_dates[0]

    if end_date is not None:
        end_date = np.datetime64(end_date, 'ns')
    else:
        end_date = all_dates[-1]

    # Filter date window
    mask = (all_dates >= start_date) & (all_dates <= end_date)
    datetime = all_dates[mask]
    hst = hst[mask]
    Z = Z[mask]

    if len(datetime) == 0:
        raise ValueError("No data in selected date window.")

    deltat = datetime[1] - datetime[0]

    # Grain type colormap
    cgt = ['greenyellow', 'darkgreen', 'pink', 'lightblue', 'blue', 'magenta', 'red', 'cyan', 'lightblue']
    cmap_gt = mcolors.ListedColormap(cgt)
    vmin = 0.5
    vmax = len(cmap_gt.colors) + 0.5

    fig, ax = plt.subplots(figsize=(14, 7))

    # --- Plot grain type pcolormesh ---
    for cnt, dt in enumerate(datetime):
        if cnt >= len(hst) or cnt >= len(Z):
            continue

        heights = hst[cnt]
        values,_,_ = parse_gtype(Z[cnt])

        if len(heights) < 2 or len(values) != len(heights) - 1:
            continue

        x_edges = [dt, dt + deltat]
        y_edges = heights

        X, Y = np.meshgrid(x_edges, y_edges)
        C = np.array(values).reshape(len(y_edges) - 1, 1)

        try:
            cb = ax.pcolormesh(X, Y, C, cmap=cmap_gt, vmin=vmin, vmax=vmax, shading='flat', alpha=0.9)
        except Exception as e:
            print(f"Error at time index {cnt}: {e}")
            continue

    # --- Overlay weak layers ---
    for event, data in weak_layers_by_event.items():
        time = np.array(data["time"], dtype='datetime64[ns]')
        rcl = np.array(data["rcl"])
        ht = np.array(data["ht"])
        layer_idx = data["layer_idx"]

        # Filter to current time window
        time_mask = (time >= start_date) & (time <= end_date)
        if not np.any(time_mask):
            continue

        rcl_clean = np.where(rcl >= 0, rcl, np.nan)
        ht_clean = np.where(np.isnan(rcl_clean), np.nan, ht)

         # Scatter plot: time vs height colored by RCL
        plt.plot(time, ht_clean, linewidth=1, color='black', alpha=0.7)


        rcl_vals = rcl_clean[time_mask]
        ht_vals = ht_clean[time_mask]
        t_vals = time[time_mask]

        if len(rcl_vals) == 0:
            continue

        # Only consider RCL values < 1
        valid_mask = (rcl_vals < 1) & ~np.isnan(rcl_vals)
        if not np.any(valid_mask):
            continue  # Skip if nothing to plot

        rcl_sub = rcl_vals[valid_mask]
        ht_sub = ht_vals[valid_mask]
        t_sub = t_vals[valid_mask]

        # Normalize and invert RCL (0 = dark, just below 1 = light)
        normed_rcl = rcl_sub / 1.0  # Since we know max = 1
        alphas = 1.0 - normed_rcl

        # Plot only points with RCL < 1
        for t, h, a in zip(t_sub, ht_sub, alphas):
            if np.isnan(h): continue
            ax.plot(t, h, marker='o', color='black', markersize=10, alpha=a, label='_nolegend_')
            


        # Annotate first valid point
        valid_idx = np.where(~np.isnan(rcl_vals))[0]
        if valid_idx.size > 0:
            start_idx = valid_idx[0]
            start_time = t_vals[start_idx]
            start_ht = ht_vals[start_idx]
            gtype_map = {4: "Fx", 5: "DH", 6: "SH"}
            gtype_label = gtype_map.get(layer_idx, f"L{layer_idx}")

            ax.annotate(
                f"{np.datetime_as_string(start_time, unit='D')} ({gtype_label})",
                (start_time, start_ht),
                textcoords="offset points",
                xytext=(-70, 20),
                ha='center',
                fontsize=10,
                color='black',
                arrowprops=dict(arrowstyle='->', color='black', lw=0.5)
            )

    # --- Plot snow surface height line ---
    try:
        hst_surface = [ht[-1] for ht in hst if len(ht) > 0]
        valid_dates = [datetime[i] for i in range(len(hst)) if len(hst[i]) > 0]
        ax.plot(valid_dates, hst_surface, 'k--', label='HST surface', linewidth=1.5)
    except Exception as e:
        print("Warning: Could not plot HST line —", e)

    # --- Final plot formatting ---
    fig.colorbar(cb, ax=ax, label="Grain Type")
    ax.set_xlabel("Time")
    ax.set_ylabel("Height (m)")
    ax.set_title("Grain Type and Tracked Weak Layers (Shaded by RCL)")
    ax.grid(True)
    fig.autofmt_xdate()

    # Custom legend for weak layers and HST
    #ax.legend(handles=[
    #    Patch(color='black', alpha=1.0, label='Low RCL (critical)'),
    #    Patch(color='black', alpha=0.3, label='High RCL (less critical)'),
    #    Patch(color='black', linestyle='--', label='HST surface')
    #])

    plt.tight_layout()
    plt.savefig(os.path.join(FIG_DIR, fig_name), dpi=300, bbox_inches='tight')
    plt.show()

def plot_grain_type_pcolormesh_with_parser(
    vars,
    fig_name='grain_type_plot.png',
    start_date=None,
    end_date=None,
    show=True
):
    """
    Plot grain type pseudocolor mesh using parsed grain type data.

    Parameters:
    - vars: dict with 'dates', 'ht', 'gt'
    - parse_gtype: function that extracts grain type ID array from Z[cnt]
    - fig_name: output filename
    - start_date, end_date: optional ISO date strings or datetime objects
    - show: whether to display the plot
    """

    datetime = np.array(vars['dates'], dtype='datetime64[ns]')
    hst = np.array(vars['ht'], dtype=object)
    Z = np.array(vars['gt'], dtype=object)

    # Convert and apply date mask
    if start_date is not None:
        start_date = np.datetime64(start_date, 'ns')
    else:
        start_date = datetime[0]

    if end_date is not None:
        end_date = np.datetime64(end_date, 'ns')
    else:
        end_date = datetime[-1]

    mask = (datetime >= start_date) & (datetime <= end_date)
    datetime = datetime[mask]
    hst = hst[mask]
    Z = Z[mask]

    if len(datetime) == 0:
        raise ValueError("No data in selected time range.")

    deltat = datetime[1] - datetime[0]

    # Color map for grain types
    cgt = ['greenyellow', 'darkgreen', 'pink', 'lightblue', 'blue',
           'magenta', 'red', 'cyan', 'lightblue']
    cmap = mcolors.ListedColormap(cgt)
    vmin = 0.5
    vmax = len(cmap.colors) + 0.5

    fig, ax = plt.subplots(figsize=(12, 6))

    for cnt, dt in enumerate(datetime):
        if cnt >= len(hst) or cnt >= len(Z):
            continue

        heights = hst[cnt]
        try:
            values, _, _ = parse_gtype(Z[cnt])
        except Exception as e:
            print(f"parse_gtype failed at index {cnt}: {e}")
            continue

        if len(heights) < 2 or len(values) != len(heights) - 1:
            print(f"Skipping invalid entry at {cnt}")
            continue

        x_edges = [dt, dt + deltat]
        y_edges = heights
        X, Y = np.meshgrid(x_edges, y_edges)
        C = np.array(values).reshape(len(y_edges) - 1, 1)

        try:
            cb = ax.pcolormesh(X, Y, C, cmap=cmap, vmin=vmin, vmax=vmax,
                               shading='flat', alpha=0.9)
        except Exception as e:
            print(f"Error at time index {cnt}: {e}")
            continue

    ax.set_xlabel("Date")
    ax.set_ylabel("Height [m]")
    ax.set_title("Time-Height Grain Type Pseudocolor Mesh")
    fig.colorbar(cb, ax=ax, label="Grain Type")
    fig.autofmt_xdate()
    plt.tight_layout()
    plt.savefig(os.path.join(FIG_DIR, fig_name), dpi=300, bbox_inches='tight')

    if show:
        plt.show()
    else:
        plt.close()

