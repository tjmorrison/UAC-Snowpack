# utils.py
import numpy as np
import xarray as xr
import pandas as pd

def compute_hst(ht_series):
    return np.array([np.max(ht) for ht in ht_series])
""
def identify_accumulation_periods_old(hst, threshold_cm=10, interval_min=15):
    """Identify accumulation periods in the HST data.

    DEPRECATED: This function is kept for backward compatibility but should not be used in new code.

    Args:
        hst (np.ndarray): The HST data array.
        threshold_cm (int, optional): The threshold for accumulation in cm. Defaults to 10.
        interval_min (int, optional): The time interval in minutes. Defaults to 15.

    Returns:
        Tuple[np.ndarray, np.ndarray]: The dry and accumulation indices.
    """
    interval = int(24 * (60 / interval_min))  # 24 hours
    diff_hst = hst[interval:] - hst[:-interval]
    dry_indices = np.where(diff_hst < threshold_cm)[0]
    accum_indices = np.setdiff1d(np.arange(len(diff_hst)), dry_indices)
    return dry_indices, accum_indices
""




def identify_accumulation_periods(hst, threshold_cm=10, interval_min=15, window_hours=24, min_accum_periods=2):
    """
    Identify accumulation and dry periods in snowpack HST time series.

    Args:
        hst (np.ndarray): 1D array of HST data.
        threshold_cm (float): Threshold for significant accumulation (cm).
        interval_min (int): Sampling interval in minutes.
        window_hours (int): Rolling window in hours for accumulation (default 24h).
        min_accum_periods (int): Minimum consecutive periods to define an accumulation event.

    Returns:
        np.ndarray: Array of labels (0 = dry, 1 = accumulation)
    """
    hst = pd.Series(hst).interpolate(limit_direction='both').fillna(method='bfill').fillna(method='ffill')  # handle missing
    periods = int(window_hours * 60 / interval_min)
    diff = hst.diff().fillna(0)
    
    # Use rolling sum of positive increments
    accum_signal = diff.where(diff > 0, 0).rolling(periods, min_periods=1).sum()
    accum_period = accum_signal > threshold_cm

    # Optional: require a minimum number of consecutive accumulation periods
    from scipy.ndimage import label
    structure = np.ones(3)  # consider neighboring points
    accum_labels, num_features = label(accum_period.values, structure=structure)
    cleaned_accum = np.zeros_like(accum_labels, dtype=bool)
    for i in range(1, num_features + 1):
        indices = np.where(accum_labels == i)[0]
        if len(indices) >= min_accum_periods:
            cleaned_accum[indices] = True

    labels = np.where(cleaned_accum, 1, 0)
    return labels


def parse_gtype(grain_code):
    """
    Parse the grain type code into its components.

    Swiss Code F1F2F3, for more information on data formats see : https://snowpack.slf.ch/doc-release/html/snowpackio.html
    With F1 as the primary grain type, F2 as the secondary grain type, and F3 as the tertiary grain type.

    Args:
        grain_code (int): The grain type code.

    Returns:
        Tuple[int, int, int]: The parsed components of the grain type code.
    """
    try:
        return grain_code // 100, (grain_code % 100) // 10, grain_code % 10
    except Exception:
        print("Failed to parse grain code")
        return None, None, None
    
def get_number_events(accum_indices):
    """Get the number of events from the accumulation indices.

    Args:
        accum_indices (np.ndarray): The accumulation indices.

    Returns:
        int: The number of events.
    """
    #determine burial date
    accum_end_index = np.where(np.diff(accum_indices)>1)[0]
    #number of event total, dry and accum
    num_events = len(accum_end_index) #36 events
    return num_events, accum_end_index

def track_layers(accum_indices,accum_end_index,idx, id, rcl, gt, rho, sn38, ht, dates):
    """
    DEPRECATED: Track all layers in the HST data.
  

    Args:
        accum_indices (np.ndarray): The accumulation indices.
        accum_end_index (np.ndarray): The accumulation end indices.
        idx (int): The index of the event to track.
        id (np.ndarray): The layer IDs.
        rcl (np.ndarray): The critical layer thickness.
        gt (np.ndarray): The grain type.
        rho (np.ndarray): The density.
        sn38 (np.ndarray): The snow height.
        ht (np.ndarray): The height.
        dates (np.ndarray): The dates.

    Returns:
        pd.DataFrame: The dataset for the tracked event.
    """

    num_new_elements = len(id[accum_indices[accum_end_index[idx]]]) - len(id[accum_indices[accum_end_index[idx-1]]])
    #print(num_new_elements)
    # get the last num new elements id
    new_layer_id = id[accum_indices[accum_end_index[idx]]][-num_new_elements:]
    #determine the max number of indices the lay could exist 
    new_layer_idex_age = len(dates) - accum_indices[accum_end_index[idx]] #
    #allocate new layer rcl will be each new elements critical cut length for each time step
    new_ids = np.zeros((new_layer_idex_age,num_new_elements))
    new_layer_rcl = np.zeros((new_layer_idex_age,num_new_elements))
    new_layer_rho = np.zeros((new_layer_idex_age,num_new_elements))
    new_layer_gt = np.zeros((new_layer_idex_age,num_new_elements))
    new_layer_sn38 = np.zeros((new_layer_idex_age,num_new_elements))
    new_layer_ht = np.zeros((new_layer_idex_age,num_new_elements))
    new_layer_dt = dates[accum_indices[accum_end_index[idx]]:]
    # Loop over layer intiation (storm) to current index
    start = accum_indices[accum_end_index[idx]]
    # needs to go to end of time -> is this right?
    end = len(dates)
    cnt = 0
    for ii in range(start, end,1):

        # Find intersection and indices in current_ids
        common_ids, pro_layer_idx, _ = np.intersect1d(id[ii], new_layer_id, return_indices=True)

        #Track the critical cut length for each new layer
        padding = [None] * (num_new_elements - rcl[ii][pro_layer_idx].size)
        new_ids[cnt][:] = np.concatenate((id[ii][pro_layer_idx], padding))
        new_layer_rcl[cnt][:] = np.concatenate((rcl[ii][pro_layer_idx], padding))
        new_layer_gt[cnt][:] = np.concatenate((gt[ii][pro_layer_idx], padding))
        new_layer_rho[cnt][:] = np.concatenate((rho[ii][pro_layer_idx], padding))
        new_layer_sn38[cnt][:] = np.concatenate((sn38[ii][pro_layer_idx], padding))
        new_layer_ht[cnt][:] = np.concatenate((ht[ii][pro_layer_idx], padding))
        #new_layer_dt[cnt][:] = np.array([dt[ii]] * num_new_elements)  # Use the same date for all new elements


        #determine weakest layer in the ID'd layer

        #Find relevant variables to track of this variable


        cnt +=1

    # Return the new layer data
    return new_layer_sn38, new_layer_ht, new_layer_dt, new_layer_id, new_layer_rcl, new_layer_gt, new_layer_rho, new_ids



def track_all_layers_xr(accum_indices, accum_end_index, id, rcl, gt, rho, sn38, shear_strength, ht, dates):
    """Track all layers in the HST data.

    Args:
        accum_indices (np.ndarray): The accumulation indices.
        accum_end_index (np.ndarray): The accumulation end indices.
        id (np.ndarray): The layer IDs.
        rcl (np.ndarray): The critical layer thickness.
        gt (np.ndarray): The grain type.
        rho (np.ndarray): The density.
        sn38 (np.ndarray): The snow height.
        ht (np.ndarray): The height.

    Returns:
        xr.Dataset: The dataset for all tracked events.
    """
    all_datasets = []
    num_events = len(accum_end_index)

    for idx in range(num_events):
        try:
            ds_event = track_single_event_xr(
                accum_indices, accum_end_index, idx, id, rcl, gt, rho, sn38, shear_strength, ht, dates
            )
            if ds_event is None:
                print(f"Skipping event {idx}: no new elements or invalid input.")
                continue
            ds_event = ds_event.expand_dims(event=[idx])
            all_datasets.append(ds_event)
        except Exception as e:
            print(f"Skipping event {idx} due to error: {e}")

    if not all_datasets:
        raise ValueError("No valid events to concatenate. Check input data.")

    return xr.concat(all_datasets, dim="event")




def track_single_event_xr(accum_indices, accum_end_index, idx, id, rcl, gt, rho, sn38, shear_strength, ht, dates):
    """Track a single event in the HST data.

    Args:
        accum_indices (np.ndarray): The accumulation indices.
        accum_end_index (np.ndarray): The accumulation end indices.
        idx (int): The index of the event to track.
        id (np.ndarray): The layer IDs.
        rcl (np.ndarray): The critical layer thickness.
        gt (np.ndarray): The grain type.
        rho (np.ndarray): The density.
        sn38 (np.ndarray): The snow height.
        ht (np.ndarray): The height.
        dates (np.ndarray): The dates.

    Returns:
        xr.Dataset: The dataset for the tracked event.
    """
    current_idx = accum_indices[accum_end_index[idx]]
    prev_idx = accum_indices[accum_end_index[idx - 1]] if idx > 0 else 0

    num_new_elements = len(id[current_idx]) - len(id[prev_idx])
    new_layer_ids = id[current_idx][-num_new_elements:]
    age = len(dates) - current_idx

    time_coords = np.array(dates[current_idx:])
    layer_coords = np.arange(num_new_elements)

    # Ensure the arrays are initialized with NaN for missing values
    rcl_arr = np.full((age, num_new_elements), np.nan)
    rho_arr = np.full((age, num_new_elements), np.nan)
    gt_arr = np.full((age, num_new_elements), np.nan)
    sn38_arr = np.full((age, num_new_elements), np.nan)
    shear_strength_arr = np.full((age, num_new_elements), np.nan)
    ht_arr = np.full((age, num_new_elements), np.nan)
    ids_arr = np.full((age, num_new_elements), np.nan)

    # Loop over time steps from current_idx to the end of dates
    for t, ii in enumerate(range(current_idx, len(dates))):
        # Find intersection and indices in current_ids
        common_ids, pro_layer_idx, _ = np.intersect1d(id[ii], new_layer_ids, return_indices=True)
        n_found = len(pro_layer_idx)

        if n_found > 0:
            rcl_arr[t, :n_found] = rcl[ii][pro_layer_idx]
            rho_arr[t, :n_found] = rho[ii][pro_layer_idx]
            gt_arr[t, :n_found] = gt[ii][pro_layer_idx]
            sn38_arr[t, :n_found] = sn38[ii][pro_layer_idx]
            shear_strength_arr[t, :n_found] = shear_strength[ii][pro_layer_idx]
            ht_arr[t, :n_found] = ht[ii][pro_layer_idx]
            ids_arr[t, :n_found] = id[ii][pro_layer_idx]

    coords = {"time": time_coords, "layer": layer_coords}
    return xr.Dataset({
        "sn38": xr.DataArray(sn38_arr, dims=["time", "layer"], coords=coords),
        "ht": xr.DataArray(ht_arr, dims=["time", "layer"], coords=coords),
        "rcl": xr.DataArray(rcl_arr, dims=["time", "layer"], coords=coords),
        "gt": xr.DataArray(gt_arr, dims=["time", "layer"], coords=coords),
        "rho": xr.DataArray(rho_arr, dims=["time", "layer"], coords=coords),
        "layer_id": xr.DataArray(ids_arr, dims=["time", "layer"], coords=coords),
        "shear_strength": xr.DataArray(shear_strength_arr, dims=["time", "layer"], coords=coords)
    })


def extract_weak_layers_by_event(ds_all):
    """
    Extract weak layers by event from the given xarray Dataset.

    Args:
        ds_all (xr.Dataset): The dataset containing all events.

    Returns:
        dict: Dictionary mapping event to tracked weak layer data.
    """
    weak_layers_by_event = {}

    for event in ds_all.event.values:
        ht = ds_all.ht.sel(event=event)
        rcl = ds_all.rcl.sel(event=event)
        sn38 = ds_all.sn38.sel(event=event)
        gt = ds_all.gt.sel(event=event)
        shear_strength = ds_all.shear_strength.sel(event=event)
        time = ds_all.time.values

        # Parse primary grain type
        gt_primary, _, _ = parse_gtype(gt)  # shape: (time, layer)
        
        # compute rc_flat
        #rc_flat_values = np.zeros_like(ht)  # Initialize with zeros
        #print(len(ht))
        #for i in range(len(ht)):
            #print(i)
        #    rc_flat_values[i,:] = rc_flat(ht[i,:], ds_all.rho.sel(event=event)[i,:], sn38[i,:], shear_strength[i,:])

        # 1. Create a mask for weak layer candidates (GT 4, 5, 6)
        weak_mask = ((gt_primary == 4) | (gt_primary == 5) | (gt_primary == 6)) #& (rcl < 0.9)  # Adjusted to include RCL condition

        # 2. Mask out invalid RCLs or only find where weak layer grain types exist 
        # in the future we may want to change rcl to P_unstable or have user defined
        rcl_masked = np.where(weak_mask==True, rcl, np.nan)
        #rcl_masked = np.where(weak_mask==True, rc_flat_values, np.nan)

        # 3. Check if any weak layers exist
        if np.isnan(rcl_masked).all():
            print(f"Event {event}: No valid weak layers (GT=4,5,6)")
            continue

        # 4. Find the (time, layer) index of the minimum RCL from the masked array. This is the layer which has had the lowest RCL value over the entire time series for that event.
        time_idx, layer_idx = np.unravel_index(np.nanargmin(rcl_masked), rcl_masked.shape)

        # 5. Extract the full time series for that layer
        weak_layers_by_event[event] = {
            "rcl": rcl[:, layer_idx].values,
            "ht": ht[:, layer_idx].values,
            "sn38": sn38[:, layer_idx].values,
            "gt": gt_primary[:, layer_idx].values,
            "time": time,
            "layer_idx": layer_idx,
            "min_rcl": rcl[time_idx, layer_idx].item(),
            "shear_strength": shear_strength[:, layer_idx].values
        }
    return weak_layers_by_event


def rc_flat(layer_top, rho, gs, strength_s):
    """Compute the flat-lying weak layer response.

    Args:
        layer_top (_type_): _description_
        rho (_type_): _description_
        gs (_type_): _description_
        strength_s (_type_): _description_

    Returns:
        _type_: _description_
    """
    rho_ice = 917. #kg m-3
    gs_0 = 0.00125 #m
    thick = np.diff(np.concatenate((np.array([0]), layer_top)))
    rho_wl = rho
    gs_wl = gs*0.001 #[m]
    rho_sl = np.append(np.flip(np.cumsum(rho[::-1]*thick[::-1])/np.cumsum(thick[::-1]))[1:len(rho)],np.nan)
    tau_p = strength_s*1000. #[Pa]
    eprime = 5.07e9*(rho_sl/rho_ice)**5.13 / (1-0.2**2) #Eprime = E' = E/(1-nu**2) ; poisson ratio nu=0.2
    dsl_over_sigman = 1. / (9.81 * rho_sl) #D_sl/sigma_n = D_sl / (rho_sl*9.81*D_sl) = 1/(9.81*rho_sl)
    a = 4.6e-9
    b = -2.
    rc_flat = np.sqrt(a*( rho_wl/rho_ice * gs_wl/gs_0 )**b)*np.sqrt(2*tau_p*eprime*dsl_over_sigman)
    return rc_flat
