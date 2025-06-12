# utils.py
import numpy as np

def compute_hst(ht_series):
    return np.array([np.max(ht) for ht in ht_series])

def identify_accumulation_periods(hst, threshold_cm=10, interval_min=15):
    interval = int(24 * (60 / interval_min))  # 24 hours
    diff_hst = hst[interval:] - hst[:-interval]
    dry_indices = np.where(diff_hst < threshold_cm)[0]
    accum_indices = np.setdiff1d(np.arange(len(diff_hst)), dry_indices)
    return dry_indices, accum_indices

def track_layers(accum_indices, id, rcl, gt, rho, sn38, ht, dates):
    #determine burial date
    accum_end_index = np.where(np.diff(accum_indices)>1)[0]
    #number of event total, dry and accum
    num_events = len(accum_end_index) #36 events

    #loop over each event
    #for .. 
    # track 1 layer for now

    idx = 12 # We will need to loop over this for each layer 
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
        #print(ii)
        #print(id[ii])
        #print(new_layer_id)

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
        #depth
        #density
        #grain types


        cnt +=1

    # Return the new layer data
    return new_layer_sn38, new_layer_ht, new_layer_dt, new_layer_id, new_layer_rcl, new_layer_gt, new_layer_rho, new_ids
