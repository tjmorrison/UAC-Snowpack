## This script contains two functions

#' Calculate proportion of unstable grid points in ValidationDataSet VD
#'
#' This function can compute the proportion for the vframe data.frame, or for the poorWKL data.frame.
#' In the vframe case, it identifies all captured and unstable layers that belong to each vframe row
#' and then computes their proportion relative to all grid points in the same subset. In the poorWKL
#' case, the function identifies all unstable layers that were not of concern, and computes their
#' proportion for each day and each model-derived date tag relative to all grid points at that day.
#' @param VD ValidationDataSet VD as obtained from [getVdata] from data base
#' @param target compute proportion for vframe, poorWKL or both?
#' @export
calculateProportionUnstable <- function(VD, target = c("vframe", "poorWKL")[1]) {

  if ("vframe" %in% target & nrow(VD$vframe) > 0) {
    if (class(VD$config$poorWKLconstraint) == "call") VD$config$poorWKLconstraint <- eval(VD$config$poorWKLconstraint)
    if (!grepl("AND gtype NOT IN ..PP....DF", VD$config$poorWKLconstraint)) stop("This function is currently only implemented for neglecting PP/DF as poorWKLs. Looks like you have a different rule in your VData --> update code!")
    if (VD$config$poorWKLconstraint ==  "pu >= 0.77 AND gtype NOT IN ('PP', 'DF')") {
      instability_rule <- "pu_noPP"
    } else if (VD$config$poorWKLconstraint %in% c("rta >= 0.8 AND sk38 <= 1.0 AND rc <= 0.4 AND gtype NOT IN ('PP', 'DF')",
                                                  "rta >= 0.8 AND sk38 <= 1.0 AND rc <= 0.3 AND gtype NOT IN ('PP', 'DF')")) {
      instability_rule <- "rtask38rc_noPP"
    } else {
      stop("This function is currently only implemented for two specific layer instability rules (check source code!). Looks like you have a different rule in your VData --> update code!")
    }

    VD$vframe$proportion_unstable <- NA
    if (instability_rule == "pu_noPP") {
      for (vfuid in VD$vframe$vf_uuid) {
        VD$vframe$proportion_unstable[VD$vframe$vf_uuid == vfuid] <- min(length(unique(VD$captWKL$vstation_id[VD$captWKL$vf_uuid == vfuid & VD$captWKL$pu >= 0.77])) / VD$vframe$nSP[VD$vframe$vf_uuid == vfuid],
                                                                         1)  # limit to 100 % in case poorWKLs are transacted to captWKLs for a wkl with a MAX_ELEV range!
      }
    } else if (instability_rule == "rtask38rc_noPP") {
      for (vfuid in VD$vframe$vf_uuid) {
        VD$vframe$proportion_unstable[VD$vframe$vf_uuid == vfuid] <- min(length(unique(VD$captWKL$vstation_id[VD$captWKL$vf_uuid == vfuid & VD$captWKL$rta >= 0.8 & VD$captWKL$sk38 <= 1.0 & VD$captWKL$rc <= 0.4])) / VD$vframe$nSP[VD$vframe$vf_uuid == vfuid],
                                                                         1)  # limit to 100 % in case poorWKLs are transacted to captWKLs for a wkl with a MAX_ELEV range!
      }
    }

  }
  if ("poorWKL" %in% target & nrow(VD$poorWKL) > 0) {
    if ("datetag" %in% names(VD$poorWKL)) {
      VD$poorWKL$proportion_unstable <- NA
      for (execid in unique(VD$poorWKL$exec_uuid)) {
        for (vdt in unique(VD$poorWKL$vdate)) {
          for (dtag in unique(VD$poorWKL$datetag)) {
            VD$poorWKL$proportion_unstable[VD$poorWKL$exec_uuid == execid & VD$poorWKL$vdate == vdt & VD$poorWKL$datetag == dtag] <-
              length(unique(VD$poorWKL$vstation_id[VD$poorWKL$exec_uuid == execid & VD$poorWKL$vdate == vdt & VD$poorWKL$datetag == dtag])) / suppressWarnings(max(VD$poorWKL$nSP[VD$poorWKL$exec_uuid == execid & VD$poorWKL$vdate == vdt & VD$poorWKL$datetag == dtag], na.rm = TRUE))
          }
        }
      }
    } else {
      warning("Need poorWKL$datetag to compute proportion_unstable!")
    }

  }

  return(VD)
}


#' Calculate proportion of structurally captured grid points in ValidationDataSet VD
#' Same as [calculateProportionUnstable] but for structurally captured layers only. Hence, this function
#' is only applied to the vframe data.frame!
#' @param VD ValidationDataSet VD as obtained from [getVdata] from data base
#' @export
calculateProportionCaptured <- function(VD) {

  for (vfuid in VD$vframe$vf_uuid) {
    VD$vframe$dist[VD$vframe$vf_uuid == vfuid] <- min(length(unique(VD$captWKL$vstation_id[VD$captWKL$vf_uuid == vfuid])) / VD$vframe$nSP[VD$vframe$vf_uuid == vfuid],
                                                      1)  # limit to 100 % in case poorWKLs are transacted to captWKLs for a wkl with a MAX_ELEV range!
  }

  return(VD)
}
