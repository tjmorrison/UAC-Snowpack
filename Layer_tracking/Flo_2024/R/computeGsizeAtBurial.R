#' Compute median simulated grain size of layer at burial
#' This function can get called from the large evaluation script `evaluate_main.R` (by setting some config flags in the beginning of the script)
#' For each group of layers associated with a date tag it traces back the layers before their burial and then computes their median grain sizes.
#' Extremely CPU intense!!
#' @param VD ValidationDataSet VD as obtained from [getVdata] from data base
#' @return same VD object with variable `gsizeAtBurial_median` added to VD$vframe.
#' @export
computeGsizeAtBurial <- function(VD) {

  if (!"captWKL" %in% names(VD)) stop("Cannot compute gsizeAtBurial: captWKL information missing!")
  VD$vframe$gsizeAtBurial_median <- NA

  ## Loop over wkl_uuids in vframe:
  for (wid in unique(VD$captWKL$wkl_uuid)) {
    gtypeclass <- unique(VD$vframe$gtype_class[VD$vframe$wkl_uuid %in% wid])
    gtyperank <- unique(VD$vframe$gtype_rank[VD$vframe$wkl_uuid %in% wid])

    ## differentiate gtype ranks
    if (gtyperank != "tertiary") {
      if (gtyperank == "primary") {
        gsizes <- sapply(unique(VD$captWKL$vstation_id[VD$captWKL$wkl_uuid %in% wid]), function(vstatid) {
          ## 1. get latest bdate for each station for all captWKLs at that station
          latestBdate <- suppressWarnings(as.Date(max(VD$captWKL$bdate[VD$captWKL$wkl_uuid %in% wid & VD$captWKL$vstation_id %in% vstatid & VD$captWKL$gtype %in% gtypeclass], na.rm = TRUE)))
          ## 2. return max gsize found at that bdate
          suppressWarnings(max(VD$captWKL$gsize[VD$captWKL$wkl_uuid %in% wid & VD$captWKL$vstation_id %in% vstatid & VD$captWKL$gtype %in% gtypeclass & VD$captWKL$vdate == latestBdate]))
        })
      } else if (gtyperank == "secondary") {
        gsizes <- sapply(unique(VD$captWKL$vstation_id[VD$captWKL$wkl_uuid %in% wid]), function(vstatid) {
          ## 1. get latest bdate for each station for all captWKLs at that station
          latestBdate <- suppressWarnings(as.Date(max(VD$captWKL$bdate[VD$captWKL$wkl_uuid %in% wid & VD$captWKL$vstation_id %in% vstatid], na.rm = TRUE)))
          ## 2. return max gsize found at that bdate
          suppressWarnings(max(VD$captWKL$gsize[VD$captWKL$wkl_uuid %in% wid & VD$captWKL$vstation_id %in% vstatid & VD$captWKL$vdate == latestBdate]))
        })
      }
      gsizes[is.infinite(gsizes)] <- NA
      median_gsizes <- median(gsizes, na.rm = TRUE)
      if (sum(is.na(gsizes)) > 0.5*length(gsizes)) median_gsizes <- NA
      VD$vframe$gsizeAtBurial_median[VD$vframe$wkl_uuid %in% wid] <- median_gsizes
    }
  }


  return(VD)
}
