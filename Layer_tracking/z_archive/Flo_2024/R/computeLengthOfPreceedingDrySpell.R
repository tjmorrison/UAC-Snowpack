#' Compute median simulated length of dry period before burial of a layer
#' This function can get called from the large evaluation script `evaluate_main.R` (by setting some config flags in the beginning of the script)
#' For each layer of concern, this function assembles the median length of the dry period before the burial of the layer.
#' To do so, it uses the information about the length of the search window that got computed for each layer of concern at each grid point
#' (during `validate_main`) and stored in VD$captWKL$tliml.
#' @param VD ValidationDataSet VD as obtained from [getVdata] from data base
#' @return same VD object with variable `drySpell_median` and `*_std` added to VD$vframe.
#' @export
computeLengthOfPreceedingDrySpell <- function(VD) {

  if (!"captWKL" %in% names(VD)) stop("Cannot compute gsizeAtBurial: captWKL information missing!")
  VD$vframe$drySpell_median <- NA
  VD$vframe$drySpell_std <- NA

  ## Loop over wkl_uuids in vframe:
  for (wid in unique(VD$captWKL$wkl_uuid[!is.na(VD$captWKL$tliml)])) {
    ## aggregate individual grid points first (could use unique, but median safer?!)
    tliml <- aggregate(tliml ~ factor(vstation), data = VD$captWKL[VD$captWKL$wkl_uuid %in% wid, ], median, simplify = FALSE)
    ## aggregate all grid points by median and std
    VD$vframe$drySpell_median[VD$vframe$wkl_uuid %in% wid] <- median(unlist(tliml$tliml), na.rm = TRUE)
    VD$vframe$drySpell_std[VD$vframe$wkl_uuid %in% wid] <- sd(unlist(tliml$tliml), na.rm = TRUE)
  }

  return(VD)
}
