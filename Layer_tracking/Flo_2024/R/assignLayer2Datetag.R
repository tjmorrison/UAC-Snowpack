#' Assign a layer to most representative datetag
#'
#' @description
#' If you have a vector of potential date tags as obtained by [derivePotentialDatetags], this function will
#' assign simulated layers to the optimal date tag based on following rules for layer assignment:
#'
#'  * from all datetags that are older than the layer's ddate
#'  * the datetag that is closest to the layer's bdate will be assigned
#'
#' If the layer formed after the oldest datetag that was provided, only rule 2 applies.
#'
#' Note that if the layer is not buried yet (surface layer), the function will use the vdate (validation date, essentially the date of the profile) instead.
#' The vdate only exists for layers in the wklvalidation database, though, and the profile date cannot be queried by this function. Therefore,
#' if the function is used outside of the package context, assign `layer$vdate` as the profile date before calling this function!
#'
#' @param layer snowprofileLayers-like object of one or multiple potential weak layers (with their ddate and bdate)
#' @param potentialDatetags array of Dates (as obtained by [derivePotentialDatetags])
#' @param silent if set to FALSE, the function will warn you if datetags seem extremely inappropriate for the layers' bdates
#'
#' @return This function will fill `layer$datetag` with the appropriate datetag
#'
#' @export
assignLayer2Datetag <- function(layer, potentialDatetags, silent = FALSE) {
  if (nrow(layer) > 0) {
    layer$datetag <- as.Date(NA)
    tmpflag <- rep(FALSE, times = nrow(layer))
    for (i in seq(nrow(layer))) {

      ## copy and modify bdate in case some layers don't have any bdates yet
      bdate <- as.Date(layer$bdate[i])
      if (is.na(bdate)) bdate <- as.Date(layer$vdate[i])

      ## first attempt: (1) ddaterule + (2) closest bdate
      ddaterule <- as.Date(potentialDatetags[potentialDatetags >= as.Date(layer$ddate[i])])
      layer$datetag[i] <- as.Date(ddaterule[which.min(abs(ddaterule - bdate))])

      ## second attempt: (1) closest bdate
      if (is.na(layer$datetag[i])) {
        layer$datetag[i] <- as.Date(potentialDatetags[which.min(abs(potentialDatetags - bdate))])
      }

      ## flag when bdate and datetags are further apart than 3 weeks, or when datetag is more than one week earlier than ddate
      if (abs(layer$datetag[i] - bdate) > 21 | (layer$datetag[i] - as.Date(layer$ddate[i])) < -7) {
        if (!is.na(layer$bdate[i])) tmpflag[i] <- TRUE
      }
    }

    ## stop if still is NA
    if (any(is.na(layer$datetag))) stop("Datetag(s) can't be assigned, check why!")
    ## raise warnings for flagged layers
    if (!silent) {
      if (any(tmpflag)) {
        warning(paste0(sum(tmpflag), " out of ", nrow(layer), " layers are potentially assigned to inappropriate datetags. \nrelevant datetag(s): ",
                       paste0(unique(layer$datetag[tmpflag]), collapse = ", "), "\nrelevant indices: ", paste0(which(tmpflag), collapse = ", ")))
      }
    }
  }

  return(layer)
}
