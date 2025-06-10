#'
#' @export
interactivelyCleanupWklList <- function(WklList) {

  ## subfunction
  read_prompt <- function(keep) {
    if (tolower(substr(keep, 1, 1)) %in% c("0", "n", "f")) {
      return(FALSE)
    } else if (tolower(substr(keep, 1, 1)) %in% c("1", "y", "t")) {
      return(TRUE)
    } else {
      print("Please answer by 0/n/F (for No), or 1/y/T (for Yes):")
      return(NA)
    }
  }

  as_WklList <- function(WklElement) {
    class(WklElement) <- "WklList"
    return(WklElement)
  }

  for (i in seq_along(WklList)) {  # LOOP over each PWL contained in WklList

    print(paste0(i, "/", length(WklList), " weak layers addressed."))
    summary(as_WklList(WklList[i]))
    for (attempt in seq(3)) {
      keep <- readline("Keep PWL for validation? ")
      keep <- read_prompt(keep)
      if (!is.na(keep)) break
    }
    if (is.na(keep)) stop("Three consecutive unvalid user prompts!")

    if (!keep) {
      WklList[[i]] <- NA  # discard Wkl layer from WklList
    } else {  # keep Wkl layer
      ## need to add/adjust ...$ASSESS$GRAIN_TYPE?
      change_gt1 <- readline("Change (primary) grain type? ")
      if (!tolower(substr(change_gt1, 1, 1)) %in% c("", "n", "0", "f")) {
        WklList[[i]]$ASSESS$GRAIN_TYPE <- change_gt1
      }

      change_gt2 <- readline("Change or add secondary grain type? ")
      if (!tolower(substr(change_gt1, 1, 1)) %in% c("", "n", "0", "f")) {
        WklList[[i]]$ASSESS$GRAIN_TYPE_SECONDARY <- change_gt2
      }
    }

  }  # END LOOP over each PWL within WklList

  WklList <- WklList[!is.na(WklList)]
  class(WklList) <- "WklList"
  return(WklList)

}
