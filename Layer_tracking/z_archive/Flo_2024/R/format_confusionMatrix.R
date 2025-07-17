#' Format data.frame containing quality/agreement indicators
#'
#' i.e., apply some filtering, calculating/formatting of gtype_classes, etc.
#'
#' @param CM the data.frame as returned by evaluate_main.R section 'deriveConfusionMatrix'
#' @param mergedVD the data.frame as returned by evaluate_main.R section 'mergeVDvframe'
#' @param WKL_SHonly LEGACY: this is only relevant for fherla's wklvalidation project. If you want to replicate the wklvalidation with new data, this feature is already automated by
#' having a column in the processingDB table `wkl` called `SH_only`. This variable will then be used instead of the data.frame provided here, which manually inserts the SH_only flag.
#' (See `scratch.R` for more information about this flag.)
#'
#' @seealso source code in evaluate_main.R
#'
#' @export
#'
format_confusionMatrix <- function(CM, mergedVD = NA, WKL_SHonly = NA) {

  ## remove infinite proportion_captured and forecasted TRUE, since those did not have problems in respective band
  ## i.e., equivalent to removing nPDays == 0
  toDiscard <- which(CM$forecasted & is.infinite(CM$pcapt_max))
  if (length(toDiscard) > 0) CM <- CM[-toDiscard, ]
  ## discard all poor layers with human datetags (remainders of transact routine)
  ## NOTE don't do this when validating aspects! think about how to deal with this!
  toDiscard <- which(!CM$forecasted & CM$datetag %in% CM$datetag[CM$forecasted])
  if (length(toDiscard) > 0) CM <- CM[-toDiscard, ]
  ## fill up NAs proportion_captured at poorWKLs with proportion_unstable
  CM$pcapt_max[is.na(CM$pcapt_max) & !CM$forecasted] <- CM$pu_max[is.na(CM$pcapt_max) & !CM$forecasted]

  if (!isTRUE(is.na(mergedVD))) {
    ## compute gtype_class
    CM$gtype_class <- sapply(seq(nrow(CM)), function(i) {
      tbl <- table(mergedVD$vframe$gtype_class[mergedVD$vframe$wkl_uuid == CM$wkl_uuid[i] & mergedVD$vframe$band == CM$band[i] & mergedVD$vframe$gtype_rank == CM$gtype_rank[i]])
      if (length(tbl) == 0 && CM$gtype_rank[i] == "secondary") tbl <- table(mergedVD$vframe$gtype_class[mergedVD$vframe$wkl_uuid == CM$wkl_uuid[i] & mergedVD$vframe$band == CM$band[i] & mergedVD$vframe$gtype_rank == "primary"])
      if (length(tbl) == 0) {
        NA
      } else if (max(tbl) == 0) {
        NA
      } else {
        names(tbl[1])
      }
    })
    ## remove layers with gtype_class "MF" (only three distinct MF layers in data set!)
    toDiscard <- which(CM$gtype_class == "MF")
    if (length(toDiscard) > 0) CM <- CM[-toDiscard, ]

    ## add SH_only flag from mergedVD$wkl if available
    if ("SH_only" %in% names(mergedVD$wkl)) {
      CM <- merge(CM, mergedVD$wkl[, c("wkl_uuid", "SH_only")])
    }
  } else {
    CM$gtype_class <- NA
  }

  ## add logical flag whether reported layer is solely a SH layer:
  if (!all(is.na(WKL_SHonly))) {
    CM$SH_only <- NA
    CM$SH_only <- sapply(seq(nrow(CM)), function(i) {
      tmp <- WKL_SHonly$SH_only[WKL_SHonly$wkl_uuid == CM$wkl_uuid[i]]
      c(tmp, NA)[1]
    })
  }
  ## adjust gtype_class labels based on exclusiveness of reported SH grain type
  if ("SH_only" %in% names(CM)) {
    ## explicitely name SH/FC mixes
    CM$gtype_class[!CM$SH_only & CM$gtype_class %in% "SH"] <- "SH/FC"
    ## re-name DH layers:
    CM$gtype_class[CM$iscrust & CM$gtype_class %in% "DH"] <- "SH/FC"
    CM$gtype_class[!CM$iscrust & CM$gtype_class %in% "DH"] <- "FC"
  }

  ## format gtype_class of aggregated poorWKLs
  CM$gtype_class[!CM$forecasted] <- sapply(which(!CM$forecasted), function(i) {
    if ("wkl_simulated" %in% names(mergedVD)) {
      k <- which(mergedVD$wkl_simulated$datetag %in% CM$datetag[i] & mergedVD$wkl_simulated$region %in% CM$region[i] &
                   mergedVD$wkl_simulated$band %in% CM$band[i] & mergedVD$wkl_simulated$gtype_rank %in% CM$gtype_rank[i])
      if (length(k) > 0) {
        if (mergedVD$wkl_simulated$gtype_primary[k] %in% "FC" & mergedVD$wkl_simulated$gtype_secondary[k] %in% "SH") {
          gtc <- "FC/SH"
        } else if (mergedVD$wkl_simulated$gtype_primary[k] %in% "SH" & mergedVD$wkl_simulated$gtype_secondary[k] %in% "FC") {
          gtc <- "SH/FC"
        } else {
          gtc <- mergedVD$wkl_simulated$gtype_class[k]
        }
      } else {
        gtc <- NA
      }
      c(gtc, NA)[1]}
    else {
      NA
    }
  })

  return(CM)
}
