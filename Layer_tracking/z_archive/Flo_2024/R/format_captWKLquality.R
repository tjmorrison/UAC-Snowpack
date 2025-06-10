#' Format data.frame containing quality/agreement indicators
#'
#' i.e., apply some filtering, merge with data from mergedVD$wkl, format data classes, compute some interesting variables
#'
#' @param VS the data.frame as returned by evaluate_main.R section 'assessQualityOfCaptWKL'
#' @param mergedVD the data.frame as returned by evaluate_main.R section 'mergeVDvframe'
#' @param simulatedGsizeAtBurial a data.frame produced in `scratch.R`, that contains information about the simulated grain size of layers of concern at their times of burial
#' @param WKL_SHonly LEGACY: this is only relevant for fherla's wklvalidation project. If you want to replicate the wklvalidation with new data, this feature is already automated by
#' having a column in the processingDB table `wkl` called `SH_only`. This variable will then be used instead of the data.frame provided here, which manually inserts the SH_only flag.
#' (See `scratch.R` for more information about this flag.)
#'
#' @seealso source code in evaluate_main.R
#'
#' @export
#'
format_captWKLquality <- function(VS, mergedVD, simulatedGsizeAtBurial = NA, WKL_SHonly = NA, floorDQ = TRUE) {


  ## remove rows where wkl_uuid is NA:
  VS <- VS[!is.na(VS$wkl_uuid), ]

  ## get data from mergedVD$wkl
  if ("SH_only" %in% names(mergedVD$wkl)) {
    VS <- merge(VS, mergedVD$wkl[, c("wkl_uuid", "season", "region", "dq", "gsize", "SH_only")])
  } else {
    VS <- merge(VS, mergedVD$wkl[, c("wkl_uuid", "season", "region", "dq", "gsize")])
  }

  colnames(VS)[colnames(VS) == "gsize"] <- "gsize_rep"

  ## get data from mergedVD$vframe: drySpell_median & *_std, gsizeAtBurial_median
  VS$drySpell_median <- sapply(seq(nrow(VS)), function(i) {
    tmp <- unique(mergedVD$vframe$drySpell_median[mergedVD$vframe$wkl_uuid == VS$wkl_uuid[i] & mergedVD$vframe$band == VS$band[i] & mergedVD$vframe$gtype_rank == VS$gtype_rank[i]])
    if (length(tmp) == 0) tmp <- NA
    tmp
  })
  VS$drySpell_std <- sapply(seq(nrow(VS)), function(i) {
    tmp <- unique(mergedVD$vframe$drySpell_std[mergedVD$vframe$wkl_uuid == VS$wkl_uuid[i] & mergedVD$vframe$band == VS$band[i] & mergedVD$vframe$gtype_rank == VS$gtype_rank[i]])
    if (length(tmp) == 0) tmp <- NA
    tmp
  })
  ## if gsizeAtBurial happens to be available, append it! if it's not available in mergedVD, it can be provided with an extra argument that is processed further down
  VS$gsizeAtBurial_median <- sapply(seq(nrow(VS)), function(i) {
    tmp <- unique(mergedVD$vframe$gsizeAtBurial_median[mergedVD$vframe$wkl_uuid == VS$wkl_uuid[i] & mergedVD$vframe$band == VS$band[i] & mergedVD$vframe$gtype_rank == VS$gtype_rank[i]])
    if (length(tmp) == 0) tmp <- NA
    tmp
  })

  ## filter nPDays == 0
  VS <- VS[VS$nPDays != 0, ]

  ## compute gtype_class
  VS$gtype_class <- sapply(seq(nrow(VS)), function(i) {
    tbl <- table(mergedVD$vframe$gtype_class[mergedVD$vframe$wkl_uuid == VS$wkl_uuid[i] & mergedVD$vframe$band == VS$band[i] & mergedVD$vframe$gtype_rank == VS$gtype_rank[i]])
    if (length(tbl) == 0 && VS$gtype_rank[i] == "secondary") tbl <- table(mergedVD$vframe$gtype_class[mergedVD$vframe$wkl_uuid == VS$wkl_uuid[i] & mergedVD$vframe$band == VS$band[i] & mergedVD$vframe$gtype_rank == "primary"])
    if (length(tbl) == 0) {
      NA
    } else if (max(tbl) == 0) {
      NA
    } else {
      names(tbl[1])
    }
  })
  ## remove layers with gtype_class "MF" (only three distinct MF layers in data set!)
  toDiscard <- which(VS$gtype_class == "MF")
  if (length(toDiscard) > 0) VS <- VS[-toDiscard, ]

  ## add gsizeAtBurial info if provided as extra argument
  if (!all(is.na(simulatedGsizeAtBurial))) {
    VS$gsizeAtBurial_median <- NA
    VS$gsizeAtBurial_median <- sapply(seq(nrow(VS)), function(i) {
      tmp <- median(simulatedGsizeAtBurial$gsizeAtBurial_median[simulatedGsizeAtBurial$wkl_uuid %in% VS$wkl_uuid[i] & simulatedGsizeAtBurial$band %in% VS$band[i] & simulatedGsizeAtBurial$gtype_rank %in% VS$gtype_rank[i]])
      c(tmp, NA)[1]
    })
  }

  ## add logical flag whether reported layer is solely a SH layer:
  if (!all(is.na(WKL_SHonly))) {
    VS$SH_only <- NA
    VS$SH_only <- sapply(seq(nrow(VS)), function(i) {
      tmp <- WKL_SHonly$SH_only[WKL_SHonly$wkl_uuid == VS$wkl_uuid[i]]
      c(tmp, NA)[1]
    })
  }

  ## adjust gtype_class labels based on exclusiveness of reported SH grain type
  if ("SH_only" %in% names(VS)) {
    ## explicitely name SH/FC mixes
    VS$gtype_class[!VS$SH_only & VS$gtype_class %in% "SH"] <- "SH/FC"
    ## re-name DH layers:
    VS$gtype_class[VS$wkl_iscrust & VS$gtype_class %in% "DH"] <- "SH/FC"
    VS$gtype_class[!VS$wkl_iscrust & VS$gtype_class %in% "DH"] <- "FC"
  }


  ## compute month of burial
  VS$wklmonth <- format(as.Date(VS$wkldate), "%m")

  ## compute offset_totaltime
  VS$offset_totaltime <- VS$nPDays_20 - VS$nPDays

  ## adjust variable classes
  factor_classes <- c("band", "gtype_rank", "gtype_class", "region", "season", "wkl_iscrust")
  if ("SH_only" %in% names(VS)) factor_classes <- c(factor_classes, "SH_only")
  for (fac in factor_classes) {
    VS[, fac] <- as.factor(VS[, fac])
  }
  for (db in c("dq", "wklmonth", "rho_llhd", "nPDays", "gsize_rep")) {
    VS[, db] <- as.double(VS[, db])
  }

  if (floorDQ) {
    VS$dq <- floor(VS$dq)
  }


  return(VS)
}
