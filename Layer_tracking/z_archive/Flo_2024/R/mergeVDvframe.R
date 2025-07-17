#' Merge two database tables `vframe`
#'
#' The ValidationData `VD` contains several different tables from the `wklvalidation` database. `vframe` is one of them,
#' containing a summary of captured layers at a given day for differet layers of concern. For the final analysis
#' it is necessary to have the data stored in a long vframe table that contains all the rows from all layers of concern,
#' all elevation bands, regions, dates, etc. However, for each layer of concern several preprocessing steps need
#' to be carried out when reading the data from the database (this is all done in `evaluate_main.R`). This function
#' facilitates merging all the individual vframe tables back together! In fact, it also merges the tables `executions`
#' and `wkl`, but discards the tables with individual layer information (which are simply too big!) `captWKL` and `poorWKL`.
#'
#' Given the workflow in `evaluate_main.R`, information from individual layers is available up to this function,
#' but not afterwards. So, this function includes several features to compute summary statistics from individual
#' layers (e.g., simulated grain size at burial, etc).
#'
#' @param VD ValidationData object as obtained from [getVdata]
#' @param VD2add ValidationData object to be merged into VD
#' @param add_gsizeAtBurial logical switch to compute median simulated grain size at time of burial
#' (and include it in vframe) with a function call to [computeGsizeAtBurial]
#' @param add_timewindows logical switch to compute median length of dry period before burial
#' (and include it in vframe) with a function call to [computeLengthOfPreceedingDrySpell]
#' @param add_poorWKLgtypeclass logical switch to compute and label the grain type classes of all unstable layers
#' that are not associated with a layer of human concern (i.e., table `poorWKL`). This is carried out with a
#' call to [determindePrevalentGtypes] and then assembling the returned gtype labels in a format consistent
#' with how gtypes have been handled in the paper.
#' @param region info from other tables (required only if add_poorWKLgtypeclass = TRUE)
#' @param band info from other tables (required only if add_poorWKLgtypeclass = TRUE)
#' @param gtype_rank info from other tables (required only if add_poorWKLgtypeclass = TRUE)
#'
#' @return object VD (tables `executions`, `vframe`, `wkl`) merged with the corresponding tables from VD2add
#'
#' @export
mergeVDvframe <- function(VD, VD2add, add_gsizeAtBurial = FALSE, add_timewindows = FALSE, add_poorWKLgtypeclass = TRUE,
                          region = NA, band = NA, gtype_rank = NA) {

  if (nrow(VD2add$vframe) > 0) {
    if (add_gsizeAtBurial) {
      VD2add <- computeGsizeAtBurial(VD2add)
    }
    if (add_timewindows) {
      VD2add <- computeLengthOfPreceedingDrySpell(VD2add)
    }
    if (add_poorWKLgtypeclass) {
      if (!"poorWKL" %in% names(VD2add)) stop("Cannot determine prevalent gtypes: poorWKL information missing!")
      VD2add_poorWKL <- determinePrevalentGtypes(VD2add$poorWKL)
      ## Loop over simulated datetags:
      udt <- unique(VD2add_poorWKL$datetag)
      VD2add$wkl_simulated <- data.frame(datetag = udt, gtype_class = rep(NA, length(udt)),
                                         region = rep(region, length(udt)), band = rep(band, length(udt)),
                                         gtype_rank = rep(gtype_rank, length(udt)))
      for (dt in VD2add$wkl_simulated$datetag) {
        gt1 <- VD2add_poorWKL$prevalentGtype1[VD2add_poorWKL$datetag %in% dt]
        gt2 <- VD2add_poorWKL$prevalentGtype2[VD2add_poorWKL$datetag %in% dt]

        if (any(c("DH", "FC", "FCxr") %in% gt1)) {
          gtc <- "FC"
        } else if ("SH" %in% gt1) {
          gtc <- "SH"
        } else if (any(c("MFcr", "IF") %in% gt1)) {
          gtc <- "MFcr"
        } else {
          gtc <- gt1
        }

        if (gtc %in% "FC" && gt2 %in% "SH") gtc <- "DH"  # this line is technically obsolete since it will be adjusted in format_confusionMatrix anyways..
        VD2add$wkl_simulated$gtype_class[VD2add$wkl_simulated$datetag %in% dt] <- gtc
        VD2add$wkl_simulated$gtype_primary[VD2add$wkl_simulated$datetag %in% dt] <- gt1
        VD2add$wkl_simulated$gtype_secondary[VD2add$wkl_simulated$datetag %in% dt] <- gt2
      }
    }
  }

  if ("vframe" %in% names(VD) && nrow(VD$vframe) > 0) {
    if ("wkl_simulated" %in% names(VD)) {
      VD <- VD[c("executions", "vframe", "wkl", "wkl_simulated")]
      if ("wkl_simulated" %in% names(VD2add)) VD$wkl_simulated <- rbind(VD$wkl_simulated, VD2add$wkl_simulated)
      VD$wkl_simulated <- VD$wkl_simulated[!duplicated(VD$wkl_simulated), ]
    } else {
      VD <- VD[c("executions", "vframe", "wkl")]
      if ("wkl_simulated" %in% names(VD2add)) {
        VD$wkl_simulated <- VD2add$wkl_simulated
        VD$wkl_simulated <- VD$wkl_simulated[!duplicated(VD$wkl_simulated), ]
      }
    }

    VD$executions <- rbind(VD$executions, VD2add$executions)
    VD$executions <- VD$executions[!duplicated(VD$executions), ]

    VD$vframe <- rbind(VD$vframe, VD2add$vframe)
    VD$vframe <- VD$vframe[!duplicated(VD$vframe), ]

    VD$wkl <- rbind(VD$wkl, VD2add$wkl)
    VD$wkl <- VD$wkl[!duplicated(VD$wkl), ]


    return(VD)

  } else {
    if ("wkl_simulated" %in% names(VD2add)) {
      return(VD2add[c("executions", "vframe", "wkl", "wkl_simulated")])
    } else {
      return(VD2add[c("executions", "vframe", "wkl")])
    }

  }


}
