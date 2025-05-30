
#' Determine and label the prevalent grain types of simulated unstable layers grouped by model-derived date-tags
#'
#' This function takes a data.frame (similar to `snowprofileLayers`), but containing layers associated with different
#' date tags, different grid points, and different points in time. All layers in this data.frame should be unstable
#' (as determined by your favorite approach to identifying instability, e.g. statistical approach of p_unstable)
#' and should be associated with a (model-derived) date tag (by [assignLayer2Datetag]).
#'
#' The function determines the prevalent grain types of each group of simulated layers. To be able to label the layer groups
#' with either a single grain type (FC, SH, or MFcr) or by a mix of two classes (e.g., SH/FC), the function identifies
#' the two most prevalent grain types within the group of layers associated with a date tag at each grid point for each day.
#' Then it identifies the two most prevalent grain types across all grid points for each day,
#' and eventually for the entire lifetime of the layer.
#'
#' To address the well-known SNOWPACK behaviour of transforming most SH layers into DH layers after several days of burial,
#' the function judges DH layers as FC layers if only a negligible amount (less than 10\%) of SH is encountered during the
#' layer's lifetime. If the fraction of SH layers was higher than 10~\%, it judges all DH layers as SH layers.
#'
#' Finally the layer groups are labeled with the resulting one or two prevalent grain types (at each day and over the entire lifetime of the layer).
#'
#' @param layer a snowprofileLayers-like object of one or multiple unstable layers from any grid point and any date that are each associated to a datetag.
#' @return This function adds the following columns to the input data.frame:
#'
#'   * prevalentGtype1Today and prevalentGtype2Today
#'   * prevalentGtype1 and prevalentGtype2
#'
#' @export
determinePrevalentGtypes <- function(layer) {

  if (nrow(layer) > 0) {
    if (!"datetag" %in% names(layer) || any(is.na(layer$datetag))) stop("Critical avalanche layers are not grouped by datetags yet. Cannot determine prevalent gtypes!")

    layer$prevalentGtype1Today <- NA
    layer$prevalentGtype2Today <- NA
    layer$prevalentGtype1 <- NA
    layer$prevalentGtype2 <- NA

    ## LOOP over each datetag
    for (dt in unique(layer$datetag)) {
      ## LOOP over each day (= vdate)
      ## determine prevalent gtypes at each day
      vdates <- unique(layer$vdate[layer$datetag %in% dt])
      gtype1_vdate <- rep(NA, length(vdates))
      gtype2_vdate <- rep(NA, length(vdates))
      weights_vdate <- rep(NA, length(vdates))
      for (today in vdates) {
        layer_today <- layer[layer$vdate %in% today & layer$datetag %in% dt, ]
        ## determine prevalent gtypes first on individual grid point level:
        gtype1_gridpoint <- sapply(unique(layer_today$vstation_id), function(id) {
          names(sort(table(layer_today$gtype[layer_today$vstation_id %in% id]), decreasing = TRUE))[1]
        })
        gtype2_gridpoint <- sapply(unique(layer_today$vstation_id), function(id) {
          tbl <- sort(table(layer_today$gtype[layer_today$vstation_id %in% id]), decreasing = TRUE)
          if ("SH" %in% names(tbl)[2:5]) {  # SH in any position other than first:
            "SH"
          } else {
            names(tbl)[2]
          }
        })
        ## ..then for all grid points
        gtype_percentage <- sort(table(c(gtype1_gridpoint, gtype2_gridpoint)) / length(na.omit(gtype1_gridpoint, gtype2_gridpoint)), decreasing = TRUE)

        ## write results to layer data.frame and store results for next step below (= entire lifetime analysis)
        layer$prevalentGtype1Today[layer$vdate %in% today & layer$datetag %in% dt] <- names(gtype_percentage[1])
        gtype1_vdate[vdates %in% today] <- names(gtype_percentage[1])
        if (length(gtype_percentage) > 1 && gtype_percentage[2] > 0.1) {
          layer$prevalentGtype2Today[layer$vdate %in% today & layer$datetag %in% dt] <- names(gtype_percentage[2])
          gtype2_vdate[vdates %in% today] <- names(gtype_percentage[2])
        }
        weights_vdate[vdates %in% today] <- max(layer_today$proportion_unstable)
      }  # END LOOP vdate

      ## determine prevalent gtypes over entire lifetime of critical avalanche layer
      ## ..weighted by proportion_unstable at each vdate
      gtypeAll_tbl <- sort(table(c(rep(gtype1_vdate, round(100*weights_vdate)), rep(gtype2_vdate, round(100*weights_vdate))), useNA = "ifany"), decreasing = TRUE, na.last = TRUE)
      gtype1_tbl <- sort(table(rep(gtype1_vdate, round(100*weights_vdate)), useNA = "ifany"), decreasing = TRUE, na.last = TRUE)
      gtype2_tbl <- sort(table(rep(gtype2_vdate, round(100*weights_vdate)), useNA = "ifany"), decreasing = TRUE, na.last = TRUE)

      ## print tables for interactive fine-tuning of rules and thresholds:
      # message(as.character(as.Date(dt)))
      # message(paste0(paste0(names(gtype1_tbl), collapse = ", "), " | ", paste0(names(gtype2_tbl), collapse = ", ")))
      # message(paste0(paste0(gtype1_tbl, collapse = ", "), " | ", paste0(gtype2_tbl, collapse = ", ")))
      # message()

      gt1 <- names(gtype1_tbl[1])
      gt2 <- names(gtype2_tbl[1])
      ## special rule for SH:
      if (all(c("DH", "SH") %in% names(gtypeAll_tbl)) &&
          gtypeAll_tbl["SH"] > 0.1*gtypeAll_tbl["DH"]) {
        if ("DH" %in% names(gtype1_tbl)[1]) {
          gt1 <- "SH"
        } else if ("DH" %in% names(gtype2_tbl)[1:2]) {
          gt2 <- "SH"
        }
      }
      ## simplification of FC types and writing of results
      if (length(gt1) > 0) {
        if (gt1 %in% c("DH", "FC", "FCxr")) gt1 <- "FC"
        layer$prevalentGtype1[layer$datetag %in% dt] <- gt1
      }
      if (length(gt2) > 0) {
        if (gt2 %in% c("DH", "FC", "FCxr")) gt2 <- "FC"
        if (gt2 %in% gt1) gt2 <- NA
        layer$prevalentGtype2[layer$datetag %in% dt] <- gt2
      }

    }  # END LOOP datetag

  }
  return(layer)
}
