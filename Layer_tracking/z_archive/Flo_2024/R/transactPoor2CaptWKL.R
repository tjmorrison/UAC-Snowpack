#' Regional check of layer matching at grouped level
#'
#' In the paper flowchart (Fig. 2a), this function corresponds to the matching step at the regionally grouped level. It is implemented in a way
#' that first the grid point level matching is carried out, and the resulting layers are written to two DB tables `captWKL` and `poorWKL`. Then
#' this function is intended to perform the regional check after reading a data subset from the database. All layers that are found to be associated
#' with the seemingly wrong data table are moved between the tables (i.e., some poorWKL layers are moved over to the captWKL table) and relevant
#' summary statistics stored in other tables (such as vframe) are recomputed.
#'
#' It is implemented in this way, because this regional check was designed many months after the grid point level matching was already in place.
#'
#' @param VD ValidationData object as obtained from [getVdata]
#' @param WX24 a data.frame with weather data containing only one record per day. Each record must include hn24 (75th percentile), hn72 (25th percentile),
#' rain24 (75th percentile) and rain72 (25th percentile).
#' @param potentialDatetags as obtained from [derivePotentialDatetags]. Not required if WX24 provided.
#' @return the VD object with slightly changed table contents and recomputed summary statistics of the relevant variables.
#' @export
transactPoor2CaptWKL <- function(VD, WX24, potentialDatetags = NULL) {

  gtype_rank <- unique(VD$vframe$gtype_rank)

  ## derive poorWKL$datetags if not precomputed
  if (!"datetag" %in% names(VD$poorWKL)) {
    if (is.null(potentialDatetags)) {
      if (is.null(WX24)) stop("Either you have to precompute poorWKL$datetag`s, or you have to provide WX24 or potentialDatetags!")
      potentialDatetags <- derivePotentialDatetags(WX24)
    }
    VD$poorWKL <- assignLayer2Datetag(VD$poorWKL, unique(c(potentialDatetags, VD$wkl$datetag)))
  }

  if (length(VD$poorWKL$datetag) > 0 & length(VD$wkl$datetag) > 0) {
    for (dtag in unique(as.character(VD$poorWKL$datetag))) {  # LOOP over poorWKL$datetags
      dtag <- as.Date(dtag)
      VD$wkl$datetag <- as.Date(VD$wkl$datetag)
      nearestWKLdatetag <- VD$wkl$datetag[which.min(abs(dtag - VD$wkl$datetag))]
      diff_datetags <- as.double(min(abs(dtag - VD$wkl$datetag)))

      ## RULE 1: if 1 day apart --> transact
      transact <- FALSE
      if (diff_datetags <= 1) {
        transact <- TRUE
      } else if (diff_datetags <= 3) {
        ## RULE 2: datetags are 2 or 3 days apart and hn24 is below 10 cm in between --> transact
        between_hn24 <- WX24$hn24[WX24$date %in% seq(min(dtag, nearestWKLdatetag), max(dtag, nearestWKLdatetag), by = 1)]
        if (max(between_hn24) <= 10) transact <- TRUE
      }

      if (transact) {
        potentiallyToTransact <- which(VD$poorWKL$datetag == dtag)
        toTransact_cumulative <- NULL
        if (length(potentiallyToTransact) > 0) {

          wids <- VD$wkl$wkl_uuid[VD$wkl$datetag == nearestWKLdatetag]  # wkl_uuid
          for (wid in wids) {
            Vwid <- VD$vframe[VD$vframe$wkl_uuid == wid, ]
            B <- VD$poorWKL[potentiallyToTransact, ]

            ## assign generics
            B$band <- sapply(B$exec_uuid, function(eid) VD$executions$band[VD$executions$exec_uuid == eid])
            B$capt_id <- NA
            B$wkl_uuid <- wid
            B$tlimu <- NA
            B$tliml <- NA

            ## assign gtype_rank
            B$gtype_rank <- "secondary"
            B$gtype_rank[B$gtype %in% c("MFcr", "IF")] <- "tertiary"

            primary_gtype_class <- unique(Vwid$gtype_class[Vwid$gtype_rank == "primary"])
            if (length(primary_gtype_class) == 1) {
              B_gtype_rank_bol <- sapply(seq(nrow(B)), function(i) {
                if (primary_gtype_class == "SH") {
                  B$gtype[i] %in% c("SH", "DH")
                } else if (primary_gtype_class %in% c("FC", "DH")) {
                  B$gtype[i] %in% c("FC", "FCxr", "DH")
                }
              })
              B$gtype_rank[B_gtype_rank_bol] <- "primary"
            } else if (length(primary_gtype_class) > 1) {
              stop("There should be only one gtype_class per wkl_uuid and gtype_rank, look into that!")
            }
            if (all(c("primary", "secondary") %in% gtype_rank) & !all(c("primary", "secondary") %in% unique(Vwid$gtype_rank))) {
              B$gtype_rank[B$gtype_rank != "tertiary"] <- unique(Vwid$gtype_rank[Vwid$gtype_rank != "tertiary"])
            }

            ## assign vfuuid
            B$vf_uuid <- sapply(seq(nrow(B)), function(i) {
              vfid <- Vwid$vf_uuid[Vwid$vdate == B$vdate[i] & Vwid$band == B$band[i] & Vwid$gtype_rank == B$gtype_rank[i]]
              if (length(vfid) == 0) vfid <- NA
              vfid
            })
            if (class(B$vf_uuid) == "list") stop("Bad error during assigning of vf_uuids: Likely too many vf_uuids that fit!")


            ## decide which poorWKLs finally do get transacted
            toTransact <- which(!is.na(B$vf_uuid))  # this line makes poorWKLs with human datetags but an age > 90 days (validation window) not to be transacted. Could be adjusted?!
            toTransact_cumulative <- c(toTransact_cumulative, toTransact)
            ## do transaction
            VD$captWKL <- rbind(VD$captWKL, B[toTransact, names(B) %in% names(VD$captWKL)])
          }  # END LOOP over potentially duplicated wkl_uuids
          VD$poorWKL <- VD$poorWKL[-unique(potentiallyToTransact[toTransact_cumulative]), ]  # do deletion of poorWKLs after loop over wkl_uuids!

        }}}}  # END LOOP simulated datetag


  ## recompute proportion_captured and proportion_unstable
  VD <- calculateProportionCaptured(VD)
  VD <- calculateProportionUnstable(VD, c("vframe", "poorWKL"))

  return(VD)
}
