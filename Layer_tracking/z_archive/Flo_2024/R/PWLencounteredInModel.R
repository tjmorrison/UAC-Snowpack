#' Finds weak layers in simulation and stores them in DB
#'
#' In the paper flowchart (Fig. 2a), this function does not correspond to a specific step, but instead it finds all 'Unstable Layers' in the simulations.
#'
#' @param simul snowprofileSet
#' @param sm simulation meta data summary
#' @param DBCon DB connection
#' @param savePoorWKL boolean switch to write to `fherla_wklvalidation.poorWKL`
#' @param exec_uuid execution uuid to reference calculation
#' @return writes to DB
#' @export
PWLencounteredInModel <- function(simul, sm = summary(simul),
                                  DBCon = NULL, savePoorWKL = ifelse(is.null(DBCon), FALSE, TRUE),
                                  exec_uuid = ifelse(is.null(DBCon), NA, stop("Must provide exec_uuid if writing to DB!")),
                                  verbose = TRUE) {


  dateperiod <- as.character(unique(sm$date))
  if (verbose) message(paste0("Looping over ", length(dateperiod), " days and stacking ", length(unique(sm$station_id)), " profiles.."))
  ## LOOP over dateperiod
  for (dt in dateperiod) {
    simul_sub <- simul[sm$date == dt]

    ## stack all profiles from one day
    id_x <- seq(length(simul_sub))

    lyrs <- data.table::rbindlist(lapply(id_x, function(i) {
      data.frame(simul_sub[[i]]$layers, list(id = i, station = simul_sub[[i]]$station, station_id = simul_sub[[i]]$station_id, aspect = simul_sub[[i]]$aspect, angle = simul_sub[[i]]$angle))
    }), use.names = TRUE, fill = TRUE)
    class(lyrs) <- append("snowprofileLayers", class(lyrs))

    ## find poor layers:
    pooridx <- sarp.snowprofile::findPWL(lyrs, pwl_gtype = c("SH", "DH", "FC", "FCxr", "MFcr", "PP", "DF"),
                                         threshold_RTA = 0.8, threshold_PU = 0.77, threshold_RC = 0.4, threshold_SK38 = 0.95)
    lyrs <- lyrs[pooridx, ]  # all lyrs have poor stability after this subset
    if (nrow(lyrs) >= 1) {
      pooridx <- seq(nrow(lyrs))  # pooridx is now a simple index along lyrs
      ## remove layers that show poor SK38 but good RTA, i.e. Monti approach
      pooridx[which(lyrs$sk38 <= 0.95 & lyrs$rta < 0.8)] <- NA
      ## only keep deepest PP/DF layer as diagnosed by p_unstable
      pooridx[which(lyrs$gtype %in% c("PP", "DF") & (lyrs$p_unstable < 0.77 | is.na(lyrs$p_unstable)))] <- NA
      for (k in which(lyrs$gtype[pooridx] %in% c("PP", "DF") & !is.na(pooridx))) {
        station_lyrs <- lyrs[lyrs$station_id %in% lyrs$station_id[k] & lyrs$gtype %in% c("PP", "DF") & lyrs$p_unstable >= 0.77, ]  # all new snow lyrs from one station (with poor p_unstable)
        pooridx[lyrs$id %in% station_lyrs$id[station_lyrs$depth < max(station_lyrs$depth, na.rm = TRUE)]] <- NA  # find station_lyrs$id to be removed and remove them in pooridx via lyrs
      }
      pooridx <- pooridx[!is.na(pooridx)]
      if (savePoorWKL & length(pooridx) > 0) {
        savePoorLayers2DB(lyrs, pooridx, DBCon, data.frame(exec_uuid = exec_uuid, vdate = dt, nSP = length(simul_sub)))
      }
    }
  }  # END LOOP over dateperiod

  ## final message
  if (verbose) {
    if (savePoorWKL) {
      nPoor <- DBI::dbGetQuery(DBCon, paste0("SELECT COUNT(*) FROM `poorWKL` WHERE exec_uuid = '", exec_uuid, "'"))
      message(paste0("Wrote  ", nPoor[[1]], " rows in `poorWKL`"))
    }
  }
}
