#' Summarize PWL information in a tabular format per grid point location
#'
#' @note This function and/or approach never made it beyond beta testing phase..
#' @author fherla
#'
#' @export
summarizePWLatGridpoint <- function(set, mergeMethods = c("bdate_20cm", "sfc_periods", "outside_adjacency"),
                                    pwl2track = expression(which(lyrs$date == d &
                                                         lyrs$gtype %in% c("FC", "FCxr", "DH", "SH") &
                                                         lyrs$gsize > 0.6 &
                                                         lyrs$hardness <= 2 &
                                                         lyrs$dhardness >= 1 &
                                                         lyrs$dgsize >= 0.4*lyrs$gsize))
                                    ) {

  ## --- assertions and initialization ----
  if (!is.snowprofileSet(set)) stop("set must be a snowprofileSet object")

  layerproperties <- names(set[[1]]$layers)
  lyrs <- rbind(set)[, c("station", "station_id", "date", "datetime", layerproperties)]
  lyrs$ddate <- as.Date(lyrs$ddate)
  lyrs <- lyrs[order(lyrs$date, lyrs$height), ]

  station_id <- unique(lyrs$station_id)
  if (length(station_id) > 1) stop("This routine is designed to summarize PWLs at one station, not multiple!")


  ## --- collect PWL info ----
  ## go through all gridpoint lyrs in time and collect info about pwls that come up:

  PWLcols <- c("id", "nL", "first_occurrence", "last_occurrence", "ddate_min", "ddate_max", "bdate_5cm", "bdate_20cm",
               "ddate_start", "ddate_end", "fdate_start", "fdate_end", "bdate_start", "bdate_end", "gtypeATddate", "gtypeATfdate", "mergeWith")
  PWL <- data.frame(matrix(ncol = length(PWLcols), nrow = 300))
  colnames(PWL) <- PWLcols
  for (d in unique(lyrs$date)) {  # LOOP over each day
    pwlrow <- eval(pwl2track)
    # pwlrow <- which(lyrs$date == d & lyrs$tsa >= 5)

    # crrow <- which(lyrs$date == d & lyrs$gtype %in% "MFcr")
    # pwlrow <- c(pwlrow, crrow)
    if (length(pwlrow) > 0) {
      for (r in pwlrow) {  ## LOOP over each PWL per day

        # ddate_start <- as.character(gridp_fperiods$from[gridp_fperiods$from <= lyrs$ddate[r] & gridp_fperiods$to >= lyrs$ddate[r]])
        # ddate_end <- as.character(gridp_fperiods$to[gridp_fperiods$from <= lyrs$ddate[r] & gridp_fperiods$to >= lyrs$ddate[r]])
        # bdate_start <- as.character(gridp_fperiods$from[which(gridp_fperiods$conditions == "storm" & gridp_fperiods$from > lyrs$ddate[r])[1]])
        # bdate_end <- as.character(gridp_fperiods$to[which(gridp_fperiods$conditions == "storm" & gridp_fperiods$from > lyrs$ddate[r])[1]])  # maybe include: period when layer depth grew above threshold (e.g. 25 cm?!)
        # fdate_start <- as.character(gridp_fperiods$from[gridp_fperiods$from <= d & gridp_fperiods$to >= d])
        # fdate_end <- as.character(gridp_fperiods$to[gridp_fperiods$from <= d & gridp_fperiods$to >= d])
        # ddate_lyrs <- lyrs[lyrs$date %in% c(lyrs$ddate[r], lyrs$ddate[r]+1) & lyrs$ddate == lyrs$ddate[r], ]
        gtypeATddate <- as.character(NA)  # names(table(as.character(sort(ddate_lyrs$gtype, decreasing = TRUE)))[1])  # i.e., prevalent gtype of layers that got deposited at ddate
        mergeWith <- as.double(NA)

        ## different tracking for SH layers versus FC, FCxr, DH:
        if (lyrs$gtype[r] == "SH") {
          gtypeATddate <- "SH"
          PWLr <- which(PWL$gtypeATddate == "SH" &
                          (
                            PWL$ddate_max == lyrs$ddate[r] |
                              (PWL$ddate_max > lyrs$ddate[r] & PWL$ddate_min <= lyrs$ddate[r]) |
                              PWL$ddate_max == (lyrs$date[r]-1)
                          ))
          if (length(PWLr) > 0) {
            ## PWLr is invalid if new height is larger than recent height (incl some buffer), AND
            ## take the closer choice in terms of height if several match previous requirements:
            heightOfRecentChoices <- sapply(PWL[PWLr, "id"], function(x) lyrs[rev(which(lyrs$id==x))[1], "height"])  # relies on ascending lyrs$date ordering!!!
            heightDiff2RecentChoices <- heightOfRecentChoices - lyrs$height[r]
            PWLr <- PWLr[which(heightDiff2RecentChoices == min(heightDiff2RecentChoices) & heightDiff2RecentChoices >= -5)]  # 5 cm buffer
          }
        } else if (lyrs$gtype[r] %in% c("FC", "FCxr", "DH")) {
          PWLr <- which(PWL$ddate_min <= lyrs$ddate[r] & PWL$ddate_max >= lyrs$ddate[r] & !(PWL$gtypeATddate %in% c("SH", "MFcr")) &
                          PWL$last_occurrence > (lyrs$date[r]-30))
          if (length(PWLr) == 0) {  # try to find a suitable PWLr based on layer heights the previous day
            heightCurrent <- c(min(lyrs$height[r] - lyrs$thickness[r]), max(lyrs$height[r]))
            idsPreviousDay <- PWL$id[(as.Date(PWL$last_occurrence)+1) %in% lyrs$date[r]]
            if (length(idsPreviousDay) > 0) {
              heightOfPrev <- lapply(idsPreviousDay, function(iPrev) {
                lyrs_idx <- which(lyrs$id == iPrev & (lyrs$date+1) == lyrs$date[r])
                suppressWarnings(c(min(lyrs$height[lyrs_idx] - lyrs$thickness[lyrs_idx]), max(lyrs$height[lyrs_idx])))
              })
              heightOfPrev <- do.call("rbind", heightOfPrev)
              heightDiff <- abs(heightCurrent - heightOfPrev)
              heightDiff <- apply(heightDiff, MARGIN = 1, FUN = min)
              mergeWith <- which(PWL$id == idsPreviousDay[intersect(which.min(heightDiff), which(heightDiff <= 5))])  # 5 cm buffer
              if (length(mergeWith) == 0) mergeWith <- as.double(NA)
            }
          }
        }
        # else if (lyrs$gtype[r] %in% "MFcr") {
        #   gtypeATddate <- "MFcr"
        #   PWLr <- which(PWL$ddate_min <= lyrs$ddate[r] & PWL$ddate_max >= lyrs$ddate[r] & PWL$gtypeATddate == "MFcr")
        # }

        if (length(PWLr) > 0) {  # update PWLr
          if (length(PWLr) > 1) {  # multiple PWLs could fit requirements
            PWLr <- PWLr[which.min(lyrs$date[r] - as.Date(PWL[PWLr, "last_occurrence"]))]
          }
          PWL[PWLr, "ddate_min"] <- as.character(min(c(lyrs$ddate[r], PWL[PWLr, "ddate_min"]), na.rm = TRUE))
          PWL[PWLr, "ddate_max"] <- as.character(max(c(lyrs$ddate[r], PWL[PWLr, "ddate_max"]), na.rm = TRUE))
          PWL[PWLr, "first_occurrence"] <- as.character(min(c(lyrs$date[r], PWL[PWLr, "first_occurrence"]), na.rm = TRUE))
          PWL[PWLr, "last_occurrence"] <- as.character(max(c(lyrs$date[r], PWL[PWLr, "last_occurrence"]), na.rm = TRUE))
        } else {  # initially fill PWLr
          filledrows <- which(!is.na(PWL$id))
          PWLr <- ifelse(length(filledrows) > 0, max(filledrows) + 1, 1)
          PWL[PWLr, "id"] <- ifelse(!length(filledrows) == 0, max(PWL$id, na.rm = TRUE)+1, 1)
          # PWL[PWLr, "ddate_start"] <- ddate_start
          # PWL[PWLr, "ddate_end"] <- ddate_end
          PWL[PWLr, "ddate_min"] <- as.character(lyrs$ddate[r])
          PWL[PWLr, "ddate_max"] <- as.character(lyrs$ddate[r])
          # PWL[PWLr, "fdate_start"] <- fdate_start
          # PWL[PWLr, "fdate_end"] <- fdate_end
          PWL[PWLr, "gtypeATddate"] <- gtypeATddate
          PWL[PWLr, "first_occurrence"] <- as.character(lyrs$date[r])
          PWL[PWLr, "last_occurrence"] <- as.character(lyrs$date[r])
          PWL[PWLr, "mergeWith"] <- mergeWith
        }

        lyrs[r, "id"] <- PWL[PWLr, "id"]
        # lyrs[r, "ddate_start"] <- ddate_start
        # lyrs[r, "ddate_end"] <- ddate_end
        # lyrs[r, "bdate_start"] <- bdate_start
        # lyrs[r, "bdate_end"] <- bdate_end
        # lyrs[r, "gtypeATddate"] <- gtypeATddate

        PWL[PWLr, "nL"] <- nrow(lyrs[which(lyrs$id == PWL[PWLr, "id"]), ])
      }  # END LOOP over each PWL per day
    }  # END IF pwl exists that day
  }  # END LOOP over each day
  filledrows <- which(!is.na(PWL$id))
  if (length(filledrows) == 0) {
    warning("Can't find any PWLs at gridpoint!")
    return(NULL)
  }
  PWL <- PWL[filledrows, ]
  PWL$ddate_start <- as.Date(PWL$ddate_start)
  PWL$ddate_end <- as.Date(PWL$ddate_end)
  PWL$ddate_min <- as.Date(PWL$ddate_min)
  PWL$ddate_max <- as.Date(PWL$ddate_max)
  PWL$fdate_start <- as.Date(PWL$fdate_start)
  PWL$fdate_end <- as.Date(PWL$fdate_end)
  PWL$bdate_start <- as.Date(PWL$bdate_start)
  PWL$bdate_end <- as.Date(PWL$bdate_end)
  PWL$bdate_5cm <- as.Date(PWL$bdate_5cm)
  PWL$bdate_20cm <- as.Date(PWL$bdate_20cm)
  PWL$first_occurrence <- as.Date(PWL$first_occurrence)
  PWL$last_occurrence <- as.Date(PWL$last_occurrence)

  ## --- plot (for debugging/development purposes)----
  # plot(set)
  # layers2col <- c(3, 9)  # PWL$id[which(!is.na(PWl$id))]
  # col <- rep(RColorBrewer::brewer.pal(8, name = "Dark2"), times = nrow(PWL))   # rep(c("black", "red", "chocolate"), times = nrow(ans$PWL))
  # for (k in seq_along(layers2col)) {
  #   rect(lyrs$date[which(lyrs$id == layers2col[k])]-0.5, lyrs$height[which(lyrs$id == layers2col[k])]- lyrs$thickness[which(lyrs$id == layers2col[k])],
  #        lyrs$date[which(lyrs$id == layers2col[k])]+0.5, lyrs$height[which(lyrs$id == layers2col[k])],
  #        border = TRUE, density = 10, col = col[k], lwd = 3)
  #   # cat(paste0("id = ", layers2col[k], "\n"))
  #   # browser()
  # }


  ## --- merge PWL list ----
  lyrs$id_rd1 <- lyrs$id
  ans <- list(PWL = PWL, lyrs = lyrs)


  ## based on mergeWith column:
  mergeWithRows <- which(!is.na(ans$PWL$mergeWith))
  if (length(mergeWithRows) > 0) {
    mergemat <- ans$PWL[mergeWithRows, c("id", "mergeWith")]
    mergemat$toDo <- TRUE
    for (mr in seq(nrow(mergemat))) {  # LOOP over each merge annotation
      if (mergemat$toDo[mr]) {
        idx2merge <- which(mergemat$id %in% mergemat[mr, c("id", "mergeWith")] | mergemat$mergeWith %in% mergemat[mr, c("id", "mergeWith")])
        mergemat$toDo[idx2merge] <- FALSE
        ids2merge <- unique(unlist(unclass(mergemat[idx2merge, ])[c("id", "mergeWith")]))
        idx2merge <- which(ans$PWL$id %in% ids2merge)
        if (length(idx2merge) >= 2) {
          ans <- suppressWarnings(summarizePWLatGridpoint_sub(ans$PWL, idx2merge, ans$lyrs))
        }
      }
    }
  }



  ## based on adjacency:
  ## comments:
  ## * does that need more restrictive requirements? e.g., adjacency needs to be fullfilled x many days, ...?
  ## * in fact: this over-aggregating unless only very few PWLs are extracted!
  if ("adjacency" %in% mergeMethods) {
    lyrsWithID <- ans$lyrs[!is.na(ans$lyrs$id), ]
    for (d in lyrsWithID$date) {  # LOOP over each day
      tmp <- lyrsWithID[lyrsWithID$date == d, ]
      nL_tmp <- nrow(tmp)
      if (nL_tmp > 1 & length(unique(tmp$id)) > 1) {
        tmp <- tmp[order(tmp$height, decreasing = TRUE), ]
        tmprow <- which(diff(tmp$height) == -tmp$thickness[1:(nrow(tmp)-1)])
        if (length(tmprow) >= 1) {
          jumpbreak <- c(0, which(diff(tmprow) > 1), length(tmprow))
          for (k in seq(length(jumpbreak)-1)) {
            ids2merge <- tmp$id[c(tmprow[(jumpbreak[k]+1):jumpbreak[k+1]], tmprow[(jumpbreak[k]+1):jumpbreak[k+1]]+1)]
            if (length(unique(ids2merge)) > 1) {
              idx2merge <- which(ans$PWL$id %in% ids2merge)
              ans <- suppressWarnings(summarizePWLatGridpoint_sub(ans$PWL, idx2merge, ans$lyrs))
              lyrsWithID <- ans$lyrs[!is.na(ans$lyrs$id), ]
            }
          }
        }
      }
    }
  }

  ## based on adjacency of MFcr:
  ## TODO problem: all other merging routines will also merge MFcr---prevent that!
  if ("adjacency_MFcr" %in% mergeMethods) {
    ans$lyrs <- getPWLidAdjacentLayers(ans$lyrs)
    ids2loop <- ans$lyrs$id[ans$lyrs$gtype == "MFcr" & !is.na(ans$lyrs$id)]
    for (id in ids2loop) {
      if (id %in% ans$PWL$id) {
        ids2merge <- unique(c(id, ans$lyrs$id_adjacentAbove[ans$lyrs$id == id], ans$lyrs$id_adjacentBelow[ans$lyrs$id == id]))
        ids2merge <- ids2merge[!is.na(ids2merge)]
        ids2merge <- ids2merge[ids2merge %in% ans$PWL$id[ans$PWL$gtypeATddate == "MFcr"]]
        idx2merge <- which(ans$PWL$id %in% ids2merge)
        if (length(idx2merge) > 1) ans <- suppressWarnings(summarizePWLatGridpoint_sub(ans$PWL, idx2merge, ans$lyrs))
      }
    }
  }


  ## --- compute attributes of PWL groups ----
  for (id in PWL$id) {
    ## bdate:
    # PWL$bdate_start[which(PWL$id == id)] <- median(as.Date(lyrs$bdate_start[which(lyrs$id == id)]))
    # PWL$bdate_end[which(PWL$id == id)] <- median(as.Date(lyrs$bdate_end[which(lyrs$id == id)]))
    ## which is the first day that the PWL is actually buried deeper than e.g. 5cm/20cm?
    bdate_low <- suppressWarnings(min(as.Date(ans$lyrs$date[which(ans$lyrs$id == id & ans$lyrs$depth > 5)])))  # ignore warnings and deal with Inf returns in next line
    if (is.infinite(bdate_low)) bdate_low <- NA  # layer actually never got buried, either b/c it disappeared or it still needs to be merged with subsequent fdate layers (mostly in case of SH)
    else if (bdate_low == ans$PWL$first_occurrence[which(ans$PWL$id == id)]) bdate_low <- NA
    ans$PWL$bdate_5cm[which(ans$PWL$id == id)] <- as.character(bdate_low)

    bdate_deep <- suppressWarnings(min(as.Date(ans$lyrs$date[which(ans$lyrs$id == id & ans$lyrs$depth > 20)])))  # ignore warnings and deal with Inf returns in next line
    if (is.infinite(bdate_deep)) bdate_deep <- NA  # layer actually never got buried, either b/c it disappeared or it still needs to be merged with subsequent fdate layers (mostly in case of SH)
    else if (bdate_deep == ans$PWL$first_occurrence[which(ans$PWL$id == id)]) bdate_deep <- NA
    ans$PWL$bdate_20cm[which(ans$PWL$id == id)] <- as.character(bdate_deep)
  }



  ## --- more merging approaches ----
  ## merge all PWLs with identical bdate_20cm
  if ("bdate_20cm" %in% mergeMethods) {
    ids2loop <- ans$PWL$id[which(!is.na(ans$PWL$bdate_20cm))]
    for (id in ids2loop) {
      if (id %in% ans$PWL$id) {
        ids2merge <- unique(c(id, ans$PWL$id[which(ans$PWL$bdate_20cm == ans$PWL$bdate_20cm[ans$PWL$id == id])]))
        idx2merge <- which(ans$PWL$id %in% ids2merge)
        if (length(idx2merge) > 1) ans <- suppressWarnings(summarizePWLatGridpoint_sub(ans$PWL, idx2merge, ans$lyrs))
      }
    }
  }

  ## define periods with surface change and merge all adjacent layers when one of them was at the snow surface within these periods:
  ## i) larger snowfall events and/or accumulated smaller snowfall events based on bdate_20cm
  ## ii) melting events with MF or MFcr at sfc
  if ("sfc_periods" %in% mergeMethods) {
    ## compute PWL IDs of adjacent layers:
    ans$lyrs <- getPWLidAdjacentLayers(ans$lyrs)

    dayOfSeason_start <- min(ans$lyrs$date)
    dayOfSeason_end <- max(ans$lyrs$date)
    sfc_periods <- sort(unique(as.Date(c(ans$lyrs$date[ans$lyrs$height >= 20][1],  # first day of season with hs >= 20 cm
                     ans$PWL$bdate_20cm[which(!is.na(ans$PWL$bdate_20cm))],  # bdate_20cm
                     ans$lyrs$date[ans$lyrs$depth < 3 & ans$lyrs$gtype %in% c("MF", "MFcr")]  # melting at surface
    ))))
    ids2loop <- ans$PWL$id[which(is.na(ans$PWL$bdate_20cm))]
    for (id in ids2loop) {
      if (id %in% ans$PWL$id) {
        # sfc_days <- unique(ans$lyrs$date[which(ans$lyrs$depth < 3 & ans$lyrs$id == id)])  # dates when that layer was at surface
        sfc_days <- sort(c(ans$PWL$ddate_max[ans$PWL$id == id], ans$PWL$first_occurrence[ans$PWL$id == id]))  # ddate_max and first occurrence dates
        # if (length(sfc_days) > 0) {
        # if (all(sfc_days %in% c("2018-10-18", "2018-10-23"))) browser()
        # if (id == 11) browser()
        if (sum(sfc_periods > sfc_days[1] & sfc_periods < sfc_days[2]) == 0) {  # ddate and first occurrence are within same period
          start <- max(c(dayOfSeason_start, suppressWarnings(max(sfc_periods[sfc_periods <= min(sfc_days)]))))  # first day of season or appropriate earlier sfc_period
          end <- min(c(suppressWarnings(min(sfc_periods[sfc_periods >= max(sfc_days)])) - 1, dayOfSeason_end))  # last day of season or appropriate later sfc_period (-1 day)
          PWL2choosefrom <- ans$PWL[ans$PWL$id != id, ]

          ids2merge <- unique(c(id,
                                PWL2choosefrom$id[which(
                                  PWL2choosefrom$ddate_min <= end & PWL2choosefrom$first_occurrence <= end &
                                  PWL2choosefrom$id %in% ans$lyrs$id[which(ans$lyrs$id_adjacentAbove == id | ans$lyrs$id_adjacentBelow == id)]
                                  )]
                                ))
          idx2merge <- which(ans$PWL$id %in% ids2merge)
          if (length(idx2merge) > 1) ans <- suppressWarnings(summarizePWLatGridpoint_sub(ans$PWL, idx2merge, ans$lyrs))
        }
      }
    }
  }

  ## merge layers that first occurred long after burial through faceting and only have an adjacent PWL either above or below:
  if ("outside_adjacency" %in% mergeMethods) {
    ## compute PWL IDs of adjacent layers:
    ans$lyrs <- getPWLidAdjacentLayers(ans$lyrs)

    ids2loop <- ans$PWL$id[which(ans$PWL$ddate_max < (ans$PWL$first_occurrence - 10))]
    for (id in ids2loop) {
      if (id %in% ans$PWL$id) {
        ids_above <- ans$lyrs$id_adjacentAbove[ans$lyrs$id == id]
        ids_below <- ans$lyrs$id_adjacentBelow[ans$lyrs$id == id]
        if (all(ids_above %in% c(id, NA)) | all(ids_below %in% c(id, NA))) {
          ids2merge <- unique(c(id, ids_above[!is.na(ids_above)][1], ids_above[!is.na(ids_above)][1]))
          idx2merge <- which(ans$PWL$id %in% ids2merge[!is.na(ids2merge)])
          if (length(idx2merge) > 1) ans <- suppressWarnings(summarizePWLatGridpoint_sub(ans$PWL, idx2merge, ans$lyrs))
        }
      }
    }
  }


  ## --- closing manipulations ----
  ## compute/update PWL IDs of adjacent layers:
  ans$lyrs <- getPWLidAdjacentLayers(ans$lyrs)

  ans$PWL$id <- paste0(station_id, "-", ans$PWL$id)
  ans$lyrs$id <- paste0(station_id, "-", ans$lyrs$id)
  ans$lyrs <- ans$lyrs[!is.na(ans$lyrs$id), ]
  ans$sfc_periods <- sfc_periods

  return(ans)



































}


