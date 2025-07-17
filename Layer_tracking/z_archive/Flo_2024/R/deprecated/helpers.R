#'
#'
summarizePWLatGridpoint_sub <- function(PWL, idx2merge, lyrs) {

  id_new <- max(PWL[idx2merge, "id"])
  lyrs$id[lyrs$id %in% PWL$id[idx2merge]] <- id_new

  PWL[idx2merge, "id"] <- id_new
  PWL[idx2merge, "nL"] <- length(which(lyrs$id == id_new))
  PWL[idx2merge, "ddate_start"] <- min(PWL[idx2merge, "ddate_start"], na.rm = TRUE)
  PWL[idx2merge, "ddate_end"] <- max(PWL[idx2merge, "ddate_end"], na.rm = TRUE)
  PWL[idx2merge, "ddate_min"] <- min(PWL[idx2merge, "ddate_min"], na.rm = TRUE)
  PWL[idx2merge, "ddate_max"] <- max(PWL[idx2merge, "ddate_max"], na.rm = TRUE)
  PWL[idx2merge, "fdate_start"] <- min(PWL[idx2merge, "fdate_start"], na.rm = TRUE)
  PWL[idx2merge, "fdate_end"] <- max(PWL[idx2merge, "fdate_end"], na.rm = TRUE)
  PWL[idx2merge, "bdate_start"] <- min(PWL[idx2merge, "bdate_start"], na.rm = TRUE)
  PWL[idx2merge, "bdate_end"] <- max(PWL[idx2merge, "bdate_end"], na.rm = TRUE)
  PWL[idx2merge, "bdate_5cm"] <- median(PWL[idx2merge, "bdate_5cm"], na.rm = TRUE)
  PWL[idx2merge, "bdate_20cm"] <- median(PWL[idx2merge, "bdate_20cm"], na.rm = TRUE)
  PWL[idx2merge, "first_occurrence"] <- min(PWL[idx2merge, "first_occurrence"], na.rm = TRUE)
  PWL[idx2merge, "last_occurrence"] <- max(PWL[idx2merge, "last_occurrence"], na.rm = TRUE)
  PWL[idx2merge, "gtypeATddate"] <- names(sort(table((PWL[idx2merge, "gtypeATddate"]), useNA = "ifany"), decreasing = TRUE))[1]
  PWL[idx2merge, "gtypeATfdate"] <- NA
  PWL[idx2merge, "mergeWith"] <- NA

  dupls <- which(duplicated(PWL))
  if (length(dupls) > 0) PWL <- PWL[-dupls, ]
  rownames(PWL) <- seq(nrow(PWL))

  return(list(PWL = PWL, lyrs = lyrs))
}


#'
#'
getPWLidAdjacentLayers <- function(lyrs) {
  lyrs$id_adjacentAbove <- NA
  lyrs$id_adjacentBelow <- NA

  for (datetime in unique(lyrs$datetime)) {
    nL_datetime <- length(lyrs$datetime[lyrs$datetime == datetime])
    if (nL_datetime > 1) {
      if (!isTRUE(all.equal(lyrs$height[lyrs$datetime == datetime][1:nL_datetime-1],
                            lyrs$height[lyrs$datetime == datetime][2:nL_datetime] - lyrs$thickness[lyrs$datetime == datetime][2:nL_datetime]))) {
        stop("Wrong layer order in 'lyrs'!")
      }
      lyrs$id_adjacentAbove[lyrs$datetime == datetime] <- c(lyrs$id[lyrs$datetime == datetime][2:nL_datetime], NA)
      lyrs$id_adjacentBelow[lyrs$datetime == datetime] <- c(NA, lyrs$id[lyrs$datetime == datetime][1:nL_datetime-1])
    }
  }
  return(lyrs)
}

