## This script contains several important helper subfunctions used in `PWLcapturedByModel.R`


## --- getMovAvgHazFactor ----
#' Extract hazard factors from avalanche problem assessments based on a moving date window stencil
#'
#' @param avprobs a data frame of avalanche problems
#' @param vdate validation date (Date or character string, not POSIXct!)
#' @param hazFactor location, distribution, or sensitivity
#' @param daterange the moving date window stencil
#'
#'
#' @export
getMovAvgHazFactor <- function(avprobs, vdate, hazFactor, daterange = seq(-2, 2, 1)) {
  ## first: try to get aspects from persistent problems
  probs <- avprobs[avprobs$CHARACTER %in% c("Persistent Slabs", "Deep Persistent Slabs") &
                      as.Date(avprobs$PUBLISH_DATE) %in% (as.Date(vdate) + as.difftime(daterange, unit = "days")), ]
  ## if no persistent problems within daterange --> also use other problems (i.e., storm)
  if (nrow(probs) == 0) probs <- avprobs[as.Date(avprobs$PUBLISH_DATE) %in% (as.Date(vdate) + as.difftime(daterange, unit = "days")), ]

  if (hazFactor == "location") {
    asps <- substr(sapply(names(probs)[grepl("ASP_", names(probs))], function(asp) rep(asp, times = nrow(probs)))
                   [which(!is.na(probs[grepl("ASP_", names(probs))]))],
                   5, 7)
    if (length(asps) > 0) {
      agg <- aggregate(asps, by = list(asps = asps), FUN = length)
      ASP <- agg$asps[agg$x >= (nrow(probs)/2)]
    } else {
      ASP <- NULL
    }
    return(ASP)
  }

  if (nrow(probs) == 0) return(as.numeric(NA))
  if (hazFactor == "distribution") {
    return(mean(as.numeric(factor(probs$DISTRIBUTION, levels = c("Isolated", "Specific", "Widespread"), ordered = TRUE))))
  } else if (hazFactor == "sensitivity") {
    return(mean(as.numeric(factor(probs$SENSITIVITY, levels = c("Unreactive", "Stubborn", "Reactive", "Touchy"), ordered = TRUE))))
  } else if (hazFactor == "likelihood_typ") {
    return(mean(as.numeric(probs$LIKELIHOOD_TYP)))
  } else if (hazFactor == "likelihood_spread") {
    return(mean(as.numeric(probs$LIKELIHOOD_MAX - probs$LIKELIHOOD_MIN)))
  } else if (hazFactor == "size_typ") {
    return(mean(as.numeric(probs$SIZE_TYP)))
  } else if (hazFactor == "size_spread") {
    return(mean(as.numeric(probs$SIZE_MAX - probs$SIZE_MIN)))
  }
}





## --- getRelevantGtype ----
#' Extract relevant weak layer grain types for a pwl contained in WklList
#'
#' Due to adjustments of our research strategy when this part of the software had already been developed, it is important to carefully read this description to clearly understand how the analysis deals with gtypes!
#' In the script(s) `scripts/prepareWkl_GNP.R` (and analogous others) each tracked Wkl is associated with a **primary**, **secondary**, and **tertiary** grain type. While the primary **and** secondary grain type
#' can be thought of as the main grain types of concern (e.g., Jan 17th SH/FC --> primary: SH, secondary: FC; both are eqyally important), the tertiary grain type can be thought of as an additional grain type that might also
#' be present and potentially also concerning (albeit less) (e.g., Feb 10th SH/MFcr --> primary: SH, tertiary: MFcr). There might be cases of Wkls that are primarily a crust, and tertiarily a SH layer, or vice versa.
#'
#' Important: The just described nomenclature (applied to the WklList) is changed within this function and therefore in the database output of the validation! Despite which gtype is associated to primary versus tertiary,
#' this function will *always* make the crust gtype of tertiary rank, so that
#'
#'   * primary rank refers to all non-crust gtypes mentioned in assessments/ comments (i.e., primary and secondary non-crust gtypes from WklList above)
#'   * secondary rank refers to all persistent non-crust gtypes (= SH DH FC FCxr) (if not all of these gtypes are already present in the primary rank)
#'   * tertiary rank refers to all crust gtypes (if present)
#'
#' Example A:
#'
#'   * `WklList[[A]]$ASSESS$GRAIN_TYPE == SH`, `WklList[[A]]$ASSESS$GRAIN_TYPE_SECONDARY == NULL`, `WklList[[pwl]]$ASSESS$GRAIN_TYPE_TERTIARY == IFsc`
#'   * --> database vframe.gtype_rank: primary = SH DH, secondary = SH DH FC FCxr, tertiary = IFsc MFcr
#'
#' Example B:
#'
#'   * `WklList[[B]]$ASSESS$GRAIN_TYPE == IFrc`, `WklList[[A]]$ASSESS$GRAIN_TYPE_SECONDARY == NULL`, `WklList[[pwl]]$ASSESS$GRAIN_TYPE_TERTIARY == FC`
#'   * --> database vframe.gtype_rank: primary = FC FCxr DH, secondary = FC FCxr DH SH, tertiary = IFrc MFcr
#'
#' Note that all of the ranks can potentially be missing. If a layer is purely a crust layer, primary and secondary will be missing in database. If primary would be identical to secondary, secondary will be missing in the database.
#' And if there is no crust, tertiary will be missing in the database.
#'
#' To still know which layer is primarily a crust layer, the database table `wkl` contains a column `iscrust`, which is set to 1 if `WklList[[X]]$ASSESS$GRAIN_TYPE == MFcr IFrc IFsc`, and 0 otherwise.
#'
#' @param pwl character name of pwl
#' @param WklList ~
#'
#' @return a list where each element holds a vector of grain types to search (e.g., `list[1] = c("SH", "DH")`, `list[2] = c("IFsc", "MFcr")`).
#' The names of the list entries are 'primary', 'secondary', and 'tertiary' (in that order, if not any one is missing).
#' @export
getRelevantGtype <- function(pwl, WklList) {

  if (length(WklList[pwl]) > 1) stop(paste0("It seems that WklList contains multiple PWLs called ", pwl, ". Update this function to deal with that case!"))

  ## which gtype to look for: classify primary / secondary / tertiary gtype tiers as described in function description
  prim <- unique(c(na.omit(WklList[[pwl]]$ASSESS$GRAIN_TYPE), na.omit(WklList[[pwl]]$ASSESS$GRAIN_TYPE_SECONDARY)))
  secn <- unique(na.omit(WklList[[pwl]]$ASSESS$GRAIN_TYPE_SECONDARY))
  tert <- unique(na.omit(WklList[[pwl]]$ASSESS$GRAIN_TYPE_TERTIARY))

  gtype_assess <- list(primary = NULL, secondary = NULL, tertiary = NULL)

  ## case crust layer
  if (any(c("MFcr", "IFsc", "IFrc", "IF") %in% prim)) {
    gtype_assess$tertiary <- prim
    if (any(c("SH", "DH", "FC", "FCxr") %in% tert)) {
      gtype_assess$primary <- tert
      gtype_assess$secondary <- unique(c(tert, c("SH", "DH", "FC", "FCxr")))
    } else {
      gtype_assess$secondary <- c("FCxr", "SH", "DH", "FC")
    }
  } else {  # case persistent layer
    gtype_assess$primary <- prim
    if (length(secn) == 0 | isTRUE(!secn %in% prim)) gtype_assess$secondary <- unique(c(prim, c("SH", "DH", "FC", "FCxr")))
    if (all(gtype_assess$secondary %in% gtype_assess$primary)) gtype_assess$secondary <- NULL
    if (any(c("MFcr", "IFsc", "IFrc", "IF") %in% tert)) gtype_assess$tertiary <- tert
  }

  gtype_assess <- gtype_assess[!sapply(gtype_assess, is.null)]

  RELEVANT_GTYPE <- lapply(gtype_assess, function(gta) {
    ## ensure that model-equivalent gtypes are being searched for
    relevant_gtype <- gta
    if (any(c("FC", "DH") %in% relevant_gtype)) {
      relevant_gtype <- c(relevant_gtype, "FC", "FCxr", "DH")
    }
    if (any(c("IFsc") %in% relevant_gtype)) {
      relevant_gtype <- c(relevant_gtype, "MFcr")
    } else if (any(c("IFrc") %in% relevant_gtype)) {
      relevant_gtype <- c(relevant_gtype, "MFcr")
    }
    if ("SH" %in% relevant_gtype) {
      relevant_gtype <- c(relevant_gtype, "DH")
    }
    unique(relevant_gtype)
  })

  names(RELEVANT_GTYPE) <- names(gtype_assess)
  return(RELEVANT_GTYPE)
}


## --- getAvprobs ----
## this is actually as simple as
# avprobs <- WklList[[pwl]]$AVPROB[[band]]


## --- getMaxElev ----
#' Get max elev of a PWL from ASSESS frame in WklList
#'
#' @param pwl character name of pwl
#' @param WklList ~
#' @return numeric scalar
#' @export
getMaxElev <- function(pwl, WklList) {

  max_elev <- unique(WklList[[pwl]]$ASSESS$MAX_ELEV)
  if (is.null(max_elev)) max_elev <- 9999

  return(max_elev)
}


## --- getSubsetOfSimul ----
#' Subset relevant snow profiles from a large simulation data set
#'
#' ... based on date, band, max_elev, and optionally relevant_gtype and aspects.
#'
#' @param simul large snowprofileSet containing entire simulation
#' @param sm a summary(simul) containing meta data
#' @param vdate validation date as character string "YYYY-MM-DD"
#' @param band elevation band character string (ALP, TL, BTL), case insensitive
#' @param gtype_class only required if IFsc to constrain aspects
#' @param validateAspects extract only flat field profiles (FALSE) or also virtual slopes (TRUE)?
#' @param max_elev extract profiles below this elevation
#' @param avprobs if validateAspects is TRUE, and you still need to extract the relevant aspects, provide a data frame containing the relevant avalanche problem assessments
#' @param aspects if not NULL, validateAspects is automatically set to TRUE. Can provide aspects instead of avprobs.
#'
#' @return a subset snowprofileSet of the input data set
#'
#' @export
getSubsetOfSimul <- function(simul, sm, vdate, band, gtype_class = "NA", validateAspects = FALSE, max_elev = 9999, avprobs = NULL, aspects = NULL) {

  if (!is.null(aspects)) validateAspects <- TRUE
  if (validateAspects) {
    # if (all(is.null(aspects)) & all(is.null(avprobs))) stop("Provide either avprobs, or aspects!")
    if (all(is.null(aspects)) & !all(is.null(avprobs))) aspects <- try({char2numAspect(getMovAvgHazFactor(avprobs, vdate, "location"))}, silent = TRUE)
    ## define rule for when no avprobs exist around the day of interest
    if (length(aspects) == 0) {
      # warning(paste0("Found no aspects in av problem assessment, returning NULL subset of profiles"))
      return(list(profiles = NULL, meta = NULL))
    }
    if (gtype_class == "IFsc") aspects <- 180  # aspects[match_with_tolerance(aspects, c(135, 180, 225, 270))]

    simul_sub <- tryCatch({simul[which(sm$date == vdate & tolower(sm$band) == tolower(band) &
                                         sm$angle != 0 & match_with_tolerance(sm$aspect, aspects) &
                                         sm$elev <= max_elev)]},
                          error = function(err) stop(err))
    sm_sub <- sm[which(sm$date == vdate & tolower(sm$band) == tolower(band) &
                         sm$angle != 0 & match_with_tolerance(sm$aspect, aspects) &
                         sm$elev <= max_elev), ]
  } else {
    simul_sub <- tryCatch({simul[which(sm$date == vdate & tolower(sm$band) == tolower(band) &
                                         sm$elev <= max_elev)]},
                          error = function(err) stop(err))
    sm_sub <- sm[which(sm$date == vdate & tolower(sm$band) == tolower(band) &
                         sm$elev <= max_elev), ]
  }
  # if (length(simul_sub) == 0) warning(paste0("Empty simulation subset. band: ", band, ", vdate: ", vdate, ", max_elev: ", max_elev))
  if (isTRUE(nrow(sm_sub) / length(unique(sm_sub$station_id)) > 1.01)) warning(paste0("Multiple profiles per station per day! band: ", band, ", vdate: ", vdate, "."), immediate. = TRUE)
  return(list(profiles= simul_sub, meta = sm_sub))
}

## --- getWxTimeWindow ----
#' Calculate PWL formation time window around burial date based on wx data
#'
#' Max time window as provided by arguments will be made tighter if cumulative hn24 > 10 cm or cumulative rain24 > 5 mm.
#'
#' @param profile only required to obtain station_id
#' @param pwl_bdate burial date of PWL
#' @param wx_daily data frame of daily resampled wx data (need `$rain24`, `$hn24`, `$station_id`)
#' @param timewindow_lower max window size before human burial date tag
#' @param timewindow_upper max window size after human burial date tag
#'
#' @return vector of length 2, providing lower and upper time window limits, respectively.
#'
#' @export
getWxTimeWindow <- function(profile, pwl_bdate, wx_daily, timewindow_lower = -30, timewindow_upper = 3) {

  wx_daily_station <- wx_daily[wx_daily$station_id == substr(profile$station_id, 1, 9), ]
  wx_daily_station <- wx_daily_station[order(wx_daily_station$date), ]

  timelimit_lower <- tryCatch({
    ## this new approach prunes the timewindow to the day where the cumulative hn24 exceeds 10cm (or 5mm for rain24)
    ## the day of exceeding this threshold is included in the timewindow!
    subsetgt0 <- which(wx_daily_station$date == pwl_bdate) + seq(-1, timewindow_lower, -1)
    subsetgt0 <- subsetgt0[subsetgt0 > 0]
    dt <- wx_daily_station$date[subsetgt0]
    hn24cum <- cumsum(wx_daily_station$hn24[subsetgt0])
    rain24cum <- cumsum(wx_daily_station$rain24[subsetgt0])
    max(timewindow_lower,
        suppressWarnings(max(dt[hn24cum > 10 | rain24cum > 5], na.rm = TRUE) - pwl_bdate),
        na.rm = TRUE)
    ## deprecated: this old approach used the actual day that exceeded the snowfall/rain thresholds without accumulating them (= rather unrealistic thresholds for BYK, for example)
    # max(timewindow_lower,
    #     suppressWarnings(max(wx_daily$date[wx_daily$date < pwl_bdate & (wx_daily$hn24 > 10 | wx_daily$psum24 > 5) & wx_daily$station_id == substr(profile$station_id, 1, 9)])) - pwl_bdate,
    #     na.rm = TRUE)
  }, warning = function(warn) {
    # message("Warning in getWxTimeWindow, browse..")
    warning(warn, immediate. = TRUE)
  })

  timelimit_upper <- tryCatch({
    ## this new approach prunes the timewindow to the day where the cumulative hn24 exceeds 10cm (or 5mm for rain24)
    ## the day of exceeding this threshold is included in the timewindow!
    subsetgt0 <- which(wx_daily_station$date == pwl_bdate) + seq(0, timewindow_upper, 1)
    subsetgt0 <- subsetgt0[subsetgt0 > 0]
    dt <- wx_daily_station$date[subsetgt0]
    hn24cum <- cumsum(wx_daily_station$hn24[subsetgt0])
    rain24cum <- cumsum(wx_daily_station$rain24[subsetgt0])
    min(timewindow_upper,
        suppressWarnings(min(dt[hn24cum > 10 | rain24cum > 5], na.rm = TRUE) - pwl_bdate),
        na.rm = TRUE)
    ## deprecated: this old approach used the actual day that exceeded the snowfall/rain thresholds without accumulating them (= rather unrealistic thresholds for BYK, for example)
    # min(timewindow_upper,
    #     suppressWarnings(min(wx_daily$date[wx_daily$date > pwl_bdate & (wx_daily$hn24 > 10) & wx_daily$station_id == substr(profile$station_id, 1, 9)])) - pwl_bdate,
    #     na.rm = TRUE)
  }, warning = function(warn) {
    # message("Warning in getWxTimeWindow, browse..")
    warning(warn, immediate. = TRUE)
  })

  return(c(timelimit_lower, timelimit_upper))
}

## --- PWLcapturedByIndividualProfile ----
#' Confirm whether or not a specific layer exists in an individual profile
#' @param x snowprofile with additional fields timelimit_lower and timelimit_upper that specify the beginning of ddate time window (which goes up until pwl_bdate)
#' and the end of bdate time window (which starts 1 day before pwl_bdate)
#' @param gtypes to search for
#' @param pwl_bdate from WklList
#'
#' @return indices of captured layers
#'
#' @export
PWLcapturedByIndividualProfile <- function(x, gtypes, pwl_bdate) {

  sarp.snowprofile::findPWL(x,
                            gtypes,
                            pwl_bdate,
                            threshold_RTA = 0.8, threshold_SK38 = 1.0, threshold_RC = 0.4, threshold_PU = 0.77,
                            threshold_gtype = c("FC", "FCxr"),
                            date_range_earlier = as.difftime(x$timelimit_lower, units = "days"), date_range_later = as.difftime(0, units = "days"),
                            bdate_range_earlier = as.difftime(-1, units = "days"), bdate_range_later = as.difftime(x$timelimit_upper, units = "days"))

}



## --- vizCapturedPWL ---
#' Vizualize and manually verify whether layer is captured or not
#'
#' This function can also be used to reconstruct the simulation subset at a later point
#'
#' @export
vizCapturedPWL <- function(WklList, simul = NA, meta = NA, validateAspects = FALSE, vf = NA,
                           pwl = vf$pwl, vdate = vf$validation_date, band = vf$band, gtype_class = vf$gtype_class,
                           timelimit_lower = vf$timewindow_lower, timelimit_upper = vf$timewindow_upper,
                           relevant_gtypes = getRelevantGtype(pwl, WklList)[[gtype_class]], gtype_rank = NA,
                           simul_subset = NA, wx_daily = NA, pwl_exists = NA,
                           savePNG = FALSE, savePNG_prefix = NA, verbose = TRUE, keep.results = TRUE) {
  # if (verbose) message(pwl)
  ## subset simulation
  if (is.na(simul_subset[1])) {
    simul_subset <- getSubsetOfSimul(simul, meta, vdate, band, gtype_class, validateAspects = validateAspects, max_elev = getMaxElev(pwl, WklList), avprobs = WklList[[pwl]]$AVPROB[[band]])
  }

  ## check whether layer captured
  if (is.na(pwl_exists[1])) {
    if (is.na(timelimit_lower) & all(is.na(wx_daily))) stop("If timelimit is NA, wx_daily must be provided!")
    pwl_exists <- sapply(simul_subset$profiles, function(sp) {
      if (is.na(timelimit_lower)) {
        timelimit <- getWxTimeWindow(sp, WklList[[pwl]]$BURIAL_DATE, wx_daily, timewindow_lower = -5, timewindow_upper = 3)
        sp$timelimit_lower <- timelimit[1]
        sp$timelimit_upper <- timelimit[2]
      }
      length(PWLcapturedByIndividualProfile(sp, relevant_gtypes, WklList[[pwl]]$BURIAL_DATE)) > 0
    })
  }


  ## offset Set plot
  signum <- c("-", "", "+")
  do.plot <- function() {
    k <- order(pwl_exists, max(simul_subset$meta$hs) - simul_subset$meta$hs, decreasing = TRUE)
    plot(simul_subset$profiles, SortMethod = 'presorted', k = k, xticklabels = "originalIndices", offset = as.Date(WklList[[pwl]]$BURIAL_DATE),
         main = paste0(pwl, ", bdate: ", format(as.Date(WklList[[pwl]]$BURIAL_DATE), "%b %d"), ", vdate (", as.character(as.Date(vdate) - as.Date(WklList[[pwl]]$BURIAL_DATE)), "): ", vdate, ", ", paste0(relevant_gtypes, collapse = " "), " (", gtype_rank, ")"),
         ylab = "Offset height (cm) -- 0 <~> bdate", ylim = quote(c(max(min(y1), -100), min(100, max(y2)))), yPadding = 0)
    abline(v = ifelse(sum(pwl_exists) > 0, max(which(pwl_exists[k]))+0.5, 0.5), lty = "dashed")
  }
  if (savePNG) {
    fname <- paste0(savePNG_prefix, format(as.Date(WklList[[pwl]]$BURIAL_DATE), "%Y"), "-", format(as.Date(WklList[[pwl]]$BURIAL_DATE), "%b-%d"), "_", gtype_rank, "_",
                    signum[sign(as.Date(vdate) - as.Date(WklList[[pwl]]$BURIAL_DATE))+2], as.character(as.Date(vdate) - as.Date(WklList[[pwl]]$BURIAL_DATE)), "_vdate", vdate, ".png")
    png(fname, width = 1800, height = 1300)
    tryCatch({do.plot()},
             error = function(err) {
               try({dev.off()}, silent = TRUE)
               stop(err)
             })
    dev.off()
    if (verbose) message(paste0("Wrote ", fname))
  } else {
    do.plot()
  }

  if (keep.results) return(list(subset = simul_subset, pwl_exists = pwl_exists, vdate = vdate, pwl = pwl))
}


