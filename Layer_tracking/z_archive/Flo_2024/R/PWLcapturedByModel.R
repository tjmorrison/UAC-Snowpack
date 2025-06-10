#' Check whether persistent weak layer is captured in model simulations
#'
#' This function validates whether PWLs reported in avalanche forecasting assessments also exist in model simulations.
#' In the paper flowchart (Fig. 2a), this function corresponds to the matching step at the grid point level.
#'
#' @param WklList a list of PWLs as extracted by `SarpBulletinTools::getWklInfoForBulletins`. Note that this routine will put `$GRAIN_TYPE` and `$GRAIN_TYPE_SECONDARY` into one
#' gtype class and `$GRAIN_TYPE_TERTIARY` into another one. See code for further clarification.
#' @param simul a `snowprofileSet` containing the simulations. The function will assess every elevation band contained in `simul`.
#' @param wx_daily a data.frame containing weather information which are aggregated to 24h intervals (required fields are `$hn24` and `$rain24`). If `wx_daily` is provided, the timewindow to
#' search for PWLs in the simulation is modified based on the underlying weather conditions. That means, `timewindow_lower` and `timewindow_upper` are pruned if hn24 (rain24) exceeds 10 cm (5 mm).
#' This feature is ignored when `wx_daily` is `NA`.
#' @param sm a summary data.frame for simul
#' @param bulletins Bulletins object that will be used to extract danger rating. Must only contain one danger rating per day and elevation band (i.e., only contain one region!) (optional)
#' @param when timing mode of validation. Choose between `"day #"`, `"first problem"`, `"first persistent problem"`, `"first deep persistent problem"`, or `"each problem day"`
#' @param validateAspects boolean switch to consider aspects while validating (i.e., extract hazardous aspects and search in corresponding profiles)
#' @param timewindow_lower scalar of unit 'days' before the reported weak layer datetag to start ddate search window in simulation
#' @param timewindow_upper scalar of unit 'days' after the reported weak layer datetag to end bdate search window in simulation
#' @param savePNG_prefix prefix to manual validation verification figure, such as directory and prefix to filename. automatic filename postfix will include pwlidx, bdate, vdate, & gtype_class. e.g.,
#' `"/home/flo/documents/sfu/code/rpackages_SARP/sarp.2021.herla.snowprofilevalidation/output/figures/validationVerificationPlots/gnp/"`. If `NA` no figure will be printed.
#' @param verbose print messages related to function progress?
#' @param DBCon an established DB connection. If NULL, no results will be written to a database (i.e., dry run)
#' @param region if DBCon provided, region must be supplied, too. e.g., "GLACIER"
#' @param exec_uuid execution UUID for DB management
#' @param saveCapturedWKL write captured layers to DB?
#' @param saveVframe write validation data.frame to DB?
#'
#' @return A data frame that contains the validation information. Additionally, the routine can plot manual validation verification figures to manually check whether the weak layers
#' were correctly deemed captured/not captured. And finally, the routine can save all captured layers to a mysql database either at Cedar or at steed (local fherla).
#' @export
PWLcapturedByModel <- function(WklList, simul, wx_daily = NA, sm = summary(simul), bulletins = NA,
                               when = "day 3",
                               validateAspects = FALSE,
                               timewindow_lower = -30, timewindow_upper = 3,
                               savePNG_prefix = NA, verbose = TRUE,
                               DBCon = NULL,  region = NA, exec_uuid = ifelse(is.null(DBCon), NA, stop("Must provide exec_uuid if writing to DB!")),
                               saveCapturedWKL = ifelse(is.null(DBCon), FALSE, TRUE), saveVframe = ifelse(is.null(DBCon), FALSE, TRUE)) {

  ## ---Assertion & Initialization----
  if (!inherits(WklList, "WklList")) stop("WklList needs to be of class WklList --> see SarpBulletinTools::getWklInfoForBulletins")
  if (!is.snowprofileSet(simul)) stop("simul needs to be of class 'snowprofileSet")
  if (any(duplicated(names(WklList)))) stop("WklList contains duplicate WKL names. That's dangerous and not allowed!")


  if (all(is.na(wx_daily))) {
    timelimit_lower <- timewindow_lower
    timelimit_upper <- timewindow_upper
  } else {
    timelimit_lower <- NA
    timelimit_upper <- NA
  }
  bands_simul <- unique(sm$band)

  nrow_max <- sum(sapply(names(WklList), function(wkl) {
    sum(sapply(names(WklList[[wkl]]$AVPROB), function(bnd) {95*3}))
  }))

  ## initialize matrices to store validation results
  WklValidation <- matrix(nrow = nrow_max, ncol = 13, dimnames = list(seq(nrow_max), c("distribution", "nProfiles_checked", "day_after_burial", "timewindow_lower",
                                                                                      "timewindow_upper", "likelihood_reported", "likelihoodSpread_reported", "distribution_reported",
                                                                                      "sensitivity_reported", "size_reported", "sizeSpread_reported", "grain_size_reported", "data_quality")))
  WklValidation_char <- matrix(nrow = nrow_max, ncol = 10, dimnames = list(seq(nrow_max), c("vf_uuid", "pwl_uuid", "pwl", "validation_date", "band", "timing_mode", "gtype_class", "gtype_rank", "comment", "danger_rating")))
  vf_uuid <- NA

  ## retrieve wkl_uuids
  if (any(c(saveCapturedWKL, saveVframe))) {
    if (is.na(region)) stop("PWLcapturedByModel interacts with DB. region argument required!")
    wkl_table <- getWKLuuid(DBCon, stringr::str_remove(names(WklList), "'"), region = region)
  }


  ## ---LOOPs----
  for (i in seq_along(WklList)) {  ## LOOP over each pwl
    pwl <- names(WklList[i])
    if (any(c(saveCapturedWKL, saveVframe))) pwl_uuid <- wkl_table$wkl_uuid[wkl_table$wkl_name == pwl]
    else pwl_uuid <- as.character(i)

    RELEVANT_GTYPE <- getRelevantGtype(pwl, WklList)  # returns a list where each entry holds a vector of similar grain types to search for; names of list elements are tier ranks (primary, ...)

    if (!length(RELEVANT_GTYPE) == 0) {
      for (rgi in seq(length(RELEVANT_GTYPE))) {   ## LOOP over pwl gtype ranks
        relevant_gtype <- RELEVANT_GTYPE[[rgi]]
        gtype_rank <- names(RELEVANT_GTYPE)[[rgi]]
        grain_size_reported <- as.numeric(WklList[[pwl]]$ASSESS[1, paste0("GRAIN_SIZE_", toupper(gtype_rank))])
        if (length(grain_size_reported) == 0) grain_size_reported <- NA

        for (band in names(WklList[[pwl]]$AVPROB)) {  # LOOP over each band
          if (tolower(band) %in% tolower(bands_simul)) {

            ## extract corresponding avalanche problem assessments (only storm & (deep) persistent!):
            avprobs <- WklList[[pwl]]$AVPROB[[band]]
            if (nrow(avprobs) > 0) {
              avprobs <- avprobs[avprobs$CHARACTER %in% c("Persistent Slabs", "Deep Persistent Slabs", "Storm Slabs"), ]

              ## when: date of validation
              if (when == "first persistent problem") {
                validation_dates <- suppressWarnings(min(as.Date(avprobs$PUBLISH_DATETIME[avprobs$CHARACTER %in% c("Persistent Slabs")])))
                if (is.na(validation_dates) | is.infinite(validation_dates)) {
                  validation_dates <- NULL  #as.Date(min(avprobs$PUBLISH_DATETIME[avprobs$LIKELIHOOD_TYP == max(avprobs$LIKELIHOOD_TYP)]))  # if problem never turns persistent, use first day when likelihood was max
                }
              } else if (when == "first deep persistent problem") {
                validation_dates <- suppressWarnings(min(as.Date(avprobs$PUBLISH_DATETIME[avprobs$CHARACTER %in% c("Deep Persistent Slabs")])))
                if (is.na(validation_dates) | is.infinite(validation_dates)) {
                  validation_dates <- NULL
                }
              } else if (when == "first problem") {
                validation_dates <- suppressWarnings(min(as.Date(avprobs$PUBLISH_DATETIME)))
                if (is.na(validation_dates) | is.infinite(validation_dates)) {
                  validation_dates <- NULL
                }
              } else if (when == "each problem day") {
                validation_dates <- unique(as.Date(avprobs$PUBLISH_DATETIME))
                if (all(is.na(validation_dates)) | all(is.infinite(validation_dates))) {
                  validation_dates <- NULL
                }
              } else if (grepl("day", when)) {
                validation_dates <- WklList[[pwl]]$BURIAL_DATE + as.numeric(substr(when, 5, 7))
              } else if (when == "full validation") {
                # fpd <- suppressWarnings(min(as.Date(avprobs$PUBLISH_DATETIME)))  # first problem day
                # fppd <- suppressWarnings(min(as.Date(avprobs$PUBLISH_DATETIME[avprobs$CHARACTER %in% c("Persistent Slabs")])))
                # fdppd <- suppressWarnings(min(as.Date(avprobs$PUBLISH_DATETIME[avprobs$CHARACTER %in% c("Deep Persistent Slabs")])))
                # pd <- unique(as.Date(avprobs$PUBLISH_DATETIME))
                validation_dates <- WklList[[pwl]]$BURIAL_DATE + seq(-5, 90)
                notCaptSince <- 0
              }
              if (length(validation_dates) > 0) {
                for (vdate in as.character(validation_dates)) {  # LOOP over each vdate

                  if (when == "full validation") {

                    avprobDistribution <- getMovAvgHazFactor(avprobs, vdate, "distribution", daterange = 0)
                    avprobSensitivity <- getMovAvgHazFactor(avprobs, vdate, "sensitivity", daterange = 0)
                    avprobLikelihood <- getMovAvgHazFactor(avprobs, vdate, "likelihood_typ", daterange = 0)
                    avprobLikelihoodSpread <- getMovAvgHazFactor(avprobs, vdate, "likelihood_spread", daterange = 0)
                    avprobSz <- getMovAvgHazFactor(avprobs, vdate, "size_typ", daterange = 0)
                    avprobSzSpread <- getMovAvgHazFactor(avprobs, vdate, "size_spread", daterange = 0)
                    bull_ids <- bulletins$Bulletins$BULLETIN_ID[as.Date(bulletins$Bulletins$PUBLISH_DATE) == vdate]
                    ## if you encounter duplicated bulletins, you can list the ones to discard here (optional):
                    bull_ids <- bull_ids[!bull_ids %in% c("PkCanAvalX_3_2020-02-09T1600_6505ca96-4cb8-4dfe-a466-ade228f6f954")]
                    dngr <- as.character(bulletins$DngRating[[band]]$DAY0[bulletins$DngRating[[band]]$BULLETIN_ID %in% bull_ids])
                    if (length(dngr) == 0) {
                      dngr <- as.character(NA)
                    } else if (length(dngr) > 1) {
                      cat(paste0("Multiple danger ratings per band '", band, "', sth is wrong on vdate ", vdate, ":\n"))
                      cat(paste0(as.character(bulletins$DngRating$Btl[bulletins$DngRating[[band]]$BULLETIN_ID %in% bull_ids, c("BULLETIN_ID", "DAY0")]), collapse = ", "))
                      cat("\nSkipping vdate!\n")
                      next
                    }


                    if (notCaptSince > 20) break
                    tmode <- as.character(NA)
                    ptype <- as.character(avprobs$CHARACTER[as.Date(avprobs$PUBLISH_DATE) == as.Date(vdate)])
                    if (length(ptype) == 1) {
                      tmode <- ptype
                    } else if (length(ptype) > 1) {
                      tmode <- ptype[ptype %in% c("Persistent Slabs", "Deep Persistent Slabs")][1]
                      if (length(tmode) == 0) tmode <- ptype[1]
                    }
                  } else {
                    tmode <- when
                    avprobDistribution <- getMovAvgHazFactor(avprobs, vdate, "distribution")
                    avprobSensitivity <- getMovAvgHazFactor(avprobs, vdate, "sensitivity")
                    avprobLikelihood <- getMovAvgHazFactor(avprobs, vdate, "likelihood_typ")
                    avprobLikelihoodSpread <- getMovAvgHazFactor(avprobs, vdate, "likelihood_spread")
                    avprobSz <- getMovAvgHazFactor(avprobs, vdate, "size_typ")
                    avprobSzSpread <- getMovAvgHazFactor(avprobs, vdate, "size_spread")
                    dngr <- as.character(bulletins$DngRating[[band]]$DAY0[bulletins$DngRating[[band]]$BULLETIN_ID %in% bulletins$Bulletins$BULLETIN_ID[as.Date(bulletins$Bulletins$PUBLISH_DATE) == vdate]])
                    if (length(dngr) == 0) {
                      dngr <- as.character(NA)
                    } else if (length(dngr) > 1) {
                      stop("Multiple danger ratings per band, sth is wrong!")
                    }
                  }

                  ## create relevant subset of simulation data (incl. extracting hazardous aspects)
                  subset <- getSubsetOfSimul(simul, sm, vdate, band, relevant_gtype[1], validateAspects = validateAspects, max_elev = getMaxElev(pwl, WklList), avprobs = avprobs)
                  simul_sub <- subset$profiles
                  if (length(simul_sub) != 0) {

                    ## create vf_uuid and data.frame row for capturedWKL table
                    if (saveCapturedWKL | saveVframe) {
                      vf_uuid <- SarpGeneral::createUuid()
                      capturedMetaData <- tryCatch({data.frame(exec_uuid = exec_uuid, wkl_uuid = pwl_uuid, vf_uuid = vf_uuid, vdate = vdate)},
                                                   error = function(err) {
                                                     if (is.na(pwl_uuid) | length(pwl_uuid) == 0) pwl_uuid <- paste0("(missing!) pwl = ", pwl)
                                                     message(paste0("exec_uuid = ", exec_uuid, ", wkl_uuid = ", pwl_uuid, ", vf_uuid = ", vf_uuid, ", vdate = ", vdate))
                                                     stop(err)
                                                   })
                    } else {
                      vf_uuid <- NA
                    }


                    ## find PWL in subset
                    captured_bol <- do.call("c", lapply(simul_sub, function(x) {
                      ## compute timewindows for PWL search
                      if (is.na(timelimit_lower)) {
                        timelimit <- getWxTimeWindow(x, WklList[[pwl]]$BURIAL_DATE, wx_daily, timewindow_lower = timewindow_lower, timewindow_upper = timewindow_upper)
                        x$timelimit_lower <- timelimit[1]
                        x$timelimit_upper <- timelimit[2]
                      } else {
                        x$timelimit_lower <- timewindow_lower
                        x$timelimit_upper <- timewindow_upper
                      }
                      layeridx <- PWLcapturedByIndividualProfile(x, relevant_gtype, WklList[[pwl]]$BURIAL_DATE)
                      capt <- length(layeridx) > 0
                      ## save captured layers to database
                      if (capt & saveCapturedWKL) saveCapturedLayers2DB(x, layeridx, DBCon, capturedMetaData)
                      capt  # return boolean: captured or not?
                    }))
                    distribution_modeled <- sum(captured_bol) / length(captured_bol)

                    ## determine row index for storing information
                    if (all(is.na(WklValidation_char))) krow <- 1
                    else krow <- which(is.na(WklValidation_char))[1]
                    ## write information to data frame
                    tryCatch({
                      WklValidation[krow, ] <- c(round(distribution_modeled, digits = 2),
                                                 length(captured_bol),
                                                 as.difftime(as.Date(vdate) - as.Date(WklList[[pwl]]$BURIAL_DATE), units = "days"),
                                                 timelimit_lower, timelimit_upper,
                                                 avprobLikelihood, avprobLikelihoodSpread, avprobDistribution, avprobSensitivity, avprobSz, avprobSzSpread, grain_size_reported,
                                                 WklList[[pwl]]$dataqualityFH)
                      WklValidation_char[krow, ] <- c(vf_uuid, pwl_uuid, pwl, as.character(vdate), band, as.character(tmode), relevant_gtype[1], gtype_rank, ifelse(!is.null(WklList[[pwl]]$commentFH), WklList[[pwl]]$commentFH, as.character(NA)), dngr)
                    }, error = function(err) stop(err))

                    ## vizualize result for later manual verification
                    if (!is.na(savePNG_prefix)) {
                      tryCatch({
                        vizCapturedPWL(WklList,
                                       pwl = pwl, vdate = vdate, band = band,
                                       relevant_gtypes = relevant_gtype, gtype_rank = gtype_rank,
                                       simul_sub = subset, pwl_exists = captured_bol,
                                       savePNG = TRUE, savePNG_prefix = savePNG_prefix, verbose = verbose, keep.results = FALSE)
                      }, error = function(err) warning(paste0(paste(err), " (during vizCapturedPWL, vdate: ", vdate, " pwl: ", pwl), immediate. = TRUE))
                    }

                    ## prepare for breaking for loop in "full validation"
                    if (when == "full validation") {
                      if (distribution_modeled > 0) {
                        notCaptSince <- 0
                      } else {
                        notCaptSince <- notCaptSince + 1
                      }
                    }

                  }  # END IF profiles exist in simulation subset
                }}  # END LOOP over each validation date
            }}}  # END LOOP over each band
    }}  # END LOOP over each pwl gtype
  }  # END LOOP over each pwl

  WklValidation <- as.data.frame(WklValidation)
  WklValidation <- cbind(WklValidation, WklValidation_char)
  WklValidation <- WklValidation[!is.na(WklValidation$pwl), c("comment", "data_quality", "vf_uuid", "pwl_uuid", "pwl",  "validation_date", "day_after_burial", "band", "distribution", "distribution_reported", "sensitivity_reported",
                                                              "likelihood_reported", "likelihoodSpread_reported", "size_reported", "sizeSpread_reported", "danger_rating", "gtype_class", "gtype_rank", "grain_size_reported", "nProfiles_checked", "timing_mode", "timewindow_lower", "timewindow_upper")]


  ## append additional information to data frame:
  try({WklValidation$season <- SarpGeneral::deriveAvSeasonFromDate(WklValidation$validation_date)})
  try({WklValidation$band <- toupper(WklValidation$band)})

  ## Write validation data frame to database
  if (saveVframe) {

    ## add columns exec_uuid, vasp
    WklValidation$exec_uuid <- exec_uuid
    WklValidation$vasp <- validateAspects
    ## ensure mysql column names
    vars <- c("exec_uuid", "vf_uuid", "pwl_uuid", "validation_date", "day_after_burial", "band", "distribution", "distribution_reported", "sensitivity_reported",
              "likelihood_reported", "likelihoodSpread_reported", "size_reported", "sizeSpread_reported", "danger_rating", "gtype_class", "gtype_rank", "grain_size_reported", "nProfiles_checked", "timing_mode", "timewindow_lower", "timewindow_upper")
    vars_rename <- c("exec_uuid", "vf_uuid", "wkl_uuid", "vdate", "age", "band", "dist", "dist_rep", "sens_rep",
                     "llhd_rep", "llhdSprd_rep", "sz_rep", "szSprd_rep", "dng_rep", "gtype_class", "gtype_rank", "gsize_rep", "nSP", "tmode", "twl", "twu")
    vars_avail <- which(vars %in% names(WklValidation))
    VF <- WklValidation[, vars[vars_avail]]
    names(VF) <- vars_rename[vars_avail]

    ## write to DB
    succ <- insertTable(DBCon, VF, "vframe")
    if (inherits(succ, "error")) {
      message(paste0("Error in PWLcapturedByModel:saveVframe, query:\n ", succ$query, "\n"))
      stop(succ)
    }
  }
  ## final message
  if (saveCapturedWKL) {
    nCapt <- DBI::dbGetQuery(DBCon, paste0("SELECT COUNT(*) FROM `captWKL` WHERE exec_uuid = '", exec_uuid, "'"))
    message(paste0("Wrote  ", nCapt[[1]], " rows in `captWKL`"))
  }
  if (saveVframe) {
    message(paste0("Wrote  ", nrow(VF), " rows in `vframe` encapsulating ", length(unique(VF$wkl_uuid)), " critical layers: "))
    message(paste0(unique(WklValidation$pwl), collapse = "\n"))
  }
  if (!is.na(savePNG_prefix)) {
    message(paste0("Plotted ", length(list.files(savePNG_prefix)), " png files in ", savePNG_prefix))
  }

  return(WklValidation)

}
