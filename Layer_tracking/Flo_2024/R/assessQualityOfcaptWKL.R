#' Assess how well model captured PWLs
#'
#' This function calculates several indicators that help to assess how well the model captured captWKLs beyond simple layer structure.
#' It get's called by the overarching evaluation script `evaluate_main.R`
#'
#' So, for each WKL the function computes
#'
#' * correlation factors for likelihood/distribution/sensitvity versus proportion of unstable grid points
#' * the maximum proportion of unstable grid points during the lifetime of the WKL
#' * the temporal lag between first forecaster concern (--> avy problem) and first time that grid points are unstable in that layer
#'     - different timing instances possible: first time (i) at least one grid point unstable, (ii) the majority of grid points is unstable, (iii) more than 50 % of max proportion unstable
#' * the temporal lag between latest forecaster concern (--> last day of avy problem) and last time that grid points are unstable in that layer
#'     - need to find rules for becoming dormant and waking up again versus becoming inactive
#'
#' @param VData object; don't include both primary/secondary and tertiary gtype_ranks!
#'
#' @export
assessQualityOfcaptWKL <- function(VData, band, stabilityindex = "p_unstable") {

  gtype_rank <- unique(VData$vframe$gtype_rank)
  if (any(c("primary", "secondary") %in% gtype_rank) & "tertiary" %in% gtype_rank) stop("You can only provide either primary and/or secondary, or tertiary gtype_ranks in your VData object to this function!")
  possible_ranks <- c("tertiary", "secondary", "primary")
  gtype_rank <- possible_ranks[possible_ranks %in% gtype_rank][1]

  nrow_max <- length(VData$wkl$wkl_uuid)
  OUTnames <- c(
    "wkl_uuid", "wkl_iscrust", "wkldate", "wkltag",
    "nPDays", "nPDays_anyunstable", "nPDays_median", "nPDays_halfofmax", "nPDays_20",
    "rho_llhd", "rho_dist", "rho_sens", "p_llhd", "p_dist", "p_sens",
    "pu_max", "offset_maxes", "offset_mean",
    "pcapt_max",
    "lagA_anyunstable", "lagA_median", "lagA_halfofmax", "lagA_20", "lagZ_anyunstable", "lagZ_median", "lagZ_halfofmax", "lagZ_20",
    "band", "stabilityindex", "gtype_rank"
  )
  OUT <- matrix(nrow = max(1, nrow_max), ncol = length(OUTnames), dimnames = list(seq(max(1, nrow_max)), OUTnames))

  for (i in seq_along(VData$wkl$wkl_uuid)) {
    wuid <- VData$wkl$wkl_uuid[i]
    wkldate <- as.character(as.Date(VData$wkl$datetag[i]))
    wkltag <- paste(format(as.Date(VData$wkl$datetag[i]), "%b %d"), substr(gtype_rank, start = 1, stop = 4))
    wkl_iscrust <- as.logical(VData$wkl$iscrust[i])

    ## extract relevant rows of tables vframe & captWKL
    VF <- VData$vframe[VData$vframe$wkl_uuid == wuid, ]
    VF <- VF[order(VF$vdate), ]
    # CAPT <- VData$captWKL[VData$captWKL$wkl_uuid == wuid & VData$captWKL$pu >= 0.77, ]

    ## compute Spearman's rank correlation rho
    likelihood <- VF$llhd_rep
    distribution <- as.numeric(VF$dist_rep)
    sensitivity <- as.numeric(VF$sens_rep)
    proportion_unstable <- as.numeric(VF$proportion_unstable)
    ## extract maximum proportion_unstable
    proportion_unstable_MAX <- max(proportion_unstable, na.rm = TRUE)
    ## extract maximum proportion_captured
    proportion_captured_MAX <- max(as.numeric(VF$dist), na.rm = TRUE)

    rho_llhd <- tryCatch({suppressWarnings(cor.test(likelihood, proportion_unstable, method = "spearman"))}, error = function(err) list(estimate = NA, p.value = NA))
    rho_dist <- tryCatch({suppressWarnings(cor.test(distribution, proportion_unstable, method = "spearman"))}, error = function(err) list(estimate = NA, p.value = NA))
    rho_sens <- tryCatch({suppressWarnings(cor.test(sensitivity, proportion_unstable, method = "spearman"))}, error = function(err) list(estimate = NA, p.value = NA))

    ## number of problem days assessed:
    nPDays <- length(likelihood[!is.na(likelihood)])
    ## simulated:
    nPDays_anyunstable <- length(unique(VF$vdate[proportion_unstable >= 0.05]))
    nPDays_median <- length(unique(VF$vdate[proportion_unstable >= 0.5]))
    nPDays_halfofmax <- length(unique(VF$vdate[proportion_unstable >= 0.5*proportion_unstable_MAX]))
    nPDays_20 <- length(unique(VF$vdate[proportion_unstable >= 0.2]))



    ## extract offset of max likelihood -- max proportion unstable
    offset_maxes <- proportion_unstable_MAX - (max(likelihood, na.rm = TRUE)/9)
    ## extract mean offset of likelihood -- proportion unstable
    offset_mean <- mean(proportion_unstable - (likelihood/9), na.rm = TRUE)

    ## compute temporal lags
    ## compute temporal lag -- first identification
    DTfirst <- data.frame(
      problem = as.Date(VF$vdate[!is.na(VF$tmode)][1]),
      anyunstable = as.Date(VF$vdate[proportion_unstable > 0.05][1]),
      median = as.Date(VF$vdate[proportion_unstable >= 0.5][1]),
      halfofmax = as.Date(VF$vdate[proportion_unstable >= 0.5*proportion_unstable_MAX][1]),
      twenty = as.Date(VF$vdate[proportion_unstable >= 0.2][1])
    )
    LAGfirst <- data.frame(
      anyunstable = as.numeric(DTfirst$anyunstable - DTfirst$problem),
      median = as.numeric(DTfirst$median - DTfirst$problem),
      halfofmax = as.numeric(DTfirst$halfofmax - DTfirst$problem),
      twenty = as.numeric(DTfirst$twenty - DTfirst$problem)
    )
    ## compute temporal lag -- last identification
    DTlast <- data.frame(
      problem = as.Date(rev(VF$vdate[!is.na(VF$tmode)])[1]),
      anyunstable = as.Date(rev(VF$vdate[proportion_unstable > 0.05])[1]),
      median = as.Date(rev(VF$vdate[proportion_unstable >= 0.5])[1]),
      halfofmax = as.Date(rev(VF$vdate[proportion_unstable >= 0.5*proportion_unstable_MAX])[1]),
      twenty = as.Date(rev(VF$vdate[proportion_unstable >= 0.2])[1])
    )
    LAGlast <- data.frame(
      anyunstable = as.numeric(DTlast$anyunstable - DTlast$problem),
      median = as.numeric(DTlast$median - DTlast$problem),
      halfofmax = as.numeric(DTlast$halfofmax - DTlast$problem),
      twenty = as.numeric(DTlast$twenty - DTlast$problem)
    )

    ## write results into maxtrix
    OUT[i, ] <- c(wuid, wkl_iscrust, wkldate, wkltag,
                  nPDays, nPDays_anyunstable, nPDays_median, nPDays_halfofmax, nPDays_20,
                  rho_llhd$estimate, rho_dist$estimate, rho_sens$estimate, rho_llhd$p.value, rho_dist$p.value, rho_sens$p.value,
                  proportion_unstable_MAX, offset_maxes, offset_mean,
                  proportion_captured_MAX,

                  LAGfirst$anyunstable, LAGfirst$median, LAGfirst$halfofmax, LAGfirst$twenty, LAGlast$anyunstable, LAGlast$median, LAGlast$halfofmax, LAGlast$twenty, NA, NA, NA)

    }  # END FOR LOOP along wkl_uuid

  OUT <- as.data.frame(OUT)
  OUT$wkldate <- as.Date(OUT$wkldate)
  OUT$wkl_iscrust <- as.logical(OUT$wkl_iscrust)
  OUT$band <- band
  OUT$stabilityindex = stabilityindex
  OUT$gtype_rank = gtype_rank

  OUT <- OUT[order(OUT$wkldate), ]

  ## assign correct classes
  for (column in c("wkl_iscrust",
                   "nPDays", "nPDays_anyunstable", "nPDays_median", "nPDays_halfofmax", "nPDays_20",
                   "rho_llhd", "rho_dist", "rho_sens", "p_llhd", "p_dist", "p_sens",
                   "pu_max", "offset_maxes", "offset_mean",
                   "pcapt_max",
                   "lagA_anyunstable", "lagA_median", "lagA_halfofmax", "lagA_20", "lagZ_anyunstable", "lagZ_median", "lagZ_halfofmax", "lagZ_20")) {
    OUT[, column] <- try({as.double(OUT[, column])})
  }

  return(OUT)
}

