## This script contains two functions for plotting avalanche problems in a way that is unique to this paper presentation

#' Plot avalanche problem likelihood of specific avalanche problem types
#'
#' This function is used to plot the avalanche problem likelihood of storm and persistent problems
#' in a barchart-way that looks similar to the barchart of proportion_unstable plots (e.g., Fig. 14c in paper).
#'
#' Instead of plotting only these three problem types, the function can be customized to plot any problem type of interest.
#' Since it plots one time series, it can only plot one problem type per day. If there are more problems issued at that
#' day, the argument `ProbOrder` defines which one is plotted.
#'
#' @param Bull Bulletin object of the desired region/season
#' @param ElevBand elevation band of interest (must be contained in `Bull`)
#' @param ProbOrder Which problem types to plot and if there are multiple at any given day, which one should be plotted with higher priority?
#' @param withXAxis plot x axis
#' @param withYAxis plot y axis
#' @param xticks_Sat xticks on each Saturday?
#' @param plotBars plot barchart? if not, points and recatangles are plotted for typ/min/max values.
#'
#' @export
plotTSAvProbLikelihood <- function(Bull, ElevBand = "Tl", ProbOrder = c("Persistent Slabs", "Deep Persistent Slabs", "Storm Slabs"),
                                   withXAxis = TRUE, withYAxis = TRUE, xticks_Sat = TRUE, plotBars = FALSE, ...) {

  ## get AvProb data frame with one row per day only,
  ## choose av probs according to ProbOrder
  PF <- merge(Bull$AvProblems[[ElevBand]], Bull$Bulletins)
  PF <- PF[PF$CHARACTER %in% ProbOrder, ]
  xdate <- zoo::as.Date(PF$PUBLISH_DATE)
  if (any(duplicated(xdate))) {
    drop <- unlist(lapply(xdate[duplicated(xdate)], function(dt) {
      dupls <- which(zoo::as.Date(PF$PUBLISH_DATE) == dt)
      dupls[-which.min(factor(PF$CHARACTER[dupls], levels = ProbOrder))]
    }))
    PF <- PF[-drop, ]
  }

  xdate <- zoo::as.Date(PF$PUBLISH_DATE)


  if (plotBars) {
    plot(xdate, rep(NA, length(xdate)), type = "n", xaxt = "n", yaxt = "n", ylim = c(0, 8), ylab = "", xlab = "", ...)
    rect(xdate-0.25, PF$LIKELIHOOD_MIN,
         xdate+0.25, PF$LIKELIHOOD_MAX, col = SarpGeneral::getAvCharColor(PF$CHARACTER, Transparency = "60"), border = NA)
    # points(xdate, PF$LIKELIHOOD_TYP, col = SarpGeneral::getAvCharColor(PF$CHARACTER), pch = 19, cex = par()$cex + 0.5)
    rect(xdate-0.5, 0,
         xdate+0.5, PF$LIKELIHOOD_TYP, col = SarpGeneral::getAvCharColor(PF$CHARACTER, Transparency = "99"), border = NA)
  } else {
    plot(xdate, rep(NA, length(xdate)), type = "n", xaxt = "n", yaxt = "n", ylim = c(0, 8), ylab = "", xlab = "", ...)
    rect(xdate-0.25, PF$LIKELIHOOD_MIN,
         xdate+0.25, PF$LIKELIHOOD_MAX, col = SarpGeneral::getAvCharColor(PF$CHARACTER, Transparency = "50"), border = NA)
    points(xdate, PF$LIKELIHOOD_TYP, col = SarpGeneral::getAvCharColor(PF$CHARACTER), pch = 19, cex = par()$cex + 0.5)
  }

  xdate <- seq(min(xdate), to = max(xdate), by = 1)
  Saturdays <- xdate[weekdays(xdate, abbreviate = FALSE) == "Saturday"]
  SaturdayLabels <- format(xdate[weekdays(xdate, abbreviate = FALSE) == "Saturday"], "%b %d")
  if (xticks_Sat) {
    abline(v = Saturdays, lty = 3, col = "gray70")
  }
  if (withXAxis) {
    axis(1, at = Saturdays, labels = SaturdayLabels, las = 2)
  }
  ## YAxis
  if (withYAxis) axis(2, at = c(0, 2, 4, 6, 8), labels = c("Unlikely", "Possible", "Likely", "Very likely", "Almost certain"), las = 2)

}


#' Shade period with specific avalanche problem types in their problem color
#'
#' This function is the equivalent to [plotTSAvProbLikelihood], shading periods with specific problems in their native color.
#'
#' @export
shadeAvProbPeriod <- function(Bull, ElevBand = "Tl", ProbOrder = c("Persistent Slabs", "Deep Persistent Slabs", "Storm Slabs")) {

  ## get AvProb data frame with one row per day only,
  ## choose av probs according to ProbOrder
  PF <- merge(Bull$AvProblems[[ElevBand]], Bull$Bulletins)
  PF <- PF[PF$CHARACTER %in% ProbOrder, ]
  xdate <- zoo::as.Date(PF$PUBLISH_DATE)
  if (any(duplicated(xdate))) {
    drop <- unlist(lapply(xdate[duplicated(xdate)], function(dt) {
      dupls <- which(zoo::as.Date(PF$PUBLISH_DATE) == dt)
      dupls[-which.min(factor(PF$CHARACTER[dupls], levels = ProbOrder))]
    }))
    PF <- PF[-drop, ]
  }

  xdate <- zoo::as.Date(PF$PUBLISH_DATE)

  ## draw av problem period
  date_dps <- unique(xdate[PF$CHARACTER %in% c("Deep Persistent Slabs")])
  date_ps <- unique(xdate[PF$CHARACTER %in% c("Persistent Slabs")])
  date_st <- unique(xdate[PF$CHARACTER %in% c("Storm Slabs")])
  if (length(date_dps) > 0) rect(date_dps-0.5, -1000, date_dps+0.5, 1000, col=SarpGeneral::getAvCharColor("Deep Persistent Slabs", Transparency = '10'), border=NA)
  if (length(date_ps) > 0) rect(date_ps-0.5, -1000, date_ps+0.5, 1000, col=SarpGeneral::getAvCharColor("Persistent Slabs", Transparency = '10'), border=NA)
  if (length(date_st) > 0) rect(date_st-0.5, -1000, date_st+0.5, 1000, col=SarpGeneral::getAvCharColor("Storm Slabs", Transparency = '20'), border=NA)

}





