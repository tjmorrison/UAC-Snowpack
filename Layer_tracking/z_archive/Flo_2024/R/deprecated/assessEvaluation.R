#' @export
plotCaptWKLIndicators <- function(evframe, indicator, WKL = NULL, sortBy = "dq", xticklabels = TRUE, cex.points = 1.5, vioplot = FALSE) {

  ## assemble additional indicators:
  if (indicator == "offset_totaltime") {
    evframe$offset_totaltime <- evframe$nPDays_20 - evframe$nPDays
  }

  ## manipulate
  evframe[, indicator] <- as.double(evframe[, indicator])
  evframe$wkldate <- as.Date(evframe$wkldate)
  ## include dataquality
  if (!all(is.null(WKL))) evframe$dq <- sapply(evframe$wkl_uuid, function(wid) WKL$dq[WKL$wkl_uuid == wid])
  if (!"dq" %in% names(evframe)) stop("dq missing in evframe! Provide WKL data.frame or include dq manually in evframe!")
  ## filter if only few assessment days
  evframe <- evframe[as.double(evframe$nPDays) > 5, ]
  ## gtypecolor
  evframe$gtypecolor <- getColoursGrainType("DH")
  evframe$gtypecolor[evframe$gtype_rank == "tertiary"] <- getColoursGrainType("MFcr")
  evframe$colAlpha <- 1
  evframe$colAlpha[evframe$gtype_rank == "primary"] <- 0.5
  evframe$colAlpha[evframe$gtype_rank == "tertiary" & !as.logical(evframe$wkl_iscrust)] <- 0.5
  evframe$gtypecolor <- sapply(seq_along(evframe$gtypecolor), function(i) adjustcolor(evframe$gtypecolor[i], alpha.f = evframe$colAlpha[i]))
  ## max indicator
  evframe$max_indicator <- sapply(evframe$wkl_uuid, function(wid) max(evframe[, indicator][evframe$wkl_uuid == wid], na.rm = TRUE))

  ## set negatives to 0
  # if (indicator %in% c("rho_llhd", "rho_dist", "rho_sens")) {
  #   evframe[, indicator][evframe[, indicator] < 0]  <- -0.05
  # }


  if (!is.null(sortBy)) {
    if (sortBy == "wkldate") {
      k <- order(evframe$wkldate)
      evframe <- evframe[k, ]
    } else if (sortBy == "dq") {
      k <- order(evframe$dq, 1-evframe$max_indicator)
      if (substr(indicator, 1, 3) == "lag" | substr(indicator, 1, 6) == "offset") k <- order(evframe$dq, evframe$max_indicator)
      evframe <- evframe[k, ]
    }
  }

  if (!vioplot) {
    ## Bubble-plot
    xx <- as.double(factor(as.character(evframe$wkldate), levels = unique(as.character(evframe$wkldate))))
    ylim <- c(-1, 1)
    if (substr(indicator, 1, 3) == "lag" | substr(indicator, 1, 6) == "offset") ylim  <- c(min(evframe[, indicator], na.rm = TRUE), max(evframe[, indicator], na.rm = TRUE))
    plot(xx, evframe[, indicator],
         col = evframe$gtypecolor, pch = 16, ylim = ylim, axes = FALSE, xlab = "", ylab = indicator, cex = cex.points)
    abline(v = unique(xx), lty = "dotted", col = "gray")
    abline(v = xx[which(diff(evframe$dq) > 0.3)]+0.5)
    if (xticklabels) {
      axis(1, unique(xx), unique(evframe$wkldate), las = 2)
    } else {
      axis(1, unique(xx), rep("", length(unique(xx))))
    }
    if (substr(indicator, 1, 3) == "lag" | substr(indicator, 1, 6) == "offset") {
      abline(h = 0, lty = "dashed", col = "gray")
      axis(2, pretty(ylim))
    } else {
      abline(h = seq(0.2, 0.8, by = 0.2), lty = "dashed", col = "gray")
      axis(2, seq(0, 1, by = 0.2))
    }
  } else {
    ## Vioplot
    vioplot::vioplot(evframe[, indicator])
    abline(h = seq(-1, 1, by = 0.25), lty = "dashed", col = "gray")
  }



  return(unique(evframe$wkldate))
}
