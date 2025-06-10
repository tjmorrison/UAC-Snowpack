## This file includes useful plot functions to viz the data from the wklvalidation processingDB
## Not all plots were super useful, but I keep them anyways

## --- Detailed timeseries of individual WKL and relevant parameters----
#' Time series of WKL
#' @export
plotTSWKL <- function(vdat, wkl_uuid, plotIndices = ifelse(length(wkl_uuid) == 1, TRUE, FALSE), cex = 1) {

  ## Initialization
  ## subfunction for getting stability indices
  extractIndex <- function(captWKL, wkl_uuid, vf_uuid, vdates, index, aggfct) {
    captWKL <- captWKL[captWKL$wkl_uuid %in% wkl_uuid, ]
    vfuuidlist <- lapply(vdates, function(dt) vf_uuid[vf_uuid %in% captWKL$vf_uuid[captWKL$vdate %in% dt]])
    lapply(vfuuidlist, function(vid) {
      sapply(unique(captWKL$vstation_id[captWKL$vf_uuid %in% vid]), function(sid) {
        aggfct(captWKL[captWKL$vf_uuid %in% vid & captWKL$vstation_id %in% sid, index])
      })
    })
  }  ## END subfunction
  opar <- par(cex = cex)
  on.exit(par(opar))

  grainCols <- rbind(grainDict, data.frame(gtype = c("IFrc", "IFsc"), colour = grainDict$colour[grainDict$gtype == "IF"]))
  # if (is.na(wkl_uuid)) wkl_uuid <- unique(vdat$vframe$wkl_uuid)
  if (!all(wkl_uuid %in% unique(vdat$vframe$wkl_uuid))) stop("Not all wkl_uuids contained in vdat!")
  vf <- vdat$vframe[vdat$vframe$wkl_uuid %in% wkl_uuid, ]
  xrange <- c(min(vf$age[vf$wkl_uuid %in% wkl_uuid])-0.5, max(vf$age[vf$wkl_uuid %in% wkl_uuid])+1)
  xdate <- sort(unique(vf$vdate))
  xage <- sort(unique(vf$age))
  xweekdays <- weekdays(as.Date(xdate), abbreviate = FALSE)
  weekday0 <- xweekdays[xage == 0]
  xlabelsdate <- format(as.Date(xdate)[xweekdays == weekday0], "%b %d")
  xlabelsage <- xage[xweekdays == weekday0]

  ## Setup figure
  title <- ifelse(length(wkl_uuid) > 1,
                  paste0(length(wkl_uuid), " WKLs from ", paste0(unique(min(vdat$wkl$season[vdat$wkl$wkl_uuid == wkl_uuid]), max(vdat$wkl$season[vdat$wkl$wkl_uuid == wkl_uuid])), collapse = "--")),
                  paste0(vdat$wkl$wkl_name[vdat$wkl$wkl_uuid == wkl_uuid]))
  if (plotIndices) {
    layout(matrix(seq(7), ncol = 1), heights = c(1, 2, 1, 1, 1, 1, 1))
    omar <- par(mar = c(0.5, 4.1, 0.5, 2.1))
    on.exit(par(omar))
    if (length(wkl_uuid == 1)) {
      ## draw reported dist and sensitivity
      upage <- sort(unique(vf$age[!is.na(vf$tmode)]))
      dist_rep <- as.numeric(sapply(upage, function(da) unique(vf$dist_rep[vf$wkl_uuid == wkl_uuid & vf$age == da])))
      dist_palette <- colorRampPalette(c("#a6cee3", "#1f78b4"))
      dist_cut <- cut(dist_rep, breaks = c(0.5, 1.6, 2.4, 3))
      dist_cols <- dist_palette(3)[as.numeric(dist_cut)]
      sens_rep <- as.numeric(sapply(upage, function(da) unique(vf$sens_rep[vf$wkl_uuid == wkl_uuid & vf$age == da])))
      sens_palette <- colorRampPalette(c("#f4cae4", "#984ea3"))
      sens_cut <- cut(sens_rep, breaks = c(0.5, 1.9, 2.5, 3.1, 4))
      sens_cols <- sens_palette(4)[as.numeric(sens_cut)]
      plot(upage+0.15, dist_rep + 0.5, type = "p", pch = 15, cex = 1.4*cex, xlim = xrange, ylim = c(1, 4), xaxt = "n", xlab = "", yaxt = "n", ylab = "Dist & Sens", col = dist_cols, xaxs = "i", cex.axis = cex, cex.lab = cex)
      abline(v = xlabelsage, lty = 3, col = "gray70")
      points(upage-0.15, sens_rep, type = "p", pch = 18, cex = 1.8*cex, col = sens_cols)
      axis(2, at = c(1.5, 2.5, 3.5), labels = c("Iso", "Spc", "Wdsp"), cex.axis = cex)
      segments(-10, c(1.5, 2.5, 3.5), max(upage), c(1.5, 2.5, 3.5), lty = "dotted", col = dist_palette(3))
      axis(4, at = c(1, 2, 3, 4), labels = c("Unr", "Stb", "Reac", "Tchy"), cex.axis = cex)
      segments(min(upage), seq(4), 1000, seq(4), lty = "dotted", col = sens_palette(4))

      text(xrange[2]-0.3, 3.49, adj = 1, labels = paste0(paste0(unique(vdat$executions$band), collapse = ", "),
                                                    "\nWKL data quality: ", vdat$wkl$dq[vdat$wkl$wkl_uuid == wkl_uuid],
                                                    "\n", vdat$wkl$comment[vdat$wkl$wkl_uuid == wkl_uuid]), cex = cex)
    }
  }

  par(mar = c(0.5, 4.1, 1.5, 2.1))
  plot(xrange, c(0, 1.05), type = 'n', ylab = "Distribution across grid cells", xlab = "", main = title, xaxt = "n", xaxs = "i", cex.axis = cex, cex.lab = cex, cex = cex, cex.main = 1.3*cex)
  abline(v = xlabelsage, lty = 3, col = "gray70")
  ## plot individual WKLs and grain classes
  for (i in seq(length(wkl_uuid))) {
    gclasses <- unique(vf$gtype_class[vf$wkl_uuid == wkl_uuid[[i]]])
    if (length(wkl_uuid) == 1) {
      ## draw av problem period
      upage <- sort(unique(vf$age[!is.na(vf$tmode)]))
      ## other wkl_uuids:
      upage_other <- vdat$vframe$age[!vdat$vframe$wkl_uuid %in% wkl_uuid & !is.na(vdat$vframe$tmode)]
      upage_other_tmode <- vdat$vframe$tmode[!vdat$vframe$wkl_uuid %in% wkl_uuid & !is.na(vdat$vframe$tmode)]
      if (isTRUE(any(upage_other_tmode == "Storm Slabs"))) rect(upage_other[upage_other_tmode == "Storm Slabs"]-0.5, 1, upage_other[upage_other_tmode == "Storm Slabs"]+0.5, 1.09, col=adjustcolor("gray95", alpha.f = 0.99), border=NA)
      if (isTRUE(any(upage_other_tmode == "Persistent Slabs"))) rect(upage_other[upage_other_tmode == "Persistent Slabs"]-0.5, 1, upage_other[upage_other_tmode == "Persistent Slabs"]+0.5, 1.09, col=adjustcolor("gray90", alpha.f = 0.99), border=NA)
      ## from current wkl_uuid:
      if (isTRUE(any(vf$tmode == "Storm Slabs"))) rect(vf$age[vf$tmode == "Storm Slabs"]-0.5, -1000, vf$age[vf$tmode == "Storm Slabs"]+0.5, 1, col=SarpGeneral::getAvCharColor("Storm Slabs", Transparency = '10'), border=NA)
      if (isTRUE(any(vf$tmode == "Persistent Slabs"))) rect(vf$age[vf$tmode == "Persistent Slabs"]-0.5, -1000, vf$age[vf$tmode == "Persistent Slabs"]+0.5, 1, col=SarpGeneral::getAvCharColor("Persistent Slabs", Transparency = '20'), border=NA)
      if (isTRUE(any(vf$tmode == "Deep Persistent Slabs"))) rect(vf$age[vf$tmode == "Deep Persistent Slabs"]-0.5, -1000, vf$age[vf$tmode == "Deep Persistent Slabs"]+0.5, 1, col=SarpGeneral::getAvCharColor("Deep Persistent Slabs", Transparency = '20'), border=NA)
    }
    ## draw actual WKL distribution timeseries
    for (j in seq(length(gclasses))) {
      entity <- vf$wkl_uuid == wkl_uuid[[i]] & vf$gtype_class == gclasses[j]
      k <- order(vf$age[entity])
      lines(vf$age[entity][k], vf$dist[entity][k], type = "b", pch = 19, col = grainCols$colour[grainCols$gtype == gclasses[j]], cex = cex)
    }
    if (plotIndices) {
      ## compute dist_max along xage
      dist_max <- sapply(xage, function(ag) max(vf$dist[vf$age == ag]))
      ## compute SLAB WKL depth
      dpth <- extractIndex(vdat$captWKL, wkl_uuid, vf$vf_uuid, xdate, "depth", max)
      dpth <- lapply(dpth, function(ve) -1*as.numeric(ve))
      names(dpth) <- xage
      if (!any(sapply(dpth, is.numeric))) {
        warning("Not a single layer captured, not drawing any boxplots..")
        return()
      }
      ## compute pu and its distribution among captWKL
      pu <- extractIndex(vdat$captWKL, wkl_uuid, vf$vf_uuid, xdate, "pu", max)
      names(pu) <- xage
      dist_pu <- sapply(seq(length(pu)), function(l) {
        sum(pu[[l]] >= 0.77)/max(vf$nSP[vf$age == xage[l]])
      })
      dist_pu[is.na(dist_pu)] <- 0
      lines(names(pu), dist_pu, type = "b", pch = 19, col = rgb(0, 0, 0, 0.5), cex = 0.7*cex)
      ## compute pu and its distribution among poorWKL
      dist_pu_poorWKL <- sapply(xdate, function(dt) {
        length(unique(vdat$poorWKL$vstation_id[vdat$poorWKL$vdate == dt]))/max(vf$nSP[vf$vdate == dt])
      })
      lines(xage, dist_pu_poorWKL, type = "l", col = rgb(0, 0, 0, 0.5), lty = "dashed")
    }
  }  # END LOOP WKLs

  ## get stability indices and reported quantities
  if (plotIndices) {
    ## draw boxplots
    box_alpha <- 0.3
    both_gclasses <- vf$wkl_uuid %in% wkl_uuid
    ## SLAB WKL depth
    par(mar = c(0.5, 4.1, 0.5, 2.1))
    dpth_poor <- lapply(xdate, function(dt) -vdat$poorWKL$depth[vdat$poorWKL$vdate == dt])
    gcol_poor <- lapply(xdate, function(dt) getColoursGrainType(vdat$poorWKL$gtype[vdat$poorWKL$vdate == dt]))
    gcol_poor <- lapply(gcol_poor, function(ve) if (length(ve) > 0) adjustcolor(ve, alpha.f = 0.6))
    # beeswarm::bxplot(dpth[sapply(dpth, is.numeric)], at = as.numeric(names(dpth)[sapply(dpth, is.numeric)]), col = "gray", ylab = "layer depth (cm)", xlim = xrange, xaxt = "n")
    beeswarm::beeswarm(dpth_poor[sapply(dpth_poor, function(el) length(el) > 0)], at = xage[sapply(dpth_poor, function(el) length(el) > 0)], method = "center", corral = "wrap", pwcol = gcol_poor[sapply(dpth_poor, function(el) length(el) > 0)], pch = 19, cex = 0.5, ylab = "layer depth (cm)", xlim = xrange, xaxt = "n", xaxs = "i", cex.axis = cex, cex.lab = cex)
    vioplot::vioplot(dpth[sapply(dpth, function(dv) length(dv) > 0 & is.numeric(dv))], at = as.numeric(names(dpth)[sapply(dpth, function(dv) length(dv) > 0 & is.numeric(dv))]), col = rgb(0, 0, 0, box_alpha), wex = dist_max[sapply(dpth, function(dv) length(dv) > 0 & is.numeric(dv))], lwd = 0.5, add = TRUE)
    # beeswarm::beeswarm(dpth[sapply(dpth, is.numeric)], at = as.numeric(names(dpth)[sapply(dpth, is.numeric)]), method = "center", corral = "wrap", col = rgb(0, 0, 0, box_alpha), pch = 19, cex = 0.5, add = TRUE)
    abline(v = xlabelsage, lty = 3, col = "gray70")
    ## rhogs
    par(mar = c(0.5, 4.1, 0.5, 2.1))
    lvar <- extractIndex(vdat$captWKL, wkl_uuid, vf$vf_uuid, sort(unique(vf$vdate)), "sl_rhogs", max)
    names(lvar) <- sort(unique(vf$age))
    # beeswarm::bxplot(lvar[sapply(lvar, is.numeric)], at = as.numeric(names(lvar)[sapply(lvar, is.numeric)]), col = "gray", ylab = "<density/grain size>_slab", xlim = xrange, xaxt = "n")
    # beeswarm::beeswarm(lvar[sapply(lvar, is.numeric)], at = as.numeric(names(lvar)[sapply(lvar, is.numeric)]), method = "center", corral = "wrap", col = rgb(0, 0, 0, box_alpha), pch = 19, cex = 0.5, add = TRUE)
    vioplot::vioplot(lvar[sapply(lvar, function(dv) length(dv) > 0 & !all(is.na(dv)))], at = as.numeric(names(lvar)[sapply(lvar, function(dv) length(dv) > 0 & !all(is.na(dv)))]), col = rgb(0, 0, 0, box_alpha), wex = dist_max[sapply(lvar, function(dv) length(dv) > 0 & !all(is.na(dv)))], lwd = 0.5, xlim = xrange, xaxt = "n", xaxs = "i", cex.axis = cex, cex.lab = cex)
    abline(v = xlabelsage, lty = 3, col = "gray70")
    mtext("<rho/gs>_slab", side = 2, line = 3, cex = 0.7*cex)
    ## PU
    par(mar = c(0.5, 4.1, 0.5, 2.1))
    pu_poor <- lapply(xdate, function(dt) vdat$poorWKL$pu[vdat$poorWKL$vdate == dt])
    # lvar <- extractIndex(vdat$captWKL, wkl_uuid, vf$vf_uuid, sort(unique(vf$vdate)), "pu", max)
    # names(lvar) <- sort(unique(vf$age))
    # beeswarm::bxplot(pu[sapply(pu, is.numeric)], at = as.numeric(names(pu)[sapply(pu, is.numeric)]), col = "gray", ylab = "p_unstable", xlim = xrange, ylim = c(0, 1), yaxt = "n", xaxt = "n")
    beeswarm::beeswarm(pu_poor[sapply(pu_poor, function(el) length(el) > 0)], at = xage[sapply(pu_poor, function(el) length(el) > 0)], method = "center", corral = "wrap", pwcol = gcol_poor[sapply(pu_poor, function(el) length(el) > 0)], pch = 19, cex = 0.5, ylab = "p_unstable", xlim = xrange, ylim = c(0, 1), yaxt = "n", xaxt = "n", xaxs = "i", cex.axis = cex, cex.lab = cex)
    abline(h = 0.77, col = "black", lty = "dotted")
    vioplot::vioplot(pu[sapply(pu, function(dv) length(dv) > 0 & !all(is.na(dv)))], at = as.numeric(names(pu)[sapply(pu, function(dv) length(dv) > 0 & !all(is.na(dv)))]), col = rgb(0, 0, 0, box_alpha), wex = dist_max[sapply(pu, function(dv) length(dv) > 0 & !all(is.na(dv)))], lwd = 0.5, add = TRUE)
    # beeswarm::beeswarm(pu[sapply(pu, is.numeric)], at = as.numeric(names(pu)[sapply(pu, is.numeric)]), method = "center", corral = "wrap", col = rgb(0, 0, 0, box_alpha), pch = 19, cex = 0.5, add = TRUE)
    axis(2, at = c(0, 0.5, 1), labels = TRUE, cex.axis = cex, cex.lab = cex)
    abline(v = xlabelsage, lty = 3, col = "gray70")
    ## SK38
    par(mar = c(0.5, 4.1, 0.5, 2.1))
    lvar <- extractIndex(vdat$captWKL, wkl_uuid, vf$vf_uuid, sort(unique(vf$vdate)), "sk38", max)
    names(lvar) <- sort(unique(vf$age))
    vioplot::vioplot(lvar[sapply(lvar, function(dv) length(dv) > 0 & !all(is.na(dv)))], at = as.numeric(names(pu)[sapply(lvar, function(dv) length(dv) > 0 & !all(is.na(dv)))]), col = rgb(0, 0, 0, box_alpha), wex = dist_max[sapply(lvar, function(dv) length(dv) > 0 & !all(is.na(dv)))], lwd = 0.5, xlim = xrange, xaxt = "n", xaxs = "i", cex.axis = cex, cex.lab = cex)
    abline(h = 1.2, col = "black", lty = "dotted")
    abline(v = xlabelsage, lty = 3, col = "gray70")
    mtext("SK38", side = 2, line = 3, cex = 0.7*cex)
    ## RTA
    par(mar = c(5.1, 4.1, 0.5, 2.1))
    lvar <- extractIndex(vdat$captWKL, wkl_uuid, vf$vf_uuid, sort(unique(vf$vdate)), "rta", max)
    names(lvar) <- sort(unique(vf$age))
    # beeswarm::bxplot(lvar[sapply(lvar, is.numeric)], at = as.numeric(names(lvar)[sapply(lvar, is.numeric)]), col = "gray", ylab = "RTA", xlim = xrange, ylim = c(0, 1), yaxt = "n", xaxt = "n", xlab = "Days since burial")
    vioplot::vioplot(lvar[sapply(lvar, function(dv) length(dv) > 0 & !all(is.na(dv)))], at = as.numeric(names(pu)[sapply(lvar, function(dv) length(dv) > 0 & !all(is.na(dv)))]), col = rgb(0, 0, 0, box_alpha), wex = dist_max[sapply(lvar, function(dv) length(dv) > 0 & !all(is.na(dv)))], lwd = 0.5, xlim = xrange, ylim = c(0, 1), yaxt = "n", xaxt = "n", xaxs = "i")
    abline(h = 0.8, col = "black", lty = "dotted")
    # beeswarm::beeswarm(lvar[sapply(lvar, is.numeric)], at = as.numeric(names(lvar)[sapply(lvar, is.numeric)]), method = "center", corral = "wrap", col = rgb(0, 0, 0, box_alpha), pch = 19, cex = 0.5, add = TRUE)
    axis(2, at = c(0, 0.5, 1), labels = TRUE, cex.axis = cex, cex.lab = cex)
    axis(1, at = xlabelsage, cex.axis = cex, cex.lab = cex)
    abline(v = xlabelsage, lty = 3, col = "gray70")
    mtext("RTA", side = 2, line = 3, cex = 0.7*cex)
    mtext(xlabelsdate, side = 1, at = xlabelsage, las = 1, line = 2, cex = 0.7*cex)
    mtext("Days since burial", side = 1, at = xage[length(xage)/2], las = 1, line = 4, cex = cex)
  }
}

## --- beeswarm timeseries (WKL age) against distribution among grid cells ----
#' Time series of summarized/aggregated WKLs
#' @export
plotTSaggregatedWKL <- function(vdat, wkl_uuid = NA, gclasses = "all", xage = "all", ...){

  ## Init
  grainCols <- rbind(grainDict, data.frame(gtype = c("IFrc", "IFsc"), colour = grainDict$colour[grainDict$gtype == "IF"]))
  if (is.na(wkl_uuid)) wkl_uuid <- unique(vdat$vframe$wkl_uuid)
  if (!all(wkl_uuid %in% unique(vdat$vframe$wkl_uuid))) stop("Not all wkl_uuids contained in vdat!")
  if (gclasses == "all") gclasses <- c("SH", "FC", "MFcr", "IFsc", "IFrc")
  vf <- vdat$vframe[vdat$vframe$wkl_uuid %in% wkl_uuid & vdat$vframe$gtype_class %in% gclasses, ]
  if (all(xage == "all")) xage <- sort(unique(vf$age))
  xrange <- c(min(xage), max(xage))

  ## Layout
  nPanels <- sum(c("Persistent Slabs", "Deep Persistent Slabs") %in% unique(vf$tmode)) + 1
  omfrow <- par()['mfrow']
  on.exit(par(omfrow))
  layout(matrix(seq(nPanels), nrow = 1), widths = c(10, rep(1, nPanels-1)))

  ## first panel
  omar <- par()["mar"]
  on.exit(par(omar))
  par(mar = c(5.1, 4.1, 4.1, 0.5))
  plot(xrange, c(0, 1), type = 'n', ylab = "Distribution across grid cells", xlab = "Days since burial", xaxt = "n", ...)
  abline(v = pretty(seq(xrange[1], xrange[2])), lty = "dotted", col = "gray70")
  pwcols <- lapply(xage, function(x) vf$gtype_class[vf$age == x])
  beeswarm::bxplot(lapply(xage, function(x) vf$dist[vf$age == x]), at = xage, add = TRUE)
  beeswarm::beeswarm(lapply(xage, function(x) vf$dist[vf$age == x]), at = xage, method = "center", add = TRUE,
                     pch = 19, pwcol = lapply(pwcols, function(cve) sapply(cve, function(gc) adjustcolor(grainCols$colour[grainCols$gtype == gc], alpha.f = 0.5))))
  axis(1, at = pretty(seq(xrange[1], xrange[2])))

  ## plot Persistent boxplot
  pp <- unlist(lapply(wkl_uuid, function(wid) {
    sapply(gclasses, function(gc) {
      vtmp <- vf[vf$wkl_uuid == wid & vf$gtype_class == gc, ]
      k <- order(vtmp$age)
      vtmp$dist[k[which(vtmp$tmode[k] == "Persistent Slabs")][1]]
    })
  }))
  pp <- pp[!is.na(pp)]
  if (length(pp) > 0) {
    par(mar = c(5.1, 2.1, 4.1, 0.5))
    beeswarm::bxplot(pp, yaxt = "n", xlab = "First time\npersistent p.", ylim = c(0, 1))
    beeswarm::beeswarm(pp, add = TRUE, method = "center",
                       pwcol = sapply(names(pp), function(gc) adjustcolor(grainCols$colour[grainCols$gtype == gc], alpha.f = 0.8)), pch = 19)
    axis(2, at = pretty(c(0, 1), n = 6))
  }

  ## plot Deep Persistent boxplot
  dpp <- unlist(lapply(wkl_uuid, function(wid) {
    sapply(gclasses, function(gc) {
      vtmp <- vf[vf$wkl_uuid == wid & vf$gtype_class == gc, ]
      k <- order(vtmp$age)
      vtmp$dist[k[which(vtmp$tmode[k] == "Deep Persistent Slabs")][1]]
    })
  }))
  dpp <- dpp[!is.na(dpp)]
  if (length(dpp) > 0) {
    par(mar = c(5.1, 2.1, 4.1, 0.5))
    beeswarm::bxplot(dpp, yaxt = "n", xlab = "First time\ndeep\npersistent p.", ylim = c(0, 1))
    beeswarm::beeswarm(dpp, add = TRUE, method = "center",
                       pwcol = sapply(names(dpp), function(gc) adjustcolor(grainCols$colour[grainCols$gtype == gc], alpha.f = 0.8)), pch = 19)
    axis(2, at = pretty(c(0, 1), n = 6))
  }

  par(omar)
  par(omfrow)
}




## --- seasonal overview problem days & captWKL/poorWKL ----
#' Timeseries seasonal overview avy problems and poor WKLs
#' @export
plotSeasonalOverviewProblemsPoorWKL <- function(VD, showPWLs = FALSE, shadeAvProbPeriods = TRUE, plotCircles = FALSE, plotBars = !plotCircles,
                                                pwcex = 1.5, withXAxis = TRUE, xticks_Sat = TRUE, captCol = RColorBrewer::brewer.pal(5, "Pastel1"), cex.tag = par()$cex.axis, ...) {

  if (any(VD$executions$flat != "flat")) stop("This function was designed for flat field analysis, not virtual slopes! The way the distributions are calculated might not be correct for vslope analysis --> look into the way nSP is handled!")
  vf <- VD$vframe
  vf$vdate <- as.Date(vf$vdate)
  capt <- VD$captWKL
  capt$vdate <- as.Date(capt$vdate)
  pwl <- VD$poorWKL
  pwl$vdate <- as.Date(pwl$vdate)
  xdate <- as.Date(seq(from = min(pwl$vdate, vf$vdate), to = max(pwl$vdate, vf$vdate), by = "day"))

  shadeAvProbPeriod <- function(vf) {
    ## draw av problem period
    date_dps <- unique(vf$vdate[vf$tmode %in% c("Deep Persistent Slabs")])
    date_ps <- unique(vf$vdate[vf$tmode %in% c("Persistent Slabs") & !vf$vdate %in% date_dps])
    date_st <- unique(vf$vdate[vf$tmode %in% c("Storm Slabs") & !vf$vdate %in% c(date_dps, date_ps)])
    if (length(date_dps) > 0) rect(date_dps-0.5, -1000, date_dps+0.5, 1000, col=SarpGeneral::getAvCharColor("Deep Persistent Slabs", Transparency = '10'), border=NA)
    if (length(date_ps) > 0) rect(date_ps-0.5, -1000, date_ps+0.5, 1000, col=SarpGeneral::getAvCharColor("Persistent Slabs", Transparency = '10'), border=NA)
    if (length(date_st) > 0) rect(date_st-0.5, -1000, date_st+0.5, 1000, col=SarpGeneral::getAvCharColor("Storm Slabs", Transparency = '20'), border=NA)
  }

  wkllist <- lapply(VD$wkl$wkl_uuid, function(wid) {
    pdays <- unique(VD$vframe$vdate[VD$vframe$wkl_uuid == wid & !is.na(VD$vframe$tmode)])
    if (length(pdays) > 0) {
      list(wkl_uuid = wid,
           wkl_name = VD$wkl$wkl_name[VD$wkl$wkl_uuid == wid],
           datetag = VD$wkl$datetag[VD$wkl$wkl_uuid == wid],
           gclasses = ifelse(as.logical(VD$wkl$iscrust[VD$wkl$wkl_uuid == wid]),
                             yes = paste(unique(na.omit(c(na.omit(c(VD$vframe$gtype_class[VD$vframe$wkl_uuid == wid & VD$vframe$gtype_rank == "tertiary"][1], "MFcr"))[1],
                                                          VD$vframe$gtype_class[VD$vframe$wkl_uuid == wid & VD$vframe$gtype_rank == "primary"][1],
                                                          VD$vframe$gtype_class[VD$vframe$wkl_uuid == wid & VD$vframe$gtype_rank == "secondary"][1])))),
                             no = paste(unique(na.omit(c(VD$vframe$gtype_class[VD$vframe$wkl_uuid == wid & VD$vframe$gtype_rank == "primary"][1],
                                                         VD$vframe$gtype_class[VD$vframe$wkl_uuid == wid & VD$vframe$gtype_rank == "secondary"][1],
                                                         VD$vframe$gtype_class[VD$vframe$wkl_uuid == wid & VD$vframe$gtype_rank == "tertiary"][1]))))),
           dq = VD$wkl$dq[VD$wkl$wkl_uuid == wid],
           pdays = sort(as.Date(pdays)))
    }
  })
  wkllist <- wkllist[!sapply(wkllist, is.null)]
  wkllist_k <- order(sapply(wkllist, function(le) le$pdays[1]))

  if (showPWLs) {
    plot(xdate, seq_along(xdate), ylim = c(0.5, length(wkllist)+0.5), type = "n", ylab = "", xlab = "", yaxt = "n", xaxt = "n",  ...)
    abline(h = seq_along(wkllist), lty = 3, col = "gray70")
    if (shadeAvProbPeriods) shadeAvProbPeriod(vf)
    for (i in seq_along(wkllist)) {
      k <- wkllist_k[i]
      yi <- length(wkllist) + 1 - i
      wkltag <- paste0(format(as.Date(wkllist[[k]]$datetag), "%b %d"), " ", wkllist[[k]]$gclasses, ":")
      if (length(captCol) > 1) {
        col <- c(captCol[(which(VD$wkl$wkl_uuid %in% wkllist[[k]]$wkl_uuid)[1] %% length(captCol))+1], "white")[1]
      } else {
        col <- captCol
      }
      # mtext(wkltag, side = 2, line = par()$mar[2]-1, at = yi, las = 2, adj = 0, cex = par()$cex.axis)
      text(as.Date(wkllist[[k]]$datetag), yi, labels = wkltag, adj = 1, cex = cex.tag)
      # mtext("Tracked layers of concern", side = 2, line = par()$mar[2]-1, cex = par()$cex.axis)
      points(wkllist[[k]]$pdays, rep(yi, times = length(wkllist[[k]]$pdays)), pch = 19,
             col = col, cex =  pwcex)  # scaled cex: (2-(wkllist[[k]]$dq/4)) * pwcex
    }
  } else {

    plot(xdate, seq_along(xdate), ylim = c(0, 1.05), type = "n", yaxs = "i", xaxt = "n", xlab = "", ...)
    # abline(h = 1)
    abline(h = c(seq(0.2, 1, by = 0.2)), col = "gray70", lty = "dotted")
    if (shadeAvProbPeriods) shadeAvProbPeriod(vf)

    ## draw line: distribution (any layer, then captWKL)
    if (plotCircles) {
      plot(xdate,
             sapply(xdate, function(xd) length(unique(c(capt$vstation_id[capt$vdate == xd & capt$pu >= 0.77], pwl$vstation_id[pwl$vdate == xd])))) / median(vf$nSP[vf$vdate %in% xdate], na.rm = TRUE),
             col = "gray70", pch = 19, type = 'b', cex = par()$cex * pwcex)
      points(xdate,
             sapply(xdate, function(xd) length(unique(capt$vstation_id[capt$vdate == xd & capt$pu >= 0.77 & capt$wkl_uuid %in% vf$wkl_uuid[vf$vdate == xd & !is.na(vf$tmode)]]))) / median(vf$nSP[vf$vdate %in% xdate], na.rm = TRUE),
             col = captCol, pch = 19, type = 'b', cex = par()$cex * pwcex)

    } else if (plotBars) {
      rect(xdate-0.5, 0, xdate+0.5,
           sapply(xdate, function(xd) length(unique(c(capt$vstation_id[capt$vdate == xd & capt$pu >= 0.77], pwl$vstation_id[pwl$vdate == xd])))) / median(vf$nSP[vf$vdate %in% xdate], na.rm = TRUE),
           col = "gray70", border = NA)
      if (length(captCol) > 1) {
        wids <- sapply(xdate, function(xd) vf$wkl_uuid[vf$vdate == xd & !is.na(vf$tmode)])
        wids <- unlist(sapply(wids, function(wid) {
          if (length(wid) == 0) {
            NA
          } else if (length(wid) == 1) {
            wid
          } else {
            VD$wkl$wkl_uuid[VD$wkl$wkl_uuid %in% wid][which.max(VD$wkl$dq[VD$wkl$wkl_uuid %in% wid])]
          }
        }))
        rect(xdate-0.5, 0, xdate+0.5,
             sapply(xdate, function(xd) length(unique(capt$vstation_id[capt$vdate == xd & capt$pu >= 0.77 & capt$wkl_uuid %in% vf$wkl_uuid[vf$vdate == xd & !is.na(vf$tmode)]]))) / median(vf$nSP[vf$vdate %in% xdate], na.rm = TRUE),
             border = NA, col = sapply(wids, function(wid) c(captCol[(which(VD$wkl$wkl_uuid %in% wid)[1] %% length(captCol))+1], "white")[1]
             ))
      } else {
        rect(xdate-0.5, 0, xdate+0.5,
             sapply(xdate, function(xd) length(unique(capt$vstation_id[capt$vdate == xd & capt$pu >= 0.77 & capt$wkl_uuid %in% vf$wkl_uuid[vf$vdate == xd & !is.na(vf$tmode)]]))) / median(vf$nSP[vf$vdate %in% xdate], na.rm = TRUE),
             border = NA, col = captCol)
      }

    }
    dist_wkl_exists <- sapply(xdate, function(xd) length(unique(capt$vstation_id[capt$vdate == xd & capt$wkl_uuid %in% vf$wkl_uuid[vf$vdate == xd & !is.na(vf$tmode)]]))) / median(vf$nSP[vf$vdate %in% xdate], na.rm = TRUE)
    if (length(captCol) > 1) {
      points(xdate[dist_wkl_exists != 0], dist_wkl_exists[dist_wkl_exists != 0], col = sapply(wids, function(wid) c(captCol[(which(VD$wkl$wkl_uuid %in% wid)[1] %% length(captCol))+1], "white")[1])[dist_wkl_exists != 0], pch = 19)
    } else {
      points(xdate[dist_wkl_exists != 0], dist_wkl_exists[dist_wkl_exists != 0], col = "black", pch = 19)
    }

  }

  Saturdays <- xdate[weekdays(xdate, abbreviate = FALSE) == "Saturday"]
  SaturdayLabels <- format(xdate[weekdays(xdate, abbreviate = FALSE) == "Saturday"], "%b %d")
  if (xticks_Sat){
    abline(v = Saturdays, lty = 3, col = "gray70")
  }
  if (withXAxis) {
      axis(1, at = Saturdays, labels = SaturdayLabels, las = 2)
  }

}




## --- assessQualityOfcaptWKL figure ----
#' @export
plotTSDngLlhdWklPuAvg <- function(VD, Bull, avgSP = NULL, band = "TL") {

  ## INIT
  BANDS <- data.frame(input = c("ALP", "TL", "BTL"), output = c("Alp", "Tl", "Btl"))
  band <- BANDS[which(BANDS$input == band), "output"]

  xdaterange <- as.Date(c(min(VD$captWKL$vdate, VD$poorWKL$vdate), max(VD$captWKL$vdate, VD$poorWKL$vdate)))
  if (!is.null(avgSP)) {
    xdaterange <- as.Date(c(max(xdaterange[1], min(avgSP$meta$date)), min(xdaterange[2], max(avgSP$meta$date))))
  }

  ## DRAW
  layout(matrix(seq(5), ncol = 1), heights = c(0.8, 2.5, 2.2, 2.5, 4))
  par(xaxs = "i", cex = 1.5)

  ## Plot danger ratings and problems
  par(mar = c(0.5, 5.1, 0.5, 1.1))
  par(bty = "n")
  SarpGeneralVis::plotTSHzdRatingAvProb(Bull, ElevBand = band, HighlightAvProb = c("STORM", "PERS", "DPERS"), plotAvProb = FALSE , DateStart = xdaterange[1], DateEnd = xdaterange[2], WithXAxis = FALSE)
  ## Likelihood
  par(mar = c(1.1, 5.1, 0.5, 1.1))
  plotTSAvProbLikelihood(Bull, ElevBand = band, withXAxis = FALSE, xlim = xdaterange)
  abline(h = c(1, 3, 5, 7), lty = "dotted", col = "gray70")

  ## plot WKL stability overview
  par(mar = c(4.1, 5.1, 0.5, 1.1))
  plotSeasonalOverviewProblemsPoorWKL(VD, xlim = xdaterange, showPWLs = TRUE, bty ="n", shadeAvProbPeriods = FALSE, pwcex = 1.1)
  # shadeAvProbPeriod(BullGNP)
  par(mar = c(1.1, 5.1, 0.5, 1.1))
  plotSeasonalOverviewProblemsPoorWKL(VD, xlim = xdaterange, xaxt = "n" , shadeAvProbPeriods = FALSE, plotBars = TRUE, withXAxis = FALSE)
  abline(h = 0)

  ## plot avgSP with ppu_all
  if (!is.null(avgSP)) {
    ## make ppu_all avail for plotting
    if (!"percentage" %in% names(avgSP$avgs[[1]]$layers)) {
      avgSP$avgs <- snowprofileSet(lapply(avgSP$avgs, function(avg) {
        avg$layers$percentage <- avg$layers$ppu_all
        avg
      }))
    }

    par(mar = c(4.1, 5.1, 0.1, 1.1))
    plot(avgSP$avgs[avgSP$meta$date >= xdaterange[1]+1 & avgSP$meta$date <= xdaterange[2]], colAlpha = 0.5, xaxs = "i", yaxs = "i", box = FALSE)
    plot(avgSP$avgs[avgSP$meta$date >= xdaterange[1]+1 & avgSP$meta$date <= xdaterange[2]], ColParam = "percentage", add = TRUE, box = FALSE)
    mtext("Height (cm)", side = 2, line = 3, cex = 1.5)
  }

}


## --- plotWXandPotentialDatetags ----
#' @export
plotWXandPotentialDatetags <- function(WX24, PDT, VD, avgSP = NULL) {
  xdaterange <- as.Date(c(min(VD$captWKL$vdate, VD$poorWKL$vdate), max(VD$captWKL$vdate, VD$poorWKL$vdate)))
  if (!is.null(avgSP)) {
    xdaterange <- as.Date(c(max(xdaterange[1], min(avgSP$meta$date)), min(xdaterange[2], max(avgSP$meta$date))))
  }

  if (!is.null(avgSP)) {
    op <- par(mfrow = c(3, 1))
  } else {
    op <- par(mfrow = c(2, 1))
  }

  plot(WX24$date, WX24$hn72, type = "l", col = "red", ylim = c(0, max(WX24$hn24, WX24$hn72, 10.5)), xlim = xdaterange, xaxs = "i", yaxs = "i")
  lines(WX24$date, WX24$hn24)
  abline(v = PDT, col = "gray70", lty = "dotted")
  abline(v = as.Date(VD$wkl$datetag), col = "red", lty = "dotted")
  abline(h = 10, col = "gray70", lty = "dotted")
  legend("topright", c("hn72", "hn24"), col = c("red", "black"), lty = c("solid", "solid"), bty = "n")
  mtext(paste(region, season), line = 1)

  plot(WX24$date, WX24$rain72, type = "l", col = "red", ylim = c(0, max(WX24$rain72, WX24$rain24, 5.5)), xlim = xdaterange, xaxs = "i", yaxs = "i")
  lines(WX24$date, WX24$rain24)
  abline(v = PDT, col = "gray70", lty = "dotted")
  abline(h = 5, col = "gray70", lty = "dotted")
  legend("topright", c("rain72", "rain24"), col = c("red", "black"), lty = c("solid", "solid"), bty = "n")

  ## plot avgSP with ppu_all
  if (!is.null(avgSP)) {
    ## make ppu_all avail for plotting
    if (!"percentage" %in% names(avgSP$avgs[[1]]$layers)) {
      avgSP$avgs <- snowprofileSet(lapply(avgSP$avgs, function(avg) {
        avg$layers$percentage <- avg$layers$ppu_all
        avg
      }))
    }

    # par(mar = c(4.1, 5.1, 0.1, 1.1))
    plot(avgSP$avgs[avgSP$meta$date >= xdaterange[1]+1 & avgSP$meta$date <= xdaterange[2]], colAlpha = 0.5, xaxs = "i", yaxs = "i", box = FALSE, VerticalGrid = FALSE)
    plot(avgSP$avgs[avgSP$meta$date >= xdaterange[1]+1 & avgSP$meta$date <= xdaterange[2]], ColParam = "percentage", add = TRUE, box = FALSE, VerticalGrid = FALSE)
    # mtext("Height (cm)", side = 2, line = 3, cex = 1.5)
    abline(v = PDT, col = "black", lty = "dashed")
    abline(v = as.Date(VD$wkl$datetag), col = "red", lty = "dashed")
  }

  par(op)

}

