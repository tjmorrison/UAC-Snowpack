## This script contains figures for the paper.
## All figures that directly result from an analysis script are plotted there,
## all other figures are plotted here.

library(sarp.2021.herla.snowprofilevalidation)
library(SarpBulletinTools)
## read and format ValidationData from GNP 2019 TL
VD <- readRDS("data/VData_GNP_2019_TL_ranks12.rds")
VD$config$poorWKLconstraint <- "pu >= 0.77 AND gtype NOT IN ('PP', 'DF')"  # original VD object contains broken config entry
WX24 <- readRDS("data/WX24_GNP_2019_TL.rds")
PDT <- derivePotentialDatetags(WX24, VD$wkl$datetag)
VD <- transactPoor2CaptWKL(VD, WX24, PDT)
## get Bulletins
Bulletins <- readRDS("data/AllBulletins_2010To2022.rds")
BullUUIDs <- suppressWarnings(Bulletins$Bulletins$BULLETIN_ID[Bulletins$Bulletins$SEASON %in% 2019 & Bulletins$Bulletins$REGION %in% RegionNamesList[["GNP"]]])
BullGNP <- extractFromBulletinObj(Bulletins, ByBulletinID = BullUUIDs)
## get average profile timeseries
avgSP <- readRDS("output/averageProfiles/GNP2019_TL.rds")


## --- gtype legend ----
png(filename = "output/figures/paper/legend_gtypes.png", width = 800, height = 60, bg="transparent")
par(mar=c(0, 0, 0, 0))
plot(NULL , xaxt='n', yaxt='n', bty='n', ylab='', xlab='', xlim=0:1, ylim=0:1)
legend("topleft", title="", legend = grainDict$gtype[c(3, 4, 1, 2, 7, 6, 5, 8, 9)], pch = 15, cex=1.8,
       col = grainDict$colour[c(3, 4, 1, 2, 7, 6, 5, 8, 9)], horiz = T, pt.cex = 4, bty = "n",
       x.intersp = 0.7)
dev.off()

## --- CTree Variable Type legend ----
# png(filename = "output/figures/paper/legend_ctreeVarType.png", width = 400, height = 120, bg="transparent")
# par(mar=c(0, 0, 0, 0))
# plot(NULL , xaxt='n', yaxt='n', bty='n', ylab='', xlab='', xlim=0:1, ylim=0:1)
# legend("topleft", title="Variable type", legend = c('Time', 'Layer', 'Quality')[seq(3)], pch = 15, cex=1.8, title.adj = 0,
#        col = RColorBrewer::brewer.pal(4, 'Pastel1')[c(1, 2, 4)], horiz = T, pt.cex = 4, bty = "n",
#        x.intersp = 0.7)
# dev.off()


## --- GNP 2019 TL methods demo ----
## Fig 4a in paper
VD$captWKL$datetag <- as.Date(sapply(VD$captWKL$wkl_uuid, function(wid) VD$wkl$datetag[VD$wkl$wkl_uuid == wid]))
ubd <- sort(unique(c(as.Date(VD$poorWKL$bdate), as.Date(VD$captWKL$bdate))))
png("output/figures/paper/histogram_datetags_poor_capt_GNP_2019_TL_ranks12_c_v6.png", 1200, height = 900)
par(mfrow = c(2, 1), cex = 1.8, cex.lab = 1.5, mar = c(2.1, 6.1, 4.1, 2.1))

## POORWKL
Saturdays <- seq(min(ubd), max(ubd), 1)[weekdays(seq(min(ubd), max(ubd), 1), abbreviate = FALSE) == "Saturday"]
SaturdayLabels <- format(seq(min(ubd), max(ubd), 1)[weekdays(seq(min(ubd), max(ubd), 1), abbreviate = FALSE) == "Saturday"], "%b %d")
datetags <- VD$poorWKL$datetag
toHist <- as.Date(unlist(lapply(unique(datetags), function(bd) rep(as.character(bd), times = 100*length(unique(VD$poorWKL$vstation[datetags == bd])) / max(VD$poorWKL$nSP[datetags == bd]) ))))
H <- hist(toHist,
          breaks = seq(min(ubd, datetags), to = max(ubd, datetags), by = 1), freq = TRUE, ylab = "Max daily proportion of\nunstable grid points (%)", col = "lightgray",
          xlim = c(min(ubd), max(ubd)), ylim = c(0, 100), xaxt = "n", xlab = "", main = "", xaxs = "i")
toHist_belowThreshold <- toHist[toHist %in% as.Date(names(table(toHist))[table(toHist) <= 50])]
H <- hist(toHist_belowThreshold,
          breaks = seq(min(ubd, datetags), to = max(ubd, datetags), by = 1), freq = TRUE, col = "gray50",
          xaxt = "n", add = TRUE)
# axis(1, at = Saturdays, labels = SaturdayLabels, las = 2)
axis(2, at = pretty(c(20, 40, 60, 80, 100)))
# abline(v = as.Date(VD$wkl$datetag), lty = "solid", col = "red")
# abline(v = unique(as.Date(VD$poorWKL$datetag)), lty = "dashed")
abline(h = c(20, 40, 60, 80, 100), lty = "dotted", col = "lightgray")

## CAPTWKL
## proportion histogram
# VD$captWKL$datetag <- as.Date(sapply(VD$captWKL$wkl_uuid, function(wid) VD$wkl$datetag[VD$wkl$wkl_uuid == wid]))
datetags <- VD$captWKL$datetag
H <- hist(as.Date(unlist(lapply(unique(datetags), function(bd) rep(as.character(bd), times = 100*length(unique(VD$captWKL$vstation[datetags == bd & VD$captWKL$pu >= 0.77])) /
                                                                     max(VD$vframe$nSP[VD$vframe$vf_uuid %in% VD$captWKL$vf_uuid[datetags == bd]], na.rm = TRUE) )))),
          breaks = seq(min(ubd, datetags), to = max(ubd), by = 1), freq = TRUE, col = "red", xaxt = "n",
          add = TRUE)
# ylab = "Proportion of unstable grid points (%)",
# xlim = c(min(ubd), max(ubd)), ylim = c(0, 100), xlab = "", main = "", xaxs = "i")

par(mar = c(4.1, 6.1, 2.1, 2.1))
## avgSP
if (!is.null(avgSP)) {
  ## make ppu_all avail for plotting
  if (!"percentage" %in% names(avgSP$avgs[[1]]$layers)) {
    avgSP$avgs <- snowprofileSet(lapply(avgSP$avgs, function(avg) {
      avg$layers$percentage <- avg$layers$ppu_all
      avg
    }))
  }

  plot(avgSP$avgs[avgSP$meta$date >= min(ubd) & avgSP$meta$date <= max(ubd)], colAlpha = 0.5, xaxs = "i", yaxs = "i", box = FALSE, ylab = "")
  plot(avgSP$avgs[avgSP$meta$date >= min(ubd) & avgSP$meta$date <= max(ubd)], ColParam = "percentage", add = TRUE, box = FALSE, ylab = "")
  mtext("Height (cm)", side = 2, line = 3, cex = 2.5)
  abline(v = unique(as.Date(VD$poorWKL$datetag)), lty = "dashed", lwd = 3)
  abline(v = unique(as.Date(VD$captWKL$datetag)), lty = "dashed", col = "red", lwd = 3)
}
dev.off()


## --- seasonal overview composite ----
## Fig 14 in paper
VD$wkl <- VD$wkl[!VD$wkl$wkl_name %in% "2019.Apr 3 Crust", ]
band <- "Tl"
xdaterange <- as.Date(c(min(VD$captWKL$vdate, VD$poorWKL$vdate), max(VD$captWKL$vdate, VD$poorWKL$vdate)))
if (!is.null(avgSP)) {
  xdaterange <- as.Date(c(max(xdaterange[1], min(avgSP$meta$date)), min(xdaterange[2], max(avgSP$meta$date))))
}
xdaterange[2] <- as.Date("2019-04-02")

png(filename = "output/figures/paper/GNP2019_composite.png", width = 1475, height = 738)
layout(matrix(seq(5), ncol = 1), heights = c(1, 2, 3, 3, 5))
par(xaxs = "i", cex.axis = 1.7, cex.lab = 1.9)

## Plot danger ratings
par(mar = c(1.1, 8.1, 0.5, 1.1))
par(bty = "n")
plotTSHzdRatingAvProb(BullGNP, ElevBand = "Tl", HighlightAvProb = c("STORM", "PERS", "DPERS"), plotAvProb = FALSE , DateStart = xdaterange[1], DateEnd = xdaterange[2], WithXAxis = FALSE)
mtext("Danger rating @TL", side = 2, line = 0, las = 1, adj = 0.5)

## plot tracked WKLs
par(mar = c(0.5, 8.1, 0.5, 1.1))
plotSeasonalOverviewProblemsPoorWKL(VD, xlim = xdaterange, showPWLs = TRUE, bty ="n", shadeAvProbPeriods = FALSE, pwcex = 3, withXAxis = FALSE)
axis(side = 2, at = seq(5), labels = rep("", 5))
shadeAvProbPeriod(BullGNP)
mtext("Tracked layers\nof concern", side = 2, line = 2)

## plot problems
par(mar = c(5.1, 8.1, 0.5, 1.1))
plotTSAvProbLikelihood(BullGNP, ElevBand = "Tl", withXAxis = TRUE, withYAxis = FALSE, xlim = xdaterange, plotBars = TRUE, axes = FALSE)
abline(h = c(0, 2, 4, 6, 8), lty = "dotted", col = "gray70")
axis(side = 2, line = -2, at = c(0, 2, 4, 6, 8), hadj = 0, tick = FALSE ,labels = c("Unlikely", "Possible", "Likely", "Very likely", "Almost cert."), las = 2)
axis(side = 2, line = 0, at = c(0, 2, 4, 6, 8), tick = TRUE ,labels = rep("", 5))
mtext("Likelihood of\n(associated)\navalanche problem", side = 2, line = 2)
legend(x = xdaterange[1]+17, y = 8.2, title = "Avalanche problem type", legend = c("Persistent Slabs", "Storm Slabs"), border = c(NA, NA), pt.cex = 2.5, pch = 15, box.lwd = "white", cex = 1.4,
       col = c(SarpGeneral::getAvCharColor("Persistent Slabs", Transparency = "60"), SarpGeneral::getAvCharColor("Storm Slabs", Transparency = "60")))

## plot simulated WKL stability
par(mar = c(1.1, 8.1, 0.5, 1.1))
plotSeasonalOverviewProblemsPoorWKL(VD, xlim = xdaterange, xaxt = "n" , withXAxis = FALSE, axes = FALSE, ylab = "")
axis(side = 2, at = seq(0,1, by = 0.2), las = 2)
mtext("Proportion of\ngrid points", side = 2, line = 4)
legend(x = xdaterange[1]+17, y = 1, c("structurally captured", "unstable (in colored layer)", "unstable (in any layer)"), title = "Proportions of grid points", title.adj = 0,
       box.lwd = "white", col = c("black", "black", "gray70"), pch = c(20, 15, 15), border = c(NA, NA, NA), cex = 1.4, pt.cex = 2.5)

## plot avgSP incl ppu_all
par(mar = c(7.1, 8.1, 0.1, 1.1))
plot(avgSP$avgs[avgSP$meta$date > xdaterange[1] & avgSP$meta$date <= xdaterange[2]], colAlpha = 0.3, xaxs = "i", yaxs = "i")
shadeAvProbPeriod(BullGNP)
## quick hack to draw white profile over problem shadings:
avgSP$avgs <- snowprofileSet(lapply(avgSP$avgs, function(avg) {
  avg$layers$density <- NA
  avg
}))
plot(avgSP$avgs[avgSP$meta$date >= xdaterange[1] & avgSP$meta$date <= xdaterange[2]], colAlpha = 1, ColParam = "density", xaxs = "i", yaxs = "i", add = TRUE, yaxis = FALSE, ylab = "")
plot(avgSP$avgs[avgSP$meta$date >= xdaterange[1] & avgSP$meta$date <= xdaterange[2]], colAlpha = 0.3, xaxs = "i", yaxs = "i", add = TRUE, yaxis = FALSE, ylab = "")
lines(avgSP$meta$date, avgSP$meta$hs_median, lwd = 1)
## median thickness of new snow
medianThicknessNewSnow <- sapply(avgSP$sets, function(set) {
  median(sapply(set, function(sp) {
    sum(sp$layers$thickness[findPWL(sp, pwl_gtype = c("PP", "DF"))])
  }))
})
lines(avgSP$meta$date, avgSP$meta$hs_median - medianThicknessNewSnow, lty = "dashed", lwd = 1)
avgSP$avgs <- snowprofileSet(lapply(avgSP$avgs, function(avg) {
  avg$layers$percentage <- avg$layers$ppu_all
  avg
}))
plot(avgSP$avgs[avgSP$meta$date >= xdaterange[1] & avgSP$meta$date <= xdaterange[2]], ColParam = "percentage", add = TRUE, yaxis = FALSE, ylab = "")
legend(as.Date("2018-09-23"), 190,
       c("<HS>", "<NEW>", "PP", "DF", "SH", "DH", "FC", "FCxr", "RG", "MF", "MFcr"),
       lty = c("solid", "dashed", rep(NA, 9)), lwd = 2,
       col = c(rep("black", 2), getColoursGrainType(c("PP", "DF", "SH", "DH", "FC", "FCxr", "RG", "MF", "MFcr"))), pch = c(NA, NA, rep(15, 9)), pt.cex = 2.5,
       density = c(rep(0, 11)), border = "transparent",
       horiz = FALSE, bty = "o", box.lwd = 0, cex = 1.4)
dev.off()






#######################################
## --- detailed WKL zoom composite ----
## Fig 15 in paper

## Avalanche problem legend
png(filename = "output/figures/paper/legend_avprobtype.png", width = 800, height = 120, bg="transparent")
par(mar=c(0, 0, 0, 0))
plot(NULL , xaxt='n', yaxt='n', bty='n', ylab='', xlab='', xlim=0:1, ylim=0:1)
legend("topleft", title="Avalanche problem type", legend = c("Persistent Slabs", "Storm Slabs"), border = c(NA, NA), pt.cex = 4, pch = 15, box.lwd = "white", cex = 1.8,
       col = c(SarpGeneral::getAvCharColor("Persistent Slabs", Transparency = "60"), SarpGeneral::getAvCharColor("Storm Slabs", Transparency = "60")))
dev.off()
## Proportion legend
png(filename = "output/figures/paper/legend_proportions.png", width = 800, height = 120, bg="transparent")
par(mar=c(0, 0, 0, 0))
plot(NULL , xaxt='n', yaxt='n', bty='n', ylab='', xlab='', xlim=0:1, ylim=0:1)
legend("topleft", c("structurally captured", "unstable (in this layer)", "unstable (in any layer)"), title = "Proportions of grid points", title.adj = 0,
       box.lwd = "white", col = c("black", "black", "gray70"), pch = c(20, 15, 15), border = c(NA, NA, NA), cex = 1.4, pt.cex = 4)
dev.off()
## gtype legend light
png(filename = "output/figures/paper/legend_gtypes_light.png", width = 800, height = 60, bg="transparent")
par(mar=c(0, 0, 0, 0))
plot(NULL , xaxt='n', yaxt='n', bty='n', ylab='', xlab='', xlim=0:1, ylim=0:1)
legend("topleft", title="Grain types", legend = grainDict$gtype[c(1, 2, 7)], pch = 15, cex=1.8, title.adj = 0,
       col = grainDict$colour[c(1, 2, 7)], horiz = T, pt.cex = 4, bty = "n",
       x.intersp = 0.7)
dev.off()


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
grainCols <- rbind(grainDict, data.frame(gtype = c("IFrc", "IFsc"), colour = grainDict$colour[grainDict$gtype == "IF"]))
wkl_uuid <- "BA673042-F9E7-50FC-BEA5-3F961C82D0E4"
gtype_rank <- c("secondary", "primary")
vf <- VD$vframe[VD$vframe$wkl_uuid %in% wkl_uuid, ]
xrange <- c(min(vf$age[vf$wkl_uuid %in% wkl_uuid])-0.5, max(vf$age[vf$wkl_uuid %in% wkl_uuid])+1)
xdate <- sort(unique(vf$vdate))
xdaterange <- as.Date(range(xdate))
xage <- sort(unique(vf$age))
xweekdays <- weekdays(as.Date(xdate), abbreviate = FALSE)
weekday0 <- xweekdays[xage == 0]
xlabelsdate <- format(as.Date(xdate)[xweekdays == weekday0], "%b %d")
xlabelsdt <- as.Date(xdate)[xweekdays == weekday0]
xlabelsage <- xage[xweekdays == weekday0]

## Setup figure
title <- ""  # paste0(VD$wkl$wkl_name[VD$wkl$wkl_uuid == wkl_uuid])
png("output/figures/paper/Jan17_composite.png", width = 1200, height = 1200)
# par(mfrow = c(9, 1))
layout(matrix(seq(9), ncol = 1), heights = c(1, rep(2, 8)))
par(cex.axis = 2, cex.lab = 2.2)
cex.mtext <- 1.5
## subset VD to the one wkl_uuid
vd <- VD
vd$wkl <- vd$wkl[vd$wkl$wkl_uuid %in% wkl_uuid, ]
vd$vframe <- vd$vframe[vd$vframe$wkl_uuid %in% wkl_uuid, ]

## plot tracked WKL
par(mar = c(0.5, 10.1, 1, 1.1))
plotSeasonalOverviewProblemsPoorWKL(vd, xlim = xdaterange, showPWLs = TRUE, bty ="n", shadeAvProbPeriods = FALSE, pwcex = 3, withXAxis = FALSE, xticks_Sat = FALSE, captCol = "black", xaxs = "i", cex.tag = 1.5)
shadeAvProbPeriod(BullGNP)
abline(v = xlabelsdt, lty = 3, col = "gray70")
mtext("Tracked\nlayer\nof concern", side = 2, line = 0.5, las = 1, at = 1, cex = cex.mtext)
mtext("(a)", side = 3, line = -0.5, at = xdaterange[1]-7, cex = cex.mtext+0.3)

## plot problems
par(mar = c(5.1, 10.1, 0.5, 1.1))
plotTSAvProbLikelihood(BullGNP, ElevBand = "Tl", withXAxis = FALSE, withYAxis = FALSE, xlim = xdaterange, plotBars = TRUE, axes = FALSE, xticks_Sat = FALSE, xaxs = "i")
abline(h = c(0, 2, 4, 6, 8), lty = "dotted", col = "gray70")
axis(side = 2, line = -2, at = c(0, 2, 4, 6, 8), hadj = 0, tick = FALSE ,labels = c("Unlikely", "Possible", "Likely", "Very likely", "Almost cert."), las = 2)
axis(side = 2, line = 0, at = c(0, 2, 4, 6, 8), tick = TRUE ,labels = rep("", 5))
mtext("Likelihood\nava. problem", side = 2, line = 4, at = 3.5, cex = cex.mtext)
axis(1, at = xlabelsdt, labels = xlabelsdate, las = 2)
mtext("(b)", side = 3, line = -1, at = xdaterange[1]-7, cex = cex.mtext+0.3)

## plot simulated WKL stability
par(mar = c(1.1, 10.1, 0.5, 1.1))
plot(xrange, c(0, 1.05), type = 'n', ylab = "", xlab = "", main = title, xaxt = "n", xaxs = "i")
mtext("Proportion of\ngrid points", side = 2, line = 4, cex = cex.mtext)
# axis(side = 2, at = seq(0,1, by = 0.2), las = 2)
# legend("topleft", c("structurally captured", "unstable (in colored layer)", "unstable (in any layer)"), bty = 'n', fill = c(NA, "black", "gray70"), pch = c(19, NA, NA), border = c(NA, NA, NA), cex = 1.4)
abline(v = xlabelsage, lty = 3, col = "gray70")
entity <- vf$wkl_uuid == wkl_uuid & vf$gtype_rank %in% gtype_rank
k <- order(vf$age[entity])
rect(xage-0.5, 0,
     xage+0.5, sapply(xdate, function(dt) {
       (length(unique(vd$poorWKL$vstation_id[vd$poorWKL$vdate == dt])) + length(unique(vd$captWKL$vstation_id[vd$captWKL$vdate == dt & vd$captWKL$pu >= 0.77])))/max(vf$nSP[vf$vdate == dt])
       }), col = "gray70", border = "gray70")
lines(vf$age[entity][k], vf$dist[entity][k], type = "b", pch = 19, col = "black")
rect(vf$age[entity][k]-0.5, 0,
     vf$age[entity][k]+0.5, vf$proportion_unstable[entity][k], col = "black")
mtext("(c)", side = 3, line = -0.5, at = xrange[1]-7, cex = cex.mtext+0.3)
## plot simulated properties
## draw distributions
box_alpha <- 0.3
# both_gclasses <- vf$wkl_uuid %in% wkl_uuid

## SLAB WKL depth
par(mar = c(0.5, 10.1, 0.5, 1.1))
dpth <- extractIndex(vd$captWKL, wkl_uuid, vf$vf_uuid, xdate, "depth", max)
dpth <- lapply(dpth, function(ve) -1*as.numeric(ve))
names(dpth) <- xage
if (!any(sapply(dpth, is.numeric))) {
  warning("Not a single layer captured, not drawing any boxplots..")
}
dpth_poor <- lapply(xdate, function(dt) -vd$poorWKL$depth[vd$poorWKL$vdate == dt])
gcol_poor <- lapply(xdate, function(dt) getColoursGrainType(vd$poorWKL$gtype[vd$poorWKL$vdate == dt]))
gcol_poor <- lapply(gcol_poor, function(ve) if (length(ve) > 0) adjustcolor(ve, alpha.f = 0.6))
# beeswarm::bxplot(dpth[sapply(dpth, is.numeric)], at = as.numeric(names(dpth)[sapply(dpth, is.numeric)]), col = "gray", ylab = "layer depth (cm)", xlim = xrange, xaxt = "n")
beeswarm::beeswarm(dpth_poor[sapply(dpth_poor, function(el) length(el) > 0)], at = xage[sapply(dpth_poor, function(el) length(el) > 0)], method = "center", corral = "wrap", pwcol = gcol_poor[sapply(dpth_poor, function(el) length(el) > 0)], pch = 19, cex = 0.5, ylab = "", xlim = xrange, xaxt = "n", xaxs = "i")
vioplot::vioplot(dpth[sapply(dpth, function(dv) length(dv) > 0 & is.numeric(dv))], at = as.numeric(names(dpth)[sapply(dpth, function(dv) length(dv) > 0 & is.numeric(dv))]), col = rgb(0, 0, 0, box_alpha), wex = vf$dist[entity][k], lwd = 0.5, add = TRUE)  # wex = dist_max[sapply(dpth, function(dv) length(dv) > 0 & is.numeric(dv))],
# beeswarm::beeswarm(dpth[sapply(dpth, is.numeric)], at = as.numeric(names(dpth)[sapply(dpth, is.numeric)]), method = "center", corral = "wrap", col = rgb(0, 0, 0, box_alpha), pch = 19, cex = 0.5, add = TRUE)
abline(v = xlabelsage, lty = 3, col = "gray70")
mtext("Layer depth (cm)", side = 2, line = 4, cex = cex.mtext)
mtext("(d)", side = 3, line = -0.5, at = xrange[1]-7, cex = cex.mtext+0.3)

## define p_unstable array of captWKLs
pu <- extractIndex(vd$captWKL, wkl_uuid, vf$vf_uuid, xdate, "pu", max)
names(pu) <- xage
## rhogs
par(mar = c(0.5, 10.1, 0.5, 1.1))
lvar_name <- "sl_rhogs"
lvar_poor <- lapply(xdate, function(dt) vd$poorWKL[vd$poorWKL$vdate == dt, lvar_name])
lvar <- extractIndex(vd$captWKL, wkl_uuid, vf$vf_uuid, sort(unique(vf$vdate)), lvar_name, max)
lvar_ylim <- c(min(c(unlist(lvar_poor), unlist(lvar)), na.rm = TRUE), max(c(unlist(lvar_poor), unlist(lvar)), na.rm = TRUE))
names(lvar) <- sort(unique(vf$age))
beeswarm::beeswarm(lvar_poor[sapply(lvar_poor, function(el) length(el) > 0)], at = xage[sapply(lvar_poor, function(el) length(el) > 0)], method = "center", corral = "wrap", pwcol = gcol_poor[sapply(lvar_poor, function(el) length(el) > 0)], pch = 19, cex = 0.5, ylab = "", xlim = xrange, ylim = lvar_ylim, xaxt = "n", xaxs = "i")
vioplot::vioplot(lvar[sapply(lvar, function(dv) length(dv) > 0 & !all(is.na(dv)))], at = as.numeric(names(pu)[sapply(lvar, function(dv) length(dv) > 0 & !all(is.na(dv)))]), col = rgb(0, 0, 0, box_alpha), wex = vf$dist[entity][k][sapply(lvar, function(dv) length(dv) > 0 & !all(is.na(dv)))], lwd = 0.5, xlim = xrange, xaxt = "n", xaxs = "i", add = TRUE)
abline(v = xlabelsage, lty = 3, col = "gray70")
mtext(bquote(paste("<")~frac(density, grain~size)~paste(">")[sl]), side = 2, line = 3, cex = cex.mtext)
mtext("(e)", side = 3, line = -0.5, at = xrange[1]-7, cex = cex.mtext+0.3)

## PU
par(mar = c(0.5, 10.1, 0.5, 1.1))
pu_poor <- lapply(xdate, function(dt) vd$poorWKL$pu[vd$poorWKL$vdate == dt])
# lvar <- extractIndex(vdat$captWKL, wkl_uuid, vf$vf_uuid, sort(unique(vf$vdate)), "pu", max)
# names(lvar) <- sort(unique(vf$age))
# beeswarm::bxplot(pu[sapply(pu, is.numeric)], at = as.numeric(names(pu)[sapply(pu, is.numeric)]), col = "gray", ylab = "p_unstable", xlim = xrange, ylim = c(0, 1), yaxt = "n", xaxt = "n")
beeswarm::beeswarm(pu_poor[sapply(pu_poor, function(el) length(el) > 0)], at = xage[sapply(pu_poor, function(el) length(el) > 0)], method = "center", corral = "wrap", pwcol = gcol_poor[sapply(pu_poor, function(el) length(el) > 0)], pch = 19, cex = 0.5, ylab = "", xlim = xrange, ylim = c(0, 1), yaxt = "n", xaxt = "n", xaxs = "i")
abline(h = 0.77, col = "black", lty = "dotted")
## need pu: defined before rhogs!
vioplot::vioplot(pu[sapply(pu, function(dv) length(dv) > 0 & !all(is.na(dv)))], at = as.numeric(names(pu)[sapply(pu, function(dv) length(dv) > 0 & !all(is.na(dv)))]), col = rgb(0, 0, 0, box_alpha), wex = vf$dist[entity][k][sapply(pu, function(dv) length(dv) > 0 & !all(is.na(dv)))], lwd = 0.5, add = TRUE)
# beeswarm::beeswarm(pu[sapply(pu, is.numeric)], at = as.numeric(names(pu)[sapply(pu, is.numeric)]), method = "center", corral = "wrap", col = rgb(0, 0, 0, box_alpha), pch = 19, cex = 0.5, add = TRUE)
axis(2, at = c(0, 0.5, 1), labels = TRUE)
abline(v = xlabelsage, lty = 3, col = "gray70")
mtext(bquote(p[unstable]), side = 2, line = 4, cex = cex.mtext)
mtext("(f)", side = 3, line = -0.5, at = xrange[1]-7, cex = cex.mtext+0.3)

## RC
par(mar = c(0.5, 10.1, 0.5, 1.1))
lvar_name <- "rc"
lvar_ylim <- c(0, 1)
lvar_poor <- lapply(xdate, function(dt) vd$poorWKL[vd$poorWKL$vdate == dt, lvar_name])
lvar <- extractIndex(vd$captWKL, wkl_uuid, vf$vf_uuid, sort(unique(vf$vdate)), lvar_name, min)
names(lvar) <- sort(unique(vf$age))
beeswarm::beeswarm(lvar_poor[sapply(lvar_poor, function(el) length(el) > 0)], at = xage[sapply(lvar_poor, function(el) length(el) > 0)], method = "center", corral = "wrap", pwcol = gcol_poor[sapply(lvar_poor, function(el) length(el) > 0)], pch = 19, cex = 0.5, ylab = "", xlim = xrange, ylim = lvar_ylim, xaxt = "n", xaxs = "i")
vioplot::vioplot(lvar[sapply(lvar, function(dv) length(dv) > 0 & !all(is.na(dv)))], at = as.numeric(names(pu)[sapply(lvar, function(dv) length(dv) > 0 & !all(is.na(dv)))]), col = rgb(0, 0, 0, box_alpha), wex = vf$dist[entity][k][sapply(lvar, function(dv) length(dv) > 0 & !all(is.na(dv)))], lwd = 0.5, xlim = xrange, xaxt = "n", xaxs = "i", add = TRUE)
abline(h = 0.3, col = "black", lty = "dotted")
abline(v = xlabelsage, lty = 3, col = "gray70")
mtext(bquote(r[c]~(m)), side = 2, line = 4, cex = cex.mtext)
mtext("(g)", side = 3, line = -0.5, at = xrange[1]-7, cex = cex.mtext+0.3)

## SK38
par(mar = c(0.5, 10.1, 0.5, 1.1))
lvar_name <- "sk38"
lvar_ylim <- c(0, 6)
lvar_poor <- lapply(xdate, function(dt) vd$poorWKL[vd$poorWKL$vdate == dt, lvar_name])
lvar <- extractIndex(vd$captWKL, wkl_uuid, vf$vf_uuid, sort(unique(vf$vdate)), lvar_name, max)
names(lvar) <- sort(unique(vf$age))
beeswarm::beeswarm(lvar_poor[sapply(lvar_poor, function(el) length(el) > 0)], at = xage[sapply(lvar_poor, function(el) length(el) > 0)], method = "center", corral = "wrap", pwcol = gcol_poor[sapply(lvar_poor, function(el) length(el) > 0)], pch = 19, cex = 0.5, ylab = "", xlim = xrange, ylim = lvar_ylim, xaxt = "n", xaxs = "i")
vioplot::vioplot(lvar[sapply(lvar, function(dv) length(dv) > 0 & !all(is.na(dv)))], at = as.numeric(names(pu)[sapply(lvar, function(dv) length(dv) > 0 & !all(is.na(dv)))]), col = rgb(0, 0, 0, box_alpha), wex = vf$dist[entity][k][sapply(lvar, function(dv) length(dv) > 0 & !all(is.na(dv)))], lwd = 0.5, xlim = xrange, xaxt = "n", xaxs = "i", add = TRUE)
abline(h = 1, col = "black", lty = "dotted")
abline(v = xlabelsage, lty = 3, col = "gray70")
mtext("SK38", side = 2, line = 4, cex = cex.mtext)
mtext("(h)", side = 3, line = -0.5, at = xrange[1]-7, cex = cex.mtext+0.3)

## RTA
par(mar = c(7.1, 10.1, 0.5, 1.1))
lvar_name <- "rta"
lvar_ylim <- c(0, 1)
lvar_poor <- lapply(xdate, function(dt) vd$poorWKL[vd$poorWKL$vdate == dt, lvar_name])
lvar <- extractIndex(vd$captWKL, wkl_uuid, vf$vf_uuid, sort(unique(vf$vdate)), lvar_name, max)
names(lvar) <- sort(unique(vf$age))
beeswarm::beeswarm(lvar_poor[sapply(lvar_poor, function(el) length(el) > 0)], at = xage[sapply(lvar_poor, function(el) length(el) > 0)], method = "center", corral = "wrap", pwcol = gcol_poor[sapply(lvar_poor, function(el) length(el) > 0)], pch = 19, cex = 0.5, ylab = "", xlim = xrange, ylim = lvar_ylim, yaxt = "n", xaxt = "n", xaxs = "i")
vioplot::vioplot(lvar[sapply(lvar, function(dv) length(dv) > 0 & !all(is.na(dv)))], at = as.numeric(names(pu)[sapply(lvar, function(dv) length(dv) > 0 & !all(is.na(dv)))]), col = rgb(0, 0, 0, box_alpha), wex = vf$dist[entity][k][sapply(lvar, function(dv) length(dv) > 0 & !all(is.na(dv)))], lwd = 0.5, xlim = xrange, xaxt = "n", xaxs = "i", add = TRUE)
axis(2, at = c(0, 0.5, 1), labels = TRUE)
axis(1, at = xlabelsage, line = 1)
abline(h = 0.8, col = "black", lty = "dotted")
abline(v = xlabelsage, lty = 3, col = "gray70")
mtext("RTA", side = 2, line = 4, cex = cex.mtext)
mtext(xlabelsdate, side = 1, at = xlabelsage, las = 1, line = 3.5, cex = cex.mtext-0.2)
mtext("Days since burial", side = 1, at = xage[length(xage)/2], las = 1, line = 5.5, cex = cex.mtext)
mtext("(i)", side = 3, line = -0.5, at = xrange[1]-7, cex = cex.mtext+0.3)


dev.off()
#######################################
## --- study area map ----

library(sp)
library(raster)
library(sf)

## Forecast regions
## old regions: https://github.com/avalanche-canada/forecast-polygons/blob/main/editting_tools/reference_regions.kml
## current regions: https://github.com/avalanche-canada/forecast-polygons/blob/main/canadian_subregions.geojson
# fx <- readRDS()
reg <- read_sf("data-raw/reference_regions.kml")
fx <- reg$geometry

## Basemap
# load(url("http://data.avalancheresearch.ca/gmted_mea300_BCAB.rda"))
# saveRDS(DEM.USGS.GT30.BCAB, file = "data-raw/basemap.rds")
# basemap <- DEM.USGS.GT30.BCAB
basemap <- readRDS("data-raw/basemap.rds")
basemap <- extend(basemap, extent(reg))
crop_extent <- c(-133.3, -113.6, 48, 58.2)  # 52.5
basemap <- crop(basemap, extent(crop_extent))

crop_extent_s2s <- c(-124.2, -121.98, 49.57, 50.55)
basemap_s2s <- crop(basemap, extent(crop_extent_s2s))
crop_extent_gnp <- c(-118, -117.1, 50.76, 51.77)  # 50.96, 51.57
basemap_gnp <- crop(basemap, extent(crop_extent_gnp))
crop_extent_byk <- c(-116.68, -115.4, 50.84, 51.86)
basemap_byk <- crop(basemap, extent(crop_extent_byk))


## Borders
# borders <- getData('GADM', country = 'CAN', level = 1, path = tempdir())
# borders <- borders[borders$NAME_1 %in% c('British Columbia', 'Alberta', 'Yukon'),]
# saveRDS(borders, file = "data-raw/borders.rds")
borders <- readRDS("data-raw/borders.rds")
borders <- crop(borders, extent(crop_extent))

## model grid points and elev bands
sa <- readRDS("snowpacksimulations/sa_sts_gnp_byk.rds")
sa$HRDPS$band <- "TL"
sa$HRDPS$band[sa$HRDPS$region == "GLACIER" &sa$HRDPS$elev < 1800] <- "BTL"
sa$HRDPS$band[sa$HRDPS$region == "GLACIER" &sa$HRDPS$elev >= 2100] <- "ALP"
sa$HRDPS$band[sa$HRDPS$region == "SEA_TO_SKY" &sa$HRDPS$elev < 1600] <- "BTL"
sa$HRDPS$band[sa$HRDPS$region == "SEA_TO_SKY" &sa$HRDPS$elev >= 1800] <- "ALP"
sa$HRDPS$band[sa$HRDPS$region == "BANFF_YOHO_KOOTENAY" &sa$HRDPS$elev < 2000] <- "BTL"
sa$HRDPS$band[sa$HRDPS$region == "BANFF_YOHO_KOOTENAY" &sa$HRDPS$elev >= 2400] <- "ALP"

## Map settings and functions
plot_outlines <- function() {
  plot(borders, border = grey(0.7), add = T, lwd = 1)
  plot(fx, border = "gray10", add = T, lwd = 2)
}
add_latlon <- function() {
  abline(h = c(50,55,60), v = c(-110,-120,-130), lty = 3, lwd = 2, col = grey(0.6))
  mtext(paste0(c(50,55,60), '° N'), side = 4, at = c(50,55,60),  adj = 1.1,  col = grey(0.4), font = 3, las = T, cex = cex.legend - 0.4)
  mtext(paste0(c(120,130), '° W'),  side = 1, at = c(-120,-130), padj = -2, col = grey(0.4), font = 3, las = T, cex = cex.legend - 0.4)
}
plot_basemap <- function(x = basemap, box = TRUE, outline = TRUE, add_latlon = TRUE, ...) {
  plot(x, xlim = crop_extent[1:2]+c(-0, +0.5), ylim = crop_extent[3:4]+c(-3, +3),
       col = rev(grey(0:100/100, alpha = 0.5)),
       bty = 'n', xaxt = 'n', yaxt = 'n', box = F, legend = F, adj = 0, cex.main = 1, ...)
  if (outline) plot_outlines()
  if (add_latlon) add_latlon()
  if (box) box()
}
label_map <- function() {
  text(-115.5, 58, '^', cex = cex.legend)
  segments(-115.5, 57.2, -115.5, 58, lwd = 2)
  text(-115.5, 57.1, 'N', pos = 1, cex = cex.legend)
  segments(-115, 56.58, -116, 56.58, lwd = 2)
  text(-115.5, 56.6, '100 km', pos = 1, cex = cex.legend-0.4)
}
# col_band <- list(BTL = "#b2df8a", TL = "#33a02c", ALP = "#a6cee3")
col_band <- list(BTL = "#2ca25f", TL = "#006d2c", ALP = "#2b8cbe") #"#bdd7e7")

## Plot
png("output/figures/paper/studyAreaMap.png", width = 800, height = 800*0.75)
cex.legend = 2
plot_basemap(main = '')
# plot(sa$HRDPS, col = as.character(col_band[sa$HRDPS$band]), border = NA, add = T)
plot(sa$HRDPS[sa$HRDPS$band %in% "BTL", ], col = as.character(col_band["BTL"]), border = NA, add = T, pch = "18", cex = 0.5)
plot(sa$HRDPS[sa$HRDPS$band %in% "TL", ], col = as.character(col_band["TL"]), border = NA, add = T, pch = "18", cex = 0.5)
plot(sa$HRDPS[sa$HRDPS$band %in% "ALP", ], col = as.character(col_band["ALP"]), border = NA, add = T, pch = "18", cex = 0.5)
legend('bottomleft', legend = c("ALP", "TL", "BTL"), title = 'Elevation band',
       bty = 'n', pch = 20, pt.cex = 4, col = rev(as.character(col_band)), cex = cex.legend)
label_map()
dev.off()

## BYK
png("output/figures/paper/studyAreaMapBYK.png", width = 800, height = 800*0.75)
par(mar=c(0, 0, 0, 0))
plot_basemap(basemap_byk, main = '', box = FALSE, outline = FALSE, add_latlon = FALSE)
plot(fx[4], border = "gray10", add = T, lwd = 2)
plot(sa$HRDPS, col = as.character(col_band[sa$HRDPS$band]), border = NA, add = T, pch = 19, cex = 2.2)
dev.off()
## S2S
png("output/figures/paper/studyAreaMapS2S.png", width = 800, height = 800*0.75)
par(mar=c(0, 0, 0, 0))
plot_basemap(basemap_s2s, main = '', box = FALSE, outline = FALSE, add_latlon = FALSE)
plot(fx[23], border = "gray10", add = T, lwd = 2)
plot(sa$HRDPS, col = as.character(col_band[sa$HRDPS$band]), border = NA, add = T, pch = 19, cex = 2.1)
dev.off()
## GNP
png("output/figures/paper/studyAreaMapGNP.png", width = 800, height = 800*0.75)
par(mar= c(0, 0, 0, 0))
plot_basemap(basemap_gnp, main = '', box = FALSE, outline = FALSE, add_latlon = FALSE)
plot(fx[22], border = "gray10", add = T, lwd = 2)
plot(sa$HRDPS, col = as.character(col_band[sa$HRDPS$band]), border = NA, add = T, pch = 19, cex = 2.5)
dev.off()
