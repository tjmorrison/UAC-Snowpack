## This script produces an overview figure of the WKL data base
## Fig. 5 in paper

library(sarp.2021.herla.snowprofilevalidation)
library(SarpBulletinTools)
library(shades)

seasons <- seq(2013, 2022)
regions <- c("S2S", "GNP", "BYK")

# VD <- getVdata(DB = "Snow_DB", captWKL = FALSE, poorWKL = FALSE, limitWKL2vframe = FALSE)  # retrieve from DB
VD <- readRDS("data/VData_WKL_GNP_BYK_S2S.rds")  # use saved data file
WKL <- VD$wkl[VD$wkl$season > min(seasons), ]
rm(VD)

## get bulletin data (from mergedVDvframe)
mergedVD <- readRDS("output/evaluate_main/S2S_BYK_GNP_v06/mergedVDvframe.rds")
mergedVF <- mergedVD$vframe
mergedVF$region <- sapply(mergedVF$exec_uuid, function(eid) mergedVD$executions$region[mergedVD$executions$exec_uuid == eid])
mergedVF$season <- SarpGeneral::deriveAvSeasonFromDate(mergedVF$vdate)
Bulletins <- readRDS(paste0("data/AllBulletins_2010To2022.rds"))

## --- each season per region----
png(filename = "output/figures/wklDB/all.png", width = 900, height = 1000)
par(cex = 1, cex.lab = 1.7, cex.axis = 1.5)
layout(matrix(seq(6), byrow = TRUE, ncol = 2), widths = rep(c(0.8, 0.2), 3))
for (region in regions) {

  ## FIRST: percentage of all problem days (d.persistent) that are associated with tracked WKL
  BullUUIDs <- suppressWarnings(Bulletins$Bulletins$BULLETIN_ID[Bulletins$Bulletins$SEASON %in% seasons & Bulletins$Bulletins$REGION  %in% RegionNamesList[[region]]])
  Bull <- extractFromBulletinObj(Bulletins, ByBulletinID = BullUUIDs)


  # browser()
  ## get AvProb data frame with one row per day only,
  dpsDays <- NULL
  for (band in c("Btl", "Tl", "Alp")) {
    PF <- merge(Bull$AvProblems[[band]], Bull$Bulletins)
    PF <- PF[PF$CHARACTER %in% c("Persistent Slabs", "Deep Persistent Slabs"), ]
    dpsDays_band <- zoo::as.Date(PF$PUBLISH_DATE)
    if (any(duplicated(dpsDays))) {
      drop <- unlist(lapply(dpsDays[duplicated(dpsDays_band)], function(dt) {
        dupls <- which(zoo::as.Date(PF$PUBLISH_DATE) == dt)
        dupls[-which.min(factor(PF$CHARACTER[dupls], levels = c("Persistent Slabs", "Deep Persistent Slabs")))]
      }))
      PF <- PF[-drop, ]
    }
    dpsDays_band <- zoo::as.Date(PF$PUBLISH_DATE)
    # browser()
    dpsDays <- as.Date(unique(c(dpsDays, dpsDays_band)))
  }

  bulletinDays_perSeason <- sapply(seasons, function(seas) {
    nrow(Bull$Bulletins[SarpGeneral::deriveAvSeasonFromDate(Bull$Bulletins$PUBLISH_DATE) == seas, ])
  })

  percentage_dpsDays_perSeason <- sapply(seasons, function(seas) {
      length(dpsDays[SarpGeneral::deriveAvSeasonFromDate(dpsDays) == seas]) / bulletinDays_perSeason[seasons == seas]
  })

  percentage_dpsDays_tracked_perSeason <- sapply(seasons, function(seas) {
    tmp_subset <- mergedVF[mergedVF$season == seas & !is.na(mergedVF$tmode) #& mergedVF$band == "TL"
                           & (!mergedVF$tmode == "Storm Slabs")
                           & mergedVF$region == region, ]
    length(unique(tmp_subset$vdate)) /
      bulletinDays_perSeason[seasons == seas]
  })

  ## SECOND: Number of WKLs per season
  wkl <- WKL[WKL$region == region, ]
  nWKL <- nrow(wkl)
  # dqfac <- factor(c(1, 2.1, 2.2, 2.3, 3.1, 3.2, 3.3, 4), levels = c(1, 2.1, 2.2, 2.3, 3.1, 3.2, 3.3, 4))
  dqfac <- factor(seq(4), levels = seq(4))
  WKL_annual <- lapply(seasons, function(seas) wkl[wkl$season == seas, ])
  nWKL_annual <- mean(sapply(WKL_annual, nrow))
  nWKL_annual_max <- max(sapply(WKL_annual, nrow))

  dqcols <- data.frame(dq = dqfac,
                       # col = c("#7570b3",
                       #         shades::gradient(c("#e41a1c", shades::saturation("#e41a1c", 0.4)), steps = 2), shades::saturation("#e41a1c", 0.2),
                       #         shades::gradient(c("#e6ab02", shades::saturation("#e6ab02", 0.4)), steps = 2), shades::saturation("#e6ab02", 0.2),
                       #         "#1b9e77"
                       # ))
                       # col = c("#7570b3",
                       #         shades::gradient(c("#e41a1c", shades::saturation("#e41a1c", 0.4)), steps = 1), shades::saturation("#e41a1c", 0.2),
                       #         c("gray20", "gray40", "gray60", "gray80", "gray95")
                       # ))
                       # col = c(as.character(shades::gradient(c("#e41a1c", shades::saturation("#e41a1c", 0.4)), steps = 2)), "gray70", "gray95"
                       col = c("#6e016b", "#8c6bb1", "gray70", "gray95")
                       )

  wkl$dq <- floor(wkl$dq)
  season_class <- t(table(wkl[, c("season", "dq")])[, as.character(sort(unique(wkl$dq)))])
  if (ncol(season_class) < 10) {
    season2add <- seasons[!seasons %in% wkl$season]
    wkl2add <- wkl[integer(), ]
    wkl2add[length(season2add), ] <- NA
    wkl2add$season <- season2add
    wkl2add$region <- region
    wkl <- rbind(wkl, wkl2add)
    season_class <- t(table(wkl[, c("season", "dq")])[, as.character(dqfac[dqfac %in% wkl$dq])])
  }

  ## THIRD: Seasonal averages
  ## global average classes 1--3:
  avg_global <- mean(colSums(season_class[seq(3), ]))

  ## season class wkl and cr
  season_class_wkl <- t(table(wkl[wkl$iscrust == 0, c("season", "dq")])[, as.character(sort(unique(wkl$dq[wkl$iscrust == 0])))])
  # avg_wkl <- rep(0, 3)
  # names(avg_wkl) <- seq(3)
  # avg_wkl_tmp <- apply(season_class_wkl, 1, mean)
  # avg_wkl[names(avg_wkl) %in% names(avg_wkl_tmp)] <- avg_wkl_tmp[which(names(avg_wkl) %in% names(avg_wkl_tmp))]

  season_class_cr <- t(table(wkl[wkl$iscrust == 1, c("season", "dq")])[, as.character(sort(unique(wkl$dq[wkl$iscrust == 1])))])
  # avg_cr <- rep(0, 3)
  # names(avg_cr) <- seq(3)
  # avg_cr_tmp <- apply(season_class_cr, 1, mean)
  # avg_cr[names(avg_cr) %in% names(avg_cr_tmp)] <- avg_cr_tmp[which(names(avg_cr) %in% names(avg_cr_tmp))]

  ## percentage wkl and cr:
  pct_wkl <- sum(colSums(season_class_wkl)) / (sum(colSums(season_class_wkl)) + sum(colSums(season_class_cr)))
  pct_cr <- sum(colSums(season_class_cr)) / (sum(colSums(season_class_wkl)) + sum(colSums(season_class_cr)))

  ## percentage dq 1--3:
  pct_dq_wkl <- rowSums(season_class_wkl[seq(3), ]) / sum(rowSums(season_class_wkl[seq(3), ]))
  pct_dq_cr <- rowSums(season_class_cr[seq(3), ]) / sum(rowSums(season_class_cr[seq(3), ]))

  class_tab <- matrix(c(avg_global*pct_wkl*pct_dq_wkl, avg_global*pct_cr*pct_dq_cr), ncol = 2)
  rownames(class_tab) <- seq(3)
  colnames(class_tab) <- c("wkl", "crust")
  class_tab <- as.table(class_tab)

  ## mean avy prob percentages:
  mean_percentage_dpsDays_tracked_perSeason <- mean(percentage_dpsDays_tracked_perSeason)
  if (region == "S2S") mean_percentage_dpsDays_tracked_perSeason <- mean(percentage_dpsDays_tracked_perSeason[seasons >= 2017])

  ## PLOT
  par(mar = c(4.1, 5.1, 3.1, 5.1))
  if (region == "S2S") {
    maintitle <- c("Layers of concern: ", "Seasonal average")
  } else {
    maintitle <- c("", "")
  }
  barplot(season_class, col = dqcols$col, space = c(0.1, 0.1), width = 0.9, xlim = c(0, 10), ylim = c(0, 12.5),
          main = "", xaxs = "i",
          ylab = "")
  grid(col = "gray60", nx = NA, ny = NULL)
  if (region == "S2S") {
    legend("topleft", horiz = TRUE, legend = c(dqfac), fill = c(dqcols$col), box.col = "transparent", bg = "white",
                              cex = 2, title = "Data quality class")
    legend("topright", horiz = FALSE, legend = c("persistent", "persistent & tracked"), lty = c("solid", "dashed"), pch = c(1, 19), box.col = "transparent", bg = "white", cex = 2, title = "Avalanche problem days (%)")
    # legend("topright", legend = rev(dqfac), fill = rev(dqcols$col), box.col = "transparent", bg = "white")
  }
  barplot(season_class, col = dqcols$col, add = TRUE, space = c(0.1, 0.1), width = 0.9)

  lines(seq_along(seasons)-0.5, (percentage_dpsDays_perSeason)*12, col = "black", lty = "solid")
  points(seq_along(seasons)-0.5, (percentage_dpsDays_perSeason)*12, col = "black", pch = 1)
  lines(seq_along(seasons)-0.5, percentage_dpsDays_tracked_perSeason*12, col = "black", lty = "dashed")  # "#e6ab02"
  points(seq_along(seasons)-0.5, percentage_dpsDays_tracked_perSeason*12, col = "black", pch = 19)
  axis(4, at = seq(0, 10, by = 2)*12/10, labels = seq(0, 100, by = 20))
  mtext("Number of tracked layers", side = 2, line = 3, cex = 1.3)
  mtext("Percentage (%)", side = 4, line = 3, cex = 1.3)
  mtext(paste0(maintitle[1], unique(wkl$region)), side = 3, line = 0, cex = 1.4)
  mtext(c("(a)", "(c)", "(e)")[which(regions %in% region)], side = 3, line = 1, at = 0, cex = 1.6)
  # for (j in seq_along(seasons)) {
  #   mtext(paste0(dpsDays_perSeason[j]), side = 3, line = 0, at = j)
  # }

  ## seasonal averages plot
  par(mar = c(4.1, 4.1, 3.1, 2.1))
  barplot(class_tab, col = dqcols$col[as.numeric(dimnames(class_tab)[[1]])], space = c(0.1, 0.1), width = 0.9, xlim = c(0, 2), ylim = c(0, 12.5),
          main = "", xaxs = "i",
          ylab = "")
  lines(seq(2)-0.5, rep(mean(percentage_dpsDays_perSeason), 2)*12, col = "black", lty = "solid")
  points(seq(2)-0.5, rep(mean(percentage_dpsDays_perSeason), 2)*12, col = "black", pch = 1)
  lines(seq(2)-0.5, rep(mean_percentage_dpsDays_tracked_perSeason, 2)*12, col = "black", lty = "dashed")  # "#e6ab02"
  points(seq(2)-0.5, rep(mean_percentage_dpsDays_tracked_perSeason, 2)*12, col = "black", pch = 19)
  grid(col = "gray60", nx = NA, ny = NULL)
  axis(4, at = seq(0, 10, by = 2)*12/10, labels = seq(0, 100, by = 20))
  mtext(paste0(maintitle[2]), side = 3, line = 0, cex = 1.4)
  mtext(c("(b)", "(d)", "(f)")[which(regions %in% region)], side = 3, line = 1, at = -0.4, cex = 1.6)
}
dev.off()

















