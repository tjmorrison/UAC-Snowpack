#' Derive all potential date tags of a season from modeled weather data
#'
#' These model-derived date tags represent regional markers for times when potential critical avalanche layers got buried.
#' Analogously to human date tags, these regional markers are primarily represented by the onset dates of pronounced storms
#' with substantial snowfall, but these markers can also be dates when little but consistent snowfall amounts buried the snow surface.
#'
#' To establish the model-derived date tags, this function first searches for dry periods by identifying all days with less than a trace amount
#' of snowfall or rain in the provided weather data. The actual model-derived date tags are defined by the start of substantial storm
#' periods following dry periods. However, since potentially critical layers can also be buried by small amounts of snow over
#' multiple days, the function inserts additional date tags in between these main storm periods if the non-storm periods were sufficiently
#' long and characterized by substantial accumulations of small daily snowfall amounts. The exact rules and thresholds for establishing
#' the model-derived date tags are described in detail in the appendix of the paper, or can be viewed directly in the source code below.
#'
#' @param WX24 data frame containing hn24, hn72, and rain24, rain72 for all days of the season and aggregated over all grid points.
#' hn24 and rain24 need to be provided as 75th percentiles, and hn72 and rain72 as 25th percentiles.
#' @param humanDatetags character or Date vector with date tags of layers of human concern
#'
#' @return an array of potential datetags as Dates
#'
#' @examples
#'
#' region <- "GNP"
#' season <- 2022
#' PDT <- derivePotentialDatetags(WX24)
#' par(mfrow = c(2, 1))
#' plot(WX24$date, WX24$hn72, type = "l", col = "red", ylim = c(0, max(WX24$hn24, WX24$hn72, 10.5)))
#' lines(WX24$date, WX24$hn24)
#' abline(v = PDT, col = "gray70", lty = "dotted")
#' abline(h = 10, col = "gray70", lty = "dotted")
#' legend("topright", c("hn72", "hn24"), col = c("red", "black"), lty = c("solid", "solid"), bty = "n")
#' mtext(paste(region, season), line = 1)
#' plot(WX24$date, WX24$rain72, type = "l", col = "red", ylim = c(0, max(WX24$rain72, WX24$rain24, 5.5)))
#' lines(WX24$date, WX24$rain24)
#' abline(v = PDT, col = "gray70", lty = "dotted")
#' abline(h = 5, col = "gray70", lty = "dotted")
#' legend("topright", c("rain72", "rain24"), col = c("red", "black"), lty = c("solid", "solid"), bty = "n")
#'
#'
#' @export
derivePotentialDatetags <- function(WX24, humanDatetags = NA) {
  WX24 <- WX24[order(WX24$date), ]

  ## --- snowfall ----
  ## (1) base rules & thresholds
  r1_snow <- which(WX24$hn24 < 2)
  r1_snow_date <- WX24$date[r1_snow]
  r23bol <- sapply(seq_along(r1_snow), function(i) {
    any(WX24$hn72[seq(r1_snow[i]+1, min(r1_snow[i]+5, r1_snow[i+1]-1, na.rm = TRUE))] > 10) &
      any(WX24$hn72[seq(r1_snow[i]+1, min(r1_snow[i]+5, r1_snow[i+1]-1, na.rm = TRUE))] > WX24$hn72[r1_snow[i]] + 5)
  })
  r23bol <- r23bol[!is.na(r23bol)]
  rSnow <- r1_snow[r23bol]
  basetagsSnow <- as.Date(WX24$date[rSnow])
  ## remove base tags when two tags too close and HN24 sum between them are below 10 cm
  if (length(basetagsSnow) > 1) {
    nDays_between <- diff(basetagsSnow)
    hn24sum_between <- sapply(seq_along(nDays_between), function(j) {
      sum(WX24$hn24[WX24$date > basetagsSnow[j] & WX24$date < basetagsSnow[j+1]])
    })
    to_remove <- which(nDays_between < 5 & hn24sum_between < 10)
    if (length(to_remove) > 0) basetagsSnow <- basetagsSnow[-to_remove]
  }
  ## combine with human tags
  tagsSnow <- sort(c(basetagsSnow, as.Date(humanDatetags)), na.last = NA)
  # tagsSnow <- tagsSnow[!is.na(tagsSnow)]  #done by 'sort' one line above
  ## (2) insert datetag at very end of time series that represents all shallow WKLs at that time
  tagsSnow <- c(tagsSnow, max(WX24$date))
  ## (3) insert more datetags where they are too far apart and little snow amounts in between accumulate
  ## don't count from tag to tag, but from storm's end of the first tag up to the next tag
  nDays_between <- diff(tagsSnow)
  stormEnd <- as.Date(sapply(seq_along(nDays_between), function(j) {
    ## first day of r1_snow after datetag, but at max 7 days later
    # as.character(min(r1_snow_date[r1_snow_date > tagsSnow[j]][1], tagsSnow[j]+7))
    ## first day after max hn24 within first week after datetag that falls below 20% of max hn24
    maxhn24idx <- which.max(WX24$hn24[WX24$date > tagsSnow[j] & WX24$date < tagsSnow[j+1]])
    maxhn24day <- WX24$date[WX24$date > tagsSnow[j] & WX24$date < tagsSnow[j+1]][maxhn24idx]
    as.character(min(WX24$date[WX24$date > maxhn24day & WX24$date < tagsSnow[j+1] & WX24$hn24 < WX24$hn24[WX24$date == maxhn24day]*0.2], tagsSnow[j]+7, na.rm = TRUE))
  }))
  hn24cum_between <- sapply(seq_along(nDays_between), function(j) {
    sum(WX24$hn24[WX24$date > stormEnd[j] & WX24$date < tagsSnow[j+1]])
  })
  gapThreshold_days <- 10
  gapThreshold_hnsum <- 25
  gaps <- which(nDays_between > gapThreshold_days & hn24cum_between > gapThreshold_hnsum)
  fillerTags <- as.Date(unlist(lapply(gaps, function(gap) {
    nfillerWindows <- min(round(hn24cum_between[gap]/gapThreshold_hnsum), round(nDays_between[gap]/gapThreshold_days), na.rm = TRUE)
    if (nfillerWindows > 1) {
      hn24 <- WX24$hn24[WX24$date > stormEnd[gap] & WX24$date < tagsSnow[gap+1]]
      dt <- WX24$date[WX24$date > stormEnd[gap] & WX24$date < tagsSnow[gap+1]]
      # r1 <- which(hn24 < 2 & c(hn24[2:length(hn24)], 0) > 2)  # days of hn24 below 2cm followed by a day above 2cm
      r1 <- which(diff(c(FALSE,diff(hn24)>0,TRUE))>0)  # finds all local minima
      r1 <- r1[!r1 %in% c(1, length(hn24))]  # exclude first and last day of gap from possible options if they are minima
      r1 <- r1[cumsum(hn24)[r1-1] > 10 & rev(cumsum(rev(hn24)))[r1+1] > 10]  # enforces min 10cm hn24 sum to outer basetags
      sapply(seq(nfillerWindows-1), function(j) {
        (cost_days <- r1 - (j*nDays_between[gap]/(nfillerWindows)))
        (cost_hn <- cumsum(hn24)[r1] - (j*hn24cum_between[gap]/(nfillerWindows)))
        (cost <- cost_days + cost_hn)
        as.character(dt[r1[which.min(abs(cost))]])
      })
    }
  })))
  tagsSnow <- sort(c(tagsSnow, fillerTags))

  ## rain (same rules as above, adjusted thresholds)
  r1_rain <- which(WX24$rain24 < 5)
  r23bol <- sapply(seq_along(r1_rain), function(i) {
    any(WX24$rain72[seq(r1_rain[i]+1, min(r1_rain[i]+5, r1_rain[i+1]-1, na.rm = TRUE))] > 5) &
      any(WX24$rain72[seq(r1_rain[i]+1, min(r1_rain[i]+5, r1_rain[i+1]-1, na.rm = TRUE))] > WX24$rain72[r1_rain[i]] + 2)
  })
  r23bol <- r23bol[!is.na(r23bol)]
  rRain <- r1_rain[r23bol]
  tagsRain <- as.Date(WX24$date[rRain])

  ## return corresponding dates
  out <- as.Date(sort(c(tagsSnow, tagsRain), na.last = NA))
  return(out)
}
