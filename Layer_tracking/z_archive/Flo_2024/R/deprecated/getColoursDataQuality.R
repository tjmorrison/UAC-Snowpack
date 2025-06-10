#' Get colors for WKL data quality
#'
#' For dataquality scale that is specific to this project
#'
#' @export
getColoursDataQuality <- function(Values, bw = TRUE) {


  dqfac <- factor(c(1, 2.1, 2.2, 2.3, 2, 3.1, 3.2, 3.3, 3, 4), levels = c(1, 2.1, 2.2, 2.3, 2, 3.1, 3.2, 3.3, 3, 4))
  if (bw) {
    # bwPal <- RColorBrewer::brewer.pal(4, "Greys")
    bwPal <- c("#000000", "#525252", "#969696", "#d9d9d9")
    dqcols <- data.frame(dq = dqfac,
                         # col = c("#7570b3",
                         #         shades::gradient(c("#e41a1c", shades::saturation("#e41a1c", 0.4)), steps = 3), shades::saturation("#e41a1c", 0.2),
                         #         shades::gradient(c("#e6ab02", shades::saturation("#e6ab02", 0.4)), steps = 3), shades::saturation("#e6ab02", 0.2),
                         #         "#1b9e77")
                         col = c(bwPal[1], rep(bwPal[2], 4), rep(bwPal[3], 4), bwPal[4])
                         )
  } else {
    dqcols <- data.frame(dq = dqfac,
                         col = c("#7570b3",
                                 shades::gradient(c("#e41a1c", shades::saturation("#e41a1c", 0.4)), steps = 3), shades::saturation("#e41a1c", 0.2),
                                 shades::gradient(c("#e6ab02", shades::saturation("#e6ab02", 0.4)), steps = 3), shades::saturation("#e6ab02", 0.2),
                                 "#1b9e77"
                         ))
  }

  Cols <- sapply(Values, function(dqval) dqcols$col[dqcols$dq == dqval])

  return(Cols)

}
