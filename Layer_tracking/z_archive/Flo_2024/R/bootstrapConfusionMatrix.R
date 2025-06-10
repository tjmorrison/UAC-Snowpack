## This script contains several function that are used to compute the confidence bands
## around the performance curves and around the skill scores.
## The functions to compute the skill scores are at the bottom of this script.

#' Compute confidence bands by bootstrapping
#' @param cm confusion matrix data frame
#' @param perf performance object (obtained from `ROCR::performance`, only required for ROC and precision-recall confidence bands!)
#' @param nboots number of bootstrap replications
#' @param target compute confidence bands for performance curves or skill scores?
#' @return data frame with confidence bands
#' @export
bootstrapConfusionMatrix <- function(cm, perf = NULL, nboots = 4001, target = c("attributes", "scores")[1]) {

  ## bootstrap:
  ## Note, the function that computes the bootstrap statistics returns a stacked vector
  ## of cutoff--recall--precision--far. Check function below in this file.
  if (target %in% "attributes") {
    tmp <- boot::boot(cm, statistic = computePerformanceAttributesFromCM, R = nboots, stype = "i")
    resolution <- ncol(tmp$t)/4  # determine the length of each stacked vector
    ## retrieve cutoff vector:
    ## Note that it remains constant during bootstrapping, due to interpolation onto a
    ## fixed grid during `computePerformanceAttributesFromCM` to ensure ci's are averaged from fixed cutoff values
    cutoff <- tmp$t[1, 1:resolution]

    ## compute confidence intervalls using proper function
    ## (boot.ci is not vectorized, so loop over each bootstrapped statistic)
    cilist <- sapply(seq(resolution+1, 4*resolution), function(i) {
      tmpci <- boot::boot.ci(tmp, type = "norm", index = i)
      c(tmpci$normal[2], tmpci$normal[3])  # lower and upper ci bounds
    })
    ## retrieve and reassemble ci's for each bootstrapped statistic
    civec1 <- unlist(sapply(cilist, function(el) el[[1]]))  # lower bounds
    civec2 <- unlist(sapply(cilist, function(el) el[[2]]))  # upper bounds
    ci <- data.frame(cutoff = cutoff,
                     recall1 = civec1[(1):(1*resolution)],
                     recall2 = civec2[(1):(1*resolution)],
                     precision1 = civec1[(1*resolution+1):(2*resolution)],
                     precision2 = civec2[(1*resolution+1):(2*resolution)],
                     far1 = civec1[(2*resolution+1):(3*resolution)],
                     far2 = civec2[(2*resolution+1):(3*resolution)])
    ci <- ci[ci$cutoff > 0, ]  # cutoff == 0 is not meaningful in our context!

    ## compute confidence intervals alternatively using simple quantiles
    ## this is likely not statistically sound!!
    # recall <- tmp$t[, (resolution+1):(2*resolution)]
    # precision <- tmp$t[, (2*resolution+1):(3*resolution)]
    # far <- tmp$t[, (3*resolution+1):(4*resolution)]
    # ci_recall <- apply(recall, 2, quantile, c(0.05, 0.95))
    # ci_precision <- apply(precision, 2, quantile, c(0.05, 0.95))
    # ci_far <- apply(far, 2, quantile, c(0.05, 0.95))
    # ci <- data.frame(cutoff = cutoff,
    #                  recall1 = ci_recall[1, ], recall2 = ci_recall[2, ],
    #                  precision1 = ci_precision[1, ], precision2 = ci_precision[2, ],
    #                  far1 = ci_far[1, ], far2 = ci_far[2, ])
    # ci <- ci[ci$cutoff > 0, ]

    if (!all(is.null(perf))) {
      ## Precision-Recall curve
      if (perf@x.name == "Recall") {
        ## translate cutoff-centered ci's into recall-centered ci's
        ## for plotting in precision-recall curve (i.e., future x-axis)
        ci_out <- data.frame(recall = seq(min(ci$recall1), max(ci$recall2), length.out = 500))
        ci_out$precision1 <- suppressWarnings(sapply(ci_out$recall, function(rc) {
          min(ci$precision1[which(ci$recall1 <= rc & ci$recall2 >= rc)])
        }))
        ci_out$precision2 <- suppressWarnings(sapply(ci_out$recall, function(rc) {
          max(ci$precision2[which(ci$recall1 <= rc & ci$recall2 >= rc)])
        }))
        ci_out$far1 <- suppressWarnings(sapply(ci_out$recall, function(rc) {
          min(ci$far1[which(ci$recall1 <= rc & ci$recall2 >= rc)])
        }))
        ci_out$far2 <- suppressWarnings(sapply(ci_out$recall, function(rc) {
          max(ci$far2[which(ci$recall1 <= rc & ci$recall2 >= rc)])
        }))
        ci_out <- ci_out[!is.infinite(ci_out$precision1) & !is.infinite(ci_out$precision2), ]
        ## if a performance object was provided, limit the ci band to the min/max values of the corresponding performance object
        ci_out <- ci_out[ci_out$recall >= min(perf@x.values[[1]]) & ci_out$recall <= max(perf@x.values[[1]]), ]


      } else if (perf@x.name == "False positive rate") {  # ROC curve
        ## translate cutoff-centered ci's into far-centered ci's
        ## for plotting in roc curve (i.e., future x-axis)
        ci <- ci[!is.infinite(ci$far1) & !is.infinite(ci$far2), ]
        ci_out <- data.frame(far = seq(min(ci$far1, na.rm = TRUE), max(ci$far2, na.rm = TRUE), length.out = 500))
        ci_out$recall1 <- suppressWarnings(sapply(ci_out$far, function(rc) {
          min(ci$recall1[which(ci$far1 <= rc & ci$far2 >= rc)])
        }))
        ci_out$recall2 <- suppressWarnings(sapply(ci_out$far, function(rc) {
          max(ci$recall2[which(ci$far1 <= rc & ci$far2 >= rc)])
        }))
        ci_out <- ci_out[!is.infinite(ci_out$recall1) & !is.infinite(ci_out$recall2), ]
        ## limit the ci band to the min/max values of the corresponding performance object
        ci_out <- ci_out[ci_out$far >= min(perf@x.values[[1]]) & ci_out$far <= max(perf@x.values[[1]]), ]
      }

    } else {
      stop("No valid performance object provided. This is required for performance attributes' confidence intervals!")
    }
    return(ci_out)

  } else if (target %in% "scores") {
    tmp <- boot::boot(cm, statistic = computePerformanceScoresFromCM, R = nboots, stype = "i")
    resolution <- ncol(tmp$t)/3  # determine the length of each stacked vector
    ## retrieve cutoff vector:
    ## Note that it remains constant during bootstrapping, due to interpolation onto a
    ## fixed grid during `computePerformanceAttributesFromCM` to ensure ci's are averaged from fixed cutoff values
    cutoff <- tmp$t[1, 1:resolution]

    ## compute confidence intervalls using proper function
    ## (boot.ci is not vectorized, so loop over each bootstrapped statistic)
    cilist <- lapply(seq(resolution+1, 3*resolution), function(i) {
      tmpci <- boot::boot.ci(tmp, type = "norm", index = i)
      c(tmpci$normal[2], tmpci$normal[3])  # lower and upper ci bounds
    })
    ## retrieve and reassemble ci's for each bootstrapped statistic
    civec1 <- unlist(sapply(cilist, function(el) el[[1]]))  # lower bounds
    civec2 <- unlist(sapply(cilist, function(el) el[[2]]))  # upper bounds
    ci <- data.frame(cutoff = cutoff,
                     pss1 = civec1[(1):(1*resolution)],
                     pss2 = civec2[(1):(1*resolution)],
                     f11 = civec1[(1*resolution+1):(2*resolution)],
                     f12 = civec2[(1*resolution+1):(2*resolution)])
    ci <- ci[ci$cutoff > 0, ]  # cutoff == 0 is not meaningful in our context!

    ## keep cutoff-centered ci's for a future cutoff--pss curve
    ci_out <- ci[!is.na(ci$pss1) & !is.na(ci$pss2) & !is.na(ci$f11) & !is.na(ci$f12), c("cutoff", "pss1", "pss2", "f11", "f12")]

    return(ci_out)
  }
}


# plot(pr, colorize = TRUE, lwd = 3, colorkey.relwidth = 0.7, ylim = c(0, 1))
# polygon(c(ci_out$recall, rev(ci_out$recall)), c(ci_out$precision1, rev(ci_out$precision2)), col = "gray70", border = "gray70")
# plot(pr, colorize = TRUE, lwd = 3, colorkey.relwidth = 0.7, ylim = c(0, 1), add = TRUE)

#' Compute performance curves for bootstrap data subsets
#' i.e., helper function for bootstrapConfusionMatrix
#' @export
computePerformanceAttributesFromCM <- function(data, i) {

  ## compute precision-recall and ROC curves for bootstrapped samples given in indices i
  pred <- prediction(data$pu_max[i], data$forecasted[i])
  pr <- performance(pred, "prec", "rec")
  roc <- performance(pred, "tpr", "fpr")

  ## define a fixed grid of cutoff (=threshold) values from [1, 0] with length `resolution`
  resolution <- 500
  cutoff_out <- seq(1, 0, length.out = resolution)

  ## compute Peirce skill score
  ## interpolate performance measures onto fixed cutoff grid
  ## and stack all four vectors into one dimension for the return object
  out <- c(
    cutoff_out,
    approx(unlist(pr@alpha.values), unlist(pr@x.values), xout = cutoff_out)$y,  # recall
    approx(unlist(pr@alpha.values), unlist(pr@y.values), xout = cutoff_out)$y,  # precision
    approx(unlist(roc@alpha.values), unlist(roc@x.values), xout = cutoff_out)$y # 1-specificity = false alarm rate
  )
  return(out)
}

#' Compute skill scores for bootstrap data subsets
#' i.e., helper function for bootstrapConfusionMatrix
#' @export
computePerformanceScoresFromCM <- function(data, i = seq(data)) {

  ## define a fixed grid of cutoff (=threshold) values from [1, 0] with length `resolution`
  resolution <- 500
  cutoff_out <- seq(1, 0, length.out = resolution)

  ## compute skill scores
  pss <- sapply(cutoff_out, function(x) {calculatePeircreSkillScore(data$pu_max[i], data$forecasted[i], x)})
  f1 <- sapply(cutoff_out, function(x) {calculateF1Score(data$pu_max[i], data$forecasted[i], x)})

  ## stack all three vectors into one dimension for the return object
  out <- c(
    cutoff_out,
    pss,
    f1
  )

  return(out)
}



#' Compute Peirce skill score...
#' ...for a given cutoff-threshold.
#' @param sim vector of numeric values between [0, 1] (e.g., proportion of unstable grid points)
#' @param obs vector of logicals (TRUE/FALSE) stating whether the layer was observed (of concern) or not.
#' @param cutoff in percentage within (0, 1]
#' @return numeric value of skill score
#' @export
calculatePeircreSkillScore <- function(sim, obs, cutoff) {
  tp <- length(sim[sim >= cutoff & obs])
  fn <- length(sim[sim < cutoff & obs])
  fp <- length(sim[sim >= cutoff & !obs])
  tn <- length(sim[sim < cutoff & !obs])
  pss <- ((tp*tn) - (fp*fn)) / ((tp+fn) * (fp+tn))
  return(pss)
}

#' Compute F1 skill score...
#' ...for a given cutoff-threshold.
#' @param sim vector of numeric values between [0, 1] (e.g., proportion of unstable grid points)
#' @param obs vector of logicals (TRUE/FALSE) stating whether the layer was observed (of concern) or not.
#' @param cutoff in percentage within (0, 1]
#' @return numeric value of skill score
#' @export
calculateF1Score <- function(sim, obs, cutoff) {
  tp <- length(sim[sim >= cutoff & obs])
  fn <- length(sim[sim < cutoff & obs])
  fp <- length(sim[sim >= cutoff & !obs])
  f1 <- 2*tp / (2*tp + fp + fn)
  return(f1)
}
