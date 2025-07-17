## This script analyzes the confusion matrix results by
## (1) marginal distributions
## (2) performance curves (ROC & precision--recall curves)


## --- initializations ----
library(sarp.2021.herla.snowprofilevalidation)
library(ROCR)
CM <- readRDS("output/evaluate_main/S2S_BYK_GNP_v06/confusionMatrix.rds")
VD <- readRDS("output/evaluate_main/S2S_BYK_GNP_v06/mergedVDvframe.rds")
WKL_SHonly <- readRDS("data/VData_WKL_GNP_BYK_S2S.rds")  # legacy, see format_confusionMatrix.R
CM <- format_confusionMatrix(CM, VD, WKL_SHonly)

CMpb1 <- readRDS("output/evaluate_main/S2S_BYK_GNP_v06_pbInst/confusionMatrix.rds")  # results for process-based instability with rc <= 0.4
VDpb1 <- readRDS("output/evaluate_main/S2S_BYK_GNP_v06_pbInst/mergedVDvframe.rds")
CMpb1 <- format_confusionMatrix(CMpb1, VDpb1, WKL_SHonly)

CMpb2 <- readRDS("output/evaluate_main/S2S_BYK_GNP_v06.2_pbInst/confusionMatrix.rds")  # results for process-based instability with rc <= 0.3
VDpb2 <- readRDS("output/evaluate_main/S2S_BYK_GNP_v06.2_pbInst/mergedVDvframe.rds")
CMpb2 <- format_confusionMatrix(CMpb2, VDpb2, WKL_SHonly)


## --- marginal distributions ----
## Fig. 6 in paper
png("output/figures/paper/distributionsPUMAX_ofConcern.png", width = 600, height = 400)
par(cex = 1.3)
CM_ <- CM
CM_ <- CM_[CM_$gtype_rank != "tertiary", ]  # & CM_$band == "TL"
CM_ofConcern <- CM_$forecasted
CM_$forecasted[CM_ofConcern] <- "YES"
CM_$forecasted[!CM_ofConcern] <- "NO"
CM_$forecasted <- factor(CM_$forecasted)
vioplot::vioplot(pu_max ~ forecasted, data = CM_, col = c("gray60", "red2"), xlab = "Of concern", ylab = "Max daily proportion unstable")
grid()
dev.off()


## --- performance curves ----
## --- stratify instability measures ----
## Fig 7 in paper
CM_ <- CM
CM_ <- CM_[CM_$gtype_rank != "tertiary", ]  # & CM_$band == "TL"
CMpb1_ <- CMpb1
CMpb1_ <- CMpb1_[CMpb1_$gtype_rank != "tertiary", ]  # & CMpb_$band == "TL"
CMpb2_ <- CMpb2
CMpb2_ <- CMpb2_[CMpb2_$gtype_rank != "tertiary", ]  # & CMpb_$band == "TL"
noSkill <- sum(CM_$forecasted)/sum(nrow(CM_))
noSkillpb <- sum(CMpb1_$forecasted, CMpb2_$forecasted)/sum(nrow(CMpb1_), nrow(CMpb2_))

## compute PR curves
pred <- prediction(CM_$pu_max, CM_$forecasted)
pr1 <- performance(pred, "prec", "rec")
pr1 <- format_ROCRPerformanceObject(pr1)
pred <- prediction(CMpb2_$pu_max, CMpb2_$forecasted)
pr2 <- performance(pred, "prec", "rec")
pr2 <- format_ROCRPerformanceObject(pr2)
pred <- prediction(CMpb1_$pu_max, CMpb1_$forecasted)
pr3 <- performance(pred, "prec", "rec")
pr3 <- format_ROCRPerformanceObject(pr3)
## compute confidence intervals
CI_pr1 <- bootstrapConfusionMatrix(CM_, perf = pr1, nboots = 4001)
CI_pr2 <- bootstrapConfusionMatrix(CMpb2_, perf = pr2, nboots = 4001)
CI_pr3 <- bootstrapConfusionMatrix(CMpb1_, perf = pr3, nboots = 4001)
## compute ROC curves
pred <- prediction(CM_$pu_max, CM_$forecasted)
roc1 <- performance(pred, "tpr", "fpr")
roc1 <- format_ROCRPerformanceObject(roc1)
pred <- prediction(CMpb2_$pu_max, CMpb2_$forecasted)
roc2 <- performance(pred, "tpr", "fpr")
roc2 <- format_ROCRPerformanceObject(roc2)
pred <- prediction(CMpb1_$pu_max, CMpb1_$forecasted)
roc3 <- performance(pred, "tpr", "fpr")
roc3 <- format_ROCRPerformanceObject(roc3)
## compute confidence intervals
CI_roc1 <- bootstrapConfusionMatrix(CM_, perf = roc1, nboots = 4001)
CI_roc2 <- bootstrapConfusionMatrix(CMpb2_, perf = roc2, nboots = 4001)
CI_roc3 <- bootstrapConfusionMatrix(CMpb1_, perf = roc3, nboots = 4001)


## plot
png("output/figures/paper/cm_stab.png", width = 700, height = 700)
cex.panel <- 1.6
par(cex = 1.5, cex.lab = 1.5, cex.axis = 1.3, mfrow = c(2, 2), mar = c(2.1, 4.1, 4.1, 1.1))
plot(pr1, colorize = TRUE, lwd = 3, colorkey.relwidth = 0.7, ylim = c(0, 1), xlim = c(0, 1), xaxs = "i", yaxs = "i", xlab = "", colorkey.pos = "top")
polygon(c(CI_pr1$recall, rev(CI_pr1$recall)), c(CI_pr1$precision1, rev(CI_pr1$precision2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")
## cutoff
cutoff <- 0.95
cutoff2 <- 0.2
if (!is.na(cutoff)) {
  recall_cutoff1a <- pr1@x.values[[1]][which.min(abs(pr1@alpha.values[[1]] - cutoff))]
  precision_cutoff1a <- pr1@y.values[[1]][which.min(abs(pr1@alpha.values[[1]] - cutoff))]
  recall_cutoff1b <- pr2@x.values[[1]][which.min(abs(pr2@alpha.values[[1]] - cutoff))]
  precision_cutoff1b <- pr2@y.values[[1]][which.min(abs(pr2@alpha.values[[1]] - cutoff))]
  points(recall_cutoff1a, precision_cutoff1a, cex = 2, col = "red", lwd = 2)
  lines(c(0, recall_cutoff1a), rep(precision_cutoff1a, 2), lty = "dotted", col = "red", lwd = 2)
  lines(rep(recall_cutoff1a, 2), c(0, precision_cutoff1a), lty = "dotted", col = "red", lwd = 2)
  # lines(c(recall_cutoff1a, recall_cutoff1b), c(precision_cutoff1a, precision_cutoff1b), lty = "dotted", col = "gray50", lwd = 2)
}
if (!is.na(cutoff2)) {
  recall_cutoff2a <- pr1@x.values[[1]][which.min(abs(pr1@alpha.values[[1]] - cutoff2))]
  precision_cutoff2a <- pr1@y.values[[1]][which.min(abs(pr1@alpha.values[[1]] - cutoff2))]
  recall_cutoff2b <- pr2@x.values[[1]][which.min(abs(pr2@alpha.values[[1]] - cutoff2))]
  precision_cutoff2b <- pr2@y.values[[1]][which.min(abs(pr2@alpha.values[[1]] - cutoff2))]
  points(recall_cutoff2a, precision_cutoff2a, cex = 2, col = "#34b0d4", lwd = 2)
  lines(c(0, recall_cutoff2a), rep(precision_cutoff2a, 2), lty = "dotted", col = "#34b0d4", lwd = 2)
  lines(rep(recall_cutoff2a, 2), c(0, precision_cutoff2a), lty = "dotted", col = "#34b0d4", lwd = 2)
  # lines(c(recall_cutoff2a, recall_cutoff2b), c(precision_cutoff2a, precision_cutoff2b), lty = "dotted", col = "black", lwd = 1)
}
plot(pr2, colorize = F, add = TRUE, lwd = 2, col = "gray70")
plot(pr3, colorize = F, add = TRUE, lwd = 2, col = "gray70")
lines(c(0, 1), c(noSkill, noSkill), lty = "dashed")  # no-skill curve
grid()
text(0.87, 0.95, bquote(bold(Statistical)), cex = 1.3)
mtext("Max daily proportion unstable", side = 3, line = 2.5, cex = 1.2)
mtext("(a)", line = 2, at = -0.12, cex = cex.panel)


## ROC curve 1
par(mar = c(2.1, 4.1, 4.1, 1.1))
## plot
plot(roc1, colorize = TRUE, lwd = 3, colorkey = FALSE, ylim = c(0, 1), xlim = c(0, 1), xaxs = "i", yaxs = "i", xlab = "", ylab = "Probability of detection (recall)")
polygon(c(CI_roc$far, rev(CI_roc$far)), c(CI_roc$recall1, rev(CI_roc$recall2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")
## cutoff
if (!is.na(cutoff)) {
  recall_cutoff <- roc1@y.values[[1]][which.min(abs(roc1@alpha.values[[1]] - cutoff))]
  far_cutoff <- roc1@x.values[[1]][which.min(abs(roc1@alpha.values[[1]] - cutoff))]
  points(far_cutoff, recall_cutoff, cex = 2, col = "red", lwd = 2)
  lines(rep(far_cutoff, 2), c(0, recall_cutoff), lty = "dotted", col = "red", lwd = 2)
  lines(c(0, far_cutoff), rep(recall_cutoff, 2), lty = "dotted", col = "red", lwd = 2)
}
if (!is.na(cutoff2)) {
  recall_cutoff <- roc1@y.values[[1]][which.min(abs(roc1@alpha.values[[1]] - cutoff2))]
  far_cutoff <- roc1@x.values[[1]][which.min(abs(roc1@alpha.values[[1]] - cutoff2))]
  points(far_cutoff, recall_cutoff, cex = 2, col = "#34b0d4", lwd = 2)
  lines(rep(far_cutoff, 2), c(0, recall_cutoff), lty = "dotted", col = "#34b0d4", lwd = 2)
  lines(c(0, far_cutoff), rep(recall_cutoff, 2), lty = "dotted", col = "#34b0d4", lwd = 2)
}
plot(roc2, colorize = F, add = TRUE, lwd = 2, col = "gray70")
plot(roc3, colorize = F, add = TRUE, lwd = 2, col = "gray70")
lines(c(0, 1), c(0, 1), lty = "dashed")  # no-skill curve
grid()
mtext("(b)", line = 2, at = -0.12, cex = cex.panel)



## PR curve 2
par(mar = c(5.1, 4.1, 2.1, 1.1))
## plot
plot(pr2, colorize = TRUE, lwd = 3, colorkey = FALSE, ylim = c(0, 1), xlim = c(0, 1), xaxs = "i", yaxs = "i", xlab = "Probability of detection (recall)")
polygon(c(CI_pr2$recall, rev(CI_pr2$recall)), c(CI_pr2$precision1, rev(CI_pr2$precision2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")
## cutoff
if (!is.na(cutoff)) {
  # recall_cutoff <- pr@x.values[[1]][which.min(abs(pr@alpha.values[[1]] - cutoff))]
  # precision_cutoff <- pr@y.values[[1]][which.min(abs(pr@alpha.values[[1]] - cutoff))]
  points(recall_cutoff1b, precision_cutoff1b, cex = 2, col = "red", lwd = 2)
  lines(c(0, recall_cutoff1b), rep(precision_cutoff1b, 2), lty = "dotted", col = "red", lwd = 2)
  lines(rep(recall_cutoff1b, 2), c(0, precision_cutoff1b), lty = "dotted", col = "red", lwd = 2)
}
if (!is.na(cutoff2)) {
  recall_cutoff <- pr2@x.values[[1]][which.min(abs(pr2@alpha.values[[1]] - cutoff2))]
  precision_cutoff <- pr2@y.values[[1]][which.min(abs(pr2@alpha.values[[1]] - cutoff2))]
  points(recall_cutoff, precision_cutoff, cex = 2, col = "#34b0d4", lwd = 2)
  lines(c(0, recall_cutoff), rep(precision_cutoff, 2), lty = "dotted", col = "#34b0d4", lwd = 2)
  lines(rep(recall_cutoff, 2), c(0, precision_cutoff), lty = "dotted", col = "#34b0d4", lwd = 2)
}
lines(c(0, 1), c(noSkillpb, noSkillpb), lty = "dashed")  # no-skill curve
polygon(c(CI_pr3$recall, rev(CI_pr3$recall)), c(CI_pr3$precision1, rev(CI_pr3$precision2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")
plot(pr3, colorize = TRUE, add = TRUE, lwd = 2)
plot(pr1, colorize = F, add = TRUE, lwd = 2, col = "gray70")
grid()
text(0.8, 0.95, bquote(bold(Process-based)), cex = 1.3)
text(0.38, 0.6, bquote(r[c] <= 0.3), cex = 1.2)
segments(0.32, 0.58, 0.23, 0.534)
text(0.23, 0.32, bquote(r[c] <= 0.4), cex = 1.2)
segments(0.23, 0.35, 0.2, 0.4)
mtext("(c)", line = 1, at = -0.12, cex = cex.panel)



## ROC curve 2
par(mar = c(5.1, 4.1, 2.1, 1.1))
## plot
plot(roc2, colorize = TRUE, lwd = 3, colorkey = FALSE, ylim = c(0, 1), xlim = c(0, 1), xaxs = "i", yaxs = "i", xlab = "False alarm rate", ylab = "Probability of detection (recall)")
# plot(roc, colorize = TRUE, add = TRUE, lwd = 1)
polygon(c(CI_roc2$far, rev(CI_roc2$far)), c(CI_roc2$recall1, rev(CI_roc2$recall2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")
polygon(c(CI_roc3$far, rev(CI_roc3$far)), c(CI_roc3$recall1, rev(CI_roc3$recall2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")
## cutoff
if (!is.na(cutoff)) {
  recall_cutoff <- roc2@y.values[[1]][which.min(abs(roc2@alpha.values[[1]] - cutoff))]
  far_cutoff <- roc2@x.values[[1]][which.min(abs(roc2@alpha.values[[1]] - cutoff))]
  points(far_cutoff, recall_cutoff, cex = 2, col = "red", lwd = 2)
  lines(rep(far_cutoff, 2), c(0, recall_cutoff), lty = "dotted", col = "red", lwd = 2)
  lines(c(0, far_cutoff), rep(recall_cutoff, 2), lty = "dotted", col = "red", lwd = 2)
}
if (!is.na(cutoff2)) {
  recall_cutoff <- roc2@y.values[[1]][which.min(abs(roc2@alpha.values[[1]] - cutoff2))]
  far_cutoff <- roc2@x.values[[1]][which.min(abs(roc2@alpha.values[[1]] - cutoff2))]
  points(far_cutoff, recall_cutoff, cex = 2, col = "#34b0d4", lwd = 2)
  lines(rep(far_cutoff, 2), c(0, recall_cutoff), lty = "dotted", col = "#34b0d4", lwd = 2)
  lines(c(0, far_cutoff), rep(recall_cutoff, 2), lty = "dotted", col = "#34b0d4", lwd = 2)
}
## plot
plot(roc3, colorize = TRUE, add = TRUE, lwd = 2)
plot(roc1, colorize = FALSE, add = TRUE, lwd = 2, col = "gray70")
lines(c(0, 1), c(0, 1), lty = "dashed")  # no-skill curve
grid()
mtext("(d)", line = 1, at = -0.12, cex = cex.panel)
dev.off()








## ---  Skill scores for overall performance results ----
## Fig 8 in paper
CM_ <- CM
CM_ <- CM_[CM_$gtype_rank != "tertiary", ]
CMpb2_ <- CMpb2
CMpb2_ <- CMpb2_[CMpb2_$gtype_rank != "tertiary", ]

cutoff <- seq(0.005, 1, len = 500)
pss <- sapply(cutoff, function(x) {calculatePeircreSkillScore(CM_$pu_max, CM_$forecasted, x)})
f1 <- sapply(cutoff, function(x) {calculateF1Score(CM_$pu_max, CM_$forecasted, x)})
CI_ss <- bootstrapConfusionMatrix(CM_, nboots = 4001, target = "scores")
pss_pb <- sapply(cutoff, function(x) {calculatePeircreSkillScore(CMpb2_$pu_max, CMpb2_$forecasted, x)})
f1_pb <- sapply(cutoff, function(x) {calculateF1Score(CMpb2_$pu_max, CMpb2_$forecasted, x)})
CI_ss_pb <- bootstrapConfusionMatrix(CMpb2_, nboots = 4001, target = "scores")

png("output/figures/paper/skillscores.png", width = 700, height = 300)
par(mfrow = c(1, 2), mar = c(5.1, 4.1, 1.1, 0), cex = 1.15)
pointCol <- rev(rainbow(256, start = 0, end = 4/6))[as.numeric(cut(cutoff, breaks = 256))]
## PSS
plot(cutoff, pss, type = "n", xaxs = "i", yaxs = "i",
     ylim = c(0, 1), ylab = "Skill score", xlab = "Maximum daily proportion unstable")
polygon(c(CI_ss$cutoff, rev(CI_ss$cutoff)), c(CI_ss$pss1, rev(CI_ss$pss2)), col = adjustcolor("gray40", alpha.f = 0.2) , border = "transparent")
polygon(c(CI_ss_pb$cutoff, rev(CI_ss_pb$cutoff)), c(CI_ss_pb$pss1, rev(CI_ss_pb$pss2)), col = adjustcolor("gray40", alpha.f = 0.2) , border = "transparent")
points(cutoff, pss, pch = 20, col = pointCol, cex = 0.5)
# points(cutoff[90], pss[90], cex = 2, col = "black")
points(cutoff, pss_pb, pch = 20, col = pointCol, cex = 0.3)
text(0.8, 0.39, "Statistical")
text(0.67, 0.1, "Process-based")
text(0.5, 0.95, bquote(bold(paste("Peirce skill score"))))
grid()
mtext("(a)", line = 0.1, at = -0.16, cex = cex.panel)
## F1
par(mar = c(5.1, 3.1, 1.1, 1.1))
plot(cutoff, pss_pb, type = "n", xaxs = "i", yaxs = "i",
     ylim = c(0, 1), ylab = "", xlab = "Maximum daily proportion unstable")
polygon(c(CI_ss$cutoff, rev(CI_ss$cutoff)), c(CI_ss$f11, rev(CI_ss$f12)), col = adjustcolor("gray40", alpha.f = 0.2) , border = "transparent")
polygon(c(CI_ss_pb$cutoff, rev(CI_ss_pb$cutoff)), c(CI_ss_pb$f11, rev(CI_ss_pb$f12)), col = adjustcolor("gray40", alpha.f = 0.2) , border = "transparent")
points(cutoff, f1, pch = 20, col = pointCol, cex = 0.5)
points(cutoff, f1_pb, pch = 20, col = pointCol, cex = 0.3)
text(0.8, 0.53, "Statistical")
text(0.67, 0.25, "Process-based")
text(0.5, 0.95, bquote(bold(F[1]~score)))
grid()
mtext("(b)", line = 0.1, at = -0.16, cex = cex.panel)
dev.off()





## --- stratify gtypes ----
## Fig 9 a, b in paper
png("output/figures/paper/cm_strat.png", width = 700, height = 1050)
cex.panel <- 1.6
cex.annot <- 1.43
par(cex = 1.5, cex.lab = 1.5, cex.axis = 1.3, mar = c(2.1, 4.1, 4.1, 1.1))
par(mfrow = c(3, 2))
## Precision-recall
## SH/DH/NA
CM_ <- CM
CM_ <- CM_[CM_$gtype_rank == "primary", ]
noSkill <- sum(CM_$forecasted)/nrow(CM_)  # valid for all bands
CM_ <- CM_[CM_$gtype_class %in% c("SH", "DH", "FC/SH", "SH/FC", NA), ]
pred <- prediction(CM_$pu_max, CM_$forecasted)
pr <- performance(pred, "prec", "rec")
pr <- format_ROCRPerformanceObject(pr)
CI_pr <- bootstrapConfusionMatrix(CM_, perf = pr, nboots = 400)
plot(pr, colorize = TRUE, lwd = 3, colorkey.relwidth = 0.5, ylim = c(0, 1.03), xlim = c(0, 1), xlab = "", colorkey.pos = "top", xaxs = "i", yaxs = "i")
polygon(c(CI_pr$recall, rev(CI_pr$recall)), c(CI_pr$precision1, rev(CI_pr$precision2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")
lines(c(0, 1), c(noSkill, noSkill), lty = "dashed")  # no-skill curve
grid()
mtext("(a)", line = 1.2, at = -0.08, cex = cex.panel)
mtext("Max daily proportion unstable", side = 3, line = 2.5, cex = 1.2)

## FC/DH/NA
CM_ <- CM
CM_ <- CM_[CM_$gtype_rank == "primary", ]
CM_ <- CM_[CM_$gtype_class %in% c("FC", "DH", "FC/SH", "SH/FC", NA), ]
pred <- prediction(CM_$pu_max, CM_$forecasted)
pr <- performance(pred, "prec", "rec")
pr <- format_ROCRPerformanceObject(pr)
CI_pr <- bootstrapConfusionMatrix(CM_, perf = pr, nboots = 400)
plot(pr, colorize = TRUE, add = TRUE, lwd = 2)
polygon(c(CI_pr$recall, rev(CI_pr$recall)), c(CI_pr$precision1, rev(CI_pr$precision2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")

## MFcr
CM_ <- CM
CM_ <- CM_[CM_$gtype_rank == "tertiary", ]
CM_ <- CM_[CM_$gtype_class %in% c("MFcr", "IFsc", "IFrc") | (CM_$gtype_class %in% "MFcr" & !CM_$forecasted), ]
pred <- prediction(CM_$pu_max, CM_$forecasted)
pr <- performance(pred, "prec", "rec")
pr <- format_ROCRPerformanceObject(pr)
# CI_pr <- bootstrapConfusionMatrix(CM_, perf = pr, nboots = 400)
plot(pr, colorize = TRUE, add = TRUE, lwd = 2)
# polygon(c(CI_pr$recall, rev(CI_pr$recall)), c(CI_pr$precision1, rev(CI_pr$precision2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")
text(0.3, 0.96, "SH", cex = cex.annot)
text(0.07, 0.83, "MFcr", cex = cex.annot)
segments(0.07, 0.87, 0.05, 0.97)
text(0.5, 0.43, "FC", cex = cex.annot)

## ROC
par(mar = c(2.1, 4.1, 4.1, 1.1))
## SH/DH/NA
CM_ <- CM
CM_ <- CM_[CM_$gtype_rank == "primary", ]
CM_ <- CM_[CM_$gtype_class %in% c("SH", "DH", "FC/SH", "SH/FC", NA), ]
pred <- prediction(CM_$pu_max, CM_$forecasted)
roc <- performance(pred, "tpr", "fpr")
roc <- format_ROCRPerformanceObject(roc)
CI_roc <- bootstrapConfusionMatrix(CM_, perf = roc, nboots = 400)
plot(roc, colorize = TRUE, lwd = 3, colorkey = FALSE, ylim = c(0, 1), xlim = c(-0.01, 1), xaxs = "i", yaxs = "i", xlab = "False alarm rate", ylab = "Probability of detection (recall)")
polygon(c(CI_roc$far, rev(CI_roc$far)), c(CI_roc$recall1, rev(CI_roc$recall2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")
lines(c(0, 1), c(0, 1), lty = "dashed")  # no-skill curve
grid()
mtext("(b)", line = 1.2, at = -0.08, cex = cex.panel)

## FC/DH/NA
CM_ <- CM
CM_ <- CM_[CM_$gtype_rank == "primary", ]
CM_ <- CM_[CM_$gtype_class %in% c("FC", "DH", "FC/SH", "SH/FC", NA), ]
pred <- prediction(CM_$pu_max, CM_$forecasted)
roc <- performance(pred, "tpr", "fpr")
roc <- format_ROCRPerformanceObject(roc)
CI_roc <- bootstrapConfusionMatrix(CM_, perf = roc, nboots = 400)
plot(roc, colorize = TRUE, add = TRUE, lwd = 2)
polygon(c(CI_roc$far, rev(CI_roc$far)), c(CI_roc$recall1, rev(CI_roc$recall2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")

## MFcr
CM_ <- CM
CM_ <- CM_[CM_$gtype_rank == "tertiary", ]
CM_ <- CM_[CM_$gtype_class %in% c("MFcr", "IFsc", "IFrc") | (CM_$gtype_class %in% "MFcr" & !CM_$forecasted), ]
pred <- prediction(CM_$pu_max, CM_$forecasted)
roc <- performance(pred, "tpr", "fpr")
roc <- format_ROCRPerformanceObject(roc)
# CI_roc <- bootstrapConfusionMatrix(CM_, perf = roc, nboots = 400)
plot(roc, colorize = TRUE, add = TRUE, lwd = 2)
# polygon(c(CI_roc$far, rev(CI_roc$far)), c(CI_roc$recall1, rev(CI_roc$recall2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")


## --- stratify bands ----
## Fig 9 c, d in paper
par(mar = c(2.1, 4.1, 4.1, 1.1))
## Precision-recall
## TL
CM_ <- CM
CM_ <- CM_[CM_$gtype_rank != "tertiary", ]
noSkill <- sum(CM_$forecasted)/nrow(CM_)  # valid for all bands
CM_ <- CM_[CM_$band == "TL", ]
pred <- prediction(CM_$pu_max, CM_$forecasted)
pr <- performance(pred, "prec", "rec")
pr <- format_ROCRPerformanceObject(pr)
CI_pr <- bootstrapConfusionMatrix(CM_, perf = pr, nboots = 400)
plot(pr, colorize = TRUE, lwd = 3, colorkey = FALSE, ylim = c(0, 1), xlim = c(0, 1), xlab = "", xaxs = "i", yaxs = "i")
polygon(c(CI_pr$recall, rev(CI_pr$recall)), c(CI_pr$precision1, rev(CI_pr$precision2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")
lines(c(0, 1), c(noSkill, noSkill), lty = "dashed")  # no-skill curve
grid()
mtext("(c)", line = 1.2, at = -0.08, cex = cex.panel)

## BTL
CM_ <- CM
CM_ <- CM_[CM_$gtype_rank != "tertiary", ]
CM_ <- CM_[CM_$band == "BTL", ]
pred <- prediction(CM_$pu_max, CM_$forecasted)
pr <- performance(pred, "prec", "rec")
pr <- format_ROCRPerformanceObject(pr)
CI_pr <- bootstrapConfusionMatrix(CM_, perf = pr, nboots = 400)
plot(pr, colorize = TRUE, add = TRUE, lwd = 2)
polygon(c(CI_pr$recall, rev(CI_pr$recall)), c(CI_pr$precision1, rev(CI_pr$precision2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")

## ALP
CM_ <- CM
CM_ <- CM_[CM_$gtype_rank != "tertiary", ]
CM_ <- CM_[CM_$band == "ALP", ]
pred <- prediction(CM_$pu_max, CM_$forecasted)
pr <- performance(pred, "prec", "rec")
pr <- format_ROCRPerformanceObject(pr)
CI_pr <- bootstrapConfusionMatrix(CM_, perf = pr, nboots = 400)
plot(pr, colorize = TRUE, add = TRUE, lwd = 2)
polygon(c(CI_pr$recall, rev(CI_pr$recall)), c(CI_pr$precision1, rev(CI_pr$precision2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")
text(0.45, 0.7, "TL", cex = cex.annot)
text(0.1, 0.6, "BTL", cex = cex.annot)
text(0.2, 0.38, "ALP", cex = cex.annot)

## ROC
par(mar = c(2.1, 4.1, 4.1, 1.1))
## TL
CM_ <- CM
CM_ <- CM_[CM_$gtype_rank != "tertiary", ]
CM_ <- CM_[CM_$band == "TL", ]
pred <- prediction(CM_$pu_max, CM_$forecasted)
roc <- performance(pred, "tpr", "fpr")
roc <- format_ROCRPerformanceObject(roc)
CI_roc <- bootstrapConfusionMatrix(CM_, perf = roc, nboots = 400)
plot(roc, colorize = TRUE, lwd = 3, colorkey = FALSE, ylim = c(0, 1), xlim = c(0, 1), xaxs = "i", yaxs = "i", xlab = "False alarm rate", ylab = "Probability of detection (recall)")
polygon(c(CI_roc$far, rev(CI_roc$far)), c(CI_roc$recall1, rev(CI_roc$recall2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")
lines(c(0, 1), c(0, 1), lty = "dashed")  # no-skill curve
grid()
mtext("(d)", line = 1.2, at = -0.08, cex = cex.panel)

## BTL
CM_ <- CM
CM_ <- CM_[CM_$gtype_rank != "tertiary", ]
CM_ <- CM_[CM_$band == "BTL", ]
pred <- prediction(CM_$pu_max, CM_$forecasted)
roc <- performance(pred, "tpr", "fpr")
roc <- format_ROCRPerformanceObject(roc)
CI_roc <- bootstrapConfusionMatrix(CM_, perf = roc, nboots = 400)
plot(roc, colorize = TRUE, add = TRUE, lwd = 2)
polygon(c(CI_roc$far, rev(CI_roc$far)), c(CI_roc$recall1, rev(CI_roc$recall2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")

## ALP
CM_ <- CM
CM_ <- CM_[CM_$gtype_rank != "tertiary", ]
CM_ <- CM_[CM_$band == "ALP", ]
pred <- prediction(CM_$pu_max, CM_$forecasted)
roc <- performance(pred, "tpr", "fpr")
roc <- format_ROCRPerformanceObject(roc)
CI_roc <- bootstrapConfusionMatrix(CM_, perf = roc, nboots = 400)
plot(roc, colorize = TRUE, add = TRUE, lwd = 2)
polygon(c(CI_roc$far, rev(CI_roc$far)), c(CI_roc$recall1, rev(CI_roc$recall2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")


## --- stratify regions ----
## Fig 9 e, f in paper
par(mar = c(5.1, 4.1, 4.1, 1.1))
## Precision-recall
## GNP
CM_ <- CM
CM_ <- CM_[CM_$gtype_rank != "tertiary", ]
noSkill <- sum(CM_$forecasted)/nrow(CM_)  # valid for all regions
CM_ <- CM_[CM_$region == "GNP", ]
pred <- prediction(CM_$pu_max, CM_$forecasted)
pr <- performance(pred, "prec", "rec")
pr <- format_ROCRPerformanceObject(pr)
CI_pr <- bootstrapConfusionMatrix(CM_, perf = pr, nboots = 400)
plot(pr, colorize = TRUE, lwd = 3, colorkey = FALSE, ylim = c(0, 1), xlim = c(0, 1), xlab = "Probability of detection (recall)", xaxs = "i", yaxs = "i")
polygon(c(CI_pr$recall, rev(CI_pr$recall)), c(CI_pr$precision1, rev(CI_pr$precision2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")
lines(c(0, 1), c(noSkill, noSkill), lty = "dashed")  # no-skill curve
grid()
mtext("(e)", line = 1.2, at = -0.08, cex = cex.panel)

## S2S
CM_ <- CM
CM_ <- CM_[CM_$gtype_rank != "tertiary", ]
CM_ <- CM_[CM_$region == "S2S", ]
pred <- prediction(CM_$pu_max, CM_$forecasted)
pr <- performance(pred, "prec", "rec")
pr <- format_ROCRPerformanceObject(pr)
CI_pr <- bootstrapConfusionMatrix(CM_, perf = pr, nboots = 400)
plot(pr, colorize = TRUE, add = TRUE, lwd = 2)
polygon(c(CI_pr$recall, rev(CI_pr$recall)), c(CI_pr$precision1, rev(CI_pr$precision2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")

## BYK
CM_ <- CM
CM_ <- CM_[CM_$gtype_rank != "tertiary", ]
CM_ <- CM_[CM_$region == "BYK", ]
pred <- prediction(CM_$pu_max, CM_$forecasted)
pr <- performance(pred, "prec", "rec")
pr <- format_ROCRPerformanceObject(pr)
CI_pr <- bootstrapConfusionMatrix(CM_, perf = pr, nboots = 400)
plot(pr, colorize = TRUE, add = TRUE, lwd = 2)
polygon(c(CI_pr$recall, rev(CI_pr$recall)), c(CI_pr$precision1, rev(CI_pr$precision2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")
text(0.475, 0.8, "GNP", cex = cex.annot)
text(0.1, 0.62, "S2S", cex = cex.annot)
text(0.15, 0.18, "BYK", cex = cex.annot)

## ROC
par(mar = c(5.1, 4.1, 4.1, 1.1))
## GNP
CM_ <- CM
CM_ <- CM_[CM_$gtype_rank != "tertiary", ]
CM_ <- CM_[CM_$region == "GNP", ]
pred <- prediction(CM_$pu_max, CM_$forecasted)
roc <- performance(pred, "tpr", "fpr")
roc <- format_ROCRPerformanceObject(roc)
CI_roc <- bootstrapConfusionMatrix(CM_, perf = roc, nboots = 99)
plot(roc, colorize = TRUE, lwd = 3, colorkey = FALSE, ylim = c(0, 1), xlim = c(0, 1), xaxs = "i", yaxs = "i", xlab = "False alarm rate", ylab = "Probability of detection (recall)")
polygon(c(CI_roc$far, rev(CI_roc$far)), c(CI_roc$recall1, rev(CI_roc$recall2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")
lines(c(0, 1), c(0, 1), lty = "dashed")  # no-skill curve
grid()
mtext("(f)", line = 1.2, at = -0.08, cex = cex.panel)

## S2S
CM_ <- CM
CM_ <- CM_[CM_$gtype_rank != "tertiary", ]
CM_ <- CM_[CM_$region == "S2S", ]
pred <- prediction(CM_$pu_max, CM_$forecasted)
roc <- performance(pred, "tpr", "fpr")
roc <- format_ROCRPerformanceObject(roc)
CI_roc <- bootstrapConfusionMatrix(CM_, perf = roc, nboots = 400)
plot(roc, colorize = TRUE, add = TRUE, lwd = 2)
polygon(c(CI_roc$far, rev(CI_roc$far)), c(CI_roc$recall1, rev(CI_roc$recall2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")

## BYK
CM_ <- CM
CM_ <- CM_[CM_$gtype_rank != "tertiary", ]
CM_ <- CM_[CM_$region == "BYK", ]
pred <- prediction(CM_$pu_max, CM_$forecasted)
roc <- performance(pred, "tpr", "fpr")
roc <- format_ROCRPerformanceObject(roc)
CI_roc <- bootstrapConfusionMatrix(CM_, perf = roc, nboots = 400)
plot(roc, colorize = TRUE, add = TRUE, lwd = 2)
polygon(c(CI_roc$far, rev(CI_roc$far)), c(CI_roc$recall1, rev(CI_roc$recall2)), col = adjustcolor("gray50", alpha.f = 0.2) , border = "transparent")

dev.off()

