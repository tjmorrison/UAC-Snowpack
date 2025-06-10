## This script analyzes the agreement indicators of captured and missed layers of concern


## --- initializations ----
library(sarp.2021.herla.snowprofilevalidation)
library(partykit)

VD <- readRDS("output/evaluate_main/S2S_BYK_GNP_v06/mergedVDvframe.rds")
VS <- readRDS("output/evaluate_main/S2S_BYK_GNP_v06/captWKLAssessment.rds")
simGsizeAtBurial <- readRDS("output/evaluate_main/S2S_BYK_GNP_v06gs/simulatedGsizeAtBurial.rds")
WKL_SHonly <- readRDS("data/VData_WKL_GNP_BYK_S2S.rds") # legacy, see format_captWKLquality.R
VS <- format_captWKLquality(VS, VD, simulatedGsizeAtBurial = simGsizeAtBurial, WKL_SHonly = WKL_SHonly$wkl)


## --- CTRees ----

## analyze the trees for each agreement indicator individually:
## Variation---correlation of reported likelihood ~ proportion unstable:
png("output/figures/paper/ctree_rhopsi.png", width = 600, height = 350)
VS_ <- VS
VS_ <- VS_[!is.na(VS_$rho_llhd) & !is.na(VS_$lagZ_20), ]
VS_ <- VS_[!VS_$gtype_class %in% c("IFrc", "IFsc", "MFcr", "FCxr") & VS_$band == "TL", ]
VS_$gtype_class[VS_$gtype_class %in% "SH" & VS_$gtype_rank == "secondary"] <- "SH/FC"
CT <- partykit::ctree(rho_llhd ~ nPDays , data = VS_, alpha = 0.05, maxdepth = 2)
Vars <- setVars4ctreeColoring(VS_)
plotCTree(CT)
dev.off()


## Timing:
## Offset total time of instability: (days with more than 20% grid points unstable) - (number of problem days)
png("output/figures/paper/ctree_lambdaduration.png", width = 800, height = 450)
VS_ <- VS
VS_ <- VS_[!is.na(VS_$rho_llhd) & !is.na(VS_$lagZ_20), ]
VS_ <- VS_[!VS_$gtype_class %in% c("IFrc", "IFsc", "MFcr", "FCxr") & VS_$band == "TL", ]
VS_$gtype_class[VS_$gtype_class %in% "SH" & VS_$gtype_rank == "secondary"] <- "SH/FC"
CT <- partykit::ctree(offset_totaltime ~ region + drySpell_median + nPDays, data = VS_, alpha = 0.05, maxdepth = 3)
Vars <- setVars4ctreeColoring(VS_)
plotCTree(CT)
dev.off()


## Lag between first day that 20% of grid points become unstable and first problem day: [-10 means model was unstable 10 days earlier than first avalanche problem]
## Note, all values of NA (i.e., pu_max < 20%) were set to 100 earlier.
png("output/figures/paper/ctree_lambdaonset.png", width = 600, height = 350)
VS_ <- VS
VS_ <- VS_[!is.na(VS_$rho_llhd) & !is.na(VS_$lagZ_20), ]
VS_ <- VS_[!VS_$gtype_class %in% c("IFrc", "IFsc", "MFcr", "FCxr") & VS_$band == "TL", ]
VS_$gtype_class[VS_$gtype_class %in% "SH" & VS_$gtype_rank == "secondary"] <- "SH/FC"
CT <- partykit::ctree(lagA_anyunstable ~ gtype_class + drySpell_median, data = VS_, alpha = 0.05, maxdepth = 2)
Vars <- setVars4ctreeColoring(VS_)
plotCTree(CT)
dev.off()

## Lag between last day that 20% of grid points... (analogon to above)
png("output/figures/paper/ctree_lambdaturnoff.png", width = 600, height = 350)
VS_ <- VS
VS_ <- VS_[!is.na(VS_$rho_llhd) & !is.na(VS_$lagZ_20), ]
VS_ <- VS_[!VS_$gtype_class %in% c("IFrc", "IFsc", "MFcr", "FCxr") & VS_$band == "TL", ]
VS_$gtype_class[VS_$gtype_class %in% "SH" & VS_$gtype_rank == "secondary"] <- "SH/FC"
CT <- partykit::ctree(lagZ_20 ~ drySpell_median +nPDays , data = VS_, alpha = 0.05, maxdepth = 1)
Vars <- setVars4ctreeColoring(VS_)
plotCTree(CT)
dev.off()


