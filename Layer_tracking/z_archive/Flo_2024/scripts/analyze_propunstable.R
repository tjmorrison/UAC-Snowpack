## This script analyzes the proportion of grid points unstable of captured and missed WKLs
## (1) CTree analyses

## --- initializations ----
library(sarp.2021.herla.snowprofilevalidation)
library(partykit)

## read data set that contains detailed assessments of captured and missed layers of concern VS
VS <- readRDS("output/evaluate_main/S2S_BYK_GNP_v06/captWKLAssessment.rds")
## read ValidationData VD
VD <- readRDS("output/evaluate_main/S2S_BYK_GNP_v06/mergedVDvframe.rds")
## format VS
simGsizeAtBurial <- readRDS("output/evaluate_main/S2S_BYK_GNP_v06gs/simulatedGsizeAtBurial.rds")
WKL_SHonly <- readRDS("data/VData_WKL_GNP_BYK_S2S.rds") # legacy, see format_captWKLquality.R
VS <- format_captWKLquality(VS, VD, simulatedGsizeAtBurial = simGsizeAtBurial, WKL_SHonly = WKL_SHonly$wkl)



## --- CTrees ----
## tree on weak grains:
png("output/figures/paper/ctree_propunstable.png", width = 1000, height = 500)
VS_ <- VS
VS_ <- VS_[!VS_$gtype_class %in% c("IFrc", "IFsc", "MFcr", "FCxr"), ]
VS_$gtype_class[VS_$gtype_class %in% "SH" & VS_$gtype_rank == "secondary"] <- "SH/FC"
CT <- partykit::ctree(pu_max ~ region + drySpell_median + dq + gtype_class, data = VS_, alpha = 0.05)
Vars <- setVars4ctreeColoring(VS_)
plotCTree(CT)
dev.off()


## zoom on season effect:
VS_ <- VS
VS_ <- VS_[!VS_$gtype_class %in% c("IFrc", "IFsc", "MFcr", "FCxr"), ]
CT <- partykit::ctree(pu_max ~ season, data = VS_, alpha = 0.05)
plot(CT, terminal_panel = node_violinplot)
## zoom on effect by gsizeAtBurial
VS_ <- VS
VS_ <- VS_[VS_$gtype_class %in% c("SH", "SH/FC", "FC/SH") & !is.na(VS_$gsizeAtBurial_median), ]
CT <- partykit::ctree(pu_max ~ gsizeAtBurial_median, data = VS_, alpha = 0.05)
plot(CT, terminal_panel = node_violinplot)

