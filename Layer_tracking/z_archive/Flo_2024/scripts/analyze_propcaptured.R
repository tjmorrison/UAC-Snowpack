## This script analyzes the proportion of grid points that structurally captured or missed WKLs
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
VS <- format_captWKLquality(VS, VD, simulatedGsizeAtBurial = simGsizeAtBurial)



## --- ctrees ----

## full tree
VS_ <- VS
CT <- partykit::ctree(pcapt_max ~ region + season + band + dq + gtype_rank + gtype_class + wkl_iscrust + nPDays + wklmonth + drySpell_median + gsizeAtBurial_median + gsize_rep, data = VS_, alpha = 0.05)
plot(CT, terminal_panel = node_violinplot)

## full tree simplified
png("output/figures/paper/ctree_propcaptured.png", width = 1000, height = 500)
VS_ <- VS
VS_ <- VS_[!VS_$gtype_class %in% c("DH", "FCxr"), ]
VS_$gtype_class <- as.character(VS_$gtype_class)
VS_$gtype_class[(VS_$gtype_class == "SH" & VS_$gtype_rank == "secondary")] <- "SH/FC"
VS_$gtype_class <- as.factor(VS_$gtype_class)
CT <- partykit::ctree(pcapt_max ~ gtype_class + drySpell_median + dq + wklmonth, data = VS_, alpha = 0.05, maxdepth = 3)
Vars <- setVars4ctreeColoring(VS_)
plotCTree(CT)
dev.off()


## zoom into crusts: i.e., RHS of full tree:
VS_ <- VS
VS_ <- VS_[VS_$gtype_class %in% c("IFrc", "IFsc", "MFcr"), ]
CT <- partykit::ctree(pcapt_max ~ season + region + band + dq + gtype_rank + gtype_class + wkl_iscrust + nPDays + wklmonth + drySpell_median, data = VS_, alpha = 0.05)
plot(CT, terminal_panel = node_violinplot)
## TAKE AWAY:
## * Early season rain and temperature crusts are well captured at BTL
## *  ~/~ on average moderately well captured at ALP and TL, but with huge spread
## * Mid- and late-season rain and temperature crusts are yet less well captured in all bands, also huge spread
## * Sun crusts are basically not captured at all (the BTL terminal node )


## zoom into weak grains: i.e., LHS of full tree:
VS_ <- VS
VS_ <- VS_[!VS_$gtype_class %in% c("IFrc", "IFsc", "MFcr"), ]
CT <- partykit::ctree(pcapt_max ~ region + season + band + dq + gtype_rank + gtype_class + wkl_iscrust + nPDays + wklmonth + drySpell_median + gsizeAtBurial_median + gsize_rep, data = VS_, alpha = 0.05)
plot(CT, terminal_panel = node_violinplot)
## TAKE AWAY:
## * Length of dry spell preceeding the burial is a driving factor: almost all WKLs with a dry spell longer than 7 days exist is almost all grid points
## * If dry spell is less than 7 days:
##   - FC are almost always captured by large majority of grid points
##   - FC are also almost always present next to SH in GNP/BYK even if they were not reported
##   - If SH or DH reported
##      > strict grain type search routine: huge spread
##   -  > relaxed routine: captured by majority of grid points


