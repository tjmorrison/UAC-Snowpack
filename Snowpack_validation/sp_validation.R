library(sarp.snowprofile.alignment)

## Compute alignment:
dtwAlignment <- dtwSP(SPpairs$A_modeled, SPpairs$A_manual, open.end = FALSE)

## Plot alignment:
png(file="/Users/travismorrison/Desktop/Example.png",width=600, height=350)
plotSPalignment(dtwAlignment = dtwAlignment)
dev.off()