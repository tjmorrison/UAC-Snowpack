#Import libraries
library(sarp.snowprofile.alignment)

#Allocate paths to files
#SP model outputs
Filename$modeled <- "/Users/travismorrison/OneDrive/Research/Projects/UAC_SNOWPACK/SP_Results/Atwater_2024/ATH20_10_05_2023_04_04_2024.pro"
# Manual pit data from Snowpilot
Filename$manual <- "/Users/travismorrison/Library/CloudStorage/OneDrive-Personal/Research/Projects/UAC_SNOWPACK/validation_pits/202401261100.SimpleProfile-v5.caaml"

#Create a profile object from the caaml v5 file
SPpairs$manual <- snowprofileCaaml(Filename$manual)


## Scan dates in modeled file
Dates <- scanProfileDates(Filename$modeled)
#print(Dates)

## Read a single profile by date 
ProfileDate <- Dates[2725] #3995 ~ 3/19; 2725 ~ 1/26

SPpairs$modeled <- snowprofilePro(Filename$modeled, ProfileDate = ProfileDate)


dtwAlignment <- dtwSP(SPpairs$modeled, SPpairs$manual, open.end = FALSE)


## Plot alignment:
#plotSPalignment(dtwAlignment = dtwAlignment)

## Inspect local cost:
plotCostDensitySP(dtwAlignment)

dtwAlignment$sim <- simSP(dtwAlignment$reference, dtwAlignment$queryWarped, verbose = TRUE, type = "HerlaEtAl2021")
## Compute alignment:
#dtwAlignment <- dtwSP(SPpairs$A_modeled, SPpairs$A_manual, open.end = FALSE)

## Plot alignment:
#png(file="/Users/travismorrison/Desktop/Example.png",width=600, height=350)
#plotSPalignment(dtwAlignment = dtwAlignment)
#dev.off()